;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/scheme2js.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-12                                           */
;*    Last change :  Wed Sep  4 17:56:44 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js/HOP.                              */
;*    -------------------------------------------------------------    */
;*    Scheme2JS entry points                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module scheme2js
   
   (option (set! *dlopen-init* "scheme2js_s"))
   
   (include "version.sch")
   
   (import config
	   nodes
	   srfi0
	   export-desc
	   module-system
	   expand
	   expanders
	   runtime-expander
	   dsssl-expander
	   dot-expand
	   pobject-conv
	   symbol
	   letrec-expansion
	   encapsulation
	   node-elimination
	   inline
	   pragma
	   call-check
	   rm-unused-vars
	   tail
	   tail-rec
	   constant-propagation
	   constants
	   scope
	   while
	   propagation
	   statements
	   rm-tail-breaks
	   out
	   scm-out
	   verbose
	   trampoline
	   dump-node
	   source-map
	   stdlib)

   (cond-expand
      (enable-callcc (import callcc)))
   
   ;; see module.scm for information on module-headers.
   (export (scheme2js-compile-expr expr
	      out-p
	      module-headers::pair-nil
	      configuration)
	   (scheme2js-compile-value expr::obj
	      out-p::output-port
	      extension-compiler::procedure
	      value-register::procedure
	      loc)
	   (scheme2js-compile-file in-file::bstring
	      out-file::bstring
	      module-headers::pair-nil
	      configuration
	      #!key (reader read))
	   (precompile-imported-module-file name::symbol file::bstring
	      reader::procedure
	      configuration
	      #!key
	      (bigloo-modules? #t)
	      (store-exports-in-ht? #f)
	      (store-exported-macros-in-ht? #f))))

;*---------------------------------------------------------------------*/
;*    pass ... ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (pass name . Lrest)
   `(begin
       ,@Lrest
       (when (eq? debug-stage ,name)
	  (scm-out tree p))))

;*---------------------------------------------------------------------*/
;*    scheme2js ...                                                    */
;*---------------------------------------------------------------------*/
(define (scheme2js module source-file p)
   (with-access::Compilation-Unit module (imports exports top-level macros name)
      (let* ((debug-stage (config 'debug-stage))
	     (top-level-e (my-expand `(begin ,@top-level) macros))
	     (dummy1 (when (eq? debug-stage 'expand)
			(pp top-level-e p)))
	     (top-level-runtime-e (runtime-expand! top-level-e))
	     (dummy2 (when (eq? debug-stage 'runtime-expand)
			(pp top-level-runtime-e p)))
	     (dsssl-e (dsssl-expand! top-level-runtime-e))
	     (dummy3 (when (eq? debug-stage 'dsssl-expand)
			(pp dsssl-e p)))
	     (tree::Module (pobject-conv dsssl-e)))
	 ;;we could do the letrec-expansion in list-form too.
	 (pass 'letrec         (letrec-expansion! tree))
;* 	 (scheme2js-dump-temporary tree "letrec")                      */
	 (pass 'symbol         (symbol-resolution tree imports exports))
	 (pass 'encapsulation  (encapsulation! tree))
	 (pass 'node-elim1     (node-elimination! tree))
	 (if (config 'frame-push)
	     (pass 'tail-rec   (tail-rec! tree))
	     (pass 'tail-rec   (tail-calls tree)))
	 (pass 'node-elim2     (node-elimination! tree))
;* 	 (scheme2js-dump-temporary tree "elim2")                       */
	 (pass 'inline         (inline! tree #t))
	 (pass 'call-check     (call-check tree))
	 (if (config 'frame-push)
	     (pass 'tail-rec2  (tail-rec! tree))
	     (pass 'tail-rec2  (tail-calls tree)))
	 (pass 'inline2        (inline! tree #f)) ;; a second faster inlining.
	 (pass 'constant       (constant-propagation! tree))
	 (pass 'pragmas        (pragmas! tree))
	 (pass 'rm-unused      (rm-unused-vars! tree))
	 (pass 'node-elim3     (node-elimination! tree))
	 (cond-expand
	    (enable-callcc
	     (pass 'call/cc-early (call/cc-early! tree))))
	 (pass 'trampoline     (trampoline tree))
;* 	 (scheme2js-dump-temporary tree "bscope")                      */
	 (pass 'scope          (scope-resolution! tree))
;* 	 (scheme2js-dump-temporary tree "ascope")                      */
	 (pass 'constants      (constants! tree))
	 (pass 'tail-rec       (tail-rec! tree))
	 (pass 'while          (tail-rec->while! tree))
	 (pass 'propagation    (propagation! tree))
	 ;(var-elimination! tree)
	 (pass 'rm-unused2     (rm-unused-vars! tree))
	 (cond-expand
	    (enable-callcc
	     (pass 'call/cc-middle (call/cc-middle tree))))
	 (pass 'flatten        (scope-flattening! tree))
	 (pass 'stmts          (statements! tree))
	 (pass 'while-optim    (optimize-while! tree))
	 (pass 'node-elim4     (node-elimination! tree))
	 (pass 'rm-breaks      (rm-tail-breaks! tree))
	 (pass 'node-elim5     (node-elimination! tree))
;* 	 (scheme2js-dump-temporary tree "last")                        */
	 (cond-expand
	    (enable-callcc
	     (pass 'call/cc-late (call/cc-late! tree))))
	 ;;(locations tree)
	 (out tree source-file p)
	 (verbose "--- compiled")
;* 	 (scheme2js-dump-temporary tree "final")                       */
	 tree)))

;*---------------------------------------------------------------------*/
;*    scheme2js-dump-temporary ...                                     */
;*---------------------------------------------------------------------*/
(define (scheme2js-dump-temporary tree suffix)
   (call-with-output-file (string-append "/tmp/FOO." suffix ".scm")
      (lambda (p)
	 (with-access::Module tree (body)
	    (display "body=" p)
	    (display (typeof body) p)
	    (newline p)
	    (write (node->list body) p)
	    (newline p)))))

;*---------------------------------------------------------------------*/
;*    scheme2js-compile-expr ...                                       */
;*---------------------------------------------------------------------*/
(define (scheme2js-compile-expr expr out-p
	   module-headers
	   configuration)
   (let ((old-configs (configs-backup)))
      (unwind-protect
	 (begin
	    (config-init! configuration)
	    (let* ((module (create-module-from-expr expr module-headers)))
	       (scheme2js module #f out-p)))
	 (configs-restore! old-configs))))

;*---------------------------------------------------------------------*/
;*    scheme2js-compile-value ...                                      */
;*---------------------------------------------------------------------*/
(define (scheme2js-compile-value val outp extension-compiler value-register loc)
   (compile-value val outp extension-compiler value-register loc))

;*---------------------------------------------------------------------*/
;*    scheme2js-compile-file ...                                       */
;*---------------------------------------------------------------------*/
(define (scheme2js-compile-file in-file out-file
	   module-headers ;; list (module-clause . kind)
	   ;; kind: provide/replace/merge-first/merge-last
	   configuration
	   #!key (reader read))
   (let ((old-configs (configs-backup)))
      (unwind-protect
	 (let ((actual-file? (not (string=? "-" out-file))))
	    ;; we need this for "verbose" outputs.
	    (config-init! configuration)
	    (with-handler
	       ;; delete unfinished file, so that we don't give impression that
	       ;; compilation was successful (for tools like 'make'...).
	       ;; note: the port was already closed before.
	       (lambda (e)
		  ;; when debugging we usually want the
		  ;; output even (or especially) if there was an error.
		  (unless (or (config 'debug-stage) (not actual-file?))
		     (delete-file out-file))
		  (raise e))
	       (let ((module (create-module-from-file in-file
				module-headers
				reader)))
		  ;; set the global-seed to the input-file's name (which is the
		  ;; same as the module's name, if any was given).
		  (unless (or (config 'statics-suffix)
			      (string=? "-" in-file))
		     (with-access::Compilation-Unit module (name)
			(config-set! 'statics-suffix
			   (symbol->string name))))
		  
		  (with-access::Compilation-Unit module (declared-module?)
		     (when (eq? (config 'export-globals) 'module)
			(config-set! 'export-globals
			   (not declared-module?)))
		     
		     (when (eq? (config 'allow-unresolved) 'module)
			(config-set! 'allow-unresolved
			   (not declared-module?))))
		  
		  (let ((tree (cond
				 (actual-file?
				  (call-with-output-file out-file
				     (lambda (out-p)
					(fprintf out-p "/* scheme2js generated file ~a */\n" (current-date))
					(when (config 'use-strict/module)
					   (fprintf out-p "\"use strict\";\n\n"))
					(scheme2js module in-file out-p))))
				 ((config 'source-map)
				  (set! actual-file? #t)
				  (set! out-file
				     (make-file-name
					(config 'tmp-dir)
					(format "~a.js" (tmpfile))))
				  (let ((tree (call-with-output-file out-file
						 (lambda (out-p)
						    (scheme2js module in-file out-p)))))
				     (call-with-input-file out-file
					(lambda (p)
					   (send-chars p (current-output-port))))
				     tree))
				 (else
				  (printf "/* scheme2js generated file ~a */\n"
				     (current-date))
				  (when (config 'use-strict/module)
				     (print "\"use strict\";\n"))
				  (scheme2js module in-file (current-output-port))))))
		     (when (config 'source-map)
			(let* ((srcmap (string-append out-file ".map"))
			       (files (call-with-output-file srcmap
					 (lambda (p)
					    (generate-source-map tree in-file out-file p)))))
			   (when (pair? files)
			      (call-with-append-file out-file
				 (lambda (p)
				    (generate-source-mapping-url
				       files srcmap p))))
			   (unless actual-file?
			      (delete-file out-file))))))))
	 (configs-restore! old-configs))))

;*---------------------------------------------------------------------*/
;*    generate-source-mapping-url ...                                  */
;*---------------------------------------------------------------------*/
(define (generate-source-mapping-url files srcmap p::output-port)
   (display "\n\n" p)
   (for-each (lambda (f)
		(fprintf p "hop_source_mapping_url( ~s, \"~a\" );\n" f srcmap))
      files)
   (fprintf p "\n//# sourceMappingURL=~a\n" srcmap))

;*---------------------------------------------------------------------*/
;*    *tmp-mutex* ...                                                  */
;*---------------------------------------------------------------------*/
(define *tmp-mutex* (make-mutex))

;*---------------------------------------------------------------------*/
;*    *tmp-cnt* ...                                                    */
;*---------------------------------------------------------------------*/
(define *tmp-cnt* (elong->fixnum (current-seconds)))

;*---------------------------------------------------------------------*/
;*    tmpfile ...                                                      */
;*---------------------------------------------------------------------*/
(define (tmpfile)
   (synchronize *tmp-mutex*
      (let ((n *tmp-cnt*))
	 (set! *tmp-cnt* (+fx 1 *tmp-cnt*))
	 (string-append "tmp" (integer->string *tmp-cnt*)))))

;*---------------------------------------------------------------------*/
;*    precompile-imported-module-file ...                              */
;*---------------------------------------------------------------------*/
(define (precompile-imported-module-file name::symbol file::bstring
	   reader::procedure
	   configuration
	   #!key
	   (bigloo-modules? #t)
	   (store-exports-in-ht? #f)
	   (store-exported-macros-in-ht? #f))
   (let ((old-configs (configs-backup)))
      (unwind-protect
	 (begin
	    (config-init! configuration)
	    (read-imported-module-file name file reader
	       :bigloo-modules? bigloo-modules?
	       :store-exports-in-ht?
	       store-exports-in-ht?
	       :store-exported-macros-in-ht?
	       store-exported-macros-in-ht?))
	 (configs-restore! old-configs))))
