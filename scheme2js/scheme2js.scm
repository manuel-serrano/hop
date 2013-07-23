;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/scheme2js.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-12                                           */
;*    Last change :  Tue Jul 23 09:55:38 2013 (serrano)                */
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
	   callcc
	   trampoline
	   dump-node)
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
(define (scheme2js module p)
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
	 (pass 'symbol         (symbol-resolution tree imports exports))
	 (pass 'encapsulation  (encapsulation! tree))
	 (pass 'node-elim1     (node-elimination! tree))
	 (pass 'tail-rec       (tail-rec! tree))
	 (pass 'node-elim2     (node-elimination! tree))
	 (pass 'inline         (inline! tree #t))
	 (pass 'call-check     (call-check tree))
	 (pass 'tail-rec2      (tail-rec! tree))
	 (pass 'inline2        (inline! tree #f)) ;; a second faster inlining.
	 (pass 'constant       (constant-propagation! tree))
	 (pass 'pragmas        (pragmas! tree))
	 (pass 'rm-unused      (rm-unused-vars! tree))
	 (pass 'node-elim3     (node-elimination! tree))
	 (pass 'call/cc-early  (call/cc-early! tree))
	 (pass 'trampoline     (trampoline tree))
	 (pass 'scope          (scope-resolution! tree))
	 (pass 'constants      (constants! tree))
	 (pass 'while          (tail-rec->while! tree))
	 (pass 'propagation    (propagation! tree))
	 ;(var-elimination! tree)
	 (pass 'rm-unused2     (rm-unused-vars! tree))
	 (pass 'call/cc-middle (call/cc-middle tree))
	 (pass 'flatten        (scope-flattening! tree))
	 (pass 'stmts          (statements! tree))
	 (pass 'while-optim    (optimize-while! tree))
	 (pass 'node-elim4     (node-elimination! tree))
	 (pass 'rm-breaks      (rm-tail-breaks! tree))
	 (pass 'node-elim5     (node-elimination! tree))
	 (pass 'call/cc-late   (call/cc-late! tree))
	 ;;(locations tree)
	 (if debug-stage
	     (let ((tmp (open-output-string)))
		(out tree tmp)
		(close-output-port tmp))
	     (out tree p))
	 (when (> (bigloo-debug) 2)
	    (tprint "TREE=" (node->list (-> tree body))))
	 ;; source map
	 (when (config 'source-map)
	    (fprint p "\n\n//@ sourceMappingURL=/tmp/mapping.js.map"))
	 (verbose "--- compiled"))))

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
	       (scheme2js module out-p)))
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
		  (unless (or (config 'debug-stage)
			      (not actual-file?))
		     (delete-file out-file))
		  (raise e))
	       (let ((module (create-module-from-file in-file
				module-headers
				reader)))
		  ;; set the global-seed to the input-file's name (which is the
		  ;; same as the module's name, if any was given).
		  (unless (or (config 'statics-suffix)
			      (string=? "-" in-file))
		     (config-set! 'statics-suffix
			(string-append "_"
			   (prefix (basename in-file)))))
		  
		  (with-access::Compilation-Unit module (declared-module?)
		     (when (eq? (config 'export-globals) 'module)
			(config-set! 'export-globals
			   (not declared-module?)))
		     
		     (when (eq? (config 'allow-unresolved) 'module)
			(config-set! 'allow-unresolved
			   (not declared-module?))))
		  
		  (let ((out-p (if actual-file?
				   (open-output-file out-file)
				   (current-output-port))))
		     (unwind-protect
			(scheme2js module out-p)
			(when actual-file? (close-output-port out-p)))))))
	 (configs-restore! old-configs))))

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
