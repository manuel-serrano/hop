;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

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
	   dot-expand
	   pobject-conv
	   symbol
	   letrec-expansion
	   encapsulation
	   node-elimination
	   inline
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
	   trampoline)
   ;; see module.scm for information on module-headers.
   (export (scheme2js-compile-expr expr
				   out-p
				   module-headers::pair-nil
				   configuration)
	   (scheme2js-compile-file in-file::bstring
				   out-file::bstring
				   module-headers::pair-nil
				   configuration
				   #!key (reader read))))

(define-macro (pass name . Lrest)
   `(begin
       ,@Lrest
       (when (eq? debug-stage ,name)
	  (scm-out tree p))))

(define (scheme2js module p)
   (let* ((debug-stage (config 'debug-stage))
	  (top-level-e (my-expand `(begin ,@(Compilation-Unit-top-level module))
				  (Compilation-Unit-macros module)))
	  (dummy1 (when (eq? debug-stage 'expand)
		     (pp top-level-e p)))
	  (top-level-runtime-e (runtime-expand! top-level-e))
	  (dummy2 (when (eq? debug-stage 'runtime-expand)
		     (pp top-level-runtime-e p)))
	  (tree (pobject-conv top-level-runtime-e)))

      ;;we could do the letrec-expansion in list-form too.
      (pass 'letrec      (letrec-expansion! tree))

      (pass 'symbol      (symbol-resolution tree
					    (Compilation-Unit-imports module)
					    (Compilation-Unit-exports module)))
      (pass 'encapsulation  (encapsulation! tree))
      (pass 'node-elim1     (node-elimination! tree))
      (pass 'tail-rec       (tail-rec! tree))
      (pass 'node-elim2     (node-elimination! tree))
      (pass 'inline         (inline! tree #t))
      (pass 'call-check     (call-check tree))
      (pass 'tail-rec2      (tail-rec! tree))
      (pass 'inline2        (inline! tree #f)) ;; a second faster inlining.
      (pass 'constant       (constant-propagation! tree))
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
      ;(locations tree)
      (if debug-stage
	  (let ((tmp (open-output-string)))
	     (out tree tmp)
	     (close-output-port tmp))
	  (out tree p))
      (verbose "--- compiled")))

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
		     
		     (when (eq? (config 'unresolved=JS) 'module)
			(config-set! 'unresolved=JS
				     (not declared-module?))))
		  
		  (let ((out-p (if actual-file?
				   (open-output-file out-file)
				   (current-output-port))))
		     (unwind-protect
			(scheme2js module out-p)
			(when actual-file? (close-output-port out-p)))))))
	 (configs-restore! old-configs))))
