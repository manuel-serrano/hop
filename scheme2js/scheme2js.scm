(module scheme2js
   (option (set! *dlopen-init* "scheme2js_s"))
   (include "version.sch")
   (import config
	   nodes
	   export
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
	   rm-unused-vars
	   tail-rec
	   constant-propagation
	   constants
	   scope
	   while
	   loop-hoist
	   propagation
	   statements
	   rm-tail-breaks
	   out
	   scm-out
	   verbose)
   ;; module-headers are module-clauses without the module-name
   ;; they are of the form (module-pairs . kind)
   ;;    kind can be either provide, replace, merge-first or merge-last
   ;;    replace (which must be unique) replaces any existing module-header
   ;;    provides are only used, if the file does not have any module-clause
   ;;    merge-first/last are added before/after the module-clause.
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
	  (top-level-e (my-expand `(begin
				      ,@(Compilation-Unit-macros module)
				      ,@(Compilation-Unit-top-level module))))
	  (dummy1 (if (eq? debug-stage 'expand)
		      (pp top-level-e p)))
	  (top-level-runtime-e (runtime-expand! top-level-e))
	  (dummy2 (if (eq? debug-stage 'runtime-expand)
		      (pp top-level-runtime-e p)))
	  (tree (pobject-conv top-level-runtime-e)))

      ;;we could do the letrec-expansion in list-form too.
      (pass 'letrec        (letrec-expansion! tree))

      (pass 'symbol      (symbol-resolution tree
					    (Compilation-Unit-imports module)
					    (Compilation-Unit-exports module)))
      (pass 'encapsulation (encapsulation! tree))
      (pass 'node-elim1    (node-elimination! tree))
      (pass 'tail-rec      (tail-rec! tree))
      (pass 'node-elim2    (node-elimination! tree))
      (pass 'inline        (inline! tree #t))
      (pass 'tail-rec2     (tail-rec! tree))
      (pass 'inline2       (inline! tree #f)) ;; a second faster inlining.
      (pass 'constant      (constant-propagation! tree))
      (pass 'rm-unused     (rm-unused-vars! tree))
      (pass 'node-elim3    (node-elimination! tree))
      ;(call/cc-early tree)
      ;(trampoline tree)
      (pass 'constants     (constants! tree))
      (pass 'scope         (scope-resolution! tree))
      (pass 'while         (tail-rec->while! tree))
      (pass 'hoist         (loop-hoist! tree))
      (pass 'propagation   (propagation! tree))
      ;(var-elimination! tree)
      (pass 'rm-unused2    (rm-unused-vars! tree))
      (pass 'flatten       (scope-flattening! tree))
      (pass 'stmts         (statements! tree))
      (pass 'while-optim   (optimize-while! tree))
      (pass 'node-elim4    (node-elimination! tree))
      (pass 'rm-breaks     (rm-tail-breaks! tree))
      (pass 'node-elim5    (node-elimination! tree))
      ;(call/cc-late! tree)
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
   (config-init! configuration)
   (let ((module (create-module-from-expr expr module-headers)))
      (scheme2js module out-p)))

(define (scheme2js-compile-file in-file out-file
				module-headers ;; list (module-clause . kind)
				;; kind: provide/replace/merge-first/merge-last
				configuration
				#!key (reader read))
   ;; we need this for "verbose" outputs.
   (config-init! configuration)
   (let* ((module (create-module-from-file in-file module-headers reader))
	  (out-port (if (string=? "-" out-file)
		       (current-output-port)
		       (open-output-file out-file))))

      ;; set the global-seed to the input-file's name (which is the same as the
      ;; module's name, if any was given).
      (unless (or (config 'statics-suffix)
		  (string=? "-" in-file))
	 (config-set! 'statics-suffix
		      (string-append "_" (prefix (basename in-file)))))

      (when (eq? (config 'export-globals) 'module)
	 (config-set! 'export-globals
		      (not (Compilation-Unit-name module))))

      (when (eq? (config 'unresolved=JS) 'module)
	 (config-set! 'unresolved=JS
		      (not (Compilation-Unit-name module))))

      (scheme2js module out-port)
      (if (not (string=? "-" out-file))
	  (close-output-port out-port))))
