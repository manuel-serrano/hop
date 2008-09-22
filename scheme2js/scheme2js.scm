(module scheme2js
   (option (set! *dlopen-init* "scheme2js_s"))
   (include "version.sch")
   (import config
	   nodes
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

(define (scheme2js module p)
   (let* ((top-level-e (my-expand `(begin
				      ,@(Module-header-macros module)
				      ,@(Module-header-top-level module))))
	  (dummy1 (if (eq? (config 'debug-stage) 'expand)
		      (pp top-level-e p)))
	  (top-level-runtime-e (runtime-expand! top-level-e))
	  (dummy2 (if (eq? (config 'debug-stage) 'runtime-expand)
		      (pp top-level-runtime-e p)))
	  (tree (pobject-conv top-level-runtime-e)))
      (letrec-expansion! tree) ;;we could do this expansion in list-form too...
      (symbol-resolution tree
			 (Module-header-imports module)
			 (Module-header-exports module))
      (encapsulation! tree)
      (node-elimination! tree)
;      (scm-out tree (current-output-port))
      (tail-rec! tree)
      (node-elimination! tree)
      (inline! tree #t)
      (tail-rec! tree)
      (inline! tree #f) ;; a second faster inlining.
      (constant-propagation! tree)
      (rm-unused-vars! tree)
      (node-elimination! tree)
      ;(call/cc-early tree)
      ;(trampoline tree)
      (constants! tree)
      (scope-resolution! tree)
      (tail-rec->while! tree)
      (loop-hoist! tree)
      (propagation! tree)
      ;(var-elimination! tree)
      (rm-unused-vars! tree)
      (scope-flattening! tree)
      (statements! tree)
      (optimize-while! tree)
      (node-elimination! tree)
      (rm-tail-breaks! tree)
      (node-elimination! tree)
      ;(call/cc-late! tree)
      ;(locations tree)
      (out tree p)
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
		      (not (Module-header-name module))))

      (when (eq? (config 'unresolved=JS) 'module)
	 (config-set! 'unresolved=JS
		      (not (Module-header-name module))))

      (scheme2js module out-port)
      (if (not (string=? "-" out-file))
	  (close-output-port out-port))))
