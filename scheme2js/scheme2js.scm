(module scheme2js
   (option (set! *dlopen-init* "scheme2js_s"))
   (option (loadq "protobject-eval.sch"))
   (include "version.sch")
   (import config
	   module-system
	   infotron
	   compile
	   pobject-conv
	   expand
	   expanders
	   runtime-expander
	   dot-expand
	   symbol
	   letrec-expansion
	   side
	   tail-rec
	   encapsulation
	   statements
	   node-elimination
	   traverse
	   inline
	   loop-hoist
	   constant-propagation
	   propagation
	   var-elimination
	   while
	   trampoline
	   scope
	   protobject
	   liveness
	   rm-unused-vars
	   constants
	   locations
	   callcc
	   verbose
	   check)
   ;; config-tables are modified during compilation. Do not reuse them!
   ;; see config.scm for config-tables (how to create them...)
   ;;
   ;; module-headers are module-clauses without the module-name
   ;; they are of the form (module-pairs . kind)
   ;;    kind can be either provide, replace, merge-first or merge-last
   ;;    replace (which must be unique) replaces any existing module-header
   ;;    provides are only used, if the file does not have any module-clause
   ;;    merge-first/last are added before/after the module-clause.
   (export (scheme2js-compile-expr expr
				   out-p
				   module-headers::pair-nil
				   config-ht)
	   (scheme2js-compile-file in-file::bstring
				   out-file::bstring
				   module-headers::pair-nil
				   config-ht
				   #!key (reader read))))

(define (dot-out p tree)
   (with-output-to-port p
      (lambda ()
	 (pobject-dot-out tree (lambda (id)
				  (not (memq id
					     '(traverse traverse0
							traverse1 traverse2
							traverse3
							traverse0! traverse1!
							traverse2! traverse3!
							clone
							deep-clone
							already-defined?
							constant?
							value
							target
							imported-vars
							runtime-vars
							exported-vars
							var
							loc
   ))))))))

(define (scheme2js module config-ht p)
   (define-macro (do-pass name action)
      `(begin
	  ,action
	  (if (eq? (config 'debug-stage) ,name)
	      (dot-out p tree)
	      (flush-output-port p))))

   (config-init! config-ht)
   (let* ((top-level-e (my-expand `(begin
				      ,@(Module-macros module)
				      ,@(Module-top-level module))))
	  (dummy1 (if (eq? (config 'debug-stage) 'expand)
		      (pp top-level-e p)))
	  (top-level-runtime-e (runtime-expand! top-level-e))
	  (dummy2 (if (eq? (config 'debug-stage) 'runtime-expand)
		      (pp top-level-runtime-e p)))
	  (tree (pobject-conv top-level-runtime-e)))
      (if (eq? (config 'debug-stage) 'tree) (dot-out p tree))
      (do-pass 'symbol (symbol-resolution tree
					  (Module-imports module)
					  (Module-exports module)))
      (do-pass 'letrec (letrec-expansion! tree))
      (do-pass 'encapsulation (encapsulation! tree))
      (do-pass 'node-elim1 (node-elimination! tree))
      (do-pass 'constant-propagation (constant-propagation! tree))
      (do-pass 'tail-rec (tail-rec! tree))
      (do-pass 'node-elim2 (node-elimination! tree))
      (do-pass 'inline (inline! tree))
      (do-pass 'rm-unused-vars (rm-unused-vars! tree))
      (do-pass 'node-elim3 (node-elimination! tree))
      (do-pass 'call/cc-early (call/cc-early! tree))
      (do-pass 'trampoline (trampoline tree))
      (do-pass 'constants (constants! tree))
      (do-pass 'scope (scope-resolution! tree))
      (do-pass 'while (tail-rec->while! tree))
      (do-pass 'loop-hoist (loop-hoist! tree))
      (do-pass 'propagation (propagation! tree))
      (do-pass 'var-elim (var-elimination! tree))
      (do-pass 'rm-unused-vars (rm-unused-vars! tree))
      (do-pass 'scope-flattening (scope-flattening! tree))
      (do-pass 'while-optim (optimize-while! tree))
      (do-pass 'statements (statements! tree))
      (do-pass 'node-elim5 (node-elimination! tree))
      (do-pass 'call/cc-late (call/cc-late! tree))
      ;(do-pass 'check (check tree))
      (do-pass 'locations (locations tree))
      (let* ((out-p (if (config 'debug-stage)
			(open-output-string)
			p))
	     (compiled (compile tree out-p)))
	 (if (config 'debug-stage) (close-output-port out-p))
	 (if (eq? (config 'debug-stage) 'compiled) (dot-out p tree))
	 (verbose "--- compiled")
	 )))

(define (scheme2js-compile-expr expr out-p
				module-headers
				config-ht)
   (config-init! config-ht)
   (let ((module (create-module-from-expr expr module-headers)))
      (scheme2js module config-ht out-p)))

(define (scheme2js-compile-file in-file out-file
				module-headers ;; list (module-clause . kind)
				;; kind: provide/replace/merge-first/merge-last
				config-ht
				#!key (reader read))
   ;; we need this for "verbose" outputs.
   (config-init! config-ht)
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
		      (not (Module-name module))))

      (when (eq? (config 'unresolved=JS) 'module)
	 (config-set! 'unresolved=JS
		      (not (Module-name module))))

      (scheme2js module config-ht out-port)
      (if (not (string=? "-" out-file))
	  (close-output-port out-port))))
