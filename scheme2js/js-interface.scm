(module js-interface
   (include "runtime/runtime_mapping.sch")
   (import verbose
	   config)
   (export (extract-js-interface::pair top-level::pair-nil
				       js-interface::pair-nil)
	   *runtime-var-mapping*))

(define *runtime-var-mapping* #f)

(define (paired-interface interface)
   (match-case interface
      ((?-  ?-) interface)
      ((and ?s (? symbol?))
       (list s s))
      (else (error "paired-interface"
		   "interfaces must be either symbol or of form (scheme js)"
		   interface))))

(define (extract-js-interface top-level js-interface)
   (define top-level-head (list 'dummy))
   (define extension-head (list 'dummy))

   ;; split js-interface clauses, and normal expressions.
   (let loop ((to-parse top-level)
	      (last-top-level top-level-head)
	      (last-extension extension-head))
      (if (null? to-parse)
	  (begin
	     (set-cdr! last-top-level '())
	     (set-cdr! last-extension '()))
	  (let ((sexp (car to-parse)))
	     (if (and (pair? sexp)
		      (eq? (car sexp) 'js))
		 (begin
		    (verbose "found js-interface")
		    (set-cdr! last-extension to-parse)
		    (loop (cdr to-parse)
			  last-top-level
			  (cdr last-extension)))
		 (begin
		    (set-cdr! last-top-level to-parse)
		    (loop (cdr to-parse)
			  (cdr last-top-level)
			  last-extension))))))

   (set! *runtime-var-mapping*
	 (cond
	    ((and (config 'mutable-strings)
		  (config 'call/cc))
	     *mutable-call/cc-runtime-var-mapping*)
	    ((config 'mutable-strings)
	     *mutable-runtime-var-mapping*)
	    ((config 'call/cc)
	     *immutable-call/cc-runtime-var-mapping*)
	    (else
	     *immutable-runtime-var-mapping*)))
   ;; add our runtime in front of js-interface in front of the extension
   ;; add 'js in front of our built-in interfaces, so they ressemble the
   ;; extracted js-clauses.
   (let ((all-interfaces (append-map
			  (lambda (js-interface-sexp)
			     (map paired-interface
				   (cdr js-interface-sexp)))
			  (cons (cons 'js *runtime-var-mapping*)
				(cons (cons 'js js-interface)
				      ;; remove the 'dummy
				      (cdr extension-head))))))
      (cons (cdr top-level-head) all-interfaces)))
