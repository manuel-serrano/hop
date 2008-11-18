(module __dollar_scheme2js_module
   (library scheme2js)
   (export (dollar-modules-adder::procedure)))

;; this module is tightly linked to Scheme2Js. Similarly to __hop_exports the
;; module is small, and the changes inside Scheme2Js not intrusive.
;; This module should "follow" changes inside Scheme2Js.

;; This module adds support for ($ (import XYZ)) inside Scheme2Js-module headers.

(define (dollar-modules! m::WIP-Unit)
   (with-access::WIP-Unit m (header)
      (when (and (pair? header)
		 (pair? (cdr header)))
	 (let loop ((header (cddr header))
		    (dollar-clauses '()))
	    (cond
	       ((or (not header) (null? header))
		(eval (cons* 'module (gensym) dollar-clauses)))
	       ((not (pair? (car header))) ;; should never happen
		(loop (cdr header) dollar-clauses))
	       ((eq? (caar header) '$)
		(loop (cdr header)
		      (append (cdr (car header)) dollar-clauses)))
	       (else
		(loop (cdr header)
		      dollar-clauses)))))))

(define (dollar-modules-adder)
   dollar-modules!)
