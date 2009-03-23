(module __dollar_scheme2js_module
   (library scheme2js)
   (export (dollar-modules-adder::procedure)))

;; this module is tightly linked to Scheme2Js. Similarly to __hop_exports the
;; module is small, and the changes inside Scheme2Js not intrusive.
;; This module should "follow" changes inside Scheme2Js.

;; This module adds support for $(import XYZ) inside Scheme2Js-module headers.

(define (dollar-modules! m::WIP-Unit)
   (with-access::WIP-Unit m (header)
      (when (and (pair? header)
		 (pair? (cdr header)))
	 (let loop ((header (cddr header))
		    (rev-dollar-clauses '()))
	    (cond
	       ((or (not header)
		    (null? header)
		    (null? (cdr header)))
		(unless (null? rev-dollar-clauses)
		   ;; remove the clauses from the header.
		   (let liip ((h header)
			      (rev-copied '()))
		      (cond
			 ((null? h)
			  (set! header (reverse! rev-copied)))
			 ((null? (cdr h))
			  (liip '() (cons (car h) rev-copied)))
			 ((eq? (car h) '$)
			  (liip (cddr h) rev-copied))
			 (else
			  (liip (cdr h)
				(cons (car h) rev-copied))))))
		(eval (cons* 'module (gensym) (reverse! rev-dollar-clauses))))
	       ((eq? (car header) '$)
		(loop (cddr header)
		      (cons (cadr header) rev-dollar-clauses)))
	       (else
		(loop (cdr header)
		      rev-dollar-clauses)))))))

(define (dollar-modules-adder)
   dollar-modules!)
