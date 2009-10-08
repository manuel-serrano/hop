(module __dollar_scheme2js_module
   (library scheme2js)
   (export (dollar-modules-adder::procedure)))

;; this module is tightly linked to Scheme2Js. Similarly to __hop_exports the
;; module is small, and the changes inside Scheme2Js not intrusive.
;; This module should "follow" changes inside Scheme2Js.

;; This module adds support for $(import XYZ) inside Scheme2Js-module headers.

(define (dollar-modules! m::WIP-Unit imported?)
   (with-access::WIP-Unit m (header)
      (when (and (pair? header)
		 (pair? (cdr header))
		 (list? header))
	 (let loop ((hdr (cddr header))
		    (rev-dollar-clauses '()))
	    (cond
	       ((or (not hdr)
		    (null? hdr)
		    (null? (cdr hdr)))
		(unless (null? rev-dollar-clauses)
		   ;; remove the clauses from the header.
		   (let liip ((h (cddr header))
			      (rev-copied (list (cadr header) (car header))))
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
		;; evaluate the server side module
		(when (and (not imported?)
			   (scheme2js-config 'hop-module-compilation))
		   (eval (cons* 'module (gensym 'module)
				(reverse! rev-dollar-clauses)))))
	       ((eq? (car hdr) '$)
		(loop (cddr hdr)
		      (cons (cadr hdr) rev-dollar-clauses)))
	       (else
		(loop (cdr hdr)
		      rev-dollar-clauses)))))))

(define (dollar-modules-adder)
   dollar-modules!)
