(module srfi0
   (include "version.sch")
   (import error expand)
   (export
    (srfi0-expand expr)
    (srfi0-declare! sym::symbol)
    (srfi0-declared?::bool sym::symbol)))

(define *srfi0-key* (gensym 'srfi0))

(define (srfi0-declare! sym)
   (putprop! sym *srfi0-key* #t))
(define (srfi0-declared? sym)
   (getprop sym *srfi0-key*))

(srfi0-declare! (string->symbol (format "scheme2js-~a" *version*)))
(srfi0-declare! 'scheme2js)
(srfi0-declare! 'srfi0)

(define (check-srfi0-test test clause)
   (define (inner-tester test)
      (match-case test
	 (((kwote and) . ?Ltests)
	  (every? inner-tester Ltests))
	 (((kwote or) . ?Ltests)
	  (any? inner-tester Ltests))
	 (((kwote not) ?test)
	  (not (inner-tester test)))
	 (else
	  (if (symbol? test)
	      (srfi0-declared? test)
	      (scheme2js-error "cond-expand"
			       "Illegal feature-requirement"
			       test
			       (if (pair? test)
				   test
				   clause))))))
   (inner-tester test))

(define (match-clauses clauses complete-form)
   (cond
      ((null? clauses)
       (scheme2js-error "cond-expand"
			"no matching clause"
			complete-form
			complete-form))
      ((not (pair? clauses))
       (scheme2js-error "cond-expand"
			"Illegal cond-expand form"
			complete-form
			complete-form))
      (else
       (match-case (car clauses)
	  ((else ?exp0 . ?Lexps)
	   (when (not (null? (cdr clauses)))
	      (scheme2js-error "cond-expand"
			       "Illegal cond-expand form"
			       complete-form
			       complete-form))
	   (cdr (car clauses)))
	  ((?test ?exp0 . ?Lexps)
	   (if (check-srfi0-test test (car clauses))
	       (cdr (car clauses))
	       (match-clauses (cdr clauses) complete-form)))
	  (else
	   (scheme2js-error "cond-expand"
			    "Illegal cond-expand clause"
			    (car clauses)
			    (if (pair? (car clauses))
				(car clauses)
				complete-form)))))))

(define (srfi0-expand expr)
   (match-case expr
      ((cond-expand ?clause . ?Lclauses)
       (match-clauses (cdr expr) expr))
      (else
       (scheme2js-error "srfi0-expand"
			"srfi0-expand called on invalid cond-expand form"
			expr
			expr))))

(install-expander!
 'cond-expand
 (lambda (x e)
    (match-case x
       ((?- ?clause . ?Lclauses)
	(e `(begin ,@(match-clauses (cdr x) x)) e))
       (else
	(scheme2js-error "cond-expand"
			 "Illegal cond-expand form"
			 x
			 x)))))
