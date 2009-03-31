(module srfi0
   (include "version.sch")
   (import expand)
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

(define (check-srfi0-test test)
   (match-case test
      (((kwote and) . ?Ltests)
       (every? check-srfi0-test Ltests))
      (((kwote or) . ?Ltests)
       (any? check-srfi0-test Ltests))
      (((kwote not) ?test)
       (not (check-srfi0-test test)))
      (else
       (if (symbol? test)
	   (srfi0-declared? test)
	   (error "cond-expand"
		  "Illegal feature-requirement"
		  test)))))

(define (match-clauses clauses complete-form)
   (cond
      ((null? clauses)
       (error "cond-expand"
	      "no matching clause"
	      complete-form))
      ((not (pair? clauses))
       (error "cond-expand"
	      "Illegal cond-expand form"
	      complete-form))
      (else
       (match-case (car clauses)
	  ((else ?exp0 . ?Lexps)
	   (when (not (null? (cdr clauses)))
	      (error "cond-expand"
		     "Illegal cond-expand form"
		     complete-form))
	   (cdr (car clauses)))
	  ((?test ?exp0 . ?Lexps)
	   (if (check-srfi0-test test)
	       (cdr (car clauses))
	       (match-clauses (cdr clauses) complete-form)))
	  (else
	   (error "cond-expand"
		  "Illegal cond-expand clause"
		  (car clauses)))))))

(define (srfi0-expand expr)
   (match-case expr
      ((cond-expand ?clause . ?Lclauses)
       (match-clauses (cdr expr) expr))
      (else
       (error "srfi0-expand"
	      "srfi0-expand called on invalid cond-expand form"
	      expr))))

(install-expander!
 'cond-expand
 (lambda (x e)
    (match-case x
       ((?- ?clause . ?Lclauses)
	(e `(begin ,@(match-clauses (cdr x) x)) e))
       (else
	(error "cond-expand"
	       "Illegal cond-expand form"
	       x)))))
