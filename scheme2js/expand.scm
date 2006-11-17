(module expand
   (import verbose)
   (include "tools.sch")
   (export (my-expand x)
	   (install-expander! id e)
	   (add-pre-expand! f::procedure)
	   (pre-expand! x)
	   (identify-expander x e macros-ht))
   (eval (export add-pre-expand!)))

(define *pre-expanders* '())

(define *mutex* (make-mutex))

;; pre-expander is shared by all parallel computations.
;; TODO: remove pre-expander.
(define (add-pre-expand! f)
   (mutex-lock! *mutex*)
   (set! *pre-expanders* (cons f *pre-expanders*))
   (mutex-unlock! *mutex*))

(define (pre-expand! x)
   (define (pre-expand!-inner x)
      (let loop ((x x)
		 (pre-expanders *pre-expanders*))
	 (if (null? pre-expanders)
	     x
	     (loop ((car pre-expanders) x)
		   (cdr pre-expanders)))))
   (mutex-lock! *mutex*)
   (let ((res (pre-expand!-inner x)))
      (mutex-unlock! *mutex*)
      res))

(define (my-expand x)
   (verbose "expanding")
   (scheme2js-initial-expander x scheme2js-initial-expander (make-eq-hashtable)))

;; macros can not be global variable. (Parallel compilation could
;; yield bad results).
(define (scheme2js-initial-expander x e macros-ht)
   (let ((e1 (cond
		((symbol? x) symbol-expander)
		((not (pair? x)) identify-expander)
		((symbol? (car x))
		 (cond
		    ;; user-defined macros win over compiler-macros
		    ((hashtable-get macros-ht (car x))
		     => (lambda (macro-e)
			   macro-e))
		    ;; compiler-macros
		    ((expander (car x))
		     => (lambda (expander)
			   expander))
		    (else
		     application-expander)))
		(else
		 application-expander)))
	 (pre-expanded-x (pre-expand! x)))
      (e1 pre-expanded-x e macros-ht)))

(define (symbol-expander x e macros-ht)
   x)
	     
(define (identify-expander x e macros-ht) x)

(define (application-expander x e macros-ht)
   (map! (lambda (y) (e y e macros-ht)) x))

;; compiler expanders are shared by all parallel compilations.
(define *expanders* (make-eq-hashtable))

(define (expander? id)
   (and (hashtable-get *expanders* id) #t))

(define (expander id)
   (hashtable-get *expanders* id))

(define (install-expander! id e)
   (hashtable-put! *expanders* id e))
