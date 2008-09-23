(module expand
   (import verbose
	   tools)
   (export (my-expand x)
	   (install-expander! id e)
	   ;; priority: lower -> later
	   (add-pre-expand! priority::bint f::procedure)
	   (pre-expand! x)
	   (identify-expander x e macros-ht))
   (eval (export add-pre-expand!)))

(define *pre-expanders* '())

(define *mutex* (make-mutex))

;; pre-expander is shared by all parallel computations.
(define (add-pre-expand! priority f)
   (define (insert L priority f)
      (cond
	 ((null? L)
	  (list (cons priority f)))
	 ((>= priority (caar L))
	  (cons (cons priority f) L))
	 (else
	  (cons (car L) (insert (cdr L) priority f)))))
   
   (mutex-lock! *mutex*)
   (set! *pre-expanders* (insert *pre-expanders* priority f))
   (mutex-unlock! *mutex*))

(define (pre-expand! x)
   (define (pre-expand!-inner x)
      (let loop ((x x)
		 (pre-expanders *pre-expanders*))
	 (if (null? pre-expanders)
	     x
	     (loop ((cdar pre-expanders) x)
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
(define *expanders* '())

(define (expander? id)
   (and (assq id *expanders*) #t))

(define (expander id)
   (let ((tmp (assq id *expanders*)))
      (and tmp
	   (cdr tmp))))

(define (install-expander! id e)
   (cons-set! *expanders* (cons id e)))
