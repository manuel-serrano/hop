(module expand
   (import verbose)
   (export (my-expand x)
	   (install-expander! id e)
	   (add-pre-expand! f::procedure)
	   (pre-expand! x))
   (eval (export add-pre-expand!)))

(define *pre-expanders* '())

(define *mutex* (make-mutex))

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
   (scheme2js-initial-expander x scheme2js-initial-expander))

(define (scheme2js-initial-expander x e)
   (let ((e1 (cond
		((symbol? x) symbol-expander)
		((not (pair? x)) identify-expander)
		((symbol? (car x))
		 (if (expander? (car x))
		     (expander (car x))
		     application-expander))
		(else
		 application-expander)))
	 (pre-expanded-x (pre-expand! x)))
      (e1 pre-expanded-x e)))

(define (symbol-expander x e)
   x)
	     
(define (identify-expander x e) x)

(define (application-expander x e)
   (map! (lambda (y) (e y e)) x))

(define *expander-list* '())

(define (expander? id)
   (pair? (assq id *expander-list*)))

(define (expander id)
   (cdr (assq id *expander-list*)))

(define (install-expander! id e)
   (set! *expander-list* (cons (cons id e)
			       *expander-list*)))

;; don't go into quotes.
(install-expander! 'quote identify-expander)

(define (pair-map f p)
   (cond
      ((null? p) '())
      ((pair? p) (cons (f (car p))
		       (pair-map f (cdr p))))
      (else (f p))))

(define (pair-map! f p)
   (let loop ((p p))
      (cond
	 ((null? p) 'done)
	 ((pair? p) (set-car! p (f (car p)))
		    (if (pair? (cdr p))
			(pair-map! f (cdr p))
			(set-cdr! p (f (cdr p)))))
	 (else (error "pair-map! (expand)"
		      "pair-map! should never encounter atom"
		      p))))
   p)

;; works for (define (f ...) ..) and (lambda (...) ...)
(define (formals-aware-expander x e)
   (if (pair? (cadr x))
       (let ((formals-pair (cdr x)))
	  (set-car! formals-pair
		    (pair-map! (lambda (y) (e y e)) (car formals-pair)))
	  (set-cdr! formals-pair (map! (lambda (y) (e y e))
				       (cdr formals-pair)))
	  x)
       (application-expander x e)))

(install-expander! 'lambda formals-aware-expander)
(install-expander! 'bind-exit formals-aware-expander)

(install-expander! 'define-expander
		   (lambda (x e)
		      (let ((id (cadr x))
			    (expander (caddr x)))
			 (install-expander! id (eval expander))
			 #unspecified)))

(install-expander! 'define-macro
		   (lambda (x e)
		      (match-case x
			 ((?- (?name . ?args) . ?body)
			  (install-expander! name
					     (eval `(lambda (x e)
						       (e (let ,(destructure args '(cdr x) '())
							     ,@body)
							  e)))))
			 (else
			  (error "define-macro" "Illegal 'define-macro' syntax" x)))))

(define (destructure pat arg bindings)
   (cond
      ((null? pat) bindings)
      ((symbol? pat) `((,pat ,arg) ,@bindings))
      ((pair? pat)
       (destructure (car pat)
		    `(car ,arg)
		    (destructure (cdr pat) `(cdr ,arg) bindings)))))
