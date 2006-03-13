;; $Id: expand.scm 133 2006-03-08 14:06:31Z flo $
(module expand
   (import verbose)
   (export (my-expand x)
	   (install-expander! id e)
	   *scheme2js-initial-expander*)
   (eval (export *scheme2js-initial-expander*)))

(define (my-expand x)
   (verbose "expanding")
   (*scheme2js-initial-expander* x *scheme2js-initial-expander*))

(define (*scheme2js-initial-expander* x e)
   (let ((e1 (cond
		((symbol? x) symbol-expander)
		((not (pair? x)) identify-expander)
		((symbol? (car x))
		 (if (expander? (car x))
		     (expander (car x))
		     application-expander))
		(else
		 application-expander))))
      (e1 x e)))

(define (symbol-expander x e)
   x)
	     
(define (identify-expander x e) x)

(define (application-expander x e)
   (map (lambda (y) (e y e)) x))

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

;; works for (define (f ...) ..) and (lambda (...) ...)
(define (formals-aware-expander x e)
   (if (pair? (cadr x))
       `(,(car x)
	 ,(pair-map (lambda (y) (e y e)) (cadr x))
	 ,@(map (lambda (y) (e y e)) (cddr x)))
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
