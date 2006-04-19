(module dot-expand
   (import verbose
	   expand)
   (export *direct-js-object-access*))

(define *direct-js-object-access* #t)

(define (split-dot id)
   (let ((splitted (map! string->symbol
			 (string-split (symbol->string id) "."))))
      splitted))

(define (split-multi-dot id)
   (let ((res (split-dot id)))
      (and (not (null? (cdr res))) ;; more than one element
	   res)))

;; but not at start.
(define (dotted-symbol? id)
   (and (symbol? id)
	(>fx (string-index (symbol->string id) #\.) 0)))

(define (starts-with-dot? id)
   (and (symbol? id)
	(let ((str (symbol->string id)))
	   (char=? #\. (string-ref str 0)))))

(define (get-dot-expand splitted)
   (let loop ((res (car splitted))
	      (splitted (cdr splitted)))
      (if (null? splitted)
	  res
	  (loop `(js-field ,res ',(car splitted))
		(cdr splitted)))))

(define (set!-dot-expand splitted val-L)
   (let loop ((res (car splitted))
	      (splitted (cdr splitted)))
      (if (null? (cdr splitted))
	  `(js-field-set! ,res ',(car splitted) ,@val-L)
	  (loop `(js-field ,res ',(car splitted))
		(cdr splitted)))))

(define (delete!-dot-expand splitted)
   (let loop ((res (car splitted))
	      (splitted (cdr splitted)))
      (if (null? (cdr splitted))
	  `(js-field-delete! ,res ',(car splitted))
	  (loop `(js-field ,res ',(car splitted))
		(cdr splitted)))))

(define (expand-indirect-accesses! x)
   (if (and (pair? x)
	    (pair? (cdr x)))
       (begin
	  (if (and (pair? (car x))
		   (starts-with-dot? (cadr x)))
	      (begin
		 (set-car! x
			   (get-dot-expand (cons (car x)
						 (split-dot (cadr x)))))
		 (set-cdr! x (cddr x))))
	  (expand-indirect-accesses! (cdr x)))))

(define (split-last p)
   (let loop ((res '())
	      (p p))
      (if (null? (cdr p))
	  (values (reverse! res) (car p))
	  (loop (cons (car p) res) (cdr p)))))

(define (undot x)
   (match-case x
      ((? symbol?)
       (cond
	  ((split-multi-dot x) => (lambda (splitted)
				     (get-dot-expand splitted)))
	  (else x)))
      ;(set! x.y ...)
      ((set! (and (? dotted-symbol?) ?x-dot-y) . ?val-L)
       (let ((splitted (split-dot x-dot-y)))
	  (set!-dot-expand splitted val-L)))
      ;(set! (get-x).y ...)
      ((set! (and (? pair?) ?p) (and (? starts-with-dot?) ?dot-y) . ?val-L)
       (let ((splitted (cons p (split-dot dot-y))))
	  (set!-dot-expand splitted val-L)))
      ;(delete! x.y)
      ((delete! (and (? dotted-symbol?) ?x-dot-y))
       (let ((splitted (split-dot x-dot-y)))
	  (delete!-dot-expand splitted)))
      ;(delete! (get-x).y)
      ((delete! (and (? pair?) ?p) (and (? starts-with-dot?) ?dot-y))
       (let ((splitted (cons p (split-dot dot-y))))
	  (delete!-dot-expand splitted)))
      ;(x.f ...)
      (((and (? dotted-symbol?) ?x-dot-f) . ?args)
       (multiple-value-bind (o f)
	  (split-last (split-dot x-dot-f))
	  `(js-method-call ,(get-dot-expand o) ',f ,@args)))
      ;((get-x).f ...)
      (((and (? pair?) ?p) (and (? starts-with-dot?) ?f) . ?args)
       (let* ((o (gensym 'o))
	      (o-dot-f (symbol-append o f)))
	  `(let ((,o ,p))
	      (,o-dot-f ,@args))))
      ;(quote ...)
      (((kwote quote) ???-)
       x)
      ;(quasiquote ...)
      (((kwote quasiquote) ???-)
       x)
      ;(let (bindings) ...)
      ((let (and (? pair?) ?bindings) . ?body)
       (for-each expand-indirect-accesses! bindings)
       x)
      (else
       (expand-indirect-accesses! x)
       x)))

(add-pre-expand! (lambda (x)
		    (if *direct-js-object-access*
			(undot x)
			x)))
