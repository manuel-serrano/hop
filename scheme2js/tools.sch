(define (make-eq-hashtable)
   (make-hashtable #unspecified #unspecified eq?))

(define (eq-hashtable-clone ht)
   (let ((cloned-ht (make-eq-hashtable)))
      (hashtable-for-each ht
			  (lambda (key val)
			     (hashtable-put! cloned-ht key val)))
      cloned-ht))

(define (hashtable-append! ht1 ht2)
   (hashtable-for-each ht2
		       (lambda (key val)
			  (hashtable-put! ht1 key val))))

;; '("a" "b" "c") -> "a,b,c"
(define (separated-list els sep . Ldefault)
   (cond
      ((null? els) (if (null? Ldefault)
		       ""
		       (car Ldefault)))
      ;; last element is verbatim returned
      ((null? (cdr els)) (car els))
      ;; otherwise add "," between elements
      (else (string-append (car els) sep (separated-list (cdr els) sep)))))

(define-macro (p-display p . Largs)
   `(begin
       ,@(map (lambda (arg)
		 `(display ,arg ,p))
	      Largs)))

(define-macro (begin0 . L)
   (let ((fst (gensym 'fst)))
      `(let ((,fst ,(car L)))
	  ,@(cdr L)
	  ,fst)))

(define-macro (cons-set! lvalue val)
   `(set! ,lvalue (cons ,val (or ,lvalue '()))))

(define-macro (cp-filter . L)
   `(map (lambda (x) x)
	 (filter ,@L)))
