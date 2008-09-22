(module tools
   (export (inline make-eq-hashtable #!optional (size #unspecified))
	   (eq-hashtable-clone ht)
	   (hashtable-append! ht1 ht2)
	   (macro begin0)
	   (macro cons-set!)
	   (macro cp-filter)
	   (macro for)))

(define-inline (make-eq-hashtable #!optional (size #unspecified))
   (make-hashtable size #unspecified eq?))

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

(define-macro (for i from to . Lbody)
   (let ((loop (gensym 'loop))
	 (to-tmp (gensym 'to)))
      `(let ((,to-tmp ,to))
	  (let ,loop ((,i ,from))
	       (when (< ,i ,to-tmp)
		  ,@Lbody
		  (,loop (+fx ,i 1)))))))
