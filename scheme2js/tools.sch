(define (make-eq-hashtable)
   (make-hashtable #unspecified #unspecified eq?))

(define (eq-hashtable-clone ht)
   (let ((cloned-ht (make-eq-hashtable)))
      (hashtable-for-each ht
			  (lambda (key val)
			     (hashtable-put! cloned-ht key val)))
      cloned-ht))

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
