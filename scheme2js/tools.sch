(define (make-eq-hashtable)
   (make-hashtable #unspecified #unspecified eq?))

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
