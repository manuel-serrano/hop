(begin
(define (starts-with-dollar? x)
   (and (symbol? id)
	(let ((str (symbol->string id)))
	   (char=? #\. (string-ref str 0)))))

(define (strip-dollar s)
   (let ((str (symbol->string s)))
      (string->symbol (substring str 1 (string-length str)))))

(define (unhop x)
   (match-case x
      ((? symbol?)
       (if (starts-with-dollar? x)
	   `(pragma (symbol->string (strip-dollar x)))
	   x))
      ;(set! x.y ...)
      ((set! (and (? dotted-symbol?) ?x.y) . ?val-L)
       (let ((splitted (split-dot x.y)))
	  (set!-dot-expand splitted val-L)))
      ;(set! (get-x).y ...)
      ((set! (and (? pair?) ?p) (and (? starts-with-dot?) ?.y) . ?val-L)
       (let ((splitted (cons p (split-dot .y))))
	  (set!-dot-expand splitted val-L)))
      (else
       x)))

(let ((old-initial-expander *scheme2js-initial-expander*))
   (define (new-initial-expander x e::procedure)
      (let ((x-unhopped (unhop x)))
	 (old-initial-expander x-unhopped e)))
   (set! *scheme2js-initial-expander* new-initial-expander))
)
