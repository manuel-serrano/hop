(module dollar-escape
   (library scheme2js))

;; ===========================================================================
;; modify scheme2js expander, so we recognize '$'escapes.
;; ===========================================================================

(define (starts-with-dollar? id)
   (and (symbol? id)
	(let ((str (symbol->string id)))
	   (char=? #\$ (string-ref str 0)))))

(define (strip-dollar s)
   (let ((str (symbol->string s)))
      (string->symbol (substring str 1 (string-length str)))))

(define (unhop x)
   (cond
      ; $var
      ((and (symbol? x)
	    (starts-with-dollar? x)
	    (not (eq? x '$)))
       `(pragma ,(symbol->string x)))
      ; '(...) `(...)
      ((and (pair? x)
	    (or (eq? (car x) 'quote)
		(eq? (car x) 'quasiquote)))
       x)
      ; (... $ (...) ....)
      ((pair? x)
       (let loop ((l x))
	  (cond
	     ((or (null? l)
		  (not (pair? l)))
	      x)
	     ((and (eq? (car l) '$)
		   (pair? (cdr l))
		   (pair? (cadr l)))
	      (set-car! l `(pragma ,(with-output-to-string
				       (lambda ()
					  (write '$)
					  (write (cadr l))))))
	      (set-cdr! l (cddr l))
	      (loop (cdr l)))
	     (else
	      (loop (cdr l))))))
      (else
       x)))

(let ((old-initial-expander *scheme2js-initial-expander*))
   (define (new-initial-expander x e::procedure)
      (let ((x-unhopped (unhop x)))
	 (old-initial-expander x-unhopped e)))
   (set! *scheme2js-initial-expander* new-initial-expander))
