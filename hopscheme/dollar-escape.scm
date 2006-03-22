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

(define (unhop-list! l)
   (let loop ((l l))
      (cond
	 ((or (null? l)
	      (not (pair? l)))
	  'done)
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
   
(define (unhop x)
   (cond
      ; $var
      ((and (symbol? x)
	    (starts-with-dollar? x)
	    (not (eq? x '$)))
       ;; split dotted notation...
       (let* ((symstr (symbol->string x))
	      (first-dot (string-index symstr ".")))
	  (if (<fx first-dot 0)
	      `(pragma ,symstr)
	      `(begin
		  (pragma ,(substring symstr 0 first-dot))
			  ,(string->symbol (substring symstr
						      first-dot
						      (string-length
						       symstr)))))))
      ; '(...) `(...)
      ((and (pair? x)
	    (or (eq? (car x) 'quote)
		(eq? (car x) 'quasiquote)))
       x)
      ;(let (bindings) ...)
      ((and (pair? x)
	    (eq? (car x) 'let)
	    (pair? (cdr x))
	    (pair? (cadr x)))
       (for-each unhop-list! (cadr x))
       x)
      ; (... $ (...) ....)
      ((pair? x)
       (unhop-list! x)
       x)
      (else
       x)))

(let ((old-initial-expander *scheme2js-initial-expander*))
   (define (new-initial-expander x e::procedure)
      (let ((x-unhopped (unhop x)))
	 (old-initial-expander x-unhopped e)))
   (set! *scheme2js-initial-expander* new-initial-expander))
