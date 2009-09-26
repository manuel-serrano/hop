(module __hopscheme_dollar-escape
   (library scheme2js)
   (import __hopscheme_config))

;; ===========================================================================
;; add scheme2js pre-expander, so we recognize '$'escapes.
;; ===========================================================================

(define (starts-with-dollar? id)
   (and (symbol? id)
	(let ((str (symbol->string id)))
	   (char=? #\$ (string-ref str 0)))))

(define (strip-dollar s)
   (let ((str (symbol->string s)))
      (string->symbol (substring str 1 (string-length str)))))

(define (dollar-eval e)
   (with-handler
      (lambda (e)
	 (if (&error? e)
	     (begin
		(error-notify e)
		#unspecified)
	     (raise e)))
      (*hop-eval* e)))
	 
(define (unhop-list! l)
   (cond
      ((or (null? l)
	   (not (pair? l)))
       l)
      ((and (eq? (car l) '$)
	    (pair? (cdr l))
	    (pair? (cadr l)))
       (if (scheme2js-config 'dollar-eval)
	   (let ((val (dollar-eval (cadr l))))
	      (if (eq? val #unspecified)
		  (unhop-list! (cddr l)) ;; skip '$ and (...)
		  (begin
		     (set-car! l `(pragma ,val))
		     (set-cdr! l (unhop-list! (cddr l)))
		     l)))
	   (begin
	      (set-car! l `(pragma ,(with-output-to-string
				       (lambda ()
					  (write '$)
					  (write (cadr l))))))
	      (set-cdr! l (unhop-list! (cddr l)))
	      l)))
      (else
       (set-cdr! l (unhop-list! (cdr l)))
       l)))
   
(define (unhop x)
   (cond
      ; $var
      ((and (symbol? x)
	    (starts-with-dollar? x)
	    (not (eq? x '$)))
       ;; split dotted notation...
       (let* ((symstr (symbol->string x))
	      (first-dot (string-index symstr ".")))
	  (if (or (not first-dot) (<fx first-dot 0))
	      `(pragma ,(if (scheme2js-config 'dollar-eval)
			    (dollar-eval (strip-dollar x))
			    symstr))
	      `(begin
		  (pragma ,(if (scheme2js-config 'dollar-eval)
			       (dollar-eval
				(string->symbol (substring symstr
							   1 ;; discard $
							   first-dot)))
			       (substring symstr 0 first-dot)))
		  ,(string->symbol (substring symstr
					      first-dot
					      (string-length
					       symstr)))))))
      ; '(...) `(...)
      ((and (pair? x)
	    (or (eq? (car x) 'quote)
		(eq? (car x) 'quasiquote)))
       x)
      ;(let loop (bindings) ...)
      ((and (pair? x)
	    (pair? (cdr x))
	    (pair? (cddr x))
	    (eq? (car x) 'let)
	    (symbol? (cadr x))
	    (list? (cadr x)))
       (let ((bindings (caddr x)))
	  (for-each (lambda (binding)
		       (when (pair? binding)
			  (set-cdr! binding (unhop-list! (cdr binding)))))
		    bindings)
	  x))
      ;(let (bindings) ...)
      ((and (pair? x)
	    (pair? (cdr x))
	    (or (eq? (car x) 'let)
		(eq? (car x) 'let*)
		(eq? (car x) 'letrec))
	    (list? (cadr x)))
       (let ((bindings (cadr x)))
	  (for-each (lambda (binding)
		       (when (pair? binding)
			  (set-cdr! binding (unhop-list! (cdr binding)))))
		    bindings)
	  x))
      ; (... $ (...) ....)
      ((pair? x)
       (unhop-list! x)
       x)
      (else
       x)))

(add-pre-expand! 10 ;; high priority: execute before other expansions.
		    ;; -> $(servic...).f becomes (pragma..).f and not
		    ;;    $ (get-field (servic...) f) 
		 (lambda (x)
		    (unhop x)))
