(module expanders
   (import expand
	   (runtime-ref pobject-conv)))

;; don't go into quotes.
(install-expander! 'quote identify-expander)

;; lambda.
(install-expander!
 'lambda
 (lambda (x e macros-hts)
    (match-case x
       ((?- ?formal ?- . (? list?))
	;; ignore formal
	(set-cdr! (cdr x) (map! (lambda (y) (e y e macros-hts))
				(cddr x))))
       (else
	(error "lambda-expand" "bad 'lambda'-form" x)))
    x))

;; not used.
; (install-expander!
;  'define-expander
;  (lambda (x e)
;     (let ((id (cadr x))
; 	  (expander (caddr x)))
;        (install-expander! id (eval expander))
;        #unspecified)))

(install-expander!
 'define-macro
 (lambda (x e macros-hts)
    (match-case x
       ((?- (?name . ?args) ?e0 . ?body)
	(let ((ht (car macros-hts)))
	   (hashtable-put! ht
			   name
			   (lazy-macro x ht))
	   #unspecified))
       (else
	(error "define-macro" "Illegal 'define-macro' syntax" x)))))

(install-expander! 'when
		   (lambda (x e macros-hts)
		      (match-case x
			 ((?- ?test . ?body)
			  (e
			   `(if ,test (begin ,@body) #f)
			   e macros-hts))
			 (else
			  (error "when-expand" "Invalid when form: " x)))))

(install-expander! 'unless
		   (lambda (x e macros-hts)
		      (match-case x
			 ((?- ?test . ?body)
			  (e
			   `(if ,test #f (begin ,@body))
			   e macros-hts))
			 (else
			  (error "unless-expand" "Invalid unless form: " x)))))

(install-expander! 'define
		   (lambda (x e macros-hts)
		      (match-case x
			 ((?- (?fun . ?formals) . ?body)
			  (e
			   `(define ,fun (lambda ,formals ,@body))
			   e macros-hts))
			 ((?- ?var ?val)
			  `(define ,(e var e macros-hts) ,(e val e macros-hts)))
			 (else
			  (error "define-expand" "Invalid define-form: " x)))))

(install-expander! 'or
		   (lambda (x e macros-hts)
		      (match-case x
			 ((?-) #f)
			 ((?- . (and (? pair?) ?tests))
			  (e (if (null? (cdr tests)) ; last element
				 (car tests)
				 (let ((tmp-var (gensym 'tmp)))
				    `(let ((,tmp-var ,(car tests)))
					(if ,tmp-var
					    ,tmp-var
					    (or ,@(cdr tests))))))
			     e macros-hts))
			 (else
			  (error "or-expand" "Invalid 'or'-form" x)))))

(install-expander! 'and
		   (lambda (x e macros-hts)
		      (match-case x
			 ((?-) #t)
			 ((?- . (and (? pair?) ?tests))
			  (e (if (null? (cdr tests)) ; last element
				 (car tests)
				 `(if ,(car tests)
				      (and ,@(cdr tests))
				      #f))
			     e macros-hts))
			 (else
			  (error "and-expand" "Invalid 'and'-form" x)))))

(install-expander!
 'do
 (lambda (x e macros-hts)
    (match-case x
       ((?- ?bindings (?test . ?finally) . ?commands)
	(unless (and (list? finally)
		     (list? commands)
		     (list? bindings)
		     (every? (lambda (b)
				(and (pair? b)
				     (symbol? (car b))
				     (pair? (cdr b))
				     (or (null? (cddr b))
					 (and (pair? (cddr b))
					      (null? (cdddr b))))))
			     bindings))
	   (error "do-expand" "Invalid 'do'-form" x))
	(let ((loop (gensym 'doloop)))
	   (e `(let ,loop ,(map (lambda (binding)
				   (list (car binding)
					 (cadr binding)))
				bindings)
		    (if ,test
			(begin ,@finally)
			(begin
			   ,@commands
			   (,loop
			    ,@(map (lambda (bind)
				      (if (null? (cddr bind))
					  (car bind)
					  (caddr bind)))
				   bindings)))))
	      e macros-hts)))
       (else
	(error "do-expand" "Invalid 'do'-form" x)))))

(define (quasiquote-expand! x level)
   ;; conservative: if the function returns #f, then there's
   ;; no escape. if #t, there might be one.
   (define (pair-contains-escapes? p)
      (let loop ((p p))
	 (cond
	    ((or (null? p)
		 (not (pair? p)))
	     #f)
	    ((or (pair? (car p))
		 (vector? (car p))
		 (eq? (car p) 'unquote))
	     #t)
	    (else
	     (loop (cdr p))))))

   ;; conservative: if the function returns #f, then there's
   ;; no escape. if #t, there might be one.
   (define (vector-contains-escapes? v)
      (let loop ((i 0))
	 (if (>= i (vector-length v))
	     #f
	     (let ((e (vector-ref v i)))
		(or (pair? e)
		    (vector? e)
		    (loop (+ i 1)))))))
   (cond
      ((and (vector? x)
	    (not (vector-contains-escapes? x)))
       `(quote ,x))
      ((vector? x) ;; transform at runtime from list to vector.
       `(list->vector ,(quasiquote-expand! (vector->list x) level)))
      ((or (number? x)
	   (string? x)
	   (char? x))
       x) ;; numbers, strings and chars don't need to be quoted
      ((not (pair? x)) ;; atom or null
       `(quote ,x))

      ;; a pair:
      ((eq? (car x) 'quasiquote) ;; nested quasiquotes.
       `(list 'quasiquote ,(quasiquote-expand! (cadr x) (+ level 1))))
      ((eq? (car x) 'unquote) ;; unquote
       (if (= level 0)
	   (cadr x)
	   `(list (quote ,(car x)) ,(quasiquote-expand! (cadr x) (- level 1)))))
      ((not (pair-contains-escapes? x))
       (list 'quote x))
      (else
       (let ((head (cons 'cons* x)))
	  (let loop ((x x)
		     (last-pair head))
	     (match-case x
		(((unquote-splicing ?unquoted-list))
		 (if (= level 0)
		     (set-cdr! last-pair (list unquoted-list))
		     (begin
			(set-car! x (quasiquote-expand! (car x) (- level 1)))
			(loop (cdr x) x))))
		(((unquote-splicing ?unquoted-list) . ?rest)
		 (if (= level 0)
		     (set-cdr! last-pair
			       `((append ,unquoted-list
					 ,(quasiquote-expand! rest level))))
		     (begin
			(set-car! x (quasiquote-expand! (car x) (- level 1)))
			(loop (cdr x) x))))
		((unquote . ?rest)
		 (set-car! x (cons 'unquote-splicing rest))
		 (set-cdr! x '())
		 (loop x last-pair))
		((unquote)
		 (error "quasiquote-expander" "Illegal Unquote form" x))
		((?fst . ?-)
		 (set-car! x (quasiquote-expand! fst level))
		 (loop (cdr x)
		       x))
		(?- ;; atom or empty list
		 (set-cdr! last-pair (list (quasiquote-expand! x level))))))
	  head))))

(install-expander! 'quasiquote
		   (lambda (x e macros-hts)
		      (if (or (null? (cdr x))
			      (not (pair? (cdr x)))
			      (not (null? (cddr x))))
			  (error "quasiquote-expander"
				 "Illegal Quasiquote form"
				 x))
		      (e (quasiquote-expand! (cadr x) 0) e macros-hts)))

(install-expander! 'cond
		   (lambda (x e macros-hts)
		      (e (match-case x
			    ((?-) #f)
			    ((?- (else . ?alternate))
			     `(begin ,@alternate))
			    ((?- (?test) . ?rest)
			     `(or ,test (cond ,@rest)))
			    ((?- (?test => ?consequent-fun) . ?rest)
			     (let ((tmp-var (gensym 'tmp)))
				`(let ((,tmp-var ,test))
				    (if ,tmp-var
					(,consequent-fun ,tmp-var)
					(cond ,@rest)))))
			    ((?- (?test . ?consequent) . ?rest)
			     `(if ,test
				  (begin ,@consequent)
				  (cond ,@rest))))
			 e macros-hts)))

;; transform let* into nested lets
(install-expander! 'let*
		   (lambda (x e macros-hts)
		      (match-case x
			 ((?- ((?v ?val) . ?bindings) . ?body)
			  (e `(let ((,v ,val))
				 ,@(if (null? bindings)
				       body
				       `((let* ,bindings
					    ,@body))))
			     e macros-hts))
			 (else
			  (error "let*-expand"
				 "Invalid 'let*'-form"
				 x)))))
		      
(define (expand-named-let expr)
   ;; we know it's of form (?- (? symbol?) (? list?) . ?-)
   (let* ((loop-name (cadr expr))
	  (binding-clauses (caddr expr))
	  (body (cdddr expr)))
      (unless (every? (lambda (b)
			 (and (pair? b)
			      (pair? (cdr b))
			      (null? (cddr b))
			      (symbol? (car b))))
		      binding-clauses)
	 (error "named-let expand"
		"Invalid named-let form"
		expr))
      (let ((vars (map car binding-clauses))
	    (init-values (map cadr binding-clauses)))
      ;; correct version would be the following expansion
;      `((letrec ((,loop-name (lambda ,vars ,@body)))
;	   ,loop-name)
;	,@init-values)
      ;; this version is however more efficient:
	 `(letrec ((,loop-name (lambda ,vars ,@body)))
	     (,loop-name ,@init-values)))))

(define (expand-let x e macros-hts)
   ;; we know it's of form (?- (? list?) . ?-)
   (let* ((bindings (cadr x))
	  (body (cddr x)))
      (unless (every? (lambda (b)
			 (and (pair? b)
			      (pair? (cdr b))
			      (null? (cddr b))
			      (symbol? (car b))))
		      bindings)
	 (error "let expand"
		"Invalid 'let' form"
		x))

      `(let ,(map (lambda (binding)
		     (list (e (car binding) e macros-hts)
			   (e (cadr binding) e macros-hts)))
		  bindings)
	  ,@(map (lambda (y) (e y e macros-hts)) body))))

(install-expander! 'let ;; named let
		   (lambda (x e macros-hts)
		      (match-case x
			 ((?- (? symbol?) (? list?) . ?-)
			  (e (expand-named-let x) e macros-hts))
			 ((?- (? list?) . ?-)
			  (expand-let x e macros-hts))
			 (else
			  (error "let expand" "Illegal form" x)))))

(install-expander! 'define-struct
 (lambda (x e macros-hts)
    (match-case x
       ((?- ?name . ?fields)
	(unless (and (symbol? name)
		     (list? fields)
		     (every (lambda (f)
			       (or (symbol? f)
				   (and (pair? f)
					(symbol? (car f))
					(pair? (cdr f))
					(null? (cddr f)))))
			    fields))
	   (error "define-struct expand"
		  "Illegal 'define-struct' form"
		  x))
	(let* ((field-ids (map (lambda (f)
				  (if (pair? f) (car f) f))
			       fields))
	       (field-getters (map (lambda (field)
				      (symbol-append name '- field))
				   field-ids))
	       (field-setters (map (lambda (field)
				      (symbol-append name '- field '-set!))
				   field-ids))
	       (defaults (map (lambda (f)
				 (if (pair? f) (cadr f) #unspecified))
			      fields))
	       (tmp (gensym)))
	   `(begin
	       (define ,(symbol-append 'make- name)
		  (lambda args (let ((,tmp (make-struct ',name)))
				  ,@(map (lambda (setter default)
					    `(if (null? args)
						 (,setter ,tmp ,default)
						 (begin
						    (,setter ,tmp (car args))
						    (set! args (cdr args)))))
					 field-setters
					 defaults)
				  ,tmp)))
	       ;; alias for make-name
	       (define ,name ,(symbol-append 'make- name))
	       (define ,(symbol-append name '?)
		  (lambda (s) (struct-named? ',name s)))
	       ,@(map (lambda (field getter setter)
			 `(begin
			     (define ,getter
				(lambda (s)
				   (struct-field s
						 ',name
						 ',(symbol-append 'f- field))))
			     (define ,setter
				(lambda (s val)
				   (struct-field-set! s
						      ',name
						      ',(symbol-append 'f- field)
						      val)))))
		      field-ids
		      field-getters
		      field-setters))))
       (else
	(error "define-struct expand"
	       "Illegal 'define-struct' form"
	       x)))))

(install-expander! 'delay
		   (lambda (x e macros-hts)
		      (match-case x
			 ((?- ?exp)
			  (e `(,(runtime-ref 'make-promise)
			       (lambda () ,exp)) e macros-hts))
			 (else
			  (error "delay expand"
				 "Illegal 'delay' form"
				 x)))))

(install-expander!
 'bind-exit
 (lambda (x e macros-hts)
    (match-case x
       ((?- (?escape) ?expr . ?Lrest)
	(e
	 `(,(runtime-ref 'bind-exit-lambda)
	   (lambda (,escape)
	      ,expr
	      ,@Lrest))
	 e macros-hts))
       (else
	(error "bind-exit" "Invalid bind-exit-form: " x)))))

(install-expander!
 'with-handler
 (lambda (x e macros-hts)
    (match-case x
       ((?- ?handler ?expr . ?Lrest)
	(e
	 `(,(runtime-ref 'with-handler-lambda)
	   ,handler
	   (lambda ()
	      ,expr
	      ,@Lrest))
	 e macros-hts))
       (else
	(error "with-handler" "Invalid with-handler-form: " x)))))

(define (receive-expander x e macros-hts)
   (match-case x
      ((?- ?vars ?producer ?expr . ?Lrest)
       (e
	`(,(runtime-ref 'call-with-values)
	  ,producer
	  (lambda ,vars
	     ,expr
	     ,@Lrest))
	e macros-hts))
      (else
       (error "receive/multiple-value-bind"
	      "Invalid form: "
	      x))))

(install-expander! 'receive receive-expander)
(install-expander! 'multiple-value-bind receive-expander)

(install-expander! '@ (lambda (x e macros-hts) x))
