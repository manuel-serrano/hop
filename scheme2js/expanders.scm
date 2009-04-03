(module expanders
   (import expand
	   error
	   (runtime-ref pobject-conv)))

(define (emap1 f orig-L)
   (let loop ((L orig-L)
	      (rev-res '()))
      (cond
	 ((null? L)
	  (reverse! rev-res))
	 ((epair? L)
	  (loop (cdr L)
		(econs (f (car L))
		       rev-res
		       (cer L))))
	 ((pair? L)
	  (loop (cdr L)
		(cons (f (car L))
		      rev-res)))
	 (else
	  (scheme2js-error
	   "expander"
	   "not a list"
	   orig-L
	   orig-L)))))
     
;; don't go into quotes.
(install-expander! 'quote identity-expander)

;; lambda.
(install-expander!
 'lambda
 (lambda (x e)
    (match-case x
       ((?- ?formal ?- . (? list?))
	;; do not expand formal
	(set-cdr! (cdr x) (map! (lambda (y) (e y e))
				(cddr x))))
       (else
	(scheme2js-error "lambda-expand"
			 "bad 'lambda'-form"
			 x x)))
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
 (lambda (x e)
    (match-case x
       ((?- (?name . ?args) ?e0 . ?body)
	(let ((ht (module-macro-ht)))
	   (hashtable-put! ht
			   name
			   (lazy-macro x ht))
	   #unspecified))
       (else
	(scheme2js-error "define-macro"
			 "Illegal 'define-macro' syntax"
			 x x)))))

(install-expander! 'when
		   (lambda (x e)
		      (match-case x
			 ((?- ?test . ?body)
			  (e
			   `(if ,test (begin ,@body) #f)
			   e))
			 (else
			  (scheme2js-error "when-expand"
					   "Invalid 'when' form"
					   x x)))))

(install-expander! 'unless
		   (lambda (x e)
		      (match-case x
			 ((?- ?test . ?body)
			  (e
			   `(if ,test #f (begin ,@body))
			   e))
			 (else
			  (scheme2js-error "unless-expand"
					   "Invalid 'unless' form"
					   x x)))))

(install-expander! 'define
		   (lambda (x e)
		      (match-case x
			 ((?- (?fun . ?formals) . ?body)
			  (e
			   `(define ,fun (lambda ,formals ,@body))
			   e))
			 ((?- ?var ?val)
			  `(define ,(e var e) ,(e val e)))
			 (else
			  (scheme2js-error "define-expand"
					   "Invalid 'define'-form"
					   x x)))))

(install-expander! 'or
		   (lambda (x e)
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
			     e))
			 (else
			  (scheme2js-error "or-expand"
					   "Invalid 'or'-form"
					   x x)))))

(install-expander! 'and
		   (lambda (x e)
		      (match-case x
			 ((?-) #t)
			 ((?- . (and (? pair?) ?tests))
			  (e (if (null? (cdr tests)) ; last element
				 (car tests)
				 `(if ,(car tests)
				      (and ,@(cdr tests))
				      #f))
			     e))
			 (else
			  (scheme2js-error "and-expand"
					   "Invalid 'and'-form"
					   x x)))))

(install-expander!
 'do
 (lambda (x e)
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
	   (scheme2js-error "do-expand" "Invalid 'do'-form" x x))
	(let ((loop (gensym 'doloop)))
	   (e `(let ,loop ,(emap1 (lambda (binding)
				     (cond
					((and (epair? binding)
					      (epair? (cdr binding)))
					 (econs (car binding)
						(econs (cadr binding)
						       '()
						       (cer (cdr binding)))
						(cer binding)))
					((and (pair? binding)
					      (pair? (cdr binding)))
					 (list (car binding) (cadr binding)))
					(else
					 (scheme2js-error
					  "do-expand"
					  "invalid binding in 'do' form"
					  binding
					  binding))))
				  bindings)
		    (if ,test
			(begin ,@finally)
			(begin
			   ,@commands
			   (,loop
			    ,@(emap1 (lambda (bind)
					(if (null? (cddr bind))
					    (car bind)
					    (caddr bind)))
				     bindings)))))
	      e)))
       (else
	(scheme2js-error "do-expand" "Invalid 'do'-form" x x)))))

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
		 (scheme2js-error "quasiquote-expander"
				  "Illegal Unquote form"
				  x x))
		((?fst . ?-)
		 (set-car! x (quasiquote-expand! fst level))
		 (loop (cdr x)
		       x))
		(?- ;; atom or empty list
		 (set-cdr! last-pair (list (quasiquote-expand! x level))))))
	  head))))

(install-expander! 'quasiquote
		   (lambda (x e)
		      (if (or (null? (cdr x))
			      (not (pair? (cdr x)))
			      (not (null? (cddr x))))
			  (scheme2js-error "quasiquote-expander"
					   "Illegal Quasiquote form"
					   x x))
		      (e (quasiquote-expand! (cadr x) 0) e)))

(install-expander! 'cond
		   (lambda (x e)
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
			 e)))

;; transform let* into nested lets
(install-expander! 'let*
		   (lambda (x e)
		      (match-case x
			 ((?- ((?v ?val) . ?bindings) . ?body)
			  (e `(let ((,v ,val))
				 ,@(if (null? bindings)
				       body
				       `((let* ,bindings
					    ,@body))))
			     e))
			 (else
			  (scheme2js-error "let*-expand"
					   "Invalid 'let*'-form"
					   x x)))))

(define (init-values bindings)
   (let loop ((bs bindings)
	      (rev-res '()))
      (if (null? bs)
	  (reverse! rev-res)
	  (let ((binding (car bs)))
	     (if (epair? (cdr binding))
		 (loop (cdr bs)
		       (econs (cadr binding)
			      rev-res
			      (cer (cdr binding))))
		 (loop (cdr bs)
		       (cons (cadr binding) rev-res)))))))

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
	 (scheme2js-error "named-let expand"
			  "Invalid named-let form"
			  expr expr))
      (let ((vars (emap1 car binding-clauses))
	    (init-vals (init-values binding-clauses)))
      ;; correct version would be the following expansion
;      `((letrec ((,loop-name (lambda ,vars ,@body)))
;	   ,loop-name)
;	,@init-values)
      ;; this version is however more efficient:
	 `(letrec ((,loop-name (lambda ,vars ,@body)))
	     (,loop-name ,@init-vals)))))

(define (expand-let x e)
   ;; we know it's of form (?- (? list?) . ?-)
   (let* ((bindings (cadr x))
	  (body (cddr x)))
      (unless (every? (lambda (b)
			 (and (pair? b)
			      (pair? (cdr b))
			      (null? (cddr b))
			      (symbol? (car b))))
		      bindings)
	 (scheme2js-error "let expand"
			  "Invalid 'let' form"
			  x x))
      (for-each (lambda (binding)
		   (set-car! binding (e (car binding) e))
		   (set-car! (cdr binding)
			     (e (cadr binding) e)))
		bindings)
		   
      `(let ,bindings
	  ,@(map! (lambda (y) (e y e)) body))))

(install-expander! 'let ;; named let
		   (lambda (x e)
		      (match-case x
			 ((?- (? symbol?) (? list?) . ?-)
			  (e (expand-named-let x) e))
			 ((?- (? list?) . ?-)
			  (expand-let x e))
			 (else
			  (scheme2js-error "let expand" "Illegal 'let' form"
					   x x)))))

(install-expander! 'define-struct
 (lambda (x e)
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
	   (scheme2js-error "define-struct expand"
			    "Illegal 'define-struct' form"
			    x x))
	(let* ((field-ids (emap1 (lambda (f)
				    (if (pair? f) (car f) f))
				 fields))
	       (field-getters (emap1 (lambda (field)
					(symbol-append name '- field))
				     field-ids))
	       (field-setters (emap1 (lambda (field)
					(symbol-append name '- field '-set!))
				     field-ids))
	       (defaults (emap1 (lambda (f)
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
	(scheme2js-error "define-struct expand"
			 "Illegal 'define-struct' form"
			 x x)))))

(install-expander! 'delay
		   (lambda (x e)
		      (match-case x
			 ((?- ?exp)
			  (e `(,(runtime-ref 'make-promise)
			       (lambda () ,exp)) e))
			 (else
			  (scheme2js-error "delay expand"
					   "Illegal 'delay' form"
					   x x)))))

(install-expander!
 'bind-exit
 (lambda (x e)
    (match-case x
       ((?- (?escape) ?expr . ?Lrest)
	(e
	 `(,(runtime-ref 'bind-exit-lambda)
	   (lambda (,escape)
	      ,expr
	      ,@Lrest))
	 e))
       (else
	(scheme2js-error "bind-exit" "Invalid 'bind-exit' form" x x)))))

(install-expander!
 'with-handler
 (lambda (x e)
    (match-case x
       ((?- ?handler ?expr . ?Lrest)
	(e
	 `(,(runtime-ref 'with-handler-lambda)
	   ,handler
	   (lambda ()
	      ,expr
	      ,@Lrest))
	 e))
       (else
	(scheme2js-error "with-handler" "Invalid 'with-handler' form" x x)))))

(define (receive-expander x e)
   (match-case x
      ((?- ?vars ?producer ?expr . ?Lrest)
       (e
	`(,(runtime-ref 'call-with-values)
	  ,producer
	  (lambda ,vars
	     ,expr
	     ,@Lrest))
	e))
      (else
       (scheme2js-error "receive/multiple-value-bind"
	      "Invalid 'receive' form"
	      x x))))

(install-expander! 'receive receive-expander)
(install-expander! 'multiple-value-bind receive-expander)

(install-expander! '@ (lambda (x e) x))
