(module expanders
   (import expand))

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
			  (error #f "Invalid define-form: " x)))))

(install-expander! 'or
		   (lambda (x e)
		      (match-case x
			 ((?-) #f)
			 ((?- . ?tests)
			  (e (if (null? (cdr tests)) ; last element
				 (car tests)
				 (let ((tmp-var (gensym 'tmp)))
				    `(let ((,tmp-var ,(car tests)))
					(if ,tmp-var
					    ,tmp-var
					    (or ,@(cdr tests))))))
			     e)))))

(install-expander! 'and
		   (lambda (x e)
		      (match-case x
			 ((?-) #t)
			 ((?- . ?tests)
			  (e (if (null? (cdr tests)) ; last element
				 (car tests)
				 `(if ,(car tests)
				      (and ,@(cdr tests))
				      #f))
			     e)))))

(install-expander! 'do
		   (lambda (x e)
		      (match-case x
			 ((?- ?bindings (?test . ?finally) . ?commands)
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
				e))))))
							  
(install-expander! 'quasiquote
		   (lambda (x e)
		      (e (match-case (cadr x)
			    (((unquote-splicing ?unquoted-list) . ?rest)
			     `(append ,unquoted-list
				      ,(list 'quasiquote rest)))
			    ((unquote ?datum)
			     datum)
			    ((?y . ?z)
			     `(cons ,(list 'quasiquote y) ,(list 'quasiquote z)))
			    (#(???-)
			     `(list->vector ,(list 'quasiquote (vector->list (cadr x)))))
			    (?- ;; atom
			     `(quote ,(cadr x))))
			 e)))

(install-expander! 'cond
		   (lambda (x e)
		      (e (match-case x
			    ((?-) #unspecified)
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

;; not needed anymore, as we have a 'case'-node now
'(install-expander! 'case
		   (lambda (x e)
		      (e (let ((key (gensym 'key))
			       (key-expr (cadr x))
			       (clauses (cddr x)))
			    `(let ((,key ,key-expr))
				,(let loop ((clauses clauses))
				    (match-case clauses
				       (() #unspecified)
				       (((else . ?alternate))
					`(begin ,@alternate))
				       (((?data . ?consequent) . ?rest)
					`(if (or ,@(map
						    (lambda (datum)
						       `(eqv? ,(list 'quote
								    datum)
							      ,key))
							data))
					     (begin ,@consequent)
					     ,(loop rest)))))))
			 e)))

;; transform let* into nested lets
(install-expander! 'let*
		   (lambda (x e)
		      (let ((bindings (cadr x)))
			 (e `(let (,(car bindings))
				,@(if (null? (cdr bindings))
				      (cddr x) ;; body
				      `((let* ,(cdr bindings)
					   ,@(cddr x)))))
			    e))))
		      
(define (expand-named-let expr)
   (let* ((loop-name (cadr expr))
	  (binding-clauses (caddr expr))
	  (body (cdddr expr))

	  (vars (map car binding-clauses))
	  (init-values (map cadr binding-clauses)))
      ;; correct version would be the following expansion
      `((letrec ((,loop-name (lambda ,vars ,@body)))
	   ,loop-name)
	,@init-values)
      ;; this version is however more efficient:
      `(letrec ((,loop-name (lambda ,vars ,@body)))
	  (,loop-name ,@init-values))))

(define (expand-let x e)
   (let* ((bindings (cadr x))
	  (body (cddr x)))
      `(let ,(map (lambda (binding)
		     (list (e (car binding) e)
			   (e (cadr binding) e)))
		  bindings)
	  ,@(map (lambda (y) (e y e)) body))))

(install-expander! 'let ;; named let
		   (lambda (x e)
		      (if (symbol? (cadr x))
			  (e (expand-named-let x) e)
			  (expand-let x e))))

(install-expander! 'define-struct
 (lambda (x e)
    (let* ((name (cadr x))
	   (fields (map (lambda (f)
			   (if (pair? f) (car f) f))
			(cddr x)))
	   (field-getters (map (lambda (field)
				  (symbol-append name '- field))
			       fields))
	   (field-setters (map (lambda (field)
				  (symbol-append name '- field '-set!))
			       fields))
	   (defaults (map (lambda (f)
			     (if (pair? f) (cadr f) #unspecified))
			  (cddr x)))
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
		  fields
		  field-getters
		  field-setters)))))

(install-expander! 'delay
		   (lambda (x e)
		      (e `(make-promise (lambda () ,@(cdr x))) e)))
