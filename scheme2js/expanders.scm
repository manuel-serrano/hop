(module expanders
   (import expand))

(define (runtime-ref id)
   ;; we use a function to differenciate this construct
   ;; from similar user-constructs.
   (list 'runtime-ref id (lambda () 'runtime-ref)))

;; don't go into quotes.
(install-expander! 'quote identify-expander)

;; lambda.
;; get all args (including var-arg), and declare them as locally defined (ie.
;; add them to the Lenv).
(install-expander!
 'lambda
 (lambda (x e macros-ht)
    ;; ignore formal
    (set-cdr! (cdr x) (map! (lambda (y) (e y e macros-ht))
			    (cddr x)))
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
 (lambda (x e macros-ht)
    (define (destructure pat arg bindings)
       (cond
	  ((null? pat) bindings)
	  ((symbol? pat) `((,pat ,arg) ,@bindings))
	  ((pair? pat)
	   (destructure (car pat)
			`(car ,arg)
			(destructure (cdr pat) `(cdr ,arg) bindings)))))

    (match-case x
       ((?- (?name . ?args) . ?body)
	(hashtable-put!
	 macros-ht
	 name
	 (eval `(lambda (x e macros-ht)
		   (define (deep-copy o)
		      (cond
			 ((epair? o)
			  (econs (deep-copy (car o))
				 (deep-copy (cdr o))
				 (deep-copy (cer o))))
			 ((pair? o)
			  (cons (deep-copy (car o)) (deep-copy (cdr o))))
			 ((vector? o)
			  (copy-vector o (vector-length o)))
			 ((string? o)
			  (string-copy o))
			 (else
			  o)))

		   ;; macros might reference lists twice.
		   ;; by deep-copying the result, we are sure, that
		   ;; we don't share anything.
		   (e (deep-copy
		       (let ,(destructure args '(cdr x) '())
			  ,@body))
		      e macros-ht))))
	#unspecified)
       (else
	(error "define-macro" "Illegal 'define-macro' syntax" x)))))

(install-expander! 'when
		   (lambda (x e macros-ht)
		      (match-case x
			 ((?- ?test . ?body)
			  (e
			   `(if ,test (begin ,@body))
			   e macros-ht))
			 (else
			  (error "when-expand" "Invalid when form: " x)))))

(install-expander! 'unless
		   (lambda (x e macros-ht)
		      (match-case x
			 ((?- ?test . ?body)
			  (e
			   `(if (not ,test) (begin ,@body))
			   e macros-ht))
			 (else
			  (error "unless-expand" "Invalid unless form: " x)))))

(install-expander! 'define
		   (lambda (x e macros-ht)
		      (match-case x
			 ((?- (?fun . ?formals) . ?body)
			  (e
			   `(define ,fun (lambda ,formals ,@body))
			   e macros-ht))
			 ((?- ?var ?val)
			  `(define ,(e var e macros-ht) ,(e val e macros-ht)))
			 (else
			  (error #f "Invalid define-form: " x)))))

(install-expander! 'or
		   (lambda (x e macros-ht)
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
			     e macros-ht)))))

(install-expander! 'and
		   (lambda (x e macros-ht)
		      (match-case x
			 ((?-) #t)
			 ((?- . ?tests)
			  (e (if (null? (cdr tests)) ; last element
				 (car tests)
				 `(if ,(car tests)
				      (and ,@(cdr tests))
				      #f))
			     e macros-ht)))))

(install-expander! 'do
		   (lambda (x e macros-ht)
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
				e macros-ht))))))

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
		   (lambda (x e macros-ht)
		      (if (or (null? (cdr x))
			      (not (pair? (cdr x)))
			      (not (null? (cddr x))))
			  (error "quasiquote-expander"
				 "Illegal Quasiquote form"
				 x))
		      (e (quasiquote-expand! (cadr x) 0) e macros-ht)))

(install-expander! 'cond
		   (lambda (x e macros-ht)
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
			 e macros-ht)))

;; transform let* into nested lets
(install-expander! 'let*
		   (lambda (x e macros-ht)
		      (let ((bindings (cadr x)))
			 (e `(let (,(car bindings))
				,@(if (null? (cdr bindings))
				      (cddr x) ;; body
				      `((let* ,(cdr bindings)
					   ,@(cddr x)))))
			    e macros-ht))))
		      
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

(define (expand-let x e macros-ht)
   (let* ((bindings (cadr x))
	  (body (cddr x)))
      `(let ,(map (lambda (binding)
		     (list (e (car binding) e macros-ht)
			   (e (cadr binding) e macros-ht)))
		  bindings)
	  ,@(map (lambda (y) (e y e macros-ht)) body))))

(install-expander! 'let ;; named let
		   (lambda (x e macros-ht)
		      (if (symbol? (cadr x))
			  (e (expand-named-let x) e macros-ht)
			  (expand-let x e macros-ht))))

(install-expander! 'define-struct
 (lambda (x e macros-ht)
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
		   (lambda (x e macros-ht)
		      (e `(,(runtime-ref 'make-promise)
			   (lambda () ,@(cdr x))) e macros-ht)))

(install-expander!
 'for-each
 (lambda (x e macros-ht)
    (match-case x
       ((?- ?proc . ?Ls)
	(e
	 (let ((L-ids (map (lambda (ignored)
			      (gensym 'L))
			   Ls))
	       (tmp-f (gensym 'tmpF)))
	    `(let ((,tmp-f ,proc))
		(do ,(map (lambda (id l)
			     `(,id ,l (cdr ,id)))
			  L-ids Ls)
		    ((null? ,(car L-ids)) #unspecified)
		    (,tmp-f ,@(map (lambda (id)
				      `(car ,id))
				   L-ids)))))
	 e macros-ht))
       (else
	(error "for-each-expander" "Invalid for-each-form: " x)))))

(install-expander!
 'map
 (lambda (x e macros-ht)
    (match-case x
       ((?- ?proc . ?Ls)
	(e
	 (let ((L-ids (map (lambda (ignored)
			      (gensym 'L))
			   Ls))
	       (rev-res (gensym 'revRes))
	       (loop (gensym 'loop))
	       (false-head (gensym 'falseHead))
	       (tail (gensym 'tail))
	       (tmp-f (gensym 'tmpF)))
	    `(let ((,tmp-f ,proc)
		   (,false-head (cons '() '())))
		(let ,loop ((,tail ,false-head)
			    ,@(map list L-ids Ls))
		     (if (null? ,(car L-ids))
			 (cdr ,false-head)
			 (begin
			    (set-cdr! ,tail
				      (cons (,tmp-f ,@(map (lambda (id)
							      `(car ,id))
							   L-ids))
					    '()))
			    (,loop (cdr ,tail)
				   ,@(map (lambda (id)
					     `(cdr ,id))
					  L-ids)))))))
	 e macros-ht))
       (else
	(error "map-expander" "Invalid map-form: " x)))))

(install-expander!
 'map!
 (lambda (x e macros-ht)
    (match-case x
       ((?- ?proc ?L1 . ?Lrest)
	(e
	 (let ((L-ids (map (lambda (ignored)
			      (gensym 'L))
			   (cons L1 Lrest)))
	       (loop (gensym 'loop))
	       (tmp-f (gensym 'tmpF))
	       (first-L (gensym 'firstL)))
	    `(let ((,tmp-f ,proc)
		   (,first-L ,L1))
		(let ,loop ((,(car L-ids) ,first-L)
			    ,@(map list (cdr L-ids) Lrest))
		     (if (null? ,(car L-ids))
			 ,first-L
			 (begin
			    (set-car! ,(car L-ids)
				      (,tmp-f ,@(map (lambda (id)
							`(car ,id))
						     L-ids)))
			    (,loop ,@(map (lambda (id)
					     `(cdr ,id))
					  L-ids)))))))
	 e macros-ht))
       (else
	(error "map!-expander" "Invalid map!-form: " x)))))

(install-expander!
 'bind-exit
 (lambda (x e macros-ht)
    (match-case x
       ((?- (?escape) ?expr . ?Lrest)
	(e
	 `(,(runtime-ref 'bind-exit-lambda)
	   (lambda (,escape)
	      ,expr
	      ,@Lrest))
	 e macros-ht))
       (else
	(error "bind-exit" "Invalid bind-exit-form: " x)))))

(install-expander!
 'with-handler
 (lambda (x e macros-ht)
    (match-case x
       ((?- ?handler ?expr . ?Lrest)
	(e
	 `(,(runtime-ref 'with-handler-lambda)
	   ,handler
	   (lambda ()
	      ,expr
	      ,@Lrest))
	 e macros-ht))
       (else
	(error "with-handler" "Invalid with-handler-form: " x)))))
