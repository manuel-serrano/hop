(module __hopscheme_dollar-escape
   (library scheme2js)
   (import __hopscheme_config)
   (export (dollar-extraction! expr)
	   (replace-dollars! expr dollar-map::pair-nil)))

(define (starts-with-dollar? id)
   (and (symbol? id)
	(let ((str (symbol->string id)))
	   (char=? #\$ (string-ref str 0)))))

(define (strip-dollar s)
   (let ((str (symbol->string s)))
      (string->symbol (substring str 1 (string-length str)))))

(define (dollar-extraction! expr)
   (let* ((dummy-head (list 'dollar-list))
	  (transformed (extract! expr 0 dummy-head)))
      (if (starts-with-dollar? transformed)
	  (values `(begin ,transformed) (cdr dummy-head))
	  (values transformed (cdr dummy-head)))))

(define (extract! expr quasi-depth dollar-map)
   (define (add-to-map! expr map)
      (let ((dollar-id (gensym 'dollar)))
	 ;; add to map.
	 (set-cdr! map (cons `(,dollar-id ,expr) (cdr map)))
	 (string->symbol (string-append "$" (symbol->string dollar-id)))))

   (define (contains-dot? sym)
      (let* ((symstr (symbol->string sym))
	     (first-dot (string-index symstr ".")))
	 (and first-dot (>=fx first-dot 0))))

   (define (split-dotted sym)
      (let* ((symstr (symbol->string sym))
	     (first-dot (string-index symstr ".")))
	 (values (string->symbol (substring symstr 0 first-dot))
		 (string->symbol (substring symstr
					    first-dot
					    (string-length symstr))))))

   (define (reassemble-dotted sym fields)
      (string->symbol (string-append (symbol->string sym)
				     (symbol->string fields))))

   (match-case expr
      ((quote ?q)
       (when (>fx quasi-depth 0)
	  (set-car! (cdr expr) (extract! q quasi-depth dollar-map)))
       expr)
      ((quasiquote ?q)
       (set-car! (cdr expr)
		 (extract! q (+fx quasi-depth 1) dollar-map))
       expr)
      ((unquote ?q)
       (if (>fx quasi-depth 0)
	   (set-car! (cdr expr) (extract! q (-fx quasi-depth 1) dollar-map))
	   (set-car! (cdr expr) (extract! q quasi-depth dollar-map)))
       expr)
      (($ ?dollar . ?-)
       (cond
	  ((>fx quasi-depth 0)
	   (extract! (cdr expr) quasi-depth dollar-map)
	   expr)
	  ((symbol? dollar)
	   (set-car! expr
		     (string->symbol (string-append "$"
						    (symbol->string dollar))))
	   (set-cdr! expr (cddr expr))
	   (extract! expr quasi-depth dollar-map))
	  (else
	      (set-car! expr (add-to-map! dollar dollar-map))
	      (set-cdr! expr (extract! (cddr expr) quasi-depth dollar-map))
	      expr)))
      ((? pair?)
       (set-car! expr (extract! (car expr) quasi-depth dollar-map))
       (set-cdr! expr (extract! (cdr expr) quasi-depth dollar-map))
       expr)
      ((? symbol?) ;; symbol
       (cond
	  ((>fx quasi-depth 0)
	   expr)
	  ((starts-with-dollar? expr)
	   (if (contains-dot? expr)
	       (receive (obj fields)
		  (split-dotted expr)
		  (reassemble-dotted (add-to-map! (strip-dollar obj)
						  dollar-map)
				     fields))
	       (add-to-map! (strip-dollar expr) dollar-map)))
	  (else expr)))
      ((? vector?)
       (let loop ((i 0))
	  (if (=fx i (vector-length expr))
	      expr
	      (begin
		 (vector-set! expr i
			      (extract! (vector-ref expr i)
					quasi-depth
					dollar-map))
		 (loop (+fx i 1))))))
      (else
       expr)))

(define (replace-dollars! expr dollar-map)
   (replace! expr 0 dollar-map))

(define (replace! expr quasi-depth dollar-map)
   (define (dollar->val id map)
      (let ((t (assq id map)))
	 (when (not t)
	    (error 'dollar-replacement
		   "Could not find value for dollar-id"
		   id))
	 (cadr t)))

   (define (contains-dot? sym)
      (let* ((symstr (symbol->string sym))
	     (first-dot (string-index symstr ".")))
	 (and first-dot (>=fx first-dot 0))))

   (define (split-dotted sym)
      (let* ((symstr (symbol->string sym))
	     (first-dot (string-index symstr ".")))
	 (values (string->symbol (substring symstr 0 first-dot))
		 (string->symbol (substring symstr
					    first-dot
					    (string-length symstr))))))

   (match-case expr
      ((quote ?q)
       (when (>fx quasi-depth 0)
	  (set-car! (cdr expr) (replace! q quasi-depth dollar-map)))
       expr)
      ((quasiquote ?q)
       (set-car! (cdr expr)
		 (replace! q (+fx quasi-depth 1) dollar-map))
       expr)
      ((unquote ?q)
       (if (>fx quasi-depth 0)
	   (set-car! (cdr expr) (replace! q (-fx quasi-depth 1) dollar-map))
	   (set-car! (cdr expr) (replace! q quasi-depth dollar-map)))
       expr)
      ((? pair?)
       (if (and (starts-with-dollar? (car expr))
		(contains-dot? (car expr)))
	   (receive (obj fields)
	      (split-dotted expr)
	      (replace! (cons* `(begin obj) fields (cdr expr))
			quasi-depth dollar-map))
	   (begin
	      (set-car! expr (replace! (car expr) quasi-depth dollar-map))
	      (set-cdr! expr (replace! (cdr expr) quasi-depth dollar-map))
	      expr)))
      ((? symbol?) ;; symbol
       (cond
	  ((>fx quasi-depth 0)
	   expr)
	  ((starts-with-dollar? expr)
	   (if (contains-dot? expr)
	       (receive (obj fields)
		  (split-dotted expr)
		  `(begin (begin ,(dollar->val (strip-dollar obj)
					       dollar-map))
			  fields))
	       (dollar->val (strip-dollar expr) dollar-map)))
	  (else expr)))
      ((? vector?)
       (let loop ((i 0))
	  (if (=fx i (vector-length expr))
	      expr
	      (begin
		 (vector-set! expr i
			      (replace! (vector-ref expr i)
					quasi-depth
					dollar-map))
		 (loop (+fx i 1))))))
      (else
       expr)))


;; ===========================================================================
;; add scheme2js pre-expander, so we recognize '$'escapes.
;; ===========================================================================

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
       (if (scheme2js-config 'hop-module-compilation)
	   (let ((val (dollar-eval (cadr l))))
	      (if (eq? val #unspecified)
		  (unhop-list! (cddr l)) ;; skip '$ and (...)
		  (begin
		     (set-car! l `(pragma ,val))
		     (set-cdr! l (unhop-list! (cddr l)))
		     l)))
	   (begin
	      (error 'dollar-escape
		     "Internal Error"
		     #f))))
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
	      `(pragma ,(if (scheme2js-config 'hop-module-compilation)
			    (dollar-eval (strip-dollar x))
			    symstr))
	      `(begin
		  (pragma ,(if (scheme2js-config 'hop-module-compilation)
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
       ;; should the quasiquote contain unescaped elements, then the expansion
       ;; will reinvoke this expander.
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
