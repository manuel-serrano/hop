(module pobject-conv
   (option (loadq "protobject-eval.sch"))
   (include "protobject.sch")
   (include "nodes.sch")
   (import nodes
	   config
	   protobject
	   verbose)
   (export (pobject-conv::pobject prog)
	   (runtime-ref id::symbol)))

;; recognized as "artificial" runtime-reference.
(define (runtime-ref id)
   ;; we use a function to differenciate this construct
   ;; from similar user-constructs.
   (list 'runtime-ref id (lambda () 'runtime-ref)))

(define (location s-expr)
   (and (epair? s-expr)
	(cer s-expr)))

(define (scheme->pobject-map l)
   (let loop ((l l)
	      (rev-res '()))
      (if (null? l)
	  (reverse! rev-res)
	  (let ((loc (location l)))
	     (loop (cdr l)
		   (cons (scheme->pobject (car l) loc)
			 rev-res))))))

(define (location-map f l)
   (let loop ((l l)
	      (rev-res '()))
      (if (null? l)
	  (reverse! rev-res)
	  (let ((loc (location l)))
	     (loop (cdr l)
		   (cons (f (car l) loc)
			 rev-res))))))

(define (attach-location o loc)
   (if loc (set! o.loc loc))
   o)
   
(define (pobject-conv prog)
   (verbose "list->pobject")
   (nodes-init!)
   (new-node Module (scheme->pobject prog (location prog))))

(define (expr-list->Begin expr-list)
   (new-node Begin (scheme->pobject-map expr-list)))

(define (lambda->pobject arguments body)
   ;; if there's a vaarg, make it a the last element of the list and return
   ;; (the list . #t)
   (define (vaarg-list! arguments)
      (cond
	 ((null? arguments)
	  (cons arguments #f))
	 ((not (pair? arguments))
	  (cons (list arguments) #t))
	 (else
	  (let* ((p (last-pair arguments))
		 (vaarg (cdr p)))
	     (if (null? vaarg)
		 (cons arguments #f)
		 (begin
		    (set-cdr! p (list vaarg)) ;; physically attach the vaarg
		    (cons arguments #t)))))))
      
   (let* ((formals/vaarg? (vaarg-list! arguments))
	  (formals (car formals/vaarg?))
	  (vaarg? (cdr formals/vaarg?))
	  (formal-decls
	   (location-map (lambda (formal loc)
			    (attach-location (new-node Decl formal) loc))
			 formals)))
      (new-node Lambda
		formal-decls
		vaarg?
		(new-node Return (expr-list->Begin body)))))

(define (let-form->pobject bindings body kind)
   (define (binding->pobject binding)
      (when (or (null? binding)
		(null? (cdr binding))
		(not (symbol? (car binding)))
		(not (null? (cddr binding))))
	 (error "pobject-conversion"
		"Bad Let-form binding"
		binding))
      (let ((var (car binding))
	    (val (cadr binding)))
	 (new-node Binding
	      (attach-location (new-node Decl var) (location binding))
	      (scheme->pobject val (location (cdr binding))))))
   
   (let ((pobject-bindings (map! binding->pobject bindings)))
      (new-node Let #f pobject-bindings (expr-list->Begin body) kind)))

(define (case->pobject key clauses)
   (define (clause->pobject clause last?)
      (let* ((consts (car clause))
	     (raw-exprs (cdr clause))
	     (exprs (scheme->pobject-map raw-exprs))
	     (begin-expr (new-node Begin exprs)))
	 (if (and last?
		  (eq? consts 'else))
	     (new-node Clause '() begin-expr #t)
	     (new-node Clause (map (lambda (const)
				 (new-node Const const))
			      consts)
		       begin-expr
		       #f))))
   
   (define (clauses->pobjects clauses rev-result)
      (cond
	 ((null? clauses) ;; should never happen
	  (reverse! rev-result))
	 ((null? (cdr clauses))
	  (let ((rev-all-clauses (cons (clause->pobject (car clauses) #t)
				       rev-result)))
	     ;; if there was no default clause, we add one.
	     (if (car rev-all-clauses).default-clause?
		 (reverse! rev-all-clauses)
		 (reverse! (cons (clause->pobject '(else #unspecified) #t)
				 rev-all-clauses)))))
	 (else
	  (clauses->pobjects (cdr clauses)
			     (cons (clause->pobject (car clauses) #f)
				   rev-result)))))

   (new-node Case
	(scheme->pobject-no-loc key)
	(clauses->pobjects clauses '())))

(define (scheme->pobject-no-loc exp)
   (cond
      ((pair? exp)
       (match-case exp
	  ((quote ?datum) (new-node Const datum))
	  ((lambda ?formals . ?body) (lambda->pobject formals body))
	  ((if ?test ?then)
	   ;(scheme->pobject `(if ,test ,then #unspecified)))
	   (set-cdr! (cddr exp) '(#unspecified))
	   (scheme->pobject-no-loc exp))
	  ((if ?test ?then ?else)
	   (new-node If
		(scheme->pobject test (location (cdr exp)))
		(scheme->pobject then (location (cddr exp)))
		(scheme->pobject else (location (cdddr exp)))))
	  ((if . ?L) (error #f "bad if-form: " exp))
	  ((case ?key . ?clauses)
	   (case->pobject key clauses))
	  ((set! (and ?var (? symbol?)) ?expr)
	   (new-node Set!
		(attach-location (new-node Var-ref var) (location (cdr exp)))
		(scheme->pobject expr (location (cddr exp)))))
	  ((set! . ?L) (error #f "bad set!-form: " exp))
	  ((let ?bindings . ?body) (let-form->pobject bindings body 'let))
	  ((letrec ?bindings . ?body) (let-form->pobject bindings body 'letrec))
	  ((begin . ?body) (new-node Begin (scheme->pobject-map body)))
	  ((define ?var ?expr)
	   (new-node Define
		(attach-location (new-node Decl var) (location (cdr exp)))
		(scheme->pobject expr (location (cddr exp)))))
	  ((pragma ?str)
	   (new-node Pragma str))
	  ((runtime-ref ?id (? procedure?))
	   (new-node Runtime-Var-ref id))
	  ((?operator . ?operands)
	   (if (and (config 'return)
		    (eq? operator 'return!))
	       (if (or (null? operands)
		       (not (null? (cdr operands))))
		   (error #f "bad return! form: " exp)
		   (new-node Return (scheme->pobject (car operands)
						     (location operands))))
	       (new-node Call
			 (scheme->pobject operator (location exp))
			 (scheme->pobject-map operands))))))
      ((eq? exp #unspecified)
	(new-node Const #unspecified))
       ;; unquoted symbols must be var-refs
      ((symbol? exp)
       (new-node Var-ref exp))
      ((vector? exp)
       (error #f "vectors must be quoted" exp))
      (else
       (new-node Const exp))))

(define (scheme->pobject exp loc)
   (attach-location (scheme->pobject-no-loc exp) loc))
