(module pobject-conv
   (include "protobject.sch")
   (import nodes
	   protobject
	   verbose)
   (export (pobject-conv::pobject prog)))

(define (pobject-conv prog)
   (verbose "list->pobject")
   (new Program
	(new Part
	     (scheme->pobject prog))))
   
(define (expr-list->Body expr-list)
   (new Body (expr-list->Begin expr-list)))

(define (expr-list->Begin expr-list)
   (new Begin (map scheme->pobject expr-list)))

(define (lambda->pobject formals-vaarg body)
   (define (split-formals-vaarg formals-vaarg)
      (let loop ((formals-vaarg formals-vaarg)
		 (rev-formals '()))
	 (cond
	    ((null? formals-vaarg)
	     (values (reverse! rev-formals) #f))
	    ((pair? formals-vaarg)
	     (loop (cdr formals-vaarg)
		   (cons (car formals-vaarg) rev-formals)))
	    (else
	     (values (reverse! rev-formals) formals-vaarg)))))

   (multiple-value-bind (formals vaarg)
      (split-formals-vaarg formals-vaarg)
      (new Lambda
	   (map (lambda (formal)
		   (new Decl formal))
		formals)
	   (and vaarg (new Decl vaarg))
	   (new Return (expr-list->Body body)))))

(define (let-form->pobject bindings body kind)
   (define (binding->pobject binding)
      (let ((var (car binding))
	    (val (cadr binding)))
	 (new Binding
	      (new Decl var)
	      (scheme->pobject val))))
   
   (let ((pobject-bindings (map binding->pobject bindings)))
      (new Let-form pobject-bindings (expr-list->Body body) kind)))

(define (case->pobject key clauses)
   (define (clause->pobject clause maybe-default-clause?)
      (let* ((consts (car clause))
	     (raw-exprs (cdr clause))
	     (exprs (map scheme->pobject raw-exprs))
	     (begin-expr (new Begin exprs)))
	 (if (and maybe-default-clause?
		  (eq? consts 'else))
	     (new Clause '() begin-expr #t)
	     (new Clause (map (lambda (const)
				 (new Const const))
			      consts)
		  begin-expr #f))))
   
   (define (clauses->pobjects clauses rev-result)
      (cond
	 ((null? clauses) ;; should never happen
	  (reverse! rev-result))
	 ((null? (cdr clauses))
	  (reverse! (cons (clause->pobject (car clauses) #t)
			  rev-result)))
	 (else
	  (clauses->pobjects (cdr clauses)
			     (cons (clause->pobject (car clauses) #f)
				   rev-result)))))

   (new Case
	(scheme->pobject key)
	(clauses->pobjects clauses '())))

(define (scheme->pobject exp)
   (cond
      ((pair? exp)
       (match-case exp
	  ((quote ?datum) (new Const datum))
	  ((lambda ?formals . ?body) (lambda->pobject formals body))
	  ((if ?test ?then)
	   (scheme->pobject `(if ,test ,then #unspecified)))
	  ((if ?test ?then ?else)
	   (new If
		(scheme->pobject test)
		(scheme->pobject then)
		(scheme->pobject else)))
	  ((if . L) (error #f "bad if-form: " exp))
	  ((case ?key . ?clauses)
	   (case->pobject key clauses))
	  ((set! ?var ?expr)
	   (new Set! (new Var-ref var) (scheme->pobject expr)))
	  ((let ?bindings . ?body) (let-form->pobject bindings body 'let))
	  ((letrec ?bindings . ?body) (let-form->pobject bindings body 'letrec))
	  ((begin . ?body) (new Begin (map scheme->pobject body)))
	  ((define ?var ?expr)
	   (new Define (new Decl var) (scheme->pobject expr)))
	  ((bind-exit (?escape) . ?body)
	   (new Bind-exit (new Decl escape) (expr-list->Body body)))
	  ((pragma ?str)
	   (new Pragma str))
	  ((part ?expr (and ?fun (? procedure?)))
	   (new Part (scheme->pobject expr) fun))
	  ((?operator . ?operands)
	   (new Call (scheme->pobject operator) (map scheme->pobject operands)))))
      ((eq? exp #unspecified)
	(new Const #unspecified))
       ;; unquoted symbols must be var-refs
      ((symbol? exp)
       (new Var-ref exp))
      ((vector? exp)
       (error #f "vectors must be quoted" exp))
      (else
       (new Const exp))))
