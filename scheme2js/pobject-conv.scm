(module pobject-conv
   (option (loadq "protobject-eval.sch"))
   (include "protobject.sch")
   (import nodes
	   protobject
	   verbose)
   (export (pobject-conv::pobject prog)))

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
   (new Program
	(new Part
	     (scheme->pobject prog (location prog)))))
   
(define (expr-list->Body expr-list)
   (new Body (expr-list->Begin expr-list)))

(define (expr-list->Begin expr-list)
   (new Begin (scheme->pobject-map expr-list)))

(define (lambda->pobject formals-vaarg body)
   (define (split-formals-vaarg! formals-vaarg)
      (and (pair? formals-vaarg)
	   (let* ((p (last-pair formals-vaarg))
		  (vaarg (cdr p)))
	      (set-cdr! p '())
	      (and (not (null? vaarg))
		   vaarg))))

   (let ((vaarg (split-formals-vaarg! formals-vaarg))
	 (formals formals-vaarg)) ;; just an alias
      (new Lambda
	   (location-map (lambda (formal loc)
			    (attach-location (new Decl formal) loc))
			 formals)
	   (and vaarg (new Decl vaarg))
	   (new Return (expr-list->Body body)))))

(define (let-form->pobject bindings body kind)
   (define (binding->pobject binding)
      (let ((var (car binding))
	    (val (cadr binding)))
	 (new Binding
	      (attach-location (new Decl var) (location binding))
	      (scheme->pobject val (location (cdr binding))))))
   
   (let ((pobject-bindings (map! binding->pobject bindings)))
      (new Let-form pobject-bindings (expr-list->Body body) kind)))

(define (case->pobject key clauses)
   (define (clause->pobject clause maybe-default-clause?)
      (let* ((consts (car clause))
	     (raw-exprs (cdr clause))
	     (exprs (scheme->pobject-map raw-exprs))
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
	(scheme->pobject-no-loc key)
	(clauses->pobjects clauses '())))

(define (scheme->pobject-no-loc exp)
   (cond
      ((pair? exp)
       (match-case exp
	  ((quote ?datum) (new Const datum))
	  ((lambda ?formals . ?body) (lambda->pobject formals body))
	  ((if ?test ?then)
	   ;(scheme->pobject `(if ,test ,then #unspecified)))
	   (set-cdr! (cddr exp) '(#unspecified))
	   (scheme->pobject-no-loc exp))
	  ((if ?test ?then ?else)
	   (new If
		(scheme->pobject test (location (cdr exp)))
		(scheme->pobject then (location (cddr exp)))
		(scheme->pobject else (location (cdddr exp)))))
	  ((if . L) (error #f "bad if-form: " exp))
	  ((case ?key . ?clauses)
	   (case->pobject key clauses))
	  ((set! ?var ?expr)
	   (new Set!
		(attach-location (new Var-ref var) (location (cdr exp)))
		(scheme->pobject expr (location (cddr exp)))))
	  ((set! . L) (error #f "bad set!-form: " exp))
	  ((let ?bindings . ?body) (let-form->pobject bindings body 'let))
	  ((letrec ?bindings . ?body) (let-form->pobject bindings body 'letrec))
	  ((begin . ?body) (new Begin (scheme->pobject-map body)))
	  ((define ?var ?expr)
	   (new Define
		(attach-location (new Decl var) (location (cdr exp)))
		(scheme->pobject expr (location (cddr exp)))))
	  ((bind-exit (?escape) . ?body)
	   (new Bind-exit (new Decl escape) (expr-list->Body body)))
	  ((pragma ?str)
	   (new Pragma str))
	  ((part ?expr (and ?fun (? procedure?)))
	   (new Part
		(scheme->pobject expr (location (cdr exp)))
		fun))
	  ((?operator . ?operands)
	   (new Call
		(scheme->pobject operator (location exp))
		(scheme->pobject-map operands)))))
      ((eq? exp #unspecified)
	(new Const #unspecified))
       ;; unquoted symbols must be var-refs
      ((symbol? exp)
       (new Var-ref exp))
      ((vector? exp)
       (error #f "vectors must be quoted" exp))
      (else
       (new Const exp))))

(define (scheme->pobject exp loc)
   (attach-location (scheme->pobject-no-loc exp) loc))
