;; $Id: nodes.scm 130 2006-03-02 17:28:56Z flo $
(module nodes
   (include "protobject.sch")
   (import protobject
	   verbose)
   (option (loadq "protobject-eval.sch"))
   (export Node
	   Program
	   Part
	   Const
	   Var-ref
	   Decl
	   Scope
	   Body
	   Lambda
	   If
	   Case
	   Clause
	   Set!
	   Binding
	   Let-form
	   Begin
	   Define
	   Bind-exit
	   Call
	   Tail-rec
	   Tail-rec-call
	   Return
	   Closure-alloc
	   Label
	   Break
	   Pragma))

(define-macro (debug-print . L)
;   (cons 'verbose L))
   #unspecified)

(define-macro (proto-traverses class . fields)
   (define (starts-with-? sym)
      (char=? #\? (string-ref (symbol->string sym) 0)))

   (define (without-? sym)
      (let ((symstr (symbol->string sym)))
	 (string->symbol (substring symstr 1 (string-length symstr)))))

   (define (access field)
      (symbol-append 'this. field))
   
   (define (gen-method define-name nb-args method-name modify-pass?)
      (define (gen-args)
	 (map (lambda (n)
		 (string->symbol
		  (string-append "arg"
				 (number->string n))))
	      (iota nb-args)))
		   
      (define (call-node node)
	 `(,(symbol-append node (symbol-append (string->symbol ".") method-name))
	   ,@(gen-args)))
   
      (define (run-over-list field-list)
	 (if modify-pass?
	     (let ((loop (gensym 'loop))
		   (node (gensym 'node))
		   (flist (gensym 'flist)))
		`(let ,loop ((,flist ,(access field-list)))
		      (unless (null? ,flist)
			 (let ((,node (car ,flist)))
			    (set-car! ,flist ,(call-node node))
			    (,loop (cdr ,flist))))))
	     (let ((node (gensym 'node)))
		`(for-each (lambda (,node)
			      ,(call-node node))
			   ,(access field-list)))))
							   
      (define (visit field)
	 (if modify-pass?
	     `(set! ,(access field) ,(call-node (access field)))
	     (call-node (access field))))

      (define (traverse-field field)
	 (cond
	    ((and (pair? field) (starts-with-? (car field)))
	     (let ((field-w/o-? (without-? (car field))))
		`(and ,(access field-w/o-?)
		      ,(run-over-list field-w/o-?))))
	    ((pair? field)
	     (run-over-list (car field)))
	    ((starts-with-? field)
	     (let ((field-w/o-? (without-? field)))
		`(and ,(access field-w/o-?)
		      ,(visit field-w/o-?))))
	    (else
	     (visit field))))
      
      (define (map-fields fields)
	 (map (lambda (field)
		 `(begin
		     (debug-print ',field)
		     ,(traverse-field field)))
	      fields))
      
      `(define-pmethod (,define-name ,@(gen-args))
	  (debug-print (pobject-name this))
	  ,@(cond
	       (modify-pass?
		(append! (map-fields fields) '(this)))
	       ((null? fields)
		'(this.default-traverse-value))
	       (else
		(map-fields fields)))))
      
   (define (gen-traverse nb-args)
      (let* ((method-name (symbol-append 'traverse (string->symbol
						    (number->string nb-args))))
	     (define-name (symbol-append class '-proto- method-name))
	     ;; always just call 'traverse. up to the user to modify it.
	     (method-definition (gen-method define-name nb-args 'traverse #f))
	     (method-name! (symbol-append 'traverse
					  (string->symbol
					   (number->string nb-args))
					  '!))
	     (define-name! (symbol-append class '-proto- method-name!))
	     ;; always just call 'traverse!. up to the user to modify it.
	     (method-definition! (gen-method define-name! nb-args 'traverse! #t)))
	 `(begin
	     ,method-definition
	     ,method-definition!
	     (set! ,(symbol-append class '.proto. method-name) ,define-name)
	     (set! ,(symbol-append class '.proto. method-name!)
		   ,define-name!))))
   `(begin
       ,@(map gen-traverse (iota 3))))

(define-pclass (Node))
(proto-traverses Node)

(define-pclass (Const value)
   (set! this.value value))
(set! Const.proto (new Node))
(proto-traverses Const)

(define-pclass (Var-ref id)
   (set! this.id id))
(set! Var-ref.proto (new Node))
(proto-traverses Var-ref)

(define-pclass (Decl id)
   (set! this.id id))
(set! Decl.proto (empty-pobject Var-ref))
(proto-traverses Decl)

(define-pclass (Scope))
(set! Scope.proto (new Node))
(proto-traverses Scope) ;; should not be necessary

(define-pclass (Program body)
   (set! this.body body))
(set! Program.proto (new Scope))
(proto-traverses Program body)

;; a Part represents a functional part of a program. Besides of free variables
;; the generated code should be fully functionally.
;; if a function is given, the function is called after code-generation with
;; the generated code as parameter. The returned string is then used as result
;; of the Part.
;; A part is *not* a scope. Two parts at the same level can therefore share
;; variables.
(define-pclass (Part body . Lfun)
   (set! this.body body)
   (set! this.fun (if (null? Lfun) (lambda (x) x) (car Lfun))))
(set! Part.proto (empty-pobject Node))
(proto-traverses Part body)

;; Body must receive an expr. (usually a Begin).
;; this way there's only one class sequencing exprs.
(define-pclass (Body expr)
   (set! this.expr expr))
(set! Body.proto (new Scope)) ;; every body might have 'defines'
(proto-traverses Body expr)

(define-pclass (Lambda formals vaarg body)
   (set! this.formals formals)
   (set! this.vaarg vaarg)
   (set! this.body body))
(set! Lambda.proto (new Scope))
(proto-traverses Lambda (formals) ?vaarg body)

(define-pclass (If test then else)
   (set! this.test test)
   (set! this.then then)
   (set! this.else else))
(set! If.proto (new Node))
(proto-traverses If test then else)

(define-pclass (Case key clauses)
   (set! this.key key)
   (set! this.clauses clauses))
(set! Case.proto (new Node))
(proto-traverses Case key (clauses))

(define-pclass (Clause consts expr default-clause?)
   (set! this.consts consts) ;; default clause just has no consts
   (set! this.expr expr)
   (set! this.default-clause? default-clause?))
(set! Clause.proto (new Node))
(proto-traverses Clause (consts) expr)

(define-pclass (Set! lvalue val)
   (set! this.lvalue lvalue)
   (set! this.val val))
(set! Set!.proto (new Node))
(proto-traverses Set! lvalue val)

(define-pclass (Binding lvalue val)
   (set! this.lvalue lvalue)
   (set! this.val val))
(set! Binding.proto (empty-pobject Set!))
(proto-traverses Binding lvalue val)

;; kind is either 'let or 'letrec
(define-pclass (Let-form bindings body kind)
   (set! this.bindings bindings)
   (set! this.body body)
   (set! this.kind kind))
(set! Let-form.proto (new Scope))
(proto-traverses Let-form (bindings) body)

;; Body and Begin are not equivalent.
(define-pclass (Begin exprs)
   (set! this.exprs exprs))
(set! Begin.proto (new Node))
(proto-traverses Begin (exprs))

(define-pclass (Define lvalue val)
   (set! this.lvalue lvalue)
   (set! this.val val))
(set! Define.proto (empty-pobject Set!))
(proto-traverses Define lvalue val)

(define-pclass (Bind-exit escape body)
   (set! this.escape escape)
   (set! this.body body))
(set! Bind-exit.proto (empty-pobject Scope))
(proto-traverses Bind-exit escape body)

(define-pclass (Call operator operands)
   (set! this.operator operator)
   (set! this.operands operands))
(set! Call.proto (new Node))
(proto-traverses Call operator (operands))


;; optimization-nodes

;; label is either a symbol or #f
(define-pclass (Tail-rec formals vaarg body label)
   (set! this.formals formals)
   (set! this.vaarg vaarg)
   (set! this.body body)
   (set! this.label label))
(set! Tail-rec.proto (empty-pobject Scope))
(proto-traverses Tail-rec (formals) ?vaarg body)

;; label is either a symbol or #f
(define-pclass (Tail-rec-call label)
   (set! this.label label))
(set! Tail-rec-call.proto (new Node))
(proto-traverses Tail-rec-call)
   
(define-pclass (Return val)
   (set! this.val val))
(set! Return.proto (new Node))
(proto-traverses Return val)

(define-pclass (Closure-alloc allocated-vars body)
   (set! this.allocated-vars allocated-vars)
   (set! this.body body))
(set! Closure-alloc.proto (new Node))
(proto-traverses Closure-alloc body)

(define-pclass (Label body id)
   (set! this.body body)
   (set! this.id id))
(set! Label.proto (new Node))
(proto-traverses Label body)

(define-pclass (Break val label)
   (set! this.val val)
   (set! this.label label))
(set! Break.proto (new Node))
(proto-traverses Break val)

(define-pclass (Pragma str)
   (set! this.str str))
(set! Pragma.proto (new Node))
(proto-traverses Pragma)
