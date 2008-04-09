(module nodes
   (include "protobject.sch")
   (import protobject
	   verbose)
   (option (loadq "protobject-eval.sch"))
   (export (node n::symbol)
	   (nodes-init!)))

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
       ,@(map gen-traverse (iota 4))))

(define (node n)
   (hashtable-get (thread-parameter '*nodes*) n))

(define (nodes-init!)
   (define nodes (or (thread-parameter '*nodes*)
		     (make-hashtable)))
   
   (thread-parameter-set! '*nodes* nodes)
   
   ;;HACK HACK HACK: begins in begins..
   (define-macro (define-node signature . Lrest)
      (let ((name (car signature))
	    (tmp (gensym 'tmp-HACK)))
	 `(define ,name
	     (let ((,tmp (create-pclass ',name
					,(if (null? Lrest)
					     'pobject-id
					     `(pmethod ,(cdr signature)
						       ,@Lrest)))))
		(hashtable-put! nodes ',name ,tmp)
		,tmp))))
;	     (define-pclass ,signature ,@Lrest)
;	     (hashtable-put! *nodes* ',name ,name))))
   
   (define-node (Node))
   (proto-traverses Node)
   (set! Node.proto.clone pobject-clone)
   (set! Node.proto.deep-clone pobject-deep-clone)

   (define-node (Const value)
      (set! this.value value))
   (set! Const.proto (new Node))
   (proto-traverses Const)

   (define-node (Var-ref id)
      (set! this.id id))
   (set! Var-ref.proto (new Node))
   (proto-traverses Var-ref)

   ;; only short-term meaning.
   ;; after symbol-pass a Runtime-Var-ref is identical to a Var-ref node.
   ;; and not all Runtime-refs are Runtime-Var-refs.
   (define-node (Runtime-Var-ref id)
      (set! this.id id))
   (set! Runtime-Var-ref.proto (empty-pobject Var-ref))
   (proto-traverses Runtime-Var-ref)

   (define-node (Decl id)
      (set! this.id id))
   (set! Decl.proto (empty-pobject Var-ref))
   (proto-traverses Decl)

   (define-node (Module body)
      (set! this.body body))
   (set! Module.proto (empty-pobject Node))
   (proto-traverses Module body)

   ;; if the fun was vaarg, then the flag vaarg? has to be set, and the vaarg
   ;; must be the last element of the formals.
   (define-node (Lambda formals vaarg? body)
      (set! this.formals formals)
      (set! this.vaarg? vaarg?)
      (set! this.body body))
   (set! Lambda.proto (empty-pobject Node))
   (proto-traverses Lambda (formals) body)

   (define-node (If test then else)
      (set! this.test test)
      (set! this.then then)
      (set! this.else else))
   (set! If.proto (new Node))
   (proto-traverses If test then else)

   (define-node (Case key clauses)
      (set! this.key key)
      (set! this.clauses clauses))
   (set! Case.proto (new Node))
   (proto-traverses Case key (clauses))

   (define-node (Clause consts expr default-clause?)
      (set! this.consts consts) ;; default clause just has no consts
      (set! this.expr expr)
      (set! this.default-clause? default-clause?))
   (set! Clause.proto (new Node))
   (proto-traverses Clause (consts) expr)

   (define-node (Set! lvalue val)
      (set! this.lvalue lvalue)
      (set! this.val val))
   (set! Set!.proto (new Node))
   (proto-traverses Set! lvalue val)

   ;; first assignment to a variable.
   (define-node (Binding lvalue val)
      (set! this.lvalue lvalue)
      (set! this.val val))
   (set! Binding.proto (empty-pobject Set!))
   (proto-traverses Binding lvalue val)

   ;; kind is either 'let or 'letrec
   ;; vars can be #f when instantiated before symbol-resolution
   (define-node (Let scope-vars bindings body kind)
      (set! this.scope-vars scope-vars)
      (set! this.bindings bindings)
      (set! this.body body)
      (set! this.kind kind))
   (set! Let.proto (empty-pobject Node))
   (proto-traverses Let (bindings) body)

   (define-node (Begin exprs)
      (set! this.exprs exprs))
   (set! Begin.proto (new Node))
   (proto-traverses Begin (exprs))

   (define-node (Define lvalue val)
      (set! this.lvalue lvalue)
      (set! this.val val))
   (set! Define.proto (empty-pobject Set!))
   (proto-traverses Define lvalue val)

   (define-node (Call operator operands)
      (set! this.operator operator)
      (set! this.operands operands))
   (set! Call.proto (new Node))
   (proto-traverses Call operator (operands))

   ;; optimization-nodes

   (define-node (Frame-alloc storage-var vars)
      (set! this.storage-var storage-var)
      (set! this.vars vars))
   (set! Frame-alloc.proto (new Node))
   (proto-traverses Frame-alloc)

   (define-node (Frame-push storage-vars body)
      (set! this.storage-vars storage-vars)
      (set! this.body body))
   (set! Frame-push.proto (new Node))
   ;; we are not visiting the storage-refs.
   (proto-traverses Frame-push body)

   (define-node (Label id)
      (set! this.id id))
   
   (define-node (Return val)
      (set! this.val val))
   (set! Return.proto (new Node))
   (proto-traverses Return val)

   (define-node (Labelled body label)
      (set! this.body body)
      (set! this.label label))
   (set! Labelled.proto (new Node))
   (proto-traverses Labelled body)

   ;; break must never reference a loop.
   ;; if this is needed, then the loop must be wrapped into a 'labelled'.
   (define-node (Break val label)
      (set! this.val val)
      (set! this.label label))
   (set! Break.proto (new Node))
   (proto-traverses Break val)

   ;; break must never reference a loop.
   ;; if this is needed, then the loop must be wrapped into a 'labelled'.
   (define-node (Break val label)
      (set! this.val val)
      (set! this.label label))
   (set! Break.proto (new Node))
   (proto-traverses Break ?val)

   (define-node (Continue label)
      (set! this.label label))
   (set! Continue.proto (new Node))
   (proto-traverses Continue)

   (define-node (Pragma str)
      (set! this.str str))
   (set! Pragma.proto (new Node))
   (proto-traverses Pragma)

   ;; inits are Set!s, but must be in same order as scope-vars
   (define-node (Tail-rec scope-vars inits body label)
      ;; I would have preferred the name 'loop-vars' but this way some
      ;; code-sharing should be possible.
      (set! this.scope-vars scope-vars)
      (set! this.inits inits)
      (set! this.body body)
      (set! this.label label))
   (set! Tail-rec.proto (new Node))
   (proto-traverses Tail-rec (inits) body)

   ;; updates here are not assignments
   ;; they must be in the same order, as the scope-vars in the Tail-rec.
   (define-node (Tail-call updates label)
      (set! this.updates updates)
      (set! this.label label))
   (set! Tail-call.proto (empty-pobject Node))
   (proto-traverses Tail-call (updates))

   (define-node (While test body label)
      (set! this.test test)
      (set! this.body body)
      (set! this.label label))
   (set! While.proto (new Node))
   (proto-traverses While test body)

   (define-node (Call/cc-Call operator operands)
      (set! this.operator operator)
      (set! this.operands operands))
   (set! Call/cc-Call.proto (empty-pobject Call))
   (proto-traverses Call/cc-Call operator (operands))

   (define-node (Call/cc-Resume index)
      (set! this.indices/vars (list (cons index #f))))
   (set! Call/cc-Resume.proto (empty-pobject Node))
   (proto-traverses Call/cc-Resume)

   (define-node (Call/cc-Counter-Update index)
      (set! this.index index))
   (set! Call/cc-Counter-Update.proto (empty-pobject Node))
   (proto-traverses Call/cc-Counter-Update)
   )
   
