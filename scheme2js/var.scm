(module var
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes)
   (export (var-init!)
	   (Decl-of-new-Var id)))

(define (var-init!)
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

   (define-node (Var id)
      (set! this.id id))
   (set! Var.proto.clone pobject-clone)
   (set! Var.proto.deep-clone pobject-deep-clone)

   (define-node (Field-Var id obj-var field-id-var)
      (set! this.id id)
      (set! this.obj obj-var)
      (set! this.field field-id-var))
   (set! Field-Var.proto (empty-pobject Var))
   (set! Field-Var.proto.clone pobject-clone)
   (set! Field-Var.proto.deep-clone pobject-deep-clone)

   (define-pmethod (Var-reference)
      (let ((var-ref (new-node Var-ref this.id)))
	 (set! var-ref.var this)
	 var-ref))
   (set! Var.proto.reference Var-reference)

   (define-pmethod (Field-Var-reference)
      (let ((closure-ref (new-node Closure-ref this.id this)))
	 closure-ref))
   (set! Field-Var.proto.reference Field-Var-reference)

   (define-pmethod (Var-assig val)
      (let ((var-ref (this.reference)))
	 (new-node Set! var-ref val)))
   (set! Var.proto.assig Var-assig)


   (define-node (JS-Var scheme-id js-id)
      (set! this.id scheme-id)
      (set! this.js-id js-id)
      (set! this.is-global? #t))
   (set! JS-Var.proto (empty-pobject Var))
   (set! JS-Var.proto.imported? #t)

   (define-node (JS-This-Var)
      (set! this.id 'this)
      (set! this.js-id 'this))
   (set! JS-This-Var.proto (empty-pobject JS-Var))
   (set! JS-This-Var.proto.assig
	 (pmethod (val)
		  (error #f "JS this variable must not be modified." val)))

   (define-node (Call/cc-indicator-Var)
      (set! this.id 'call/cc-indicator)
      (set! this.js-id 'call/cc-indicator-dummy))
   (set! Call/cc-indicator-Var.proto (empty-pobject JS-Var)))

(define (Decl-of-new-Var id)
   (let ((decl (new-node Decl id))
	 (var (new (node 'Var) id)))
      (set! decl.var var)
      decl))
