(module var
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes)
   (export Var
	   JS-Var
	   JS-This-Var
	   (Decl-of-new-Var id)))

(define-pclass (Var id)
   (set! this.id id))

(define-pmethod (Var-reference)
   (let ((var-ref (new Var-ref this.id)))
      (set! var-ref.var this)
      var-ref))
(set! Var.proto.reference Var-reference)

(define-pmethod (Var-assig val)
   (let ((var-ref (pcall this Var-reference)))
      (new Set! var-ref val)))
(set! Var.proto.assig Var-assig)


(define-pclass (JS-Var scheme-id js-id)
   (set! this.id scheme-id)
   (set! this.js-id js-id))
(set! JS-Var.proto (empty-pobject Var))
(set! JS-Var.proto.imported? #t)

(define-pclass (JS-This-Var)
   (set! this.id 'this))
(set! JS-This-Var.proto (empty-pobject JS-Var))
(set! JS-This-Var.proto.assig
      (pmethod (val)
	 (error #f "JS this variable must not be modified." val)))

(define (Decl-of-new-Var id)
   (let ((decl (new Decl id))
	 (var (new Var id)))
      (set! decl.var var)
      decl))
