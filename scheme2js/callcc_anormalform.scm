(module callcc-a-normal-form
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   callcc-locations
	   var
	   verbose)
   (export (call/cc-a-normal-form! tree::pobject)))

;; TODO: can be better: currently the following expression is decomposed into
;; several tmp-assignments:
;; (some-f (some-g (call/cc ...)))
;; =>
;; (let* ((tmp (call/cc)))
;;        (tmp2 (some-g tmp)))
;;     (some-f tmp2)
;;
;; better solution would be:
;; (let ((tmp (call/cc)))
;;    (some-f (some-g tmp)))
;;
;; ===
;;
;; (some_f ... (call/cc) ...)
;; =>
;; (let ((tmp (call/cc)))
;;    (some_f ... tmp ...))
;;
;; despite the name it is not really the A-Normal form, as only arguments
;; containing call/cc calls are moved in front of the call.
(define (call/cc-a-normal-form! tree)
   (call/cc-locations tree)
   (overload traverse! a-normal! (Node
				  Call
				  Tail-call
				  Lambda)
	     (tree.traverse! #f)))

(define-pmethod (Node-a-normal! arg-node)
   (this.traverse1! arg-node))

;; returns a list of outer-assigs. The given ops-list is physically modified.
(define (ops-a-normal-form! ops)
   (let loop ((ops ops)
	      (outer-assigs '()))
      (cond
	 ((and (null? ops)
	       (null? outer-assigs))
	  '()) ;; just return an empty list.
	 ((null? ops)
	  outer-assigs)
	 (else
	  (let ((op (car ops)))
	     (if op.call/cc-argument?
		 (let* ((tmp-decl (Decl-of-new-Var 'tmp))
			(assig (new-node Binding tmp-decl op)))
		    (delete! op.call/cc-argument?)
		    (set-car! ops (tmp-decl.var.reference))
		    (loop (cdr ops)
			  (cons assig outer-assigs)))
		 (loop (cdr ops)
		       outer-assigs)))))))
   
;; if we are inside a Call, then arg-node represents the node of the argument.
;; We look, if we do a call/cc-call ourselves. If not, have to look for
;; arguments with call/ccs too.
(define-pmethod (Call-a-normal! arg-node)
   (if (and arg-node
	    this.call/cc?)
       (set! arg-node.call/cc-argument? #t))
   
   (let* ((ops (cons this.operator this.operands))
	  (ops-traversed (map! (lambda (op)
				  (let ((res (op.traverse! op)))
				     (if op.call/cc-argument?
					 (set! res.call/cc-argument? #t))
				     res))
			       ops))
	  (outer-assigs (ops-a-normal-form! ops-traversed)))
      (if (null? outer-assigs)
	  this
	  (begin
	     (set! this.operator (car ops-traversed))
	     (set! this.operands (cdr ops-traversed))
	     (if arg-node
		 (set! arg-node.call/cc-argument? #t))
	     (new-node Let
		       (map (lambda (assig)
			       assig.lvalue.var)
			    outer-assigs)
		       outer-assigs
		       this
		       'let)))))

(define-pmethod (Tail-call-a-normal! arg-node)
   (let* ((updates-traversed
	   (map! (lambda (op)
		    (let ((res (op.traverse! op)))
		       ;; res and op might not be the same node
		       (when op.call/cc-argument?
			  (delete! op.call/cc-argument?)
			  (set! res.call/cc-argument? #t))
		       res))
		 this.updates))
	  (outer-assigs (ops-a-normal-form! updates-traversed)))
      (if (null? outer-assigs)
	  this
	  (begin
	     (set! this.updates updates-traversed)
	     (if arg-node
		 (set! arg-node.call/cc-argument? #t))
	     (new-node Let
		       (map (lambda (assig)
			       assig.lvalue.var)
			    outer-assigs)
		       outer-assigs
		       this
		       'let)))))

(define-pmethod (Lambda-a-normal! arg-node)
   ;; lambda shields from arg-node
   (this.traverse1! #f))

