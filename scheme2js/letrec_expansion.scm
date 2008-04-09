(module letrec-expansion
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   var
	   verbose
	   gen-js)
   (export (letrec-expansion! tree::pobject)))

;; According to the spec a (letrec ((x0 v0) (x1 v1) ...) body) is equivalent
;; to: (let ((x0 _undef) (x1 _undef) ...) (set! x0 v0) (set! x1 v1) ... body)
;;
;; Our optimizations however do not like it, when there are two assignments to
;; the same variable. Therefore we expand into this form only for call/cc, and
;; when this expansion actually could make a difference.
;;
;; In other words: if v0, v1, ... are constants (or lambda-constructions), then
;; they can stay where they are. Obvious example:
;;  (letrec ((x0 (lambda (...) ... x3))
;;           (x1 3)
;;           (x2 "...")
;;           (x3 (lambda (...) x0 x1 x2)))
;;      body)
;; would become:
;;  (let ((x0 _undef) (x1 _undef) (x2 _undef))
;;     (set! x0 (lambda () ... x3))
;;     (set! x1 3)
;;     (set! x2 "...")
;;     (set! x3 (lambda (...) x0 x1 x2))
;;     body)
;;
;; Obviously the initial assignments can directly be replaced by the following
;; assignments. In this case the letrec only served as a way to mutually
;; reference the variables (the most common use for letrec).
;;
;; If however we have:
;;  (letrec ((x0 (some-call ...))
;;           (x1 (some-other-call ...)))
;;     body)
;;
;; then the calls might be call/cc calls, and we expand the letrec (at least
;; for the concerned terms. The spec explitely states, that the assignments can
;; happen in any order. As such we can selectively expand, and leave some terms
;; before the extracted terms (thereby reordering the initial bindings).
;;
;; This pass has to happen quite early, as we assume that the bindings are of
;; the form (Set! x0 v0). (Something, that could easily change in later
;; passes).
;;
;; Let-recs are introduced by symbol-pass. This pass must hence happen after
;; the symbol-pass.
(define (letrec-expansion! tree)
   (verbose "letrec-expansion")
   (overload traverse expand! (Node
			       Let)
	     (tree.traverse)))

(define-pmethod (Node-expand!)
   (this.traverse0))

(define (letrec-constant? n)
   (or (inherits-from? n (node 'Const))
       (inherits-from? n (node 'Var-ref))
       (inherits-from? n (node 'Lambda))))

(define-pmethod (Let-expand!)
   (this.traverse0)
   (when (eq? this.kind 'letrec)
      (let loop ((bindings this.bindings)
		 (body-assigs '()))
	 (cond
	    ((and (null? bindings)
		  (null? body-assigs))
	     'do-nothing)
	    ((null? bindings)
	     (set! this.body (new-node Begin
				       (append! body-assigs
						(list this.body)))))
	    ((letrec-constant? (car bindings).val)
	     (loop (cdr bindings)
		   body-assigs))
	    (else
	     (let* ((binding (car bindings))
		    (binding-val binding.val))
		(set! binding.val (new-node Const #unspecified))
		(loop (cdr bindings)
		      (cons (binding.lvalue.var.assig binding-val)
			    body-assigs))))))))
