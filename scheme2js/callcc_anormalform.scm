(module callcc-a-normal-form
   (import config
	   nodes
	   export-desc
	   walk
	   callcc-locations
	   verbose
	   side
	   tools)
   (export (call/cc-a-normal-form! tree::Module)))

;; Calls must be in A-Normal form when using call/cc.
;; Otherwise the order of parameter-evaluation is not yet clear, which
;; could lead to big troubles...
;; Ex:
;; (some_f (let ((x 0)) (capture! x))
;;         (call/cc))
;; there are two ways to evaluate this:
;; (let ((tmp (let ((x 0)) (capture! x))))
;;    (some_f tmp (call/cc)))
;; or
;; (let ((tmp (call/cc)))
;;   (some_f (let ((x 0)) (capture! x)) tmp))
;;
;; in the first case the x can/must be reused.
;; in the second it must be reconstructed at each iteration.

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

;; Ifs and Case must be in a-normal form too. Indeed, their tests/keys must be
;; constant variables.

(define (call/cc-a-normal-form! tree)
   (side-effect tree)
   (a-normal! tree #f #f))

(define (make-box v)
   (cons '*box* v))

(define (box-set! b v)
   (and b
	(set-cdr! b v)))

(define (unbox b)
   (and b
	(cdr b)))

;; '<-call/cc?' contains the return-value of the call. #t if there was a
;; call/cc, #f otherwise.
(define-nmethod (Node.a-normal! <-call/cc?)
   (default-walk! this <-call/cc?))

(define (move-into-let! l n)
   (let* ((tmp-ref (Ref-of-new-Var 'tmp))
	  (assig (instantiate::Set! (lvalue tmp-ref) (val n))))
      (with-access::Let l (scope-vars bindings)
	 (cons-set! scope-vars (Ref-var tmp-ref))
	 (cons-set! bindings assig)
	 (Ref-var tmp-ref))))

;; physically modifies the given list, so that all ops are safe.
;; the nodes are traversed here. so no need to traverse them elsewhere.
;; if temporary variables have been created, then a 'Let' is returned.
;;   otherwise #f. Temporary variables are created when there is a call/cc.
;; -> let-n equates call/cc?.
(define (ops-a-normal-form! ops body walk!)
   (let ((<-call/cc? (make-box #f)))
      (let loop ((ops ops)
		 (let-n #f))
	 (if (null? ops)
	     let-n
	     (begin
		(box-set! <-call/cc? #f)
		(let* ((op (car ops))
		       (traversed (walk! op <-call/cc?)))
		   (cond
		      ((not (unbox <-call/cc?)) ;; no call/cc in op
		       (loop (cdr ops) let-n))
		      ((not let-n)
		       (let* ((let-n (instantiate::Let
					(scope-vars '())
					(bindings '())
					(body body)
					(kind 'let)))
			      (var (move-into-let! let-n traversed)))
			  (set-car! ops (var-reference var))
			  (loop (cdr ops) let-n)))
		      (else
		       (let ((var (move-into-let! let-n traversed)))
			  (set-car! ops (var-reference var))
			  (loop (cdr ops) let-n))))))))))

(define-nmethod (Call.a-normal! <-call/cc?)
   (with-access::Call this (operator operands call/cc?)
      (let* ((l (cons operator operands))
	     (let-n (ops-a-normal-form! l this walk!)))

	 ;; update return-value
	 (when (or let-n call/cc?)
	    (box-set! <-call/cc? #t))

	 ;; and the node.
	 (set! operator (car l))
	 (set! operands (cdr l))
	 (or let-n this))))
      
(define-nmethod (Tail-rec-Call.a-normal! <-call/cc?)
   (with-access::Tail-rec-Call this (updates)
      (let ((let-n (ops-a-normal-form! updates this walk!)))
	 ;; update return-value
	 (when let-n (box-set! <-call/cc? #t))

	 (or let-n this))))

(define-nmethod (Case.a-normal! <-call/cc?)
   (with-access::Case this (key clauses)
      (let ((tmp-call/cc? (unbox <-call/cc?)))
	 (box-set! <-call/cc? #f)
	 (default-walk! this <-call/cc?)
	 (let ((case-call/cc? (unbox <-call/cc?)))
	    ;; update the return-value
	    (box-set! <-call/cc? (or tmp-call/cc? case-call/cc?))
	    (cond
	       ;; if the case does not contain any call/cc there is nothing to
	       ;; do.
	       ((not case-call/cc?)
		this)
	       ;; else we have make sure that the key is actually a
	       ;; constant variable. This way a restoration will reenter
	       ;; the same branch.
	       ((and (Ref? key)
		     (Var-constant? (Ref-var key)))
		;; note: implies that the call/cc was in the clauses
		this)
	       (else ;; introduce temporary variable...
		(let* ((r (Ref-of-new-Var 'ctmp))
		       (var (Ref-var r))
		       (binding (var-assig var key)))
		   (set! key r)
		   (instantiate::Let
		      (scope-vars (list var))
		      (bindings (list binding))
		      (body this)
		      (kind 'let)))))))))

;; resembles Case.
(define-nmethod (If.a-normal! <-call/cc?)
   (with-access::If this (test then else)
      (let ((tmp-call/cc? (unbox <-call/cc?)))
	 (box-set! <-call/cc? #f)
	 (default-walk! this <-call/cc?)
	 (let ((if-call/cc? (unbox <-call/cc?)))
	    ;; update the return-value
	    (box-set! <-call/cc? (or tmp-call/cc? if-call/cc?))

	    ;; if the if does not contain any call/cc then there is nothing
	    ;; to do.
	    (cond
	       ((not if-call/cc?)
		this)
	       ;; otherwise the test must be a constant variable.
	       ((and (Ref? test)
		     (Var-constant? (Ref-var test)))
		;; note: implies that the call/cc was in one of the branches.
		this)
	       (else ;; create a temporary constant variable.
		(let* ((r (Ref-of-new-Var 'ctmp))
		       (var (Ref-var r))
		       (binding (var-assig var test)))
		   (set! test r)
		   (instantiate::Let
		      (scope-vars (list var))
		      (bindings (list binding))
		      (body this)
		      (kind 'let)))))))))

(define-nmethod (Lambda.a-normal! <-call/cc?)
   ;; lambda shields from <-call/cc?s
   (default-walk! this (make-box #f)))
