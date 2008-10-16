(module letrec-expansion
   (import config
	   nodes
	   export
	   walk
	   verbose
	   gen-js)
   (export (letrec-expansion! tree::Module)))

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
;;           (x2 "some_string")
;;           (x3 (lambda (...) x0 x1 x2)))
;;      body)
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
   (letrec-expand tree #f)) ;; no environment.

(define-nmethod (Node.letrec-expand)
   (default-walk this))

(define (letrec-constant? n)
   (or (Const? n)
       (Ref? n)
       (Lambda? n)))

(define-nmethod (Let.letrec-expand)
   (default-walk this)
   (with-access::Let this (kind body bindings)
      (when (eq? kind 'letrec)
	 (let loop ((bindings bindings)
		    (body-assigs '()))
	    (cond
	       ((and (null? bindings)
		     (null? body-assigs))
		'do-nothing)
	       ((null? bindings)
		(set! body (instantiate::Begin
			      (exprs (append! body-assigs
					      (list body))))))
	       ((letrec-constant? (Set!-val (car bindings)))
		(loop (cdr bindings)
		      body-assigs))
	       (else
		(with-access::Set! (car bindings) (lvalue val)
		   (let ((old-val val))
		      (set! val (instantiate::Const (value #unspecified)))
		      (loop (cdr bindings)
			    (cons (var-assig (Ref-var lvalue) old-val)
				  body-assigs))))))))))
