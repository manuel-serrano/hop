(module var-elimination
   (import config
	   tools
	   nodes
	   walk
	   var-ref-util
	   side
	   use-count
	   verbose)
   (export (var-elimination! tree::Module)))

;; TODO TODO TODO: rewrite pass.

;; uses While -> after while-pass
;;
;; suppose the following case:
;;   (lambda (x)
;;      (let ((y x))
;;          .. y ..
;;
;; then 'x' can be replaced by 'y'.
;;   (lambda (y)
;;          .. y ..

;; this pass is really just an approximation: If and Case are not treated
;; specially -> if there's a call/cc in one early branch, then the other branch
;; can't reuse the variables.

(define (var-elimination! tree)
   (when (config 'var-elimination)
      (verbose "var-elimination")
      (side-effect tree)
      (use-count tree)
      (elim! tree #f #f #f)))

(define (var-is-available? var)
   (and (=fx var.uses 1)
	var.constant?
	(not var.escapes?)))
   
(define-nmethod (Node.elim! avail-vars call/cc?)
   (default-walk! this avail-vars call/cc?))

(define-nmethod (Module.elim! avail-vars call/cc?)
   (default-walk! this (make-eq-hashtable) (cons #f #f)))

(define-nmethod (Lambda.elim! avail-vars call/cc?)
   (with-access::Lambda this (formals scope-vars)
      (let ((fun-ht (make-eq-hashtable)))
	 (let loop ((formals formals))
	    (unless (null? formals)
	       (with-access::Ref (car formals) (var)
		  (when (var-is-available? var)
		     (hashtable-put! fun-ht var
				     (lambda (new-lvar)
					(let ((ref (var-reference new-lvar)))
					   (widen!::Replaced-Var var)
					   (set-car! formals ref))))
		     (loop (cdr formals))))))
	 (default-walk! this fun-ht (cons #f #f))
	 (set! scope-vars Ref-var formals))
      this))

;; TODO TODO TODO.
(define-pmethod (While-var-elim! avail-vars call/cc?)
   ;; even if the avail-vars are used only once in the while loop, due to the
   ;; iterations the use-count is actually multiplied by each iteration. -> we
   ;; can't reuse the vars.
   (this.traverse2! (make-eq-hashtable) call/cc?))

(define-pmethod (Let-var-elim! avail-vars call/cc?)
   (this.traverse2! avail-vars call/cc?)
   (set! this.scope-vars (filter! (lambda (var)
				     (not var.replaced?))
				  this.scope-vars))
   this)

(define-pmethod (Call/cc-Call-var-elim! avail-vars call/cc?)
   (this.traverse2! avail-vars call/cc?)
   (set-car! call/cc? #t)
   (set-cdr! call/cc? #t)
   this)

(define-pmethod (Binding-var-elim! avail-vars call/cc?)
   (define (replace-lvalue! new-lvar)
;      (verbose "replacing " this.lvalue.var.id " by " new-lvar.id)
      (set! this.lvalue.var.replaced? #t)
      (set! this.lvalue (new-lvar.reference)))
   
   (let ((lvar this.lvalue.var)
	 (after-call/cc? (car call/cc?)))
      (this.traverse2! avail-vars call/cc?)
      (let ((rvar (and (inherits-from? this.val (node 'Var-ref))
		       this.val.var)))
	 (cond
	    ((and (not after-call/cc?)
		  rvar
		  (hashtable-get avail-vars rvar)
		  (not rvar.escapes?)
		  (not lvar.needs-frame?)
		  (not lvar.needs-boxing?)
		  (not lvar.needs-uniquization?)
		  (not rvar.needs-frame?)
		  (not rvar.needs-boxing?)
		  (not rvar.needs-uniquization?))
;	     (verbose "replacing " rvar.id " by " lvar.id)
	     (let ((replace-fun! (hashtable-get avail-vars rvar)))
		(replace-fun! lvar)
		(when (var-is-available? lvar)
		   ;; if we get replaced, directly replace
		   (hashtable-put! avail-vars lvar replace-fun!))
		(new-node Const #unspecified)))
	    (else
	     (when (var-is-available? lvar)
		(hashtable-put! avail-vars lvar replace-lvalue!))
	     this)))))
