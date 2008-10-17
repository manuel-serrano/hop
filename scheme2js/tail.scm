(module tail
   (import nodes
	   export-desc
	   walk
	   verbose)
   (export (wide-class Tail-Call::SCall))
   (static (wide-class Tail-Label::Label))
   (export (tail-calls tree::Module)))

;; might be called only after Node-elimination.
;; but then anytime. so don't assume nodes exist or not.
(define (tail-calls tree)
   (verbose "tail")
   (tail tree #f #f))

(define-nmethod (Node.tail tail?)
   (default-walk this #f)) ;; be conservative here.

(define-nmethod (Module.tail tail?)
   (default-walk this #t))

(define-nmethod (Lambda.tail tail?)
   (default-walk this #t))

(define-nmethod (If.tail tail?)
   (with-access::If this (test then else)
      (walk test #f)
      (walk then tail?)
      (walk else tail?)))

(define-nmethod (Case.tail tail?)
   (with-access::Case this (key clauses)
      (walk key #f)
      (for-each (lambda (clause)
		   (walk clause tail?))
		clauses)))

(define-nmethod (Clause.tail tail?)
   ;; default-walk is fine. (Consts do nothing with 'tail?')
   (default-walk this tail?))

(define-nmethod (Set!.tail tail?)
   (default-walk this #f))

(define-nmethod (Let.tail tail?)
   (with-access::Let this (bindings body)
      (for-each (lambda (n) (walk n #f)) bindings)
      (walk body tail?)))

(define-nmethod (Begin.tail tail?)
   (with-access::Begin this (exprs)
      (let loop ((exprs exprs))
	 (cond
	    ((null? exprs) 'do-nothing)
	    ((null? (cdr exprs))
	     (walk (car exprs) tail?))
	    (else
	     (walk (car exprs) #f)
	     (loop (cdr exprs)))))))

(define-nmethod (SCall.tail tail?)
   (cond
      (tail?
       (widen!::Tail-Call this))
      ((Tail-Call? this)
       (shrink! this)))
   (default-walk this #f))

(define-nmethod (Frame-alloc.tail tail?)
   (default-walk this tail?))

(define-nmethod (Return.tail tail?)
   (default-walk this #t))

(define-nmethod (Labeled.tail tail?)
   (with-access::Labeled this (label)
      (cond
	 (tail?
	  (widen!::Tail-Label label))
	 ((Tail-Label? label)
	  (shrink! label))))
   (default-walk this tail?))

(define-nmethod (Break.tail tail?)
   (with-access::Break this (label val)
      (if (Tail-Label? label)
	  (walk val #t)
	  (walk val #f))))

(define-nmethod (Tail-rec-Call.tail tail?)
   (default-walk this #f))

(define-nmethod (While.tail tail?)
   (default-walk this #f))

(define-nmethod (Call/cc-Call.tail tail?)
   (error #f "TODO Call/cc-Call.tail TODO" #f))

(define-nmethod (Call/cc-Resume.tail tail?)
   (error #f "TODO Call/cc-Resume.tail TODO" #f))

(define-nmethod (Call/cc-Counter-Update.tail tail?)
   (error #f "TODO Call/cc-Counter-Update.tail TODO" #f))
