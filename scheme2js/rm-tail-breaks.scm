(module rm-tail-breaks
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   verbose)
   (export (rm-tail-breaks! tree::Module)))

;; the name is actually misleading, as continues, too, are removed.

;; remove tail-continues.
;; ex:
;; label: while (test) {
;;          xxx
;;          continue label;
;;        }
;; or
;; label: {
;;    blabla
;;    break label;
;; }
;;
;; Also replace labels if the default-label is fine.
;;
;; In theory all breaks and continues are in tail-position, but as I'm not
;; entirely sure and (more importantly) as this may change, we verify
;; tail-position.

(define (rm-tail-breaks! tree)
   (verbose " rm tail breaks")
   (tail! tree #f #f #f)
   (default! tree #f #f #f))


;; if a label is given, then a break/continue would be in tail-position
(define-nmethod (Node.tail! break-label while-label)
   (default-walk! this break-label while-label))

(define-nmethod (Lambda.tail! break-label while-label)
   (default-walk! this #f #f))

(define-nmethod (If.tail! break-label while-label)
   (with-access::If this (test then else)
      (set! test (walk! test #f #f))
      (set! then (walk! then break-label while-label))
      (set! else (walk! else break-label while-label)))
   this)

(define-nmethod (Case.tail! break-label while-label)
   (with-access::Case this (key clauses)
      (set! key (walk! key #f #f))
      (set! clauses (map! (lambda (clause)
			     (walk! clause break-label while-label))
			  clauses))
      this))

;; Clause does not need special treatment. (Consts do nothing with 'tail?', and
;; expr follows default rule

(define-nmethod (Set!.tail! break-label while-label)
   (default-walk! this #f #f))

(define-nmethod (Let.tail! break-label while-label)
   (with-access::Let this (bindings body)
      (set! bindings (map! (lambda (b) (walk! b #f #f))
			   bindings))
      (set! body (walk! body break-label while-label))
      this))

(define-nmethod (Begin.tail! break-label while-label)
   (with-access::Begin this (exprs)
      (let loop ((exprs exprs))
	 (cond
	    ((null? exprs)
	     'done)
	    ((null? (cdr exprs))
	     (set-car! exprs (walk! (car exprs) break-label while-label)))
	    (else
	     (set-car! exprs (walk! (car exprs) #f #f))
	     (loop (cdr exprs)))))
      this))

(define-nmethod (Call.tail! break-label while-label)
   (default-walk! this #f #f))

(define-nmethod (Return.tail! break-label while-label)
   (default-walk! this #f #f))

(define-nmethod (While.tail! break-label while-label)
   (with-access::While this (label)
      ;; test is exp. -> no 'continue'/'break'
      (default-walk! this #f label)))

(define-nmethod (Labeled.tail! break-label while-label)
   (with-access::Labeled this (label)
      (default-walk! this label while-label)))

(define-nmethod (Break.tail! break-label while-label)
   (with-access::Break this (label val)
      (if (eq? break-label label)
	  (default-walk! val break-label while-label)
	  (default-walk! this break-label while-label))))

(define-nmethod (Continue.tail! break-label while-label)
   (with-access::Continue this (label)
      (if (eq? while-label label)
	  (instantiate::Const (value #unspecified))
	  this)))


;; break-label will be different to #f only if a while is at tail-position of a
;; labeled.
(define-nmethod (Node.default! break-label while-label)
   (default-walk! this break-label while-label))

(define-nmethod (Module.default! break-label while-label)
   (default-walk! this #f #f))

(define-nmethod (Lambda.default! break-label while-label)
   (default-walk! this #f #f))

(define-nmethod (Case.default! break-label while-label)
   (default-walk! this #f while-label))

(define-nmethod (While.default! break-label while-label)
   (with-access::While this (label)
      (default-walk! this break-label label)))

(define-nmethod (Break.default! break-label while-label)
   (with-access::Break this (label)
      (when (eq? label break-label)
	 (set! label (default-label)))
      (default-walk! this break-label while-label)))

(define-nmethod (Continue.default! break-label while-label)
   (with-access::Continue this (label)
      (when (eq? while-label label)
	 (set! label (default-label)))
      this))
