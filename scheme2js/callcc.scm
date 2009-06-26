;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module callcc
   (import config
	   nodes
	   walk
	   side
	   free-vars
	   tail
	   mark-statements
	   var-ref-util
	   verbose
	   node-elimination
	   tools
	   export-desc
	   callcc-a-normal-form
	   callcc-check-point
	   callcc-locations
	   callcc-resume-push)
   (export (final-class Call/cc-Scope-Info
	      (id::symbol read-only)
	      (vars::pair-nil (default '())) ;; might contain indirect vars!
	      (indices::pair-nil (default '()))

	      (finally-vars::pair-nil (default '()))
	      (surrounding-while read-only)

	      (counter-id-nb (default #f)))) ;; only applies to Whiles
	   
   (export (call/cc-early! tree::Module)
	   (call/cc-middle tree::Module)
	   (call/cc-late! tree::Module)
	   ))

(define (call/cc-early! tree)
   (when (config 'suspend/resume)
      (verbose "call/cc early")
      (call/cc-locations tree)
      (call/cc-a-normal-form! tree)
      (call/cc-check-point tree)))


;; find variables that have to be saved, ... when a call/cc occurs.
;; This has to happen late, after temporary variables have been introduced, but
;; still before the scope-flattening pass.
(define (call/cc-middle tree)
   (when (config 'suspend/resume)
      (verbose "call/cc middle (scoping)")
      (call/cc-mark-instrumented tree)
      (call/cc-scoping tree)))

;; the statement-pass introduces new Set!s. We shift the call/cc information
;; down to the Set!s.
(define (call/cc-late! tree)
   (when (config 'suspend/resume)
      (verbose "call/cc late")
      (call/cc-resume-push! tree)
      (remove-While-inits! tree)
      (node-elimination! tree)
      (call/cc-ranges tree)))

;; just reupdate the call/cc? fields in scopes. Contrary to before we only mark
;; scopes that actually need instrumenting. In particulary tail-calls are
;; ignored now.
(define (call/cc-mark-instrumented tree)
   (mark tree #f #f))

(define-nmethod (Node.mark surrounding-scope)
   (default-walk this surrounding-scope))

(define-nmethod (Scope.mark surrounding-scope)
   (with-access::Scope this (call/cc?)
      (set! call/cc? #f)
      (default-walk this this)
      (when (and call/cc? surrounding-scope)
	 (with-access::Scope surrounding-scope (call/cc?)
	    (set! call/cc? #t)))))

(define-nmethod (Let.mark surrounding-scope)
   (with-access::Let this (bindings body call/cc?)
      (set! call/cc? #f)
      (for-each (lambda (binding)
		   (walk binding surrounding-scope))
		bindings)
      (walk body this)
      (when (and call/cc? surrounding-scope)
	 (with-access::Scope surrounding-scope (call/cc?)
	    (set! call/cc? #t)))))

(define-nmethod (Lambda.mark surrounding-scope)
   (with-access::Scope this (call/cc?)
      (set! call/cc? #f)
      (default-walk this this))) ;; do not propagate call/cc

(define-nmethod (Call.mark surrounding-scope)
   (default-walk this surrounding-scope)
   (with-access::Call this (call/cc? call/cc-index)
      ;; ignore tail-calls (which do not have any call/cc-index)
      (when (and call/cc? call/cc-index surrounding-scope)
	 (with-access::Scope surrounding-scope (call/cc?)
	    (set! call/cc? #t)))))


(define (finally-var? var)
   (and (not (Var-indirect? var))
	(Var-needs-update? var)))

(define (call/cc-scoping tree)
   (scope tree #f #f #f '()))

(define-nmethod (Node.scope surrounding-fun surrounding-while scopes)
   (default-walk this surrounding-fun surrounding-while scopes))

(define-nmethod (Lambda.scope surrounding-fun surrounding-while scopes)
   (with-access::Lambda this (call/cc? call/cc-finally-scopes
				       call/cc-contained-scopes
				       call/cc-nb-while-counters
				       scope-vars)
      ;; Note: function scope-vars can not be indirect. (Otherwise they would
      ;; have been replaced by temporary variables).
      (if (and call/cc? (not (null? scope-vars)))
	  (let ((scope-info (instantiate::Call/cc-Scope-Info
			       (id (gensym 'lambda))
			       (vars scope-vars)
			       (indices '())
			       (surrounding-while #f)
			       (finally-vars (filter finally-var?
						     scope-vars)))))
	     (when (not (null? (Call/cc-Scope-Info-finally-vars scope-info)))
		(set! call/cc-finally-scopes (list scope-info)))
	     
	     (set! call/cc-contained-scopes (list scope-info))

	     (set! call/cc-nb-while-counters 0)
	     
	     (default-walk this this #f call/cc-contained-scopes))
	  (begin
	     (set! call/cc-contained-scopes '())
	     (set! call/cc-finally-scopes '())
	     (set! call/cc-nb-while-counters 0)
	     (default-walk this this #f '())))))

;; we set the call/cc-counter-id here too.
(define-nmethod (While.scope surrounding-fun surrounding-while scopes)
   (with-access::While this (call/cc? call/cc-finally-scopes
				       scope-vars call/cc-counter-nb)
      (if (not call/cc?)
	  (default-walk this surrounding-fun this scopes)
	  (with-access::Lambda surrounding-fun (call/cc-contained-scopes
						call/cc-nb-while-counters)
	     [assert (surrounding-fun) surrounding-fun]
	     (set! call/cc-counter-nb call/cc-nb-while-counters)
	     (set! call/cc-nb-while-counters (+fx 1 call/cc-counter-nb))

	     (let* ((to-save-vars (filter (lambda (var) (not (Var-indirect? var)))
					  scope-vars))
		    (finally-vars (filter finally-var? to-save-vars)))

		;; even when there are no variables to save. there is always
		;; the loop-counter.
		;; TODO: but there is no need, when we only have suspend/resume.
		(let ((scope-info (instantiate::Call/cc-Scope-Info
				     (id (gensym 'while))
				     (vars to-save-vars)
				     (indices '())
				     (surrounding-while this)
				     (finally-vars finally-vars)
				     (counter-id-nb call/cc-counter-nb))))

		       (when (not (null? finally-vars))
			  (set! call/cc-finally-scopes (list scope-info)))

		       (cons-set! call/cc-contained-scopes scope-info)

		       (default-walk this surrounding-fun this
			             (cons scope-info scopes))))))))

(define-nmethod (Let.scope surrounding-fun surrounding-while scopes)
   (with-access::Let this (call/cc? scope-vars bindings body)
      (if (not call/cc?)
	  (default-walk this surrounding-fun surrounding-while scopes)
	  (let* ((to-save-vars (filter (lambda (var) (not (Var-indirect? var)))
				       scope-vars))
		 (finally-vars (filter finally-var? to-save-vars)))
	     (if (null? to-save-vars)
		 (default-walk this surrounding-fun surrounding-while scopes)
		 ;; letrecs have been expanded so they can not have any call/cc
		 ;; in the right-hand-side. -> no need to look at kind. We can
		 ;; always treet the Let as if it was a 'let (and not 'letrec).
		 (let ((scope-info (instantiate::Call/cc-Scope-Info
				      (id (gensym 'let))
				      (vars to-save-vars)
				      (indices '())
				      (surrounding-while surrounding-while)
				      (finally-vars finally-vars))))
		    (when (not (null? finally-vars))
		       (if surrounding-while
			   (with-access::While surrounding-while
				 (call/cc-finally-scopes)
			      (cons-set! call/cc-finally-scopes scope-info))
			   (with-access::Lambda surrounding-fun
				 (call/cc-finally-scopes)
			      (cons-set! call/cc-finally-scopes scope-info))))
		    
		    [assert (surrounding-fun) surrounding-fun]
		    (with-access::Lambda surrounding-fun
			  (call/cc-contained-scopes)
		       (cons-set! call/cc-contained-scopes scope-info))
		    
		    (for-each (lambda (binding)
				 (walk binding surrounding-fun
				       surrounding-while scopes))
			      bindings)
		    (walk body surrounding-fun surrounding-while
			  (cons scope-info scopes))))))))

(define-nmethod (Call.scope surrounding-fun surrounding-while scopes)
   (with-access::Call this (call/cc? call/cc-index)
      (default-walk this surrounding-fun surrounding-while scopes)
      (when (and call/cc? call/cc-index) ;; ignore tail calls.
	 (for-each (lambda (scope)
		      (with-access::Call/cc-Scope-Info scope (indices)
			 (cons-set! indices call/cc-index)))
		   scopes))))

;; While nodes must not have Inits, as they can't be skipped this way.
(define (remove-While-inits! tree)
   (verbose " remove While inits")
   (rm! tree #f))

(define-nmethod (Node.rm!)
   (default-walk! this))

(define-nmethod (While.rm!)
   (default-walk! this)
   (with-access::While this (call/cc? init)
      (cond
	 ((not call/cc?)
	  this)
	 ((Const? init)
	  this)
	 (else
	  (let ((old-init init))
	     (set! init (instantiate::Const
			   (location (Node-location this))
			   (value #unspecified)))
	     (instantiate::Begin
		(exprs (list old-init this))))))))

;; In our current implementation Begin's are the only way to skip expressions.
(define (call/cc-ranges tree)
   (verbose " call/cc ranges")
   (ranges tree #f #f))

;; TODO: containers should be sorted...
(define (make-range-container)
   (cons 'ranges '()))

;; result will be in cont1
(define (range-merge! cont1 cont2)
   (when cont1
      (let ((l1 (cdr cont1))
	    (l2 (cdr cont2)))
	 (let loop ((l l2)
		    (res l1))
	    (cond
	       ((null? l)
		(set-cdr! cont1 res))
	       ((memq (car l) l1)
		(loop (cdr l) res))
	       (else
		(loop (cdr l) (cons (car l) res))))))))

(define (range-add! cont1 is)
   (when cont1
      (for-each (lambda (i)
		   (unless (memq i (cdr cont1))
		      (set-cdr! cont1 (cons i (cdr cont1)))))
		is)))

(define (container-ranges cont)
   (cdr cont))

(define-nmethod (Node.ranges container)
   (default-walk this container))

(define-nmethod (Begin.ranges container)
   (with-access::Begin this (exprs call/cc? call/cc-ranges)
      (set! call/cc-ranges
	    (map (lambda (exp)
		    (let ((c (make-range-container)))
		       (walk exp c)
		       (when (not (null? (container-ranges c)))
			  (set! call/cc? #t)
			  (range-merge! container c)
			  (container-ranges c))))
		 exprs))
      (when (not call/cc?)
	 ;; free memory
	 (set! call/cc-ranges '()))))

(define-nmethod (Execution-Unit.ranges container)
   (default-walk this #f))

(define-nmethod (Call/cc-Resume.ranges container)
   (default-walk this container)
   (with-access::Call/cc-Resume this (indices)
      (range-add! container indices)))
