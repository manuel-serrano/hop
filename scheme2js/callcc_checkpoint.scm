(module callcc-check-point
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   tail
	   callcc-locations
	   verbose)
   (export (call/cc-check-point! tree::pobject)))

;; simply replaces Calls that might reach a call/cc by a Call/cc-Call followed
;; by a Resume.
;; Each Call/cc call has an index (thus the really really bad name
;; "check-point").
;; Numbers always start at 1, and are linear. That is, a 'else' clause of an
;; 'if' must always have numbers higher than the 'then' branch.
;; This pass does not ensure, that the Resume node is at
;; statement-position (ie not inside a "Set!". This will be done in another
;; pass.
(define (call/cc-check-point! tree)
   (when (config 'suspend/resume)
      (verbose " call/cc check-point!")
      (tail-exprs tree
		  #f) ;; intermediate nodes are not considered to be tail.
      (overload traverse! check-point! (Node
					Module
					Lambda
					Call)
		(tree.traverse! #f))))

(define-pmethod (Node-check-point! index)
   (this.traverse1! index))

(define-pmethod (Module-check-point! index)
   (this.traverse1! (list 1)))

(define-pmethod (Lambda-check-point! index)
   (this.traverse1! (list 1)))

(define-pmethod (Call-check-point! index)
   (this.traverse1! index)

   (let ((unsafe-call? (and (not this.tail?)
			    this.call/cc?)))
      (delete! this.target)
      (delete! this.higher-targets)
      (delete! this.call/cc?)
      (if unsafe-call?
	  (let* ((call/cc-call (new-node Call/cc-Call
					 this.operator
					 this.operands)))
	     (set! call/cc-call.call/cc-index (car index))
	     (set-car! index (+ (car index) 1))
	     call/cc-call)
	  this)))

