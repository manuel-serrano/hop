(module callcc-check-point
   (import config
	   nodes
	   export-desc
	   walk
	   tail
	   callcc-locations
	   verbose
	   tools)
   (export (call/cc-check-point tree::Module)))

;; simply assigns an index variable to each call/cc-call.
;; the 'check-point' name is hence really bad. TODO: change name
;; Numbers always start at 1, and are linear. That is, a 'else' clause of an
;; 'if' must always have numbers higher than the 'then' branch.
;; This pass does not ensure, that the Resume node is at
;; statement-position (ie not inside a "Set!"). This will be done in another
;; pass.
(define (call/cc-check-point tree)
   (when (config 'suspend/resume)
      (verbose " call/cc check-point!")
      (tail-calls tree)
      (check-point tree #f #f)))

(define-nmethod (Node.check-point index)
   (default-walk this index))

(define-nmethod (Module.check-point index)
   (default-walk this (list 1)))

(define-nmethod (Lambda.check-point index)
   (with-access::Lambda this (call/cc-nb-indices)
      (let ((box (list 1)))
	 (default-walk this box)
	 (set! call/cc-nb-indices (-fx (car box) 1)))))

(define-nmethod (Call.check-point index)
   (default-walk this index)

   (with-access::Call this (call/cc? call/cc-index)
      (when call/cc?
	 (set! call/cc-index (car index))
	 (set-car! index (+fx call/cc-index 1)))))

(define-nmethod (Tail-Call.check-point index)
   (default-walk this index)

   ;; do not assign any index-var.
   (with-access::Call this (call/cc-index)
      (set! call/cc-index #f)))
