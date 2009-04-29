(module push-declarations
   (import nodes
	   export-desc
	   config
	   walk
	   tools
	   verbose)
   (export (wide-class Decl-Set!::Set!))
   (export (push-var-declarations tree::Module)))

;; tries to push var declarations to assignments.
;; instead of having:
;;   var x;
;;   x = 3;
;; we want
;;   var x = 3;
;;
;; We don't try to do fancy stuff here and we miss many opportunities, but in
;; the end this pass is not very important and we thus keep it simple.

(define (push-var-declarations tree)
   (when (config 'compress) ;; TODO create var-declaration push flag?
      (verbose "   push var-declarations")
      (push tree #f #f #f)))

(define-nmethod (Node.push undeclared-vars-ht stmt?)
   (default-walk this undeclared-vars-ht #f))

(define-nmethod (Module.push undeclared-vars-ht stmt?)
   (with-access::Module this (declared-vars scope-vars body)
      ;; declared-vars includes exported vars
      (let ((ht (make-eq-hashtable)))
	 (for-each (lambda (var)
		      (hashtable-put! ht var #t))
		   declared-vars)
	 (walk body ht #t)
	 (set! declared-vars (hashtable-key-list ht))
	 (set! scope-vars (filter (lambda (var)
				     (hashtable-get ht var))
				  scope-vars)))))

(define-nmethod (Lambda.push undeclared-vars-ht stmt?)
   (with-access::Lambda this (declared-vars body)
      (let ((ht (make-eq-hashtable)))
	 (for-each (lambda (var)
		      (hashtable-put! ht var #t))
		   declared-vars)
	 (walk body ht #t)
	 (set! declared-vars (hashtable-key-list ht)))))

;; if the variable has not yet been declared and this assignment is a stmt,
;; then use it to declare the var.
(define-nmethod (Set!.push ht stmt?)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (when (and stmt?
		    (hashtable-get ht var))
	    (widen!::Decl-Set! this)
	    (hashtable-remove! ht var)))
      (walk val ht #f)))

(define-nmethod (If.push ht stmt?)
   (with-access::If this (test then else)
      (walk test ht #f)
      (walk then ht stmt?)
      (walk else ht stmt?)))

(define-nmethod (Case.push ht stmt?)
   (with-access::Case this (key clauses)
      (walk key ht #f)
      (for-each (lambda (clause)
		   (walk clause ht #t))
		clauses)))

(define-nmethod (Begin.push ht stmt?)
   (default-walk this ht stmt?))

(define-nmethod (Labeled.push ht stmt?)
   (default-walk this ht #t))

(define-nmethod (While.push ht stmt?)
   (with-access::While this (init test body label)
      (walk init ht stmt?)
      (walk test ht #f)
      (walk body ht #t)))
