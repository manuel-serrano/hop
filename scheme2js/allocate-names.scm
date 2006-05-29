(module allocate-names
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   var
	   verbose
	   gen-js)
   (export (gen-var-names tree)))

(define (gen-var-names tree)
   (define *reusable-var-names* '())

   (define-pmethod (Node-name-gen)
      (if this.live-begin-vars
	  (for-each (lambda (var)
		       (var.allocate-name #t))
		    this.live-begin-vars))
      (this.traverse0)
      (if this.live-end-vars
	  (for-each (lambda (var)
		       (var.free-name))
		    this.live-end-vars)))

   (define-pmethod (Var-ref-name-gen)
      (for-each (lambda (var)
		   (var.allocate-name #t))
		(or this.live-begin-vars '()))
      (let ((var this.var))
	 (if (not var.compiled) ;; imported, captured or something else
	     (var.allocate-name #f)))
      (for-each (lambda (var)
		   (var.free-name))
		(or this.live-end-vars '())))

   (define-pmethod (Var-allocate-name use-generic-name?)
      (cond
	 (this.compiled
	  'do-nothing)
	 (this.is-global?
	  (set! this.compiled (mangle-JS-sym this.id)))
	 ((and (config 'optimize-var-number)
	       use-generic-name?)
	  (if (null? *reusable-var-names*)
	      (set! this.compiled (gen-JS-sym 'var))
	      (begin
		 (set! this.compiled (car *reusable-var-names*))
		 (set! *reusable-var-names* (cdr *reusable-var-names*)))))
	 (else
	  (set! this.compiled (gen-JS-sym this.id)))))

   ; (define-pmethod (Var-allocate-name)
   ;    (let ((compiled this.compiled))
   ;       (or compiled
   ; 	  (let ((compiled (gen-code-var this.id)))
   ; 	     (set! this.compiled compiled)
   ; 	     compiled))))
   
   (define-pmethod (JS-Var-allocate-name use-generic-name?)
      (set! this.compiled (symbol->string this.js-id)))

   (define-pmethod (JS-This-Var-allocate-name use-generic-name?)
      (set! this.compiled "this"))

   (define-pmethod (Var-free-name)
      (set! *reusable-var-names* (cons this.compiled *reusable-var-names*)))

   (define-pmethod (JS-Var-free-name)
      'do-nothing)

   ;; ===================================================================
   ;; procedure starts here
   ;; ===================================================================
   (verbose "  generating names for vars")
   (set! *reusable-var-names* '())
   (overload traverse name-gen (Node Var-ref)
	     (overload allocate-name allocate-name (Var JS-Var JS-This-Var)
		       (overload free-name free-name (Var JS-Var)
				 (tree.traverse))))
   (set! *reusable-var-names* '()))
