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
   (export (gen-var-names tree)
	   (call/cc-indicator-name index)))

(define (call/cc-indicator-name index)
   (string-append "sc_callccIndicator" (number->string index) "_"))

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

   (define-pmethod (Closure-ref-name-gen)
      (for-each (lambda (var)
		   (var.allocate-name #t))
		(or this.live-begin-vars '()))
      (this.traverse0)
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
		 (set! *reusable-var-names* (cdr *reusable-var-names*))))
	  (set! this.reusable? #t))
	 (else
	  (set! this.compiled (gen-JS-sym this.id)))))

   (define-pmethod (Field-Var-allocate-name use-generic-name?)
      (this.obj.allocate-name #f)
      (this.field.allocate-name #f)
      (if this.compiled
	  'do-nothing
	  (set! this.compiled
		(string-append this.obj.compiled "." this.field.compiled))))

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

   (define-pmethod (Call/cc-indicator-Var-allocate-name use-generic-name?)
      (set! this.compiled (call/cc-indicator-name this.indicator-index)))

   (define-pmethod (Var-free-name)
      (if this.reusable?
	  (set! *reusable-var-names* (cons this.compiled
					   *reusable-var-names*)))
      (delete! this.reusable?))

   ;; ===================================================================
   ;; procedure starts here
   ;; ===================================================================
   (verbose "  generating names for vars")
   (set! *reusable-var-names* '())
   (overload traverse name-gen (Node Var-ref Closure-ref)
	     (overload allocate-name allocate-name (Var Field-Var JS-Var
							JS-This-Var
							Call/cc-indicator-Var)
		       (overload free-name free-name (Var)
				 (tree.traverse))))
   (set! *reusable-var-names* '()))
