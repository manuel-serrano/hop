(module constants
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (export (constants! tree::pobject))
   (import protobject
	   config
	   nodes
	   mark-statements
	   var
	   verbose))

(define (constants! tree)
   (verbose "constants")
   (when (config 'optimize-consts)
      (overload traverse! constants! (Node
				      Module
				      Lambda
				      Const)
		(tree.traverse! #f))))

(define-pmethod (Node-constants! constant-ht)
   (this.traverse1! constant-ht))

(define (make-constants-let constants-ht body)
   (let ((bindings (hashtable-map constants-ht
				  (lambda (const decl)
				     (new-node Binding
					       decl
					       (new-node Const const))))))
      (if (pair? bindings)
	  (new-node Let
		    (map (lambda (binding)
			    binding.lvalue.var)
			 bindings)
		    bindings
		    body
		    'let)
	  body)))
   
(define-pmethod (Module-constants! ht)
   (if (config 'encapsulate-modules)
       (this.traverse1! #f)
       (let ((ht (make-hashtable)))
	  (this.traverse1! ht)
	  (set! this.body (make-constants-let ht this.body))
	  this)))

(define-pmethod (Lambda-constants! ht)
   (if ht ;; either module or another lambda already created the ht)
       (this.traverse1! ht)
       (let* ((ht (make-hashtable)))
	  (this.traverse1! ht)
	  ;; body must be a Return. keep it that way.
	  (set! this.body.val (make-constants-let ht this.body.val))
	  this)))

(define-pmethod (Const-constants! constant-ht)
   (define (long-enough-list? l)
      (let loop ((l l)
		 (nb 0))
	 (cond
	    ((> nb 5) #t)
	    ((and (pair? l)
		  (or (pair? (car l))
		      (vector? (car l))))
	     #t)
	    ((pair? l)
	     (loop (cdr l) (+ nb 1)))
	    ((vector? l)
	     #t)
	    (else
	     #f))))
   (define (long-enough-vector? v)
      (or (> (vector-length v) 5)
	  (let loop ((i 0))
	     (cond
		((>= i (vector-length v))
		 #f)
		((or (pair? (vector-ref v i))
		     (vector? (vector-ref v i)))
		 #t)
		(else (loop (+ i 1)))))))
   (let ((value this.value))
      (if (or (and (pair? value)
		   (long-enough-list? value))
	      (and (vector? value)
		   (long-enough-vector? value)))
	  (let ((cached (hashtable-get constant-ht value)))
	     (if cached
		 (cached.var.reference)
		 (let ((new-const (Decl-of-new-Var 'const)))
		    (hashtable-put! constant-ht value new-const)
		    (new-const.var.reference))))
	  this)))
