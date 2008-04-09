(module compile-optimized-boolify
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (export (compile-boolified  p
			       node::pobject))
   (import protobject
	   config
	   nodes
	   var
	   verbose))

(define (compile-optimized-if-boolify p n)
   (if (and (inherits-from? n.else (node 'Const))
	    (not n.else.value))
       (begin
	  (p-display p "(")
	  (n.test.compile p)
	  (p-display p "&&")
	  (compile-optimized-boolify p n.then)
	  (p-display p ")"))
       (compile-unoptimized-boolify p n)))

(define (compile-optimized-boolify p n)
   (cond
      ((inherits-from? n (node 'Call))
       (let ((op n.operator))
	  (if (inherits-from? op (node 'Var-ref))
	      (let* ((var op.var))
		 (if (and var.constant?
			  (eq? var.return-type 'bool))
		     (n.compile p)
		     (compile-unoptimized-boolify p n)))
	      (compile-unoptimized-boolify p n))))
      ((inherits-from? n (node 'If))
       (compile-optimized-if-boolify p n))
      ((inherits-from? n (node 'Const))
       (if (not n.value)
	   (p-display p "false")
	   (p-display p "true")))
      (else
       (compile-unoptimized-boolify p n))))

(define (compile-unoptimized-boolify p node)
   (p-display p "(")
   (node.compile p)
   (p-display p "!== false)"))
   
(define (compile-boolified p node)
   (if (config 'optimize-boolify)
       (compile-optimized-boolify p node)
       (compile-unoptimized-boolify p node)))

