(module var-ref-util
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes)
   (export (constant-var n)
	   (runtime-var n)
	   (call-target operator::pobject)
	   (runtime-var-ref-id n)
	   (runtime-var-ref? n)
	   (higher-order-runtime-var-ref? n)))

(define (constant-var n)
   (and n
	(inherits-from? n (node 'Var-ref))
	n.var.constant?
	n.var))

(define (runtime-var n)
   (let ((v (constant-var n)))
      (and v
	   v.runtime?
	   v)))

(define (runtime-var-ref? n)
   (and (runtime-var n)
	#t))

(define (higher-order-runtime-var-ref? n)
   (let ((v (runtime-var n)))
      (and v
	   v.higher?
	   #t)))

(define (runtime-var-ref-id n)
   (let ((v (runtime-var n)))
      (and v
	   v.id)))

(define (call-target operator)
   (cond
      ((inherits-from? operator (node 'Lambda))
       operator)
      ((runtime-var-ref-id operator)
       operator)
      ((and (inherits-from? operator (node 'Var-ref))
	    operator.var.constant?
	    operator.var.value)
       (call-target operator.var.value))
      (else
       #f)))
