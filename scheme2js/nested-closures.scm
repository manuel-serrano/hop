(module nested-closures
   (import nodes
	   walk
	   captured-vars
	   verbose)
   (export (nested-closures tree::Module)))


(define (nested-closures tree)
   (verbose " nested-closures")
   (captured-vars tree)
   (nested tree #f #f))

(define-nmethod (Node.nested surrounding-fun)
   (default-walk this surrounding-fun))

(define-nmethod (Lambda.nested surrounding-fun)
   (with-access::Lambda this (closure?)
      (if (and surrounding-fun
	       closure?)
	  (Lambda-nested-closures?-set! surrounding-fun #t)))
   (default-walk this this))
