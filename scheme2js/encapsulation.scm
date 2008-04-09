(module encapsulation
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   symbol
	   nodes
	   var
	   config
	   verbose)
   (export (encapsulation! tree::pobject)))

;; if we are encapsulating the modules (ie putting it into an anonymous
;; function), then we need/want a 'return' in the anonymous function.
;; don't forget to transmit the 'this' either.
;; js-call is not inlined in later passes (otherwise we would need to flag this
;; node)
(define (encapsulation! tree::pobject)
   (when (or (config 'encapsulate-modules)
	     (config 'suspend/resume))
      (verbose " encapsulation")
      (let* ((encapsulated-body (new-node Return tree.body))
	     (encapsulation-lambda (new-node Lambda
					     '()
					     #f
					     encapsulated-body))
	     (call (new-node Call
			     (runtime-reference 'js-call)
			     (list ((new-node JS-This-Var).reference)
				   encapsulation-lambda))))
	 (set! encapsulation-lambda.scope-vars '())
	 (set! tree.body call))))
