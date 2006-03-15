;; $Id: collect-vars.scm 130 2006-03-02 17:28:56Z flo $
(module collect-vars
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   verbose)
   (export (collect-vars tree::pobject)))

(define (collect-vars tree::pobject)
   (verbose " collect-vars")
   (overload traverse collect-vars (Node
				    Part
				    Lambda
				    Decl)
	     (tree.traverse #f)))

(define-pmethod (Node-collect-vars ht)
   (this.traverse1 ht))
      
(define-pmethod (Part-collect-vars ht)
   (let ((collected-vars-ht (make-eq-hashtable)))
      (set! this.collected-vars collected-vars-ht)
      (this.traverse1 collected-vars-ht)))

(define-pmethod (Lambda-collect-vars ht)
   (let ((collected-vars-ht (make-eq-hashtable)))
      (set! this.collected-vars collected-vars-ht)
      ;; don't go into formals and vaarg.
      ;; we only collect local-vars.
      (this.body.traverse collected-vars-ht)))

(define-pmethod (Decl-collect-vars ht)
   (hashtable-put! ht this.var #t))
