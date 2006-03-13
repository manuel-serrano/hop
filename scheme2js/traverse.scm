;; $Id: traverse.scm 126 2006-02-22 11:22:58Z flo $
(module traverse
   (include "protobject.sch")
   (include "nodes.sch")
   (import protobject
	   nodes
	   verbose)
   (export (traverse! tree::pobject)
	   (traverse tree::pobject)))

;; a debug module. testing the integrity of the nodes.

(define (traverse! tree::pobject)
   (verbose "traverse!")
   (overload traverse! traverse! (Node)
	     (tree.traverse!)))

(define (traverse tree::pobject)
   (verbose "traverse")
   (overload traverse traverse (Node)
	     (tree.traverse)))

(define-pmethod (Node-traverse!)
   (this.traverse0!))

(define-pmethod (Node-traverse)
   (this.traverse0))
   
