(module constants
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (export (constants! tree::pobject))
   (import protobject
	   nodes
	   mark-statements
	   var
	   verbose))

(define *constants* '())

(define (constants! tree)
   (verbose "constants")
   (set! *constants* (make-eq-hashtable))
   (overload traverse! constants! (Node
				   Part
				   Const)
	     (tree.traverse!)))

(define-pmethod (Node-constants!)
   (this.traverse0!))

(define-pmethod (Part-constants!)
   (let ((old-constants *constants*))
      (set! *constants* (make-eq-hashtable))
      (let ((new-body (this.body.traverse!))
	    (assigs (hashtable-map *constants*
				   (lambda (const decl)
				      (let ((s (new-node Set!
							 decl
							 (new-node Const const))))
					 (mark-statement-form! s #t)
					 s)))))
	 (if (pair? assigs)
	     (let ((bnode (new-node Begin (append! assigs (list new-body)))))
		(mark-statement-form! bnode #t)
		(set! this.body bnode))
	     (set! this.body new-body)))
      (set! *constants* old-constants)
      this))

(define-pmethod (Const-constants!)
   (let ((value this.value))
      (if (or (pair? value)
	      (vector? value))
	  (let ((cached (hashtable-get *constants* value)))
	     (if cached
		 (cached.var.reference)
		 (let ((new-const (Decl-of-new-Var (gensym 'const))))
		    (hashtable-put! *constants* value new-const)
		    (new-const.var.reference))))
	  this)))
