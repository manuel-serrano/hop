(module deep-clone
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   var
	   verbose)
   (export (deep-clone o::pobject)))

(define (deep-clone o)
   ;; variables and labels are only to clone, if they are in the scope.
   ;; 'to-clone' keeps track of them.
   (define to-clone (make-eq-hashtable))
   
   (define (to-clone-add! ht)
      (hashtable-for-each ht
			  (lambda (key val)
			     (hashtable-put! to-clone key #t))))
   
   (define-pmethod (Var-deep-clone cloned-ht)
      (if (hashtable-get to-clone this)
	  (pcall this pobject-deep-clone cloned-ht)
	  this))

   (define-pmethod (Lambda-deep-clone cloned-ht)
      (to-clone-add! this.local-vars-ht)
      (pcall this pobject-deep-clone cloned-ht))

   (define-pmethod (Call-deep-clone cloned-ht)
      (let ((cloned-fun this.cloned-fun))
	 (delete! this.cloned-fun)
	 (let ((res (pcall this pobject-deep-clone cloned-ht)))
	    (if cloned-fun
		(set! this.cloned-fun cloned-fun))
	    res)))

   (define-pmethod (Label-deep-clone cloned-ht)
      (if (hashtable-get to-clone this)
	  (let ((new-this (pcall this pobject-deep-clone cloned-ht)))
	     (set! new-this.id (gensym 'label))
	     new-this)
	  this))
      
   (define-pmethod (Labelled-deep-clone cloned-ht)
      (hashtable-put! to-clone this.label #t)
      (pcall this pobject-deep-clone cloned-ht))

   (define-pmethod (Tail-rec-deep-clone cloned-ht)
      (hashtable-put! to-clone this.label #t)
      (pcall this pobject-deep-clone cloned-ht))

   ;; not needed (if I'm not wrong...)
   ;(to-clone-add! fun.local-vars)
   (overload deep-clone deep-clone (Var
				    Lambda
				    Call
				    Label
				    Labelled
				    Tail-rec)
	     (o.deep-clone (make-eq-hashtable))))
