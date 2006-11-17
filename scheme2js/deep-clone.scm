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
   (define to-clone (make-eq-hashtable))
   
   (define label-ht (make-eq-hashtable))
   (define (label-map label)
      (or (hashtable-get label-ht label)
	  (let ((label-replacement (gensym 'label)))
	     (hashtable-put! label-ht label-replacement label-replacement)
	     (hashtable-put! label-ht label label-replacement)
	     label-replacement)))
   
   (define (to-clone-add! ht)
      (hashtable-for-each ht
			  (lambda (key val)
			     (hashtable-put! to-clone key #t))))
   
   (define-pmethod (Var-deep-clone cloned-ht)
      (if (hashtable-get to-clone this)
	  (pcall this pobject-deep-clone cloned-ht)
	  this))

   (define-pmethod (Lambda-deep-clone cloned-ht)
      (to-clone-add! this.local-vars)
      (pcall this pobject-deep-clone cloned-ht))

   (define-pmethod (Call-deep-clone cloned-ht)
      (let ((cloned-fun this.cloned-fun))
	 (delete! this.cloned-fun)
	 (let ((res (pcall this pobject-deep-clone cloned-ht)))
	    (if cloned-fun
		(set! this.cloned-fun cloned-fun))
	    res)))

   (define-pmethod (Tail-rec-deep-clone cloned-ht)
      (let ((res (pcall this pobject-deep-clone cloned-ht)))
	 (set! res.label (label-map res.label))
	 res))

   (define-pmethod (Tail-rec-call-deep-clone cloned-ht)
      (let ((res (pcall this pobject-deep-clone cloned-ht)))
	 (set! res.label (label-map res.label))
	 res))

   (define-pmethod (Labelled-deep-clone cloned-ht)
      (let ((res (pcall this pobject-deep-clone cloned-ht)))
	 (set! res.label (label-map res.label))
	 res))

   ;; not needed (if I'm not wrong...)
   ;(to-clone-add! fun.local-vars)
   (overload deep-clone deep-clone (Var
				    Lambda
				    Call
				    Tail-rec
				    Tail-rec-call
				    Labelled)
	     (o.deep-clone (make-eq-hashtable))))
