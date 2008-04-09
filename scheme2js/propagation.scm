(module propagation
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
      (import protobject
	      var-ref-util
	      free-vars
	      config
	      nodes
	      var
	      side
	      use-count
	      verbose)
   (export (propagation! tree::pobject)))

;; uses While -> must be after while-pass
;;
;; locally propagate variables/constants. -> Removes var-count.
;; 
;; first pass: determine which variables are modified inside loops.
;; call/cc-calls count as loops. suspend/resumes don't.
;; each of these loops have a ht with all variables.
;; Also determine if a variable is changed outside its local scope (that is, if
;; it escapes, is it modified in these functions).
;;
;; second pass: linearly walk through the code. a ht holds the current value
;; for all variables. a set obviously updates the ht. a loop kills all the
;; variables that are affected.
;; if we have a binding x = y, and this is the only use of y (see
;; var-elimination), then we replace x by y.
(define (propagation! tree)
   (when (config 'propagation)
      (verbose "propagation")
      (side-effect tree)
      (free-vars tree)
      (pass1 tree)
      (pass2! tree)))

(define (pass1 tree)
   (verbose " propagation1")
   (overload traverse changed (Node
			       Module
			       Lambda
			       Call/cc-Call
			       While
			       Set!)
	     (tree.traverse #f '())))

(define-pmethod (Node-changed ht surrounding-whiles)
   (this.traverse2 ht surrounding-whiles))

(define-pmethod (Module-changed ht surrounding-whiles)
   (this.traverse2 (make-eq-hashtable) '()))

(define-pmethod (Lambda-changed ht surrounding-whiles)
   (let ((fun-ht (make-eq-hashtable)))
      (hashtable-put! fun-ht this #t)
      (set! this.changed-vars-ht (make-eq-hashtable))
      (this.traverse2 fun-ht surrounding-whiles)
      (hashtable-for-each this.free-vars-ht
			  (lambda (var ign)
			     (when (hashtable-get this.changed-vars-ht var)
				(set! var.changed-outside-local? #t))))
      (delete! this.changed-vars-ht)))
			  
(define-pmethod (Call/cc-Call-changed ht surrounding-whiles)
   (this.traverse2 ht surrounding-whiles)
   (when (config 'call/cc)
      (hashtable-put! ht this #t)
      ;; the call/cc might come back into the while-loop -> the variables
      ;; inside the while are not "safe" anymore.
      ;; Ex:
      ;;   var x = 3;
      ;;   while(..) {
      ;;      print(x);
      ;;      call/cc(..);
      ;;   }
      ;;   var x = 5;       // <----
      ;;   invoke_call/cc
      ;;
      ;;  x is changed outside the while, but the call/cc can bring the change
      ;;  back into the loop.
      (for-each (lambda (while)
		   (hashtable-put! ht while 'call/cc))
		surrounding-whiles)
      (set! this.changed-vars-ht (make-eq-hashtable))))

(define-pmethod (While-changed ht surrounding-whiles)
   (hashtable-put! ht this #t)
   (set! this.changed-vars-ht (make-eq-hashtable))
   (this.traverse2 ht (cons this surrounding-whiles))
   (unless (eq? (hashtable-get ht this) 'call/cc)
      (hashtable-remove! ht this)))

(define-pmethod (Set!-changed ht surrounding-whiles)
   (this.traverse2 ht surrounding-whiles)
   (hashtable-for-each ht
		       (lambda (loop/fun ignored)
			  (hashtable-put! loop/fun.changed-vars-ht
					  this.lvalue.var
					  #t))))

(define (pass2! tree)
   (verbose " propagation2")
   (overload traverse! propagate! (Node
				   Module
				   Lambda
				   While
				   Call/cc-Call
				   If
				   Case
				   Labelled
				   Break
				   Continue
				   Set!
				   Var-ref)
	     (tree.traverse! #f)))

;; ht1 will be the result-ht!
(define (merge-vals! ht1 . hts)
   (let ((result-ht ht1))
      (let loop ((hts hts))
	 (if (null? hts)
	     result-ht
	     (let ((ht (car hts)))
		(hashtable-for-each
		 ht
		 (lambda (var val)
		    (cond
		       ((eq? val 'unknown)
			(hashtable-put! result-ht var 'unknown))
		       ((hashtable-get result-ht var)
			=>
			(lambda (current)
			   (cond
			      ((eq? val current)
			       'do-nothing)
			      ((and (inherits-from? val (node 'Const))
				    (inherits-from? current (node 'Const)))
			       (unless (eqv? val.value current.value)
				  (hashtable-put! result-ht var 'unknown)))
			      ((and (inherits-from? val (node 'Var-ref))
				    (inherits-from? current (node 'Var-ref))))
			      (else
			       (hashtable-put! result-ht var 'unknown)))))
		       (else
			(hashtable-put! result-ht var val)))))
		(loop (cdr hts)))))))

(define (duplicate-ht ht)
   (let ((res (make-eq-hashtable)))
      (hashtable-for-each ht
			  (lambda (key val)
			     (hashtable-put! res key val)))
      res))

(define-pmethod (Node-propagate! var/vals-ht)
   (this.traverse1! var/vals-ht))

(define-pmethod (Module-propagate! var/vals-ht)
   (this.traverse1! (make-eq-hashtable)))

(define-pmethod (Lambda-propagate! var/vals-ht)
   ;; no need to add formals.
   (this.traverse1! (make-eq-hashtable)))

(define-pmethod (While-propagate! var/vals-ht)
   (hashtable-for-each this.changed-vars-ht
		       (lambda (var ign)
			  (hashtable-put! var/vals-ht var 'unknown)))
   (set! this.label.var/vals-hts '())
   (this.traverse1! var/vals-ht)
   (let ((continue-hts this.label.var/vals-hts))
      (delete! this.label.var/vals-hts)
      (apply merge-vals! var/vals-ht continue-hts)
      this))

   

(define-pmethod (Call/cc-Call-propagate! var/vals-ht)
   (this.traverse1! var/vals-ht)
   (when (config 'call/cc)
      (hashtable-for-each this.changed-vars-ht
			  (lambda (var ign)
			     (hashtable-put! var/vals-ht var 'unknown))))
   this)

(define-pmethod (If-propagate! var/vals-ht)
   (set! this.test (this.test.traverse! var/vals-ht))
   (let ((ht2 (duplicate-ht var/vals-ht)))
      (set! this.then (this.then.traverse! var/vals-ht))
      (set! this.else (this.else.traverse! ht2))
      (merge-vals! var/vals-ht ht2)
      this))

(define-pmethod (Case-propagate! var/vals-ht)
   (set! this.key (this.key.traverse! var/vals-ht))
   
   ;; expensive... :(
   (let ((var/vals-hts (map (lambda (ign)
			       (duplicate-ht var/vals-ht))
			    this.clauses)))
      (set! this.clauses (map! (lambda (clause ht)
				  (clause.traverse! ht))
			       this.clauses
			       var/vals-hts))
      (apply merge-vals! var/vals-ht var/vals-hts)
      this))

(define-pmethod (Labelled-propagate! var/vals-ht)
   (set! this.label.var/vals-hts '())
   (set! this.body (this.body.traverse! var/vals-ht))
   (let ((break-hts this.label.var/vals-hts))
      (delete! this.label.var/vals-hts)
      (apply merge-vals! var/vals-ht break-hts)
      this))

(define-pmethod (Break-propagate! var/vals-ht)
   (set! this.val (and this.val (this.val.traverse! var/vals-ht)))
   (set! this.label.var/vals-hts (cons var/vals-ht
				       this.label.var/vals-hts))
   this)

(define-pmethod (Continue-propagate! var/vals-ht)
   (set! this.label.var/vals-hts (cons var/vals-ht
				       this.label.var/vals-hts))
   this)

(define-pmethod (Set!-propagate! var/vals-ht)
   (define (transitive-value val)
      (if (inherits-from? val (node 'Begin))
	  (transitive-value (car (last-pair val.exprs)))
	  val))

   (set! this.val (this.val.traverse! var/vals-ht))
   (let ((rvalue (transitive-value this.val))
	 (var this.lvalue.var))
      (hashtable-put! var/vals-ht
		      var
		      rvalue)
      ;; kill all vars that depend on lvalue.var
      (hashtable-for-each var/vals-ht
			  (lambda (key val)
			     (if (and (instance-of? val (node 'Var-ref))
				      (eq? val.var var)
				      (not (eq? key var)))
				 (hashtable-update! var/vals-ht
						    key
						    (lambda (current-val)
						       'unknown)
						    'unknown))))
      this))

(define-pmethod (Var-ref-propagate! var/vals-ht)
   (let* ((var this.var)
	  (val (hashtable-get var/vals-ht var)))
      (cond
	 ((or (not val)
	      (eq? val 'unknown)
	      var.changed-outside-local?)
	  this)
	 ((and (inherits-from? val (node 'Var-ref))
	       (not val.var.changed-outside-local?))
	  (val.var.reference))
	 ((and (inherits-from? val (node 'Const))
	       (or (number? val.value)
		   (symbol? val.value)
		   (char? val.value)
		   (boolean? val.value)
		   (eqv? #unspecified val.value)))
	  (new-node Const val.value))
	 (else
	  this))))
