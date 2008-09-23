(module callcc-resume-push
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   config
	   nodes
	   tail
	   mark-statements
	   verbose)
   (export (call/cc-resume-push! tree::pobject)))

;; When call/cc resumes, the instruction following the call/cc should be
;; executed.
;; Call/cc-Resume represents this location.
;; The Resume node is then (sometimes) pushed further, to push it to cheaper
;; locations. All skipped expressions are copied into the Resume-node and need
;; to be executed before jumping to the resume-point. At the moment only the
;; "Set!" node is skipped (and the copied expression currently is just the
;; skipped var).
(define (call/cc-resume-push! tree)
   (verbose " call/cc resume push")
   (overload traverse! resume! (Node
				Call/cc-Call
				If
				Case
				Begin
				Set!
				Break
				While
				Labeled)
	     (tree.traverse!)))

;; a resume-begin is a begin finishing with a resume-node.
(define (resume-begin? n)
   (and (inherits-from? n (node 'Begin))
	(inherits-from? (car (last-pair n.exprs)) (node 'Call/cc-Resume))))

(define (resume-begin-split bnode)
   (let ((exprs bnode.exprs))
      (if (= (length exprs) 2) ;; something + resume
	  (cons (car exprs) (cadr exprs))
	  (begin
	     ;; remove last element
	     ;; not very elegant. I know.
	     (let* ((rev-exprs (reverse! exprs))
		    (resume (car rev-exprs)))
		(set! bnode.exprs (reverse! (cdr rev-exprs)))
		(cons bnode resume))))))

;; merges all Resumes into the first of the list.
;; returns #f if the list is empty.
(define (resumes-merge! resumes)
   (and resumes
	(not (null? resumes))
	(let ((first-resume (car resumes)))
	   ;; first merge all the Resumes
	   (let loop ((resumes (cdr resumes))
		      (indices/vars first-resume.indices/vars))
	      (if (null? resumes)
		  (set! first-resume.indices/vars indices/vars)
		  (loop (cdr resumes)
			(append (car resumes).indices/vars
				indices/vars))))
	   first-resume)))
   
(define-pmethod (Node-resume!)
   (this.traverse0!))

;; add Resume node.
(define-pmethod (Call/cc-Call-resume!)
   (this.traverse0!)
   (let* ((resume-node (new-node Call/cc-Resume this.call/cc-index))
	  (bnode (new-node Begin (list this resume-node))))
      (mark-statement-form! bnode #t)
      bnode))

(define-pmethod (If-resume!)
   (this.traverse0!)
   (let ((then/resume (and (resume-begin? this.then)
			   (resume-begin-split this.then)))
	 (else/resume (and (resume-begin? this.else)
			   (resume-begin-split this.else))))
      (when then/resume
	 (set! this.then (car then/resume)))
      (when else/resume
	 (set! this.else (car else/resume)))
      (if (or then/resume else/resume)
	  (let* ((resume (cond
			    ((and then/resume else/resume)
			     (resumes-merge! (list (cdr then/resume)
						   (cdr else/resume))))
			    (then/resume
			     (cdr then/resume))
			    (else
			     (cdr else/resume))))
		 (bnode (new-node Begin (list this resume))))
	     (mark-statement-form! bnode #t)
	     bnode)
	  this)))

(define-pmethod (Case-resume!)
   ;; TODO: implement Case-resume
   (this.traverse0!))

(define-pmethod (Begin-resume!)
   (this.traverse0!)
   (let ((exprs this.exprs))
      ;; merge nested Begins.
      (let loop ((exprs exprs))
	 (unless (null? exprs)
	    (let ((expr (car exprs)))
	       (cond
		  ((inherits-from? expr (node 'Begin))
		   ;; insert into our list.
		   (let ((other-exprs expr.exprs)
			 (exprs-tail (cdr exprs)))
		      ;; we know there must be at least 2 elements.
		      ;; otherwise we wouldn't have gotten a 'Begin'.
		      (set-car! exprs (car other-exprs))
		      (set-cdr! exprs (cdr other-exprs))
		      (set-cdr! (last-pair other-exprs) exprs-tail))
		   (loop exprs))
		  ((inherits-from? expr (node 'Call/cc-Resume))
		   (let ((next (and (not (null? (cdr exprs)))
				    (cadr exprs))))
		      (cond
			 ((not next)
			  (loop (cdr exprs)))
			 ;; merge two consecutive Resumes into one.
			 ((inherits-from? next (node 'Call/cc-Resume))
			  (set-car! exprs (resumes-merge! (list (car exprs) next)))
			  (set-cdr! exprs (cddr exprs))
			  (loop exprs))
			 ((or (inherits-from? next (node 'Continue))
			      (and (inherits-from? next (node 'Break))
				   (not next.val)))
			  (set! next.label.resumes
				(cons expr (or next.label.resumes
					       '())))
			  ;; remove Resume from this Begin
			  (set-car! exprs (cadr exprs))
			  (set-cdr! exprs (cddr exprs))
			  (loop exprs))
			 (else
			  (loop (cdr exprs))))))
		  (else
		   (loop (cdr exprs))))))))
   this)

(define-pmethod (Set!-resume!)
   (this.traverse0!)
   (if (resume-begin? this.val)
       (let* ((val/resume (resume-begin-split this.val))
	      (new-val (car val/resume))
	      (resume (cdr val/resume)))
	  ;; TODO: optimize. reuse the begin-node.
	  (set! this.val new-val)
	  ;; there must only one entry in the a-list.
	  ;; before resuming the execution at our resume-point we need to
	  ;; update the lvalue.
	  ;; ex:
	  ;;    case 0:
	  ;;      index = 2;
	  ;;      x = call/cc();
	  ;;    case 2:
	  ;;      ....
	  ;; In this case we would need to update 'x' before resuming at 'case 2:'.
	  (set-cdr! (car resume.indices/vars) this.lvalue.var)
	  (let ((bnode (new-node Begin (list this resume))))
	     (mark-statement-form! bnode #t)
	     bnode))
       this))

(define-pmethod (Break-resume!)
   (this.traverse0!)
   (let* ((val/resume (and (resume-begin? this.val)
			    (resume-begin-split this.val))))
      (when val/resume
	 (set! this.val (car val/resume))
	 (set! this.label.resumes
	       (cons (cdr val/resume)
		     (or this.label.resumes '()))))
      this))
   
(define-pmethod (While-resume!)
   (this.traverse0!)
   (let* ((body/resume (and (resume-begin? this.body)
			    (resume-begin-split this.body)))
	  ;; a body-resume continues at the end of the while -> like a continue
	  ;; We put it just before the loop.
	  (body-resume (and body/resume
			    (cdr body/resume)))
	  ;; a continue-resume initially was just before a
	  ;; continue. We put it just before the loop now.
	  (continue-resumes (or this.label.resumes '()))
	  (resume (if body-resume
		      (resumes-merge! (cons body-resume continue-resumes))
		      (and (not (null? continue-resumes))
			   continue-resumes))))
      (delete! this.label.resumes)
      (if body/resume
	  (set! this.body (car body/resume)))
      (if resume
	  ;; note the 'resume' before the 'this'.
	  (let ((bnode (new-node Begin (list resume this))))
	     (mark-statement-form! bnode #t)
	     bnode)
	  this)))

(define-pmethod (Labeled-resume!)
   (this.traverse0!)
   (let* ((body/resume (and (resume-begin? this.body)
			    (resume-begin-split this.body)))
	  (body-resume (if body/resume
			   (list (cdr body/resume))
			   '()))
	  (resumes (or this.label.resumes
		       '()))
	  (resume (resumes-merge! (append body-resume resumes))))
      (delete! this.label.resumes)
      (if body/resume
	  (set! this.body (car body/resume)))
      (if resume
	  ;; all resumes are after Labeled.
	  (let ((bnode (new-node Begin (list this resume))))
	     (mark-statement-form! bnode #t)
	     bnode)
	  this)))

