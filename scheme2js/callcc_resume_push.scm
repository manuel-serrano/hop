;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module callcc-resume-push
   (import config
	   nodes
	   export-desc
	   walk
	   tail
	   mark-statements
	   verbose
	   tools)
   (static (wide-class Call/cc-Label::Label
	      (resumes::pair-nil (default '()))))
   (export (call/cc-resume-push! tree::Module)))

;; When call/cc resumes, the instruction following the call/cc should be
;; executed.
;; Call/cc-Resume represents this location. These nodes are introduced here.
;;
;; The Resume node is then (sometimes) pushed further, to push it to cheaper
;; locations. All skipped expressions are copied into the Resume-node and need
;; to be executed before jumping to the resume-point. At the moment only the
;; "Set!" node is skipped (and the copied expression currently is just the
;; skipped var).
(define (call/cc-resume-push! tree)
   (verbose " call/cc resume push")
   (resume! tree #f #f))

(define (update-hoisted! fun index instr)
   (with-access::Lambda fun (call/cc-hoisted)
      (let ((rev-hoisted (assq index call/cc-hoisted)))
	 (if (not rev-hoisted) ;; first hoisting for variable
	     (cons-set! call/cc-hoisted `(,index ,instr))
	     ;; physically update the element
	     (set-cdr! rev-hoisted (cons instr (cdr rev-hoisted)))))))

;; if n is a Begin:
;; returns #f or the last element of the begin, if it was a Call/cc-Resume
;; physically modifies the begin, so it does not contain the resume-node
;; anymore.
(define (resume-begin-split n)
   (when (Begin? n)
      (with-access::Begin n (exprs)
	 (let loop ((exprs exprs))
	    (cond
	       ((null? exprs) #f) ;; should never happen
	       ((null? (cdr exprs)) #f)
	       ((and (null? (cddr exprs))
		     (Call/cc-Resume? (cadr exprs)))
		(let ((resume (cadr exprs)))
		   (set-cdr! exprs '())
		   resume))
	       (else
		(loop (cdr exprs))))))))

(define (simplified-begin bnode::Begin)
   (with-access::Begin bnode (exprs)
      (cond
	 ((null? exprs) (instantiate::Const
			   (location (Node-location bnode))
			   (value #unspecified)))
	 ((null? (cdr exprs)) (car exprs))
	 (else
	  bnode))))

;; merges all Resumes into the first of the list.
;; returns #f if the list is empty.
(define (resumes-merge! resumes)
   (and resumes
	(not (null? resumes))
	(let ((first-resume (car resumes)))
	   (with-access::Call/cc-Resume first-resume (indices)
	      ;; first merge all the Resumes
	      (let loop ((resumes (cdr resumes))
			 (inds indices))
		 (if (null? resumes)
		     (set! indices inds)
		     (loop (cdr resumes)
			   (append (Call/cc-Resume-indices (car resumes))
				   inds)))))
	   first-resume)))
   
(define-nmethod (Node.resume! surrounding-fun)
   (default-walk! this surrounding-fun))

(define-nmethod (Lambda.resume! surrounding-fun)
   (default-walk! this this))

;; add Resume node.
(define-nmethod (Call.resume! surrounding-fun)
   (with-access::Call this (call/cc? call/cc-index)
      (if (or (not call/cc?)
	      (not call/cc-index)) ;; tail-call
	  (default-walk! this surrounding-fun)
	  (begin
	     (default-walk! this surrounding-fun)
	     (let ((resume-node (instantiate::Call/cc-Resume
				   (location (Node-location this))
				   (indices (list call/cc-index)))))
		(instantiate::Begin
		   (exprs (list this resume-node))))))))

(define-nmethod (If.resume! surrounding-fun)
   (default-walk! this surrounding-fun)
   (with-access::If this (then else)
      (let ((then-resume (resume-begin-split then))
	    (else-resume (resume-begin-split else)))
      (when then-resume
	 ;; in case there is only one element left.
	 (set! then (simplified-begin then)))
      (when else-resume
	 (set! else (simplified-begin else)))
      (if (or then-resume else-resume)
	  (let* ((resume (cond
			    ((and then-resume else-resume)
			     (resumes-merge! (list then-resume else-resume)))
			    (else
			     (or then-resume else-resume))))
		 (bnode (instantiate::Begin (exprs (list this resume)))))
	     bnode)
	  this))))

(define-nmethod (Case.resume! surrounding-fun)
   ;; TODO: implement Case-resume
   (default-walk! this surrounding-fun))

(define-nmethod (Begin.resume! surrounding-fun)
   (default-walk! this surrounding-fun)
   (with-access::Begin this (exprs)
      ;; merge nested Begins.
      (let loop ((exprs exprs))
	 (unless (null? exprs)
	    (let ((expr (car exprs)))
	       (cond
		  ((Begin? expr)
		   ;; insert into our list.
		   (let ((other-exprs (Begin-exprs expr))
			 (exprs-tail (cdr exprs)))
		      ;; we know there must be at least 2 elements.
		      ;; otherwise we wouldn't have gotten a 'Begin'.
		      (set-car! exprs (car other-exprs))
		      (set-cdr! exprs (cdr other-exprs))
		      (set-cdr! (last-pair other-exprs) exprs-tail))
		   (loop exprs))
		  ((Call/cc-Resume? expr)
		   (let ((next (and (not (null? (cdr exprs)))
				    (cadr exprs))))
		      (cond
			 ((not next)
			  (loop (cdr exprs)))
			 ;; merge two consecutive Resumes into one.
			 ((Call/cc-Resume? next)
			  (set-car! exprs (resumes-merge! (list (car exprs) next)))
			  (set-cdr! exprs (cddr exprs))
			  (loop exprs))
			 ((or (Continue? next)
			      (and (Break? next)
				   (not (Break-val next))))
			  (let ((label (if Continue?
					   (Continue-label next)
					   (Break-label next))))
			     (unless (Call/cc-Label? label)
				(widen!::Call/cc-Label label))
			     (with-access::Call/cc-Label label (resumes)
				(cons-set! resumes expr)))
			  ;; remove Resume from this Begin
			  (set-car! exprs (cadr exprs))
			  (set-cdr! exprs (cddr exprs))
			  (loop exprs))
			 (else
			  (loop (cdr exprs))))))
		  (else
		   (loop (cdr exprs))))))))
   this)

(define-nmethod (Set!.resume! surrounding-fun)
   (default-walk! this surrounding-fun)
   (with-access::Set! this (val lvalue)
      (let ((resume (resume-begin-split val)))
	 (if resume
	     (begin
		(set! val (simplified-begin val))
		;; before resuming the execution at our resume-point we need to
		;; update the lvalue.
		;; ex:
		;;    case 0:
		;;      index = 2;
		;;      x = call/cc();
		;;    case 2:
		;;      ....
		;; Here we need to update 'x' before resuming at 'case 2:'.
		(with-access::Call/cc-Resume resume (indices)
		   ;; there could be several indices for one variable.
		   ;; ex:
		   ;;     (set! a (if xxx
		   ;;                 (call/cc..)
		   ;;                 (call/cc..)))
		   (for-each (lambda (index)
				(update-hoisted! surrounding-fun index
						 `(set! ,(Ref-var lvalue))))
			     indices)
		   (instantiate::Begin (exprs (list this resume)))))
	     this))))

(define-nmethod (Break.resume! surrounding-fun)
   (default-walk! this surrounding-fun)
   (with-access::Break this (val label)
      (let ((resume (resume-begin-split val)))
	 (when resume
	    (set! val (simplified-begin val))
	    (unless (Call/cc-Label? label)
	       (widen!::Call/cc-Label label))
	    (with-access::Call/cc-Label label (resumes)
	       (cons-set! resumes resume)))))
   this)
   
(define-nmethod (While.resume! surrounding-fun)
   (default-walk! this surrounding-fun)
   (with-access::While this (body label)
      (let ((body-resume (resume-begin-split body))
	    (continue-resumes (and (Call/cc-Label? label)
				   (Call/cc-Label-resumes label))))
	 (when body-resume (set! body (simplified-begin body)))
	  ;; a body-resume continues at the end of the while -> like a continue
	  ;; We put it just before the loop.
	  ;; a continue-resume initially was just before a
	  ;; continue. We put it just before the loop now, too.
	 (let ((resume (cond
			  ((and body-resume continue-resumes)
			   (resumes-merge! (cons body-resume
						 continue-resumes)))
			  (body-resume
			   body-resume)
			  (continue-resumes
			   (resumes-merge! continue-resumes))
			  (else #f))))
	    (when (Call/cc-Label? label) (shrink! label))
	    (if resume
		;; note the 'resume' before the 'this'
		(instantiate::Begin (exprs (list resume this)))
		this)))))

(define-nmethod (Labeled.resume! surrounding-fun)
   (default-walk! this surrounding-fun)
   (with-access::Labeled this (body label)
      (let ((body-resume (resume-begin-split body))
	    (label-resumes (and (Call/cc-Label? label)
				(Call/cc-Label-resumes label))))
	 (when body-resume (set! body (simplified-begin body)))
	 (let ((resume (cond
			  ((and body-resume label-resumes)
			   (resumes-merge! (cons body-resume label-resumes)))
			  (body-resume
			   body-resume)
			  (label-resumes
			   (resumes-merge! label-resumes))
			  (else #f))))
	    (when (Call/cc-Label? label) (shrink! label))
	    (if resume
		;; all resumes are after Labelled.
		(instantiate::Begin (exprs (list this resume)))
		this)))))

(define-nmethod (Return.resume! surrounding-fun)
   (default-walk! this surrounding-fun)
   (with-access::Return this (val)
      (let ((return-resume (resume-begin-split val)))
	 ;; if there is (are) actually a return-resume, then it must be a
	 ;; resume assigning a variable. Otherwise the call would be in
	 ;; tail-position and thus would not have any resume.
	 ;; ex:
	 ;;    (return (if xyz
	 ;;                (set! a (call/cc..))
	 ;;                (set! b (if xxx
	 ;;                            (call/cc..)
	 ;;                            (call/cc..)))))
	 ;;
	 ;;   all call/cc-calls are not in tail-position. but once the resume
	 ;;   has been pushed after the 'set!'s, the resumes are in
	 ;;   tail-position.
	 (when return-resume
	    (with-access::Call/cc-Resume return-resume (indices)
	       (for-each (lambda (index)
			    (update-hoisted! surrounding-fun index '(return)))
			 indices))
	    ;; drop the resume-node, as it is not needed anymore
	    )))
   this)
