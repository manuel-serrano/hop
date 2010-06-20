;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-10 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module push-set
   (import nodes
	   export-desc
	   walk
	   verbose)
   (static (wide-class Push-Label::Label
	      var/return))
   (export (push-set!s/return! tree::Module)))

;; and break.
;; TODO: rename function (for break).
(define (push-set!s/return! tree)
   (verbose "  push-set!s/return!")
   (push! tree #f #f))

(define (assign! node to)
   (cond
      ((not to) node)
      ((Return? to)
       (instantiate::Return
	  (location (Node-location to))
	  (val node)))
      ((Break? to)
       (with-access::Break to (label)
	  (with-access::Push-Label label (var/return)
	     (cond
		((Return? var/return)
		 (assign! node var/return))
		((Break? var/return)
		 (assign! node var/return))
		(else
		 (let ((tmp (assign! node var/return)))
		    (duplicate::Break to (val tmp)))))))) ;; reuse label.
      (else
       (var-assig to node))))

;; if var/return is not #f push it (and finally assign a value to it)
(define-nmethod (Node.push! var/return)
   (error "push-set!s/return"
	  "Internal Error: forgot node type"
	  this))

(define-nmethod (Const.push! var/return)
   (assign! this var/return))

(define-nmethod (Ref.push! var/return)
   (assign! this var/return))

(define-nmethod (Module.push! var/return)
   (default-walk! this #f))

(define-nmethod (Lambda.push! var/return)
   (default-walk! this #f)
   (assign! this var/return))

(define-nmethod (If.push! var/return)
   (with-access::If this (test then else)
      (set! test (walk! test #f))
      (set! then (walk! then var/return))
      (set! else (walk! else var/return))
      this))

(define-nmethod (Case.push! var/return)
   (with-access::Case this (key clauses)
      (set! key (walk! key #f))
      (set! clauses (map! (lambda (clause)
			     (walk! clause var/return))
			  clauses))
      this))

(define-nmethod (Clause.push! var/return)
   (with-access::Clause this (expr)
      ;; no need to walk consts
      (set! expr (walk! expr var/return))
      this))

(define-nmethod (Set!.push! var/return)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (if (Var? var/return)
	     ;; ignore var/return. the value of set! is unspecified.
	     (walk! val var)
	     (assign! (walk! val var)
		      var/return)))))

;; Let must not exist at this stage anymore

(define-nmethod (Begin.push! var/return)
   (with-access::Begin this (exprs)
      (let loop ((exprs exprs))
	 (cond
	    ((null? exprs) ;; empty Begin
	     'done)
	    ((null? (cdr exprs))
	     (set-car! exprs (walk! (car exprs) var/return)))
	    (else
	     (set-car! exprs (walk! (car exprs) #f))
	     (loop (cdr exprs)))))
      this))

(define-nmethod (Call.push! var/return)
   (default-walk! this #f)
   (assign! this var/return))

(define-nmethod (Frame-alloc.push! var/return)
   (default-walk! this #f)
   (assign! this var/return)) ;; var/return should always be #f.

(define-nmethod (Frame-push.push! var/return)
   (default-walk! this var/return))

(define-nmethod (Return.push! var/return)
   ;; ignore any var/return
   (with-access::Return this (val)
      (walk! val this)))

(define-nmethod (Labeled.push! var/return)
   (with-access::Labeled this (body label)
      (widen!::Push-Label label
	 (var/return var/return))
      (set! body (walk! body var/return))
      this))

(define-nmethod (Break.push! var/return)
   (with-access::Break this (val)
      (walk! val this)))

(define-nmethod (Continue.push! var/return)
   this)

(define-nmethod (Pragma.push! var/return)
   (assign! this var/return))

;; Tail-rec and Tail-rec-Call do not exist anymore (at this level)

(define-nmethod (While.push! var/return)
   (with-access::While this (test body)
      (set! test (walk! test #f))
      (set! body (walk! body #f))
      this))

;; TODO: Call/cc stuff.
	     
