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

(module tail
   (import nodes
	   export-desc
	   walk
	   verbose)
   (export (wide-class Tail-Call::Call))
   (static (wide-class Tail-Label::Label))
   (export (tail-calls tree::Module)))

;; might be called only after Node-elimination.
;; but then anytime. so don't assume nodes exist or not.
(define (tail-calls tree)
   (verbose "tail")
   (tail tree #f #f))

(define-nmethod (Node.tail tail?)
   (default-walk this #f)) ;; be conservative here.

(define-nmethod (Module.tail tail?)
   (default-walk this #t))

(define-nmethod (Lambda.tail tail?)
   (default-walk this #t))

(define-nmethod (If.tail tail?)
   (with-access::If this (test then else)
      (walk test #f)
      (walk then tail?)
      (walk else tail?)))

(define-nmethod (Case.tail tail?)
   (with-access::Case this (key clauses)
      (walk key #f)
      (for-each (lambda (clause)
		   (walk clause tail?))
		clauses)))

(define-nmethod (Clause.tail tail?)
   ;; default-walk is fine. (Consts do nothing with 'tail?')
   (default-walk this tail?))

(define-nmethod (Set!.tail tail?)
   (default-walk this #f))

(define-nmethod (Let.tail tail?)
   (with-access::Let this (bindings body)
      (for-each (lambda (n) (walk n #f)) bindings)
      (walk body tail?)))

(define-nmethod (Begin.tail tail?)
   (with-access::Begin this (exprs)
      (let loop ((exprs exprs))
	 (cond
	    ((null? exprs) 'do-nothing)
	    ((null? (cdr exprs))
	     (walk (car exprs) tail?))
	    (else
	     (walk (car exprs) #f)
	     (loop (cdr exprs)))))))

(define-nmethod (Call.tail tail?)
   (cond
      (tail?
       (widen!::Tail-Call this))
      ((Tail-Call? this)
       (shrink! this)))
   (default-walk this #f))

(define-nmethod (Frame-alloc.tail tail?)
   (default-walk this tail?))

(define-nmethod (Return.tail tail?)
   (default-walk this #t))

(define-nmethod (Labeled.tail tail?)
   (with-access::Labeled this (label)
      (cond
	 (tail?
	  (widen!::Tail-Label label))
	 ((Tail-Label? label)
	  (shrink! label))))
   (default-walk this tail?))

(define-nmethod (Break.tail tail?)
   (with-access::Break this (label val)
      (if (Tail-Label? label)
	  (walk val #t)
	  (walk val #f))))

(define-nmethod (Tail-rec.tail tail?)
   (with-access::Tail-rec this (inits body label)
      (for-each (lambda (init) (walk init #f)) inits)
      (when (Tail-Label? label) (shrink! label))
      (walk body tail?)))
   
(define-nmethod (Tail-rec-Call.tail tail?)
   (default-walk this #f))

(define-nmethod (While.tail tail?)
   (default-walk this #f))
