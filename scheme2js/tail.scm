;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/tail.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-13                                           */
;*    Last change :  Mon Aug 19 08:32:05 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    tail property                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tail
   
   (import nodes
	   dump-node
	   export-desc
	   walk
	   verbose)
   
   (export (wide-class Tail-Call::Call))
   
   (static (wide-class Tail-Label::Label))
   
   (export (tail-calls tree::Module)))

;*---------------------------------------------------------------------*/
;*    tail-calss ...                                                   */
;*    -------------------------------------------------------------    */
;*    might be called only after Node-elimination.                     */
;*    but then anytime. so don't assume nodes exist or not.            */
;*---------------------------------------------------------------------*/
(define (tail-calls tree)
   (verbose "tail")
   (tail tree #f #f))

;*---------------------------------------------------------------------*/
;*    tail ::Node ...                                                  */
;*---------------------------------------------------------------------*/
(define-nmethod (Node.tail tail?)
   ;; be conservative here.
   (default-walk this #f)) 

;*---------------------------------------------------------------------*/
;*    tail ::Module ...                                                */
;*---------------------------------------------------------------------*/
(define-nmethod (Module.tail tail?)
   (default-walk this #t))

;*---------------------------------------------------------------------*/
;*    tail ::Lambda ...                                                */
;*---------------------------------------------------------------------*/
(define-nmethod (Lambda.tail tail?)
   (default-walk this #t))

;*---------------------------------------------------------------------*/
;*    tail ::If ...                                                    */
;*---------------------------------------------------------------------*/
(define-nmethod (If.tail tail?)
   (with-access::If this (test then else)
      (walk test #f)
      (walk then tail?)
      (walk else tail?)))

;*---------------------------------------------------------------------*/
;*    tail ::Case ...                                                  */
;*---------------------------------------------------------------------*/
(define-nmethod (Case.tail tail?)
   (with-access::Case this (key clauses)
      (walk key #f)
      (for-each (lambda (clause)
		   (walk clause tail?))
		clauses)))

;*---------------------------------------------------------------------*/
;*    tail ::Clause ...                                                */
;*---------------------------------------------------------------------*/
(define-nmethod (Clause.tail tail?)
   ;; default-walk is fine. (Consts do nothing with 'tail?')
   (default-walk this tail?))

;*---------------------------------------------------------------------*/
;*    tail ::Set! ...                                                  */
;*---------------------------------------------------------------------*/
(define-nmethod (Set!.tail tail?)
   (default-walk this #f))

;*---------------------------------------------------------------------*/
;*    tail ::Let ...                                                   */
;*---------------------------------------------------------------------*/
(define-nmethod (Let.tail tail?)
   (with-access::Let this (bindings body)
      (for-each (lambda (n) (walk n #f)) bindings)
      (walk body tail?)))

;*---------------------------------------------------------------------*/
;*    tail ::Begin ...                                                 */
;*---------------------------------------------------------------------*/
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

;*---------------------------------------------------------------------*/
;*    tail ::Call ...                                                  */
;*---------------------------------------------------------------------*/
(define-nmethod (Call.tail tail?)
   (cond
      (tail?
       (widen!::Tail-Call this))
      ((isa? this Tail-Call)
       (shrink! this)))
   (default-walk this #f))

;*---------------------------------------------------------------------*/
;*    tail ::Frame-alloc ...                                           */
;*---------------------------------------------------------------------*/
(define-nmethod (Frame-alloc.tail tail?)
   (default-walk this tail?))

;*---------------------------------------------------------------------*/
;*    tail ::Return ...                                                */
;*---------------------------------------------------------------------*/
(define-nmethod (Return.tail tail?)
   (default-walk this #t))

;*---------------------------------------------------------------------*/
;*    tail ::Labeled ...                                               */
;*---------------------------------------------------------------------*/
(define-nmethod (Labeled.tail tail?)
   (with-access::Labeled this (label)
      (cond
	 (tail?
	  (widen!::Tail-Label label))
	 ((isa? label Tail-Label)
	  (shrink! label))))
   (default-walk this tail?))

;*---------------------------------------------------------------------*/
;*    tail ::Break ...                                                 */
;*---------------------------------------------------------------------*/
(define-nmethod (Break.tail tail?)
   (with-access::Break this (label val)
      (if (isa? label Tail-Label)
	  (walk val #t)
	  (walk val #f))))

;*---------------------------------------------------------------------*/
;*    tail ::Tail-rec ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (Tail-rec.tail tail?)
   (with-access::Tail-rec this (inits body label)
      (for-each (lambda (init) (walk init #f)) inits)
      (when (isa? label Tail-Label) (shrink! label))
      (walk body tail?)))

;*---------------------------------------------------------------------*/
;*    tail ::Tail-rec-Call ...                                         */
;*---------------------------------------------------------------------*/
(define-nmethod (Tail-rec-Call.tail tail?)
   (default-walk this #f))

;*---------------------------------------------------------------------*/
;*    tail ::While ...                                                 */
;*---------------------------------------------------------------------*/
(define-nmethod (While.tail tail?)
   (default-walk this #f))
