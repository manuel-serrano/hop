;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/js2scheme/utils.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 16:59:06 2013                          */
;*    Last change :  Sat Oct 19 11:12:15 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Utility functions                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_utils

   (import __js2scheme_ast
	   __js2scheme_dump)

   (export (pass ::bstring)
	   (error/loc proc obj msg loc)
	   (illegal-node ::bstring ::J2SNode)
	   (append-after-header stmts nodes)))
   
;*---------------------------------------------------------------------*/
;*    pass ...                                                         */
;*---------------------------------------------------------------------*/
(define (pass name)
   (print name))

;*---------------------------------------------------------------------*/
;*    error/loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (error/loc proc obj msg loc)
   (match-case loc
      ((at ?fname ?point)
       (error/location proc obj msg fname point))
      (else
       (error proc obj msg))))

;*---------------------------------------------------------------------*/
;*    illegal-node ...                                                 */
;*---------------------------------------------------------------------*/
(define (illegal-node pass this::J2SNode)
   (with-access::J2SNode this (loc)
      (error/loc pass
	 (format "~a should have been eliminated" (typeof this))
	 (j2s->list this)
	 loc)))

;*---------------------------------------------------------------------*/
;*    is-end-of-header? ...                                            */
;*---------------------------------------------------------------------*/
(define (is-end-of-header? n)
   (when (isa? n J2SPragma)
      (with-access::J2SPragma n (expr)
	 (when (equal? expr "end-of-header")
	    n))))

;*---------------------------------------------------------------------*/
;*    skip-header ...                                                  */
;*---------------------------------------------------------------------*/
(define (skip-header stmts)
   (let loop ((s stmts))
      (cond
	 ((null? s)
	  stmts)
	 ((is-end-of-header? (car s))
	  (cdr s))
	 (else
	  (loop (cdr s))))))

;*---------------------------------------------------------------------*/
;*    append-after-header ...                                          */
;*    -------------------------------------------------------------    */
;*    Add news nodes in the AST after the header (i.e., the rts        */
;*    initialization).                                                 */
;*---------------------------------------------------------------------*/
(define (append-after-header stmts nodes)
   (cond
      ((null? nodes)
       stmts)
      ((null? stmts)
       nodes)
      (else
       (let loop ((s stmts)
                  (p #f))
          (cond
             ((null? s)
              (append stmts nodes))
             ((is-end-of-header? (car s))
              (if p
                  (begin
                     (set-cdr! p (append nodes (cdr s)))
                     stmts)
                  (append nodes stmts)))
             (else
              (loop (cdr s) s)))))))
