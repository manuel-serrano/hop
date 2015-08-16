;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/verbose_expd.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 16:36:28 2006                          */
;*    Last change :  Sun Jun 17 17:34:58 2012 (serrano)                */
;*    Copyright   :  2006-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This file implements the hop-verb expanders. It is used both     */
;*    at compile-time and runtime-time.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    hop-verb-expander ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-verb-expander x e)
   (match-case x
      ((?- ?level . ?rest)
       (let ((v (gensym)))
	  `(let ((,v ,(e level e)))
	      (if (>=fx (hop-verbose) ,v)
		  (hop-verb ,v ,@(map (lambda (x) (e x e)) rest))
		  #f))))
      (else
       (error "hop-verb" "Illegal form" x))))
