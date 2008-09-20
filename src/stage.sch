;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/src/stage.sch                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 20 20:23:18 2008                          */
;*    Last change :  Sat Sep 20 20:24:44 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Stage macros (optmization)                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (option (define (stage-expander x e)
	      (match-case x
		 ((?stage ?scd ?thread ?proc . ?args)
		  (let ((s (gensym 'scd))
			(eproc (e proc e))
			(ethread (e thread e))
			(escd (e scd e))
			(eargs (map (lambda (x) (e x e)) args)))
		     `(let ((,s ,escd))
			 (if (row-scheduler? ,s)
			     (,eproc ,s ,ethread ,@eargs)
			     (,stage ,s ,ethread ,eproc ,@eargs)))))
		 (else
		  (map (lambda (x) (e x e)) x))))))

;*---------------------------------------------------------------------*/
;*    stage ...                                                        */
;*---------------------------------------------------------------------*/
(define-expander stage0 stage-expander)
(define-expander stage1 stage-expander)
(define-expander stage2 stage-expander)
(define-expander stage3 stage-expander)
(define-expander stage4 stage-expander)
(define-expander stage5 stage-expander)
		


   
