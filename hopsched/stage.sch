;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/src/stage.sch                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 20 20:23:18 2008                          */
;*    Last change :  Sat Nov 12 08:41:26 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Stage macros (optmization)                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (option (define (spawn-expander x e)
	      (match-case x
		 ((?spawn ?scd ?proc . ?args)
		  (let ((eproc (e proc e))
			(escd (e scd e))
			(eargs (map (lambda (x) (e x e)) args))
			(spw (string->symbol (format "spawn~a" (length args)))))
		     `(,spw ,escd  ,eproc ,@eargs)))
		 (else
		  (map (lambda (x) (e x e)) x))))

	   (define (stage-expander x e)
	      (match-case x
		 ((?stage ?scd ?thread ?proc . ?args)
		  (let ((s (gensym 'scd))
			(eproc (e proc e))
			(ethread (e thread e))
			(escd (e scd e))
			(eargs (map (lambda (x) (e x e)) args))
			(stg (string->symbol (format "stage~a" (length args)))))
		     `(let ((,s ,escd))
			 (if (isa? ,s row-scheduler)
			     (,eproc ,s ,ethread ,@eargs)
			     (,stg ,s ,ethread ,eproc ,@eargs)))))
		 (else
		  (map (lambda (x) (e x e)) x))))))

;*---------------------------------------------------------------------*/
;*    spawn ...                                                        */
;*---------------------------------------------------------------------*/
(define-expander spawn spawn-expander)

;*---------------------------------------------------------------------*/
;*    stage ...                                                        */
;*---------------------------------------------------------------------*/
(define-expander stage stage-expander)
		


   
