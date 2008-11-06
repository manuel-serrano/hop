;*=====================================================================*/
;*    serrano/prgm/project/hop/1.10.x/src/stage.sch                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 20 20:23:18 2008                          */
;*    Last change :  Thu Nov  6 08:58:05 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
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
			(stg (string->symbol (format "spawn~a" (length args)))))
		     `(,stg ,escd  ,eproc ,@eargs)))
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
			 (if (row-scheduler? ,s)
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
(define-expander stage0 stage-expander)
(define-expander stage1 stage-expander)
(define-expander stage2 stage-expander)
(define-expander stage3 stage-expander)
(define-expander stage4 stage-expander)
(define-expander stage5 stage-expander)
		


   
