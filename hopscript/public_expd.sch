;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/public_expd.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 23 07:35:40 2017                          */
;*    Last change :  Sat Sep 22 15:33:24 2018 (serrano)                */
;*    Copyright   :  2017-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript public expanders                                       */
;*    -------------------------------------------------------------    */
;*    See expanders.sch                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-let-set!-expander ...                                         */
;*---------------------------------------------------------------------*/
(define (js-let-set!-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?id) ?val ?loc ?%this)
       (e `(if (eq? ,id #\Z)
	       (js-raise-reference-error/loc ,%this ,loc "dead-zone access" ',id)
	       (set! ,id ,val))
	  e))
      (else
       (error "js-let-set!" "bad syntax" x))))

;*---------------------------------------------------------------------*/
;*    js-null-or-undefined?-expander ...                               */
;*---------------------------------------------------------------------*/
(define (js-null-or-undefined?-expander x e)
   (match-case x
      ((?- ?expr)
       (cond-expand
	  (bigloo4.3c
	   (if (symbol? expr)
	       (e `(or (eq? ,expr (js-undefined)) (eq? ,expr (js-null))) e)
	       (let ((tmp (gensym)))
		  (e `(let ((,tmp ,expr))
			 (or (eq? ,expr (js-undefined)) (eq? ,expr (js-null))))
		     e))))
	  (else
	   (e `(null-or-unspecified? ,expr) e))))))
