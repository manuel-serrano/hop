;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/public_expd.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 23 07:35:40 2017                          */
;*    Last change :  Wed Aug 23 08:00:53 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript public expanders                                       */
;*    -------------------------------------------------------------    */
;*    See expanders.sch and public.sch                                 */
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
