;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/lib_expd.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 23 07:35:40 2017                          */
;*    Last change :  Sun Oct 10 10:06:47 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript lib expanders                                          */
;*    -------------------------------------------------------------    */
;*    See expanders.sch                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-jsobject->jsarray ...                                         */
;*---------------------------------------------------------------------*/
(define (js-jsobject->jsarray-expander x e)
   (match-case x
      ((js-jsobject->jsarray (and (? symbol?) ?a) (and (? symbol?) ?%this))
       (e `(if (js-array? ,a)
	       ,a
	       ((@ js-jsobject->jsarray __hopscript_lib) ,a ,%this))
	  e))
      ((js-jsobject->jsarray ?a ?%this)
       (e `((@ js-jsobject->jsarray __hopscript_lib) ,a ,%this) e))
      (else
       (error "js-jsobject->jsarray" "bad form" x))))
	  
