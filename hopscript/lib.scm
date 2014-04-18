;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/lib.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:16:17 2013                          */
;*    Last change :  Mon Apr 14 16:51:14 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Hop client-side compatibility kit (share/hop-lib.js)         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_lib

   (library hop)
   
   (import __hopscript_types
	   __hopscript_property
	   __hopscript_object
	   __hopscript_function
	   __hopscript_public
	   __hopscript_worker)

   (export (alist->jsobject ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    alist->jsobject ...                                              */
;*---------------------------------------------------------------------*/
(define (alist->jsobject alist)
   (let ((%this (js-initial-global-object)))
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new %this js-object)))
	    (for-each (lambda (e)
			 (js-put! obj (car e) (cdr e) #f %this))
	       alist)
	    obj))))


