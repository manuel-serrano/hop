;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/hopscript/lib.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:16:17 2013                          */
;*    Last change :  Tue Jan 28 10:39:07 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Hop client-side compatibility kit (share/hop-lib.js)         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_lib

   (import __hopscript_types
	   __hopscript_property
	   __hopscript_object
	   __hopscript_function
	   __hopscript_public)

   (export (alist->jsobject ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    alist->jsobject ...                                              */
;*---------------------------------------------------------------------*/
(define (alist->jsobject alist)
   (let ((obj (js-new js-object)))
      (for-each (lambda (e)
		   (js-put! obj (car e) (cdr e) #f))
	 alist)
      obj))


