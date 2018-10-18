;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/hopscript/rts.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 08:09:57 2013                          */
;*    Last change :  Tue Nov  5 09:13:15 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    runtime support functions.                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_rts

   (library hop)
   
   (import __hopscript_types
	   __hopscript_private
	   __hopscript_function
	   __hopscript_error
	   __hopscript_object
	   __hopscript_public
	   __hopscript_property)

   (export (js-debug . l)))

;*---------------------------------------------------------------------*/
;*    js-debug ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-debug . l)
   (call-with-output-string
      (lambda (p)
	 (for-each (lambda (e) (display e p)) l))))

