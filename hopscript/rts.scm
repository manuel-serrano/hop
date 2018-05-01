;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/rts.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 08:09:57 2013                          */
;*    Last change :  Tue May  1 18:25:43 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
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
	   __hopscript_property
	   __hopscript_stringliteral)

   (export (js-debug . l)))

;*---------------------------------------------------------------------*/
;*    js-debug ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-debug . l)
   (call-with-output-string
      (lambda (p)
	 (for-each (lambda (e) (display e p)) l))))

