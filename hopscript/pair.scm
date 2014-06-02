;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/pair.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May 24 07:51:25 2014                          */
;*    Last change :  Tue May 27 15:02:52 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript JS/Hop pair binding                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_pair

   (library hop)

   (import __hopscript_types
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_function
	   __hopscript_property)
   
   (export  (js-get-pair o::pair prop::symbol %this::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    js-get-pair ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-get-pair o::pair prop %this)
   (case prop
      ((car)
       (car o))
      ((cdr)
       (cdr o))
      ((length)
       (js-make-function %this length
	  0 "length"))
      ((map)
       (js-make-function %this
	  (lambda (this proc)
	     (map (lambda (x) (js-call1 this proc o x)) o))
	  1 "map"))
      ((forEach)
       (js-make-function %this
	  (lambda (this proc)
	     (for-each (lambda (x) (js-call1 this proc o x)) o))
	  1 "forEach"))
      ((assoc)
       (js-make-function %this (lambda (this key) (assoc o key))
	  1 "assoc"))
      ((reverse)
       (js-make-function %this reverse
	  0 "reverse"))
      ((concat)
       (js-make-function %this (lambda (this . l) (apply append this l))
	  -1 "concat"))
      (else
       (js-raise-type-error %this
	  (format "no such field \"~a\" ~~a" (js-toname prop %this)) o))))
   

