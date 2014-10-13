;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/comprehension.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 13 18:06:59 2014                          */
;*    Last change :  Mon Oct 13 18:15:04 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    EcmaScript7 comprehensions                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_comprehension
   
   (library hop)
   
   (include "../nodejs/nodejs_debug.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_number
	   __hopscript_worker)

   (export (js-comprehension ::JsGlobalObject ::obj ::JsFunction
	      ::obj ::symbol ::bstring)))

;*---------------------------------------------------------------------*/
;*    js-comprehension ...                                             */
;*---------------------------------------------------------------------*/
(define (js-comprehension %this iterable fun test _name _ast)
   (if (eq? test #t)
       ;; a mere map
       (with-access::JsGlobalObject %this (js-array-prototype)
	  (let ((jsmap (js-get js-array-prototype 'map %this)))
	     (js-call1 %this jsmap iterable fun)))
       ;; a map filter
       (with-access::JsGlobalObject %this (js-array-prototype)
	  (let ((jsmap (js-get js-array-prototype 'map %this))
		(jsfilter (js-get js-array-prototype 'filter %this)))
	     (let ((i (js-call1 %this jsfilter iterable test)))
		(js-call1 %this jsmap i fun))))))

   

	
