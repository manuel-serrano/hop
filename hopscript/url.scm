;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/url.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 16 16:40:42 2024                          */
;*    Last change :  Thu May 16 17:26:07 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of whatwg URL                              */
;*    -------------------------------------------------------------    */
;*    https://url.spec.whatwg.org/#url                                 */
;*    https://developer.mozilla.org/en-US/docs/Web/API/URL             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_url

   (library web hop)
   
   (include "types.sch" "stringliteral.sch" "property.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_function
	   __hopscript_error
	   __hopscript_array
	   __hopscript_profile)

   (export (js-init-url! ::JsObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-init-url! ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.12        */
;*---------------------------------------------------------------------*/
(define (js-init-url! %this)
   (with-access::JsGlobalObject %this (js-url js-url-prototype js-function)
      ;; local constant strings
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      
      ;; url prototype object
      (set! js-url-prototype
	 (instantiateJsObject
	    (cmap (js-make-jsconstructmap))
	    (__proto__ (js-object-proto %this))
	    (elements ($create-vector 47))))
      
      ;; https://developer.mozilla.org/en-US/docs/Web/API/URL/URL
      (define (%js-url this url base)
	 (let* ((s (js-tostring url %this))
		(m (pregexp-match "/([a-zA-Z]+:)\/\/(.*)/" s)))
	    (if m
		(begin
		   (js-put! this (& "protocol")
		      (js-string->jsstring (cadr m))
		      #f %this)
		   (js-put! this (& "path")
		      (js-string->jsstring (caddr m))
		      #f %this))
		(js-raise-type-error %this "Invalid URL" url))))
      
      (set! js-url
	 (js-make-function %this %js-url
	    (js-function-arity 0 -1 'scheme)
	    (js-function-info :name "URL" :len 2)
	    :__proto__ (js-object-proto js-function)
	    :prototype js-url-prototype
	    :size 3
	    :shared-cmap #f))
      
      ;; url constructor
      (js-bind! %this %this (& "URL")
	 :configurable #f :enumerable #f :value js-url :hidden-class #t)
      
      js-url))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
