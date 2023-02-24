;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/napi.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 24 16:10:01 2023                          */
;*    Last change :  Fri Feb 24 18:46:21 2023 (serrano)                */
;*    Copyright   :  2023 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The Scheme part of the node_api.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_napi
   
   (library hop hopscript)
   
   (extern (include "node_api.h")

	   (macro $napi_property_descriptor*::$napi_property_descriptor* (::void*) "(napi_property_descriptor *)")
	   (type $napi_property_descriptor* "napi_property_descriptor *")
	   (infix macro $napi_property_descriptor->name::obj (::$napi_property_descriptor*) "->name")
	   (infix macro $napi_property_descriptor->utf8name::string (::$napi_property_descriptor*) "->utf8name")
	   (infix macro $napi_property_descriptor->method::obj (::$napi_property_descriptor*) "->method")
	   (infix macro $napi_property_descriptor->getter::obj (::$napi_property_descriptor*) "->getter")
	   (infix macro $napi_property_descriptor->setter::obj (::$napi_property_descriptor*) "->setter")
	   (infix macro $napi_property_descriptor->value::obj (::$napi_property_descriptor*) "->value")
	   (infix macro $napi_property_descriptor->attributes::obj (::$napi_property_descriptor*) "->attributes")
	   (infix macro $napi_property_descriptor->data::obj (::$napi_property_descriptor*) "->data")

	   (macro $obj-null?::bool (::obj) "0L ==")
	   
	   (export napi-create-string-utf8 "bgl_napi_create_string_utf8")
	   (export napi-get-element "bgl_napi_get_element")
	   (export napi-get-named-property "bgl_napi_get_named_property")
	   (export napi-define-property "bgl_napi_define_property"))
   
   (export (napi-create-string-utf8::obj ::obj ::bstring)
	   (napi-get-named-property::obj ::obj ::obj ::bstring)
	   (napi-get-element::obj ::obj ::obj ::int)
	   (napi-define-property::obj ::obj ::obj ::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    napi-create-string-utf8 ...                                      */
;*---------------------------------------------------------------------*/
(define (napi-create-string-utf8 %this string)
   (js-string->jsstring string))

;*---------------------------------------------------------------------*/
;*    napi-get-named-property ...                                      */
;*---------------------------------------------------------------------*/
(define (napi-get-named-property %this this prop)
   (js-get this (js-string->name prop) %this))

;*---------------------------------------------------------------------*/
;*    napi-get-element ...                                             */
;*---------------------------------------------------------------------*/
(define (napi-get-element %this this index)
   (js-array-ref this index %this))

;*---------------------------------------------------------------------*/
;*    napi-define-property ...                                         */
;*---------------------------------------------------------------------*/
(define (napi-define-property %this this prop met)
   (let ((proc (js-make-function %this met
		  (js-function-arity met)
		  (js-function-info :name prop :len 1)
		  :alloc js-no-alloc)))
      (js-put! this (js-string->name prop) proc #f %this)
      this))
   
