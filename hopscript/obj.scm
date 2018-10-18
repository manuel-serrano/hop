;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/obj.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul  9 17:41:45 2017                          */
;*    Last change :  Tue Sep 12 13:38:44 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    ScmObject binding                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_obj
   
   (library hop js2scheme)
   
   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_string
	   __hopscript_stringliteral
	   __hopscript_symbol
	   __hopscript_promise
	   __hopscript_generator
	   __hopscript_function
	   __hopscript_number
	   __hopscript_math
	   __hopscript_boolean
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_arraybuffer
	   __hopscript_arraybufferview
	   __hopscript_date
	   __hopscript_error
	   __hopscript_json
	   __hopscript_service
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_worker
	   __hopscript_websocket
	   __hopscript_lib))

;*---------------------------------------------------------------------*/
;*    js-obj->jsobject ::object ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-obj->jsobject obj::object %this::JsGlobalObject)
   obj)

;*---------------------------------------------------------------------*/
;*    js-tostring ::object ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-tostring obj::object %this::JsGlobalObject)
   (format "[~a Object]" (class-name (object-class obj))))

;*---------------------------------------------------------------------*/
;*    js-inspect ::object ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-inspect obj::object cnt)
   (js-string->jsstring (typeof obj)))

;*---------------------------------------------------------------------*/
;*    js-properties-name ::object ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-properties-name o::object enump::bool %this::JsGlobalObject)
   (vector-map (lambda (f)
		  (js-string->jsstring
		     (symbol->string! (class-field-name f))))
      (class-all-fields (object-class o))))

;*---------------------------------------------------------------------*/
;*    js-has-property ::object ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::object name::obj %this)
   (let ((n (js-toname o %this))
	 (k (object-class o)))
      (find-class-field k n)))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::object ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::object p::obj %this::JsGlobalObject)
   (let* ((n (js-toname p %this))
	  (k (object-class o))
	  (f (find-class-field k n)))
      (if f
	  (instantiate::JsValueDescriptor
	     (writable (class-field-mutable? f))
	     (enumerable #t)
	     (configurable #f)
	     (name n)
	     (value (js-obj->jsobject ((class-field-accessor f) o) %this)))
	  (instantiate::JsValueDescriptor
	     (writable #f)
	     (enumerable #f)
	     (configurable #f)
	     (name n)
	     (value (js-undefined))))))

;*---------------------------------------------------------------------*/
;*    js-get ::object ...                                              */
;*    -------------------------------------------------------------    */
;*    Accessing Bigloo objects from hopscript                          */
;*---------------------------------------------------------------------*/
(define-method (js-get o::object prop %this)
   (let* ((name (js-toname prop %this))
	  (clazz (object-class o))
	  (field (find-class-field clazz name)))
      (if (not field)
	  (case name
	     ((inspect)
	      (js-undefined))
;* 	      (js-make-function %this js-inspect 1 'inspect))          */
	     ((constructor)
	      (js-undefined))
	     ((toString)
	      (js-make-function %this
		 (lambda (this)
		    (js-tostring this %this))
		 0
		 'toString))
	     ((isSealed)
	      (js-make-function %this (lambda (this) #t) 1 'isSealed))
	     ((isFrozen)
	      (js-make-function %this (lambda (this) #t) 1 'isFrozen))
	     (else
	      (js-raise-type-error %this
		 (format "no such field \"~a\" ~~a" name) o)))
	  (let ((v ((class-field-accessor field) o)))
	     (js-obj->jsobject v %this)))))

;*---------------------------------------------------------------------*/
;*    js-put! ::object ...                                             */
;*    -------------------------------------------------------------    */
;*    Mutating Bigloo objects from hopscript                           */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::object prop v::obj throw::bool %this::JsGlobalObject)
   (let* ((name (js-toname prop %this))
	  (clazz (object-class o))
	  (field (find-class-field clazz name)))
      (cond
	 ((not field)
	  (js-raise-type-error %this (format "no such field \"~a\" ~~a" name) o))
	 ((not (class-field-mutable? field))
	  (if throw
	      (js-raise-type-error %this (format "field \"~a\" read-only ~~a" name) o)
	      (js-undefined)))
	 (else
	  ((class-field-mutator field) o v)))))

