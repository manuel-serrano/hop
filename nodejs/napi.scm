;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/napi.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 24 16:10:01 2023                          */
;*    Last change :  Sun Apr  2 06:36:07 2023 (serrano)                */
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

	   (macro $napi_undefined::int "napi_undefined")
	   (macro $napi_null::int "napi_null")
	   (macro $napi_boolean::int "napi_boolean")
	   (macro $napi_number::int "napi_number")
	   (macro $napi_string::int "napi_string")
	   (macro $napi_symbol::int "napi_symbol")
	   (macro $napi_object::int "napi_object")
	   (macro $napi_function::int "napi_function")
	   (macro $napi_external::int "napi_external")
	   (macro $napi_bigint::int "napi_bigint")

	   (macro $obj-null?::bool (::obj) "0L ==")

	   (export napi-throw "bgl_napi_throw")
	   (export napi-throw-error "bgl_napi_throw_error")
	   (export napi-throw-type-error "bgl_napi_throw_type_error")
	   (export napi-throw-range-error "bgl_napi_throw_range_error")
	   (export napi-throw-syntax-error "bgl_napi_throw_syntax_error")
	   (export napi-create-string-utf8 "bgl_napi_create_string_utf8")
	   (export napi-get-element "bgl_napi_get_element")
	   (export napi-set-element! "bgl_napi_set_element")
	   (export napi-get-named-property "bgl_napi_get_named_property")
	   (export napi-put-named-property! "bgl_napi_put_named_property")
	   (export napi-define-named-property! "bgl_napi_define_named_property")
	   (export napi-make-property-descriptor "bgl_napi_make_property_descriptor")
	   (export napi-get-array-length "bgl_napi_get_array_length")
	   (export napi-has-element "bgl_napi_has_element")
	   (export napi-delete-element! "bgl_napi_delete_element")
	   (export napi-create-function "bgl_napi_create_function")
	   (export napi-create-object "bgl_napi_create_object")
	   (export napi-create-array "bgl_napi_create_array")
	   (export napi-create-array-with-length "bgl_napi_create_array_with_length")
	   (export napi-create-promise "bgl_napi_create_promise")
	   (export napi-create-date "bgl_napi_create_date")
	   (export napi-create-error "bgl_napi_create_error")
	   (export napi-create-type-error "bgl_napi_create_type_error")
	   (export napi-create-range-error "bgl_napi_create_range_error")
	   (export napi-create-syntax-error "bgl_napi_create_syntax_error")
	   (export napi-is-array? "bgl_napi_is_array")
	   (export napi-is-date? "bgl_napi_is_date")
	   (export napi-is-error? "bgl_napi_is_error")
	   (export napi-typeof "bgl_napi_typeof")
	   (export napi-uvloop "bgl_napi_uvloop")
	   (export napi-jsstring? "bgl_napi_jsstringp")
	   (export napi-jsstring->string "bgl_napi_jsstring_to_string")
	   (export napi-jsstring->string-latin1 "bgl_napi_jsstring_to_string_latin1")
	   (export napi-jsstring->string-utf16 "bgl_napi_jsstring_to_string_utf16")
	   (export napi-coerce-to-bool "bgl_napi_coerce_to_bool")
	   (export napi-coerce-to-number "bgl_napi_coerce_to_number")
	   (export napi-coerce-to-object "bgl_napi_coerce_to_object")
	   (export napi-coerce-to-string "bgl_napi_coerce_to_string")
	   (export napi-get-date-value "bgl_napi_get_date_value"))
   
   (export (napi-throw ::obj ::obj)
	   (napi-throw-error ::obj ::string ::string)
	   (napi-throw-type-error ::obj ::string ::string)
	   (napi-throw-range-error ::obj ::string ::string)
	   (napi-throw-syntax-error ::obj ::string ::string)
	   (napi-create-string-utf8::obj ::obj ::bstring)
	   (napi-get-named-property::obj ::obj ::obj ::bstring)
	   (napi-put-named-property!::obj ::obj ::obj ::bstring ::obj)
	   (napi-define-named-property!::obj ::obj ::obj ::bstring ::obj)
	   (napi-make-property-descriptor ::obj ::obj ::bstring ::obj ::bool ::bool ::bool ::obj ::obj)
	   (napi-get-array-length::uint32 ::obj ::obj)
	   (napi-has-element::bool ::obj ::obj ::int)
	   (napi-delete-element!::obj ::obj ::obj ::int)
	   (napi-get-element::obj ::obj ::obj ::int)
	   (napi-set-element!::obj ::obj ::obj ::int ::obj)
	   (napi-create-function::obj ::obj ::procedure ::bstring)
	   (napi-create-object::obj ::obj)
	   (napi-create-array::obj ::obj)
	   (napi-create-array-with-length::obj ::obj ::long)
	   (napi-create-promise::obj ::obj ::obj)
	   (napi-create-date::obj ::obj ::double)
	   (napi-create-error::obj ::obj ::obj ::obj)
	   (napi-create-type-error::obj ::obj ::obj ::obj)
	   (napi-create-range-error::obj ::obj ::obj ::obj)
	   (napi-create-syntax-error::obj ::obj ::obj ::obj)
	   (napi-is-array?::bool ::obj) 
	   (napi-is-date?::bool ::obj) 
	   (napi-is-error?::bool ::obj) 
	   (napi-typeof::int ::obj ::obj)
	   (napi-uvloop::$uv_loop_t ::obj)
	   (napi-jsstring?::bool ::obj)
	   (napi-jsstring->string::bstring ::obj)
	   (napi-jsstring->string-latin1::bstring ::obj)
	   (napi-jsstring->string-utf16::ucs2string ::obj)
	   (napi-coerce-to-bool::obj ::obj ::obj)
	   (napi-coerce-to-number::obj ::obj ::obj)
	   (napi-coerce-to-object::obj ::obj ::obj)
	   (napi-coerce-to-string::obj ::obj ::obj)
	   (napi-get-date-value::double ::obj)))

;*---------------------------------------------------------------------*/
;*    napi-throw-error ...                                             */
;*---------------------------------------------------------------------*/
(define (napi-throw %this obj)
   (raise obj))

;*---------------------------------------------------------------------*/
;*    napi-throw-error ...                                             */
;*---------------------------------------------------------------------*/
(define (napi-throw-error %this code msg)
   (raise
      (instantiate::JsError
	 (name code)
	 (msg msg)
	 (stack (js-get-trace-stack))
	 (%this %this))))

;* {*---------------------------------------------------------------------*} */
;* {*    napi-throw-range-error ...                                       *} */
;* {*---------------------------------------------------------------------*} */
;* (define (napi-throw-range-error %this code msg)                     */
;*    (raise                                                           */
;*       (instantiate::JsError                                         */
;* 	 (name code)                                                   */
;* 	 (msg msg)                                                     */
;* 	 (stack (js-get-trace-stack))                                  */
;* 	 (%this %this))))                                              */

;*---------------------------------------------------------------------*/
;*    napi-throw-type-error ...                                        */
;*---------------------------------------------------------------------*/
(define (napi-throw-type-error %this code msg)
   (js-raise-type-error %this (format "~a: ~~~a" code) msg))

;*---------------------------------------------------------------------*/
;*    napi-throw-range-error ...                                       */
;*---------------------------------------------------------------------*/
(define (napi-throw-range-error %this code msg)
   (js-raise-range-error %this (format "~a: ~~~a" code) msg))

;*---------------------------------------------------------------------*/
;*    napi-throw-syntax-error ...                                      */
;*---------------------------------------------------------------------*/
(define (napi-throw-syntax-error %this code msg)
   (js-raise-syntax-error %this (format "~a: ~~~a" code) msg))

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
;*    napi-put-named-property! ...                                     */
;*---------------------------------------------------------------------*/
(define (napi-put-named-property! %this this prop val)
   (js-put! this (js-string->name prop) val #f %this))

;*---------------------------------------------------------------------*/
;*    napi-define-named-property! ...                                  */
;*---------------------------------------------------------------------*/
(define (napi-define-named-property! %this this prop desc)
   (js-define-own-property this (js-string->name prop) desc #t %this))

;*---------------------------------------------------------------------*/
;*    napi-make-property-descriptor ...                                */
;*---------------------------------------------------------------------*/
(define (napi-make-property-descriptor %this this prop val writable enumerable configurable get set)
   (let ((name (js-string->name prop)))
      (cond
	 ((and (not get) (not set))
	  (instantiate::JsValueDescriptor
	     (name name)
	     (configurable configurable)
	     (enumerable enumerable)
	     (writable writable)
	     (value val)))
	 (else
	  (instantiate::JsAccessorDescriptor
	     (name name)
	     (configurable configurable)
	     (enumerable enumerable)
	     (get get)
	     (set set)
	     (%get (if get
		       (with-access::JsProcedure get (procedure) procedure)
		       (lambda (this) (js-undefined))))
	     (%set (if set
		       (with-access::JsProcedure set (procedure) procedure)
		       (lambda (this v) (js-undefined)))))))))

;*---------------------------------------------------------------------*/
;*    napi-get-array-length ...                                        */
;*---------------------------------------------------------------------*/
(define (napi-get-array-length %this this)
   (js-array-length this))

;*---------------------------------------------------------------------*/
;*    napi-get-element ...                                             */
;*---------------------------------------------------------------------*/
(define (napi-get-element %this this index)
   (js-array-ref this index %this))

;*---------------------------------------------------------------------*/
;*    napi-has-element ...                                             */
;*---------------------------------------------------------------------*/
(define (napi-has-element %this this index)
   (js-has-property this index %this))

;*---------------------------------------------------------------------*/
;*    napi-delete-element! ...                                         */
;*---------------------------------------------------------------------*/
(define (napi-delete-element! %this this index)
   (js-delete! this index #f %this))

;*---------------------------------------------------------------------*/
;*    napi-set-element! ...                                            */
;*---------------------------------------------------------------------*/
(define (napi-set-element! %this this index val)
   (js-array-set! this index val #f %this))

;*---------------------------------------------------------------------*/
;*    napi-create-function ...                                         */
;*---------------------------------------------------------------------*/
(define (napi-create-function %this fun name)
   (js-make-function %this fun
      (js-function-arity fun)
      (js-function-info :name name :len 1)
      :alloc js-object-alloc))

;*---------------------------------------------------------------------*/
;*    empty-cmap ...                                                   */
;*---------------------------------------------------------------------*/
(define empty-cmap
   (js-make-jsconstructmap :props '#() :methods '#()))

;*---------------------------------------------------------------------*/
;*    napi-create-object ...                                           */
;*---------------------------------------------------------------------*/
(define (napi-create-object %this)
   (instantiateJsObject
      (cmap empty-cmap)
      (__proto__ (js-object-proto %this))
      (elements (make-vector 0))))

;*---------------------------------------------------------------------*/
;*    napi-create-array ...                                            */
;*---------------------------------------------------------------------*/
(define (napi-create-array %this)
   (js-vector->jsarray (vector 4) %this))

;*---------------------------------------------------------------------*/
;*    napi-create-array-with-length ...                                */
;*---------------------------------------------------------------------*/
(define (napi-create-array-with-length %this len)
   (js-vector->jsarray (vector len) %this))

;*---------------------------------------------------------------------*/
;*    napi-is-array? ...                                               */
;*---------------------------------------------------------------------*/
(define (napi-is-array? obj)
   (js-array? obj))
   
;*---------------------------------------------------------------------*/
;*    napi-is-date? ...                                                */
;*---------------------------------------------------------------------*/
(define (napi-is-date? obj)
   (isa? obj JsDate))
   
;*---------------------------------------------------------------------*/
;*    napi-is-error? ...                                               */
;*---------------------------------------------------------------------*/
(define (napi-is-error? obj)
   (isa? obj JsError))
   
;*---------------------------------------------------------------------*/
;*    napi-typeof ...                                                  */
;*---------------------------------------------------------------------*/
(define (napi-typeof %this obj)
   (cond
      ((js-number? obj) $napi_number)
      ((null? obj) $napi_null)
      ((boolean? obj) $napi_boolean)
      ((js-jsstring? obj) $napi_string)
      ((js-function? obj) $napi_function)
      ((bignum? obj) $napi_bigint)
      ((eq? obj (js-undefined)) $napi_undefined)
      ((js-symbol? obj) $napi_symbol)
      (else $napi_object)))

;*---------------------------------------------------------------------*/
;*    napi-create-promise ...                                          */
;*---------------------------------------------------------------------*/
(define (napi-create-promise %this deferred)
   (js-new-promise/procedure %this
      (lambda (%this resolve reject)
	 (pragma "*((void **)($1)) = ($2)" deferred (cons resolve reject)))))

;*---------------------------------------------------------------------*/
;*    napi-create-date ...                                             */
;*---------------------------------------------------------------------*/
(define (napi-create-date %this tm)
   (let ((this (js-date-alloc %this)))
      (with-access::JsDate this (time)
	 (set! time (flonum->llong tm)))
      this))

;*---------------------------------------------------------------------*/
;*    napi-create-error ...                                            */
;*---------------------------------------------------------------------*/
(define (napi-create-error %this code msg)
   (with-access::JsGlobalObject %this (js-error)
      (let ((o (js-new1 %this js-error msg)))
	 (js-put! o (& "code") code #f %this)
	 o)))

;*---------------------------------------------------------------------*/
;*    napi-create-type-error ...                                       */
;*---------------------------------------------------------------------*/
(define (napi-create-type-error %this code msg)
   (with-access::JsGlobalObject %this (js-type-error)
      (let ((o (js-new1 %this js-type-error msg)))
	 (js-put! o (& "code") code #f %this)
	 o)))

;*---------------------------------------------------------------------*/
;*    napi-create-range-error ...                                      */
;*---------------------------------------------------------------------*/
(define (napi-create-range-error %this code msg)
   (with-access::JsGlobalObject %this (js-range-error)
      (let ((o (js-new1 %this js-range-error msg)))
	 (js-put! o (& "code") code #f %this)
	 o)))

;*---------------------------------------------------------------------*/
;*    napi-create-syntax-error ...                                     */
;*---------------------------------------------------------------------*/
(define (napi-create-syntax-error %this code msg)
   (with-access::JsGlobalObject %this (js-syntax-error)
      (let ((o (js-new1 %this js-syntax-error msg)))
	 (js-put! o (& "code") code #f %this)
	 o)))

;*---------------------------------------------------------------------*/
;*    napi-uvloop ...                                                  */
;*---------------------------------------------------------------------*/
(define (napi-uvloop %this)
   (with-access::JsGlobalObject %this (worker)
      (with-access::WorkerHopThread worker (%loop)
	 (with-access::UvLoop %loop ($builtin)
	    $builtin))))

;*---------------------------------------------------------------------*/
;*    napi-jsstring? ...                                               */
;*---------------------------------------------------------------------*/
(define (napi-jsstring? obj)
   (js-jsstring? obj))

;*---------------------------------------------------------------------*/
;*    napi-jsstring->string ...                                        */
;*---------------------------------------------------------------------*/
(define (napi-jsstring->string obj)
   (js-jsstring->string obj))

;*---------------------------------------------------------------------*/
;*    napi-jsstring->string-latin1 ...                                 */
;*---------------------------------------------------------------------*/
(define (napi-jsstring->string-latin1 obj)
   (charset-convert (js-jsstring->string obj) 'utf8 'latin1))

;*---------------------------------------------------------------------*/
;*    napi-jsstring->string-utf16 ...                                  */
;*---------------------------------------------------------------------*/
(define (napi-jsstring->string-utf16 obj)
   (utf8-string->ucs2-string (js-jsstring->string obj)))

;*---------------------------------------------------------------------*/
;*    napi-coerce-to-bool ...                                          */
;*---------------------------------------------------------------------*/
(define (napi-coerce-to-bool %this val)
   (js-toboolean val))

;*---------------------------------------------------------------------*/
;*    napi-coerce-to-number ...                                        */
;*---------------------------------------------------------------------*/
(define (napi-coerce-to-number %this val)
   (js-tonumber val %this))

;*---------------------------------------------------------------------*/
;*    napi-coerce-to-object ...                                        */
;*---------------------------------------------------------------------*/
(define (napi-coerce-to-object %this val)
   (js-toobject %this val))

;*---------------------------------------------------------------------*/
;*    napi-coerce-to-string ...                                        */
;*---------------------------------------------------------------------*/
(define (napi-coerce-to-string %this val)
   (cond
      ((string? val)
       (js-string->jsstring val))
      ((isa? val JsSymbolLiteral)
       (napi-throw-type-error %this "napi-coerce-to-string" "invalid argument"))
      (else
       (js-string->jsstring (js-tostring val %this)))))

;*---------------------------------------------------------------------*/
;*    napi-get-date-value ...                                          */
;*---------------------------------------------------------------------*/
(define (napi-get-date-value dt)
   (let ((v (js-date-gettime dt)))
      (if (fixnum? v)
	  (fixnum->flonum v)
	  v)))
