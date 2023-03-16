;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/napi.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 24 16:10:01 2023                          */
;*    Last change :  Thu Mar 16 05:44:10 2023 (serrano)                */
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
	   (export napi-throw-range-error "bgl_napi_throw_range_error")
	   (export napi-create-string-utf8 "bgl_napi_create_string_utf8")
	   (export napi-get-element "bgl_napi_get_element")
	   (export napi-set-element! "bgl_napi_set_element")
	   (export napi-get-named-property "bgl_napi_get_named_property")
	   (export napi-put-named-property! "bgl_napi_put_named_property")
	   (export napi-get-array-length "bgl_napi_get_array_length")
	   (export napi-has-element "bgl_napi_has_element")
	   (export napi-delete-element! "bgl_napi_delete_element")
	   (export napi-define-property "bgl_napi_define_property")
	   (export napi-create-function "bgl_napi_create_function")
	   (export napi-create-object "bgl_napi_create_object")
	   (export napi-create-array "bgl_napi_create_array")
	   (export napi-create-array-with-length "bgl_napi_create_array_with_length")
	   (export napi-create-promise "bgl_napi_create_promise")
	   (export napi-is-array? "bgl_napi_is_array")
	   (export napi-typeof "bgl_napi_typeof")
	   (export napi-uvloop "bgl_napi_uvloop"))
   
   (export (napi-throw ::obj ::obj)
	   (napi-throw-error ::obj ::string ::string)
	   (napi-throw-range-error ::obj ::string ::string)
	   (napi-create-string-utf8::obj ::obj ::bstring)
	   (napi-get-named-property::obj ::obj ::obj ::bstring)
	   (napi-put-named-property!::obj ::obj ::obj ::bstring ::obj)
	   (napi-get-array-length::uint32 ::obj ::obj)
	   (napi-has-element::bool ::obj ::obj ::int)
	   (napi-delete-element!::obj ::obj ::obj ::int)
	   (napi-get-element::obj ::obj ::obj ::int)
	   (napi-set-element!::obj ::obj ::obj ::int ::obj)
	   (napi-define-property::obj ::obj ::obj ::bstring ::obj)
	   (napi-create-function::obj ::obj ::procedure ::bstring)
	   (napi-create-object::obj ::obj)
	   (napi-create-array::obj ::obj)
	   (napi-create-array-with-length::obj ::obj ::long)
	   (napi-create-promise::obj ::obj ::obj)
	   (napi-is-array?::bool ::obj) 
	   (napi-typeof::int ::obj ::obj)
	   (napi-uvloop::$uv_loop_t ::obj)))

;*---------------------------------------------------------------------*/
;*    env-debug ...                                                    */
;*---------------------------------------------------------------------*/
(define env-debug
   (let ((env (getenv "HOP_DEBUG")))
      (if (string? env)
	  (string->integer env)
	  0)))

;*---------------------------------------------------------------------*/
;*    napi-get-trace-stack ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (napi-get-trace-stack)
   (when (>fx env-debug 0) (get-trace-stack)))

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
	 (stack (napi-get-trace-stack))
	 (%this %this))))

;*---------------------------------------------------------------------*/
;*    napi-throw-range-error ...                                       */
;*---------------------------------------------------------------------*/
(define (napi-throw-range-error %this code msg)
   (raise
      (instantiate::JsError
	 (name code)
	 (msg msg)
	 (stack (napi-get-trace-stack))
	 (%this %this))))

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
;*    napi-define-property ...                                         */
;*---------------------------------------------------------------------*/
(define (napi-define-property %this this prop met)
   (let ((proc (js-make-function %this met
		  (js-function-arity met)
		  (js-function-info :name prop :len 1)
		  :alloc js-no-alloc)))
      (js-put! this (js-string->name prop) proc #f %this)
      this))
   
;*---------------------------------------------------------------------*/
;*    napi-create-function ...                                         */
;*---------------------------------------------------------------------*/
(define (napi-create-function %this fun name)
   (js-make-function %this fun
      (js-function-arity fun)
      (js-function-info :name name :len 1)
      :alloc js-no-alloc))

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
;*    napi-uvloop ...                                                  */
;*---------------------------------------------------------------------*/
(define (napi-uvloop %this)
   (with-access::JsGlobalObject %this (worker)
      (with-access::WorkerHopThread worker (%loop)
	 (with-access::UvLoop %loop ($builtin)
	    $builtin))))
