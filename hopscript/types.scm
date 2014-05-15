;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/types.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 21 10:17:45 2013                          */
;*    Last change :  Thu May 15 05:56:01 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript types                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_types

   (library hop)
   
   (use __hopscript_object
	__hopscript_string
	__hopscript_number
	__hopscript_error
	__hopscript_boolean
	__hopscript_private
	__hopscript_function
	__hopscript_property
	__hopscript_public)
   
   (export (class JsPropertyDescriptor
	      (name::symbol read-only)
	      (configurable (default #f))
	      (enumerable (default #f)))
	   (class JsDataDescriptor::JsPropertyDescriptor
	      (writable (default #f)))
	   (final-class JsValueDescriptor::JsDataDescriptor
	      (value (default (js-undefined))))
	   (final-class JsIndexDescriptor::JsDataDescriptor
	      (index::int (default -1))
	      (owner::JsObject read-only))
	   (final-class JsAccessorDescriptor::JsPropertyDescriptor
	      get
	      set)
	   
	   (class JsPropertyCache
	      (cmap::obj (default #unspecified))
	      (name::symbol (default '||))
	      (index::long (default -1)))
	   
	   (class JsConstructMap
	      (transition::pair (default (cons #f #f)))
	      (nextmap (default #f))
	      (names::vector read-only (default '#()))
	      (descriptors::vector read-only (default '#())))

	   (class JsObject
	      (__proto__ (default (js-null)))
	      (extensible::bool read-only (default #t))
	      (properties::pair-nil (default '()))
	      (cmap (default #f))
	      (elements::vector (default '#())))
	   
	   (class JsGlobalObject::JsObject
	      (js-object::JsFunction (default (class-nil JsFunction)))
	      (js-array::JsFunction (default (class-nil JsFunction)))
	      (js-array-prototype::JsArray (default (class-nil JsArray)))
	      (js-boolean::JsFunction (default (class-nil JsFunction)))
	      (js-string::JsFunction (default (class-nil JsFunction)))
	      (js-number::JsFunction (default (class-nil JsFunction)))
	      (js-function::JsFunction (default (class-nil JsFunction)))
	      (js-function-prototype::JsFunction (default (class-nil JsFunction)))
	      (js-math::JsMath (default (class-nil JsMath)))
	      (js-regexp::JsFunction (default (class-nil JsFunction)))
	      (js-regexp-prototype::JsRegExp (default (class-nil JsRegExp)))
	      (js-date::JsFunction (default (class-nil JsFunction)))
	      (js-json::JsJSON (default (class-nil JsJSON)))
	      (js-service-prototype::JsService (default (class-nil JsService)))
	      (js-error::JsFunction (default (class-nil JsFunction)))
	      (js-syntax-error::JsFunction (default (class-nil JsFunction)))
	      (js-type-error::JsFunction (default (class-nil JsFunction)))
	      (js-uri-error::JsFunction (default (class-nil JsFunction)))
	      (js-eval-error::JsFunction (default (class-nil JsFunction)))
	      (js-range-error::JsFunction (default (class-nil JsFunction)))
	      (js-reference-error::JsFunction (default (class-nil JsFunction)))
	      (js-worker::JsFunction (default (class-nil JsFunction)))
	      (js-worker-prototype::JsWorker (default (class-nil JsWorker)))
	      (js-others::pair-nil (default '())))
	   
	   (class JsArray::JsObject
	      (sealed::bool (default #f))
	      (frozen::bool (default #f))
	      (vec::vector (default '#())))
	   
	   (class JsArguments::JsObject
	      vec::vector)
	   
	   (class JsString::JsObject
	      (val::bstring (default "")))
	   
	   (class JsFunction::JsObject
	      (name read-only)
	      (alloc::procedure read-only)
	      (construct::procedure read-only)
	      (constrsize::int (default 3))
	      (constrmap (default #f)) 
	      (arity::int read-only)
	      (procedure::procedure read-only))
	   
	   (class JsService::JsFunction
	      (svc::obj read-only))
	   
	   (class JsNumber::JsObject
	      (val::obj read-only (default 0)))
	   
	   (class JsMath::JsObject)
	   
	   (class JsRegExp::JsObject
	      (rx::obj read-only))
	   
	   (class JsBoolean::JsObject
	      (val::bool (default #t)))
	   
	   (class JsError::JsObject
	      (name (default "Error"))
	      (msg (default ""))
	      (stack (default #f))
	      (fname (default #f))
	      (location (default #f)))
	   
	   (class JsDate::JsObject
	      (val read-only (default #f)))

	   (class JsJSON::JsObject)

	   (class JsWorker::JsObject
	      (thread::obj (default #unspecified)))
	   
	   (generic js-clone::obj ::obj)
	   
	   (inline js-undefined)
	   (inline js-null)
	   (js-absent)

	   (js-typeof obj)))

;*---------------------------------------------------------------------*/
;*    object-print ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (object-print obj::JsObject port print-slot::procedure)
   
   (define (class-field-write/display field)
      (let* ((name (class-field-name field))
	     (get-value (class-field-accessor field)))
	 (display " [" port)
	 (display name port)
	 (display #\: port)
	 (display #\space port)
	 (if (memq name '(__proto__ elements cmap))
	     (display (typeof (get-value obj)) port)
	     (print-slot (get-value obj) port))
	 (display #\] port)))

   (let* ((class (object-class obj))
	  (class-name (class-name class))
	  (fields (class-all-fields class))
	  (len (vector-length fields)))
      (display "#|" port)
      (display class-name port)
      (if (nil? obj)
	  (display " nil|" port)
	  (let loop ((i 0))
	     (if (=fx i len)
		 (display #\| port)
		 (begin
		    (class-field-write/display (vector-ref-ur fields i))
		    (loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    object-print ::JsPropertyDescriptor ...                          */
;*---------------------------------------------------------------------*/
(define-method (object-print p::JsPropertyDescriptor port pslot::procedure)
   (with-access::JsPropertyDescriptor p (name configurable enumerable)
      (fprintf port
	 "#|~s name=~a configurable=~a enumerable=~a|"
	 (class-name (object-class p))
	 name
	 configurable
	 enumerable)))

;*---------------------------------------------------------------------*/
;*    object-print ::JsValueDescriptor ...                             */
;*---------------------------------------------------------------------*/
(define-method (object-print p::JsValueDescriptor port pslot::procedure)
   (with-access::JsValueDescriptor p (name configurable enumerable writable value)
      (fprintf port
	 "#|~s name=~a configurable=~a enumerable=~a writable=~a value=~a|"
	 (class-name (object-class p))
	 name
	 configurable
	 enumerable
	 writable
	 (if (not (isa? value JsObject)) value (typeof value)))))

;*---------------------------------------------------------------------*/
;*    object-print ::JsIndexDescriptor ...                             */
;*---------------------------------------------------------------------*/
(define-method (object-print p::JsIndexDescriptor port pslot::procedure)
   (with-access::JsIndexDescriptor p (name configurable enumerable writable index)
      (fprintf port
	 "#|~s name=~a configurable=~a enumerable=~a writable=~a index=~a|"
	 (class-name (object-class p))
	 name
	 configurable
	 enumerable
	 writable
	 index)))

;*---------------------------------------------------------------------*/
;*    js-clone ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (js-clone obj)
   ;; NOT IMPLEMENTED YET
   obj)

;*---------------------------------------------------------------------*/
;*    js-clone ::JsGlobalObject ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-clone obj::JsGlobalObject)
   (with-access::JsObject obj (properties __proto__ cmap elements)
      (duplicate::JsGlobalObject obj
	 (__proto__ (js-clone __proto__))
	 (cmap cmap)
	 (elements (when (vector? elements) (vector-map js-clone elements)))
	 (properties (js-properties-clone properties)))))

;*---------------------------------------------------------------------*/
;*    js-undefined ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.1          */
;*---------------------------------------------------------------------*/
(define-inline (js-undefined)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    js-null ...                                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.2          */
;*---------------------------------------------------------------------*/
(define-inline (js-null)
   '())

;*---------------------------------------------------------------------*/
;*    absent-value ...                                                 */
;*---------------------------------------------------------------------*/
(define absent-value
   (cons #f #f))

;*---------------------------------------------------------------------*/
;*    js-absent ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-absent)
   absent-value)

;*---------------------------------------------------------------------*/
;*    js-typeof ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.3       */
;*---------------------------------------------------------------------*/
(define (js-typeof obj)
   (cond
      ((isa? obj JsFunction)
       "function")
      ((isa? obj JsObject)
       "object")
      ((or (real? obj) (integer? obj))
       "number")
      ((boolean? obj)
       "boolean")
      ((eq? obj (js-undefined))
       "undefined")
      ((string? obj)
       "string")
      ((eq? obj (js-null))
       "object")
      (else
       (typeof obj))))

