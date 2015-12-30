;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/types.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 21 10:17:45 2013                          */
;*    Last change :  Wed Dec 30 07:27:01 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
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
	__hopscript_stringliteral
	__hopscript_symbol
	__hopscript_number
	__hopscript_date
	__hopscript_error
	__hopscript_boolean
	__hopscript_private
	__hopscript_function
	__hopscript_property
	__hopscript_public)
   
   (export (class WorkerHopThread::hopthread
	      (%loop (default #f))
	      (keep-alive::bool (default #f))
	      (mutex::mutex read-only (default (make-mutex)))
	      (condv::condvar read-only (default (make-condition-variable)))
	      (prehook (default #f))
	      (alivep (default #f))
	      (tqueue::pair-nil (default '()))
	      (listeners::pair-nil (default '()))
	      (exitlisteners::pair-nil (default '()))
	      (onmessage::obj (default (js-undefined)))
	      (onexit::obj (default (js-undefined)))
	      (%this::JsGlobalObject read-only)
	      (%process (default #f))
	      (%retval::int (default 0))
	      (async (default #f))
	      (state::symbol (default 'init))
	      (module-cache::obj (default #f))
	      (parent::obj (default #f))
	      (subworkers::pair-nil (default '()))
	      (uvhandles::vector (default (make-vector 32)))
	      (call::procedure (default (lambda (cb) (cb))))
	      (handlers::pair-nil (default '()))
	      (services::pair-nil (default '())))

	   (class MessageEvent::event
	      data::obj)

	   (class JsPropertyDescriptor
	      (name::obj read-only)
	      (configurable (default #f))
	      (enumerable (default #f)))
	   (class JsDataDescriptor::JsPropertyDescriptor
	      (writable (default #f)))
	   (final-class JsValueDescriptor::JsDataDescriptor
	      (value (default (js-undefined))))
	   (final-class JsIndexDescriptor::JsDataDescriptor
	      (index::int (default -1)))
	   (final-class JsAccessorDescriptor::JsPropertyDescriptor
	      get
	      set)
	   
	   (class JsPropertyCache
	      (cmap::obj (default #unspecified))
	      (name::obj (default '||))
	      (index::long (default -1)))
	   
	   (class JsConstructMap
	      (transition::pair (default (cons #f #f)))
	      (nextmap (default #f))
	      (names::vector read-only (default '#()))
	      (descriptors::vector read-only (default '#())))

	   ;; Not a jsobject. This class is used to implement
	   ;; JS string literal which are not plain Scheme string
	   ;; for the sake of concat performance
	   (final-class JsStringLiteral
	      ;; the actual characters (string, tree, list)
	      val::obj
	      state::uint8)
	   
	   (class JsObject
	      (__proto__ (default (js-null)))
	      (extensible::bool (default #t))
	      (properties::pair-nil (default '()))
	      (cmap (default #f))
	      (elements::vector (default '#())))

	   (class JsWrapper::JsObject
	      obj
	      data)
	   
	   (class JsGlobalObject::JsObject
	      (js-object::JsFunction (default (class-nil JsFunction)))
	      (js-array::JsFunction (default (class-nil JsFunction)))
	      (js-array-prototype::JsArray (default (class-nil JsArray)))
	      (js-arraybuffer::JsFunction (default (class-nil JsFunction)))
	      (js-int8array::JsFunction (default (class-nil JsFunction)))
	      (js-uint8array::JsFunction (default (class-nil JsFunction)))
	      (js-uint8clampedarray::JsFunction (default (class-nil JsFunction)))
	      (js-int16array::JsFunction (default (class-nil JsFunction)))
	      (js-uint16array::JsFunction (default (class-nil JsFunction)))
	      (js-int32array::JsFunction (default (class-nil JsFunction)))
	      (js-uint32array::JsFunction (default (class-nil JsFunction)))
	      (js-float32array::JsFunction (default (class-nil JsFunction)))
	      (js-float64array::JsFunction (default (class-nil JsFunction)))
	      (js-dataview::JsFunction (default (class-nil JsFunction)))
	      (js-boolean::JsFunction (default (class-nil JsFunction)))
	      (js-string::JsFunction (default (class-nil JsFunction)))
	      (js-symbol::JsFunction (default (class-nil JsFunction)))
	      (js-number::JsFunction (default (class-nil JsFunction)))
	      (js-function::JsFunction (default (class-nil JsFunction)))
	      (js-function-prototype::JsFunction (default (class-nil JsFunction)))
	      (js-::JsFunction (default (class-nil JsFunction)))
	      (js-math::JsMath (default (class-nil JsMath)))
	      (js-regexp::JsFunction (default (class-nil JsFunction)))
	      (js-regexp-prototype::JsRegExp (default (class-nil JsRegExp)))
	      (js-date::JsFunction (default (class-nil JsFunction)))
	      (js-json::JsJSON (default (class-nil JsJSON)))
	      (js-service-prototype::JsService (default (class-nil JsService)))
	      (js-hopframe-prototype (default (class-nil JsFunction)))
	      (js-error::JsFunction (default (class-nil JsFunction)))
	      (js-syntax-error::JsFunction (default (class-nil JsFunction)))
	      (js-type-error::JsFunction (default (class-nil JsFunction)))
	      (js-uri-error::JsFunction (default (class-nil JsFunction)))
	      (js-eval-error::JsFunction (default (class-nil JsFunction)))
	      (js-range-error::JsFunction (default (class-nil JsFunction)))
	      (js-reference-error::JsFunction (default (class-nil JsFunction)))
	      (js-worker::JsFunction (default (class-nil JsFunction)))
	      (js-promise (default (class-nil JsFunction)))
	      (js-worker-prototype::JsWorker (default (class-nil JsWorker)))
	      (js-generator-prototype::JsObject (default (class-nil JsObject)))
	      (js-buffer-proto (default #f))
	      (js-slowbuffer-proto (default #f))
	      (js-symbol-table read-only (default (js-symbol-table)))
	      (js-symbol-iterator (default (js-undefined)))
	      (js-main (default (js-null))))
	   
	   (class JsArray::JsObject
	      (inline::bool (default #t))
	      (sealed::bool (default #f))
	      (frozen::bool (default #f))
	      (vec::vector (default '#())))

	   (class JsArrayBuffer::JsObject
	      (frozen::bool (default #f))
	      (data (default '#u8())))
	   
	   (abstract-class JsArrayBufferView::JsObject
	      (frozen::bool (default #f))
	      (buffer::JsArrayBuffer (default (class-nil JsArrayBuffer)))
	      (%data (default '#u8()))
	      (byteoffset::uint32 (default #u32:0)))

	   (abstract-class JsTypedArray::JsArrayBufferView
	      (length::uint32 (default #u32:0))
	      bpe::uint32)

	   (class JsInt8Array::JsTypedArray)
	   (class JsUint8Array::JsTypedArray)
	   (class JsUint8ClampedArray::JsTypedArray)
	   (class JsInt16Array::JsTypedArray)
	   (class JsUint16Array::JsTypedArray)
	   (class JsInt32Array::JsTypedArray)
	   (class JsUint32Array::JsTypedArray)
	   (class JsFloat32Array::JsTypedArray)
	   (class JsFloat64Array::JsTypedArray)
	   
	   (class JsDataView::JsArrayBufferView)
	   
	   (class JsArguments::JsObject
	      vec::vector)

	   (class JsString::JsObject
	      val::JsStringLiteral)

	   (class JsSymbol::JsObject
	      val::bstring)
	   
	   (class JsFunction::JsObject
	      (name::bstring read-only)
	      alloc::procedure
	      (construct::procedure read-only)
	      (constrsize::int (default 3))
	      (constrmap (default #f)) 
	      (arity::int read-only (default -1))
	      (minlen::int read-only (default -1))
	      (len::int read-only)
	      (rest::bool read-only (default #f))
	      (procedure::procedure read-only)
	      (src read-only (default #f)))
	   
	   (class JsService::JsFunction
	      (worker::obj read-only)
	      (svc::obj read-only))

	   (class JsHopFrame::JsObject
	      (%this read-only)
	      (args read-only (default #f))
	      (srv read-only (default #f))
	      (options (default #f))
	      (header (default #f))
	      (path read-only))

	   (class JsServer::JsObject
	      (data::pair-nil (default '()))
	      (obj::server read-only))
	   
	   (class JsNumber::JsObject
	      (val::obj (default 0)))
	   
	   (class JsMath::JsObject)
	   
	   (class JsRegExp::JsObject
	      rx::obj)
	   
	   (class JsBoolean::JsObject
	      (val::bool (default #t)))
	   
	   (class JsError::JsObject
	      (name::JsStringLiteral (default (js-string->jsstring "Error")))
	      msg
	      (stack (default #f))
	      (fname (default #f))
	      (location (default #f)))
	   
	   (class JsDate::JsObject
	      (val (default #f)))

	   (class JsJSON::JsObject)

	   (class JsWorker::JsObject
	      (thread::obj (default #unspecified)))

	   (class JsPromise::JsObject
	      (state::symbol (default 'pending))
	      (val::obj (default #unspecified))
	      (thens::pair-nil (default '()))
	      (catches::pair-nil (default '()))
	      worker 
	      (resolvers::pair-nil (default '()))
	      (rejecters::pair-nil (default '()))
	      (watches::pair-nil (default '())))

	   (class JsGenerator::JsObject
	      %next)
	   
	   (generic js-clone::obj ::obj)
	   (generic js-donate ::obj ::WorkerHopThread ::JsGlobalObject)
	   
	   (inline js-undefined)
	   (inline js-null)
	   (js-absent)

	   (generic js-typeof::JsStringLiteral obj)

	   (generic js-arraybuffer-length ::JsArrayBuffer)
	   (generic js-arraybuffer-ref ::JsArrayBuffer ::int)
	   (generic js-arraybuffer-set! ::JsArrayBuffer ::int ::obj)

	   (generic js-buffer->jsbuffer ::JsObject ::pair-nil ::JsGlobalObject)

	   (generic js-typedarray-ref::procedure ::JsTypedArray)
	   (generic js-typedarray-set!::procedure ::JsTypedArray)))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsWrapper ...                              */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value o::JsWrapper)
   (with-access::JsWrapper o (obj)
      (obj)))

;*---------------------------------------------------------------------*/
;*    xml-write ::JsWrapper ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write o::JsWrapper p backend)
   (with-access::JsWrapper o (obj)
      (xml-write obj p backend)))

;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::JsWrapper ...                              */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute o::JsWrapper id p backend)
   (with-access::JsWrapper o (obj)
      (xml-write-attribute obj id p backend)))

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
;*    thread-specific ::WorkerHopThread ...                            */
;*---------------------------------------------------------------------*/
(define-method (thread-specific obj::WorkerHopThread)
   (unless (eq? obj (class-nil WorkerHopThread))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    thread-specific-set! ::WorkerHopThread ...                       */
;*---------------------------------------------------------------------*/
(define-method (thread-specific-set! obj::WorkerHopThread val)
   (unless (eq? obj (class-nil WorkerHopThread))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    thread-cleanup ::WorkerHopThread ...                             */
;*---------------------------------------------------------------------*/
(define-method (thread-cleanup obj::WorkerHopThread)
   (unless (eq? obj (class-nil WorkerHopThread))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    thread-cleanup-set! ::WorkerHopThread ...                        */
;*---------------------------------------------------------------------*/
(define-method (thread-cleanup-set! obj::WorkerHopThread val)
   (unless (eq? obj (class-nil WorkerHopThread))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-clone ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (js-clone obj)
   obj)

;*---------------------------------------------------------------------*/
;*    js-clone ::JsGlobalObject ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-clone obj::JsGlobalObject)
   (with-access::JsObject obj (properties __proto__ cmap elements)
      (duplicate::JsGlobalObject obj
	 (__proto__ (js-clone __proto__))
	 (cmap (js-clone cmap))
	 (elements (when (vector? elements) (vector-map js-clone elements)))
	 (properties (js-properties-clone properties)))))

;*---------------------------------------------------------------------*/
;*    js-clone ::JsConstructMap ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-clone obj::JsConstructMap)
   (with-access::JsConstructMap obj (names descriptors)
      (duplicate::JsConstructMap obj
	 (names (vector-copy names))
	 (descriptors (vector-copy descriptors)))))

;*---------------------------------------------------------------------*/
;*    js-donate ...                                                    */
;*    -------------------------------------------------------------    */
;*    This generic is used when a value is subject to a postMessage.   */
;*---------------------------------------------------------------------*/
(define-generic (js-donate obj worker::WorkerHopThread %this::JsGlobalObject)
   (cond
      ((cell? obj)
       (make-cell
	  (js-donate (cell-ref obj) worker %this)))
      ((epair? obj)
       (econs
	  (js-donate (car obj) worker %this)
	  (js-donate (cdr obj) worker %this)
	  (js-donate (cer obj) worker %this)))
      ((pair? obj)
       (cons
	  (js-donate (car obj) worker %this)
	  (js-donate (cdr obj) worker %this)))
      (else
       obj)))

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
;*    Constant strings ...                                             */
;*---------------------------------------------------------------------*/
(define js-string-undefined (js-string->jsstring "undefined"))
(define js-string-object (js-string->jsstring "object"))
(define js-string-symbol (js-string->jsstring "symbol"))
(define js-string-number (js-string->jsstring "number"))
(define js-string-boolean (js-string->jsstring "boolean"))
(define js-string-string (js-string->jsstring "string"))
(define js-string-function (js-string->jsstring "function"))

;*---------------------------------------------------------------------*/
;*    js-typeof ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.3       */
;*---------------------------------------------------------------------*/
(define-generic (js-typeof obj)
   (cond
      ((isa? obj JsFunction)
       js-string-function)
      ((isa? obj JsSymbol)
       js-string-symbol)
      ((isa? obj JsObject)
       js-string-object)
      ((or (real? obj) (integer? obj))
       js-string-number)
      ((boolean? obj)
       js-string-boolean)
      ((eq? obj (js-undefined))
       js-string-undefined)
      ((or (string? obj) (js-jsstring? obj))
       js-string-string)
      ((eq? obj (js-null))
       js-string-object)
      (else
       (js-string->jsstring (typeof obj)))))

;*---------------------------------------------------------------------*/
;*    js-arraybuffer-length ::JsArrayBuffer ...                        */
;*---------------------------------------------------------------------*/
(define-generic (js-arraybuffer-length o::JsArrayBuffer)
   (with-access::JsArrayBuffer o (data)
      (u8vector-length data)))

;*---------------------------------------------------------------------*/
;*    js-arraybuffer-ref ::JsArrayBuffer ...                           */
;*---------------------------------------------------------------------*/
(define-generic (js-arraybuffer-ref o::JsArrayBuffer index)
   (with-access::JsArrayBuffer o (data)
      (uint8->fixnum (u8vector-ref data index))))

;*---------------------------------------------------------------------*/
;*    js-arraybuffer-set! ::JsArrayBuffer ...                          */
;*---------------------------------------------------------------------*/
(define-generic (js-arraybuffer-set! o::JsArrayBuffer index val)
   (with-access::JsArrayBuffer o (data)
      (u8vector-set! data index (fixnum->uint8 val))))

;*---------------------------------------------------------------------*/
;*    js-buffer->jsbuffer ::JsObject ...                               */
;*---------------------------------------------------------------------*/
(define-generic (js-buffer->jsbuffer buf::JsObject args %this))

;*---------------------------------------------------------------------*/
;*    js-typedarray-ref ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (js-typedarray-ref::procedure a::JsTypedArray))

;*---------------------------------------------------------------------*/
;*    js-typedarray-set! ::JsTypedArray ...                            */
;*---------------------------------------------------------------------*/
(define-generic (js-typedarray-set!::procedure a::JsTypedArray))

   
