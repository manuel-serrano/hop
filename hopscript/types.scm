;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/types.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 21 10:17:45 2013                          */
;*    Last change :  Wed Mar  8 16:08:23 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
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
	      (errorlisteners::pair-nil (default '()))
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
	      (services::pair-nil (default '()))
	      (%exn (default #unspecified)))

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
	   (final-class JsAccessorDescriptor::JsPropertyDescriptor
	      get set
	      %get::procedure %set::procedure)
	   
	   (class JsPropertyCache
	      (cmap::obj (default #unspecified))
	      (pmap::obj (default #t))
	      (index::long (default -1))
	      (owner::obj (default #f))
	      (name::obj (default '||))
	      (method::obj (default #f)))
	   
	   (class JsConstructMap
	      (transition::pair (default (cons '|| -1)))
	      (nextmap (default #f))
	      (names::vector read-only (default '#()))
	      (methods::vector read-only (default '#()))
	      (backup::pair-nil (default '())))

	   ;; Literal strings that are not plain Scheme string
	   ;; for the sake of concat performance
	   (abstract-class JsStringLiteral
	      ;; the actual characters (string, tree, list)
	      weight::uint32
	      left::obj
	      (right::obj (default #f)))

	   (final-class JsStringLiteralASCII::JsStringLiteral)
	   (final-class JsStringLiteralUTF8::JsStringLiteral
	      (%idxutf8::long (default 0))
	      (%idxstr::long (default 0))
	      (%culen::uint32 (default #u32:0)))
	   
	   (class JsObject
	      (__proto__ (default (js-null)))
	      (mode::byte (default (js-object-default-mode)))
	      ;; (extensible::bool (default #t))
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
	      (js-math::JsMath (default (class-nil JsMath)))
	      (js-regexp::JsFunction (default (class-nil JsFunction)))
	      (js-regexp-prototype::JsRegExp (default (class-nil JsRegExp)))
	      (js-date::JsFunction (default (class-nil JsFunction)))
	      (js-json::JsJSON (default (class-nil JsJSON)))
	      (js-service-prototype::JsService (default (class-nil JsService)))
	      (js-hopframe-prototype (default (class-nil JsFunction)))
	      (js-server-prototype (default (class-nil JsFunction)))
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
	      (js-symbol-species (default (js-undefined)))
	      (js-main (default (js-null))))
	   
	   (class JsArray::JsObject
	      ;; (sealed::bool (default #f))
	      ;; (frozen::bool (default #f))
	      (length::uint32 (default #u32:0))
	      (vec::vector (default '#()))
	      (ilen::uint32 (default #u32:0)))

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
	      val::obj)

	   (class JsSymbol::JsObject
	      val::bstring)
	   
	   (class JsFunction::JsObject
	      (name::bstring read-only)
	      (constructor::obj read-only (default #f))
	      prototype::JsObject
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

	   (final-class JsFunction1::JsFunction)
	   (final-class JsFunction2::JsFunction)
	   (final-class JsFunction3::JsFunction)
	   (final-class JsFunction4::JsFunction)
	   (final-class JsFunction5::JsFunction)

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
	      (name (default (js-string->jsstring "Error")))
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
	      (resolver (default #f))
	      (rejecter (default #f))
	      worker
	      %this
	      (%name (default (gensym 'promise))))

	   (class JsGenerator::JsObject
	      %next)

	   (inline js-object-default-mode::byte)
	   
	   (inline js-object-mode-extensible?::bool ::JsObject)
	   (inline js-object-mode-extensible-set! ::JsObject ::bool)

	   (inline js-object-mode-frozen?::bool ::JsObject)
	   (inline js-object-mode-frozen-set! ::JsObject ::bool)

	   (inline js-object-mode-sealed?::bool ::JsObject)
	   (inline js-object-mode-sealed-set! ::JsObject ::bool)

	   (inline js-object-mode-inline?::bool ::JsObject)
	   (inline js-object-mode-inline-set! ::JsObject ::bool)

	   (inline js-object-mode-getter?::bool ::JsObject)
	   (inline js-object-mode-getter-set! ::JsObject ::bool)

	   (inline JS-OBJECT-MODE-EXTENSIBLE::byte)
	   (inline JS-OBJECT-MODE-SEALED::byte)
	   (inline JS-OBJECT-MODE-FROZEN::byte)
	   (inline JS-OBJECT-MODE-INLINE::byte)
	   (inline JS-OBJECT-MODE-GETTER::byte)
	   
	   (generic js-clone::obj ::obj)
	   (generic js-donate ::obj ::WorkerHopThread ::JsGlobalObject)

	   (inline js-undefined?::bool ::obj)
	   (inline js-undefined)
	   
	   (inline js-null?::bool ::obj)
	   (inline js-null)

	   %absent-value
	   (inline js-absent)
	   (inline js-absent?::bool ::obj)

	   (generic js-typeof obj)

	   (generic js-arraybuffer-length ::JsArrayBuffer)
	   (generic js-arraybuffer-ref ::JsArrayBuffer ::int)
	   (generic js-arraybuffer-set! ::JsArrayBuffer ::int ::obj)

	   (generic js-buffer->jsbuffer ::JsObject ::pair-nil ::JsGlobalObject)

	   (generic js-typedarray-ref::procedure ::JsTypedArray)
	   (generic js-typedarray-set!::procedure ::JsTypedArray)))

;*---------------------------------------------------------------------*/
;*    js-object-default-mode ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-object-default-mode)
   (bit-or (JS-OBJECT-MODE-EXTENSIBLE) (JS-OBJECT-MODE-INLINE)))

(define-inline (JS-OBJECT-MODE-EXTENSIBLE) 1)
(define-inline (JS-OBJECT-MODE-SEALED) 2)
(define-inline (JS-OBJECT-MODE-FROZEN) 4)
(define-inline (JS-OBJECT-MODE-INLINE) 8)
(define-inline (JS-OBJECT-MODE-GETTER) 16)

(define-macro (JS-OBJECT-MODE-EXTENSIBLE) 1)
(define-macro (JS-OBJECT-MODE-SEALED) 2)
(define-macro (JS-OBJECT-MODE-FROZEN) 4)
(define-macro (JS-OBJECT-MODE-INLINE) 8)
(define-macro (JS-OBJECT-MODE-GETTER) 16)

(define-inline (js-object-mode-extensible? o)
   (with-access::JsObject o (mode)
      (=fx (bit-and (JS-OBJECT-MODE-EXTENSIBLE) mode) (JS-OBJECT-MODE-EXTENSIBLE))))

(define-inline (js-object-mode-extensible-set! o flag)
   (with-access::JsObject o (mode)
      (if flag
	  (set! mode (bit-or mode (JS-OBJECT-MODE-EXTENSIBLE)))
	  (set! mode (bit-and mode (bit-not (JS-OBJECT-MODE-EXTENSIBLE)))))))

(define-inline (js-object-mode-frozen? o)
   (with-access::JsObject o (mode)
      (=fx (bit-and (JS-OBJECT-MODE-FROZEN) mode) (JS-OBJECT-MODE-FROZEN))))

(define-inline (js-object-mode-frozen-set! o flag)
   (with-access::JsObject o (mode)
      (if flag
	  (set! mode (bit-or mode (JS-OBJECT-MODE-FROZEN)))
	  (set! mode (bit-and mode (bit-not (JS-OBJECT-MODE-FROZEN)))))))

(define-inline (js-object-mode-sealed? o)
   (with-access::JsObject o (mode)
      (=fx (bit-and (JS-OBJECT-MODE-SEALED) mode) (JS-OBJECT-MODE-SEALED))))

(define-inline (js-object-mode-sealed-set! o flag)
   (with-access::JsObject o (mode)
      (if flag
	  (set! mode (bit-or mode (JS-OBJECT-MODE-SEALED)))
	  (set! mode (bit-and mode (bit-not (JS-OBJECT-MODE-SEALED)))))))

(define-inline (js-object-mode-inline? o)
   (with-access::JsObject o (mode)
      (=fx (bit-and (JS-OBJECT-MODE-INLINE) mode) (JS-OBJECT-MODE-INLINE))))

(define-inline (js-object-mode-inline-set! o flag)
   (with-access::JsObject o (mode)
      (if flag
	  (set! mode (bit-or mode (JS-OBJECT-MODE-INLINE)))
	  (set! mode (bit-and mode (bit-not (JS-OBJECT-MODE-INLINE)))))))

(define-inline (js-object-mode-getter? o)
   (with-access::JsObject o (mode)
      (=fx (bit-and (JS-OBJECT-MODE-GETTER) mode) (JS-OBJECT-MODE-GETTER))))

(define-inline (js-object-mode-getter-set! o flag)
   (with-access::JsObject o (mode)
      (if flag
	  (set! mode (bit-or mode (JS-OBJECT-MODE-GETTER)))
	  (set! mode (bit-and mode (bit-not (JS-OBJECT-MODE-GETTER)))))))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsWrapper ...                              */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value o::JsWrapper)
   (with-access::JsWrapper o (obj)
      obj))

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
   (with-access::JsConstructMap obj (names)
      (duplicate::JsConstructMap obj
	 (names (vector-copy names)))))

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
;*    js-undefined? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-undefined? obj)
   (eq? obj (js-undefined)))

;*---------------------------------------------------------------------*/
;*    js-undefined ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.1          */
;*---------------------------------------------------------------------*/
(define-inline (js-undefined)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    js-null? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (js-null? obj)
   (null? obj))

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
(define %absent-value
   (cons #f #f))

;*---------------------------------------------------------------------*/
;*    js-absent ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-absent)
   %absent-value)

(define-inline (js-absent? x)
   (eq? x %absent-value))

;*---------------------------------------------------------------------*/
;*    Constant strings ...                                             */
;*---------------------------------------------------------------------*/
(define js-string-undefined (js-ascii->jsstring "undefined"))
(define js-string-object (js-ascii->jsstring "object"))
(define js-string-symbol (js-ascii->jsstring "symbol"))
(define js-string-number (js-ascii->jsstring "number"))
(define js-string-boolean (js-ascii->jsstring "boolean"))
(define js-string-string (js-ascii->jsstring "string"))
(define js-string-function (js-ascii->jsstring "function"))

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
      ((js-jsstring? obj)
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
