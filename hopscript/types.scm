;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/types.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 21 10:17:45 2013                          */
;*    Last change :  Sun Feb 18 05:57:59 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
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
	   (final-class JsWrapperDescriptor::JsDataDescriptor
	      (value (default (js-undefined)))
	      %set::procedure)
	   
	   (final-class JsPropertyCache
	      (imap::obj (default #f))
	      (cmap::obj (default #f))
	      (pmap::obj (default #t))
	      (amap::obj (default #t))
	      (index::long (default -1))
	      (vindex::long (default (js-not-a-index)))
	      (owner::obj (default #f))
	      (point::long (default -1))
	      (name::obj (default '||))
	      (method::obj (default #f))
	      (pcache::obj (default #f))
	      (usage::symbol (default '-))
	      (cntmiss::long (default 0))
	      (cntimap::long (default 0))
	      (cntcmap::long (default 0))
	      (cntpmap::long (default 0))
	      (cntamap::long (default 0))
	      (cntvtable::long (default 0)))
	   
	   (final-class JsConstructMap
	      (%id::uint32 read-only (default (gencmapid)))
	      (size::long (default 0))
	      (props::vector (default '#()))
	      (methods::vector (default '#()))
	      (transitions::pair-nil (default '()))
	      (ctor::obj (default #f))
	      (single::bool read-only (default #f))
	      (vlen::long (default 0))
	      (vtable::vector (default '#())))
	   
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
	      (cmap::JsConstructMap (default (js-not-a-cmap)))
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
	      (js-function-strict-prototype::JsObject (default (class-nil JsObject)))
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
	      (js-generatorfunction-prototype::JsObject (default (class-nil JsObject)))
	      (js-buffer-proto (default #f))
	      (js-slowbuffer-proto (default #f))
	      (js-symbol-ctor::procedure (default list))
	      (js-symbol-table read-only (default (js-symbol-table)))
	      (js-symbol-iterator (default (js-undefined)))
	      (js-symbol-species (default (js-undefined)))
	      (js-symbol-hasinstance (default (js-undefined)))
	      (js-symbol-tostringtag (default (js-undefined)))
	      (js-main (default (js-null)))
	      (js-call (default #f))
	      (js-apply (default #f))
	      (js-vindex (default 0))
	      (js-pmap-valid::bool (default #f))
	      (js-input-port (default #f)))
	   
	   (final-class JsArray::JsObject
	      (length::uint32 (default #u32:0))
	      (ilen::uint32 (default #u32:0))
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
	      val::obj)
	   
	   (class JsSymbol::JsObject
	      val::obj)

	   (final-class JsSymbolLiteral
	      val::bstring)
	   
	   (class JsFunction::JsObject
	      (name::bstring read-only)
	      (constructor::obj read-only (default #f))
	      %prototype::JsObject
	      alloc::procedure
	      (construct::procedure read-only)
	      (constrsize::long (default 3))
	      (maxconstrsize::long (default 100))
	      (constrmap (default #f)) 
	      (arity::int read-only (default -1))
	      (minlen::int read-only (default -1))
	      (len::int read-only)
	      (rest::bool read-only (default #f))
	      (procedure::procedure read-only)
	      (method::procedure read-only)
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
	      rx::obj
	      (lastindex::JsValueDescriptor read-only)
	      (global::JsValueDescriptor read-only))
	   
	   (class JsBoolean::JsObject
	      (val::bool (default #t)))
	   
	   (class JsError::JsObject
	      (name (default ((@ js-string->jsstring __hopscript_stringliteral) "Error")))
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
	   
	   (inline js-object-default-mode::uint32)
	   (inline js-array-default-mode::uint32)
	   
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
	   
	   (inline js-object-mode-hasinstance?::bool ::JsObject)
	   (inline js-object-mode-hasinstance-set! ::JsObject ::bool)
	   
	   (inline js-object-mode-instance?::bool ::JsObject)
	   (inline js-object-mode-instance-set! ::JsObject ::bool)
	   
	   (inline JS-OBJECT-MODE-EXTENSIBLE::uint32)
	   (inline JS-OBJECT-MODE-SEALED::uint32)
	   (inline JS-OBJECT-MODE-FROZEN::uint32)
	   (inline JS-OBJECT-MODE-INLINE::uint32)
	   (inline JS-OBJECT-MODE-GETTER::uint32)
	   (inline JS-OBJECT-MODE-HASINSTANCE::uint32)
	   (inline JS-OBJECT-MODE-INSTANCE::uint32)
	   (inline JS-OBJECT-MODE-JSOBJECTTAG::uint32)
	   (inline JS-OBJECT-MODE-JSARRAYTAG::uint32)

	   (inline js-object-inline-elements?::bool ::JsObject)
	   (inline js-object-inline-ref ::JsObject ::long)
	   (inline js-object-inline-set! ::JsObject ::long ::obj)
	   
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
	   (generic js-typedarray-set!::procedure ::JsTypedArray)
	   
	   *js-not-a-cmap*
	   (inline js-not-a-cmap::JsConstructMap)
	   (inline js-not-a-index::long)
	   
	   (inline js-number?::bool ::obj)
	   (inline js-object?::bool ::obj)
	   (inline js-array?::bool ::obj)
	   (inline js-function?::bool ::obj)
	   (inline js-symbol?::bool ::obj)

	   (inline js-object-cmap ::JsObject)
	   
	   (inline js-object-properties ::JsObject)
	   (inline js-object-properties-set! ::JsObject ::obj)
	   
	   (inline js-object-mode::uint32 ::object)
	   (inline js-object-mode-set! ::object ::uint32)
	   
	   (gencmapid::uint32))
   
   (pragma (js-not-a-cmap side-effect-free)
	   (js-null side-effect-free)
	   (js-undefined side-effect-free)
	   (js-object-default-mode side-effect-free))
   
   (cond-expand
      ((not bigloo4.3a)
       (pragma (gencmapid default-inline)))))


;*---------------------------------------------------------------------*/
;*    js-object-default-mode ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-object-default-mode)
   (bit-oru32 (JS-OBJECT-MODE-EXTENSIBLE)
      (bit-oru32 (JS-OBJECT-MODE-INLINE) (JS-OBJECT-MODE-JSOBJECTTAG))))

(define-inline (js-array-default-mode)
   (bit-oru32 (js-object-default-mode) (JS-OBJECT-MODE-JSARRAYTAG)))

(define-inline (JS-OBJECT-MODE-EXTENSIBLE) #u32:1)
(define-inline (JS-OBJECT-MODE-SEALED) #u32:2)
(define-inline (JS-OBJECT-MODE-FROZEN) #u32:4)
(define-inline (JS-OBJECT-MODE-INLINE) #u32:8)
(define-inline (JS-OBJECT-MODE-GETTER) #u32:16)
(define-inline (JS-OBJECT-MODE-HASINSTANCE) #u32:32)
(define-inline (JS-OBJECT-MODE-INSTANCE) #u32:64)
(define-inline (JS-OBJECT-MODE-JSOBJECTTAG) #u32:128)
(define-inline (JS-OBJECT-MODE-JSARRAYTAG) #u32:256)

(define-macro (JS-OBJECT-MODE-EXTENSIBLE) #u32:1)
(define-macro (JS-OBJECT-MODE-SEALED) #u32:2)
(define-macro (JS-OBJECT-MODE-FROZEN) #u32:4)
(define-macro (JS-OBJECT-MODE-INLINE) #u32:8)
(define-macro (JS-OBJECT-MODE-GETTER) #u32:16)
(define-macro (JS-OBJECT-MODE-HASINSTANCE) #u32:32)
(define-macro (JS-OBJECT-MODE-INSTANCE) #u32:64)
(define-macro (JS-OBJECT-MODE-JSOBJECTTAG) #u32:128)
(define-macro (JS-OBJECT-MODE-JSARRAYTAG) #u32:256)

(define-inline (js-object-mode-extensible? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-EXTENSIBLE) (js-object-mode o))
      (JS-OBJECT-MODE-EXTENSIBLE)))

(define-inline (js-object-mode-extensible-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-EXTENSIBLE))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-EXTENSIBLE))))))

(define-inline (js-object-mode-frozen? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-FROZEN) (js-object-mode o))
      (JS-OBJECT-MODE-FROZEN)))

(define-inline (js-object-mode-frozen-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-FROZEN))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-FROZEN))))))

(define-inline (js-object-mode-sealed? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-SEALED) (js-object-mode o))
      (JS-OBJECT-MODE-SEALED)))

(define-inline (js-object-mode-sealed-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-SEALED))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-SEALED))))))

(define-inline (js-object-mode-inline? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-INLINE) (js-object-mode o))
      (JS-OBJECT-MODE-INLINE)))

(define-inline (js-object-mode-inline-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-INLINE))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-INLINE))))))

(define-inline (js-object-mode-getter? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-GETTER) (js-object-mode o))
      (JS-OBJECT-MODE-GETTER)))

(define-inline (js-object-mode-getter-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-GETTER))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-GETTER))))))

(define-inline (js-object-mode-hasinstance? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-HASINSTANCE) (js-object-mode o))
      (JS-OBJECT-MODE-HASINSTANCE)))

(define-inline (js-object-mode-hasinstance-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-HASINSTANCE))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-HASINSTANCE))))))

(define-inline (js-object-mode-instance? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-INSTANCE) (js-object-mode o))
      (JS-OBJECT-MODE-INSTANCE)))

(define-inline (js-object-mode-instance-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-INSTANCE))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-INSTANCE))))))

;*---------------------------------------------------------------------*/
;*    js-object-inline-elements? ...                                   */
;*    -------------------------------------------------------------    */
;*    See bgl_make_jsobject in _bglhopscript.c.                        */
;*---------------------------------------------------------------------*/
(define-inline (js-object-inline-elements?::bool o::JsObject)
   (cond-expand
      (bigloo-c
       (with-access::JsObject o (elements)
	  (eq? elements
	     (pragma::obj "BVECTOR( (obj_t)(( ((obj_t *)(&(((BgL_jsobjectz00_bglt)(CREF($1)))->BgL_elementsz00))) + 1)))"
		o))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    js-object-inline-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-object-inline-ref o::JsObject idx::long)
   (cond-expand
      (bigloo-c
       (pragma::obj "VECTOR_REF( BVECTOR( (obj_t)(( ((obj_t *)(&(((BgL_jsobjectz00_bglt)(CREF($1)))->BgL_elementsz00))) + 1))), $2 )" o idx))
      (else
       (with-access::JsObject o (elements)
	  (vector-ref element idx)))))

;*---------------------------------------------------------------------*/
;*    js-object-inline-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-object-inline-set! o::JsObject idx::long val::obj)
   (cond-expand
      (bigloo-c
       (pragma::obj "VECTOR_SET( BVECTOR( (obj_t)(( ((obj_t *)(&(((BgL_jsobjectz00_bglt)(CREF($1)))->BgL_elementsz00))) + 1))), $2, $3 )" o idx val))
      (else
       (with-access::JsObject o (elements)
	  (vector-set! element idx val)))))

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
	     (let ((val (get-value obj)))
		(if (vector? val)
		    (display (format "vector[~a]" (vector-length val)) port)
		    (display (typeof val) port)))
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
   (with-access::JsObject obj (__proto__ cmap elements)
      (let ((nobj (duplicate::JsGlobalObject obj
		     (__proto__ (js-clone __proto__))
		     (cmap (js-clone cmap))
		     (elements (vector-map js-clone elements)))))
	 (let ((properties (js-object-properties obj)))
	    (js-object-properties-set! nobj (js-properties-clone properties))
	    (js-object-mode-set! nobj (js-object-mode obj))
	    nobj))))

;*---------------------------------------------------------------------*/
;*    js-clone ::JsConstructMap ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-clone obj::JsConstructMap)
   (if (eq? obj (js-not-a-cmap))
       obj
       (with-access::JsConstructMap obj (props)
	  (duplicate::JsConstructMap obj
	     (props (vector-copy props))))))

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
      ((isa? obj JsSymbolLiteral)
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

;*---------------------------------------------------------------------*/
;*    gencmapid ...                                                    */
;*---------------------------------------------------------------------*/
(define (gencmapid)
   (set! cmapid (+fx 1 cmapid))
   (fixnum->uint32 cmapid))

;*---------------------------------------------------------------------*/
;*    cmapid ...                                                       */
;*---------------------------------------------------------------------*/
(define cmapid 0)

;*---------------------------------------------------------------------*/
;*    *js-not-a-cmap* ...                                              */
;*---------------------------------------------------------------------*/
(define *js-not-a-cmap*
   (instantiate::JsConstructMap
      (%id 0)))

;*---------------------------------------------------------------------*/
;*    js-not-a-cmap ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-not-a-cmap::JsConstructMap)
   *js-not-a-cmap*)

;*---------------------------------------------------------------------*/
;*    js-not-a-index ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-not-a-index::long)
   (bit-lsh 1 28))

;*---------------------------------------------------------------------*/
;*    js-number? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-number? o)
   (or (fixnum? o) (flonum? o) (int32? o) (uint32? o)))

;*---------------------------------------------------------------------*/
;*    js-object? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-object? o)
   (and (%object? o) (>u32 (js-object-mode o) #u32:0)))
;*    (isa? o JsObject))                                               */

;*---------------------------------------------------------------------*/
;*    js-array? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-array? o)
   (and (%object? o) (>=u32 (js-object-mode o) (JS-OBJECT-MODE-JSARRAYTAG))))
;*    (isa? o JsArray))                                                */

;*---------------------------------------------------------------------*/
;*    js-function? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-function? o)
   (isa? o JsFunction))

;*---------------------------------------------------------------------*/
;*    js-symbol? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-symbol? o)
   (isa? o JsSymbolLiteral))

;*---------------------------------------------------------------------*/
;*    js-object-cmap ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-object-cmap o)
   (with-access::JsObject o (cmap) cmap))

;*---------------------------------------------------------------------*/
;*    js-object-properties ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-object-properties o)
   (object-widening o))

;*---------------------------------------------------------------------*/
;*    js-object-properties-set! ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-object-properties-set! o p)
   (object-widening-set! o p))

;*---------------------------------------------------------------------*/
;*    js-object-mode ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-object-mode o)
   (fixnum->uint32 (object-header-size o)))

;*---------------------------------------------------------------------*/
;*    js-object-mode-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-object-mode-set! o p)
   (object-header-size-set! o (uint32->fixnum p)))
