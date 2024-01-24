;*=====================================================================*/
;*    serrano/prgm/project/hop/3.7.x/hopscript/types.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 21 10:17:45 2013                          */
;*    Last change :  Wed Jan 24 18:18:45 2024 (serrano)                */
;*    Copyright   :  2013-24 Manuel Serrano                            */
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
	__hopscript_symbol
	__hopscript_date
	__hopscript_error
	__hopscript_boolean
	__hopscript_private
	__hopscript_function
	__hopscript_property
	__hopscript_profile
	__hopscript_lib)

   (extern (include "bglhopscript.h")
	   (include "bglhopscript_malloc.h"))

   (extern (macro $pointer?::bool (::obj) "POINTERP")
	   (macro $hop-object-header-size::long (::object)
		  "HOP_OBJECT_HEADER_SIZE")
	   (macro $hop-object-header-size-set!::long (::object ::long)
		  "HOP_OBJECT_HEADER_SIZE_SET")
	   ($js-init-jsalloc::int (::uint32)
	      "bgl_init_jsalloc")
	   ($js-init-worker-jsalloc::int ()
	      "bgl_init_worker_jsalloc")
	   ($js-init-jsalloc-proxy::int (::obj ::obj ::uint32)
	      "bgl_init_jsalloc_proxy")
	   ($js-init-jsalloc-function::int (::JsConstructMap ::JsConstructMap
					      ::obj ::obj
					      ::uint32)
	      "bgl_init_jsalloc_function")
	   ($js-init-jsalloc-method::int (::JsConstructMap ::JsConstructMap
					    ::obj ::obj
					    ::uint32)
	      "bgl_init_jsalloc_method")
	   ($js-init-jsalloc-procedure::int (::JsConstructMap
					       ::uint32)
	      "bgl_init_jsalloc_procedure")
	   ($js-init-jsalloc-stringliteralascii::int (::uint32 ::uint32 ::obj ::uint32)
	      "bgl_init_jsalloc_stringliteralascii")
	   ($js-init-jsalloc-date::int (::uint32)
	      "bgl_init_jsalloc_date")
	   ($js-make-jsobject::JsObject (::int ::JsConstructMap ::obj ::uint32)
	      "bgl_make_jsobject")
	   ($js-make-jsproxy::JsProxy (::obj ::obj ::obj ::obj ::obj ::uint32)
	      "bgl_make_jsproxy")
	   ($js-make-jsfunction::JsFunction (::procedure
					       ::long ::long
					       ::obj ::obj)
	      "bgl_make_jsfunction")
	   ($js-make-jsmethod::JsMethod (::procedure ::procedure
					   ::long ::long
					   ::obj ::obj)
	      "bgl_make_jsmethod")
	   ($js-make-jsprocedure::JsProcedure (::procedure ::long ::obj)
	      "bgl_make_jsprocedure")
	   ($js-make-stringliteralascii::JsStringLiteralASCII (::uint32 ::obj ::obj)
	      "bgl_make_jsstringliteralascii")
	   ($js-make-jsgenerator::JsGenerator (::obj ::obj ::long ::obj ::uint32)
	      "bgl_make_jsgenerator")
	   ($js-make-jsdate::JsDate (::obj ::obj)
	      "bgl_make_jsdate")
	   (macro $js-object-inline-elements::vector (::JsObject)
		  "HOP_JSOBJECT_INLINE_ELEMENTS")
	   (macro $js-object-inline-elements-length::long (::JsObject)
		  "HOP_JSOBJECT_INLINE_ELEMENTS_LENGTH")
	   (macro $js-object-inline-elements-ref::obj (::JsObject ::long)
		  "HOP_JSOBJECT_INLINE_ELEMENTS_REF")
	   (macro $js-object-inline-elements-set!::obj (::JsObject ::long ::obj)
		  "HOP_JSOBJECT_INLINE_ELEMENTS_SET")
	   (macro $js-object-vector-inline?::bool (::JsArray)
		  "HOP_JSARRAY_VECTOR_INLINEP")
	   (macro $bgl-object-class-set!::long (::JsObject ::long)
		  "BGL_OBJECT_CLASS_NUM_SET")
	   (macro $bgl-class-num::long (::class)
		  "BGL_CLASS_NUM")

 	   (macro $js-object?::bool (::obj ::uint32)
		  "HOP_JSOBJECTP")
 	   (macro $js-array?::bool (::obj ::uint32)
		  "HOP_JSARRAYP")
	   (macro $js-jsstring?::bool (::obj ::uint32)
		  "HOP_JSSTRINGP")
	   (macro $js-object-jsstring?::bool (::obj ::uint32)
		  "HOP_OBJECT_JSSTRINGP")
	   (macro $js-procedure?::bool (::obj ::uint32)
		  "HOP_JSPROCEDUREP")
	   (macro $js-object-procedure?::bool (::obj ::uint32)
		  "HOP_OBJECT_JSPROCEDUREP")
	   (macro $js-object-mode-inline?::bool (::JsObject ::uint32)
		  "HOP_JSOBJECT_MODE_INLINEP")

	   ($$hopscript-breakpoint::obj (::obj) "hopjs_breakpoint"))
   
   (export (class WorkerHopThread::hopthread
	      (%loop (default #f))
	      (keep-alive::bool (default #f))
	      (mutex::mutex read-only (default (make-mutex)))
	      (condv::condvar read-only (default (make-condition-variable)))
	      (prehook (default #f))
	      (alivep (default #f))
	      (listeners::pair-nil (default '()))
	      (exitlisteners::pair-nil (default '()))
	      (errorlisteners::pair-nil (default '()))
	      (onmessage::obj (default (js-undefined)))
	      (onexit::obj (default (js-undefined)))
	      (%this::JsGlobalObject (default (class-nil JsGlobalObject)))
	      (%process (default #f))
	      (%retval::int (default 0))
	      (async (default #f))
	      (state::symbol (default 'init))
	      (module-cache::obj (default #f))
	      (parent::obj (default #f))
	      (subworkers::pair-nil (default '()))
	      (uvhandles::vector (default (vector (cons #unspecified #unspecified) (cons #unspecified #unspecified) (cons #unspecified #unspecified)))) 
	      (%call (default #f))
	      (handlers::pair-nil (default '()))
	      (services::pair-nil (default '()))
	      (%exn (default #unspecified)))

	   (class MessageEvent::event
	      data::obj)
	   
	   (class JsPropertyDescriptor
	      name::obj
	      (configurable (default #f))
	      (enumerable (default #f)))
	   (class JsDataDescriptor::JsPropertyDescriptor
	      (writable (default #f)))
	   (final-class JsValueDescriptor::JsDataDescriptor
	      (value (default (js-undefined))))
	   (final-class JsAccessorDescriptor::JsPropertyDescriptor
	      get %get::procedure
	      set %set::procedure)
	   (final-class JsWrapperDescriptor::JsDataDescriptor
	      (%get::procedure read-only)
	      (%set::procedure read-only))
	   
	   (final-class JsPropertyCache
	      (js-property-cache-init!)
	      (imap::JsConstructMap (default (js-not-a-pmap)))
	      (emap::JsConstructMap (default (js-not-a-pmap)))
	      (cmap::JsConstructMap (default (js-not-a-pmap)))
	      (pmap::JsConstructMap (default (js-not-a-pmap)))
	      (nmap::JsConstructMap (default (js-not-a-pmap)))
	      (amap::JsConstructMap (default (js-not-a-pmap)))
	      (xmap::JsConstructMap (default (js-not-a-pmap)))
	      (nextemap::obj (default (js-not-a-pmap)))
	      (nextnmap::obj (default (js-not-a-pmap)))
	      (iindex::long (default -1))
	      (eindex::long (default -1))
	      (cindex::long (default -1))
	      (pindex::long (default -1))
	      (nindex::long (default -1))
	      (aindex::long (default -1))
	      (vindex::long (default (js-not-a-index)))
	      (owner::obj (default #f))
	      (src::bstring read-only (default ""))
	      (point::long (default -1))
	      (name::JsStringLiteral (default (class-nil JsStringLiteralASCII)))
	      (method::obj (default #f))
	      (function::obj (default #f))
	      (pctable::obj (default #f))
	      (usage::symbol (default '-))
	      (registered::bool (default #f))
	      (cntmiss::uint32 (default #u32:0))
	      (cntimap::uint32 (default #u32:0))
	      (cntemap::uint32 (default #u32:0))
	      (cntcmap::uint32 (default #u32:0))
	      (cntpmap::uint32 (default #u32:0))
	      (cntnmap::uint32 (default #u32:0))
	      (cntamap::uint32 (default #u32:0))
	      (cntxmap::uint32 (default #u32:0))
	      (cntvtable::uint32 (default #u32:0)))
	   
	   (final-class JsConstructMap
	      (%id::uint32 read-only)
	      (lock read-only (default (make-spinlock "JsConstructMap")))
	      (props::vector (default '#()))
	      (methods::vector (default '#()))
	      ;; list for all non __proto__ transitions
	      (rtransitions::pair-nil (default '()))
	      ;; hashtable for __proto__ transitions
	      (ptransitions::obj (default #f))
	      (detachcnt::long (default 0))
	      (detachlocs::pair-nil (default '()))
	      (ctor::obj (default #f))
	      (single::bool read-only (default #f))
	      (vtable::vector (default '#()))
	      (parent::JsConstructMap (default (class-nil JsConstructMap)))
	      ;; sealed class methods procedures and names
	      (mptable::vector (default '#()))
	      (mrtable::vector (default '#()))
	      (mntable::vector (default '#())))

	   ;; Literal strings that are not plain Scheme strings.
	   ;; For performance sake they are trees.
	   (abstract-class JsStringLiteral
	      length::uint32
	      left::obj
	      (right::obj (default (js-not-a-string-cache))))
	   
	   (class JsStringLiteralASCII::JsStringLiteral)
	   
	   (final-class JsStringLiteralIndex::JsStringLiteralASCII
	      (index::uint32 read-only))
	   
	   (class JsStringLiteralSubstring::JsStringLiteralASCII)
	   
	   (class JsStringLiteral8BITS::JsStringLiteralASCII)
	   
	   (class JsStringLiteralBuffer::JsStringLiteralSubstring)
	   
	   (final-class JsStringLiteralUTF8::JsStringLiteral
	      (%idxutf8::long (default 0))
	      (%idxstr::long (default 0))
	      (%culen::uint32 (default #u32:0)))

	   (class JsObject
	      (cmap::JsConstructMap (default (js-not-a-cmap)))
	      (elements (default '#())))

	   (class JsRecord::JsObject)
	   
	   (class JsWrapper::JsObject
	      obj
	      data)

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
	   (class JsBigInt64Array::JsTypedArray)
	   (class JsBigUint64Array::JsTypedArray)
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
	      val::JsStringLiteral)

	   (class JsProcedure::JsObject
	      ;; see js-call@public.scm for arity documentation
	      (procedure::procedure read-only)
	      (arity::int read-only (default -1)))
	      
	   (class JsProcedureInfo::JsProcedure
	      (info::vector read-only (default '#())))
	      
	   (class JsFunction::JsProcedureInfo
	      (constrsize::int (default 3))
	      ;; alloc cannot be read-only, see _buffer.scm
	      alloc::procedure
	      (constrmap::JsConstructMap (default (js-not-a-cmap)))
	      prototype)
	   
	   (class JsMethod::JsFunction
	      (method::procedure read-only))
	   
	   (class JsService::JsFunction
	      (worker::obj read-only)
	      (svc::obj read-only))

	   (class JsClass::JsFunction
	      (constructor::procedure read-only)
	      (clazz read-only (default #f)))
	   
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

	   (class JsBigInt::JsObject
	      (val::bignum (default #z0)))

	   (class JsMath::JsObject)
	   
	   (class JsRegExp::JsObject
	      rx::obj
	      (source read-only)
	      (flags::uint32 read-only))
	   
	   (class JsBoolean::JsObject
	      (val::bool (default #t)))
	   
	   (class JsError::JsObject
	      name
	      msg
	      %this
	      (stack (default #f))
	      (fname (default #f))
	      (location (default #f)))
	   
	   (class JsDate::JsObject
	      (time (default #f))
	      (%val (default #f)))
	   
	   (class JsJSON::JsObject)
	   
	   (class JsModule::JsObject
	      (%module (default #f))
	      ;; EVARS is the vector implementing exported variables; 
	      ;; each entry holds the value of the corresponing variable
	      (evars::vector (default '#()))
	      (defaultexport::obj (default (js-undefined)))
	      ;; EXPORTS is the description of the exported variable;
	      ;; each entry is: #(var-id index redirect-index|-1 ronly)
	      (exports::vector (default '#()))
	      (namespace::obj (default #f))
	      ;; IMPORTS is the list of imported JsModule as returned
	      ;; by NODEJS-IMPORT-MODULE
	      (imports::vector (default '#()))
	      (redirects::vector (default '#()))
	      (default (default #f))
	      (checksum (default 0)))
	   
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
	      (%name (default #f)))
	   
	   (final-class JsGenerator::JsObject
	      %next
	      (%bounce (default #unspecified))
	      (%env::vector read-only))

	   (abstract-class JsYield::JsObject
	      ;; only used for stack allocation, (see public_expd.sch)
	      %vheader
	      %vlength::ulong
	      %vobj0
	      %vobj1)

	   (final-class JsProxy::JsObject
	      (handler::JsObject (default (class-nil JsObject)))
	      (getcache read-only (default (instantiate::JsPropertyCache)))
	      (setcache read-only (default (instantiate::JsPropertyCache)))
	      (applycache read-only (default (instantiate::JsPropertyCache))))

	   (class JsMap::JsObject
	      (mapdata read-only)
	      vec::vector
	      cursor::long)

	   (class JsGlobalObject::JsObject
	      (name read-only)
	      (worker::obj (default #f))
	      (js-hop-builtin::JsObject (default (class-nil JsObject)))
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
	      (js-bigint64array::JsFunction (default (class-nil JsFunction)))
	      (js-biguint64array::JsFunction (default (class-nil JsFunction)))
	      (js-float32array::JsFunction (default (class-nil JsFunction)))
	      (js-float64array::JsFunction (default (class-nil JsFunction)))
	      (js-dataview::JsFunction (default (class-nil JsFunction)))
	      (js-vector::JsFunction (default (class-nil JsFunction)))
	      (js-vector-prototype::JsArray (default (class-nil JsArray)))
	      (js-boolean::JsFunction (default (class-nil JsFunction)))
	      (js-string::JsFunction (default (class-nil JsFunction)))
	      (js-string-prototype::JsString (default (class-nil JsString)))
	      (js-symbol::JsFunction (default (class-nil JsFunction)))
	      (js-number::JsFunction (default (class-nil JsFunction)))
	      (js-number-prototype::JsNumber (default (class-nil JsNumber)))
	      (js-bigint::JsFunction (default (class-nil JsFunction)))
	      (js-bigint-prototype::JsBigInt (default (class-nil JsBigInt)))
	      (js-function::JsFunction (default (class-nil JsFunction)))
	      (js-function-prototype::JsFunction (default (class-nil JsFunction)))
	      (js-math::JsObject (default (class-nil JsObject)))
	      (js-regexp::JsFunction (default (class-nil JsFunction)))
	      (js-regexp-prototype::JsRegExp (default (class-nil JsRegExp)))
	      (js-regexp-last-match::vector (default (make-vector 3)))
	      (js-date::JsFunction (default (class-nil JsFunction)))
	      (js-date-prototype::JsDate (default (class-nil JsDate)))
	      (js-json::JsObject (default (class-nil JsObject)))
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
	      (js-promise-prototype::JsPromise (default (class-nil JsPromise)))
	      (js-proxy (default (class-nil JsFunction)))
	      (js-worker-prototype::JsWorker (default (class-nil JsWorker)))
	      (js-generator-prototype::JsObject (default (class-nil JsObject)))
	      (js-generatorfunction-prototype::JsObject (default (class-nil JsObject)))
	      (js-buffer-proto (default #f))
	      (js-slowbuffer-proto (default #f))
	      (js-stats-proto (default #f))
	      (js-symbol-ctor::procedure (default list))
	      (js-symbol-table read-only (default (js-symbol-table)))
	      (js-symbol-iterator (default (js-undefined)))
	      (js-symbol-species (default (js-undefined)))
	      (js-symbol-hasinstance (default (js-undefined)))
	      (js-symbol-tostringtag (default (js-undefined)))
	      (js-symbol-unscopables (default (js-undefined)))
	      (js-main (default (js-null)))
	      (js-call (default #f))
	      (js-apply (default #f))
	      (js-vindex (default 0))
	      (js-input-port (default #f))
	      (js-new-target (default (js-undefined)))
	      ;; functions property
	      (js-function-prototype-property-rw (default #f))
	      (js-function-prototype-property-ro (default #f))
	      (js-function-prototype-property-null (default #f))
	      (js-function-prototype-property-undefined (default #f))
	      (js-function-strict-elements (default #f))
	      ;; dom elements
	      (js-xml-markups (default #f))
	      ;; char table
	      (char-table::vector (default '#()))
	      ;; regexp tmp positions buffer
	      (js-regexp-positions::vector (default (make-vector 2)))
	      ;; pcaches
	      (js-nodejs-pcache::vector (default '#()))
	      (js-object-pcache::vector (default '#()))
	      (js-array-pcache::vector (default '#()))
	      (js-function-pcache::vector (default '#()))
	      (js-regexp-pcache::vector (default '#()))
	      (js-string-pcache::vector (default '#()))
	      (js-stringliteral-pcache::vector (default '#()))
	      (js-spawn-pcache::vector (default '#()))
	      (js-service-pcache::vector (default '#()))
	      (js-number-pcache::vector (default '#()))
	      (js-json-pcache::vector (default '#()))
	      (js-proxy-pcache::vector (default '#()))
	      (js-generator-pcache::vector (default '#()))
	      (js-promise-pcache::vector (default '#()))
	      ;; cmaps
	      (js-tcp-cmap (default #f))
	      (js-udp-cmap (default #f))
	      (js-pipe-cmap (default #f))
	      (js-tty-cmap (default #f))
	      (js-buffer-cmap (default #f))
	      (js-slowbuffer-cmap (default #f))
	      (js-initial-cmap (default (class-nil JsConstructMap)))
	      (js-arguments-cmap (default (class-nil JsConstructMap)))
	      (js-strict-arguments-cmap (default (class-nil JsConstructMap)))
	      (js-array-cmap (default (class-nil JsConstructMap)))
	      (js-function-cmap (default (class-nil JsConstructMap)))
	      (js-function-sans-prototype-cmap (default (class-nil JsConstructMap)))
	      (js-function-strict-cmap (default (class-nil JsConstructMap)))
	      (js-function-strict-bind-cmap (default (class-nil JsConstructMap)))
	      (js-function-writable-cmap (default (class-nil JsConstructMap)))
	      (js-function-writable-strict-cmap (default (class-nil JsConstructMap)))
	      (js-function-prototype-cmap (default (class-nil JsConstructMap)))
	      (js-generator-cmap (default (class-nil JsConstructMap)))
	      (js-yield-cmap (default (class-nil JsConstructMap)))
	      (js-regexp-cmap (default (class-nil JsConstructMap)))
	      (js-regexp-exec-cmap (default (class-nil JsConstructMap)))
	      (js-scope-cmap (default (class-nil JsConstructMap)))
	      (js-date-cmap (default (class-nil JsConstructMap)))
	      (js-promise-cmap (default (class-nil JsConstructMap)))
	      (js-property-descriptor-value-cmap (default (class-nil JsConstructMap)))
	      (js-property-descriptor-getter-cmap (default (class-nil JsConstructMap))))

	   (class JsResponse
	      (%this::JsGlobalObject read-only)
	      (obj::JsObject read-only))

	   (js-property-cache-init!::JsPropertyCache ::obj)
	   
	   (inline js-make-jsobject::JsObject ::int ::obj ::obj)
	   (inline js-make-jsrecord::JsObject ::int ::obj ::obj ::obj)
	   (inline js-make-jsconstructmap::JsConstructMap
	      #!key (%id (gencmapid))
	      single ctor
	      (methods '#()) (props '#())
	      (mptable '#()) (mrtable '#()) (mntable '#()))
	   
	   (js-jsconstructmap-size::long ::JsConstructMap)

	   (inline js-object-default-mode::uint32)
	   (inline js-globalobject-default-mode::uint32)
	   (inline js-proxy-default-mode::uint32)
	   (inline js-record-default-mode::uint32)
	   (inline js-array-default-mode::uint32)
	   (inline js-vector-default-mode::uint32)
	   (inline js-function-default-mode::uint32)
	   (inline js-procedure-default-mode::uint32)
	   (inline js-procedure-hopscript-mode::uint32)
	   (inline js-arraybuffer-default-mode::uint32)
	   (inline js-typedarray-default-mode::uint32)
	   (inline js-dataview-default-mode::uint32)
	   (inline js-jsstring-default-ascii-mode::uint32)
	   (inline js-jsstring-normalized-ascii-mode::uint32)
	   (inline js-jsstring-default-index-mode::uint32)
	   (inline js-jsstring-default-substring-mode::uint32)
	   (inline js-jsstring-default-buffer-mode::uint32)
	   (inline js-jsstring-default-utf8-mode::uint32)
	   (inline js-jsstring-normalized-utf8-mode)
	   (inline js-jsstring-normalized-private-mode::uint32)
	   
	   (inline js-jsstring-normalized-mode::uint32)
	   (inline js-jsstring-normalized-index-mode::uint32)
	   (inline js-jsstring-normalized-substring-mode::uint32)
	   
	   (inline js-object-mode-extensible?::bool ::JsObject)
	   (inline js-object-mode-plain-extensible?::bool ::JsObject)
	   (inline js-object-mode-extensible-set! ::JsObject ::bool)
	   
	   (inline js-object-mode-frozen?::bool ::JsObject)
	   (inline js-object-mode-frozen-set! ::JsObject ::bool)
	   
	   (inline js-object-mode-sealed?::bool ::JsObject)
	   (inline js-object-mode-sealed-set! ::JsObject ::bool)
	   
	   (inline js-object-mode-inline?::bool ::JsObject)
	   (inline js-object-mode-inline-set! ::JsObject ::bool)
	   
	   (inline js-object-mode-isprotoof?::bool ::JsObject)
	   (inline js-object-mode-isprotoof-set! ::JsObject ::bool)
	   
	   (inline js-object-mode-hasinstance?::bool ::JsObject)
	   (inline js-object-mode-hasinstance-set! ::JsObject ::bool)
	   
	   (inline js-object-mode-arrayinline?::bool ::JsObject)
	   (inline js-object-mode-arrayinline-set! ::JsObject ::bool)
	   
	   (inline js-object-mode-arrayholey?::bool ::JsObject)
	   (inline js-object-mode-arrayholey-set! ::JsObject ::bool)
	   
	   (inline js-object-mode-plain?::bool ::JsObject)
	   (inline js-object-mode-plain-set! ::JsObject ::bool)
	   
	   (inline js-object-mode-enumerable?::bool ::JsObject)
	   (inline js-object-mode-enumerable-set! ::JsObject ::bool)
	   
	   (inline js-object-mode-hasnumeralprop?::bool ::JsObject)
	   (inline js-object-mode-hasnumeralprop-set! ::JsObject ::bool)

	   (inline js-proxy-mode-revoked?::bool ::JsObject)
	   (inline js-proxy-mode-revoked-set! ::JsObject ::bool)

	   (inline js-proxy-mode-function?::bool ::JsObject)
	   (inline js-proxy-mode-function-set! ::JsObject ::bool)

	   (inline js-procedure-hopscript-mode?::bool ::JsProcedure)
	   (inline js-procedure-hopscript-mode-set! ::JsProcedure ::bool)
	   
	   (inline JS-OBJECT-MODE-JSSTRINGTAG::uint32)
	   (inline JS-OBJECT-MODE-JSFUNCTIONTAG::uint32)
	   (inline JS-OBJECT-MODE-JSVECTORTAG::uint32)
	   (inline JS-OBJECT-MODE-JSARRAYTAG::uint32)
	   (inline JS-OBJECT-MODE-JSOBJECTTAG::uint32)
	   (inline JS-OBJECT-MODE-JSPROCEDURETAG::uint32)
	   
	   (inline JS-OBJECT-MODE-EXTENSIBLE::uint32)
	   (inline JS-OBJECT-MODE-SEALED::uint32)
	   (inline JS-OBJECT-MODE-FROZEN::uint32)
	   (inline JS-OBJECT-MODE-INLINE::uint32)
	   (inline JS-OBJECT-MODE-ISPROTOOF::uint32)
	   (inline JS-OBJECT-MODE-HASINSTANCE::uint32)
	   (inline JS-OBJECT-MODE-JSARRAYINLINE::uint32)
	   (inline JS-OBJECT-MODE-ENUMERABLE::uint32)
	   (inline JS-OBJECT-MODE-HASNUMERALPROP::uint32)
	   (inline JS-OBJECT-MODE-PLAIN::uint32)
	   
	   (inline JS-OBJECT-MODE-JSPROXYREVOKED::uint32)
	   (inline JS-OBJECT-MODE-JSPROXYFUNCTION::uint32)
	   (inline JS-OBJECT-MODE-JSPROCEDUREHOPSCRIPT::uint32)
	   
	   (inline JS-OBJECT-MODE-JSARRAYHOLEY::uint32)
	   (inline JS-OBJECT-MODE-JSSTRINGNORMALIZED::uint32)
	   (inline JS-OBJECT-MODE-JSSTRINGASCII::uint32)
	   (inline JS-OBJECT-MODE-JSSTRINGINDEX::uint32)
	   (inline JS-OBJECT-MODE-JSSTRINGUTF8::uint32)
	   (inline JS-OBJECT-MODE-JSSTRINGSUBSTRING::uint32)
	   (inline JS-OBJECT-MODE-JSSTRINGBUFFER::uint32)
	   (inline JS-OBJECT-MODE-JSSTRINGPRIVATE::uint32)

	   (inline JS-REGEXP-FLAG-IGNORECASE::uint32)
	   (inline JS-REGEXP-FLAG-MULTILINE::uint32)
	   (inline JS-REGEXP-FLAG-GLOBAL::uint32)
	   (inline JS-REGEXP-FLAG-UNICODE::uint32)
	   (inline JS-REGEXP-FLAG-INDICES::uint32)
	   (inline JS-REGEXP-FLAG-DOTALL::uint32)
	   (inline JS-REGEXP-FLAG-UNICODESETS::uint32)
	   (inline JS-REGEXP-FLAG-STICKY::uint32)

	   (inline js-regexp-flags-ignorecase?::bool ::uint32)
	   (inline js-regexp-flags-multiline?::bool ::uint32)
	   (inline js-regexp-flags-global?::bool ::uint32)
	   (inline js-regexp-flags-unicode?::bool ::uint32)
	   (inline js-regexp-flags-indices?::bool ::uint32)
	   (inline js-regexp-flags-dotall?::bool ::uint32)
	   (inline js-regexp-flags-unicodesets?::bool ::uint32)
	   (inline js-regexp-flags-sticky?::bool ::uint32)

	   (inline js-object-inline-elements::vector ::JsObject)
	   (inline js-object-inline-elements-length::long ::JsObject)
	   (inline js-object-alloc-elements::vector ::JsObject)	   
	   (inline js-object-noinline-elements::vector ::JsObject)
	   (inline js-object-inline-length::long ::JsObject)
	   (inline js-object-noinline-length::long ::JsObject)
	   (inline js-object-length::long ::JsObject)
	   (inline js-object-inline-ref ::JsObject ::long)
	   (inline js-object-inline-set! ::JsObject ::long ::obj)
	   (inline js-object-noinline-relative-ref ::JsObject ::long)
	   (inline js-object-noinline-relative-set! ::JsObject ::long ::obj)
	   (inline js-object-noinline-ref ::JsObject ::long)
	   (inline js-object-noinline-set! ::JsObject ::long ::obj)
	   (inline js-object-ref ::JsObject ::long)
	   (inline js-object-set! ::JsObject ::long ::obj)

	   (js-object-for-each ::JsObject ::procedure)

	   (inline js-vector-inline-ref ::JsArray ::long)
	   (inline js-vector-inline-set! ::JsArray ::long ::obj)

	   (generic js-clone::obj ::obj)
	   (generic js-donate ::obj ::WorkerHopThread ::JsGlobalObject)
	   
	   (inline js-undefined?::bool ::obj)
	   (inline js-undefined)
	   
	   (inline js-null?::bool ::obj)
	   (inline js-null)
	   
	   %absent-value
	   (inline js-absent)
	   (inline js-absent?::bool ::obj)
	   
	   (generic js-arraybuffer-length ::JsArrayBuffer)
	   (generic js-arraybuffer-ref ::JsArrayBuffer ::int)
	   (generic js-arraybuffer-set! ::JsArrayBuffer ::int ::obj)
	   
	   (generic js-buffer->jsbuffer ::JsObject ::pair-nil ::JsGlobalObject)
	   
	   (generic js-typedarray-ref::procedure ::JsTypedArray)
	   (generic js-typedarray-set!::procedure ::JsTypedArray)
	   
	   *js-not-a-cmap*
	   *js-not-a-pmap*
	   *js-uncachable-pmap*
	   *js-not-a-string-cache*
	   (inline js-not-a-cmap::JsConstructMap)
	   (inline js-not-a-pmap::JsConstructMap)
	   (inline js-uncachable-pmap::JsConstructMap)
	   (inline js-not-a-index::long)
	   (inline js-not-a-string-cache::pair)
	   
	   (inline $object?::bool ::obj)
	   (inline js-object?::bool ::obj)
	   (inline js-jsobject?::bool ::obj)
	   (inline js-object-mapped?::bool ::JsObject)
	   (inline js-object-hashed?::bool ::JsObject)
	   
	   (inline js-number?::bool ::obj)
	   (inline js-jsstring?::bool ::obj)
	   (inline js-object-jsstring?::bool ::obj)
	   (inline js-jsstring-ascii?::bool ::JsStringLiteral)
	   (inline js-jsstring-index?::bool ::JsStringLiteral)
	   (inline js-jsstring-substring?::bool  ::JsStringLiteral)
	   (inline js-jsstring-buffer?::bool  ::JsStringLiteral)
	   (inline js-jsstring-private?::bool  ::JsStringLiteral)
	   (inline js-jsstring-utf8?::bool ::JsStringLiteral)
	   (inline js-jsstring-normalized?::bool ::JsStringLiteral)
	   (inline js-jsstring-normalized! ::JsStringLiteral)
	   (inline js-array?::bool ::obj)
	   (inline js-vector?::bool ::obj)
	   (inline js-function?::bool ::obj)
	   (inline js-method?::bool ::obj)
	   (inline js-procedure-proxy?::bool ::obj)
	   (inline js-procedure?::bool ::obj)
	   (inline js-object-procedure?::bool ::obj)
	   (inline js-callable?::bool ::obj)
	   (inline js-service?::bool ::obj)
	   (inline js-symbol?::bool ::obj)
	   (inline js-date?::bool ::obj)
	   (inline js-proxy?::bool ::obj)
	   (js-proxy-array?::bool ::obj)
	   (inline js-proxy-function?::bool ::obj)
	   (inline js-map?::bool ::obj)
	   (inline js-weakmap?::bool ::obj)
	   (inline js-set?::bool ::obj)
	   (inline js-weakset?::bool ::obj)
	   
	   (inline js-object-cmap ::JsObject)
	   (inline js-object-cmap-set! ::JsObject ::JsConstructMap)

	   (inline js-object-proto ::JsObject)
	   (inline js-object-proto-set! ::JsObject ::obj)
	   
	   (inline js-object-mode::uint32 ::object)
	   (inline js-object-mode-set! ::object ::uint32)

	   (inline js-object-procedure-tag?::bool ::obj)
	   (inline js-object-function-tag?::bool ::obj)
	   
	   (inline js-procedure-arity::int ::JsProcedure)
	   (inline js-procedure-procedure::procedure ::JsProcedure)
	   
	   (inline js-proxy-target::JsObject ::JsProxy)
	   (inline js-proxy-target-set! ::JsProxy ::JsObject)
	   
	   (inline jsindex12-max::uint32)
	   
	   (gencmapid::uint32)

	   (inline js-generator-inline-ref ::JsGenerator ::long)
	   (inline js-generator-inline-set! ::JsGenerator ::long ::obj))
   
   (pragma (js-not-a-cmap side-effect-free)
	   (js-null side-effect-free)
	   (js-undefined side-effect-free)
	   (js-object-default-mode side-effect-free)
	   (js-globalobject-default-mode side-effect-free)
	   (js-proxy-default-mode side-effect-free)
	   (js-array-default-mode side-effect-free)
	   (js-function-default-mode side-effect-free)
	   (js-procedure-default-mode side-effect-free)
	   (js-record-default-mode side-effect-free))
   
   (cond-expand
      ((not |bigloo4.3a|)
       (pragma (gencmapid default-inline)))))

;*---------------------------------------------------------------------*/
;*    js-worker-init! ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (js-worker-init! o::WorkerHopThread))

;*---------------------------------------------------------------------*/
;*    cache-mutex ...                                                  */
;*---------------------------------------------------------------------*/
(define cache-mutex (make-spinlock "cache-mutex"))

;*---------------------------------------------------------------------*/
;*    js-property-cache-init! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-property-cache-init! o)
   (js-profile-register-pcache o)
   o)

;*---------------------------------------------------------------------*/
;*    js-make-jsobject ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-make-jsobject constrsize constrmap __proto__)
   (let ((mode (js-object-default-mode)))
      (cond-expand
	 ((and bigloo-c (not disable-inline))
	  ($js-make-jsobject constrsize constrmap __proto__ mode))
	 (else
	  (let ((o (instantiate::JsObject
		      (cmap constrmap)
		      (elements (make-vector constrsize (js-undefined))))))
	     (js-object-proto-set! o __proto__)
	     (js-object-mode-set! o
		(bit-andu32 mode (bit-notu32 (JS-OBJECT-MODE-INLINE))))
	     o)))))

;*---------------------------------------------------------------------*/
;*    js-make-jsrecord ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-make-jsrecord constrsize constrmap __proto__ clazz)
   (let ((mode (js-record-default-mode)))
      (let ((o ($js-make-jsobject constrsize constrmap __proto__ mode)))
	 ($bgl-object-class-set! o ($bgl-class-num clazz))
	 (js-object-mode-set! o mode)
	 o)))
	   
;*---------------------------------------------------------------------*/
;*    js-make-jsconstructmap ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-make-jsconstructmap::JsConstructMap
		  #!key (%id (gencmapid))
		  single ctor
		  (methods '#()) (props '#())
		  (mptable '#()) (mrtable '#()) (mntable '#()))
   (instantiate::JsConstructMap
      (%id %id)
      (single single)
      (methods methods)
      (props props)
      (ctor ctor)
      (mptable mptable)
      (mrtable mrtable)
      (mntable mntable)))

;*---------------------------------------------------------------------*/
;*    js-jsconstructmap-size ...                                       */
;*---------------------------------------------------------------------*/
(define (js-jsconstructmap-size::long this::JsConstructMap)
   (with-access::JsConstructMap this (props)
      (vector-length props)))

;*---------------------------------------------------------------------*/
;*    js-object-default-mode ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-object-default-mode)
   (bit-oru32 (JS-OBJECT-MODE-EXTENSIBLE)
      (bit-oru32 (JS-OBJECT-MODE-PLAIN)
	 (bit-oru32 (JS-OBJECT-MODE-INLINE)
	    (bit-oru32 (JS-OBJECT-MODE-JSOBJECTTAG)
	       (bit-oru32 (JS-OBJECT-MODE-ENUMERABLE)
		  (JS-OBJECT-MODE-HASNUMERALPROP)))))))

(define-inline (js-globalobject-default-mode)
   (bit-andu32 (js-object-default-mode)
      (bit-notu32 (JS-OBJECT-MODE-INLINE))))

(define-inline (js-proxy-default-mode)
   (bit-andu32 (js-object-default-mode)
      (bit-notu32 (JS-OBJECT-MODE-INLINE))))

(define-inline (js-record-default-mode)
   (bit-oru32 (JS-OBJECT-MODE-SEALED)
      (bit-oru32 (JS-OBJECT-MODE-PLAIN)
	 (bit-oru32 (JS-OBJECT-MODE-INLINE)
	    (bit-oru32 (JS-OBJECT-MODE-JSOBJECTTAG)
	       (JS-OBJECT-MODE-ENUMERABLE))))))

(define-inline (js-array-default-mode)
   (bit-oru32 (bit-andu32 (js-object-default-mode)
		 (bit-notu32 (JS-OBJECT-MODE-INLINE)))
      (bit-oru32 (JS-OBJECT-MODE-JSARRAYINLINE)
	 (bit-oru32 (JS-OBJECT-MODE-JSARRAYHOLEY)
	    (bit-oru32 (JS-OBJECT-MODE-JSOBJECTTAG)
	       (bit-oru32 (JS-OBJECT-MODE-JSARRAYTAG)
		  (bit-oru32 (JS-OBJECT-MODE-ENUMERABLE)
		     (JS-OBJECT-MODE-HASNUMERALPROP))))))))

(define-inline (js-vector-default-mode)
   (bit-oru32 (JS-OBJECT-MODE-JSVECTORTAG)
      (bit-andu32 (js-array-default-mode)
	 (bit-notu32
	    (bit-oru32 (JS-OBJECT-MODE-PLAIN)
	       (JS-OBJECT-MODE-EXTENSIBLE))))))

(define-inline (js-jsstring-default-mode)
   (JS-OBJECT-MODE-JSSTRINGTAG))

(define-inline (js-jsstring-default-ascii-mode)
   (bit-oru32 (JS-OBJECT-MODE-JSSTRINGTAG)
      (JS-OBJECT-MODE-JSSTRINGASCII)))

(define-inline (js-jsstring-normalized-ascii-mode)
   (bit-oru32 (js-jsstring-default-ascii-mode)
      (js-jsstring-normalized-mode)))

(define-inline (js-jsstring-default-index-mode)
   (bit-oru32 (JS-OBJECT-MODE-JSSTRINGTAG)
      (JS-OBJECT-MODE-JSSTRINGINDEX)))

(define-inline (js-jsstring-default-utf8-mode)
   (bit-oru32 (JS-OBJECT-MODE-JSSTRINGTAG)
      (JS-OBJECT-MODE-JSSTRINGUTF8)))

(define-inline (js-jsstring-default-substring-mode)
   (bit-oru32 (JS-OBJECT-MODE-JSSTRINGTAG)
      (bit-oru32 (JS-OBJECT-MODE-JSSTRINGASCII)
	 (JS-OBJECT-MODE-JSSTRINGSUBSTRING))))

(define-inline (js-jsstring-default-buffer-mode)
   (bit-oru32 (js-jsstring-default-substring-mode)
      (JS-OBJECT-MODE-JSSTRINGBUFFER)))

(define-inline (js-jsstring-normalized-utf8-mode)
   (bit-oru32 (js-jsstring-default-utf8-mode)
      (js-jsstring-normalized-mode)))

(define-inline (js-jsstring-normalized-private-mode)
   (bit-oru32 (JS-OBJECT-MODE-JSSTRINGTAG)
      (bit-oru32 (JS-OBJECT-MODE-JSSTRINGPRIVATE)
	 (JS-OBJECT-MODE-JSSTRINGNORMALIZED))))

(define-inline (js-jsstring-normalized-mode)
   (bit-oru32 (JS-OBJECT-MODE-JSSTRINGTAG)
      (JS-OBJECT-MODE-JSSTRINGNORMALIZED)))

(define-inline (js-jsstring-normalized-index-mode)
   (bit-oru32 (js-jsstring-default-index-mode)
      (js-jsstring-normalized-mode)))

(define-inline (js-jsstring-normalized-substring-mode)
   (bit-oru32 (js-jsstring-default-substring-mode)
      (js-jsstring-normalized-mode)))

(define-inline (js-function-default-mode)
   (bit-oru32 (JS-OBJECT-MODE-EXTENSIBLE)
      (bit-oru32 (JS-OBJECT-MODE-PLAIN)
	 (bit-oru32 (JS-OBJECT-MODE-JSOBJECTTAG)
	    (bit-oru32 (JS-OBJECT-MODE-JSFUNCTIONTAG)
	       (bit-oru32 (JS-OBJECT-MODE-JSPROCEDURETAG)
		  (bit-oru32 (JS-OBJECT-MODE-ENUMERABLE)
		     (JS-OBJECT-MODE-HASNUMERALPROP))))))))

(define-inline (js-procedure-default-mode)
   (bit-oru32 (JS-OBJECT-MODE-EXTENSIBLE)
      (bit-oru32 (JS-OBJECT-MODE-PLAIN)
	 (bit-oru32 (JS-OBJECT-MODE-JSOBJECTTAG)
	    (bit-oru32 (JS-OBJECT-MODE-JSPROCEDURETAG)
	       (bit-oru32 (JS-OBJECT-MODE-ENUMERABLE)
		  (JS-OBJECT-MODE-HASNUMERALPROP)))))))

(define-inline (js-procedure-hopscript-mode)
   (bit-oru32 (js-procedure-default-mode)
      (JS-OBJECT-MODE-JSPROCEDUREHOPSCRIPT)))

(define-inline (js-arraybuffer-default-mode)
   (bit-andu32 (js-object-default-mode)
      (bit-notu32 (JS-OBJECT-MODE-INLINE))))

(define-inline (js-typedarray-default-mode)
   (bit-andu32 (js-object-default-mode)
      (bit-notu32 (JS-OBJECT-MODE-INLINE))))

(define-inline (js-dataview-default-mode)
   (bit-andu32 (js-object-default-mode)
      (bit-notu32 (JS-OBJECT-MODE-INLINE))))

;*---------------------------------------------------------------------*/
;*    Object header tag (max size 1<<15==32768)                        */
;*---------------------------------------------------------------------*/
;;     1: JSSTRINGTAG (must be !)
;;     2: JSFUNCTIONTAG (must be !)
;;     4: JSOBJECTTAG (must be !)
;;     8: INLINE
;;    16: ISPROTOOF
;;    32: HASINSTANCE, JSARRAYINLINE
;;    64: ENUMERABLE
;;   128: PLAIN
;;   256: HSNUMERALPROP
;;   512: EXTENSIBLE
;;  1024: SEALED, PROXYREVOKED
;;  2048: FROZEN
;;  4096: JSPROXYFUNCTION, JSPROCEDUREHOPSCRIPT, JSVECTORTAG
;;  8192: JSPROCEDURETAG
;; 16384: JSARRAYTAG (must be !)
;; 32768: JSARRAYHOLEY (must be !)
(define-inline (JS-OBJECT-MODE-JSSTRINGTAG) #u32:1)
(define-inline (JS-OBJECT-MODE-JSFUNCTIONTAG) #u32:2)
(define-inline (JS-OBJECT-MODE-JSOBJECTTAG) #u32:4)
(define-inline (JS-OBJECT-MODE-JSPROCEDURETAG) #u32:8192)
(define-inline (JS-OBJECT-MODE-JSARRAYTAG) #u32:16384)
(define-inline (JS-OBJECT-MODE-JSVECTORTAG) #u32:4096)

(define-macro (JS-OBJECT-MODE-JSSTRINGTAG) #u32:1)
(define-macro (JS-OBJECT-MODE-JSFUNCTIONTAG) #u32:2)
(define-macro (JS-OBJECT-MODE-JSOBJECTTAG) #u32:4)
(define-macro (JS-OBJECT-MODE-JSPROCEDURETAG) #u32:8192)
(define-macro (JS-OBJECT-MODE-JSARRAYTAG) #u32:16384)
(define-macro (JS-OBJECT-MODE-JSVECTORTAG) #u32:4096)

;; common objects attributes
(define-inline (JS-OBJECT-MODE-INLINE) #u32:8)
;; used to mark objects used has __proto__ of others
(define-inline (JS-OBJECT-MODE-ISPROTOOF) #u32:16)
(define-inline (JS-OBJECT-MODE-HASINSTANCE) #u32:32)
(define-inline (JS-OBJECT-MODE-JSARRAYINLINE) #u32:32)
(define-inline (JS-OBJECT-MODE-ENUMERABLE) #u32:64)
(define-inline (JS-OBJECT-MODE-PLAIN) #u32:128)
(define-inline (JS-OBJECT-MODE-HASNUMERALPROP) #u32:256)
(define-inline (JS-OBJECT-MODE-EXTENSIBLE) #u32:512)
(define-inline (JS-OBJECT-MODE-SEALED) #u32:1024)
(define-inline (JS-OBJECT-MODE-FROZEN) #u32:2048)

(define-macro (JS-OBJECT-MODE-INLINE) #u32:8)
(define-macro (JS-OBJECT-MODE-ISPROTOOF) #u32:16)
(define-macro (JS-OBJECT-MODE-HASINSTANCE) #u32:32)
(define-macro (JS-OBJECT-MODE-JSARRAYINLINE) #u32:32)
(define-macro (JS-OBJECT-MODE-ENUMERABLE) #u32:64)
(define-macro (JS-OBJECT-MODE-PLAIN) #u32:128)
(define-macro (JS-OBJECT-MODE-HASNUMERALPROP) #u32:256)
(define-macro (JS-OBJECT-MODE-EXTENSIBLE) #u32:512)
(define-macro (JS-OBJECT-MODE-SEALED) #u32:1024)
(define-macro (JS-OBJECT-MODE-FROZEN) #u32:2048)

;; per type attributes
(define-inline (JS-OBJECT-MODE-JSPROXYREVOKED) #u32:1024)
;; JSPROXYFUNCTION but not be included in js-object-default-mode
(define-inline (JS-OBJECT-MODE-JSPROXYFUNCTION) #u32:4096)
(define-inline (JS-OBJECT-MODE-JSPROCEDUREHOPSCRIPT) #u32:4096)
;; WARNING: must be the two last constants (see js-array?)
(define-inline (JS-OBJECT-MODE-JSARRAYHOLEY) #u32:32768)

(define-macro (JS-OBJECT-MODE-JSPROXYREVOKED) #u32:1024)
(define-macro (JS-OBJECT-MODE-JSPROXYFUNCTION) #u32:4096)
(define-macro (JS-OBJECT-MODE-JSPROCEDUREHOPSCRIPT) #u32:4096)
(define-macro (JS-OBJECT-MODE-JSARRAYHOLEY) #u32:32768)

;; string types and attributed
(define-inline (JS-OBJECT-MODE-JSSTRINGASCII) #u32:8)
(define-inline (JS-OBJECT-MODE-JSSTRINGUTF8) #u32:16)
(define-inline (JS-OBJECT-MODE-JSSTRINGNORMALIZED) #u32:32)
(define-inline (JS-OBJECT-MODE-JSSTRINGINDEX) #u32:64)
(define-inline (JS-OBJECT-MODE-JSSTRINGCACHE) #u32:128)
(define-inline (JS-OBJECT-MODE-JSSTRINGSUBSTRING) #u32:256)
(define-inline (JS-OBJECT-MODE-JSSTRINGBUFFER) #u32:512)
(define-inline (JS-OBJECT-MODE-JSSTRINGPRIVATE) #u32:1024)

(define-macro (JS-OBJECT-MODE-JSSTRINGASCII) #u32:8)
(define-macro (JS-OBJECT-MODE-JSSTRINGUTF8) #u32:16)
(define-macro (JS-OBJECT-MODE-JSSTRINGNORMALIZED) #u32:32)
(define-macro (JS-OBJECT-MODE-JSSTRINGINDEX) #u32:64)
(define-macro (JS-OBJECT-MODE-JSSTRINGCACHE) #u32:128)
(define-macro (JS-OBJECT-MODE-JSSTRINGSUBSTRING) #u32:256)
(define-macro (JS-OBJECT-MODE-JSSTRINGPRIVATE) #u32:1024)

(define-inline (js-object-mode-extensible? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-EXTENSIBLE) (js-object-mode o))
      (JS-OBJECT-MODE-EXTENSIBLE)))

(define-inline (js-object-mode-plain-extensible? o)
   (=u32 (bit-andu32
	    (bit-oru32 (JS-OBJECT-MODE-PLAIN) (JS-OBJECT-MODE-EXTENSIBLE))
	    (js-object-mode o))
      (bit-oru32 (JS-OBJECT-MODE-PLAIN) (JS-OBJECT-MODE-EXTENSIBLE))))

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
   (cond-expand
      (bigloo-c
       ($js-object-mode-inline? o (JS-OBJECT-MODE-INLINE)))
      (else
       (=u32 (bit-andu32 (JS-OBJECT-MODE-INLINE) (js-object-mode o))
	  (JS-OBJECT-MODE-INLINE)))))

(define-inline (js-object-mode-inline-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-INLINE))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-INLINE))))))

(define-inline (js-object-mode-isprotoof? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-ISPROTOOF) (js-object-mode o))
      (JS-OBJECT-MODE-ISPROTOOF)))

(define-inline (js-object-mode-isprotoof-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-ISPROTOOF))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-ISPROTOOF))))))

(define-inline (js-object-mode-hasinstance? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-HASINSTANCE) (js-object-mode o))
      (JS-OBJECT-MODE-HASINSTANCE)))

(define-inline (js-object-mode-hasinstance-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-HASINSTANCE))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-HASINSTANCE))))))

(define-inline (js-object-mode-arrayinline? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-JSARRAYINLINE) (js-object-mode o))
      (JS-OBJECT-MODE-JSARRAYINLINE)))

(define-inline (js-object-mode-arrayinline-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-JSARRAYINLINE))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-JSARRAYINLINE))))))

(define-inline (js-object-mode-arrayholey? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-JSARRAYHOLEY) (js-object-mode o))
      (JS-OBJECT-MODE-JSARRAYHOLEY)))

(define-inline (js-object-mode-arrayholey-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-JSARRAYHOLEY))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-JSARRAYHOLEY))))))

(define-inline (js-object-mode-plain? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-PLAIN) (js-object-mode o))
      (JS-OBJECT-MODE-PLAIN)))

(define-inline (js-object-mode-plain-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-PLAIN))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-PLAIN))))))

(define-inline (js-object-mode-enumerable? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-ENUMERABLE) (js-object-mode o))
      (JS-OBJECT-MODE-ENUMERABLE)))

(define-inline (js-object-mode-enumerable-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-ENUMERABLE))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-ENUMERABLE))))))

(define-inline (js-object-mode-hasnumeralprop? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-HASNUMERALPROP) (js-object-mode o))
      (JS-OBJECT-MODE-HASNUMERALPROP)))

(define-inline (js-object-mode-hasnumeralprop-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-HASNUMERALPROP))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-HASNUMERALPROP))))))

(define-inline (js-proxy-mode-revoked? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-JSPROXYREVOKED) (js-object-mode o))
      (JS-OBJECT-MODE-JSPROXYREVOKED)))

(define-inline (js-proxy-mode-revoked-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-JSPROXYREVOKED))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-JSPROXYREVOKED))))))

(define-inline (js-proxy-mode-function? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-JSPROXYFUNCTION) (js-object-mode o))
      (JS-OBJECT-MODE-JSPROXYFUNCTION)))

(define-inline (js-proxy-mode-function-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-JSPROXYFUNCTION))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-JSPROXYFUNCTION))))))

(define-inline (js-procedure-hopscript-mode? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-JSPROCEDUREHOPSCRIPT) (js-object-mode o))
      (JS-OBJECT-MODE-JSPROCEDUREHOPSCRIPT)))

(define-inline (js-procedure-hopscript-mode-set! o flag)
   (js-object-mode-set! o
      (if flag
	  (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-JSPROCEDUREHOPSCRIPT))
	  (bit-andu32 (js-object-mode o) (bit-notu32 (JS-OBJECT-MODE-JSPROCEDUREHOPSCRIPT))))))

;*---------------------------------------------------------------------*/
;*    regexp                                                           */
;*---------------------------------------------------------------------*/
(define-inline (JS-REGEXP-FLAG-IGNORECASE) #u32:1)
(define-inline (JS-REGEXP-FLAG-MULTILINE) #u32:2)
(define-inline (JS-REGEXP-FLAG-GLOBAL) #u32:4)
(define-inline (JS-REGEXP-FLAG-UNICODE) #u32:8)
(define-inline (JS-REGEXP-FLAG-INDICES) #u32:16)
(define-inline (JS-REGEXP-FLAG-DOTALL) #u32:32)
(define-inline (JS-REGEXP-FLAG-UNICODESETS) #u32:64)
(define-inline (JS-REGEXP-FLAG-STICKY) #u32:128)

;*---------------------------------------------------------------------*/
;*    js-regexp-flags-XXX ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-regexp-flags-ignorecase? flags)
   (>u32 (bit-andu32 flags (JS-REGEXP-FLAG-IGNORECASE)) #u32:0))
(define-inline (js-regexp-flags-multiline? flags)
   (>u32 (bit-andu32 flags (JS-REGEXP-FLAG-MULTILINE)) #u32:0))
(define-inline (js-regexp-flags-global? flags)
   (>u32 (bit-andu32 flags (JS-REGEXP-FLAG-GLOBAL)) #u32:0))
(define-inline (js-regexp-flags-unicode? flags)
   (>u32 (bit-andu32 flags (JS-REGEXP-FLAG-UNICODE)) #u32:0))
(define-inline (js-regexp-flags-indices? flags)
   (>u32 (bit-andu32 flags (JS-REGEXP-FLAG-INDICES)) #u32:0))
(define-inline (js-regexp-flags-dotall? flags)
   (>u32 (bit-andu32 flags (JS-REGEXP-FLAG-DOTALL)) #u32:0))
(define-inline (js-regexp-flags-unicodesets? flags)
   (>u32 (bit-andu32 flags (JS-REGEXP-FLAG-UNICODESETS)) #u32:0))
(define-inline (js-regexp-flags-sticky? flags)
   (>u32 (bit-andu32 flags (JS-REGEXP-FLAG-STICKY)) #u32:0))

;*---------------------------------------------------------------------*/
;*    js-object-inline-elements ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-object-inline-elements o::JsObject)
   (cond-expand
      ((and bigloo-c (not disable-inline))
       (if (js-object-mode-inline? o)
	   ($js-object-inline-elements o)
	   '#()))
      (else
       '#())))

;*---------------------------------------------------------------------*/
;*    js-object-inline-elements-length ...                             */
;*---------------------------------------------------------------------*/
(define-inline (js-object-inline-elements-length o::JsObject)
   (cond-expand
      (bigloo-c
       ($js-object-inline-elements-length o))
      (else
       (vector-length (js-object-inline-elements o)))))

;*---------------------------------------------------------------------*/
;*    js-object-noinline-elements ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-object-noinline-elements o::JsObject)
   (with-access::JsObject o (elements)
      elements))

;*---------------------------------------------------------------------*/
;*    js-object-alloc-elements ...                                     */
;*    -------------------------------------------------------------    */
;*    Only to be used to instantiateJsObjectXXX                        */
;*---------------------------------------------------------------------*/
(define-inline (js-object-alloc-elements o::JsObject)
   (cond-expand
      ((and bigloo-c (not disable-inline))
       ($js-object-inline-elements o))
      (else
       (js-object-noinline-elements o))))

;*---------------------------------------------------------------------*/
;*    js-object-inline-length ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-object-inline-length o::JsObject)
   (if (js-object-mode-inline? o)
       (vector-length (js-object-inline-elements o))
       0))

;*---------------------------------------------------------------------*/
;*    js-object-noinline-length ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-object-noinline-length o::JsObject)
   (vector-length (js-object-noinline-elements o)))

;*---------------------------------------------------------------------*/
;*    js-object-length ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-object-length o::JsObject)
   (+fx (vector-length (js-object-noinline-elements o))
      (if (js-object? o)
	  (vector-length (js-object-inline-elements o))
	  0)))

;*---------------------------------------------------------------------*/
;*    js-object-inline-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-object-inline-ref o::JsObject idx::long)
   (cond-expand
      ((and bigloo-c (not disable-inline) debug)
       (if (<fx idx (vector-length (js-object-inline-elements o)))
	   ;; (pragma::obj "VECTOR_REF( BVECTOR( (obj_t)(( ((obj_t *)(&(((BgL_jsobjectz00_bglt)(COBJECT($1)))->BgL_elementsz00))) + 1))), $2 )" o idx)
	   ($js-object-inline-elements-ref o idx)
	   (begin
	      (tprint "*** ASSERT ERROR:js-object-inline-ref idx=" idx)
	      (js-inspect-object o)
	      ($$hopscript-breakpoint "js-object-inline-ref"))))
      ((and bigloo-c (not disable-inline))
       ;; (pragma::obj "VECTOR_REF( BVECTOR( (obj_t)(( ((obj_t *)(&(((BgL_jsobjectz00_bglt)(COBJECT($1)))->BgL_elementsz00))) + 1))), $2 )" o idx)
       ($js-object-inline-elements-ref o idx))
      (else
       (js-object-noinline-ref o idx))))

;*---------------------------------------------------------------------*/
;*    js-object-inline-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-object-inline-set! o::JsObject idx::long val::obj)
   (cond-expand
      ((and bigloo-c debug (not disable-inline))
       (if (<fx idx (vector-length (js-object-inline-elements o)))
	   ;; (pragma::obj "VECTOR_SET( BVECTOR( (obj_t)(( ((obj_t *)(&(((BgL_jsobjectz00_bglt)(COBJECT($1)))->BgL_elementsz00))) + 1))), $2, $3 )" o idx val)
	   ($js-object-inline-elements-set! o idx val)
	   (begin
	      (tprint "*** ASSERT ERROR:js-object-inline-set! idx=" idx)
	      (js-inspect-object o)
	      ($$hopscript-breakpoint "js-object-inline-set!"))))
      ((and bigloo-c (not disable-inline))
       ;; (pragma::obj "VECTOR_SET( BVECTOR( (obj_t)(( ((obj_t *)(&(((BgL_jsobjectz00_bglt)(COBJECT($1)))->BgL_elementsz00))) + 1))), $2, $3 )" o idx val)
       ($js-object-inline-elements-set! o idx val))
      (else
       (js-object-noinline-set! o idx val))))

;*---------------------------------------------------------------------*/
;*    js-object-noinline-relative-ref ...                              */
;*---------------------------------------------------------------------*/
(define-inline (js-object-noinline-relative-ref o::JsObject idx::long)
   (with-access::JsObject o (elements)
      (vector-ref elements idx)))

;*---------------------------------------------------------------------*/
;*    js-object-noinline-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-object-noinline-ref o::JsObject idx::long)
   (with-access::JsObject o (elements)
      (js-object-noinline-relative-ref o
	 (-fx idx (vector-length (js-object-inline-elements o))))))

;*---------------------------------------------------------------------*/
;*    js-object-noinline-relative-set! ...                             */
;*---------------------------------------------------------------------*/
(define-inline (js-object-noinline-relative-set! o::JsObject idx::long val::obj)
   (with-access::JsObject o (elements)
      (vector-set! elements idx val)))

;*---------------------------------------------------------------------*/
;*    js-object-noinline-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-object-noinline-set! o::JsObject idx::long val::obj)
   (with-access::JsObject o (elements)
      (js-object-noinline-relative-set! o
	 (-fx idx (vector-length (js-object-inline-elements o))) val)))

;*---------------------------------------------------------------------*/
;*    js-object-ref ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-object-ref o::JsObject idx::long)
   (if (js-object-mode-inline? o)
       (let ((ilen (js-object-inline-elements-length o)))
	  (if (<fx idx ilen)
	      (js-object-inline-ref o idx)
	      (js-object-noinline-relative-ref o (-fx idx ilen))))
       (js-object-noinline-relative-ref o idx)))

;*---------------------------------------------------------------------*/
;*    js-object-set! ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-object-set! o::JsObject idx::long val)
   (if (js-object-mode-inline? o)
       (let ((ilen (js-object-inline-elements-length o)))
	  (if (<fx idx ilen)
	      (js-object-inline-set! o idx val)
	      (js-object-noinline-relative-set! o (-fx idx ilen) val)))
       (js-object-noinline-relative-set! o idx val)))

;*---------------------------------------------------------------------*/
;*    js-object-for-each ...                                           */
;*---------------------------------------------------------------------*/
(define (js-object-for-each o::JsObject proc)
   (when (js-object-mode-inline? o)
      (let ((ilen (js-object-inline-elements-length o)))
	 (let loop ((i 0))
	    (when (<fx i ilen)
	       (proc (js-object-inline-ref o i))
	       (loop (+fx i 1))))))
   (with-access::JsObject o (elements)
      (vector-for-each proc elements)))
   
;*---------------------------------------------------------------------*/
;*    js-vector-inline-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-vector-inline-ref o::JsArray idx::long)
   (cond-expand
      ((and bigloo-c (not disable-inline))
       (pragma::obj "VECTOR_REF( BVECTOR( (obj_t)(( ((obj_t *)(&(((BgL_jsarrayz00_bglt)(COBJECT($1)))->BgL_vecz00))) + 1))), $2 )" o idx))
      (else
       (with-access::JsArray o (vec)
	  (vector-ref vec idx)))))

;*---------------------------------------------------------------------*/
;*    js-vector-inline-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-vector-inline-set! o::JsArray idx::long val)
   (cond-expand
      ((and bigloo-c (not disable-inline))
       (pragma::obj "VECTOR_SET( BVECTOR( (obj_t)(( ((obj_t *)(&(((BgL_jsarrayz00_bglt)(COBJECT($1)))->BgL_vecz00))) + 1))), $2, $3 )" o idx val))
      (else
       (with-access::JsArray o (vec)
	  (vector-set! vec idx val)))))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsWrapper ...                              */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value o::JsWrapper ctx)
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
;*    object-print ::JsObject ...                                      */
;*---------------------------------------------------------------------*/
(define-method (object-print obj::JsObject op proc)
   (display "#<" op)
   (display (class-name (object-class obj)) op)
   (display " " op)
   (display (js-object-length obj) op)
   (cond
      ((js-object-mapped? obj) (display " mapped>" op))
      ((js-object-hashed? obj) (display " hashed>" op))
      (else (display ">" op))))

;* {*---------------------------------------------------------------------*} */
;* {*    object-print ...                                                 *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (object-print obj::JsObject port print-slot::procedure) */
;*                                                                     */
;*    (define (class-field-write/display field)                        */
;*       (let* ((name (class-field-name field))                        */
;* 	     (get-value (class-field-accessor field)))                 */
;* 	 (display " [" port)                                           */
;* 	 (display name port)                                           */
;* 	 (display #\: port)                                            */
;* 	 (display #\space port)                                        */
;* 	 (if (memq name '(__proto__ elements cmap))                    */
;* 	     (let ((val (get-value obj)))                              */
;* 		(if (vector? val)                                      */
;* 		    (display (format "vector[~a]" (vector-length val)) port) */
;* 		    (display (typeof val) port)))                      */
;* 	     (print-slot (get-value obj) port))                        */
;* 	 (display #\] port)))                                          */
;*                                                                     */
;*    (let* ((class (object-class obj))                                */
;* 	  (class-name (class-name class))                              */
;* 	  (fields (class-all-fields class))                            */
;* 	  (len (vector-length fields)))                                */
;*       (display "#|" port)                                           */
;*       (display class-name port)                                     */
;*       (if (nil? obj)                                                */
;* 	  (display " nil|" port)                                       */
;* 	  (let loop ((i 0))                                            */
;* 	     (if (=fx i len)                                           */
;* 		 (display #\| port)                                    */
;* 		 (begin                                                */
;* 		    (class-field-write/display (vector-ref fields i))  */
;* 		    (loop (+fx i 1))))))))                             */

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
;*    object-print ::JsAccessorDescriptor ...                          */
;*---------------------------------------------------------------------*/
(define-method (object-print p::JsAccessorDescriptor port pslot::procedure)
   (with-access::JsAccessorDescriptor p (name configurable enumerable get set)
      (fprintf port
	 "#|~s name=~a configurable=~a enumerable=~a get=~a set=~a|"
	 (class-name (object-class p))
	 name
	 configurable
	 enumerable
	 (typeof get)
	 (typeof set))))

;*---------------------------------------------------------------------*/
;*    object-print ::JsWrapperDescriptor ...                           */
;*---------------------------------------------------------------------*/
(define-method (object-print p::JsWrapperDescriptor port pslot::procedure)
   (with-access::JsWrapperDescriptor p (name configurable enumerable writable)
      (fprintf port
	 "#|~s name=~a configurable=~a enumerable=~a writable=~a|"
	 (class-name (object-class p))
	 name
	 configurable
	 enumerable
	 writable)))

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
	 (if (not (js-object? value)) value (typeof value)))))

;*---------------------------------------------------------------------*/
;*    object-print ::JsDataDescriptor ...                              */
;*---------------------------------------------------------------------*/
(define-method (object-print p::JsDataDescriptor port pslot::procedure)
   (with-access::JsValueDescriptor p (name configurable enumerable writable value)
      (fprintf port
	 "#|~s name=~a configurable=~a enumerable=~a writable=~a|"
	 (class-name (object-class p))
	 name
	 configurable
	 enumerable
	 writable)))

;*---------------------------------------------------------------------*/
;*    object-equal? ::JsPropertyDescriptor ...                         */
;*---------------------------------------------------------------------*/
(define-method (object-equal?::bool x::JsPropertyDescriptor y::obj)
   (when (and (object? y) (eq? (object-class x) (object-class y)))
      (with-access::JsPropertyDescriptor x ((xname name)
					    (xconfigurable configurable)
					    (xenumerable enumerable))
	 (with-access::JsPropertyDescriptor y ((yname name)
					       (yconfigurable configurable)
					       (yenumerable enumerable))
	    (and (eq? xconfigurable yconfigurable)
		 (eq? xenumerable yenumerable)
		 (equal? xname yname))))))

;*---------------------------------------------------------------------*/
;*    object-equal? ::JsDataDescriptor ...                             */
;*---------------------------------------------------------------------*/
(define-method (object-equal?::bool x::JsDataDescriptor y::obj)
   (when (call-next-method)
      (with-access::JsDataDescriptor x ((xwritable writable))
	 (with-access::JsDataDescriptor y ((ywritable writable))
	    (eq? xwritable ywritable)))))

;*---------------------------------------------------------------------*/
;*    object-equal? ::JsValueDescriptor ...                            */
;*---------------------------------------------------------------------*/
(define-method (object-equal?::bool x::JsValueDescriptor y::obj)
   (when (call-next-method)
      (with-access::JsValueDescriptor x ((xvalue value))
	 (with-access::JsValueDescriptor y ((yvalue value))
	    (equal? xvalue yvalue)))))

;*---------------------------------------------------------------------*/
;*    object-equal? ::JsAccessorDescriptor ...                         */
;*---------------------------------------------------------------------*/
(define-method (object-equal?::bool x::JsAccessorDescriptor y::obj)
   (when (call-next-method)
      (with-access::JsAccessorDescriptor x ((xget get)
					    (xset set))
	 (with-access::JsAccessorDescriptor y ((yget get)
					       (yset set))
	    (and (eq? xget yget) (eq? xset yset))))))

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
   (with-access::JsObject obj (cmap elements)
      (let ((nobj (duplicate::JsGlobalObject obj
		     (cmap (js-clone cmap))
		     (elements (vector-map js-clone elements)))))
	 (js-object-proto-set! nobj (js-clone (js-object-proto obj)))
	 (js-object-mode-set! nobj (js-object-mode obj))
	 nobj)))

;*---------------------------------------------------------------------*/
;*    js-clone ::JsConstructMap ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-clone obj::JsConstructMap)
   (if (eq? obj (js-not-a-cmap))
       obj
       (with-access::JsConstructMap obj (props)
	  (duplicate::JsConstructMap obj
	     (%id (gencmapid))
	     (props (vector-copy props))))))

;*---------------------------------------------------------------------*/
;*    js-clone ::JsValueDescriptor ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-clone obj::JsValueDescriptor)
   (with-access::JsValueDescriptor obj (writable)
      (if writable
	  (duplicate::JsValueDescriptor obj)
	  obj)))

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
      ((js-absent? obj)
       obj)
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
;*    *js-not-a-vtable* ...                                            */
;*---------------------------------------------------------------------*/
(define *js-not-a-vtable*
   (make-vector 10))

;*---------------------------------------------------------------------*/
;*    *js-not-a-cmap* ...                                              */
;*---------------------------------------------------------------------*/
(define *js-not-a-cmap*
   (js-make-jsconstructmap :%id 0))

;*---------------------------------------------------------------------*/
;*    *js-not-a-pmap* ...                                              */
;*---------------------------------------------------------------------*/
(define *js-not-a-pmap*
   (js-make-jsconstructmap :%id -1))

;*---------------------------------------------------------------------*/
;*    *js-uncachable-pmap* ...                                         */
;*---------------------------------------------------------------------*/
(define *js-uncachable-pmap*
   (js-make-jsconstructmap :%id -1))

;*---------------------------------------------------------------------*/
;*    *js-not-a-string-cache* ...                                      */
;*---------------------------------------------------------------------*/
(define *js-not-a-string-cache*
   (cons #f #f))

;*---------------------------------------------------------------------*/
;*    js-not-a-cmap ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-not-a-cmap::JsConstructMap)
   *js-not-a-cmap*)

;*---------------------------------------------------------------------*/
;*    js-not-a-pmap ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-not-a-pmap::JsConstructMap)
   *js-not-a-pmap*)

;*---------------------------------------------------------------------*/
;*    js-uncachable-pmap ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-uncachable-pmap::JsConstructMap)
   *js-uncachable-pmap*)

;*---------------------------------------------------------------------*/
;*    js-not-a-index ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-not-a-index::long)
   (bit-lsh 1 28))

;*---------------------------------------------------------------------*/
;*    js-not-a-string-cache ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-not-a-string-cache::pair)
   *js-not-a-string-cache*)

;*---------------------------------------------------------------------*/
;*    js-not-a-vtable ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-not-a-vtable::vector)
   *js-not-a-vtable*)

;*---------------------------------------------------------------------*/
;*    $object? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline ($object? o)
   (cond-expand
      ((and bigloo-c bigloo-unsafe) ($pointer? o))
      (else (%object? o))))

;*---------------------------------------------------------------------*/
;*    js-object? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-object? o)
   (cond-expand
      (bigloo-c
       ($js-object? o (JS-OBJECT-MODE-JSOBJECTTAG)))
      (else
       (and ($object? o)
	    (=u32 (JS-OBJECT-MODE-JSOBJECTTAG)
	       (bit-andu32 (js-object-mode o) (JS-OBJECT-MODE-JSOBJECTTAG)))))))

;*---------------------------------------------------------------------*/
;*    js-jsobject? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-jsobject? o)
   (and (%object? o) (eq? (object-class o) JsObject)))

;*---------------------------------------------------------------------*/
;*    js-object-mapped? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-object-mapped? o::JsObject)
   (with-access::JsObject o (cmap)
      (not (eq? cmap (js-not-a-cmap)))))

;*---------------------------------------------------------------------*/
;*    js-object-hashed? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-object-hashed? o::JsObject)
   (with-access::JsObject o (cmap elements)
      (and (eq? cmap (js-not-a-cmap)) (not (vector? elements)))))

;*---------------------------------------------------------------------*/
;*    js-number? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-number? o)
   (or (fixnum? o) (flonum? o)))

;*---------------------------------------------------------------------*/
;*    js-jsstring? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring? o)
   (cond-expand
      (bigloo-c
       ($js-jsstring? o (JS-OBJECT-MODE-JSSTRINGTAG)))
      (else
       (and ($object? o)
	    (=u32 (JS-OBJECT-MODE-JSSTRINGTAG)
	       (bit-andu32 (js-object-mode o) (JS-OBJECT-MODE-JSSTRINGTAG)))))))

;*---------------------------------------------------------------------*/
;*    js-object-jsstring? ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-object-jsstring? o)
   (cond-expand
      (bigloo-c
       ($js-object-jsstring? o (JS-OBJECT-MODE-JSSTRINGTAG)))
      (else
       (=u32 (JS-OBJECT-MODE-JSSTRINGTAG)
	  (bit-andu32 (js-object-mode o) (JS-OBJECT-MODE-JSSTRINGTAG))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-ascii? ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-ascii? o)
   (not (=u32 (bit-andu32
		 (bit-oru32 (JS-OBJECT-MODE-JSSTRINGASCII)
		    (JS-OBJECT-MODE-JSSTRINGINDEX))
		 (js-object-mode o))
	   #u32:0)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-index? ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-index? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-JSSTRINGINDEX) (js-object-mode o))
      (JS-OBJECT-MODE-JSSTRINGINDEX)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-substring? ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-substring? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-JSSTRINGSUBSTRING) (js-object-mode o))
      (JS-OBJECT-MODE-JSSTRINGSUBSTRING)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-buffer? ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-buffer? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-JSSTRINGBUFFER) (js-object-mode o))
      (JS-OBJECT-MODE-JSSTRINGBUFFER)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-private? ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-private? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-JSSTRINGPRIVATE) (js-object-mode o))
      (JS-OBJECT-MODE-JSSTRINGPRIVATE)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-utf8? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-utf8? o)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-JSSTRINGUTF8) (js-object-mode o))
      (JS-OBJECT-MODE-JSSTRINGUTF8)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalized? ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-normalized?::bool o::JsStringLiteral)
   (=u32 (bit-andu32 (JS-OBJECT-MODE-JSSTRINGNORMALIZED) (js-object-mode o))
      (JS-OBJECT-MODE-JSSTRINGNORMALIZED)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalized! ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-normalized! o::JsStringLiteral)
   (js-object-mode-set! o
      (bit-oru32 (js-object-mode o) (JS-OBJECT-MODE-JSSTRINGNORMALIZED))))

;*---------------------------------------------------------------------*/
;*    js-array? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-array? o)
   (cond-expand
      (bigloo-c
       ($js-array? o (JS-OBJECT-MODE-JSARRAYTAG)))
      (else
       ;; assumes that all bits > JSARRAYTAG are about arrays
       (and ($object? o)
	    (>=u32 (js-object-mode o) (JS-OBJECT-MODE-JSARRAYTAG))))))

;*---------------------------------------------------------------------*/
;*    js-vector? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-vector? o)
   (and (%object? o)
	(=u32 (bit-andu32 (js-object-mode o)
		 (bit-oru32 (JS-OBJECT-MODE-JSARRAYTAG)
		    (JS-OBJECT-MODE-JSVECTORTAG)))
	   (bit-oru32 (JS-OBJECT-MODE-JSARRAYTAG)
	      (JS-OBJECT-MODE-JSVECTORTAG)))))

;*---------------------------------------------------------------------*/
;*    js-function? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-function? o)
   (and (%object? o)
	(=u32 (JS-OBJECT-MODE-JSFUNCTIONTAG)
	   (bit-andu32 (js-object-mode o) (JS-OBJECT-MODE-JSFUNCTIONTAG)))))

;*---------------------------------------------------------------------*/
;*    js-method? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-method? o)
   (isa? o JsMethod))

;*---------------------------------------------------------------------*/
;*    js-procedure-proxy? ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-procedure-proxy? o)
   (and (%object? o)
	(>u32 (bit-andu32 (js-object-mode o)
		 (bit-oru32 (JS-OBJECT-MODE-JSPROCEDURETAG)
		    (JS-OBJECT-MODE-JSPROXYFUNCTION)))
	   #u32:0)))

;*---------------------------------------------------------------------*/
;*    js-procedure? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-procedure? o)
   (cond-expand
      (bigloo-c
       ($js-procedure? o (JS-OBJECT-MODE-JSPROCEDURETAG)))
      (else
       (and (%object? o)
	    (=u32 (JS-OBJECT-MODE-JSPROCEDURETAG)
	       (bit-andu32 (js-object-mode o) (JS-OBJECT-MODE-JSPROCEDURETAG)))))))

;*---------------------------------------------------------------------*/
;*    js-object-procedure? ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-object-procedure? o)
   (cond-expand
      (bigloo-c
       ($js-object-procedure? o (JS-OBJECT-MODE-JSPROCEDURETAG)))
      (else
       (=u32 (JS-OBJECT-MODE-JSPROCEDURETAG)
	  (bit-andu32 (js-object-mode o) (JS-OBJECT-MODE-JSPROCEDURETAG))))))

;*---------------------------------------------------------------------*/
;*    js-callable? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-callable? o)
   (and (%object? o)
	(not (=u32 #u32:0
		(bit-andu32 (js-object-mode o)
		   (bit-oru32 (JS-OBJECT-MODE-JSFUNCTIONTAG)
		      (JS-OBJECT-MODE-JSPROCEDURETAG)))))))

;*---------------------------------------------------------------------*/
;*    js-service? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-service? o)
   (isa? o JsService))

;*---------------------------------------------------------------------*/
;*    js-symbol? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-symbol? o)
   (isa? o JsSymbolLiteral))

;*---------------------------------------------------------------------*/
;*    js-date? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (js-date? o)
   (when (js-object? o)
      (eq? (object-class o) JsDate)))

;*---------------------------------------------------------------------*/
;*    js-proxy? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-proxy? o)
   (when (js-object? o)
      (eq? (object-class o) JsProxy)))

;*---------------------------------------------------------------------*/
;*    js-proxy-array? ...                                              */
;*---------------------------------------------------------------------*/
(define (js-proxy-array? o)
   (when (js-proxy? o)
      (let ((target (js-proxy-target o)))
	 (or (js-array? target)
	     (js-proxy-array? target)))))

;*---------------------------------------------------------------------*/
;*    js-proxy-function? ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-proxy-function? o)
   (and (%object? o)
	(>u32 (bit-andu32 (js-object-mode o) (JS-OBJECT-MODE-JSPROXYFUNCTION))
	   #u32:0)))

;*---------------------------------------------------------------------*/
;*    js-map? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-map? o)
   (isa? o JsMap))

;*---------------------------------------------------------------------*/
;*    js-weakmap? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-weakmap? o)
   (isa? o JsMap))

;*---------------------------------------------------------------------*/
;*    js-set? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-set? o)
   (isa? o JsMap))

;*---------------------------------------------------------------------*/
;*    js-weakset? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-weakset? o)
   (isa? o JsMap))

;*---------------------------------------------------------------------*/
;*    js-object-cmap ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-object-cmap o)
   (with-access::JsObject o (cmap) cmap))

;*---------------------------------------------------------------------*/
;*    js-object-cmap ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-object-cmap-set! o cm)
   (with-access::JsObject o (cmap) (set! cmap cm)))

;*---------------------------------------------------------------------*/
;*    js-object-proto ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-object-proto o)
   (object-widening o))

;*---------------------------------------------------------------------*/
;*    js-object-proto-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-object-proto-set! o p)
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
   (cond-expand
      (bigloo-c ($hop-object-header-size-set! o (uint32->fixnum p)))
      (else (object-header-size-set! o (uint32->fixnum p)))))

;*---------------------------------------------------------------------*/
;*    js-object-function-tag? ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-object-function-tag? o)
   (=u32 (JS-OBJECT-MODE-JSFUNCTIONTAG)
      (bit-andu32 (js-object-mode o) (JS-OBJECT-MODE-JSFUNCTIONTAG))))

;*---------------------------------------------------------------------*/
;*    js-object-procedure-tag? ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (js-object-procedure-tag? o)
   (=u32 (JS-OBJECT-MODE-JSPROCEDURETAG)
      (bit-andu32 (js-object-mode o) (JS-OBJECT-MODE-JSPROCEDURETAG))))

;*---------------------------------------------------------------------*/
;*    jsindex12-max ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (jsindex12-max::uint32)
   #u32:4096)

;*---------------------------------------------------------------------*/
;*    js-procedure-arity ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-procedure-arity o)
   (with-access::JsProcedure o (arity) arity))

;*---------------------------------------------------------------------*/
;*    js-procedure-procedure ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-procedure-procedure o)
   (with-access::JsProcedure o (procedure) procedure))

;*---------------------------------------------------------------------*/
;*    js-proxy-target ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-proxy-target o)
   (object-widening o))

;*---------------------------------------------------------------------*/
;*    js-proxy-target-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-proxy-target-set! o t)
   (object-widening-set! o t))

;*---------------------------------------------------------------------*/
;*    js-proxy-target* ...                                             */
;*---------------------------------------------------------------------*/
(define (js-proxy-target* o)
   (let ((o (js-proxy-target o)))
      (if (js-proxy? o)
	  (js-proxy-target* o)
	  o)))

;*---------------------------------------------------------------------*/
;*    js-generator-inline-ref ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (js-generator-inline-ref o::JsGenerator idx::long)
   (cond-expand
      ((and bigloo-c (not disable-inline))
       (pragma::obj "VECTOR_REF( BVECTOR( (obj_t)(( ((obj_t *)(&(((BgL_jsgeneratorz00_bglt)(COBJECT($1)))->BgL_z52envz52))) + 1))), $2 )" o idx))
      (else
       (with-access::JsGenerator o (%env)
	  (vector-ref %env idx)))))

;*---------------------------------------------------------------------*/
;*    js-generator-inline-set! ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (js-generator-inline-set! o::JsGenerator idx::long val::obj)
   (cond-expand
      ((and bigloo-c (not disable-inline))
       (pragma::obj "VECTOR_SET( BVECTOR( (obj_t)(( ((obj_t *)(&(((BgL_jsgeneratorz00_bglt)(COBJECT($1)))->BgL_z52envz52))) + 1))), $2, $3 )" o idx val))
      (else
       (with-access::JsGenerator o (%env)
	  (vector-set! %env idx val)))))

