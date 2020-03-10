;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/property.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 25 07:05:26 2013                          */
;*    Last change :  Mon Mar  9 17:55:10 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript property handling (getting, setting, defining and     */
;*    deleting).                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_property

   (library hop)
   
   (option  (bigloo-compiler-debug-set! 0))
   
   (include "stringliteral.sch"
	    "property.sch"
	    "types.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_worker
	   __hopscript_pair
	   __hopscript_obj
	   __hopscript_function
	   __hopscript_lib
	   __hopscript_profile
	   __hopscript_arithmetic
	   __hopscript_proxy)

   (use    __hopscript_array
	   __hopscript_stringliteral
	   __hopscript_names
	   __hopscript_arraybufferview)
   
   (extern ($js-make-pcache-table::obj (::obj ::int ::obj ::obj ::JsPropertyCache)
	      "bgl_make_pcache_table"))

   (export (js-init-property! ::JsGlobalObject)
	   (js-debug-object-cmap-id ::JsObject)
	   (generic js-debug-object ::obj #!optional (msg ""))
	   (js-debug-pcache ::obj #!optional (msg ""))
	   (js-debug-cmap ::obj #!optional (msg ""))
	   (%define-pcache ::int)
	   (js-make-pcache-table ::int ::obj #!optional profile-info-table)
	   (js-pcache-table-profile-init::obj ::obj ::long ::obj)
	   (js-validate-pmap-pcache! ::JsPropertyCache)
	   (js-pcache-update-descriptor! ::JsPropertyCache ::long ::JsObject ::obj)
	   (inline js-pcache-ref ::obj ::int)
	   (inline js-pcache-imap ::JsPropertyCache)
	   (inline js-pcache-emap ::JsPropertyCache)
	   (inline js-pcache-cmap ::JsPropertyCache)
	   (inline js-pcache-amap ::JsPropertyCache)
	   
	   (inline property-name::JsStringLiteral ::struct)

	   (js-names->cmap::JsConstructMap ::vector ::bool)
	   (js-strings->cmap::JsConstructMap ::vector ::JsGlobalObject)
	   (js-object-literal-init! ::JsObject)
	   (js-object-literal-spread-assign! ::JsObject ::obj ::JsGlobalObject)

	   (js-object-unmap! ::JsObject)
	   
	   (inline js-is-accessor-descriptor?::bool obj)
	   (inline js-is-data-descriptor?::bool obj)
	   (inline js-is-generic-descriptor?::bool obj)
	   (js-from-property-descriptor ::JsGlobalObject propname desc ::obj)
	   (js-to-property-descriptor ::JsGlobalObject desc ::obj)
	   (inline js-property-amap-value ::obj ::JsObject ::obj ::JsPropertyDescriptor ::JsGlobalObject)
	   (inline js-property-amap-value-set! ::obj ::JsObject ::obj ::JsPropertyDescriptor ::obj ::JsGlobalObject)
	   (js-property-value ::obj ::JsObject ::obj ::JsPropertyDescriptor ::JsGlobalObject)
	   (js-property-value-set! ::obj ::JsObject ::obj ::JsPropertyDescriptor ::obj ::JsGlobalObject)
	   
	   (js-object-add! obj::JsObject index::long value)
	   (js-object-ctor-add! obj::JsObject index::long value)
	   (js-object-ctor-push! obj::JsObject index::long value)
	   
	   (generic js-properties-names::pair-nil ::obj ::bool ::JsGlobalObject)
	   (generic js-properties-name::vector ::obj ::bool ::JsGlobalObject)
	   (generic js-properties-symbol::vector ::obj ::JsGlobalObject)
	   
	   (generic js-has-property::bool ::obj ::obj ::JsGlobalObject)
	   (generic js-has-own-property::bool ::obj ::obj ::JsGlobalObject)
	   (generic js-get-own-property ::obj ::obj ::JsGlobalObject)
	   
	   (generic js-get-property-value ::obj ::obj ::obj ::JsGlobalObject)
	   (js-get-jsobject-property-value ::JsObject ::obj ::obj ::JsGlobalObject)
	   
	   (js-get-property ::JsObject ::obj ::JsGlobalObject)
	   
	   (generic js-get ::obj ::obj ::JsGlobalObject)

	   (js-get-jsobject ::JsObject ::obj ::obj ::JsGlobalObject)
	   
	   (generic js-get-length::obj ::obj ::JsGlobalObject #!optional cache)
	   (js-get-lengthu32::uint32 ::obj ::JsGlobalObject #!optional cache)
	   
	   (js-get/debug ::obj ::obj ::JsGlobalObject loc)

	   (js-get/name-cache ::obj ::obj ::JsGlobalObject)
	   (js-get-jsobject/name-cache ::JsObject ::obj ::JsGlobalObject)
	   (js-get-name/cache ::obj ::obj ::bool ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   (js-get-jsobject-name/cache ::JsObject ::obj
	      ::bool ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   
	   (generic js-get-jsobject-name/cache-miss ::JsObject ::obj
	      ::bool ::JsGlobalObject ::JsPropertyCache)
	   (generic js-method-jsobject-get-name/cache-miss ::JsObject ::JsStringLiteral
	      ::bool ::JsGlobalObject ::JsPropertyCache)
	   
	   (js-get-jsobject-name/cache-imap+ ::JsObject ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   (js-get-jsobject-name/cache-cmap+ ::JsObject ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   
	   (js-global-object-get-name ::JsObject ::JsStringLiteral ::obj
	      ::JsGlobalObject)
	   (js-global-object-get-name/cache ::JsObject ::JsStringLiteral ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache)
	   
	   (js-can-put o::JsObject ::obj ::JsGlobalObject)
	   (js-unresolved-put! ::JsObject ::obj ::obj ::bool ::obj ::JsGlobalObject)
	   (js-unresolved-eval-put! ::JsObject ::obj ::obj ::bool ::obj ::JsGlobalObject)
	   (js-decl-eval-put! ::JsObject ::obj ::obj ::bool ::JsGlobalObject)
	   
	   (generic js-put! ::obj ::obj ::obj ::bool ::JsGlobalObject)
	   (generic js-put-length! ::obj ::obj ::bool ::obj ::JsGlobalObject)
	   (js-put/debug! ::obj ::obj ::obj ::bool ::JsGlobalObject loc)
	   (generic js-put/cache! ::obj ::obj ::obj ::bool ::JsGlobalObject
	      #!optional (point -1) (cspecs '()) (src "") (cachefun #t))
	   (js-put-name/cache! ::obj ::JsStringLiteral ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache
	      #!optional (point -1) (cspecs '()) (cachefun #t))
	   (js-put-jsobject-name/cache! ::JsObject ::obj ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache
	      #!optional (point -1) (cspecs '()) (cachefun #t))

	   (js-put-jsobject/name-cache! o::JsObject prop::JsStringLiteral v::obj
	      throw::bool %this
	      #!optional (point -1) (cspecs '()) (src ""))
	   
	   (generic js-put-jsobject-name/cache-miss! ::JsObject
	      ::JsStringLiteral ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache ::long ::bool)
	   (js-put-jsobject-name/cache-imap+! ::JsObject ::obj ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache
	      #!optional (point -1) (cspecs '()) (cachefun #f))
	   (js-put-jsobject-name/cache-cmap+! ::JsObject ::obj ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache
	      #!optional (point -1) (cspecs '()) (cachefun #f))
	   (js-put-jsobject-name/cache-pmap+! ::JsObject ::obj ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache
	      #!optional (point -1) (cspecs '()) (cachefun #f))
	   
	   (generic js-delete! ::obj ::obj ::bool ::JsGlobalObject)
	   
	   (generic js-replace-own-property! ::JsObject
	      old::JsPropertyDescriptor
	      new::JsPropertyDescriptor)
	   
	   (js-define-own-property%::bool o::JsObject
	      name::obj desc::JsPropertyDescriptor throw::bool
	      ::JsGlobalObject)
	   
	   (generic js-define-own-property::bool o::JsObject p
	      desc::JsPropertyDescriptor throw::bool
	      ::JsGlobalObject)

	   (generic js-getprototypeof o::obj ::JsGlobalObject ::obj)
	   (generic js-setprototypeof o::obj ::obj ::JsGlobalObject ::obj)
	   
	   (generic js-seal ::JsObject ::obj)
	   (generic js-freeze ::JsObject ::obj)
	   (js-prevent-extensions ::JsObject)
	   
	   (generic js-for-in ::obj ::procedure ::JsGlobalObject)
	   (generic js-for-in-prototype ::JsObject ::JsObject ::procedure ::JsGlobalObject)
	   (generic js-for-of ::obj ::procedure ::bool ::JsGlobalObject)
	   (js-for-of-iterator ::obj ::obj ::procedure ::bool ::JsGlobalObject)
	   
	   (js-bind! ::JsGlobalObject ::JsObject name::obj
	      #!key
	      (value #f)
	      (get #f)
	      (set #f)
	      (writable #t)
	      (enumerable #t)
	      (configurable #t)
	      (hidden-class #t))

	   (js-define ::JsGlobalObject ::JsObject
	      ::JsStringLiteral ::procedure ::obj ::obj ::obj
	      #!key (hidden-class #t))
	   
	   (js-method-call-name/cache ::JsGlobalObject ::obj ::obj
	      ::JsPropertyCache ::JsPropertyCache ::long ::pair-nil ::pair-nil . args)
	   (js-method-jsobject-call-name/cache ::JsGlobalObject ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache ::long ::pair-nil ::pair-nil . args)
	   
	   (js-method-jsobject-call/cache-miss ::JsGlobalObject
	      ::JsObject ::obj ::pair-nil
	      ::JsPropertyCache ::JsPropertyCache ::long ::pair-nil ::pair-nil)
	   
	   (js-call/cache ::JsGlobalObject ::JsPropertyCache obj this . args)
	   (js-call/cache-miss0 ::JsGlobalObject ::JsPropertyCache obj this)
	   (js-call/cache-miss1 ::JsGlobalObject ::JsPropertyCache obj this a0)
	   (js-call/cache-miss2 ::JsGlobalObject ::JsPropertyCache obj this a0 a1)
	   (js-call/cache-miss3 ::JsGlobalObject ::JsPropertyCache obj this a0 a1 a2)
	   (js-call/cache-miss4 ::JsGlobalObject ::JsPropertyCache obj this a0 a1 a2 a3)
	   (js-call/cache-miss5 ::JsGlobalObject ::JsPropertyCache obj this a0 a1 a2 a3 a4)
	   (js-call/cache-miss6 ::JsGlobalObject ::JsPropertyCache obj this a0 a1 a2 a3 a4 a5)
	   (js-call/cache-miss7 ::JsGlobalObject ::JsPropertyCache obj this a0 a1 a2 a3 a4 a5 a6)
	   (js-call/cache-miss8 ::JsGlobalObject ::JsPropertyCache obj this a0 a1 a2 a3 a4 a5 a6 a7)
	   
	   (js-get-vindex::long ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    inline thresholds ...                                            */
;*---------------------------------------------------------------------*/
(define (inline-threshold) #u32:100)
(define (vtable-threshold) #u32:200)
(define (method-invalidation-threshold) 8)

;*---------------------------------------------------------------------*/
;*    js-cache-table ...                                               */
;*---------------------------------------------------------------------*/
(define js-cache-table-lock (make-spinlock "js-cache-table"))
(define js-cache-vtable-lock (make-spinlock "js-cache-vtable"))
(define js-cache-table (make-vector 256))
(define js-cache-index 0)

(define js-pmap-valid #f)

;*---------------------------------------------------------------------*/
;*    miss-object ...                                                  */
;*    -------------------------------------------------------------    */
;*    This dummy object is used by JS-GET-JSOBJECT-NAME/CACHE-MISS     */
;*    to fill its cache on a miss.                                     */
;*---------------------------------------------------------------------*/
(define miss-object #f)

;*---------------------------------------------------------------------*/
;*    js-validate-pmap-pcache! ...                                     */
;*---------------------------------------------------------------------*/
(define (js-validate-pmap-pcache! pcache)
   (with-access::JsPropertyCache pcache (registered)
      (set! js-pmap-valid #t)
      (unless registered
	 (synchronize js-cache-table-lock
	    (set! registered #t)
	    (let ((len (vector-length js-cache-table)))
	       (when (=fx js-cache-index len)
		  (let ((nvec (copy-vector js-cache-table (*fx 2 len))))
		     (set! js-cache-table nvec)))
	       (vector-set! js-cache-table js-cache-index pcache)
	       (set! js-cache-index (+fx js-cache-index 1)))))))

;*---------------------------------------------------------------------*/
;*    js-invalidate-pmap-pcaches! ...                                  */
;*    -------------------------------------------------------------    */
;*    Called when:                                                     */
;*       1) a __proto__ is changed, or                                 */
;*       2) an accessor property or a non default data property is     */
;*          added to an object, or                                     */
;*       3) a property is deleted, or                                  */
;*       4) a property hidding a prototype property is added.          */
;*       5) ...                                                        */
;*---------------------------------------------------------------------*/
(define (js-invalidate-pmap-pcaches! %this::JsGlobalObject reason who)
   
   (define (invalidate-pcache-pmap! pcache)
      (with-access::JsPropertyCache pcache (pmap emap amap)
	 (when (object? pmap) (reset-cmap-vtable! pmap))
	 (when (object? amap) (reset-cmap-vtable! amap))
	 (set! pmap #t)
	 (set! emap #t)
	 (set! amap #t)))

   (when js-pmap-valid
      (synchronize js-cache-table-lock
	 (when #f
	    (tprint "--- invalidate " reason " " who " len=" js-cache-index " ---------------------------")
	    (set! invcount (+fx 1 invcount))
	    (tprint "invcount=" invcount))
	 (let loop ((i (-fx js-cache-index 1)))
	    (when (>=fx i 0)
	       (invalidate-pcache-pmap! (vector-ref js-cache-table i))
	       (loop (-fx i 1))))
	 (set! js-pmap-valid #f)
	 (log-pmap-invalidation! reason))))

(define invcount 0)

;*---------------------------------------------------------------------*/
;*    js-invalidate-cache-method! ...                                  */
;*    -------------------------------------------------------------    */
;*    Method invalidation must be proppagated to all the sub hidden    */
;*    classes (see bug nodejs/simple/test-stream2-readable-wrap.js).   */
;*---------------------------------------------------------------------*/
(define (js-invalidate-cache-method! cmap::JsConstructMap idx::long reason who)
   (with-access::JsConstructMap cmap (methods transitions)
      (when (vector-ref methods idx)
	 (vector-set! methods idx #f)
	 (for-each (lambda (tr)
		      (let ((ncmap (transition-nextmap tr)))
			 (js-invalidate-cache-method! ncmap idx reason who)))
	    transitions))))
   
;*---------------------------------------------------------------------*/
;*    js-init-property! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-init-property! %this)
   (unless (vector? __js_strings)
      (set! __js_strings (&init!))
      (set! miss-object
	 (instantiateJsObject
	    (__proto__ (js-null))
	    (elements (vector (js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    js-debug-object-cmap-id ...                                      */
;*---------------------------------------------------------------------*/
(define (js-debug-object-cmap-id o)
   (with-access::JsObject o (cmap)
      (with-access::JsConstructMap cmap (%id)
	 %id)))

;*---------------------------------------------------------------------*/
;*    js-debug-object ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (js-debug-object obj #!optional (msg ""))
   (if (js-object? obj)
       (with-access::JsObject obj (cmap elements)
	  (if (not (js-object-mapped? obj))
	      (with-access::JsConstructMap cmap (%id props)
		 (fprint (current-error-port) "=== " msg (typeof obj) " UNMAPPED"
		    " length=" (vector-length elements)
		    "\n   prop.names="
		    (map (lambda (d)
			    (with-access::JsPropertyDescriptor d (name)
			       name))
		       (vector->list elements))
		    "\n   props="
		    (map (lambda (p) (format "~s" p))
		       (vector->list elements))))
	      (with-access::JsConstructMap cmap (%id props methods size)
		 (fprint (current-error-port) "===" msg (typeof obj) " MAPPED"
		    " length=" (vector-length elements)
		    " plain=" (js-object-mode-plain? obj)
		    " inline=" (js-object-inline-elements? obj)
		    " size=" size
		    " extensible=" (js-object-mode-extensible? obj)
		    " mlengths=" (vector-length methods)
		    "\n   cmap.%id=" %id
		    "\n   elements=" (vector-map
					(lambda (v)
					   (if (js-object? v)
					       (typeof v)
					       v))
					elements)
		    "\n   prop.names=" (vector-map prop-name props)
		    "\n   cmap.props=" props))))
       (fprint (current-error-port) msg (typeof obj))))

;*---------------------------------------------------------------------*/
;*    js-debug-pcache ...                                              */
;*---------------------------------------------------------------------*/
(define (js-debug-pcache pcache #!optional (msg ""))
   (if (isa? pcache JsPropertyCache)
       (with-access::JsPropertyCache pcache (src imap cmap pmap nmap amap index vindex cntmiss)
	  (cond
	     ((isa? cmap JsConstructMap)
	      (fprint (current-error-port) "--- " msg (typeof pcache)
		 " src=" src
		 " index=" index " vindex=" vindex " cntmiss=" cntmiss)
	      (when (isa? imap JsConstructMap)
		 (with-access::JsConstructMap imap ((%iid %id) (iprops props))
		    (fprint (current-error-port) "  imap.%id=" %iid
		       " imap.props=" iprops)))
	      (when (isa? cmap JsConstructMap)
		 (with-access::JsConstructMap cmap ((%cid %id) (cprops props))
		    (fprint (current-error-port) "  cmap.%id=" %cid
		       " cmap.props=" cprops)))
	      (when (isa? pmap JsConstructMap)
		 (with-access::JsConstructMap pmap ((%pid %id) (pprops props))
		    (fprint (current-error-port) "  pmap.%id=" %pid
		       " pmap.props=" pprops)))
	      (when (isa? nmap JsConstructMap)
		 (with-access::JsConstructMap nmap ((%pid %id) (pprops props))
		    (fprint (current-error-port) "  nmap.%id=" %pid
		       " nmap.props=" pprops)))
	      (when (isa? amap JsConstructMap)
		 (with-access::JsConstructMap amap ((%aid %id) (aprops props))
		    (fprint (current-error-port) "  amap.%id=" %aid
		       " amap.props=" aprops
		       " owner=" (typeof (js-pcache-owner pcache))))))
	     ((isa? imap JsConstructMap)
	      (with-access::JsConstructMap imap ((%iid %id) (iprops props))
		 (fprint (current-error-port) "--- " msg (typeof pcache)
		    " src=" src
		    " index=" index " vindex=" vindex " cntmiss=" cntmiss)
		 (fprint (current-error-port) "  imap.%id=" %iid
		    " imap.props=" iprops)))
	     ((isa? pmap JsConstructMap)
	      (with-access::JsConstructMap pmap ((%pid %id) (pprops props))
		 (fprint (current-error-port) "--- " msg (typeof pcache)
		    " index=" index " vindex=" vindex 
		    "\n  pmap.%id=" %pid
		    " pmap.props=" pprops)))
	     ((isa? amap JsConstructMap)
	      (with-access::JsConstructMap amap ((%aid %id) (aprops props))
		 (fprint (current-error-port) "--- " msg (typeof pcache)
		    " src=" src
		    " index=" index " vindex=" vindex
		    "\n  amap.%id=" %aid
		    " amap.props=" aprops
		    " owner=" (typeof (js-pcache-owner pcache)))))
	     (else
	      (fprint (current-error-port) "--- " msg (typeof pcache)
		 " src=" src
		 " vindex=" vindex " cntmiss=" cntmiss " no map"))))
       (fprint (current-error-port) msg (typeof pcache))))

;*---------------------------------------------------------------------*/
;*    js-debug-cmap ...                                                */
;*---------------------------------------------------------------------*/
(define (js-debug-cmap cmap #!optional (msg ""))
   (with-access::JsConstructMap cmap (%id props methods size transitions inline sibling vlen)
      (fprint (current-error-port) "~~~~ " msg (typeof cmap)
	 " cmap.%id=" %id
	 " size=" size
	 " inline=" inline
	 " sibling=" (typeof sibling)
	 " plength=" (vector-length props)
	 " mlength=" (vector-length methods)
	 " vlen=" vlen
	 "\nprops.names=" (vector-map prop-name props)
	 "\nprops=" props
	 "\ntransitions="
	 (map (lambda (tr)
		 (format "~a:~a[~a]->~a"
		    (transition-name tr)
		    (typeof (transition-value tr))
		    (transition-flags tr)
		    (with-access::JsConstructMap (transition-nextmap tr) (%id)
		       %id)))
	    transitions)
	 "\n")))

;*---------------------------------------------------------------------*/
;*    js-object-add! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-object-add! obj::JsObject idx::long value)
   (with-access::JsObject obj (elements cmap)
      (let ((nlen (if (>fx (vector-length elements) 16)
		      (+fx 2 (vector-length elements))
		      (+fx 2 (*fx 2 (vector-length elements))))))
	 (let ((nels (copy-vector elements nlen)))
	    (cond-expand (profile (profile-cache-extension nlen)))
	    (vector-set! nels idx value)
	    (when ($jsobject-elements-inline? obj)
	       ;; cleanup for the GC
	       (vector-fill! elements #unspecified)
	       ;; when switching from inlined properties to non-inlined
	       ;; properties, the object cmap must change
	       (set! cmap (sibling-cmap! cmap #f)))
	    (set! elements nels)
	    obj))))

;*---------------------------------------------------------------------*/
;*    js-object-ctor-add! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-object-ctor-add! obj::JsObject idx::long value)
   (with-access::JsObject obj (cmap)
      (with-access::JsConstructMap cmap (ctor)
	 (when (js-function? ctor)
	    (with-access::JsFunction ctor (constrsize maxconstrsize)
	       (when (<fx constrsize maxconstrsize)
		  (set! constrsize (+fx 1 constrsize)))))))
   (js-object-add! obj idx value)
   obj)

;*---------------------------------------------------------------------*/
;*    js-object-push! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-object-push! obj::JsObject idx::long value)
   (with-access::JsObject obj (elements)
      (if (>=fx idx (vector-length elements))
	  (js-object-add! obj idx value)
	  (vector-set! elements idx value))))

;*---------------------------------------------------------------------*/
;*    js-object-ctor-push! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-object-ctor-push! obj::JsObject idx::long value)
   (with-access::JsObject obj (elements)
      (if (>=fx idx (vector-length elements))
	  (js-object-ctor-add! obj idx value)
	  (vector-set! elements idx value))))

;*---------------------------------------------------------------------*/
;*    js-object-push/ctor! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-object-push/ctor! obj::JsObject idx::long value ctor)
   (with-access::JsObject obj (elements)
      (if (>=fx idx (vector-length elements))
	  (begin
	     (js-object-add! obj idx value)
	     (when (js-function? ctor)
		(with-access::JsFunction ctor (constrmap constrsize maxconstrsize)
		   (when (<fx constrsize maxconstrsize)
		      (set! constrsize (+fx 1 constrsize))
		      (set! constrmap
			 (instantiate::JsConstructMap
			    (ctor ctor)
			    (size constrsize)))))))
	  (vector-set! elements idx value))))

;*---------------------------------------------------------------------*/
;*    function0->proc ...                                              */
;*---------------------------------------------------------------------*/
(define (function0->proc fun %this::JsGlobalObject)
   (if (js-function? fun)
       (with-access::JsFunction fun (procedure)
	  (if (correct-arity? procedure 1)
	      procedure
	      (lambda (this)
		 (js-call0 %this fun this))))
       (lambda (this)
	  (js-undefined))))

;*---------------------------------------------------------------------*/
;*    function1->proc ...                                              */
;*---------------------------------------------------------------------*/
(define (function1->proc fun %this::JsGlobalObject)
   (if (js-function? fun)
       (with-access::JsFunction fun (procedure)
	  (if (correct-arity? procedure 2)
	      procedure
	      (lambda (this v)
		 (js-call1 %this fun this v))))
       (lambda (this v)
	  (js-undefined))))

;*---------------------------------------------------------------------*/
;*    %define-pcache ...                                               */
;*    -------------------------------------------------------------    */
;*    This function is overriden by the macro of property_expd.sch.    */
;*    It only makes sense when compiling to C. Otherwise it is a       */
;*    dummy function.                                                  */
;*---------------------------------------------------------------------*/
(define (%define-pcache size)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    js-make-pcache-table ...                                         */
;*    -------------------------------------------------------------    */
;*    This function is used by a macro of property_expd.sch.           */
;*---------------------------------------------------------------------*/
(define (js-make-pcache-table len src #!optional profile-info-table)
   (let ((pctable ($make-vector-uncollectable len #unspecified)))
      (let loop ((i 0))
	 (when (<fx i len)
	    (vector-set! pctable i
	       (instantiate::JsPropertyCache
		  (src src)
		  (pctable pctable)))
	    (loop (+fx i 1))))
      (if (vector? profile-info-table)
	  ;; optional profile-info-table that is only provided when
	  ;; the module is compiled in profile mode
	  (js-pcache-table-vector-profile-init pctable len profile-info-table)
	  pctable)))

;*---------------------------------------------------------------------*/
;*    js-pcache-table-vector-profile-init ...                          */
;*---------------------------------------------------------------------*/
(define (js-pcache-table-vector-profile-init pctable len profile-info-table)
   (when (vector? profile-info-table)
      (js-init-names!)
      (let loop ((i (-fx len 1)))
	 (when (>=fx i 0)
	    (let ((c (vector-ref pctable i))
		  (e (vector-ref profile-info-table i)))
	       (with-access::JsPropertyCache c (name point usage)
		  (set! point (vector-ref e 0))
		  (set! name (js-name->jsstring (vector-ref e 1)))
		  (set! usage (vector-ref e 2)))
	       (loop (-fx i 1))))))
   pctable)

;*---------------------------------------------------------------------*/
;*    js-pcache-table-profile-init ...                                 */
;*---------------------------------------------------------------------*/
(define (js-pcache-table-profile-init pctable len profile-info-table)
   
   (define (pcache-ref pctable i::long)
      (free-pragma::JsPropertyCache "(BgL_jspropertycachez00_bglt)BOBJECT(&(((struct BgL_jspropertycachez00_bgl *)$1)[ $2 ]))" pctable i))
   
   (when (vector? profile-info-table)
      (js-init-names!)
      (let loop ((i (-fx len 1)))
	 (when (>=fx i 0)
	    (let ((c (pcache-ref pctable i))
		  (e (vector-ref profile-info-table i)))
	       (with-access::JsPropertyCache c (name point usage)
		  (set! point (vector-ref e 0))
		  (set! name (js-name->jsstring (vector-ref e 1)))
		  (set! usage (vector-ref e 2)))
	       (loop (-fx i 1))))))
   pctable)

;*---------------------------------------------------------------------*/
;*    js-pcache-ref ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-pcache-ref pcache index)
   (vector-ref pcache index))

;*---------------------------------------------------------------------*/
;*    js-pcache-Xmap ...                                               */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define-inline (js-pcache-cmap pcache)
   (with-access::JsPropertyCache pcache (cmap)
      cmap))

(define-inline (js-pcache-imap pcache)
   (with-access::JsPropertyCache pcache (imap)
      imap))

(define-inline (js-pcache-emap pcache)
   (with-access::JsPropertyCache pcache (emap)
      emap))

(define-inline (js-pcache-amap pcache)
   (with-access::JsPropertyCache pcache (amap)
      amap))

;*---------------------------------------------------------------------*/
;*    js-pcache-update-descriptor! ...                                 */
;*    -------------------------------------------------------------    */
;*    Used to access an object's descriptor                            */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-descriptor! pcache::JsPropertyCache i o::JsObject obj)
   [assert (obj) (js-object? obj)]   
   (with-access::JsObject o ((omap cmap))
      (unless (eq? omap (js-not-a-cmap))
	 (with-access::JsPropertyCache pcache (imap cmap pmap emap amap index owner)
	    (js-validate-pmap-pcache! pcache)
	    (set! imap #t)
	    (set! cmap #t)
	    (set! pmap #t)
	    (set! emap #t)
	    (set! amap omap)
	    (set! owner obj)
	    (set! index i)))))

;*---------------------------------------------------------------------*/
;*    js-pcache-get-direct! ...                                        */
;*    -------------------------------------------------------------    */
;*    Used to get a direct object property.                            */
;*---------------------------------------------------------------------*/
(define (js-pcache-get-direct! pcache::JsPropertyCache i o::JsObject inlp::bool)
   
   (define (update-inline! pcache omap)
      (with-access::JsPropertyCache pcache (imap cmap emap pmap amap index)
	 (with-access::JsConstructMap omap (sibling)
	    ;; set to either a sibling or #f
	    (set! cmap sibling))
	 (set! imap omap)
	 (set! pmap #t)
	 (set! emap #t)
	 (set! amap #t)
	 (set! index i)))

   (define (update-noinline! pcache omap)
      (with-access::JsPropertyCache pcache (imap cmap emap pmap amap index)
	 (with-access::JsConstructMap omap (sibling)
	    (if sibling
		(set! imap sibling)
		(set! imap #t)))
	 (set! cmap omap)
	 (set! pmap #t)
	 (set! emap #t)
	 (set! amap #t)
	 (set! index i)))
   
   (with-access::JsObject o (cmap)
      (with-access::JsConstructMap cmap (inline)
	 (if inline
	     (update-inline! pcache cmap)
	     (update-noinline! pcache cmap)))))

;*---------------------------------------------------------------------*/
;*    js-pcache-update-direct! ...                                     */
;*    -------------------------------------------------------------    */
;*    Used to access a direct object property.                         */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-direct! pcache::JsPropertyCache i o::JsObject inlp::bool)
   
   (define (update-inline! pcache omap)
      (with-access::JsPropertyCache pcache (imap cmap emap nmap pmap amap index)
	 (set! cmap omap)
	 (set! imap omap)
	 (set! pmap #t)
	 (set! nmap #t)
	 (set! emap #t)
	 (set! amap #t)
	 (set! index i)))

   (define (update-noinline! pcache omap)
      (with-access::JsPropertyCache pcache (imap cmap emap nmap pmap amap index)
	 (with-access::JsConstructMap omap (sibling)
	    (if sibling
		(set! imap sibling)
		(set! imap #t)))
	 (set! cmap omap)
	 (set! pmap #t)
	 (set! nmap #t)
	 (set! emap #t)
	 (set! amap #t)
	 (set! index i)))
   
   (with-access::JsObject o (cmap)
      (with-access::JsConstructMap cmap (inline)
	 (if inline
	     (update-inline! pcache cmap)
	     (update-noinline! pcache cmap)))))

;*---------------------------------------------------------------------*/
;*    js-pcache-update-owner! ...                                      */
;*    -------------------------------------------------------------    */
;*    Used to access a prototype object property.                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-owner! pcache::JsPropertyCache i o::JsObject obj)
   [assert (i) (>=fx i 0)]
   [assert (obj) (js-object? obj)]
   (with-access::JsObject o ((omap cmap))
      (unless (eq? omap (js-not-a-cmap))
	 (with-access::JsPropertyCache pcache (imap cmap nmap pmap emap amap index owner)
	    (js-validate-pmap-pcache! pcache)
	    (set! imap #t)
	    (set! cmap #t)
	    (set! pmap omap)
	    (set! nmap omap)
	    (set! emap #t)
	    (set! amap #t)
	    (unless (js-proxy? obj) (set! owner obj))
	    (set! index i)))))

;*---------------------------------------------------------------------*/
;*    js-pcache-next-direct! ...                                       */
;*    -------------------------------------------------------------    */
;*    Used when adding a direct property to an object.                 */
;*---------------------------------------------------------------------*/
(define (js-pcache-next-direct! pcache::JsPropertyCache o::JsObject nextmap i)
   
   (define (next-inline! pcache omap)
      (with-access::JsPropertyCache pcache (imap cmap emap pmap nmap amap index owner)
	 (set! imap nextmap)
	 (set! emap omap)
	 (set! cmap nextmap)
	 (set! nmap omap)
	 (set! pmap #f)
	 (set! amap #t)
	 (set! owner #f)
	 (set! index i)))

   (define (next-noinline! pcache omap)
      (with-access::JsPropertyCache pcache (imap cmap emap pmap nmap amap index owner)
	 (with-access::JsConstructMap omap (sibling)
	    (if sibling
		(set! imap sibling)
		(set! imap #t)))
	 (set! emap #t)
	 (set! cmap nextmap)
	 (set! nmap omap)
	 (set! pmap #f)
	 (set! amap #t)
	 (set! owner #f)
	 (set! index i)))   
   
   (with-access::JsObject o (cmap)
      (unless (eq? cmap (js-not-a-cmap))
	 (js-validate-pmap-pcache! pcache)
	 (with-access::JsConstructMap nextmap (inline)
	    (if inline
		(next-inline! pcache cmap)
		(next-noinline! pcache cmap))))))

;*---------------------------------------------------------------------*/
;*    cmap-size ...                                                    */
;*---------------------------------------------------------------------*/
(define (cmap-size cmap::JsConstructMap)
   (with-access::JsConstructMap cmap (props) (vector-length props)))

;*---------------------------------------------------------------------*/
;*    js-object-inline-next-element? ...                               */
;*---------------------------------------------------------------------*/
(define-inline (js-object-inline-next-element? o::JsObject idx::long)
   (with-access::JsObject o (elements)
      (and (js-object-inline-elements? o) (<fx idx (vector-length elements)))))

;*---------------------------------------------------------------------*/
;*    transition ...                                                   */
;*---------------------------------------------------------------------*/
(define-struct transition name value flags nextmap)

;*---------------------------------------------------------------------*/
;*    link-cmap! ...                                                   */
;*---------------------------------------------------------------------*/
(define (link-cmap! omap::JsConstructMap nmap::JsConstructMap
	   name value flags::int)
   (with-access::JsConstructMap omap (transitions lock)
      (let ((val (when (eq? name (& "__proto__"))
		    value)))
	 (set! transitions (cons (transition name val flags nmap) transitions)))
      (with-access::JsConstructMap nmap (parent)
	 (set! parent omap))
      nmap))

;*---------------------------------------------------------------------*/
;*    vector-extend ...                                                */
;*---------------------------------------------------------------------*/
(define (vector-extend::vector vec::vector val)
   ;; extend a vector with one additional slot
   (let* ((len (vector-length vec))
	  (nvec (copy-vector vec (+fx 1 len))))
      (vector-set! nvec len val)
      nvec))

;*---------------------------------------------------------------------*/
;*    extend-cmap ...                                                  */
;*---------------------------------------------------------------------*/
(define (extend-cmap omap::JsConstructMap name flags::long inline::bool)
   (with-access::JsConstructMap omap (props methods ctor)
      (let ((newprops (vector-extend props (prop name flags)))
	    (newmethods (vector-extend methods #unspecified)))
	 (instantiate::JsConstructMap
	    (inline inline)
	    (ctor ctor)
	    (props newprops)
	    (methods newmethods)))))

;*---------------------------------------------------------------------*/
;*    extend-cmap! ...                                                 */
;*---------------------------------------------------------------------*/
(define (extend-cmap! omap::JsConstructMap name flags::long)
   (with-access::JsConstructMap omap (props methods)
      (let ((newprops (vector-extend props (prop name flags)))
	    (newmethods (vector-extend methods #unspecified)))
	 (set! props newprops)
	 (set! methods newmethods)
	 omap)))

;*---------------------------------------------------------------------*/
;*    clone-cmap ...                                                   */
;*---------------------------------------------------------------------*/
(define (clone-cmap cmap::JsConstructMap)
   (with-access::JsConstructMap cmap (props methods)
      (let ((newprops (vector-copy props))
	    (newmethods (vector-copy methods)))
	 (duplicate::JsConstructMap cmap
	    (%id (gencmapid))
	    (lock (make-spinlock "JsConstructMap"))
	    (props newprops)
	    (methods newmethods)
	    (sibling #f)))))

;*---------------------------------------------------------------------*/
;*    sibling-cmap! ...                                                */
;*    -------------------------------------------------------------    */
;*    Create a sibling on demand. Siblings are connecting cmap of      */
;*    non-inlined and inlined objects (objects for which the elements  */
;*    vector is inlined or not).                                       */
;*---------------------------------------------------------------------*/
(define (sibling-cmap! cmap::JsConstructMap inl)
   (with-access::JsConstructMap cmap (sibling inline)
      (unless sibling
	 (let ((ncmap (duplicate::JsConstructMap cmap
			 (inline inl)
			 (sibling cmap)
			 (lock (make-spinlock "JsConstructMap")))))
	    (set! sibling ncmap)))
      sibling))

;*---------------------------------------------------------------------*/
;*    cmap-find-sibling ...                                            */
;*---------------------------------------------------------------------*/
(define (cmap-find-sibling cmap::JsConstructMap inl::bool)
   (with-access::JsConstructMap cmap (sibling inline)
      (if (eq? inline inl)
	  cmap
	  (sibling-cmap! cmap inl))))
   
;*---------------------------------------------------------------------*/
;*    cmap-find-transition ...                                         */
;*---------------------------------------------------------------------*/
(define (cmap-find-transition omap::JsConstructMap name val flags::int)
   
   (define (is-transition? t)
      (and (eq? (transition-name t) name)
	   (=fx (transition-flags t) flags)
	   (or (not (transition-value t))
	       (eq? (transition-value t) val))))
   
   (with-access::JsConstructMap omap (transitions lock)
      (cond
	 ((null? transitions)
	  #f)
	 ((is-transition? (car transitions))
	  (transition-nextmap (car transitions)))
	 (else
	  (let loop ((trs (cdr transitions))
		     (prev transitions))
	     (when (pair? trs)
		(let ((t (car trs)))
		   (if (is-transition? t)
		       (begin
			  ;; move the transition in the front of the list
			  (set-cdr! prev (cdr trs))
			  (set-cdr! trs transitions)
			  (set! transitions trs)
			  (transition-nextmap t))
		       (loop (cdr trs) trs)))))))))

;*---------------------------------------------------------------------*/
;*    cmap-next-proto-cmap ...                                         */
;*---------------------------------------------------------------------*/
(define (cmap-next-proto-cmap %this::JsGlobalObject cmap::JsConstructMap old new)
   (with-access::JsConstructMap cmap (parent lock (%cid %id))
      (let ((flags (property-flags #t #t #t #f)))
	 ;; 1- try to find a transition from the current cmap
	 (synchronize lock
	    (let ((nextmap (cmap-find-transition cmap (& "__proto__") new flags)))
	       (or nextmap
		   ;; 2- create a new plain cmap connected to its parent
		   ;; via a regular link
		   (let ((newmap (duplicate::JsConstructMap cmap
				    (sibling #f)
				    (%id (gencmapid)))))
		      (link-cmap! cmap newmap (& "__proto__") new flags)
		      newmap)))))))

;*---------------------------------------------------------------------*/
;*    reset-cmap-vtable! ...                                           */
;*---------------------------------------------------------------------*/
(define (reset-cmap-vtable! omap::JsConstructMap)
   (synchronize js-cache-vtable-lock
      (with-access::JsConstructMap omap (vtable vlen)
	 (set! vlen 0)
	 (set! vtable '#()))))
   
;*---------------------------------------------------------------------*/
;*    js-cmap-vtable-add! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-cmap-vtable-add! o::JsConstructMap idx::long obj cache::JsPropertyCache)
   (with-access::JsConstructMap o (vlen vcache vtable)
      (with-access::JsPropertyCache cache (pctable)
	 (synchronize js-cache-vtable-lock
	    (let ((l vlen))
	       (cond
		  ((=fx l 0)
		   (set! vlen (+fx idx 1))
		   (set! vtable (make-vector (+fx idx 1) #unspecified))
		   (log-vtable! idx vtable '#()))
		  ((>=fx idx l)
		   (let ((old vtable))
		      (set! vlen (+fx idx 1))
		      (set! vtable (copy-vector vtable (+fx idx 1)))
		      (log-vtable! idx vtable old)
		      (vector-fill! vtable #unspecified l))))
	       (vector-set! vtable idx obj)
	       (set! vcache pctable)
	       obj)))))

;*---------------------------------------------------------------------*/
;*    js-get-vindex ...                                                */
;*---------------------------------------------------------------------*/
(define (js-get-vindex %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-vindex)
      (let ((i js-vindex))
	 (set! js-vindex (+fx 1 js-vindex))
	 i)))

;*---------------------------------------------------------------------*/
;*    property-name ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (property-name p)
   (struct-ref p 0))

;*---------------------------------------------------------------------*/
;*    js-names->cmap ...                                               */
;*    -------------------------------------------------------------    */
;*    Used by j2sscheme to create literal objects.                     */
;*---------------------------------------------------------------------*/
(define (js-names->cmap names inline)
   (instantiate::JsConstructMap
      (inline inline)
      (props (vector-map (lambda (n) (prop n (property-flags #t #t #t #f)))
		names))
      (methods (make-vector (vector-length names) #unspecified))))
      
;*---------------------------------------------------------------------*/
;*    js-strings->cmap ...                                             */
;*    -------------------------------------------------------------    */
;*    Used by j2sscheme to create literal objects.                     */
;*---------------------------------------------------------------------*/
(define (js-strings->cmap names %this)
   (instantiate::JsConstructMap
      (inline #t)
      (props (vector-map (lambda (n)
			    (prop (js-name->jsstring n)
			       (property-flags #t #t #t #f)))
		names))
      (methods (make-vector (vector-length names) #unspecified))))
      
;*---------------------------------------------------------------------*/
;*    js-object-literal-init! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-object-literal-init! o::JsObject)
   (with-access::JsObject o ((%elements elements) cmap)
      (with-access::JsConstructMap cmap ((%methods methods) props)
	 (let ((elements %elements)
	       (methods %methods))
	    (let loop ((i (-fx (vector-length elements) 1)))
	       (if (=fx i -1)
		   o
		   (let ((v (vector-ref elements i)))
		      (cond
			 ((not (js-function? v))
			  (js-invalidate-cache-method! cmap i
			     "non function in literal"
			     (vector-ref props i)))
			 ((eq? (vector-ref methods i) #unspecified)
			  (vector-set! methods i v))
			 (else
			  (js-invalidate-cache-method! cmap i
			     "new function in literal"
			     (vector-ref props i))))
		      (loop (-fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    js-object-literal-spread-assign! ...                             */
;*---------------------------------------------------------------------*/
(define (js-object-literal-spread-assign! target::JsObject src %this::JsGlobalObject)
	 
   (define (idx-cmp a b)
      (<=fx (string-natural-compare3 (js-name->string a) (js-name->string b))
	 0))

   (unless (or (null? src) (eq? src (js-undefined)))
      (let* ((fro (js-toobject %this src))
	     (names (js-properties-names fro #t %this))
	     (keys '())
	     (idx '()))
	 ;; keys have to be sorted as specified in
	 ;; section 9.1.12
	 (for-each (lambda (name)
		      (if (js-isindex? (js-toindex name))
			  (set! idx (cons name idx))
			  (set! keys (cons name keys))))
	    names)
	 (for-each (lambda (k)
		      (let ((val (js-get-jsobject fro fro k %this)))
			 (js-put-jsobject! target k val #t %this
			    :extend #t :override #f)))
	    (sort idx-cmp idx))
	 (for-each (lambda (k)
		      (let ((val (js-get-jsobject fro fro k %this)))
			 (js-put-jsobject! target k val #t %this
			    :extend #t :override #f)))
	    (reverse! keys))))

   target)

;*---------------------------------------------------------------------*/
;*    jsobject-map-find ...                                            */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-map-find o p succeed fail)
   (let ((i (gensym 'i))
	 (cmap (gensym 'cmap))
	 (loop (gensym 'loop)))
      `(with-access::JsObject ,o ((,cmap cmap))
	  (with-access::JsConstructMap ,cmap (props)
	     (let ((props props))
		(let ,loop ((,i (-fx (vector-length props) 1)))
		   (cond
		      ((=fx ,i -1)
		       (if (js-proxy? ,o)
			   (,succeed ,o (js-proxy-property-descriptor-index ,o ,p))
			   (,fail)))
		      ((eq? (prop-name (vector-ref props ,i)) ,p)
		       (,succeed ,o ,i))
		      (else
		       (,loop (-fx ,i 1))))))))))

;*---------------------------------------------------------------------*/
;*    jsobject-properties-find ...                                     */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-properties-find o p succeed fail)
   (let ((desc (gensym 'desc))
	 (name (gensym 'name))
	 (prop (gensym 'properties))
	 (i (gensym 'i))
	 (loop (gensym 'loop)))
      `(let ((,prop (with-access::JsObject ,o (elements) elements)))
	  (let ,loop ((,i (-fx (vector-length ,prop) 1)))
	     (if (=fx ,i -1)
		 (if (js-proxy? ,o)
		     (with-access::JsObject ,o (elements)
			(let ((,i (js-proxy-property-descriptor-index ,o ,p)))
			   (,succeed ,o (vector-ref elements ,i) ,i)))
		     (,fail))
		 (let ((,desc (vector-ref ,prop ,i)))
		    (with-access::JsPropertyDescriptor ,desc ((,name name))
		       (if (eq? ,name ,p)
			   (,succeed ,o ,desc ,i)
			   (,loop (-fx ,i 1))))))))))

;*---------------------------------------------------------------------*/
;*    js-object-find ...                                               */
;*    -------------------------------------------------------------    */
;*    This is a general macro that walks thru a prototype chain        */
;*    (iff the optional loop argument is provided) looking for a       */
;*    property. It calls one of the successXXX hooks when found.       */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-find o base name foundinmap foundinprop notfound . loop)

   (define (find obj name)
      `(with-access::JsObject ,obj (cmap)
	 (if (js-object-mapped? ,obj)
	     (jsobject-map-find ,obj ,name ,foundinmap
		(lambda ()
		   ,(if (pair? loop)
			`(let ((__proto__ (js-object-proto ,obj)))
			    (if (js-object? __proto__)
				(,(car loop) __proto__)
				(,notfound ,base)))
			`(,notfound ,base))))
	     (jsobject-properties-find ,obj ,name ,foundinprop
		(lambda ()
		   ,(if (pair? loop)
			`(let ((__proto__ (js-object-proto ,obj)))
			    (if (js-object? __proto__)
				(,(car loop) __proto__)
				(,notfound ,base)))
			`(,notfound ,base)))))))

   (if (and (symbol? o) (symbol? base))
       (let ((obj (gensym 'obj)))
	  `(let ((,obj ,o))
	      ,(if (symbol? name)
		   (find obj name)
		   (let ((nm (gensym 'name)))
		      `(let ((,nm ,name))
			  ,(find obj nm))))))
       (error "js-object-find" "arguments must be a symbol" (cons obj base))))

;*---------------------------------------------------------------------*/
;*    js-object-unmap! ...                                             */
;*    -------------------------------------------------------------    */
;*    Give up with the mapped representation for an object. I.e.,      */
;*    switch from the mapped representation to a plain property        */
;*    based representation.                                            */
;*---------------------------------------------------------------------*/
;; MS CARE 27 sep 2014: This function is used by js-freeze and js-seal
;; I don't understand why.
(define (js-object-unmap! o::JsObject)
   (js-object-mode-enumerable-set! o #t)
   (js-object-mode-plain-set! o #f)
   (with-access::JsObject o (cmap elements)
      (when (js-object-mapped? o)
	 (with-access::JsConstructMap cmap (props)
	    (let loop ((i (-fx (vector-length props) 1)))
	       (cond
		  ((=fx i -1)
		   (set! cmap (js-not-a-cmap))
		   (vector-shrink! elements (vector-length props)))
		  ((not (vector-ref props i))
		   (error "js-object-unmap!" "illegal property descriptor" i))
		  ((isa? (vector-ref elements i) JsPropertyDescriptor)
		   (loop (-fx i 1)))
		  (else
		   (let* ((name (prop-name (vector-ref props i)))
			  (flags (prop-flags (vector-ref props i)))
			  (desc (instantiate::JsValueDescriptor
				   (enumerable (flags-enumerable? flags))
				   (writable (flags-writable? flags))
				   (configurable (flags-configurable? flags))
				   (name name)
				   (value (vector-ref elements i)))))
		      (vector-set! elements i desc)
		      (loop (-fx i 1)))))))))
   o)

;*---------------------------------------------------------------------*/
;*    js-is-accessor-descriptor? ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.10.1       */
;*---------------------------------------------------------------------*/
(define-inline (js-is-accessor-descriptor? obj)
   (isa? obj JsAccessorDescriptor))

;*---------------------------------------------------------------------*/
;*    js-is-data-descriptor? ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.10.2       */
;*---------------------------------------------------------------------*/
(define-inline (js-is-data-descriptor? obj)
   (isa? obj JsDataDescriptor))

;*---------------------------------------------------------------------*/
;*    js-is-generic-descriptor? ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.10.3       */
;*---------------------------------------------------------------------*/
(define-inline (js-is-generic-descriptor? obj)
   (and (isa? obj JsPropertyDescriptor)
	(and (not (isa? obj JsDataDescriptor))
	     (not (isa? obj JsAccessorDescriptor)))))

;*---------------------------------------------------------------------*/
;*    js-from-property-descriptor ...                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.10.4       */
;*---------------------------------------------------------------------*/
(define (js-from-property-descriptor %this::JsGlobalObject propname desc owner)

   (define (js-or x y)
      (if (eq? x (js-undefined)) y x))

   (define (js-put! o::JsObject p v throw %this)
      (let ((newdesc (instantiate::JsValueDescriptor
                          (name p)
                          (value v)
                          (writable #t)
                          (enumerable #t)
                          (configurable #t))))
;          (js-invalidate-pcaches-pmap! %this p)
         (js-define-own-property o p newdesc throw %this)))
   
   (define (from-object obj desc)
      ;; proxy getOwnPropertyDescriptor returns regular objects
      ;; as property descriptors
      (js-put! obj (& "value")
	 (js-get desc (& "value") %this) #f %this)
      (js-put! obj (& "get")
	 (js-get desc (& "get") %this) #f %this)
      (js-put! obj (& "set")
	 (js-get desc (& "set") %this) #f %this)
      (js-put! obj (& "writable")
	 (js-or (js-get desc (& "writable") %this) #f) #f %this)
      (js-put! obj (& "enumerable")
	 (js-or (js-get desc (& "enumerable") %this) #f) #f %this)
      (js-put! obj (& "configurable")
	 (js-or (js-get desc (& "configurable") %this) #f) #f %this)
      obj)

   (define (from-desc obj desc)
      (cond
	 ((isa? desc JsValueDescriptor)
	  ;; 3
	  (with-access::JsValueDescriptor desc (writable value)
	     (js-put! obj (& "value") value #f %this)
	     (js-put! obj (& "writable") writable #f %this)))
	 ((isa? desc JsAccessorDescriptor)
	  ;; 4
	  (with-access::JsAccessorDescriptor desc (get set)
	     (js-put! obj (& "get") (or get (js-undefined)) #f %this)
	     (js-put! obj (& "set") (or set (js-undefined)) #f %this)))
	 ((isa? desc JsWrapperDescriptor)
	  (with-access::JsWrapperDescriptor desc (%get writable)
	     (js-put! obj (& "value") (%get owner owner propname %this) #f %this)
	     (js-put! obj (& "writable") writable #f %this))))
      (with-access::JsPropertyDescriptor desc (enumerable configurable)
	 ;; 5
	 (js-put! obj (& "enumerable") enumerable #f %this)
	 ;; 6
	 (js-put! obj (& "configurable") configurable #f %this))
      obj)

   (if (eq? desc (js-undefined))
       desc
       (with-access::JsGlobalObject %this (js-object)
	  (let ((obj (js-new %this js-object)))
	     (if (js-object? desc)
		 (from-object obj desc)
		 (from-desc obj desc))))))

;*---------------------------------------------------------------------*/
;*    js-to-property-descriptor ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.10.5       */
;*---------------------------------------------------------------------*/
(define (js-to-property-descriptor %this::JsGlobalObject obj name::obj)
   (if (not (js-object? obj))
       (js-raise-type-error %this "Property description must be an object: ~s" obj)
       (let* ((enumerable (if (js-has-property obj (& "enumerable") %this)
			      (js-toboolean (js-get obj (& "enumerable") %this))
			      (js-undefined)))
	      (configurable (if (js-has-property obj (& "configurable") %this)
				(js-toboolean (js-get obj (& "configurable") %this))
				(js-undefined)))
	      (hasget (js-has-property obj (& "get") %this))
	      (hasset (js-has-property obj (& "set") %this)))
	  (cond
	     ((or hasget hasset)
	      (let ((get (js-get obj (& "get") %this))
		    (set (js-get obj (& "set") %this)))
		 (if (or (js-has-property obj (& "writable") %this)
			 (js-has-property obj (& "value") %this)
			 (and (not (js-function? get))
			      (not (eq? get (js-undefined))))
			 (and (not (js-function? set))
			      (not (eq? set (js-undefined)))))
		     (js-raise-type-error %this
			"Illegal property descriptor ~s" obj)
		     (instantiate::JsAccessorDescriptor
			(name name)
			(get (when hasget get))
			(set (when hasset set))
			(%get (function0->proc get %this))
			(%set (function1->proc set %this))
			(enumerable enumerable)
			(configurable configurable)))))
	     ((js-has-property obj (& "value") %this)
	      (let ((writable (if (js-has-property obj (& "writable") %this)
				  (js-toboolean (js-get obj (& "writable") %this))
				  (js-undefined))))
		 (instantiate::JsValueDescriptor
		    (name name)
		    (value (js-get obj (& "value") %this))
		    (writable writable)
		    (enumerable enumerable)
		    (configurable configurable))))
	     ((js-has-property obj (& "writable") %this)
	      (instantiate::JsDataDescriptor
		 (name name)
		 (writable (js-toboolean (js-get obj (& "writable") %this)))
		 (enumerable enumerable)
		 (configurable configurable)))
	     (else
	      (instantiate::JsPropertyDescriptor
		 (name name)
		 (enumerable enumerable)
		 (configurable configurable)))))))

;*---------------------------------------------------------------------*/
;*    js-property-amap-value ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (js-property-amap-value obj propowner propname desc %this)
   (with-access::JsWrapperDescriptor desc (%get)
      ;; JsWrapperDescriptor are used by JsFunction and JsProxy. 
      ;; Each proxy object, uses exactly one JsWrapperDescriptor for
      ;; all its attributes. Hence, the NAME property of the descriptor
      ;; is meaningless and this is why the %GET and %SET functions of
      ;; JsWrapperDescriptor take an explicit name as argument
      (%get obj propowner propname %this)))

;*---------------------------------------------------------------------*/
;*    js-property-amap-value-set! ...                                  */
;*    -------------------------------------------------------------    */
;*    Set the value of a property.                                     */
;*---------------------------------------------------------------------*/
(define-inline (js-property-amap-value-set! obj propowner propname desc v %this)
   (with-access::JsWrapperDescriptor desc (%set)
      ;; see JS-PROPERTY-VALUE for name
      (%set obj v propowner propname %this)))
   
;*---------------------------------------------------------------------*/
;*    js-property-value ...                                            */
;*    -------------------------------------------------------------    */
;*    Get the value of a property.                                     */
;*---------------------------------------------------------------------*/
(define (js-property-value obj propowner propname desc %this)
   (let ((cn (object-class desc)))
      (cond
	 ((eq? cn JsAccessorDescriptor)
	  (with-access::JsAccessorDescriptor desc (%get)
	     (%get obj)))
	 ((eq? cn JsValueDescriptor)
	  (with-access::JsValueDescriptor desc (value)
	     value))
	 ((eq? cn JsWrapperDescriptor)
	  (js-property-amap-value obj propowner propname desc %this))
	 (else
	  (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    js-property-value-set! ...                                       */
;*    -------------------------------------------------------------    */
;*    Set the value of a property.                                     */
;*---------------------------------------------------------------------*/
(define (js-property-value-set! obj propowner propname desc v %this)
   (let ((cn (object-class desc)))
      (cond
	 ((eq? cn JsAccessorDescriptor)
	  (with-access::JsAccessorDescriptor desc (%set)
	     (%set obj v)))
	 ((eq? cn JsValueDescriptor)
	  (with-access::JsValueDescriptor desc (value)
	     (set! value v)
	     v))
	 ((eq? cn JsWrapperDescriptor)
	  (js-property-amap-value-set! obj propowner propname desc v %this))
	 (else
	  (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    js-properties-names ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (js-properties-names o::obj enump::bool %this))

;*---------------------------------------------------------------------*/
;*    js-properties-names ::JsObject ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-properties-names o::JsObject enump::bool %this)
   
   (define (cmap->names cmap)
      (with-access::JsConstructMap cmap (props)
	 (let loop ((i (-fx (vector-length props) 1))
		    (acc '()))
	    (cond
	       ((=fx i -1)
		acc)
	       ((vector-ref props i)
		=>
		(lambda (prop)
		   (let ((p (vector-ref props i)))
		      (if (or (not enump) (flags-enumerable? (prop-flags p)))
			  (loop (-fx i 1) (cons (prop-name prop) acc))
			  (loop (-fx i 1) acc)))))
	       (else
		(loop (-fx i 1) acc))))))
   
   (define (desc->names elements)
      (let loop ((i (-fx (vector-length elements) 1))
		 (acc '()))
	 (if (=fx i -1)
	     acc
	     (with-access::JsPropertyDescriptor (vector-ref elements i) (name)
		(loop (-fx i 1) (cons name acc))))))
   
   (define (enum-desc->names elements)
      (let loop ((i (-fx (vector-length elements) 1))
		 (acc '()))
	 (if (=fx i -1)
	     acc
	     (with-access::JsPropertyDescriptor (vector-ref elements i) (enumerable name)
		(if enumerable
		    (loop (-fx i 1) (cons name acc))
		    (loop (-fx i 1) acc))))))
   
   
   (with-access::JsObject o (cmap elements)
      (cond
	 ((js-object-mapped? o)
	  (cmap->names cmap))
	 ((not enump)
	  (desc->names elements))
	 (else
	  (enum-desc->names elements)))))

;*---------------------------------------------------------------------*/
;*    js-properties-name ...                                           */
;*    -------------------------------------------------------------    */
;*    Returns a vector of the properties name.                         */
;*---------------------------------------------------------------------*/
(define-generic (js-properties-name::vector o enump::bool %this::JsGlobalObject)
   (if (pair? o)
       (js-properties-name-pair o %this)
       (js-raise-type-error %this "[[PROP]]: not an object ~s" o)))

;*---------------------------------------------------------------------*/
;*    js-properties-name ::JsObject ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-properties-name::vector o::JsObject enump::bool %this::JsGlobalObject)
   (apply vector
      (filter-map (lambda (n)
		     (when (js-jsstring? n)
			n))
	 (js-properties-names o enump %this))))

;*---------------------------------------------------------------------*/
;*    js-properties-symbol ...                                         */
;*    -------------------------------------------------------------    */
;*    Returns a vector of the properties symbol.                       */
;*---------------------------------------------------------------------*/
(define-generic (js-properties-symbol::vector o %this::JsGlobalObject)
   (if (pair? o)
       '#()
       (js-raise-type-error %this "[[PROP]]: not an object ~s" o)))

;*---------------------------------------------------------------------*/
;*    js-properties-symbol::vector ::JsObject ...                      */
;*---------------------------------------------------------------------*/
(define-method (js-properties-symbol::vector o::JsObject %this::JsGlobalObject)
   (apply vector
      (filter-map (lambda (n)
		     (when (isa? n JsSymbolLiteral)
			n))
	 (js-properties-names o #f %this))))

;*---------------------------------------------------------------------*/
;*    js-has-property ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.6       */
;*---------------------------------------------------------------------*/
(define-generic (js-has-property::bool o name::obj %this)
   #f)

;*---------------------------------------------------------------------*/
;*    js-has-property ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.6       */
;*---------------------------------------------------------------------*/
(define-method (js-has-property::bool o::JsObject name::obj %this)
   (jsobject-find o o (js-toname name %this)
      ;; cmap search
      (lambda (owner i) #t)
      ;; property search
      (lambda (owner d i) #t)
      ;; failure
      (lambda (o) #f)
      ;; prototype search
      (lambda (__proto__) (js-has-property __proto__ name %this))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ...                                          */
;*    -------------------------------------------------------------    */
;*    This generic is used to implement Object.hasOwnProperty (see     */
;*    object.scm)                                                      */
;*---------------------------------------------------------------------*/
(define-generic (js-has-own-property::bool o p::obj %this)
   (not (eq? (js-get-own-property o p %this) (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ::JsObject ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-has-own-property o::JsObject p::obj %this)
   (jsobject-find o o (js-toname p %this)
      ;; cmap search
      (lambda (owner i) #t)
      ;; prototype search
      (lambda (owner d i) #t)
      ;; not found
      (lambda (o) #f)))

;*---------------------------------------------------------------------*/
;*    js-has-upto-property ...                                         */
;*    -------------------------------------------------------------    */
;*    This function searches a property in a delimited subset of       */
;*    the prototype chain. It is used by JS-FOR-IN.                    */
;*---------------------------------------------------------------------*/
(define (js-has-upto-property o::JsObject proto::JsObject p::obj %this)
   (let loop ((obj o))
      (if (js-has-own-property obj p %this)
	  #t
	  (let ((__proto__ (js-object-proto obj)))
	     (unless (eq? __proto__ proto)
		(loop __proto__))))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.1       */
;*    -------------------------------------------------------------    */
;*    Returns the property _directly_ owned by the object (i.e.,       */
;*    without traversing the prototype chain).                         */
;*---------------------------------------------------------------------*/
(define-generic (js-get-own-property o::obj p::obj %this::JsGlobalObject)
   (if (pair? o)
       (js-get-own-property-pair o p %this)
       (js-undefined)))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ::JsObject ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsObject p::obj %this::JsGlobalObject)
   ;; JsObject x obj x JsGlobalObject -> JsPropertyDescriptor | Undefined
   (jsobject-find o o (js-toname p %this)
      ;; cmap search
      (lambda (owner i)
	 (with-access::JsObject owner (cmap elements)
	    (if (isa? (vector-ref elements i) JsPropertyDescriptor)
		(vector-ref elements i)
		(with-access::JsConstructMap cmap (props)
		   (let ((name (prop-name (vector-ref props i)))
			 (flags (prop-flags (vector-ref props i))))
		      (instantiate::JsValueDescriptor
			 (writable (flags-writable? flags))
			 (enumerable (flags-enumerable? flags))
			 (configurable (flags-configurable? flags))
			 (name name)
			 (value (vector-ref elements i))))))))
      ;; prototype search
      (lambda (owner d i)
	 d)
      ;; not found
      (lambda (o) (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-get-property ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.2       */
;*    -------------------------------------------------------------    */
;*    Returns the property of the object (i.e., traverses the          */
;*    prototype chain if the object does not own it directly).         */
;*---------------------------------------------------------------------*/
(define (js-get-property o::JsObject p::obj %this::JsGlobalObject)
   ;; JsObject x obj x JsGlobalObject -> JsPropertyDescriptor | Undefined
   (let ((desc (js-get-own-property o p %this)))
      (if (eq? desc (js-undefined))
	  (let ((__proto__ (js-object-proto o)))
	     (if (js-object? __proto__)
		 (js-get-property __proto__ p %this)
		 (js-undefined)))
	  desc)))

;*---------------------------------------------------------------------*/
;*    js-get-property-value ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.2       */
;*    -------------------------------------------------------------    */
;*    This function is a generic to allow subclass optimization.       */
;*    See for instance sj JsString class. By overriding this generic,  */
;*    it avoids, allocating a JsValueDescriptor that is immediately    */
;*    destructured by JS-GET-PROPERTY-VALUE. This optimization only    */
;*    applies to the first level of the prototype root chain.          */
;*---------------------------------------------------------------------*/
(define-generic (js-get-property-value o::obj base p::obj %this::JsGlobalObject)
   (if (pair? o)
       (js-get-property-value-pair o base p %this)
       (js-undefined)))

;*---------------------------------------------------------------------*/
;*    js-get-property-value ::JsObject ...                             */
;*---------------------------------------------------------------------*/
(define-method (js-get-property-value o::JsObject base p::obj %this::JsGlobalObject)
   (js-get-jsobject-property-value o base p %this))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject-property-value ...                               */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject-property-value o::JsObject base p::obj %this::JsGlobalObject)
   ;; JsObject x obj x JsGlobalObject -> value | Absent
   (jsobject-find o o (js-toname p %this)
      ;; cmap search
      (lambda (owner i)
	 (with-access::JsObject owner (elements)
	    (let ((e (vector-ref elements i)))
	       (if (isa? e JsPropertyDescriptor)
		   (js-property-value base owner p e %this)
		   e))))
      ;; property search
      (lambda (owner d i)
	 (js-property-value base o p d %this))
      ;; not found
      (lambda (o)
	 (js-absent))
      ;; prototype search
      (lambda (__proto__)
	 (js-get-property-value __proto__ base p %this))))

;*---------------------------------------------------------------------*/
;*    js-get-notfound ...                                              */
;*---------------------------------------------------------------------*/
(define (js-get-notfound name throw %this)
   (cond
      ((not throw)
       (js-undefined))
      ((pair? throw)
       (match-case throw
	  ((at ?fname ?point)
	   (js-raise-reference-error %this "\"~a\" is not defined" name
	      fname point))
	  (else
	   (js-raise-reference-error %this "\"~a\" is not defined" name))))
      (else
       (js-raise-reference-error %this "\"~a\" is not defined" name))))

;*---------------------------------------------------------------------*/
;*    js-get ...                                                       */
;*    -------------------------------------------------------------    */
;*    ECMA-262, Section 8.12.3                                         */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.3       */
;*---------------------------------------------------------------------*/
(define-generic (js-get o prop %this::JsGlobalObject)
   (cond
      ((pair? o)
       (js-get-pair o (js-toname prop %this) %this))
      ((null? o)
       (js-get-null o (js-toname prop %this) %this))
      (else
       (let ((obj (js-toobject %this o)))
	  (if obj
	      (js-get-jsobject obj o prop %this)
	      (js-raise-type-error %this "[[GET]]: not an object ~s" o))))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsObject ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsObject prop %this::JsGlobalObject)
   (js-get-jsobject o o prop %this))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject ::JsObject ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.3       */
;*    -------------------------------------------------------------    */
;*    This function seconds JS-GET. It is required because in strict   */
;*    mode, "this" is not converted in a object, which demands         */
;*    to keep the base object (the actual receiver) available.         */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject o::JsObject base prop %this)
   (let ((pval (js-get-property-value o base prop %this)))
      (if (eq? pval (js-absent))
          (js-undefined)
          pval)))

;*---------------------------------------------------------------------*/
;*    js-get/debug ...                                                 */
;*    -------------------------------------------------------------    */
;*    Instrumented version of js-get to provide information about      */
;*    potential type errors.                                           */
;*---------------------------------------------------------------------*/
(define (js-get/debug _o prop %this::JsGlobalObject loc)
   (when *profile-cache*
      (cond
	 ((js-jsstring? prop)
	  (js-profile-log-get prop loc))
	 ((number? prop)
	  (unless (isa? _o JsArray)
	     (js-profile-log-get
		(js-ascii-name->jsstring (number->string prop)) loc)))))
   (cond
      ((pair? _o)
       (js-get-pair _o (js-toname prop %this) %this))
      ((null? _o)
       (js-get-null _o (js-toname prop %this) %this))
      (else
       (let ((o (js-toobject/debug %this loc _o)))
	  ;; must still use _o object
	  ;; see https://bugs.ecmascript.org/show_bug.cgi?id=333
	  ;; or test262 10.4.3-1-106.js
	  (js-get _o prop %this)))))

;*---------------------------------------------------------------------*/
;*    js-get-length ::obj ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (js-get-length o::obj %this::JsGlobalObject #!optional cache)
   (if cache
       (js-get-name/cache o (& "length") #f %this cache)
       (js-get o (& "length") %this)))

;*---------------------------------------------------------------------*/
;*    js-get-lengthu32::uint32 ::obj ...                               */
;*    -------------------------------------------------------------    */
;*    This is not defined as a generic to avoid boxing uint32          */
;*    results.                                                         */
;*---------------------------------------------------------------------*/
(define (js-get-lengthu32::uint32 o::obj %this::JsGlobalObject
	   #!optional cache)
   (cond
      ((js-array? o)
       (js-array-length o))
      ((isa? o JsStringLiteralASCII)
       (js-jsstring-length o))
      ((js-jsstring? o)
       (js-jsstring-codeunit-length o))
      ((isa? o JsTypedArray)
       (js-typedarray-lengthu32 o %this cache))
      (else
       (js-touint32
	  (if cache
	      (js-get-name/cache o (& "length") #f %this cache)
	      (js-get o (& "length") %this))
	  %this))))

;*---------------------------------------------------------------------*/
;*    js-get/name-cache ::obj ...                                      */
;*    -------------------------------------------------------------    */
;*    Use a per site cache for the [[GET]] operation. The property     */
;*    name is not known statically.                                    */
;*---------------------------------------------------------------------*/
(define (js-get/name-cache o prop::obj %this::JsGlobalObject)
   (cond
      ((js-object? o)
       (%js-get-jsobject/name-cache o prop %this))
      (else
       (js-get o prop %this))))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject/name-cache ...                                   */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject/name-cache o prop %this)
   (%js-get-jsobject/name-cache o prop %this))

;*---------------------------------------------------------------------*/
;*    %js-get-jsobject/name-cache ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (%js-get-jsobject/name-cache o prop %this)
   (cond
      ((js-jsstring? prop)
       (if (js-jsstring-index? prop)
	   (js-get o prop %this)
	   (synchronize-name
	      (let ((pname (js-jsstring-toname-unsafe prop)))
		 (cond
		    ((js-name-pcacher pname)
		     =>
		     (lambda (cache)
			(js-get-jsobject-name/cache o pname #f
			   %this
			   cache -1 '(imap emap cmap pmap amap vtable omiss))))
		    ((js-jsstring-index? pname)
		     (js-get o prop %this))
		    (else
		     (let ((cache (instantiate::JsPropertyCache
				     (usage 'dget)
				     (name pname)
				     (src "js-get-jsobject/name-cache"))))
			(js-name-pcacher-set! pname cache)
			(js-get-jsobject-name/cache o pname #f
			   %this
			   cache -1 '()))))))))
      ((js-array? o)
       (js-array-ref o prop %this))
      (else
       (js-get o prop %this))))

;*---------------------------------------------------------------------*/
;*    js-get-name/cache ...                                            */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*    -------------------------------------------------------------    */
;*    Use a per site cache for the [[GET]] operation. The name is a    */
;*    static constant, so the actual value is not compared against     */
;*    the cache value.                                                 */
;*---------------------------------------------------------------------*/
(define (js-get-name/cache obj name::obj throw::bool
	   %this::JsGlobalObject cache::JsPropertyCache
	   #!optional (point -1) (cspecs '()))
   (if (js-object? obj)
       (js-get-jsobject-name/cache obj name throw %this cache point cspecs)
       (js-get obj name %this)))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject-name/cache ...                                   */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject-name/cache o::JsObject name::obj
	   throw::bool %this::JsGlobalObject cache::JsPropertyCache
	   #!optional (point -1) (cspecs '()))
   (js-get-jsobject-name/cache o name throw %this cache point cspecs))

;*---------------------------------------------------------------------*/
;*    js-global-object-get-name ...                                    */
;*    -------------------------------------------------------------    */
;*    This is an inlined version of js-get-own-property.               */
;*---------------------------------------------------------------------*/
(define (js-global-object-get-name o::JsObject name::JsStringLiteral
	   throw-or-loc %this::JsGlobalObject)
   (let ((pval (js-get-property-value o o name %this)))
      (if (eq? pval (js-absent))
	  (js-get-notfound name throw-or-loc %this)
	  pval)))

;*---------------------------------------------------------------------*/
;*    js-global-object-get-name/cache ...                              */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-global-object-get-name/cache o::JsObject name::JsStringLiteral
	   throw::bool %this::JsGlobalObject
	   cache::JsPropertyCache)
   (with-access::JsObject o ((omap cmap) elements)
      (with-access::JsPropertyCache cache (cmap pmap amap index owner)
	 (cond
	    ((eq? pmap omap)
	     (with-access::JsObject owner (elements)
		(vector-ref elements index)))
	    ((eq? cmap omap)
	     (vector-ref elements index))
	    ((eq? amap omap)
	     (with-access::JsObject owner (elements)
		(let ((desc (vector-ref elements index)))
		   (js-property-value o owner name desc %this))))
	    (else
	     (js-get-jsobject-name/cache-miss o name throw %this cache))))))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject-name/cache-miss ...                              */
;*    -------------------------------------------------------------    */
;*    Use a per site cache for the [[GET]] operation. The name is a    */
;*    static constant, so the actual value is not compared against     */
;*    the cache value.                                                 */
;*---------------------------------------------------------------------*/
(define-generic (js-get-jsobject-name/cache-miss o::JsObject
		   name::obj
		   throw::bool %this::JsGlobalObject
		   cache::JsPropertyCache)
   
   (define (js-pcache-vtable! omap cache i)
      (with-access::JsPropertyCache cache (cntmiss vindex)
         (when (=fx vindex (js-not-a-index))
            (set! vindex (js-get-vindex %this)))
         (js-cmap-vtable-add! omap vindex i cache)))
   
   (with-access::JsPropertyCache cache (cntmiss (cname name) (cpoint point))
      (set! cntmiss (+u32 #u32:1 cntmiss)))

   (let loop ((obj o))
      (jsobject-find obj o name
         ;; map search
         (lambda (obj i)
            (with-access::JsObject o ((omap cmap))
               (with-access::JsObject obj (elements)
                  (with-access::JsPropertyCache cache (index owner cntmiss)
                     (let ((el-or-desc (vector-ref elements i)))
                        (cond
                           ((isa? el-or-desc JsPropertyDescriptor)
                            ;; accessor property
                            (js-pcache-update-descriptor! cache i o obj)
                            (js-property-value o obj name el-or-desc %this))
                           ((eq? o obj)
                            ;; direct access to the direct object
                            [assert (i) (<=fx i (vector-length elements))]
                            (cond
                               ((<u32 cntmiss (inline-threshold))
                                (js-pcache-get-direct! cache i o #t))
                               ((<u32 cntmiss (vtable-threshold))
                                (js-pcache-get-direct! cache i o #f))
                               ((not (eq? prop (& "__proto__")))
                                (js-pcache-vtable! omap cache i)))
                            el-or-desc)
                           (else
                            ;; direct access to a prototype object
                            (js-pcache-update-owner! cache i o obj)
                            el-or-desc)))))))
         ;; property search
         (lambda (obj desc i)
	    (with-access::JsObject obj (elements)
	       (js-property-value o obj name desc %this)))
         ;; not found
         (lambda (_o)
	    (with-access::JsObject o (cmap)
	       (unless (or (eq? cmap (js-not-a-cmap)) throw)
		  (js-pcache-update-owner! cache 0 o miss-object)))
            (js-get-notfound name throw %this))
         ;; loop
         loop)))

;*---------------------------------------------------------------------*/
;*    js-method-jsobject-get-name/cache-miss ...                       */
;*    -------------------------------------------------------------    */
;*    This function is a mix of JS-GET-JSOBJECT-NAME/CACHE-MISS and    */
;*    JS-METHOD-JSOBJECT-CALL/CACHE-MISS. It behaves as the latter     */
;*    regarding caches and misses but it returns the value as the      */
;*    former.                                                          */
;*---------------------------------------------------------------------*/
(define-generic (js-method-jsobject-get-name/cache-miss o::JsObject
		   name::JsStringLiteral
		   throw::bool %this::JsGlobalObject
		   cache::JsPropertyCache)

   (define (funval obj el-or-desc)
      (with-access::JsGlobalObject %this (js-call)
	 (let loop ((el-or-desc el-or-desc))
	    (cond
	       ((isa? el-or-desc JsPropertyDescriptor)
		(loop (js-property-value o obj name el-or-desc %this)))
	       (else
		el-or-desc)))))
   
   (define (js-pcache-vtable! omap cache f)
      (with-access::JsPropertyCache cache (cntmiss vindex)
         (when (=fx vindex (js-not-a-index))
            (set! vindex (js-get-vindex %this)))
         (js-cmap-vtable-add! omap vindex f cache)))

   (define (invalidate-pcache! cache)
      (with-access::JsPropertyCache cache (pmap emap cmap)
	 (set! pmap #t)
	 (set! emap #t)
	 (set! cmap #t)))
   
   (with-access::JsPropertyCache cache (cntmiss (cname name) (cpoint point))
      (set! cntmiss (+u32 #u32:1 cntmiss)))

   (let loop ((obj o))
      (jsobject-find obj o name
	 ;; map search
	 (lambda (obj i)
	    (with-access::JsObject o ((omap cmap) __proto__)
	       (with-access::JsObject obj ((wmap cmap) elements)
		  (with-access::JsConstructMap wmap (methods)
		     (let ((el-or-desc (vector-ref elements i)))
			(cond
			   ((or (isa? el-or-desc JsAccessorDescriptor)
				(isa? el-or-desc JsWrapperDescriptor))
			    (invalidate-pcache! cache)
			    (js-property-value o obj name el-or-desc %this))
			   ((js-function? (vector-ref methods i))
			    (let ((f (funval obj el-or-desc)))
			       (when (js-function? f)
				  (with-access::JsFunction f (len method arity)
				     (if (<fx arity 0)
					 ;; varargs functions, currently not cached...
					 (invalidate-pcache! cache)
					 (with-access::JsPropertyCache cache (cntmiss)
					    ;; correct arity, put in cache
					    (js-validate-pmap-pcache! cache)
					    ;; vtable
					    (cond
					       ((<u32 cntmiss (vtable-threshold))
						(js-pcache-update-owner! cache i o obj))
					       (else
						(js-pcache-vtable! omap cache f)))))))
			       f))
			   (else
			    (invalidate-pcache! cache)
			    (funval obj el-or-desc))))))))
	 ;; property search
	 (lambda (obj v i)
	    (with-access::JsObject obj (elements)
	       (invalidate-pcache! cache)
	       (js-property-value o obj name v %this)))
	 ;; not found
	 (lambda (o)
	    (js-raise-type-error %this "call: not a function ~s"
	       (js-undefined)))
	 ;; loop
	 loop)))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject-name/cache-cmap+ ...                             */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject-name/cache-cmap+ obj::JsObject name::JsStringLiteral
	   throw::bool %this::JsGlobalObject
	   cache::JsPropertyCache
	   #!optional (point -1) (cspecs '()))
   ;; WARNING: because of the caching of cache misses that uses the
   ;; pmap test to cache misses, pmap cannot be used in assigop.
   ;; see j2s-scheme@js2scheme/scheme.scm
   (js-get-jsobject-name/cache obj name throw %this cache point
      '(amap vtable)))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject-name/cache-imap+ ...                             */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject-name/cache-imap+ obj::JsObject name::JsStringLiteral
	   throw::bool %this::JsGlobalObject
	   cache::JsPropertyCache
	   #!optional (point -1) (cspecs '()))
   (js-get-jsobject-name/cache obj name throw %this cache point
      '(cmap pmap amap vtable)))

;*---------------------------------------------------------------------*/
;*    js-can-put ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.4       */
;*---------------------------------------------------------------------*/
(define (js-can-put o::JsObject p::obj %this::JsGlobalObject)

   (define (js-get-inherited-property o::JsObject name::obj)
      (jsobject-find o o name
	 (lambda (o i) i)
	 (lambda (o d i) d)
	 (lambda (o) #f)))

   (let ((desc (js-get-own-property o p %this)))
      ;; 1
      (cond
	 ((fixnum? desc)
	  #t)
	 ((js-is-data-descriptor? desc)
	  ;; 2.b
	  (with-access::JsDataDescriptor desc (writable)
	     (eq? writable #t)))
	 ((js-is-accessor-descriptor? desc)
	  ;; 2.a
	  (with-access::JsAccessorDescriptor desc (set)
	     (not (eq? set (js-undefined)))))
	 (else
	  (let ((proto (js-object-proto o)))
	     ;; 3
	     (if (eq? proto (js-null))
		 ;; 4
		 (js-object-mode-extensible? o)
		 ;; 5
		 (let ((inherited (js-get-inherited-property proto p)))
		    (cond
		       ((eq? inherited #f)
			;; 6
			(js-object-mode-extensible? o))
		       ((js-is-accessor-descriptor? inherited)
			;; 7
			(with-access::JsAccessorDescriptor inherited (set)
			   (not (eq? set (js-undefined)))))
		       ((fixnum? inherited)
			;; 8
			#t)
		       (else
			;; 8
			(with-access::JsDataDescriptor inherited (writable)
			   (when (js-object-mode-extensible? o)
			      writable)))))))))))

;*---------------------------------------------------------------------*/
;*    js-unresolved-put! ...                                           */
;*---------------------------------------------------------------------*/
(define (js-unresolved-put! o::JsObject p value throw::bool loc %this::JsGlobalObject)
   (js-put-jsobject! o p value throw %this :extend #f :override #f :loc loc))

;*---------------------------------------------------------------------*/
;*    js-unresolved-eval-put! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-unresolved-eval-put! scope::JsObject p value throw::bool loc %this::JsGlobalObject)
   (if (eq? (js-get-own-property scope p %this) (js-undefined))
       (js-put-jsobject! %this p value throw %this
	  :extend (not throw) :override #f :loc loc)
       (js-put! scope p value throw %this)))

;*---------------------------------------------------------------------*/
;*    js-decl-eval-put! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-decl-eval-put! scope::JsObject p value throw::bool %this::JsGlobalObject)
   (if (eq? (js-get-own-property scope p %this) (js-undefined))
       (js-put-jsobject! %this p value throw %this
	  :extend #t :override #f)
       (js-put! scope p value throw %this)))

;*---------------------------------------------------------------------*/
;*    js-put! ...                                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.5       */
;*---------------------------------------------------------------------*/
(define-generic (js-put! _o prop v::obj throw::bool %this::JsGlobalObject)
   (cond
      ((pair? _o)
       (js-put-pair! _o prop v throw %this))
      (else
       (let ((o (js-toobject %this _o)))
	  (if o
	      (js-put! o prop v throw %this)
	      (js-raise-type-error %this "[[PUT]]: not an object ~s" _o))))))

;*---------------------------------------------------------------------*/
;*    js-put-length! ::obj ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (js-put-length! o::obj v::obj throw::bool cache %this::JsGlobalObject)
   (if cache
       (js-put-name/cache! o (& "length") v throw %this cache)
       (js-put! o (& "length") v throw %this)))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsObject ...                                           */
;*    -------------------------------------------------------------    */
;*    [[Put]]                                                          */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.4    */
;*    [[CanPut]]                                                       */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.5    */
;*    [[DefineOwnProperty]]                                            */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.9    */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsObject p value throw %this)
   (js-put-jsobject! o p value throw %this
      :extend #t :override #f :cachefun #t))

;*---------------------------------------------------------------------*/
;*    js-put-jsobject! ...                                             */
;*    -------------------------------------------------------------    */
;*    [[Put]]                                                          */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.4    */
;*    [[CanPut]]                                                       */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.5    */
;*    [[DefineOwnProperty]]                                            */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.9    */
;*    -------------------------------------------------------------    */
;*    This function does not need to be generic because PUT only       */
;*    stores values in the direct object. It might use the setter      */
;*    of one of the object's prototypes but once again, the object     */
;*    to be used in the setter, is the direct object itself.           */
;*                                                                     */
;*    At the first level, special put! form for Array, String, etc.    */
;*    are overriden by method of the js-put! function.                 */
;*---------------------------------------------------------------------*/
(define (js-put-jsobject! o prop v throw::bool %this::JsGlobalObject
	   #!key
	   (extend #t)
	   (override #f)
	   (cache #f)
	   (loc #f)
	   (writable #t)
	   (enumerable #t)
	   (configurable #t)
	   (cachefun #t))
   
   (define name (js-toname prop %this))
   
   (define (reject msg)
      (if throw
	  (js-raise-type-error/loc %this loc
	     (format "[[PUT]], ~a ~~s" msg) name)
	  v))
   
   (define (check-unplain! obj prop)
      (when (or (and (js-function? obj)
		     (or (eq? prop (& "call"))
			 (eq? prop (& "apply"))))
		(not writable)
		(not enumerable)
		(not configurable))
	 (js-object-mode-plain-set! obj #f)))
   
   (define (update-from-descriptor! o propobj index::long v desc)
      ;; 8.12.5
      (with-access::JsAccessorDescriptor desc (set %set)
	 (if (js-function? set)
	     ;; 8.12.5, step 5
	     (begin
		(when (and (>=fx index 0) cache)
		   (js-pcache-update-descriptor! cache index o propobj))
		(%set o v)
		;;(js-call1 %this set o v)
		v)
	     ;; 8.12.4, setp 2.a
	     (reject "No setter defined"))))
   
   (define (update-from-wrapper-descriptor! o propobj v desc)
      (js-property-value-set! o propobj prop desc v %this))
   
   (define (update-mapped-object-value! obj cmap i v)
      (with-access::JsObject obj (cmap elements)
	 (with-access::JsConstructMap cmap (nextmap methods props)
	    (cond
	       ((and cachefun (js-function? v))
		(cond
		   ((eq? v (vector-ref methods i))
		    ;; not cached
		    v)
		   ((eq? (vector-ref methods i) #f)
		    ;; normal cached
		    (when cache
		       (js-pcache-update-direct! cache i o #t))
		    (vector-set! elements i v)
		    v)
		   (else
		    ;; invalidate cache method and cache
		    (js-invalidate-cache-method! cmap i "update-mapped with new function" name)
		    (js-invalidate-pmap-pcaches! %this "update-mapped.1" name)
		    (reset-cmap-vtable! cmap)
		    (when cache
		       (js-pcache-update-direct! cache i o #t))
		    (vector-set! elements i v)
		    v)))
	       ((js-function? (vector-ref methods i))
		;; invalidate cache method and cache
		(js-invalidate-cache-method! cmap i "update-mapped with non function" name)
		(js-invalidate-pmap-pcaches! %this "update-mapped.2" name)
		(reset-cmap-vtable! cmap)
		(when cache
		   (js-pcache-update-direct! cache i o #t))
		(vector-set! elements i v)
		v)
	       (else
		;; normal caching
		(when cache
		   (with-access::JsPropertyCache cache (cntmiss)
		      (js-pcache-update-direct! cache i o 
			 (<=u32 cntmiss (inline-threshold)))))
		(vector-set! elements i v)
		v)))))
   
   (define (update-mapped-object! obj i)
      (with-trace 'prop "update-mapped-object"
	 (trace-item "name=" name)
	 (with-access::JsObject obj (cmap elements)
	    (with-access::JsConstructMap cmap (nextmap methods props)
	       (let ((el-or-desc (vector-ref elements i)))
		  (cond
		     ((and (not override) (isa? el-or-desc JsAccessorDescriptor))
		      (with-trace 'prop "update-mapped-object.1"
			 (trace-item "name=" name)
			 ;; 8.12.5, step 5
			 (update-from-descriptor! o obj i v el-or-desc)))
		     ((eq? o obj)
		      (cond
			 ((isa? el-or-desc JsPropertyDescriptor)
			  ;; 8.12.5, step 3
			  (with-access::JsDataDescriptor el-or-desc (writable)
			     (cond
				((not writable)
				 ;; 8.12.4, step 2.b
				 (reject "Read-only property"))
				((isa? el-or-desc JsValueDescriptor)
				 ;; 8.12.5, step 3,b
				 (with-access::JsValueDescriptor el-or-desc (value)
				    (set! value v)
				    v))
				((isa? el-or-desc JsWrapperDescriptor)
				 ;; hopjs extension
				 (when cache
				    (js-pcache-update-descriptor! cache i o o))
				 (js-property-value-set! o obj name el-or-desc v %this))
				(else
				 (update-mapped-object-value! obj cmap i v)))))
			 ((flags-writable? (prop-flags (vector-ref props i)))
			  (update-mapped-object-value! obj cmap i v)) 
			 (else
			  (reject "Read-only property"))))
		     ((isa? el-or-desc JsWrapperDescriptor)
		      (let ((v (js-property-value-set! o obj name el-or-desc v %this)))
			 (if (eq? v (js-absent))
			     (if (js-proxy? obj)
				 ;; see js-proxy-property-value-set!
				 (let ((target (js-proxy-target obj)))
				    (let loop ((obj target))
				       (jsobject-find obj target name
					  update-mapped-object!
					  update-properties-object!
					  extend-object!
					  loop)))
				 (reject "Illegal object"))
			     (begin
				(when cache
				   (if (js-proxy? obj)
				       (js-pcache-update-descriptor! cache i obj obj)
				       (js-pcache-update-descriptor! cache i o o)))
				v))))
		     ;; hopjs extension
		     ((js-object-mode-frozen? obj)
		      ;; 8.12.9, step 3
		      (reject "frozen object"))
		     ((not (isa? el-or-desc JsPropertyDescriptor))
		      ;; 8.12.5, step 6
		      (if (or override (flags-writable? (prop-flags (vector-ref props i))))
			  (extend-object! o)
			  (reject "Read-only property")))
		     ((and (not (js-object-mode-extensible? obj))
			   (not (js-object-mode-sealed? obj)))
		      ;; special hop situation to handle hop-builtin object
		      (extend-object! o))
		     ((not (isa? el-or-desc JsDataDescriptor))
		      (extend-object! o))
		     (else
		      (with-access::JsDataDescriptor el-or-desc (writable)
			 (if writable
			     ;; 8.12.5, step 6
			     (extend-object! o)
			     ;; 8.12.4, step 8.b
			     (reject "Read-only property"))))))))))

   (define (extend-mapped-object/nmap nmap index flags)
      (with-access::JsObject o (cmap elements)
	 ;; follow the next map
	 (let ((nextmap (cmap-find-sibling nmap
			   (and (js-object-inline-elements? o)
				(<fx index (vector-length elements))))))
	    (with-access::JsConstructMap nextmap (ctor methods props detachcnt detachlocs)
	       (cond
		  ((or (not cachefun) (not (js-function? v)))
		   (when (js-function? (vector-ref methods index))
		      ;; invalidate cache method and cache
		      (js-invalidate-cache-method! nextmap index
			 "extend-mapped with non-function" v)
		      (js-invalidate-pmap-pcaches! %this "extend-mapped.3" name)
		      (reset-cmap-vtable! nextmap))
		   (when cache
		      (js-pcache-next-direct! cache o nextmap index)))
		  ((eq? v (vector-ref methods index))
		   (when cache
		      (js-pcache-next-direct! cache o nextmap index)))
		  ((eq? (vector-ref methods index) #f)
		   ;; invalidate the pmap caches as it might
		   ;; be that this function will be now be used
		   ;; when searching for a prototype chain
		   (js-invalidate-pmap-pcaches! %this "extend-method" name)
		   (when cache
		      (js-pcache-next-direct! cache o nextmap index)))
		  ((or (>=fx detachcnt (method-invalidation-threshold))
		       (memq loc detachlocs))
		   ;; MS 2019-01-19
		   ;; on method conflicts, instead of
		   ;; invalidating all methods,
		   ;; a new cmap is created. see
		   ;; see the prototype initialization
		   ;; in js-make-function@function.scm
		   ;; invalidate cache method and cache
		   (js-invalidate-cache-method! nextmap index
		      "extend-mapped polymorphic threshold" name)
		   (when (<=fx detachcnt (method-invalidation-threshold))
		      (js-invalidate-pmap-pcaches! %this "extend-mapped.1" name))
		   (reset-cmap-vtable! nextmap)
		   (when cache
		      ;; MS 9may2019 CARE INVALIDATE
		      (js-pcache-next-direct! cache o nextmap index)))
		  (else
		   (let ((detachedmap (extend-cmap cmap name flags
					 (and (js-object-inline-elements? o)
					      (<fx index (vector-length elements))))))
		      (set! detachcnt (+fx 1 detachcnt))
		      ;; store the loc so that if a second methods
		      ;; is added at the same location, a detached
		      ;; map will not be created and a method
		      ;; invalidation will take place.
		      (when loc
			 (set! detachlocs (cons loc detachlocs)))
		      (with-access::JsConstructMap detachedmap (methods ctor %id)
			 ;; validate cache method and don't cache
			 (vector-set! methods index v))
		      (set! nextmap detachedmap)
		      v)))
	       (js-object-push/ctor! o index v ctor)
	       (set! cmap nextmap)
	       v))))
   
   (define (extend-mapped-object!)
      (js-object-mode-enumerable-set! o #t)
      (when (and (js-jsstring? name) (js-jsstring->number name))
	 (js-object-mode-hasnumeralprop-set! o #t))
      ;; 8.12.5, step 6
      (with-access::JsObject o (cmap elements)
	 (with-access::JsConstructMap cmap (props single lock)
	    (let* ((index (vector-length props))
		   (flags (property-flags writable enumerable configurable #f)))
	       (synchronize lock
		  (let loop ()
		     (cond
			((cmap-find-transition cmap name v flags)
			 =>
			 (lambda (nmap)
			    (extend-mapped-object/nmap nmap index flags)))
			(single
			 (js-invalidate-pmap-pcaches! %this "extend-mapped.4" name)
			 (extend-cmap! cmap name flags)
			 (with-access::JsConstructMap cmap (ctor methods)
			    (if (and cachefun (js-function? v))
				;; validate cache method and don't cache
				(vector-set! methods index v)
				(begin
				   (js-invalidate-cache-method! cmap index
				      "extend-mapped single non-function" name)
				   (when cache
				      (js-pcache-next-direct! cache o cmap index))))
			    (js-object-push/ctor! o index v ctor))
			 v)
			(else
			 ;; create a new map
			 (let ((nextmap (extend-cmap cmap name flags
					   (and (js-object-inline-elements? o)
						(<fx index (vector-length elements))))))
			    (js-invalidate-pmap-pcaches! %this "extend-mapped.5" name)
			    (with-access::JsConstructMap nextmap (methods ctor)
			       (if (and cachefun (js-function? v))
				   ;; validate cache method and don't cache
				   (vector-set! methods index v)
				   ;; invalidate cache method and cache
				   (begin
				      (js-invalidate-cache-method! nextmap index
					 "exptend-mapped non-function" name)
				      (when cache
					 (js-pcache-next-direct! cache o nextmap index))))
			       (link-cmap! cmap nextmap name v flags)
			       (js-object-push/ctor! o index v ctor))
			    (set! cmap nextmap)
			    v)))))))))
   
   (define (update-properties-object! obj desc i::long)
      (with-trace 'prop "update-properties-object!"
	 (cond
	    ((and (not override) (isa? desc JsAccessorDescriptor))
	     ;; 8.12.5, step 5
	     (update-from-descriptor! o obj -1 v desc))
	    ((eq? o obj)
	     ;; 8.12.5, step 3
	     (let ((owndesc desc))
		(with-access::JsDataDescriptor owndesc (writable name)
		   (if (not writable)
		       ;; 8.12.4, step 2.b
		       (reject "Read-only property")
		       ;; 8.12.5, step 3,b
		       (js-property-value-set! o o name owndesc v %this)))))
	    ((isa? desc JsWrapperDescriptor)
	     (let ((v (update-from-wrapper-descriptor! o obj v desc)))
		(cond
		   ((eq? v (js-absent))
		    (if (js-proxy? obj)
			(let ((target (js-proxy-target obj)))
			   (let loop ((obj target))
			      (jsobject-find obj target name
				 update-mapped-object!
				 update-properties-object!
				 extend-object!
				 loop)))
			(reject "Illegal object")))
		   (v
		    v)
		   ((js-object-mode-extensible? obj)
		    (extend-object! o))
		   (else
		    (reject "sealed object")))))
	    ((not (js-object-mode-extensible? obj))
	     ;; 8.12.9, step 3
	     (reject "sealed object"))
	    ((not (isa? desc JsDataDescriptor))
	     (extend-object! o))
	    (else
	     (with-access::JsDataDescriptor desc (writable)
		(if writable
		    ;; 8.12.5, step 6
		    (extend-object! o)
		    ;; 8.12.4, step 8.b
		    (reject "Read-only property")))))))
   
   (define (extend-properties-object!)
      (with-trace 'prop "extend-properties-object!"
	 (let ((newdesc (instantiate::JsValueDescriptor
			   (name name)
			   (value v)
			   (writable #t)
			   (enumerable #t)
			   (configurable #t))))
	    (js-invalidate-pmap-pcaches! %this "extend-property" name)
	    (js-define-own-property o name newdesc throw %this)
	    v)))
   
   (define (extend-object! _o)
      (with-trace 'prop "extend-object!"
	 (with-access::JsObject o (cmap)
	    (cond
	       ((not (js-object-mode-extensible? o))
		;; 8.12.9. step 3
		(reject "sealed objet"))
	       ((not extend)
		;; 11.13.1
		(js-raise-reference-error/loc %this loc
		   "[[PUT]], \"~a\" is not defined" name))
	       ((js-object-mapped? o)
		;; 8.12.5, step 6
		(extend-mapped-object!))
	       (else
		;; 8.12.5, step 6
		(extend-properties-object!))))))

   (unless cachefun
      (when (isa? v JsFunction)
	 (with-access::JsFunction v (src)
	    (match-case src
	       (((at crypto.js ?-) . ?-)
		(tprint "src=" src))))))
   (check-unplain! o name)
   (let loop ((obj o))
      (jsobject-find obj o name
	 update-mapped-object!
	 update-properties-object!
	 extend-object!
	 loop)))

;*---------------------------------------------------------------------*/
;*    js-put/debug! ...                                                */
;*---------------------------------------------------------------------*/
(define (js-put/debug! _o prop v::obj throw::bool %this::JsGlobalObject loc)
   (when *profile-cache*
      (cond
	 ((js-jsstring? prop)
	  (js-profile-log-put prop loc))
	 ((number? prop)
	  (js-profile-log-put
	     (js-ascii-name->jsstring (number->string prop)) loc))))
   (cond
      ((pair? _o)
       (js-put-pair! _o (js-toname prop %this) v throw %this))
      (else
       (let ((o (js-toobject/debug %this loc _o)))
	  (js-put! _o prop v throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-put/cache! ::obj ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (js-put/cache! o prop v::obj throw::bool %this
		   #!optional (point -1) (cspecs '()) (src "") (cachefun #t))
   (js-put! o prop v throw %this))

;*---------------------------------------------------------------------*/
;*    js-put/cache! ::JsObject ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-put/cache! o::JsObject prop v::obj throw::bool %this
		  #!optional (point -1) (cspecs '()) (src "") (cachefun #t))
   (cond
      ((js-jsstring? prop)
       (let ((pname (js-jsstring-toname prop)))
	  (synchronize-name
	     (cond
		((js-name-pcachew pname)
		 =>
		 (lambda (cache)
		    (js-put-jsobject-name/cache! o pname v throw
		       %this cache point '(imap emap cmap nmap pmap amap) #f)))
		((eq? pname (& "length"))
		 (js-put-length! o v throw #f %this))
		((isa? pname JsStringLiteralIndex)
		 (js-put! o prop v throw %this))
		(else
		 (let ((cache (instantiate::JsPropertyCache
				 (usage 'dput)
				 (src src))))
		    (js-name-pcachew-set! pname cache)
		    (js-put-jsobject-name/cache! o pname v throw
		       %this cache point cspecs cachefun)))))))
      (else
       (js-put! o prop v throw %this))))

;*---------------------------------------------------------------------*/
;*    js-put-jsobject/name-cache! ...                                  */
;*---------------------------------------------------------------------*/
(define (js-put-jsobject/name-cache! o::JsObject prop::JsStringLiteral v::obj
	   throw::bool %this
	   #!optional (point -1) (cspecs '()) (src ""))
   (let ((pname (js-jsstring-toname prop)))
      (synchronize-name
	 (cond
	    ((js-name-pcachew pname)
	     =>
	     (lambda (cache)
		(js-put-jsobject-name/cache! o pname v throw
		   %this cache point '(imap emap cmap pmap amap) #f)))
	    ((eq? pname (& "length"))
	     (js-put-length! o v throw #f %this))
	    ((isa? pname JsStringLiteralIndex)
	     (js-put! o prop v throw %this))
	    (else
	     (let ((cache (instantiate::JsPropertyCache
			     (usage 'dput)
			     (src src))))
		(js-name-pcachew-set! pname cache)
		(js-put-jsobject-name/cache! o pname v throw
		   %this cache point cspecs #f)))))))

;*---------------------------------------------------------------------*/
;*    js-put-name/cache ...                                            */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-put-name/cache! o prop::JsStringLiteral v::obj throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache
	   #!optional (point -1) (cspecs '()) (cachefun #t))
   (if (js-object? o)
       (js-put-jsobject-name/cache! o prop v throw %this cache point cspecs cachefun)
       (js-put! o prop v throw %this)))

;*---------------------------------------------------------------------*/
;*    js-put-jsobject-name/cache! ...                                  */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-put-jsobject-name/cache! o::JsObject prop::JsStringLiteral
	   v::obj throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache
	   #!optional (point -1) (cspecs '()) (cachefun #t))
   ;; rewritten by macro expansion
   (js-put-jsobject-name/cache! o prop v throw %this cache point cspecs cachefun))

;*---------------------------------------------------------------------*/
;*    js-put-jsobject-name/cache-miss! ...                             */
;*---------------------------------------------------------------------*/
(define-generic (js-put-jsobject-name/cache-miss! o::JsObject
		   prop::JsStringLiteral
		   v::obj throw::bool
		   %this::JsGlobalObject cache::JsPropertyCache
		   point cachefun)
   (with-access::JsObject o (cmap)
      (let* ((%omap cmap)
	     (tmp (js-put-jsobject! o prop v throw %this
		     :extend #t :override #f :cache cache
		     :loc point :cachefun cachefun)))
	 (with-access::JsPropertyCache cache (cntmiss name (cpoint point) usage)
	    (set! cntmiss (+u32 #u32:1 cntmiss))
	    (set! name prop)
	    (set! cpoint point))
;* 	    (set! usage 'put))                                         */
	 (unless (or (eq? %omap (js-not-a-cmap))
		     (eq? prop (& "__proto__")))
	    (with-access::JsPropertyCache cache (index vindex cntmiss)
	       (when (>=u32 cntmiss (vtable-threshold))
		  (when (>=fx index 0)
		     (when (=fx vindex (js-not-a-index))
			(set! vindex (js-get-vindex %this)))
		     (js-cmap-vtable-add! %omap vindex (cons index cmap) cache)))))
	 tmp)))

;*---------------------------------------------------------------------*/
;*    js-put-jsobject-name/cache-imap+! ...                            */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define (js-put-jsobject-name/cache-imap+! o::JsObject prop::JsStringLiteral
	   v::obj throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache
	   #!optional (point -1) (cspecs '()) (cachefun #f))
   (js-put-jsobject-name/cache! o prop v throw %this cache
      point '(cmap pmap amap vtable) cachefun))

;*---------------------------------------------------------------------*/
;*    js-put-jsobject-name/cache-cmap+! ...                            */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define (js-put-jsobject-name/cache-cmap+! o::JsObject prop::JsStringLiteral
	   v::obj throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache
	   #!optional (point -1) (cspecs '()) (cachefun #f))
   ;; WARNING: because of the caching of cache misses that uses the
   ;; pmap test to cache misses, pmap cannot be used in assigop.
   ;; see j2s-scheme@js2scheme/scheme.scm
   (js-put-jsobject-name/cache! o prop v throw %this cache
      point '(amap vtable) cachefun))

;*---------------------------------------------------------------------*/
;*    js-put-jsobject-name/cache-pmap+! ...                            */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define (js-put-jsobject-name/cache-pmap+! o::JsObject prop::JsStringLiteral
	   v::obj throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache
	   #!optional (point -1) (cspecs '()) (cachefun #f))
   (js-put-jsobject-name/cache! o prop v throw %this cache
      point '(amap vtable) cachefun))

;*---------------------------------------------------------------------*/
;*    js-bind! ...                                                     */
;*    -------------------------------------------------------------    */
;*    This is a simplified version of defineOwnProperty used to build  */
;*    the library objects. This function always binds the value in the */
;*    mentionned object. It does not follow the prototype chain. It    */
;*    does not check the extensibility flags.                          */
;*    -------------------------------------------------------------    */
;*    When HIDDEN-CLASS is FALSE, it is assumed that the object        */
;*    CMAP is not shared with any other object.                        */
;*    -------------------------------------------------------------    */
;*    [[Put]]                                                          */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.4    */
;*    [[CanPut]]                                                       */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.5    */
;*    [[DefineOwnProperty]]                                            */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.9    */
;*---------------------------------------------------------------------*/
(define (js-bind! %this::JsGlobalObject o::JsObject name::obj
	   #!key
	   (value #f)
	   (get #f)
	   (set #f)
	   (writable #t)
	   (enumerable #t)
	   (configurable #t)
	   (hidden-class #t))
   
   (define (validate-cache-method! v methods i)
      (when (js-function? v)
	 (vector-set! methods i v)))
   
   (define (accessor-property? get set)
      (or (and get (not (eq? get (js-undefined))))
	  (and set (not (eq? set (js-undefined))))))
   
   (define (check-accessor-property! get set)
      (cond
	 ((not (or (js-function? get) (eq? get (js-undefined))))
	  (js-raise-type-error %this
	     (format "wrong getter for property \"~a\", ~~a" name)
	     get))
	 ((and set (not (eq? set (js-undefined))) (not (js-function? set)))
	  (js-raise-type-error %this
	     (format "wrong setter for property \"~a\", ~~a" name) set))))
   
   (define (update-mapped-object! obj i)
      (cond
	 ((not (eq? obj o))
	  (error "js-bind!"
	     (format "cannot rebind mapped prototype property \"~a\"" name)
	     (typeof o)))
	 ((or get set)
	  (with-access::JsObject o (elements)
	     (let ((old (vector-ref elements i)))
		(when (isa? old JsAccessorDescriptor)
		   (with-access::JsAccessorDescriptor old ((oget get)
							   (oset set))
		      (when (eq? get (js-undefined))
			 (set! get oget))
		      (when (eq? set (js-undefined))
			 (set! set oset))))
		(vector-set! elements i
		   (instantiate::JsAccessorDescriptor
		      (name name)
		      (get get)
		      (set set)
		      (%get (function0->proc get %this))
		      (%set (function1->proc set %this))
		      (enumerable enumerable)
		      (configurable configurable))))))
	 (else
	  (with-access::JsObject o (elements)
	     (vector-set! elements i value)
	     value))))
   
   (define (next-cmap o::JsObject name value flags inline::bool)
      (with-access::JsObject o (cmap elements)
	 (with-access::JsConstructMap cmap (single)
	    (if (and hidden-class (not single))
		(let ((nextmap (extend-cmap cmap name flags inline)))
		   (link-cmap! cmap nextmap name value flags)
		   (set! cmap nextmap)
		   nextmap)
		(begin
		   (extend-cmap! cmap name flags)
		   cmap)))))
   
   (define (extend-mapped-object!)
      (when enumerable
	 (js-object-mode-enumerable-set! o #t))
      (when (and (js-jsstring? name) (js-jsstring->number name))
	 (js-object-mode-hasnumeralprop-set! o #t))
      ;; 8.12.5, step 6
      (with-access::JsObject o (cmap elements)
	 (with-access::JsConstructMap cmap (props lock)
	    (synchronize lock
	       (let* ((axs (accessor-property? get set))
		      (index (vector-length props))
		      (flags (property-flags writable enumerable configurable axs)))
		  (cond
		     ((cmap-find-transition cmap name value flags)
		      =>
		      (lambda (nmap)
			 (let ((nextmap (cmap-find-sibling nmap
					   (js-object-inline-elements? o)))
			       (val-or-desc (if (accessor-property? get set)
						(instantiate::JsAccessorDescriptor
						   (name name)
						   (get get)
						   (set set)
						   (%get (function0->proc get %this))
						   (%set (function1->proc set %this))
						   (enumerable enumerable)
						   (configurable configurable))
						value)))
			    ;; follow the next map 
			    (with-access::JsConstructMap nextmap (props)
			       (set! cmap nextmap)
			       (js-object-push! o index val-or-desc)
			       value))))
		     (axs
		      ;; create a new map with a JsAccessorDescriptor
		      (let* ((newdesc (instantiate::JsAccessorDescriptor
					 (name name)
					 (get get)
					 (set set)
					 (%get (function0->proc get %this))
					 (%set (function1->proc set %this))
					 (enumerable enumerable)
					 (configurable configurable)))
			     (nextmap (next-cmap o name #f flags
					 (js-object-inline-elements? o))))
			 (check-accessor-property! get set)
			 ;; extending the elements vector is mandatory
			 (set! cmap nextmap)
			 (js-object-push! o index newdesc)
			 (js-undefined)))
		     (else
		      (let ((nextmap (next-cmap o name value flags
					(js-object-inline-elements? o))))
			 (with-access::JsConstructMap nextmap (methods)
			    (validate-cache-method! value methods index))
			 (set! cmap nextmap)
			 (js-object-push! o index value)
			 value))))))))
   
   (define (update-properties-object! obj owndesc i)
      (if (or (isa? owndesc JsAccessorDescriptor) get)
	  (let ((newdesc (instantiate::JsAccessorDescriptor
			    (name name)
			    (get get)
			    (set set)
			    (%get (function0->proc get %this))
			    (%set (function1->proc set %this))
			    (enumerable enumerable)
			    (configurable configurable))))
	     (js-define-own-property o name newdesc #f %this)
	     (js-undefined))
	  (with-access::JsObject obj (elements)
	     (let ((desc (instantiate::JsValueDescriptor
			    (name name)
			    (value value)
			    (writable writable)
			    (enumerable enumerable)
			    (configurable configurable))))
		(vector-set! elements i desc))))
      value)
   
   (define (extend-properties-object!)
      (let ((desc (if get
		      (instantiate::JsAccessorDescriptor
			 (name name)
			 (get get)
			 (set set)
			 (%get (function0->proc get %this))
			 (%set (function1->proc set %this))
			 (enumerable enumerable)
			 (configurable configurable))
		      (instantiate::JsValueDescriptor
			 (name name)
			 (value value)
			 (writable writable)
			 (enumerable enumerable)
			 (configurable configurable)))))
	 (js-define-own-property o name desc #f %this)
	 (js-undefined)))
   
   (with-access::JsObject o (cmap)
      (if (js-object-mapped? o)
	  (jsobject-map-find o name
	     update-mapped-object!
	     extend-mapped-object!)
	  (jsobject-properties-find o name
	     update-properties-object!
	     extend-properties-object!))))

;*---------------------------------------------------------------------*/
;*    js-define ...                                                    */
;*    -------------------------------------------------------------    */
;*    Wrapper to js-bind! used to keep generated files smaller.        */
;*---------------------------------------------------------------------*/
(define (js-define %this obj id::JsStringLiteral
	   get set src pos #!key (hidden-class #t))
   (let ((name (js-name->jsstring
		  (string-append src ":" (integer->string pos)))))
      (js-bind! %this obj id
	 :configurable #f
	 :hidden-class hidden-class
	 :get (js-make-function %this get 1 name)
	 :set (when set (js-make-function %this set 2 name)))))

;*---------------------------------------------------------------------*/
;*    js-delete! ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.7       */
;*---------------------------------------------------------------------*/
(define-generic (js-delete! _o p throw %this)
   
   (define (configurable-mapped-property? obj i)
      (with-access::JsObject obj (cmap)
	 (with-access::JsConstructMap cmap (props)
	    (flags-configurable? (prop-flags (vector-ref props i))))))
   
   (define (delete-configurable o configurable proc)
      (cond
	 (configurable
	  (proc o)
	  #t)
	 (throw 
	  (js-raise-type-error %this
	     (string-append "delete: cannot delete ~s."
		(js-jsstring->string (js-toname p %this)))
	     o))
	 (else
	  #f)))

   (define (vector-delete! v i)
      (vector-copy! v i v (+fx i 1))
      (vector-shrink! v (-fx (vector-length v) 1)))

   (let ((n (js-toname p %this))
	 (o (js-toobject %this _o)))
      (cond
	 ((js-object? o)
	  (with-access::JsObject o (cmap)
	     (if (js-object-mapped? o)
		 (jsobject-map-find o n
		    (lambda (o i)
		       (delete-configurable o
			  (configurable-mapped-property? o i)
			  (lambda (o)
			     (js-invalidate-pmap-pcaches! %this "js-delete" p)
			     ;; create a new cmap for the object
			     (let ((nextmap (clone-cmap cmap)))
				(link-cmap! cmap nextmap n #f -1)
				[assert (o) (isa? nextmap JsConstructMap)]
				(set! cmap
				   (cmap-find-sibling nextmap
				      (js-object-inline-elements? o)))
				(with-access::JsConstructMap nextmap (props)
				   ;; remove the prop from the cmap
				   (vector-set! props i (prop #f 0))
				   #t)))))
		    (lambda () #t))
		 (jsobject-properties-find o n
		    (lambda (o d i)
		       (with-access::JsPropertyDescriptor d (configurable)
			  (delete-configurable o configurable
			     (lambda (o)
				(with-access::JsObject o (elements)
				   (vector-delete! elements i))
				#t))))
		    (lambda () #t)))))
	 (else
	  (js-raise-type-error %this
	     "delete: not an object ~s" _o)))))

;*---------------------------------------------------------------------*/
;*    js-define-own-property ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.9       */
;*---------------------------------------------------------------------*/
(define-generic (js-define-own-property::bool o::JsObject p desc throw %this)
   (js-define-own-property% o (js-toname p %this) desc throw %this))

;*---------------------------------------------------------------------*/
;*    js-define-own-property% ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.9       */
;*---------------------------------------------------------------------*/
(define (js-define-own-property%::bool o::JsObject name::obj desc::JsPropertyDescriptor throw %this::JsGlobalObject)
   
   (define rejected #f)
   
   (define (reject msg)
      (set! rejected #t)
      (when throw
	 (js-raise-type-error %this "defineOwnProperty: ~a" msg)))
   
   (define (configurable desc)
      (with-access::JsPropertyDescriptor desc (configurable)
	 configurable))
   
   (define (enumerable desc)
      (with-access::JsPropertyDescriptor desc (enumerable)
	 enumerable))
   
   (define (writable desc)
      (with-access::JsDataDescriptor desc (writable)
	 writable))
   
   (define (value desc)
      (with-access::JsValueDescriptor desc (value)
	 value))
   
   (define (set desc)
      (with-access::JsAccessorDescriptor desc (set)
	 set))
   
   (define (get desc)
      (with-access::JsAccessorDescriptor desc (get)
	 get))
   
   (define (boolify val)
      (when (boolean? val) val))
   
   (define (replace-property-descriptor! current desc)
      (when (isa? desc JsDataDescriptor)
	 (with-access::JsDataDescriptor desc (writable)
	    (unless (boolean? writable)
	       (set! writable #f))))
      (js-replace-own-property! o current desc)
      #t)
   
   (define (propagate-property-descriptor! current desc)
      (with-access::JsPropertyDescriptor current (configurable enumerable)
	 (with-access::JsPropertyDescriptor desc
	       ((denumerable enumerable)
		(dconfigurable configurable))
	    (when (boolean? denumerable)
	       (set! enumerable denumerable))
	    (when (boolean? dconfigurable)
	       (set! configurable dconfigurable))))
      #t)
   
   (define (propagate-property-descriptor2! current desc)
      (with-access::JsPropertyDescriptor current (configurable enumerable)
	 (with-access::JsPropertyDescriptor desc
	       ((denumerable enumerable)
		(dconfigurable configurable))
	    (when (and (not (boolean? enumerable)) (boolean? denumerable))
	       (set! enumerable denumerable))
	    (when (and (not (boolean? configurable)) (boolean? dconfigurable))
	       (set! configurable dconfigurable))))
      #t)
   
   (define (propagate-data-descriptor! current desc)
      (when (isa? current JsWrapperDescriptor)
	 (let ((ncurrent (duplicate::JsWrapperDescriptor current)))
	    (if (js-object-inline-elements? o)
		(with-access::JsObject o (elements)
		   (let ((len (vector-length elements)))
		      (let loop ((i 0))
			 (when (<fx i len)
			    (if (eq? (vector-ref elements i) current)
				(vector-set! elements i ncurrent)
				(loop (+fx i 1)))))))
		(with-access::JsObject o (elements)
		   (vector-map! (lambda (p)
				   (if (eq? p current) ncurrent p))
		      elements)))
	    (set! current ncurrent)))
      (propagate-property-descriptor! current desc)
      (with-access::JsDataDescriptor current (writable name)
	 (if (isa? desc JsValueDescriptor)
	     (with-access::JsValueDescriptor desc ((dvalue value))
		(when (js-function? dvalue)
		   (js-invalidate-pmap-pcaches! %this
		      "js-define-own-property%, both accessor" name))
		(cond
		   ((isa? current JsValueDescriptor)
		    (with-access::JsDataDescriptor desc ((dwritable writable))
		       (when (boolean? dwritable)
			  (set! writable dwritable)))
		    (with-access::JsValueDescriptor current (value)
		       (when (js-function? value)
			  (js-invalidate-pmap-pcaches! %this
			     "js-define-own-property%, both accessor" name))
		       (set! value dvalue)))
		   ((isa? current JsWrapperDescriptor)
		    (with-access::JsWrapperDescriptor current (%set)
		       (js-property-amap-value-set! o o name current dvalue %this)))))
	     (with-access::JsDataDescriptor desc ((dwritable writable))
		(when (boolean? dwritable)
		   (set! writable dwritable)))))
      #t)
   
   (define (propagate-accessor-descriptor! current desc)
      (propagate-property-descriptor! current desc)
      (with-access::JsAccessorDescriptor current (get %get set %set)
	 (with-access::JsAccessorDescriptor desc ((dget get)
						  (%dget %get)
						  (dset set)
						  (%dset %set))
	    (when dget (set! get dget) (set! %get %dget))
	    (when dset (set! set dset) (set! %set %dset))))
      #t)
   
   (define (same-value v1 v2)
      (if (flonum? v1)
	  (or (and (flonum? v2)
		   (or (and (=fl v1 v2) (=fx (signbitfl v1) (signbitfl v2)))
		       (and (nanfl? v1) (nanfl? v2))))
	      (and (fixnum? v2) (=fl v1 (fixnum->flonum v2))))
	  (equal? v1 v2)))
   
   (define (same-property-descriptor? current::JsPropertyDescriptor desc::JsPropertyDescriptor)
      (with-access::JsPropertyDescriptor current
	    ((cname name) (cconf configurable) (cenum enumerable))
	 (with-access::JsPropertyDescriptor desc
	       ((dname name) (dconf configurable) (denum enumerable))
	    (and (eq? cname dname)
		 (or (not (boolean? dconf)) (eq? cconf dconf))
		 (or (not (boolean? denum)) (eq? cenum denum))))))
   
   (define (same-accessor-descriptor? current::JsAccessorDescriptor desc::JsAccessorDescriptor)
      (with-access::JsAccessorDescriptor current ((cget get) (cset set))
	 (with-access::JsAccessorDescriptor desc ((dget get) (dset set))
	    (and (eq? cget dget) (eq? cset dset)))))
   
   (define (same-data-descriptor? current::JsDataDescriptor desc::JsDataDescriptor)
      (with-access::JsDataDescriptor current ((cwritable writable))
	 (with-access::JsDataDescriptor desc ((dwritable writable))
	    (eq? cwritable dwritable))))
   
   (define (same-value-descriptor? current::JsDataDescriptor desc::JsDataDescriptor)
      (when (isa? current JsValueDescriptor)
	 (same-data-descriptor? current desc)))
   
   (define (same-descriptor? current desc)
      (when (same-property-descriptor? current desc)
	 (cond
	    ((isa? current JsAccessorDescriptor)
	     (and (isa? desc JsAccessorDescriptor)
		  (same-accessor-descriptor? current desc)))
	    ((isa? desc JsValueDescriptor)
	     (and (same-value-descriptor? current desc)
		  (with-access::JsValueDescriptor current ((cvalue value))
		     (with-access::JsValueDescriptor desc ((dvalue value))
			(equal? cvalue dvalue)))))
	    ((isa? desc JsDataDescriptor)
	     (and (same-data-descriptor? current desc)
		  (with-access::JsValueDescriptor current (value)
		     (eq? value (js-undefined)))))
	    ((isa? desc JsAccessorDescriptor)
	     #f)
	    ((isa? desc JsWrapperDescriptor)
	     #f)
	    (else
	     (with-access::JsValueDescriptor current (value writable)
		(and (eq? value (js-undefined)) #f))))))

   (define (define-own-property-extend-mapped o name desc)
      (cond
	 ((isa? desc JsValueDescriptor)
	  ;; 4.a
	  (with-access::JsValueDescriptor desc
		(value writable enumerable configurable)
	     (js-put-jsobject! o name value #f %this
		:extend #t :override #t 
		:writable (boolify writable)
		:enumerable (boolify enumerable)
		:configurable (boolify configurable))))
	  ((isa? desc JsDataDescriptor)
	   (with-access::JsDataDescriptor desc
		 (writable enumerable configurable)
	      (js-put-jsobject! o name (js-undefined) #f %this
		 :extend #t :override #t 
		 :writable (boolify writable)
		 :enumerable (boolify enumerable)
		 :configurable (boolify configurable))))
	  (else
	   (js-object-unmap! o)
	   (define-own-property-extend-unmapped o name desc))))

   (define (define-own-property-extend-unmapped o name desc)
      (js-invalidate-pmap-pcaches! %this
	 "js-define-own-property%, one accessor" name)
      (let ((ndesc (cond
		      ((isa? desc JsValueDescriptor)
		       ;; 4.a
		       (with-access::JsValueDescriptor desc
			     (writable enumerable configurable)
			  (unless (boolean? writable)
			     (set! writable #f))
			  (unless (boolean? enumerable)
			     (set! enumerable #f))
			  (unless (boolean? configurable)
			     (set! configurable #f)))
		       desc)
		      ((isa? desc JsDataDescriptor)
		       (with-access::JsDataDescriptor desc
			     (enumerable configurable name writable)
			  (instantiate::JsValueDescriptor
			     (name name)
			     (writable (boolify writable))
			     (enumerable (boolify enumerable))
			     (configurable (boolify configurable))
			     (value (js-undefined)))))
		      ;; 4.b
		      ((isa? desc JsAccessorDescriptor)
		       (with-access::JsAccessorDescriptor desc
			     (enumerable configurable get set name)
			  (unless (boolean? enumerable)
			     (set! enumerable #f))
			  (unless (boolean? configurable)
			     (set! configurable #f))
			  (unless get
			     (set! get (js-undefined)))
			  (unless set
			     (set! set (js-undefined))))
		       desc)
		      (else
		       ;; 4.a
		       (with-access::JsPropertyDescriptor desc
			     (enumerable configurable name)
			  (instantiate::JsValueDescriptor
			     (name name)
			     (writable #f)
			     (enumerable (boolify enumerable))
			     (configurable (boolify configurable))
			     (value (js-undefined))))))))
	 (with-access::JsObject o (elements)
	    (set! elements (vector-extend elements ndesc)))
	 #t))

   (when (and (js-jsstring? name) (js-jsstring->number name))
      (js-object-mode-hasnumeralprop-set! o #t))

   ;; 1 & 2
   (if (not (js-has-own-property o name %this))
       (cond
	  ((not (js-object-mode-extensible? o))
	   ;; 3
	   (reject (format "\"~a\" not extensible" (js-typeof o %this))))
	  ((js-object-mapped? o)
	   (define-own-property-extend-mapped o name desc))
	  (else
	   (define-own-property-extend-unmapped o name desc)))
       (let ((current (js-get-own-property o name %this)))
	  (cond
	     ((same-descriptor? current desc)
	      #t)
	     ((equal? current desc)
	      ;; 5 & 6
	      (error "define-property" "equal but not same" name)
	      #t)
	     (else
	      (when (eq? (configurable current) #f)
		 ;; 7
		 (cond
		    ((eq? (configurable desc) #t)
		     ;; 7.a
		     (reject
			(format "\"~a.~a\" configurability mismatch"
			   (js-typeof o %this) name)))
		    ((and (boolean? (enumerable desc))
			  (not (eq? (enumerable current) (enumerable desc))))
		     ;; 7.b
		     (reject
			(format "\"~a.~a\" enumerability mismatch"
			   (js-typeof o %this) name)))))
	      (unless rejected
		 (js-object-unmap! o)
		 (set! current (js-get-own-property o name %this))
		 (cond
		    ((js-is-generic-descriptor? desc)
		     ;; 8
		     (propagate-property-descriptor! current desc))
		    ((not (eq? (isa? current JsDataDescriptor)
			     (isa? desc JsDataDescriptor)))
		     ;; 9
		     (cond
			((eq? (configurable current) #f)
			 ;; 9.a
			 (reject
			    (format "\"~a.~a\" configurability mismatch"
			       (js-typeof o %this) name)))
			((isa? current JsDataDescriptor)
			 ;; 9.b
			 (when (isa? desc JsAccessorDescriptor)
			    (with-access::JsAccessorDescriptor desc (set get)
			       (unless (js-function? get)
				  (set! get (js-undefined)))
			       (unless (js-function? set)
				  (set! set (js-undefined)))))
			 (propagate-property-descriptor2! desc current)
			 (replace-property-descriptor! current desc))
			(else
			 ;; 9.c
			 (propagate-property-descriptor2! desc current)
			 (replace-property-descriptor! current desc))))
		    ((and (isa? current JsDataDescriptor)
			  (isa? desc JsDataDescriptor))
		     ;; 10
		     (if (eq? (configurable current) #f)
			 (cond
			    ((and (eq? (writable current) #f)
				  (eq? (writable desc) #t))
			     ;; 10.a.i
			     (reject
				(format "\"~a.~a\" read-only"
				   (js-typeof o %this) name)))
			    ((eq? (writable current) #f)
			     ;; 10.a.ii
			     (if (and (isa? desc JsValueDescriptor)
				      (isa? current JsValueDescriptor)
				      (not (same-value (value desc) (value current))))
				 (reject
				    (format "\"~a.~a\" value mismatch"
				       (js-typeof o %this) name))
				 #t))
			    (else
			     (propagate-data-descriptor! current desc)))
			 (propagate-data-descriptor! current desc)))
		    ((and (isa? current JsAccessorDescriptor)
			  (isa? desc JsAccessorDescriptor))
		     ;; 11
		     (if (eq? (configurable current) #f)
			 (cond
			    ((and (set desc) (not (equal? (set current) (set desc))))
			     (reject
				(format "\"~a.~a\" setter mismatch"
				   (js-typeof o %this) name)))
			    ((and (get desc)
				  (not (equal? (get current) (get desc))))
			     (reject
				(format "\"~a.~a\" getter mismatch"
				   (js-typeof o %this) name)))
			    (else
			     (propagate-accessor-descriptor! current desc)))
			 (propagate-accessor-descriptor! current desc)))
		    (else

		     (js-invalidate-pmap-pcaches! %this "js-define-own-property%, one accessor" name)
		     ;; 12 & 13
		     (propagate-property-descriptor! current desc)))))))))

;*---------------------------------------------------------------------*/
;*    js-getprototypeof ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (js-getprototypeof o %this::JsGlobalObject msg::obj)
   (js-getprototypeof (js-cast-object o %this msg) %this msg))

;*---------------------------------------------------------------------*/
;*    js-getprototypeof ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-getprototypeof o::JsObject %this::JsGlobalObject msg::obj)
   (js-object-proto o))

;*---------------------------------------------------------------------*/
;*    js-setprototypeof ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (js-setprototypeof o v %this::JsGlobalObject msg)
   (js-setprototypeof (js-cast-object o %this msg) v %this msg))

;*---------------------------------------------------------------------*/
;*    js-setprototypeof ::JsObject ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-setprototypeof o::JsObject v %this msg)
   (let ((v (if (eq? v '()) v (js-cast-object v %this msg))))
      (if (not (js-object-mode-extensible? o))
	  (js-raise-type-error %this 
	     "Prototype of non-extensible object mutated" v)
	  (with-access::JsObject o (cmap)
	     (unless (eq? (js-object-proto o) v)
		(js-invalidate-pmap-pcaches! %this "js-setprototypeof" "__proto__")
		(unless (eq? cmap (js-not-a-cmap))
		   (with-access::JsConstructMap cmap (parent single)
		      
		      (if single
			  (set! cmap
			     (duplicate::JsConstructMap cmap
				(sibling #f)
				(%id (gencmapid))))
			  (set! cmap
			     (cmap-find-sibling
				(cmap-next-proto-cmap %this cmap
				   (js-object-proto o) v)
				(js-object-inline-elements? o))))))
		(js-object-proto-set! o v))
	     o))))

;*---------------------------------------------------------------------*/
;*    js-replace-own-property! ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (js-replace-own-property! o::JsObject old new)

   (define (descriptor->flags desc::JsPropertyDescriptor)
      (cond
	 ((isa? desc JsDataDescriptor)
	  (with-access::JsDataDescriptor desc (writable enumerable configurable)
	     (property-flags writable enumerable configurable #f)))
	 ((isa? desc JsDataDescriptor)
	  (error "js-replace-own-property!" "not a dataproperty" new))
	 (else
	  (with-access::JsPropertyDescriptor desc (enumerable configurable)
	     (property-flags #t enumerable configurable #f)))))

   (define (replace-vector! vec old new)
      (let loop ((i (-fx (vector-length vec) 1)))
	 (cond
	    ((=fx i -1)
	     #f)
	    ((eq? (vector-ref vec i) old)
	     (vector-set! vec i new)
	     #t)
	    (else
	     (loop (-fx i 1))))))

   (define (find-transition cmap name)
      (with-access::JsConstructMap cmap (transitions)
	 (find (lambda (t) (eq? (transition-name t) name)) transitions)))

   (unless (isa? new JsValueDescriptor)
      ;; to be improved
      (js-object-unmap! o))

   (if (js-object-mapped? o)
       ;; update the mapped object
       (with-access::JsPropertyDescriptor new (name)
	  (with-access::JsObject o (cmap)
	     (let loop ((cmap cmap))
		(with-access::JsConstructMap cmap (transitions parent lock)
		   (synchronize lock
		      (let ((tr (find-transition cmap name)))
			 (if tr
			     (with-access::JsValueDescriptor new (value)
				(let ((flags (descriptor->flags new)))
				   (link-cmap! cmap (transition-nextmap tr)
				      name value flags)))
			     (loop parent))))))))
       ;; update the unmapped object
       (with-access::JsObject o (elements)
	  (unless (replace-vector! elements old new)
	     (js-replace-own-property! (js-object-proto o) old new)))))

;*---------------------------------------------------------------------*/
;*    js-seal ::JsObject ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (js-seal o::JsObject obj::obj))

;*---------------------------------------------------------------------*/
;*    js-freeze ::JsObject ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (js-freeze o::JsObject obj::obj))

;*---------------------------------------------------------------------*/
;*    js-prevent-extensions ::JsObject ...                             */
;*---------------------------------------------------------------------*/
(define (js-prevent-extensions o::JsObject)
   (js-object-mode-extensible-set! o #f))

;*---------------------------------------------------------------------*/
;*    js-for-in ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.4       */
;*    http://www.ecma-international.org/ecma-262/6.0/#sec-13.7.5       */
;*---------------------------------------------------------------------*/
(define-generic (js-for-in obj proc %this)
   (cond
      ((or (eq? obj (js-undefined)) (eq? obj (js-null)))
       (js-undefined))
      ((js-jsstring? obj)
       (js-jsstring-for-in obj proc %this))
      (else
       (js-for-in (js-cast-object obj %this "for") proc %this))))

;*---------------------------------------------------------------------*/
;*    js-for-in-prototype ...                                          */
;*    -------------------------------------------------------------    */
;*    JS-FOR-IN is the "surface" function, i.e., called on the         */
;*    object itself. JS-FOR-IN-PROTOTYPE is the "deep" function,       */
;*    i.e., called on the __proto__ of the initial object.             */
;*---------------------------------------------------------------------*/
(define-generic (js-for-in-prototype obj::JsObject owner::JsObject proc %this)

   (define (in-mapped-property prop)
      (when prop
	 (let ((name (prop-name prop)))
	    (when (js-jsstring? name)
	       (when (flags-enumerable? (prop-flags prop))
		  (unless (js-has-upto-property owner obj name %this)
		     (proc name %this)))))))
   
   (define (vfor-in vecname)
      (let ((len (vector-length vecname)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (in-mapped-property (vector-ref vecname i))
	       (loop (+fx i 1))))))
   
   (define (in-property p)
      (when (isa? p JsPropertyDescriptor)
	 (with-access::JsPropertyDescriptor p (name enumerable)
	    (when (js-jsstring? name)
	       (when (eq? enumerable #t)
		  (unless (js-has-upto-property owner obj name %this)
		     (proc name %this)))))))
   
   (with-access::JsObject obj (cmap elements)
      (when (js-object-mode-enumerable? obj)
	 (if (js-object-mapped? obj)
	     (with-access::JsConstructMap cmap (props)
		(vfor-in props))
	     (with-access::JsObject obj (elements)
		(vector-for-each in-property elements))))
      (let ((__proto__ (js-object-proto obj)))
	 (when (js-object? __proto__)
	    (js-for-in-prototype __proto__ owner proc %this)))))
   
;*---------------------------------------------------------------------*/
;*    js-for-in ::object ...                                           */
;*---------------------------------------------------------------------*/
(define-method (js-for-in obj::object proc %this)
   (let ((jsobj (js-toobject %this obj)))
      (if (eq? obj jsobj)
	  (let ((fields (class-all-fields (object-class obj))))
	     (let loop ((i 0))
		(when (<fx i (vector-length fields))
		   (proc
		      (js-string->jsstring
			 (symbol->string!
			    (class-field-name (vector-ref fields i))))
		      %this)
		   (loop (+fx i 1)))))
	  (js-for-in jsobj proc %this))))

;*---------------------------------------------------------------------*/
;*    js-for-of ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/6.0/#sec-13.7.5       */
;*---------------------------------------------------------------------*/
(define-generic (js-for-of obj proc close %this)
   (if (js-jsstring? obj)
       (js-jsstring-for-of obj proc %this)
       (with-access::JsGlobalObject %this (js-symbol-iterator)
	  (let ((fun (js-get obj js-symbol-iterator %this)))
	     (when (js-function? fun)
		(js-for-of-iterator (js-call0 %this fun obj)
		   obj proc close %this))))))

;*---------------------------------------------------------------------*/
;*    js-for-of-iterator ...                                           */
;*---------------------------------------------------------------------*/
(define (js-for-of-iterator iterator obj proc close %this)
   
   (define (for next)
      (let loop ()
	 (let ((n (js-call0 %this next iterator)))
	    (unless (eq? (js-get n (& "done") %this) #t)
	       (proc (js-get n (& "value") %this) %this)
	       (loop))))
      #t)
   
   (let ((next (js-get iterator (& "next") %this))
	 (exn #t))
      (if (js-function? next)
	  (if close
	      (unwind-protect
		 (begin
		    (for next)
		    (set! exn #f))
		 (when exn
		    (let ((return (js-get iterator (& "return") %this)))
		       (when (js-function? return)
			  (js-call0 %this return iterator)))))
	      (for next)))))
			 
;*---------------------------------------------------------------------*/
;*    js-call/cache ...                                                */
;*    -------------------------------------------------------------    */
;*    Overriden by a property_expd.sch macro.                          */
;*---------------------------------------------------------------------*/
(define (js-call/cache %this cache obj this . args)
   (let ((largs (length args)))
      (with-access::JsPropertyCache cache (owner method cmap)
	 (cond
	    ((eq? owner obj)
	     (apply method this args))
	    ((and (js-function? obj)
		  (with-access::JsFunction obj (len arity)
		     (and (>=fx arity 0) (=fx len largs))))
	     (with-access::JsFunction obj (procedure)
		(set! owner obj)
		(set! method procedure)
		(apply procedure this args)))
	    (else
	     (js-apply %this obj this args))))))

;*---------------------------------------------------------------------*/
;*    js-call/cache-miss ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (gen-call/cache-miss %this cache obj this . args)
   
   (define (gen-call/cache-miss-generic largs)
      `(with-access::JsPropertyCache ,cache (owner method cmap)
	  (cond
	     ((and (js-function? ,obj)
		   (with-access::JsFunction ,obj (procedure arity)
		      (and (>=fx arity 0)
			   (correct-arity? procedure ,(+fx largs 1)))))
	      (with-access::JsFunction ,obj (procedure)
		 (set! owner ,obj)
		 (set! method procedure)
		 (procedure ,this ,@args)))
	     ((js-proxy? ,obj)
	      ,(let ((call (symbol-append 'js-call-proxy/cache-miss
			      (string->symbol (integer->string largs)))))
		  `(,call ,%this ,obj ,this ,@args)))
	     (else
	      ,(let ((call (symbol-append 'js-call
			      (string->symbol (integer->string largs)))))
		  `(,call ,%this ,obj ,this ,@args))))))
   
   (define (gen-call/cache-miss-fast largs)
      `(with-access::JsPropertyCache ,cache (owner method cmap)
	  (cond
	     ((not (object? ,obj))
	      ,(let ((call (symbol-append 'js-call
			      (string->symbol (integer->string largs)))))
		  `(,call ,%this ,obj ,this ,@args)))
	     ((and (eq? (object-class ,obj)
		      ,(symbol-append 'JsFunction
			  (string->symbol (integer->string (+fx largs 1))))))
	      (with-access::JsFunction ,obj (procedure)
		 (set! owner ,obj)
		 (set! method procedure)
		 (procedure ,this ,@args)))
	     ((eq? (object-class ,obj) JsProxy)
	      ,(let ((call (symbol-append 'js-call-proxy/cache-miss
			      (string->symbol (integer->string largs)))))
		  `(,call ,%this ,obj ,this ,@args)))
	     ((and (js-function? ,obj)
		   (with-access::JsFunction ,obj (procedure arity)
		      (and (>=fx arity 0)
			   (correct-arity? procedure ,(+fx largs 1)))))
	      (with-access::JsFunction ,obj (procedure)
		 (set! owner ,obj)
		 (set! method procedure)
		 (procedure ,this ,@args)))
	     (else
	      ,(let ((call (symbol-append 'js-call
			      (string->symbol (integer->string largs)))))
		  `(,call ,%this ,obj ,this ,@args))))))
   
   (let ((largs (length args)))
      (if (>=fx largs 4)
	  (gen-call/cache-miss-generic largs)
	  (gen-call/cache-miss-fast largs))))

(define (js-call/cache-miss0 %this cache obj this)
   (gen-call/cache-miss %this cache obj this))

(define (js-call/cache-miss1 %this cache obj this a0)
   (gen-call/cache-miss %this cache obj this a0))

(define (js-call/cache-miss2 %this cache obj this a0 a1)
   (gen-call/cache-miss %this cache obj this a0 a1))

(define (js-call/cache-miss3 %this cache obj this a0 a1 a2)
   (gen-call/cache-miss %this cache obj this a0 a1 a2))

(define (js-call/cache-miss4 %this cache obj this a0 a1 a2 a3)
   (gen-call/cache-miss %this cache obj this a0 a1 a2 a3))

(define (js-call/cache-miss5 %this cache obj this a0 a1 a2 a3 a4)
   (gen-call/cache-miss %this cache obj this a0 a1 a2 a3 a4))

(define (js-call/cache-miss6 %this cache obj this a0 a1 a2 a3 a4 a5)
   (gen-call/cache-miss %this cache obj this a0 a1 a2 a3 a4 a5))

(define (js-call/cache-miss7 %this cache obj this a0 a1 a2 a3 a4 a5 a6)
   (gen-call/cache-miss %this cache obj this a0 a1 a2 a3 a4 a5 a6))

(define (js-call/cache-miss8 %this cache obj this a0 a1 a2 a3 a4 a5 a6 a7)
   (gen-call/cache-miss %this cache obj this a0 a1 a2 a3 a4 a5 a6 a7))

;*---------------------------------------------------------------------*/
;*    js-method-call-name/cache ...                                    */
;*---------------------------------------------------------------------*/
(define (js-method-call-name/cache %this::JsGlobalObject obj::obj name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   point::long ccspecs::pair-nil ocspecs
	   . args)
   (if (js-object? obj)
       (apply js-method-jsobject-call-name/cache %this obj name
	  ccache ocache point ccspecs ocspecs args)
       (let ((o (js-toobject %this obj)))
	  (js-apply %this (js-get/name-cache o name %this) o args))))

;*---------------------------------------------------------------------*/
;*    js-method-jsobject-call-name/cache ...                           */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-method-jsobject-call-name/cache %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   point::long ccspecs::pair-nil ocspecs
	   . args)
   (with-access::JsObject obj ((omap cmap) __proto__)
      (with-access::JsPropertyCache ccache (cmap pmap method vindex)
	 (cond
	    ((eq? pmap omap)
	     ;; in cache
	     (apply method obj args))
	    ((not omap)
	     ;; uncachable
	     (let ((f (js-get/name-cache obj name %this)))
		(js-apply %this f obj args)))
	    (else
	     ;; cache miss
	     (with-access::JsConstructMap omap (vlen vtable %id)
		(if (and (<fx vindex vlen)
			 (procedure? (vector-ref vtable vindex)))
		    (let ((m (vector-ref vtable vindex)))
		       (apply m obj args))
		    (js-method-jsobject-call/cache-miss %this obj name args
		       ccache ocache point ccspecs ocspecs))))))))

;*---------------------------------------------------------------------*/
;*    js-method-jsobject-call/cache-miss ...                           */
;*    -------------------------------------------------------------    */
;*    This function is called on a true cache miss, i.e., this call    */
;*    has already been filled with another method.                     */
;*---------------------------------------------------------------------*/
(define (js-method-jsobject-call/cache-miss %this::JsGlobalObject
	   o::JsObject name::obj args::pair-nil
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   point::long ccspecs::pair-nil ocspecs::pair-nil)
   
   (with-access::JsPropertyCache ccache (cntmiss (cname name) (cpoint point) usage)
      (set! cntmiss (+u32 #u32:1 cntmiss))
      (set! cname name)
      (set! cpoint point)
      (set! usage 'call))

   (with-access::JsPropertyCache ccache (pmap vindex method cntmiss)
      (when (and (procedure? method)
		 (isa? pmap JsConstructMap)
		 (=fx (procedure-arity method) (+fx 1 (length args))))
	 (when (=fx vindex (js-not-a-index))
	    (set! vindex (js-get-vindex %this)))
	 (js-cmap-vtable-add! pmap vindex method ccache))
      (js-method-jsobject-call/cache-fill %this o name args
	 ccache ocache point ccspecs ocspecs)))

;*---------------------------------------------------------------------*/
;*    js-method-jsobject-call/cache-fill ...                           */
;*    -------------------------------------------------------------    */
;*    This function is called when a ccache has to be filled, i.e.,    */
;*    on the first call.                                               */
;*    -------------------------------------------------------------    */
;*    .call and .apply (not yet for the former) are handled by this    */
;*    function.                                                        */
;*---------------------------------------------------------------------*/
(define (js-method-jsobject-call/cache-fill %this::JsGlobalObject
	   o::JsObject name::obj args::pair-nil
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   point::long ccspecs::pair-nil ospecs::pair-nil)
   
   (define (jsapply method)
      (if (procedure? method)
	  (apply method args)
	  (apply js-calln %this method o args)))

   (define (funval obj el-or-desc)
      (let loop ((el-or-desc el-or-desc))
	 (cond
	    ((isa? el-or-desc JsPropertyDescriptor)
	     (loop (js-property-value o obj name el-or-desc %this)))
	    (else
	     el-or-desc))))
   
   (define (method->procedure f)
      (case (procedure-arity f)
	 ((1)
	  (lambda (_ this) (f this)))
	 ((2)
	  (lambda (_ this a0) (f this a0)))
	 ((3)
	  (lambda (_ this a0 a1) (f this a0 a1)))
	 ((4)
	  (lambda (_ this a0 a1 a2) (f this a0 a1 a2)))
	 ((5)
	  (lambda (_ this a0 a1 a2 a3) (f this a0 a1 a2 a3)))
	 ((6)
	  (lambda (_ this a0 a1 a2 a3 a4) (f this a0 a1 a2 a3 a4)))
	 ((7)
	  (lambda (_ this a0 a1 a2 a3 a4 a5) (f this a0 a1 a2 a3 a4 a5)))
	 ((8)
	  (lambda (_ this a0 a1 a2 a3 a4 a5 a6) (f this a0 a1 a2 a3 a4 a5 a6)))
	 (else
	  (lambda (_ . args) (apply f args)))))

   (define (procedureN procedure largs)
      (case (procedure-arity procedure)
	 ((1)
	  (case largs
	     ((0) procedure)
	     ((1) (lambda (this a0) (procedure this)))
	     ((2) (lambda (this a0 a1) (procedure this)))
	     ((3) (lambda (this a0 a1 a2) (procedure this)))
	     ((4) (lambda (this a0 a1 a2 a3) (procedure this)))
	     ((5) (lambda (this a0 a1 a2 a3 a4) (procedure this)))
	     ((6) (lambda (this a0 a1 a2 a3 a4 a5) (procedure this)))
	     ((7) (lambda (this a0 a1 a2 a3 a4 a5 a6) (procedure this)))
	     (else (lambda (this a0 a1 a2 a3 a4 a5 a6 . _) (procedure this)))))
	 ((2)
	  (case largs
	     ((0) (lambda (this) (procedure this (js-undefined))))
	     ((1) procedure)
	     ((2) (lambda (this a0 a1) (procedure this a0)))
	     ((3) (lambda (this a0 a1 a2) (procedure this a0)))
	     ((4) (lambda (this a0 a1 a2 a3) (procedure this a0)))
	     ((5) (lambda (this a0 a1 a2 a3 a4) (procedure this a0)))
	     ((6) (lambda (this a0 a1 a2 a3 a4 a5) (procedure this a0)))
	     ((7) (lambda (this a0 a1 a2 a3 a4 a5 a6) (procedure this a0)))
	     (else (lambda (this a0 a1 a2 a3 a4 a5 a6 . _) (procedure this a0)))))
	 ((3)
	  (case largs
	     ((0) (lambda (this) (procedure this (js-undefined) (js-undefined))))
	     ((1) (lambda (this a0) (procedure this a0 (js-undefined))))
	     ((2) procedure)
	     ((3) (lambda (this a0 a1 a2) (procedure this a0 a1)))
	     ((4) (lambda (this a0 a1 a2 a3) (procedure this a0 a1)))
	     ((5) (lambda (this a0 a1 a2 a3 a4) (procedure this a0 a1)))
	     ((6) (lambda (this a0 a1 a2 a3 a4 a5) (procedure this a0 a1)))
	     ((7) (lambda (this a0 a1 a2 a3 a4 a5 a6) (procedure this a0 a1)))
	     (else (lambda (this a0 a1 a2 a3 a4 a5 a6 . _) (procedure this a0 a1)))))
	 ((4)
	  (case largs
	     ((0) (lambda (this) (procedure this (js-undefined) (js-undefined) (js-undefined))))
	     ((1) (lambda (this a0) (procedure this a0 (js-undefined) (js-undefined))))
	     ((2) (lambda (this a0 a1) (procedure this a0 a1 (js-undefined))))
	     ((3) procedure)
	     ((4) (lambda (this a0 a1 a2 a3) (procedure this a0 a1)))
	     ((5) (lambda (this a0 a1 a2 a3 a4) (procedure this a0 a1)))
	     ((6) (lambda (this a0 a1 a2 a3 a4 a5) (procedure this a0 a1)))
	     ((7) (lambda (this a0 a1 a2 a3 a4 a5 a6) (procedure this a0 a1)))
	     (else (lambda (this a0 a1 a2 a3 a4 a5 a6 . _) (procedure this a0 a1)))))
	 (else
	  #f)))

   (js-profile-log-method name point)

   (let ((n (js-toname name %this)))
      (let loop ((obj o))
	 (jsobject-find obj o n
	    ;; map search
	    (lambda (obj i)
	       (with-access::JsObject o ((omap cmap) __proto__)
		  (with-access::JsObject obj ((wmap cmap) elements)
		     (with-access::JsConstructMap wmap (methods %id)
			(let ((el-or-desc (vector-ref elements i)))
			   (cond
			      ((or (isa? el-or-desc JsAccessorDescriptor)
				   (isa? el-or-desc JsWrapperDescriptor))
			       (with-access::JsPropertyCache ccache (pmap emap cmap function)
				  (set! function #f)
				  (set! pmap #t)
				  (set! emap #t)
				  (set! cmap #t))
			       (jsapply (js-property-value o obj name el-or-desc %this)))
			      ((js-function? (vector-ref methods i))
			       (let ((f (funval obj el-or-desc)))
				  (cond
				     ((procedure? f)
				      (error "js-method-jsobject-call/cache-fill" "should not be here" f))
				     ((js-function? f)
				      (with-access::JsFunction f (len method arity)
					 (cond
					    ((<fx arity 0)
					     ;; varargs functions, currently not cached...
					     (with-access::JsPropertyCache ccache (pmap emap cmap)
						(set! pmap #t)
						(set! emap #t)
						(set! cmap #t)))
					    ((=fx (procedure-arity method) (+fx 1 (length args)))
					     (with-access::JsPropertyCache ccache (pmap emap cmap index (cmethod method) function)
						;; correct arity, put in cache
						(js-validate-pmap-pcache! ccache)
						(set! pmap omap)
						(set! emap #t)
						(set! cmap #f)
						(set! index i)
						(set! function f)
						(procedure-attr-set! method f)
						(set! cmethod method)))
					    ((procedureN method (length args))
					     =>
					     (lambda (procedure)
						(with-access::JsPropertyCache ccache (pmap emap cmap index (cmethod method) function)
						   ;; correct arity, put in cache
						   (js-validate-pmap-pcache! ccache)
						   (set! pmap omap)
						   (set! emap #t)
						   (set! cmap #f)
						   (set! index i)
						   (set! function f)
						   (procedure-attr-set! procedure f)
						   (set! cmethod procedure))))
					    (else
					     ;; arity missmatch, never cache
					     (with-access::JsPropertyCache ccache (pmap emap cmap)
						(set! pmap #t)
						(set! emap #t)
						(set! cmap #t)))))))
				  (jsapply f)))
			      ((eq? obj o)
			       (with-access::JsPropertyCache ccache (pmap cmap emap index)
				  ;; invalidate the call cache and update the
				  ;; object cache
				  (set! cmap omap)
				  (set! index i)
				  (set! pmap #t)
				  (set! emap #t)
				  (jsapply (funval obj el-or-desc))))
			      (else
			       (with-access::JsPropertyCache ccache (pmap cmap emap)
				  ;; invalidate the call cache and update the
				  ;; object cache
				  (set! cmap #t)
				  (set! pmap #t)
				  (set! emap #t)
				  (jsapply (funval obj el-or-desc))))))))))
	    ;; property search
	    (lambda (obj v i)
	       (with-access::JsPropertyCache ccache (cmap emap pmap)
		  (set! pmap #t)
		  (set! emap #t)
		  (set! cmap #t)
		  (jsapply (js-property-value o obj name v %this))))
	    ;; not found
	    (lambda (o)
	       (js-raise-type-error %this "call: not a function ~s"
		  (js-undefined)))
	    ;; loop
	    loop))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
