;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/property.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 25 07:05:26 2013                          */
;*    Last change :  Tue Apr 18 08:23:54 2023 (serrano)                */
;*    Copyright   :  2013-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript property handling (getting, setting, defining and     */
;*    deleting).                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_property

   (library hop)
   
   ;; (option  (bigloo-compiler-debug-set! 0))
   
   (include "stringliteral.sch"
	    "property.sch"
	    "types.sch"
	    "function.sch")

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
	   __hopscript_number
	   __hopscript_bigint
	   __hopscript_proxy)

   (use    __hopscript_array
	   __hopscript_stringliteral
	   __hopscript_names
	   __hopscript_arguments
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
	   (inline js-pcache-ref ::obj ::int)
	   (inline js-pcache-imap ::JsPropertyCache)
	   (inline js-pcache-emap ::JsPropertyCache)
	   (inline js-pcache-cmap ::JsPropertyCache)
	   (inline js-pcache-amap ::JsPropertyCache)
	   
	   (inline property-name::JsStringLiteral ::struct)

	   (merge-cmap!::JsConstructMap ::JsConstructMap ::JsConstructMap)
	   (js-names->cmap::JsConstructMap ::vector #!key (writable #f) (enumerable #t) (configurable #t) (accessor #f) (inline #f))
	   (js-strings->cmap::JsConstructMap ::vector)
	   (js-object-literal-init! ::JsObject)
	   (js-object-literal-spread-assign! ::JsObject ::obj ::JsGlobalObject)

	   (js-object-unmap! ::JsObject)
	   (js-object-unhash! ::JsObject)

	   (js-property-descriptor ::JsGlobalObject isvalue::bool
	      #!key writable enumerable configurable value get set)
	   
	   (inline js-is-accessor-descriptor?::bool obj)
	   (inline js-is-data-descriptor?::bool obj)
	   (inline js-is-generic-descriptor?::bool obj)
	   (js-from-property-descriptor ::JsGlobalObject propname desc ::obj)
	   (js-to-property-descriptor ::JsGlobalObject desc ::obj)
	   (inline js-property-amap-value ::obj ::JsObject ::obj ::JsPropertyDescriptor ::JsGlobalObject)
	   (inline js-property-amap-value-set! ::obj ::JsObject ::obj ::JsPropertyDescriptor ::obj ::JsGlobalObject)
	   (js-property-value ::obj ::JsObject ::obj ::JsPropertyDescriptor ::JsGlobalObject)
	   (js-property-value-set! ::obj ::JsObject ::obj ::JsPropertyDescriptor ::obj ::JsGlobalObject)
	   
	   (js-object-cmap-push! ::JsObject ::long ::obj ::JsConstructMap)
	   (js-object-vtable-push! ::JsObject ::long ::obj ::JsConstructMap)
	   
	   (generic js-properties-names::pair-nil ::obj ::bool ::JsGlobalObject)
	   (generic js-properties-name::vector ::obj ::bool ::JsGlobalObject)
	   (generic js-properties-symbol::vector ::obj ::JsGlobalObject)
	   
	   (js-in?::bool ::obj ::obj ::JsGlobalObject)
	   (js-in?/debug::bool ::obj ::obj ::JsGlobalObject loc)

	   (generic js-has-property::bool ::obj ::obj ::JsGlobalObject)
	   (generic js-has-own-property::bool ::obj ::obj ::JsGlobalObject)
	   (js-has-own-property-jsobject::bool ::JsObject ::obj ::JsGlobalObject)
	   (generic js-get-own-property ::obj ::obj ::JsGlobalObject)
	   (generic js-get-own-property-descriptor ::obj ::obj ::JsGlobalObject)
	   
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
	   (js-put-jsobject/cache! o::JsObject prop v::obj throw::bool %this
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
	   
	   (js-get-vindex::long ::JsGlobalObject)
	   (inline js-call-with-stack-list ::pair-nil ::procedure))

   (option (register-srfi! 'open-string-hashtable))
   
   (cond-expand
      ((config have-c99-stack-alloc #t)
       (pragma
	  (js-for-in (args-noescape))
	  (js-for-of (args-noescape))))))

;*---------------------------------------------------------------------*/
;*    debug-pmap ...                                                   */
;*---------------------------------------------------------------------*/
(define debug-pmap
   (let ((env (getenv "HOPTRACE")))
      (cond
	 ((not (string? env)) #f)
	 ((string-contains env "hopscript:pmap") #t)
	 (else #f))))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    prop-hashtable-weak ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (prop-hashtable-weak)
   (cond-expand
      (open-string-hashtable 'open-string)
      (string-hashtable 'string)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    prop-hashtable-get ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (prop-hashtable-get table key)
   (cond-expand
      (open-string-hashtable (open-string-hashtable-get table key))
      (string-hashtable (string-hashtable-get table key))
      (else (hashtable-get table key))))

;*---------------------------------------------------------------------*/
;*    prop-hashtable-put! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (prop-hashtable-put! table key val)
   (cond-expand
      (open-string-hashtable (open-string-hashtable-put! table key val))
      (string-hashtable (string-hashtable-put! table key val))
      (else (hashtable-put! table key val))))

;*---------------------------------------------------------------------*/
;*    inline thresholds ...                                            */
;*---------------------------------------------------------------------*/
(define (vtable-threshold) #u32:200)
(define (vtable-max-threshold) #u32:300)
(define (method-invalidation-threshold) 8)

(define (hash-object-threshold) 192)
(define (hash-object-name-threshold) #u32:24)

(define (ctor-max-constrsize) 4096)

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
      (with-access::JsPropertyCache pcache (pmap emap amap nmap xmap)
	 (when (object? pmap) (reset-cmap-vtable! pmap reason who))
	 (when (object? xmap) (reset-cmap-vtable! xmap reason who))
	 (set! pmap (js-not-a-pmap))
	 (set! nmap (js-not-a-pmap))
	 (set! xmap (js-not-a-pmap))
	 (set! emap (js-not-a-pmap))
	 ;; amap must be invalidated too as amap may point to another object
	 (set! amap (js-not-a-pmap))
	 ))

   (when js-pmap-valid
      (synchronize js-cache-table-lock
	 (when debug-pmap
	    (tprint "--- invalidate " reason " [" who "] pcache-table-len=" js-cache-index " !!!!!!!!!!!!!!!!!!!!!!!!!!!")
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
   (with-access::JsConstructMap cmap (methods transitions %id)
      (let ((m (vector-ref methods idx)))
	 (when m
	    (let ((r (not (eq? m #unspecified))))
	       (vector-set! methods idx #f)
	       (for-each (lambda (tr)
			    (let ((ncmap (transition-nextmap tr)))
			       (when (js-invalidate-cache-method! ncmap idx reason who)
				  (set! r #t))))
		  transitions)
	       r)))))

;*---------------------------------------------------------------------*/
;*    js-invalidate-cache-pmap-method! ...                             */
;*    -------------------------------------------------------------    */
;*    Method invalidation must be propagated to all the sub hidden     */
;*    classes (see bug nodejs/simple/test-stream2-readable-wrap.js).   */
;*---------------------------------------------------------------------*/
(define (js-invalidate-cache-pmap-method! %this::JsGlobalObject
	   cmap::JsConstructMap idx::long reason who)
   (when (js-invalidate-cache-method! cmap idx reason who)
      (js-invalidate-pmap-pcaches! %this reason who)))

;*---------------------------------------------------------------------*/
;*    js-init-property! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-init-property! %this)
   
   (define (make-cmap props)
      (js-make-jsconstructmap
	 :methods (make-vector (vector-length props))
	 :props props))

   (unless (vector? __js_strings)
      (set! __js_strings (&init!))
      (set! miss-object
	 (instantiateJsObject
	    (__proto__ (js-null))
	    (elements (vector (js-undefined)))))
      (with-access::JsGlobalObject %this (js-property-descriptor-value-cmap
					    js-property-descriptor-getter-cmap)
	 (set! js-property-descriptor-value-cmap
	    (make-cmap
	       `#(,(prop (& "enumerable") (property-flags #t #t #t #t #f))
		  ,(prop (& "configurable") (property-flags #t #t #t #t #f))
		  ,(prop (& "value") (property-flags #t #t #t #t #f))
		  ,(prop (& "writable") (property-flags #t #t #t #t #f)))))
	 (set! js-property-descriptor-getter-cmap
	    (make-cmap
	       `#(,(prop (& "enumerable") (property-flags #t #t #t #t #f))
		  ,(prop (& "configurable") (property-flags #t #t #t #t #f))
		  ,(prop (& "get") (property-flags #t #t #t #t #f))
		  ,(prop (& "set") (property-flags #t #t #t #t #f))))))))

;*---------------------------------------------------------------------*/
;*    js-debug-object-cmap-id ...                                      */
;*---------------------------------------------------------------------*/
(define (js-debug-object-cmap-id o)
   (with-access::JsObject o (cmap)
      (with-access::JsConstructMap cmap (%id)
	 %id)))

;*---------------------------------------------------------------------*/
;*    js-assert-object ...                                             */
;*---------------------------------------------------------------------*/
(define-expander js-assert-object
   (lambda (x e)
      (cond-expand
	 ((or devel debug)
	  #t)
	 (else
	  #unspecified))))

;*---------------------------------------------------------------------*/
;*    js-debug-prop ...                                                */
;*---------------------------------------------------------------------*/
(define (js-debug-prop prop)
   (let ((name (prop-name prop))
	 (flags (prop-flags prop)))
      (format "~a[~a]" name
	 (string-append
	    (if (flags-inline? flags) "I" "V")
	    (if (flags-writable? flags) "w" "r")
	    (if (flags-enumerable? flags) "e" "-")
	    (if (flags-configurable? flags) "c" "-")
	    (if (flags-accessor? flags) "a" "-")))))
	 
;*---------------------------------------------------------------------*/
;*    js-debug-object ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (js-debug-object obj #!optional (msg ""))
   (fprint (current-error-port) msg (typeof obj)))

;*---------------------------------------------------------------------*/
;*    js-debug-cmap-chain ...                                          */
;*---------------------------------------------------------------------*/
(define (js-debug-cmap-chain o::JsConstructMap)
   (let loop ((o o))
      (if (or (not o) (eq? o (js-not-a-pmap)))
	  '()
	  (with-access::JsConstructMap o (%id parent)
	     (if (eq? parent o)
		 '()
		 (cons %id (loop parent)))))))

;*---------------------------------------------------------------------*/
;*    js-debug-object ::JsObject ...                                   */
;*---------------------------------------------------------------------*/
(define-method (js-debug-object obj::JsObject #!optional (msg ""))
   (with-access::JsObject obj (cmap elements)
      (cond
	 ((js-object-mapped? obj)
	  (with-access::JsConstructMap cmap (%id props methods)
	     (fprint (current-error-port) "== " msg (typeof obj) " MAPPED"
		" Ilen=" (vector-length (js-object-inline-elements obj))
		" Vlen=" (vector-length (js-object-noinline-elements obj))
		" inline=" (js-object-mode-inline? obj)
		" plain=" (js-object-mode-plain? obj)
		" isproto=" (js-object-mode-isprotoof? obj)
		" ext=" (js-object-mode-extensible? obj)
		" met.vlen=" (vector-length methods)
		"\n   cmap.%id=" %id
		"\n   Iels="
		(vector-length (js-object-inline-elements obj))
		" " (vector-map
		       (lambda (v)
			  (if (js-object? v)
			      (typeof v)
			      v))
		       (js-object-inline-elements obj))
		"\n   Vels="
		(vector-length (js-object-noinline-elements obj))
		" " 
		(vector-map
		   (lambda (v)
		      (if (js-object? v)
			  (typeof v)
			  v))
		   (js-object-noinline-elements obj))
		"\n   cmap.props="
		(vector-length props)
		" " 
		(vector-map js-debug-prop props))))
	 ((js-object-hashed? obj)
	  (with-access::JsConstructMap cmap (%id props)
	     (fprint (current-error-port) "== " msg (typeof obj) " HASHED"
		" els.vlen=" (hashtable-size elements)
		"\n   prop.names="
		(hashtable-key-list elements)
		"\n   prop.values="
		(hashtable-map elements (lambda (k v) (typeof (cell-ref v)))))))
	 (else
	  (with-access::JsConstructMap cmap (%id props)
	     (fprint (current-error-port) "== " msg (typeof obj) " UNMAPPED"
		" els.vlen=" (vector-length elements)
		"\n   prop.names="
		(map (lambda (d)
			(if (isa? d JsPropertyDescriptor)
			    (with-access::JsPropertyDescriptor d (name)
			       name)
			    d))
		   (vector->list elements))
		"\n   els="
		(map (lambda (p) (format "~s" p))
		   (vector->list elements))))))))

;*---------------------------------------------------------------------*/
;*    js-debug-pcache ...                                              */
;*---------------------------------------------------------------------*/
(define (js-debug-pcache pcache #!optional (msg ""))
   (if (isa? pcache JsPropertyCache)
       (with-access::JsPropertyCache pcache (point src imap cmap pmap nmap emap amap xmap cntmiss iindex cindex pindex nindex aindex eindex)
	  (fprint (current-error-port) "-- " msg (typeof pcache)
	     " loc=" point ":" src
	     " cntmiss=" cntmiss)
	  (unless (eq? imap (js-not-a-pmap))
	     (with-access::JsConstructMap imap (%id props)
		(fprint (current-error-port) "  imap %id=" %id
		   " iindex=" iindex " props=" (vector-length props) " " (vector-map js-debug-prop props))))
	  (unless (eq? cmap (js-not-a-pmap))
	     (with-access::JsConstructMap cmap (%id props)
		(fprint (current-error-port) "  cmap %id=" %id
		   " cindex=" cindex " props=" (vector-length props) " " (vector-map js-debug-prop props))))
	  (unless (eq? pmap (js-not-a-pmap))
	     (with-access::JsConstructMap pmap (%id props)
		(fprint (current-error-port) "  pmap %id=" %id
		   " pindex=" pindex " props=" (vector-length props) " " (vector-map js-debug-prop props))))
	  (unless (eq? nmap (js-not-a-pmap))
	     (with-access::JsConstructMap nmap (%id props)
		(fprint (current-error-port) "  nmap %id=" %id
		   " nindex=" nindex " props=" (vector-length props) " " (vector-map js-debug-prop props))))
	  (unless (eq? emap (js-not-a-pmap))
	     (with-access::JsConstructMap emap (%id props)
		(fprint (current-error-port) "  emap %id=" %id
		   " eindex=" eindex " props=" (vector-length props) " " (vector-map js-debug-prop props))))
	  (unless (eq? amap (js-not-a-pmap))
	     (with-access::JsConstructMap amap (%id props)
		(fprint (current-error-port) "  amap %id=" %id
		   " aindex=" aindex " props=" (vector-length props) " " (vector-map js-debug-prop props))))
	  (unless (eq? xmap (js-not-a-pmap))
	     (with-access::JsConstructMap xmap (%id props)
		 (fprint (current-error-port) "  xmap %id=" %id
		    " props=" (vector-length props) " " (vector-map js-debug-prop props))))
	  (with-access::JsPropertyCache pcache (vindex)
	     (unless (=fx vindex (js-not-a-index))
		(fprint (current-error-port) "  vindex=" vindex))))
       (fprint (current-error-port) msg (typeof pcache))))

;*---------------------------------------------------------------------*/
;*    js-debug-cmap ...                                                */
;*---------------------------------------------------------------------*/
(define (js-debug-cmap cmap #!optional (msg ""))
   (with-access::JsConstructMap cmap (%id props methods transitions vtable mrtable mptable)
      (fprint (current-error-port) "~~ " msg (typeof cmap)
	 " %id=" %id
	 " prop.vlen=" (vector-length props)
	 " met.vlen=" (vector-length methods)
	 " vtable.len=" (vector-length vtable)
	 "\n  props=" (vector-map js-debug-prop props)
	 "\n  mrtable=" (when (vector? mrtable) (vector-length mrtable))
	 " mptable=" (when (vector? mptable) (vector-length mptable))
	 "\n  transitions="
	 (map (lambda (tr)
		 (format "~a:~a[~a]->~a"
		    (transition-name tr)
		    (typeof (transition-value tr))
		    (transition-flags tr)
		    (with-access::JsConstructMap (transition-nextmap tr) (%id)
		       %id)))
	    transitions))))

;*---------------------------------------------------------------------*/
;*    js-ctor-constrsize-extend! ...                                   */
;*---------------------------------------------------------------------*/
(define (js-ctor-constrsize-extend! ctor sz)
   (cond
      ((js-function? ctor)
       (with-access::JsFunction ctor (constrsize info constrmap)
	  (cond
	     ((>fx constrsize sz)
	      #unspecified)
	     ((>=fx constrsize (js-function-info-maxconstrsize info))
	      #unspecified)
	     ((<fx sz (js-function-info-maxconstrsize info))
	      (set! constrmap (clone-cmap constrmap))
	      (set! constrsize sz))
	     (else
	      (set! constrmap (clone-cmap constrmap))
	      (set! constrsize (+fx constrsize 1))))))
      ((cell? ctor)
       (tprint "SHOULD NOT BE HERE...")
       (cond
	  (#t #f)
	  ((>fx (cell-ref ctor) sz)
	   #unspecified)
	  ((>=fx (cell-ref ctor) (ctor-max-constrsize))
	   #unspecified)
	  ((<fx sz (ctor-max-constrsize))
	   (cell-set! ctor (+fx sz 1)))
	  (else
	   (cell-set! ctor (+fx (cell-ref ctor) 1)))))
      ((pair? ctor)
       (cond
	  ((>fx (cdr ctor) sz)
	   #unspecified)
	  ((>=fx (cdr ctor) (ctor-max-constrsize))
	   #unspecified)
	  ((<fx sz (ctor-max-constrsize))
	   (set-car! ctor (clone-cmap (car ctor)))
	   (set-cdr! ctor (+fx sz 1)))
	  (else
	   (set-car! ctor (clone-cmap (car ctor)))
	   (set-cdr! ctor (+fx (cdr ctor) 1)))))))

;*---------------------------------------------------------------------*/
;*    js-object-extend! ...                                            */
;*    -------------------------------------------------------------    */
;*    This function extends an existing object.                        */
;*    WARNING: Its cmap must be updated by the caller.                 */
;*---------------------------------------------------------------------*/
(define (js-object-extend! obj::JsObject idx::long value nlen::long)
   (cond-expand (profile (profile-cache-extension nlen)))
   (with-access::JsObject obj (elements)
      (let* ((ilen (js-object-inline-length obj))
	     (rnlen (-fx nlen ilen))
	     (nels (copy-vector elements rnlen)))
;* 	 (tprint "COPY-VECTOR el=" (vector-length elements) " rnlne=" rnlen */
;* 	    " ilen=" ilen " idx=" idx " nlen=" nlen)                   */
	 (vector-set! nels (-fx idx ilen) value)
	 (set! elements nels)
	 obj)))

;*---------------------------------------------------------------------*/
;*    js-object-put-push! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-object-put-push! obj::JsObject idx::long value)
   (with-trace 'prop "js-object-put-push!"
      (trace-item "idx=" idx
	 " ilen=" (js-object-inline-length obj)
	 " len=" (vector-length (js-object-noinline-elements obj)))
      (if (<fx idx (js-object-inline-length obj))
	  (js-object-inline-set! obj idx value)
	  (let ((ridx (-fx idx (js-object-inline-length obj))))
	     (with-access::JsObject obj (elements)
		(if (<fx ridx (vector-length elements))
		    (vector-set! elements ridx value)
		    (with-access::JsObject obj (cmap)
		       (unless (eq? cmap (js-not-a-cmap))
			  (with-access::JsConstructMap cmap (ctor)
			     (js-ctor-constrsize-extend! ctor (+fx idx 1))))
		       (js-object-extend! obj idx value (+fx idx 1)))))))))

;*---------------------------------------------------------------------*/
;*    js-object-bind-push! ...                                         */
;*    -------------------------------------------------------------    */
;*    Only used by js-bind!. Normal extensions go through              */
;*    JS-OBJECT-PUT-PUSH!                                              */
;*---------------------------------------------------------------------*/
(define (js-object-bind-push! obj::JsObject idx::long value)
   (with-trace 'prop "js-object-bind-push!"
      (trace-item "obj=" (typeof obj) " idx=" idx " value=" (typeof value))
      (if (<fx idx (js-object-inline-length obj))
	  (js-object-inline-set! obj idx value)
	  (let ((ridx (-fx idx (js-object-inline-length obj))))
	     (with-access::JsObject obj (elements)
		(if (<fx ridx (vector-length elements))
		    (vector-set! elements ridx value)
		    (js-object-extend! obj idx value (+fx idx 1))))))))

;*---------------------------------------------------------------------*/
;*    js-object-cmap-push! ...                                         */
;*    -------------------------------------------------------------    */
;*    Only used by inline caches when extending an object, after       */
;*    nmap match (see JS-PUT-JS-OBJECT-NAME/CACHE-EXPANDER and         */
;*    JS-PCACHE-UPDATE-NEXT-DIRECT!).                                  */
;*---------------------------------------------------------------------*/
(define (js-object-cmap-push! obj::JsObject idx::long value ncmap)
   (with-access::JsObject obj (elements cmap)
      (with-access::JsConstructMap cmap (ctor)
	 (js-ctor-constrsize-extend! ctor (+fx idx 1)))
      (js-object-extend! obj idx value (+fx idx 1))
      (set! cmap ncmap)
      obj))

;*---------------------------------------------------------------------*/
;*    js-object-vtable-push! ...                                       */
;*    -------------------------------------------------------------    */
;*    Only used after a vtable cache hit                               */
;*    (see JS-PUT-JS-OBJECT-NAME/CACHE-EXPANDER).                      */
;*---------------------------------------------------------------------*/
(define (js-object-vtable-push! obj::JsObject idx::long value ncmap)
   (with-access::JsObject obj (elements cmap)
      (cond
	 ((<fx idx (js-object-inline-length obj))
	  (js-object-inline-set! obj idx value)
	  (set! cmap ncmap)
	  obj)
	 ((>=fx idx (vector-length elements))
	  (js-object-cmap-push! obj idx value ncmap))
	 (else
	  (vector-set! elements idx value)
	  (set! cmap ncmap)
	  obj))))

;*---------------------------------------------------------------------*/
;*    function0->proc ...                                              */
;*---------------------------------------------------------------------*/
(define (function0->proc fun %this::JsGlobalObject)
   (if (js-procedure? fun)
       (with-access::JsProcedure fun (procedure)
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
   (if (js-procedure? fun)
       (with-access::JsProcedure fun (procedure)
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
		  (name (& "?"))
		  (pmap (js-not-a-pmap))
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
		  (set! name (js-string->name (vector-ref e 1)))
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
		  (set! name (js-string->name (vector-ref e 1)))
		  (set! usage (vector-ref e 2)))
	       (loop (-fx i 1))))))
   pctable)

;*---------------------------------------------------------------------*/
;*    js-pcache-ref ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-pcache-ref pcache index)
   (vector-ref pcache index))

;*---------------------------------------------------------------------*/
;*    js-pcache-XXXmap ...                                             */
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
;*    js-pcache-update-miss! ...                                       */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-miss! pcache::JsPropertyCache o::JsObject)
   (with-access::JsObject o ((omap cmap))
      (unless (eq? omap (js-not-a-cmap))
	 (with-access::JsPropertyCache pcache (xmap)
	    (set! xmap omap)
	    (js-validate-pmap-pcache! pcache)))))
   
;*---------------------------------------------------------------------*/
;*    js-pcache-update-get-direct! ...                                 */
;*    -------------------------------------------------------------    */
;*    Used to get a direct object property.                            */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-get-direct! pcache::JsPropertyCache i o::JsObject PROP)
   
   (define (update-inline! pcache omap)
      (with-access::JsPropertyCache pcache (imap iindex)
	 (set! imap omap)
	 (set! iindex i)))

   (define (update-noinline! pcache omap)
      (with-access::JsPropertyCache pcache (cmap cindex)
	 (set! cmap omap)
	 (set! cindex (-fx i (js-object-inline-length o)))))

   (with-access::JsObject o (cmap)
      (if (<fx i (js-object-inline-length o))
	  (update-inline! pcache cmap)
	  (update-noinline! pcache cmap))))

;*---------------------------------------------------------------------*/
;*    js-pcache-update-get-prototype! ...                              */
;*    -------------------------------------------------------------    */
;*    Used to access a prototype object property.                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-get-prototype! pcache::JsPropertyCache i o::JsObject obj)
   (with-access::JsObject o ((omap cmap))
      (with-access::JsPropertyCache pcache (pmap pindex owner)
	 (js-validate-pmap-pcache! pcache)
	 (set! pmap omap)
	 (set! pindex i)
	 (set! owner obj))))

;*---------------------------------------------------------------------*/
;*    js-pcache-update-put-direct! ...                                 */
;*    -------------------------------------------------------------    */
;*    Used to access a direct object property.                         */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-put-direct! pcache::JsPropertyCache i o::JsObject)
   
   (define (update-inline! pcache omap)
      (with-access::JsPropertyCache pcache (imap iindex)
	 (set! imap omap)
	 (set! iindex i)))

   (define (update-noinline! pcache omap)
      (with-access::JsPropertyCache pcache (cmap cindex)
	 (set! cmap omap)
	 (set! cindex (-fx i (js-object-inline-length o)))))
   
   (with-access::JsObject o (cmap)
      (if (<fx i (js-object-inline-length o))
	  (update-inline! pcache cmap)
	  (update-noinline! pcache cmap))))

;*---------------------------------------------------------------------*/
;*    js-pcache-update-accessor! ...                                   */
;*    -------------------------------------------------------------    */
;*    Used to access an object from an accessor                        */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-accessor! pcache::JsPropertyCache i o::JsObject)
   (with-access::JsObject o ((omap cmap))
      (with-access::JsPropertyCache pcache (imap cmap pmap emap amap aindex owner)
	 (js-validate-pmap-pcache! pcache)
	 (set! pmap (js-not-a-pmap)) ;; WHY NEEDED?
	 (set! emap (js-not-a-pmap)) ;; WHY NEEDED?
	 (set! amap omap)
	 (set! aindex i)
	 (set! owner o))))

;*---------------------------------------------------------------------*/
;*    js-pcache-update-descriptor! ...                                 */
;*    -------------------------------------------------------------    */
;*    Used to access an object's descriptor                            */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-descriptor! pcache::JsPropertyCache i o::JsObject ow::JsObject)
   (with-access::JsObject o ((omap cmap))
      (with-access::JsPropertyCache pcache (imap cmap pmap emap amap aindex owner)
	 (js-validate-pmap-pcache! pcache)
	 (set! pmap (js-not-a-pmap))
	 (set! emap (js-not-a-pmap))
	 (set! amap omap)
	 (set! aindex i)
	 (set! owner ow))))

;*---------------------------------------------------------------------*/
;*    js-pcache-update-next-direct! ...                                */
;*    -------------------------------------------------------------    */
;*    Used when adding a direct property to an object.                 */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-next-direct! pcache::JsPropertyCache o::JsObject nextmap i inlp::bool)
   
   (define (next-inline! pcache omap)
      (with-access::JsPropertyCache pcache (nextemap emap eindex owner)
	 (set! nextemap nextmap)
	 (set! emap omap)
	 (set! eindex i)
	 (set! owner #f)))

   (define (next-noinline! pcache omap)
      (with-access::JsPropertyCache pcache (nextnmap nmap nindex owner)
	 (set! nextnmap nextmap)
	 (set! nmap omap)
	 (set! nindex i)
	 (set! owner #f)))   
   
   (with-access::JsObject o (cmap)
      (unless (eq? cmap (js-not-a-cmap))
	 (js-validate-pmap-pcache! pcache)
	 (if inlp
	     (next-inline! pcache cmap)
	     (next-noinline! pcache cmap)))))

;*---------------------------------------------------------------------*/
;*    js-pcache-vtable! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-pcache-vtable! cache::JsPropertyCache omap i %this::JsGlobalObject)
      (with-access::JsPropertyCache cache (cntmiss vindex point)
	 (when (=fx vindex (js-not-a-index))
	    (set! vindex (js-get-vindex %this)))
	 (js-cmap-vtable-add! omap vindex i cache)))

;*---------------------------------------------------------------------*/
;*    cmap-size ...                                                    */
;*---------------------------------------------------------------------*/
(define (cmap-size cmap::JsConstructMap)
   (with-access::JsConstructMap cmap (props) (vector-length props)))

;*---------------------------------------------------------------------*/
;*    js-object-inline-next-elements? ...                              */
;*---------------------------------------------------------------------*/
(define-inline (js-object-inline-next-elements? o::JsObject idx::long)
   (<fx idx (vector-length (js-object-inline-elements o))))

;*---------------------------------------------------------------------*/
;*    transition ...                                                   */
;*---------------------------------------------------------------------*/
(define-struct transition name value flags nextmap)

;*---------------------------------------------------------------------*/
;*    link-cmap! ...                                                   */
;*---------------------------------------------------------------------*/
(define (link-cmap! omap::JsConstructMap nmap::JsConstructMap
	   name value flags::int)
   (with-access::JsConstructMap omap (transitions)
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
(define (extend-cmap omap::JsConstructMap name flags::long)
   (with-access::JsConstructMap omap (props methods ctor mptable mrtable mntable)
      (let ((newprops (vector-extend props (prop name flags)))
	    (newmethods (vector-extend methods #unspecified)))
	 (js-make-jsconstructmap
	    :ctor ctor
	    :props newprops
	    :methods newmethods
	    :mrtable mrtable
	    :mntable mntable
	    :mptable mptable))))

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
	    (transitions '())))))

;*---------------------------------------------------------------------*/
;*    merge-cmap! ...                                                  */
;*    -------------------------------------------------------------    */
;*    Add all the properties of src in target if not alread there.     */
;*    This function is invoked by JS-FUNCTION-MAYBE-EXTEND-CMAP to     */
;*    merge the cmap of a class and the cmap of its super class.       */
;*---------------------------------------------------------------------*/
(define (merge-cmap!::JsConstructMap target::JsConstructMap src::JsConstructMap)
   
   (define (memq-prop vec prop)
      (let ((name (prop-name prop)))
	 (let loop ((i (-fx (vector-length vec) 1)))
	    (cond
	       ((=fx i -1) #f)
	       ((eq? (prop-name (vector-ref vec i)) name) #t)
	       (else (loop (-fx i 1)))))))
   
   (with-access::JsConstructMap target ((tprops props) (tmets methods))
      (with-access::JsConstructMap src ((sprops props) (smets methods))
	 (let ((nprops '())
	       (nmets '()))
	    (let loop ((i (-fx (vector-length sprops) 1)))
	       (when (>=fx i 0)
		  (let ((prop (vector-ref sprops i))
			(met (vector-ref smets i)))
		     (unless (memq-prop tprops prop)
			(set! nprops (cons prop nprops))
			(set! nmets (cons met nmets))))
		  (loop (-fx i 1))))
	    (if (pair? nprops)
		(let* ((nlen (length nprops))
		       (rprops (make-vector (+fx (vector-length tprops) nlen)))
		       (rmets (make-vector (+fx (vector-length tprops) nlen))))
		   (vector-copy! rprops nlen tprops 0 (vector-length tprops))
		   (vector-copy! rmets nlen tprops 0 (vector-length tmets))
		   (let loop ((i (-fx nlen 1))
			      (nprops nprops))
		      (if (null? nprops)
			  (begin
			     (set! tprops rprops)
			     (set! tmets rmets)
			     target)
			  (begin
			     (vector-set! rprops i (car nprops))
			     (vector-set! rmets i (car nmets))
			     (loop (-fx i 1) (cdr nprops))))))
		target)))))

;*---------------------------------------------------------------------*/
;*    cmap-find-transition ...                                         */
;*---------------------------------------------------------------------*/
(define (cmap-find-transition omap::JsConstructMap name val flags::int)
   
   (define (is-transition? t)
      (and (eq? (transition-name t) name)
	   (=fx (transition-flags t) flags)
	   (or (not (transition-value t))
	       (eq? (transition-value t) val))))
   
   (with-access::JsConstructMap omap (transitions)
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
      (let ((flags (property-flags #t #t #t #f #t)))
	 ;; 1- try to find a transition from the current cmap
	 (synchronize lock
	    (let ((nextmap (cmap-find-transition cmap (& "__proto__") new flags)))
	       (or nextmap
		   ;; 2- create a new plain cmap connected to its parent
		   ;; via a regular link
		   (let ((newmap (duplicate::JsConstructMap cmap
				    (%id (gencmapid)))))
		      (link-cmap! cmap newmap (& "__proto__") new flags)
		      newmap)))))))

;*---------------------------------------------------------------------*/
;*    reset-cmap-vtable! ...                                           */
;*---------------------------------------------------------------------*/
(define (reset-cmap-vtable! cmap::JsConstructMap reason who)
   (synchronize js-cache-vtable-lock
      (with-access::JsConstructMap cmap (%id vtable)
	 ;;(tprint "RESET-VTABLE..." %id " " reason " " who)
	 (set! vtable '#()))))
   
;*---------------------------------------------------------------------*/
;*    js-cmap-vtable-add! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-cmap-vtable-add! o::JsConstructMap idx::long obj cache::JsPropertyCache)
   (with-access::JsConstructMap o (vtable %id)
      (with-access::JsPropertyCache cache (point)
	 (synchronize js-cache-vtable-lock
	    (let ((l (vector-length vtable)))
	       (cond
		  ((=fx l 0)
		   (set! vtable (make-vector (+fx idx 1) #unspecified))
		   (log-vtable! idx vtable '#()))
		  ((>=fx idx l)
		   (let ((old vtable))
		      (set! vtable (copy-vector vtable (+fx idx 1)))
		      (log-vtable! idx vtable old)
		      (vector-fill! vtable #unspecified l))))
	       (vector-set! vtable idx obj)
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
(define (js-names->cmap names #!key (writable #f) (enumerable #t) (configurable #t) (accessor #f) (inline #f))
   (js-make-jsconstructmap
      :props (vector-map (lambda (n)
			    (prop n
			       (property-flags
				  writable enumerable configurable accessor inline)))
		names)
      :methods (make-vector (vector-length names) #unspecified)))
      
;*---------------------------------------------------------------------*/
;*    js-strings->cmap ...                                             */
;*    -------------------------------------------------------------    */
;*    Used by j2sscheme to create literal objects.                     */
;*---------------------------------------------------------------------*/
(define (js-strings->cmap names)
   (let* ((len (vector-length names))
	  (ctor (cons #unspecified len))
	  (props (vector-map (lambda (e)
				(let ((n (if (pair? e) (car e) e))
				      (c (if (pair? e) (cdr e) #t)))
				   (if (and (>fx (string-length n) 0)
					    (char=? (string-ref n 0) #\#))
				       ;; private name
				       (prop (js-string->private-name n)
					  (property-flags #t #f #f #f #t))
				       (prop (js-string->name n)
					  (property-flags #t #t c #f #t)))))
		    names))
	  (cmap (js-make-jsconstructmap
		   :ctor ctor
		   :props props
		   :methods (make-vector len #unspecified))))
      (set-car! ctor cmap)
      cmap))
      
;*---------------------------------------------------------------------*/
;*    js-object-literal-init! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-object-literal-init! o::JsObject)
   (with-access::JsObject o (cmap)
      (with-access::JsConstructMap cmap ((%methods methods) (%props props))
	 (let ((methods %methods)
	       (props %props))
	    ;; Because the inline size might be dynamically extended
	    ;; we may have elements.length > props.length so to avoid
	    ;; array overflow, it is required to scan only the elements
	    ;; that have an associated prop
	    (let loop ((i (-fx (vector-length props) 1)))
	       (if (=fx i -1)
		   o
		   (let ((v (js-object-inline-ref o i)))
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
(define-macro (jsobject-map-find o p succeed fail . proxy)
   (let ((i (gensym 'i))
	 (cmap (gensym 'cmap))
	 (loop (gensym 'loop)))
      `(with-access::JsObject ,o ((,cmap cmap))
	  (with-access::JsConstructMap ,cmap (props)
	     (let ((props props))
		(let ,loop ((,i (-fx (vector-length props) 1)))
		     (cond
			((=fx ,i -1)
			 ,(if (or (null? proxy) (car proxy))
			      `(if (js-proxy? ,o)
				   (,succeed ,o (js-proxy-property-descriptor-index ,o ,p))
				   (,fail))
			      `(,fail)))
			((eq? (prop-name (vector-ref props ,i)) ,p)
			 (,succeed ,o ,i))
			(else
			 (,loop (-fx ,i 1))))))))))

;*---------------------------------------------------------------------*/
;*    jsobject-map-find/string ...                                     */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-map-find/string o p succeed fail)
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
		      ((string=? (js-jsstring->string (prop-name (vector-ref props ,i))) ,p)
		       (,succeed ,o ,i))
		      (else
		       (,loop (-fx ,i 1))))))))))

;*---------------------------------------------------------------------*/
;*    jsobject-hash-find ...                                           */
;*    -------------------------------------------------------------    */
;*    Contrary to other object property search, the property key       */
;*    has not always been transformed yet into a name.                 */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-hash-find o p succeed fail . proxy)
   (let ((hash (gensym 'hash))
	 (prop (gensym 'prop))
	 (name (gensym 'name)))
      `(if (js-jsstring? ,p)
	   (with-access::JsObject ,o ((,hash elements))
	      (let ((,prop (prop-hashtable-get ,hash (js-jsstring->string ,p))))
		 (cond
		    (,prop
		     (,succeed ,o ,prop))
		    ,@(if (or (null? proxy) (car proxy))
		       `(((js-proxy? ,o)
			  (,succeed ,o (make-cell (js-proxy-property-descriptor-index ,o ,p)))))
		       '())
		    (else
		     (set! ,p (js-toname ,p %this))
		     (,fail)))))
	   (,fail))))

;*---------------------------------------------------------------------*/
;*    jsobject-hash-find/string ...                                    */
;*    -------------------------------------------------------------    */
;*    Contrary to other object property search, the property key       */
;*    has not always been transformed yet into a name.                 */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-hash-find/string o p succeed fail)
   (let ((hash (gensym 'hash))
	 (prop (gensym 'prop))
	 (name (gensym 'name)))
      `(with-access::JsObject ,o ((,hash elements))
	  (let ((,prop (prop-hashtable-get ,hash ,p)))
	     (cond
		(,prop
		 (,succeed ,o ,prop))
		((js-proxy? ,o)
		 (,succeed ,o (make-cell (js-proxy-property-descriptor-index ,o ,p))))
		(else
		 (,fail)))))))

;*---------------------------------------------------------------------*/
;*    jsobject-properties-find ...                                     */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-properties-find o p succeed fail . proxy)
   (let ((desc (gensym 'desc))
	 (name (gensym 'name))
	 (prop (gensym 'properties))
	 (i (gensym 'i))
	 (loop (gensym 'loop)))
      `(let ((,prop (with-access::JsObject ,o (elements) elements)))
	  (let ,loop ((,i (-fx (vector-length ,prop) 1)))
	       (if (=fx ,i -1)
		   ,(if (or (null? proxy) (car proxy))
			`(if (js-proxy? ,o)
			     (let ((,i (js-proxy-property-descriptor-index ,o ,p)))
				(,succeed ,o (vector-ref ,prop ,i) ,i))
			     (,fail))
			`(,fail))
		 (let ((,desc (vector-ref ,prop ,i)))
		    (with-access::JsPropertyDescriptor ,desc ((,name name))
		       (if (eq? ,name ,p)
			   (,succeed ,o ,desc ,i)
			   (,loop (-fx ,i 1))))))))))

;*---------------------------------------------------------------------*/
;*    jsobject-properties-find ...                                     */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-properties-find/string o p succeed fail)
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
		       (if (string=? (js-jsstring->string ,name) ,p)
			   (,succeed ,o ,desc ,i)
			   (,loop (-fx ,i 1))))))))))

;*---------------------------------------------------------------------*/
;*    jsobject-find ...                                                */
;*    -------------------------------------------------------------    */
;*    This is a general macro that walks thru a prototype chain        */
;*    (iff the optional loop argument is provided) looking for a       */
;*    property. It calls one of the successXXX hooks when found.       */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-find o base name foundinmap foundinhash foundinprop notfound . loop)

   (define (find obj name)
      `(with-access::JsObject ,obj (cmap)
	  (cond
	     ((js-object-mapped? ,obj)
	      (jsobject-map-find ,obj ,name ,foundinmap
		 (lambda ()
		    ,(if (pair? loop)
			 `(let ((__proto__ (js-object-proto ,obj)))
			     (if (js-object? __proto__)
				 (begin
				    (js-object-mode-isprotoof-set! __proto__ #t)
				    (,(car loop) __proto__))
				 (,notfound ,base)))
			 `(,notfound ,base)))))
	     ((js-object-hashed? ,obj)
	      (jsobject-hash-find ,obj ,name ,foundinhash
		 (lambda ()
		    ,(if (pair? loop)
			 `(let ((__proto__ (js-object-proto ,obj)))
			     (if (js-object? __proto__)
				 (begin
				    (js-object-mode-isprotoof-set! __proto__ #t)
				    (,(car loop) __proto__))
				 (,notfound ,base)))
			 `(,notfound ,base)))))
	     (else
	      (jsobject-properties-find ,obj ,name ,foundinprop
		 (lambda ()
		    ,(if (pair? loop)
			 `(let ((__proto__ (js-object-proto ,obj)))
			     (if (js-object? __proto__)
				 (begin
				    (js-object-mode-isprotoof-set! __proto__ #t)
				    (,(car loop) __proto__))
				 (,notfound ,base)))
			 `(,notfound ,base))))))))

   (if (and (symbol? o) (symbol? base))
       (let ((obj (gensym 'obj)))
	  `(let ((,obj ,o))
	      ,(if (symbol? name)
		   (find obj name)
		   (let ((nm (gensym 'name)))
		      `(let ((,nm ,name))
			  ,(find obj nm))))))
       (error "jsobject-find" "arguments must be a symbol" (cons obj base))))

;*---------------------------------------------------------------------*/
;*    jsobject-find/w-proxy ...                                        */
;*    -------------------------------------------------------------    */
;*    A variant that does not check proxy objects.                     */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-find/w-proxy o base name foundinmap foundinhash foundinprop notfound . loop)

   (define (find obj name)
      `(with-access::JsObject ,obj (cmap)
	  (cond
	     ((js-object-mapped? ,obj)
	      (jsobject-map-find ,obj ,name ,foundinmap
		 (lambda ()
		    ,(if (pair? loop)
			 `(let ((__proto__ (js-object-proto ,obj)))
			     (if (js-object? __proto__)
				 (begin
				    (js-object-mode-isprotoof-set! __proto__ #t)
				    (,(car loop) __proto__))
				 (,notfound ,base)))
			 `(,notfound ,base)))
		 #f))
	     ((js-object-hashed? ,obj)
	      (jsobject-hash-find ,obj ,name ,foundinhash
		 (lambda ()
		    ,(if (pair? loop)
			 `(let ((__proto__ (js-object-proto ,obj)))
			     (if (js-object? __proto__)
				 (begin
				    (js-object-mode-isprotoof-set! __proto__ #t)
				    (,(car loop) __proto__))
				 (,notfound ,base)))
			 `(,notfound ,base)))
		 #f))
	     (else
	      (jsobject-properties-find ,obj ,name ,foundinprop
		 (lambda ()
		    ,(if (pair? loop)
			 `(let ((__proto__ (js-object-proto ,obj)))
			     (if (js-object? __proto__)
				 (begin
				    (js-object-mode-isprotoof-set! __proto__ #t)
				    (,(car loop) __proto__))
				 (,notfound ,base)))
			 `(,notfound ,base)))
		 #f)))))

   (if (and (symbol? o) (symbol? base))
       (let ((obj (gensym 'obj)))
	  `(let ((,obj ,o))
	      ,(if (symbol? name)
		   (find obj name)
		   (let ((nm (gensym 'name)))
		      `(let ((,nm ,name))
			  ,(find obj nm))))))
       (error "jsobject-find" "arguments must be a symbol" (cons obj base))))

;*---------------------------------------------------------------------*/
;*    jsobject-find/string ...                                         */
;*    -------------------------------------------------------------    */
;*    A variant of jsobject-find that does not use hashed prop names   */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-find/string o base name foundinmap foundinhash foundinprop notfound . loop)

   (define (find obj name)
      `(with-access::JsObject ,obj (cmap)
	  (cond
	     ((js-object-mapped? ,obj)
	      (jsobject-map-find/string ,obj ,name ,foundinmap
		 (lambda ()
		    ,(if (pair? loop)
			 `(let ((__proto__ (js-object-proto ,obj)))
			     (if (js-object? __proto__)
				 (begin
				    (js-object-mode-isprotoof-set! __proto__ #t)
				    (,(car loop) __proto__))
				 (,notfound ,base)))
			 `(,notfound ,base)))))
	     ((js-object-hashed? ,obj)
	      (jsobject-hash-find/string ,obj ,name ,foundinhash
		 (lambda ()
		    ,(if (pair? loop)
			 `(let ((__proto__ (js-object-proto ,obj)))
			     (if (js-object? __proto__)
				 (begin
				    (js-object-mode-isprotoof-set! __proto__ #t)
				    (,(car loop) __proto__))
				 (,notfound ,base)))
			 `(,notfound ,base)))))
	     (else
	      (jsobject-properties-find/string ,obj ,name ,foundinprop
		 (lambda ()
		    ,(if (pair? loop)
			 `(let ((__proto__ (js-object-proto ,obj)))
			     (if (js-object? __proto__)
				 (begin
				    (js-object-mode-isprotoof-set! __proto__ #t)
				    (,(car loop) __proto__))
				 (,notfound ,base)))
			 `(,notfound ,base))))))))

   (if (and (symbol? o) (symbol? base))
       (let ((obj (gensym 'obj)))
	  `(let ((,obj ,o))
	      ,(if (symbol? name)
		   (find obj name)
		   (let ((nm (gensym 'name)))
		      `(let ((,nm ,name))
			  ,(find obj nm))))))
       (error "jsobject-find" "arguments must be a symbol" (cons obj base))))

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
   (js-object-mode-isprotoof-set! o #t)
   (with-access::JsObject o (cmap elements)
      (when (js-object-mapped? o)
	 (with-access::JsConstructMap cmap (props)
	    (let ((vec (make-vector (vector-length props))))
	       (let loop ((i (-fx (vector-length props) 1)))
		  (cond
		     ((=fx i -1)
		      (set! cmap (js-not-a-cmap))
		      (vector-fill! (js-object-inline-elements o) (js-undefined))
		      (set! elements vec))
		     ((not (vector-ref props i))
		      (error "js-object-unmap!" "illegal property descriptor" i))
		     ((isa? (js-object-ref o i) JsPropertyDescriptor)
		      (vector-set! vec i (js-object-ref o i))
		      (loop (-fx i 1)))
		     (else
		      (let* ((name (prop-name (vector-ref props i)))
			     (flags (prop-flags (vector-ref props i)))
			     (desc (instantiate::JsValueDescriptor
				      (enumerable (flags-enumerable? flags))
				      (writable (flags-writable? flags))
				      (configurable (flags-configurable? flags))
				      (name name)
				      (value (js-object-ref o i)))))
			 (vector-set! vec i desc)
			 (loop (-fx i 1))))))))))
   o)

;*---------------------------------------------------------------------*/
;*    js-object-unhash! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-object-unhash! o::JsObject)
   (js-object-mode-enumerable-set! o #t)
   (js-object-mode-plain-set! o #f)
   (js-object-mode-isprotoof-set! o #t)
   (with-access::JsObject o (cmap elements)
      (let ((vec (make-vector (hashtable-size elements)))
	    (i -1))
	 (hashtable-for-each elements
	    (lambda (k c)
	       (set! i (+fx i 1))
	       (let ((p (cell-ref c)))
		  (vector-set! vec i p))))
	 (set! elements vec)
	 (set! cmap (js-not-a-cmap))
	 o)))

;*---------------------------------------------------------------------*/
;*    js-object-hashable? ...                                          */
;*    -------------------------------------------------------------    */
;*    An object can be hashed if all the property names are strings.   */
;*---------------------------------------------------------------------*/
(define (js-object-hashable? o::JsObject)
   (with-access::JsObject o (cmap)
      (with-access::JsConstructMap cmap (props)
	 (let ((props props))
	    (let loop ((i (-fx (vector-length props) 1)))
	       (cond
		  ((=fx i -1)
		   #t)
		  ((js-jsstring? (prop-name (vector-ref props i)))
		   (loop (-fx i 1)))
		  (else
		   #f)))))))

;*---------------------------------------------------------------------*/
;*    js-object-hash! ...                                              */
;*    -------------------------------------------------------------    */
;*    This function turns a mapped object into a hash table when the   */
;*    property number exceeds HASH-OBJECT-THRESHOLD.                   */
;*---------------------------------------------------------------------*/
(define (js-object-hash! o::JsObject)
   (js-object-mode-enumerable-set! o #t)
   (js-object-mode-plain-set! o #f)
   (let ((table (create-hashtable
		   :weak (prop-hashtable-weak)
		   :size (+fx (/fx (hash-object-threshold) 3) (hash-object-threshold))
		   :max-length 65536
		   :max-bucket-length 20)))
      (with-access::JsObject o (cmap elements)
	 (with-access::JsConstructMap cmap (props)
	    (let loop ((i (-fx (vector-length props) 1)))
	       (cond
		  ((=fx i -1)
		   (set! cmap (js-not-a-cmap))
		   (set! elements table))
		  ((not (vector-ref props i))
		   (error "js-object-hash!" "illegal property descriptor" i))
		  ((isa? (js-object-ref o i) JsPropertyDescriptor)
		   (with-access::JsPropertyDescriptor (js-object-ref o i) (name)
		      (prop-hashtable-put! table (js-jsstring->string name)
			 (make-cell (js-object-ref o i)))
		      (loop (-fx i 1))))
		  (else
		   (let* ((name (prop-name (vector-ref props i)))
			  (flags (prop-flags (vector-ref props i)))
			  (descv (if (eq? flags (property-flags-default))
				     (js-object-ref o i)
				     (instantiate::JsValueDescriptor
					(enumerable (flags-enumerable? flags))
					(writable (flags-writable? flags))
					(configurable (flags-configurable? flags))
					(name name)
					(value (js-object-ref o i))))))
		      (prop-hashtable-put! table (js-jsstring->string name)
			 (make-cell descv))
		      (loop (-fx i 1))))))))
      o))

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
;*    js-property-descriptor ...                                       */
;*---------------------------------------------------------------------*/
(define (js-property-descriptor %this::JsGlobalObject isvalue::bool
	   #!key writable enumerable configurable value get set)
   (with-access::JsGlobalObject %this (js-property-descriptor-value-cmap
					 js-property-descriptor-getter-cmap)
      (if isvalue
	  (instantiateJsObject
	     (cmap js-property-descriptor-value-cmap)
	     (__proto__ (js-object-proto %this))
	     (elements (vector enumerable configurable value writable)))
	  (instantiateJsObject
	     (cmap js-property-descriptor-getter-cmap)
	     (__proto__ (js-object-proto %this))
	     (elements (vector enumerable configurable get set))))))
   
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
         (js-define-own-property o p newdesc throw %this)))
   
   (define (from-object desc)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new %this js-object)))
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
	    obj)))

   (define (from-desc desc)
      (with-access::JsPropertyDescriptor desc (enumerable configurable)
	 (cond
	    ((isa? desc JsValueDescriptor)
	     ;; 3
	     (with-access::JsValueDescriptor desc (writable value)
		(js-property-descriptor %this #t
		   :writable writable
		   :enumerable enumerable
		   :configurable configurable
		   :value value)))
	    ((isa? desc JsAccessorDescriptor)
	     ;; 4
	     (with-access::JsAccessorDescriptor desc (get set)
		(js-property-descriptor %this #f
		   :writable #unspecified
		   :enumerable enumerable
		   :configurable configurable
		   :get (or get (js-undefined))
		   :set (or set (js-undefined)))))
	    ((isa? desc JsWrapperDescriptor)
	     (with-access::JsWrapperDescriptor desc (writable %get)
		(js-property-descriptor %this #t
		   :writable writable
		   :enumerable enumerable
		   :configurable configurable
		   :value (%get owner owner propname %this))))
	    (else
	     (error "js-from-property-descriptor"
		(format "Illegal descriptor (~a)" (typeof desc))
		desc)))))

   (cond
      ((eq? desc (js-undefined))
       desc)
      ((js-object? desc)
       (from-object desc))
      (else
       (from-desc desc))))

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
			 (and (not (js-procedure? get))
			      (not (eq? get (js-undefined))))
			 (and (not (js-procedure? set))
			      (not (eq? set (js-undefined)))))
		     (js-raise-type-error %this
			(format "Illegal property descriptor (~a) ~~s"
			   (typeof obj))
			obj)
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
		   (if (or (not enump) (flags-enumerable? (prop-flags prop)))
		       (loop (-fx i 1) (cons (prop-name prop) acc))
		       (loop (-fx i 1) acc))))
	       (else
		(loop (-fx i 1) acc))))))

   (define (hash->names elements)
      (map! js-string->jsstring (hashtable-key-list elements)))

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
	 ((js-object-hashed? o)
	  (hash->names elements))
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
;*    js-in? ...                                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.7       */
;*---------------------------------------------------------------------*/
(define (js-in? obj name %this)
   (if (not (js-object? obj))
       (js-raise-type-error %this "in: not an object ~s" obj)
       (let loop ((obj obj))
	  (jsobject-find/w-proxy obj obj name
	     ;; cmap search
	     (lambda (owner i) #t)
	     ;; hash seach
	     (lambda (owner e) #t)
	     ;; property search
	     (lambda (owner d i) #t)
	     ;; failure
	     (lambda (o) #f)
	     ;; prototype search
	     (lambda (__proto__)
		(cond
		   ((not (or (eq? (object-class obj) JsObject)
			     (isa? obj JsRecord)))
		    (js-has-property obj name %this))
		   ((and (js-object? __proto__)
			 (eq? (object-class __proto__) JsObject))
		    (loop __proto__))
		   (else
		    (js-has-property __proto__ name %this))))))))

(define (js-in?/debug obj name %this loc)
   (if (not (js-object? obj))
       (js-raise-type-error/loc %this loc "in: not an object ~s" obj)
       (js-has-property obj name %this)))

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
(define-method (js-has-property::bool o::JsObject prop::obj %this)
   (let ((name (js-toname prop %this)))
      (jsobject-find o o name
	 ;; cmap search
	 (lambda (owner i) #t)
	 ;; hash search
	 (lambda (owner e) #t)
	 ;; property search
	 (lambda (owner d i) #t)
	 ;; failure
	 (lambda (o) #f)
	 ;; prototype search
	 (lambda (__proto__) (js-has-property __proto__ name %this)))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ...                                          */
;*    -------------------------------------------------------------    */
;*    This generic is used to implement Object.hasOwnProperty (see     */
;*    object.scm)                                                      */
;*---------------------------------------------------------------------*/
(define-generic (js-has-own-property::bool o p::obj %this)
   (not (eq? (js-get-own-property (js-toobject-fast o %this) p %this) (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ::JsObject ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-has-own-property o::JsObject p::obj %this)
   (js-has-own-property-jsobject o p %this))

;*---------------------------------------------------------------------*/
;*    js-has-own-property-jsobject ...                                 */
;*---------------------------------------------------------------------*/
(define (js-has-own-property-jsobject o::JsObject p::obj %this)
   
   (define (js-has-own-property/w-cache o p)
      (jsobject-find o o (js-toname p %this)
	 ;; cmap search
	 (lambda (owner i) #t)
	 ;; hash search 
	 (lambda (owner e) #t)
	 ;; prototype search
	 (lambda (owner d i) #t)
	 ;; not found
	 (lambda (o) #f)))
   
   (define (js-has-own-property/cache o pname cache)
      (with-access::JsObject o ((omap cmap))
	 (with-access::JsPropertyCache cache (cmap)
	    (or (eq? cmap omap)
		(jsobject-find o o pname
		   ;; cmap search
		   (lambda (owner i)
		      (set! cmap omap)
		      #t)
		   ;; hash search
		   (lambda (owner e)
		      #t)
		   ;; prototype search
		   (lambda (owner d i)
		      #t)
		   ;; not found
		   (lambda (o)
		      #f))))))

   (if (js-jsstring? p)
       (let ((pname (js-toname p %this)))
	  (let ((cacher (js-name-pcacher pname)))
	     (if cacher
		 (js-has-own-property/cache o pname cacher)
		 (js-has-own-property/w-cache o pname))))
       (js-has-own-property/w-cache o p)))

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
	 (with-access::JsObject owner (cmap)
	    (let ((el (js-object-ref owner i)))
	       (if (isa? el JsPropertyDescriptor)
		   el
		   (with-access::JsConstructMap cmap (props)
		      (let ((name (prop-name (vector-ref props i)))
			    (flags (prop-flags (vector-ref props i))))
			 (instantiate::JsValueDescriptor
			    (writable (flags-writable? flags))
			    (enumerable (flags-enumerable? flags))
			    (configurable (flags-configurable? flags))
			    (name name)
			    (value el))))))))
      ;; hash search
      (lambda (owner e)
	 (let ((d (cell-ref e)))
	    (if (isa? d JsPropertyDescriptor)
		d
		(let ((nd (instantiate::JsValueDescriptor
			     (writable #t)
			     (enumerable #t)
			     (configurable #t)
			     (name (js-toname p %this))
			     (value d))))
		   (cell-set! e nd)
		   nd))))
      ;; property search
      (lambda (owner d i)
	 d)
      ;; not found
      (lambda (o)
	 (if (and (js-jsstring? p) (js-jsstring-private? p))
	     (js-raise-type-error %this "Cannot read private member ~s" p)
	     (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property-descriptor ...                               */
;*---------------------------------------------------------------------*/
(define-generic (js-get-own-property-descriptor o::obj p::obj %this::JsGlobalObject)
   ;; This is not compatible with the recent semantics.
   ;; https://www.ecma-international.org/ecma-262/#sec-object.getownpropertydescriptor
   (let ((o (js-cast-object o %this "getOwnPropertyDescriptor")))
      (let ((desc (js-get-own-property o p %this)))
	 (js-from-property-descriptor %this p desc o))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property-descriptor ::JsObject ...                    */
;*    -------------------------------------------------------------    */
;*    This must be overriden for all its subclasses!                   */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property-descriptor o::JsObject p::obj %this::JsGlobalObject)
   (jsobject-find o o (js-toname p %this)
      ;; cmap search
      (lambda (owner i)
	 (with-access::JsObject owner (cmap)
	    (let ((el (js-object-ref owner i)))
	       (if (isa? el JsPropertyDescriptor)
		   (js-from-property-descriptor %this p el o)
		   (with-access::JsConstructMap cmap (props)
		      (let ((name (prop-name (vector-ref props i)))
			    (flags (prop-flags (vector-ref props i))))
			 (js-property-descriptor %this #t
			    :writable (flags-writable? flags)
			    :enumerable (flags-enumerable? flags)
			    :configurable (flags-configurable? flags)
			    :value el)))))))
      ;; hash search
      (lambda (owner e)
	 (let ((d (cell-ref e)))
	    (if (isa? d JsPropertyDescriptor)
		(js-from-property-descriptor %this p d o)
		(with-access::JsConstructMap cmap (props)
		   (let ((name (js-toname p %this)))
		      (js-property-descriptor %this #t
			 :writable #t
			 :enumerable #t
			 :configurable #t
			 :value d))))))
      ;; property search
      (lambda (owner d i)
	 (js-from-property-descriptor %this p d o))
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
;*    js-get-property-value/string ::obj ...                           */
;*    -------------------------------------------------------------    */
;*    A variant of js-get-property-value that does not use             */
;*    hashed property names.                                           */
;*---------------------------------------------------------------------*/
(define (js-get-property-value/string o::obj base p::JsStringLiteral %this::JsGlobalObject)
   (if (and (js-object? o) (eq? (object-class o) JsObject))
       (js-get-jsobject-property-value/string o base p %this)
       (js-get-property-value o base p %this)))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject-property-value ...                               */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject-property-value o::JsObject base p::obj %this::JsGlobalObject)
   ;; JsObject x obj x JsGlobalObject -> value | Absent
   (jsobject-find o o (js-toname p %this)
      ;; cmap search
      (lambda (owner i)
	 (let ((e (js-object-ref owner i)))
	    (if (isa? e JsPropertyDescriptor)
		(js-property-value base owner p e %this)
		e)))
      ;; hash search
      (lambda (owner e)
	 (let ((d (cell-ref e)))
	    (if (isa? d JsPropertyDescriptor)
		(js-property-value base o p d %this)
		d)))
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
;*    js-get-jsobject-property-value/string ...                        */
;*    -------------------------------------------------------------    */
;*    A variant of js-get-jsobject-property-value that does not use    */
;*    hashed property names.                                           */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject-property-value/string o::JsObject base p::JsStringLiteral %this::JsGlobalObject)
   ;; JsObject x obj x JsGlobalObject -> value | Absent
   (jsobject-find/string o o (js-jsstring->string p)
      ;; cmap search
      (lambda (owner i)
	 (let ((e (js-object-ref owner i)))
	    (if (isa? e JsPropertyDescriptor)
		(js-property-value base owner p e %this)
		e)))
      ;; hash search
      (lambda (owner e)
	 (let ((d (cell-ref e)))
	    (if (isa? d JsPropertyDescriptor)
		(js-property-value base o p d %this)
		d)))
      ;; property search
      (lambda (owner d i)
	 (js-property-value base o p d %this))
      ;; not found
      (lambda (o)
	 (js-absent))
      ;; prototype search
      (lambda (__proto__)
	 (if (and (js-object? __proto__) (eq? (object-class __proto__) JsObject))
	     (js-get-jsobject-property-value/string __proto__ base p %this)
	     (js-get-property-value __proto__ base p %this)))))

;*---------------------------------------------------------------------*/
;*    js-get-notfound ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-get-notfound name throw %this)
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
      ((null? o)
       (js-get-null o (js-toname prop %this) %this))
      ((number? o)
       (let ((obj (if (bignum? o)
		      (js-bigint->jsbigint o %this)
		      (js-number->jsNumber o %this))))
	  (js-get-jsobject obj o prop %this)))
      ((pair? o)
       (js-get-pair o (js-toname prop %this) %this))
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
;*    js-get-jsobject-hashed ...                                       */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject-hashed o::JsObject base prop::JsStringLiteral %this)
   (jsobject-hash-find/string o (js-jsstring->string prop)
      (lambda (owner e)
	 (let ((d (cell-ref e)))
	    (if (isa? d JsPropertyDescriptor)
		(js-property-value base o prop d %this)
		d)))
      (lambda ()
	 (let* ((__proto__ (js-object-proto o))
		(pval (if (>u32 (js-jsstring-length prop) (hash-object-name-threshold))
			  (js-get-property-value/string __proto__ base prop %this)
			  (js-get-property-value __proto__ base prop %this))))
	    (if (eq? pval (js-absent))
		(js-undefined)
		pval)))))

;*---------------------------------------------------------------------*/
;*    js-get/debug ...                                                 */
;*    -------------------------------------------------------------    */
;*    Instrumented version of js-get to provide information about      */
;*    potential type errors.                                           */
;*---------------------------------------------------------------------*/
(define (js-get/debug o prop %this::JsGlobalObject loc)
   (when *profile-cache*
      (cond
	 ((js-jsstring? prop)
	  (js-profile-log-get prop loc))
	 ((number? prop)
	  (unless (isa? o JsArray)
	     (js-profile-log-get
		(js-ascii-name->jsstring (number->string prop)) loc)))))
   (cond
      ((js-object? o)
       (js-get o prop %this))
      ((null? o)
       (js-get-null o (js-toname prop %this) %this))
      ((number? o)
       (let ((obj (if (bignum? o)
		      (js-bigint->jsbigint o %this)
		      (js-number->jsNumber o %this))))
	  (js-get-jsobject obj o prop %this)))
      ((pair? o)
       (js-get-pair o (js-toname prop %this) %this))
      ((object? o)
       ;; see toobject
       (js-get o prop %this))
      (else
       (let ((obj (js-toobject-for-property/debug %this loc o prop)))
	  (js-get-jsobject obj o prop %this)))))

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
      ((isa? o JsString)
       (fixnum->uint32 (js-get-length o %this cache)))
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
      ((and (fixnum? prop) (js-array? o))
       (js-array-ref o prop %this))
      ((js-object? o)
       (%js-get-jsobject/name-cache o prop %this))
      (else
       (js-get o prop %this))))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject/name-cache ...                                   */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject/name-cache o prop %this)
   ;; BGl_jszd2getzd2jsobjectzf2namezd2cachez20zz__hopscript_propertyz00
   (%js-get-jsobject/name-cache o prop %this))

;*---------------------------------------------------------------------*/
;*    %js-get-jsobject/name-cache ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (%js-get-jsobject/name-cache o prop %this)
   (cond
      ((js-jsstring? prop)
       (cond
	  ((js-object-hashed? o)
	   (js-get-jsobject-hashed o o prop %this))
	  ((js-jsstring-index? prop)
	   (js-get o prop %this))
	  (else
	   (synchronize-name
	      (let ((pname (js-jsstring-toname-unsafe prop)))
		 (cond
		    ((js-name-pcacher pname)
		     =>
		     (lambda (cache)
			(js-get-jsobject-name/cache o pname #f
			   %this
			   cache -2 '(imap emap cmap pmap amap xmap vtable))))
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
			   cache -3 '())))))))))
      ((js-array? o)
       (js-array-ref o prop %this))
      ((eq? (object-class o) JsArguments)
       (js-arguments-ref o prop %this))
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
   (js-get-jsobject-name/cache o name throw %this cache point '*))

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
      (with-access::JsPropertyCache cache (cmap pmap amap pindex cindex aindex owner)
	 (cond
	    ((eq? pmap omap)
	     (js-object-ref owner pindex))
	    ((eq? cmap omap)
	     (js-object-noinline-relative-ref o cindex))
	    ((eq? amap omap)
	     (let ((desc (js-object-ref owner aindex)))
		(js-property-value o owner name desc %this)))
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
   
   (with-access::JsPropertyCache cache (cntmiss (cname name) (cpoint point))
      (set! cntmiss (+u32 #u32:1 cntmiss)))

;*    (with-access::JsPropertyCache cache (src point)                  */
;*       (let ((s (getenv "DEBUG")))                                   */
;* 	 (when (and (string? s)                                        */
;* 		    (string-contains s (js-tostring name %this)))      */
;* 	     (tprint "MISS name=" name " " point ":" src)              */
;* 	     (js-debug-object o)                                       */
;* 	     (js-debug-pcache cache))))                                */
   
   (let loop ((obj o))
      (jsobject-find obj o name
	 ;; map search
	 (lambda (obj i)
	    (with-access::JsPropertyCache cache (index owner cntmiss)
	       (let ((el-or-desc (js-object-ref obj i)))
		  (js-assert-object obj "js-get-jsobject-name/cache-miss")
		  (cond
		     ((isa? el-or-desc JsPropertyDescriptor)
		      ;; accessor property
		      (if (eq? o obj)
			  (js-pcache-update-accessor! cache i o)
			  (js-pcache-update-descriptor! cache i o obj))
		      (js-property-value o obj name el-or-desc %this))
		     ((eq? o obj)
		      ;; direct access to the direct object
		      (cond
			 ((<u32 cntmiss (vtable-threshold))
			  (js-pcache-update-get-direct! cache i obj name))
			 ((not (eq? prop (& "__proto__")))
			  (js-pcache-update-get-direct! cache i obj name)			  
			  (with-access::JsObject o (cmap)
			     (js-pcache-vtable! cache cmap i %this))))
		      el-or-desc)
		     (else
		      ;; direct access to a prototype object
		      (unless (js-proxy? obj)
			 (js-pcache-update-get-prototype! cache i o obj))
		      el-or-desc)))))
	 ;; hash search
	 (lambda (obj e)
	    (let ((d (cell-ref e)))
	       (if (isa? d JsPropertyDescriptor)
		   (js-property-value o obj name d %this)
		   d)))
	 ;; property search
	 (lambda (obj desc i)
	    (js-property-value o obj name desc %this))
	 ;; not found
	 (lambda (_o)
	    (with-access::JsObject o (cmap)
	       (cond
		  ((or (eq? cmap (js-not-a-cmap)) throw)
		   (js-get-notfound name throw %this))
		  ((and (js-jsstring? name) (js-jsstring-private? name))
		   (js-raise-type-error %this "Cannot read private member ~s" name))
		  (else
		   (js-pcache-update-miss! cache o)
		   (js-undefined)))))
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
   
   (define (invalidate-pcache! cache)
      (with-access::JsPropertyCache cache (pmap emap cmap)
	 (set! pmap (js-not-a-pmap))
	 (set! emap (js-not-a-pmap))
	 (set! cmap (js-uncachable-pmap))))
   
   (with-access::JsPropertyCache cache (cntmiss (cname name) (cpoint point))
      (set! cntmiss (+u32 #u32:1 cntmiss)))

   (let loop ((obj o))
      (jsobject-find obj o name
	 ;; map search
	 (lambda (obj i)
	    (with-access::JsObject o ((omap cmap) __proto__)
	       (with-access::JsObject obj ((wmap cmap))
		  (with-access::JsConstructMap wmap (methods)
		     (let ((el-or-desc (js-object-ref obj i)))
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
					    ;; vtable
					    (cond
					       ((<u32 cntmiss (vtable-threshold))
						(unless (js-proxy? obj)
						   (js-pcache-update-get-prototype! cache i o obj)))
					       (else
						(js-pcache-vtable! cache omap f %this)))))))
			       f))
			   (else
			    (invalidate-pcache! cache)
			    (funval obj el-or-desc))))))))
	 ;; hash search
	 (lambda (obj e)
	    (let ((d (cell-ref e)))
	       (invalidate-pcache! cache)
	       (if (isa? d JsPropertyDescriptor)
		   (js-property-value o obj name d %this)
		   d)))
	 ;; property search
	 (lambda (obj v i)
	    (invalidate-pcache! cache)
	    (js-property-value o obj name v %this))
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
      '(pmap cmap amap vtable)))

;*---------------------------------------------------------------------*/
;*    js-can-put ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.4       */
;*---------------------------------------------------------------------*/
(define (js-can-put o::JsObject p::obj %this::JsGlobalObject)

   (define (js-get-inherited-property o::JsObject name::obj)
      (jsobject-find o o name
	 (lambda (o i) i)
	 (lambda (o e) 0)
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
	      (js-raise-type-error %this
		 (format "[[PUT]]: not an object ~~s (~s)" prop)
		 _o))))))

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
	 (if (js-procedure? set)
	     ;; 8.12.5, step 5
	     (begin
		(when (and (>=fx index 0) cache)
		   (with-access::JsObject propobj (cmap)
		      (unless (eq? cmap (js-not-a-cmap))
			 (if (eq? o propobj)
			     (js-pcache-update-accessor! cache index propobj)
			     (js-pcache-update-descriptor! cache index o propobj)))))
		(%set o v)
		v)
	     ;; 8.12.4, setp 2.a
	     (reject "No setter defined"))))
   
   (define (update-from-wrapper-descriptor! o propobj v desc)
      (js-property-value-set! o propobj prop desc v %this))
   
   (define (update-mapped-object-value! obj cmap i v)
      (with-access::JsObject obj (cmap)
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
		       (js-pcache-update-put-direct! cache i obj))
		    (js-object-set! obj i v)
		    v)
		   (else
		    ;; invalidate cache method and re-cache
		    (cond
		       ((js-object-mode-isprotoof? obj)
			;; MS: 31mar2023, I'm not totally sure about this one as pmap
			;; cache entries are used to search the prototype chain and also
			;; to implement cached fast method calls.
			(js-invalidate-cache-pmap-method! %this cmap i
			   "update-mapped with new function" name))
		       (cache
			(with-access::JsPropertyCache cache (pmap)
			   (set! pmap (js-not-a-pmap)))))
		    (reset-cmap-vtable! cmap "update-mapped" name)
		    (when cache
		       (js-pcache-update-put-direct! cache i obj))
		    (js-object-set! obj i v)
		    v)))
	       ((js-function? (vector-ref methods i))
		;; invalidate cache method and cache
		(js-invalidate-cache-pmap-method! %this cmap i
		   "update-mapped with non function" name)
		(reset-cmap-vtable! cmap "update-mapped:function" name)
		(when cache
		   (js-pcache-update-put-direct! cache i obj))
		(js-object-set! obj i v)
		v)
	       (else
		;; normal caching
		(when cache
		   (with-access::JsPropertyCache cache (cntmiss)
		      (js-pcache-update-put-direct! cache i obj)))
		(js-object-set! obj i v)
		v)))))
   
   (define (update-mapped-object! obj i)
      (with-trace 'prop "update-mapped-object"
	 (trace-item "name=" name)
	 (with-access::JsObject obj (cmap)
	    (with-access::JsConstructMap cmap (nextmap methods props)
	       (let ((el-or-desc (js-object-ref obj i)))
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
				    (js-pcache-update-accessor! cache i o))
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
					  update-hashed-object!
					  update-properties-object!
					  extend-object!
					  loop)))
				 (reject "Illegal object"))
			     (begin
				(when cache
				   (if (js-proxy? obj)
				       (js-pcache-update-accessor! cache i obj)
				       (js-pcache-update-accessor! cache i o)))
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
      (with-trace 'prop "extend-mapped-object/nmap"
	 (trace-item "v=" (typeof v) " index=" index)
	 (with-access::JsObject o (cmap)
	    ;; follow the next map
	    (let ((nextmap nmap))
	       (with-access::JsConstructMap nextmap (ctor methods props detachcnt detachlocs)
		  (cond
		     ((or (not cachefun) (not (js-function? v)))
		      (when (js-function? (vector-ref methods index))
			 ;; invalidate cache method and cache
			 (cond
			    ((js-object-mode-isprotoof? o)
			    (js-invalidate-cache-pmap-method! %this nextmap index
			     "extend-mapped with non-function" v))
			    (cache
			     ;; see update-mapped-object-value
			     (with-access::JsPropertyCache cache (pmap)
				(set! pmap (js-not-a-pmap)))))
			 (reset-cmap-vtable! nextmap "extend-mapped" v))
		      (when cache
			 (js-pcache-update-next-direct! cache o nextmap index
			    (flags-inline? flags))))
		     ((eq? v (vector-ref methods index))
		      (when cache
			 (js-pcache-update-next-direct! cache o nextmap index
			    (flags-inline? flags))))
		     ((eq? (vector-ref methods index) #f)
		      ;; invalidate the pmap caches as it might
		      ;; be that this function will be now used
		      ;; when searching for a prototype chain
		      (when cache
			 (js-pcache-update-next-direct! cache o nextmap index
			    (flags-inline? flags))))
		     ((or (>=fx detachcnt (method-invalidation-threshold))
			  (memq loc detachlocs))
		      ;; MS 2019-01-19
		      ;; on method conflicts, instead of
		      ;; invalidating all methods,
		      ;; a new cmap is created. see
		      ;; see the prototype initialization
		      ;; in js-make-function@function.scm
		      ;; invalidate cache method and cache
		      (js-invalidate-cache-pmap-method! %this nextmap index
			 "extend-mapped polymorphic threshold" name)
		      (reset-cmap-vtable! nextmap "extend-mapped" name)
		      (when cache
			 (js-pcache-update-next-direct! cache o nextmap index
			    (flags-inline? flags))))
		     (else
		      (let ((detachedmap (extend-cmap cmap name flags)))
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
		  (set! cmap nextmap)
		  (js-object-put-push! o index v)
		  (js-assert-object o "js-put-jsobject.extend/mapped")
		  v)))))
   
   (define (extend-mapped-object!)
      (with-trace 'prop "extend-mappend-object!"
	 ;; 8.12.5, step 6
	 (with-access::JsObject o (cmap)
	    (with-access::JsConstructMap cmap (props single lock %id)
	       (let ((index (vector-length props)))
		  (trace-item "name=" name " index=" index
		     " ilen=" (js-object-inline-length o))
		  (if (and (=fx index (hash-object-threshold))
			   (js-object-hashable? o))
		      (begin
			 (js-object-hash! o)
			 (extend-hashed-object!))
		      (let ((flags (property-flags writable enumerable configurable #f
				      (<fx index (js-object-inline-length o)))))
			 (js-object-mode-enumerable-set! o #t)
			 (when (and (js-jsstring? name) (js-jsstring->number name))
			    (js-object-mode-hasnumeralprop-set! o #t))
			 (when (js-object-mode-isprotoof? o)
			    (js-invalidate-pmap-pcaches! %this "extend-mapped.5" name))
			 (synchronize lock
			    (cond
			       ((cmap-find-transition cmap name v flags)
				=>
				(lambda (nmap)
				   (extend-mapped-object/nmap nmap index flags)))
			       (single
				(with-trace 'prop "extend-mapped-object.single"
				   (extend-cmap! cmap name flags)
				   (with-access::JsConstructMap cmap (ctor methods)
				      (if (and cachefun (js-function? v))
					  ;; validate cache method and don't cache
					  (vector-set! methods index v)
					  (begin
					     (js-invalidate-cache-pmap-method! %this cmap index
						"extend-mapped single non-function" name)
					     (when cache
						(js-pcache-update-next-direct! cache o cmap index
						   (flags-inline? flags)))))
				      (js-object-put-push! o index v)
				      (js-assert-object o "js-put-jsobject-extend1"))
				   v))
			       (else
				(with-trace 'prop "extend-mapped-object.newmap"
				   ;; create a new map
				   (let ((nextmap (extend-cmap cmap name flags)))
				      (with-access::JsConstructMap nextmap (methods ctor)
					 (if (and cachefun (js-function? v))
					     ;; validate cache method and don't cache
					     (vector-set! methods index v)
					     ;; invalidate cache method and cache
					     (begin
						(js-invalidate-cache-pmap-method! %this nextmap index
						   "extend-mapped non-function" name)
						(when cache
						   (js-pcache-update-next-direct! cache o nextmap index
						      (flags-inline? flags)))))
					 (link-cmap! cmap nextmap name v flags)
					 (set! cmap nextmap)
					 (js-object-put-push! o index v)
					 (js-assert-object o "js-put-jsobject.extend2")
					 v)))))))))))))
   
   (define (update-hashed-object! obj prop)
      (with-trace 'prop "update-hashed-object"
	 (trace-item "name=" name)
	 (let ((desc (cell-ref prop)))
	    (cond
	       ((and (not override) (isa? desc JsAccessorDescriptor))
		;; 8.12.5, step 5
		(update-from-descriptor! o obj -1 v desc))
	       ((eq? o obj)
		;; 8.12.5, step 3
		(let ((owndesc desc))
		   (if (not (isa? owndesc JsPropertyDescriptor))
		       (begin
			  (cell-set! prop v)
			  v)
		       (with-access::JsDataDescriptor owndesc (writable name)
			  (if (not writable)
			      ;; 8.12.4, step 2.b
			      (reject "Read-only property")
			      ;; 8.12.5, step 3,b
			      (js-property-value-set! o o name owndesc v %this))))))
	       ((isa? desc JsWrapperDescriptor)
		(let ((v (update-from-wrapper-descriptor! o obj v desc)))
		   (cond
		      ((eq? v (js-absent))
		       (if (js-proxy? obj)
			   (let ((target (js-proxy-target obj)))
			      (let loop ((obj target))
				 (jsobject-find obj target name
				    update-mapped-object!
				    update-hashed-object!
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
		       (reject "Read-only property"))))))))
   
   (define (extend-hashed-object!)
      (with-trace 'prop "extend-hashed-object!"
	 (js-object-mode-enumerable-set! o #t)
	 (when (and (js-jsstring? name) (js-jsstring->number name))
	    (js-object-mode-hasnumeralprop-set! o #t))
	 (when (js-object-mode-isprotoof? o)
	    (js-invalidate-pmap-pcaches! %this "extend-hashed" name))
	 (let ((descv (if (and writable enumerable configurable)
			  v
			  (instantiate::JsValueDescriptor
			     (name name)
			     (value v)
			     (writable #t)
			     (enumerable #t)
			     (configurable #t)))))
	    (with-access::JsObject o ((table elements))
	       (prop-hashtable-put! table (js-jsstring->string name)
		  (make-cell descv))))
	 v))
   
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
				 update-hashed-object!
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
	 (trace-item "o=" (typeof o) " name=" name)
	 (let ((newdesc (instantiate::JsValueDescriptor
			   (name name)
			   (value v)
			   (writable #t)
			   (enumerable #t)
			   (configurable #t))))
	    (when (js-object-mode-isprotoof? o)
	       (js-invalidate-pmap-pcaches! %this "extend-property" name))
	    (js-define-own-property o name newdesc throw %this)
	    v)))

   (define (sealed-typeof o)
      (let ((n (typeof o)))
	 (if (string-prefix? "&JsRec" n)
	     (substring n 6)
	     n)))
   
   (define (extend-object! _o)
      (with-trace 'prop "extend-object!"
	 (trace-item "name=" name)
	 (with-access::JsObject o (cmap)
	    (cond
	       ((not (js-object-mode-extensible? o))
		;; 8.12.9. step 3
		(reject (format "sealed object (~s)" (sealed-typeof o))))
	       ((not extend)
		;; 11.13.1
		(js-raise-reference-error/loc %this loc
		   "[[PUT]], \"~a\" is not defined" name))
	       ((and (js-jsstring? name) (js-jsstring-private? name))
		(js-raise-type-error %this "Cannot write private member ~s" name))
	       ((js-object-mapped? o)
		;; 8.12.5, step 6
		(extend-mapped-object!))
	       ((js-object-hashed? o)
		;; 8.12.5, step 6
		(if (js-jsstring? name)
		    (extend-hashed-object!)
		    (begin
		       (js-object-unhash! o)
		       (extend-properties-object!))))
	       (else
		;; 8.12.5, step 6
		(extend-properties-object!))))))

   ;;(tprint "JS-PUT-JSOBJECT " name)
   (check-unplain! o name)
   (let loop ((obj o))
      (jsobject-find obj o name
	 update-mapped-object!
	 update-hashed-object!
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
       (let ((o (js-toobject-for-property/debug %this loc _o prop)))
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
   (js-put-jsobject/cache! o prop v throw %this point cspecs src cachefun))

;*---------------------------------------------------------------------*/
;*    js-put-jsobject/cache! ...                                       */
;*---------------------------------------------------------------------*/
(define (js-put-jsobject/cache! o::JsObject prop v::obj throw::bool %this
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
				 (name pname)
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
		   %this cache point '(imap emap cmap nmap pmap amap vtable) #f)))
	    ((eq? pname (& "length"))
	     (js-put-length! o v throw #f %this))
	    ((isa? pname JsStringLiteralIndex)
	     (js-put! o prop v throw %this))
	    (else
	     (let ((cache (instantiate::JsPropertyCache
			     (usage 'dput)
			     (name pname)
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
   
   (define (profile-cache-miss! cache prop point)
      (with-access::JsPropertyCache cache (cntmiss name (cpoint point) usage)
	 (set! cntmiss (+u32 #u32:1 cntmiss))
	 (set! name prop)
	 (set! cpoint point)))
   
   (define (cache-vtable-add! o %omap cache)
      (with-access::JsPropertyCache cache (cindex vindex
					     imap iindex
					     cmap cindex
					     emap eindex
					     nmap nindex
					     pmap pindex)
	 (let* ((omap (with-access::JsObject o (cmap) cmap))
		(vtindex (cond
			    ((eq? omap imap) iindex)
			    ((eq? omap cmap) cindex)
			    ((eq? omap emap) eindex)
			    ((eq? omap nmap) nindex)
			    ((eq? omap pmap) pindex)
			    (else -1))))
	    (when (>=fx vtindex 0)
	       (when (=fx vindex (js-not-a-index))
		  (set! vindex (js-get-vindex %this)))
	       (js-cmap-vtable-add! %omap vindex (cons vtindex omap) cache)))))

   (with-access::JsObject o (cmap)
      (let* ((%omap cmap)
	     (res (js-put-jsobject! o prop v throw %this
		     :extend #t :override #f :cache cache
		     :loc point :cachefun cachefun)))
	 (profile-cache-miss! cache prop point)
	 (unless (or (eq? %omap (js-not-a-cmap)) (eq? prop (& "__proto__")))
	    (with-access::JsPropertyCache cache (cntmiss)
	       (when (and (>=u32 cntmiss (vtable-threshold))
			  (<=u32 cntmiss (vtable-max-threshold)))
		  (cache-vtable-add! o %omap cache))))
	 res)))

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
	 ((not (or (js-procedure? get) (eq? get (js-undefined))))
	  (js-raise-type-error %this
	     (format "wrong getter for property \"~a\", ~~a" name)
	     get))
	 ((and set (not (eq? set (js-undefined))) (not (js-procedure? set)))
	  (js-raise-type-error %this
	     (format "wrong setter for property \"~a\", ~~a" name) set))))
   
   (define (update-mapped-object! obj i)
      (with-trace 'prop "update-mapped-object"
	 (trace-item "obj=" (typeof obj) " i=" i)
	 (cond
	    ((not (eq? obj o))
	     (error "js-bind!"
		(format "cannot rebind mapped prototype property \"~a\"" name)
		(typeof o)))
	    ((or get set)
	     (let ((old (js-object-ref o i)))
		(when (isa? old JsAccessorDescriptor)
		   (with-access::JsAccessorDescriptor old ((oget get)
							   (oset set))
		      (when (eq? get (js-undefined))
			 (set! get oget))
		      (when (eq? set (js-undefined))
			 (set! set oset))))
		(js-object-set! o i
		   (instantiate::JsAccessorDescriptor
		      (name name)
		      (get get)
		      (set set)
		      (%get (function0->proc get %this))
		      (%set (function1->proc set %this))
		      (enumerable enumerable)
		      (configurable configurable)))))
	    (else
	     (js-object-set! o i value)
	     value))))
   
   (define (next-cmap o::JsObject name value flags)
      (with-access::JsObject o (cmap)
	 (with-access::JsConstructMap cmap (single)
	    (if (and hidden-class (not single))
		(let ((nextmap (extend-cmap cmap name flags)))
		   (link-cmap! cmap nextmap name value flags)
		   (set! cmap nextmap)
		   nextmap)
		(begin
		   (extend-cmap! cmap name flags)
		   cmap)))))
   
   (define (extend-mapped-object!)
      (with-trace 'prop "extend-mapped-object"
	 (when enumerable
	    (js-object-mode-enumerable-set! o #t))
	 (when (and (js-jsstring? name) (js-jsstring->number name))
	    (js-object-mode-hasnumeralprop-set! o #t))
	 ;; 8.12.5, step 6
	 (with-access::JsObject o (cmap)
	    (with-access::JsConstructMap cmap (props lock)
	       (synchronize lock
		  (let* ((axs (accessor-property? get set))
			 (index (vector-length props))
			 (flags (property-flags
				   writable enumerable configurable axs
				   (<fx index (js-object-inline-length o)))))
		     (trace-item "index=" index
			" ilen=" (js-object-inline-length o))
		     (cond
			((cmap-find-transition cmap name value flags)
			 =>
			 (lambda (nmap)
			    (let ((val-or-desc (if (accessor-property? get set)
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
			       (with-access::JsConstructMap nmap (props)
				  (js-object-bind-push! o index val-or-desc)
				  (set! cmap nmap)
				  (js-assert-object o "js-put-jsobject.4")
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
				(nmap (next-cmap o name #f flags))
				(oi (js-object-inline-next-elements? o index))
				(os (with-access::JsConstructMap cmap (single) single)))
			    (check-accessor-property! get set)
			    ;; extending the elements vector is mandatory
			    (js-object-bind-push! o index newdesc)
			    (set! cmap nmap)
			    (js-assert-object o "js-put-jsobject.5")
			    (js-undefined)))
			(else
			 (let ((nmap (next-cmap o name value flags)))
			    (with-access::JsConstructMap nmap (methods)
			       (validate-cache-method! value methods index))
			    (js-object-bind-push! o index value)
			    (set! cmap nmap)
			    (js-assert-object o "js-put-jsobject.6")
			    value)))))))))
   
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
	  (let ((desc (instantiate::JsValueDescriptor
			 (name name)
			 (value value)
			 (writable writable)
			 (enumerable enumerable)
			 (configurable configurable))))
	     (js-object-set! obj i desc)))
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

   (with-trace 'prop "js-bind!"
      (trace-item "o=" (typeof o) " name=" name
	 " ilen=" (js-object-inline-length o)
	 " vlen=" (vector-length (js-object-noinline-elements o)))
      (with-access::JsObject o (cmap)
	 (cond
	    ((js-object-mapped? o)
	     (jsobject-map-find o name
		update-mapped-object!
		extend-mapped-object!))
	    ((js-object-hashed? o)
	     (error "js-bind!" "js-bind cannot be used on hashed object" name))
	    (else
	     (jsobject-properties-find o name
		update-properties-object!
		extend-properties-object!))))))

;*---------------------------------------------------------------------*/
;*    js-define ...                                                    */
;*    -------------------------------------------------------------    */
;*    Wrapper to js-bind! used to keep generated files smaller.        */
;*---------------------------------------------------------------------*/
(define (js-define %this obj id::JsStringLiteral
	   get set src pos #!key (hidden-class #t))
   (let ((name (string-append src ":" (integer->string pos))))
      (js-bind! %this obj id
	 :configurable #f
	 :hidden-class hidden-class
	 :get (js-make-function %this get
		 (js-function-arity get)
		 (js-function-info :name name :len 1))
	 :set (when set
		 (js-make-function %this set
		    (js-function-arity set)
		    (js-function-info :name name :len 2))))))

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

   (define (check-cmap-parent cmap n)
      (with-access::JsConstructMap cmap (parent transitions)
	 (with-access::JsConstructMap parent ((ptransitions transitions))
	    (when (=fx (+fx 1 (length ptransitions)) (length transitions))
	       (let loop ((ptransitions ptransitions))
		  (when (pair? ptransitions)
		     (let ((tr (car ptransitions)))
			(if (and (eq? (transition-name tr) n)
				 (eq? (transition-nextmap tr) cmap))
			    parent
			    (loop (cdr ptransitions))))))))))
   
   (define (vector-delete! v i)
      (vector-copy! v i v (+fx i 1))
      (vector-shrink! v (-fx (vector-length v) 1)))

   (let ((n (js-toname p %this))
	 (o (js-toobject %this _o)))
      (cond
	 ((js-object? o)
	  (with-access::JsObject o (cmap)
	     (cond
		((js-object-mapped? o)
		 (jsobject-map-find o n
		    (lambda (o i)
		       (delete-configurable o
			  (configurable-mapped-property? o i)
			  (lambda (o)
			     (when (js-object-mode-isprotoof? o)
				(js-invalidate-pmap-pcaches! %this "js-delete" p))
			     (cond
				((cmap-find-transition cmap n #f -1)
				 =>
				 (lambda (prevmap)
				    ;; re-use the existing previous map for that deletion
				    (set! cmap prevmap) 
				    (js-assert-object o "js-delete!.1")
				    #t))
				((check-cmap-parent cmap n)
				 =>
				 (lambda (parentmap)
				    ;; re-use the existing previous map for that deletion
				    (set! cmap parentmap)
				    (js-assert-object o "js-delete!.2")
				    #t))
				(else
				 ;; create a new cmap for the object
				 (let ((nextmap (clone-cmap cmap)))
				    (link-cmap! cmap nextmap n #f -1)
				    [assert (o) (isa? nextmap JsConstructMap)]
				    (set! cmap nextmap)
				    (js-assert-object o "js-delete!.3")
				    (with-access::JsConstructMap nextmap (props)
				       ;; remove the prop from the cmap
				       (if (=fx i (-fx (vector-length props) 1))
					   (vector-shrink! props (-fx (vector-length props) 1))
					   (vector-set! props i (prop #f 0)))
				       #t)))))))
		    (lambda () #t)))
		((js-object-hashed? o)
		 (jsobject-hash-find o n
		    (lambda (o e)
		       (let ((d (cell-ref e)))
			  (delete-configurable o
			     (if (isa? d JsPropertyDescriptor)
				 (with-access::JsPropertyDescriptor d (configurable)
				    configurable)
				 #t)
			     (lambda (o)
				(with-access::JsObject o (elements)
				   (hashtable-remove! elements
				      (js-jsstring->string n)))
				#t))))
		    (lambda () #t)))
		(else
		 (jsobject-properties-find o n
		    (lambda (o d i)
		       (with-access::JsPropertyDescriptor d (configurable)
			  (delete-configurable o configurable
			     (lambda (o)
				(with-access::JsObject o (elements)
				   (vector-delete! elements i))
				#t))))
		    (lambda () #t))))))
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
	    (vector-map! (lambda (p)
			    (if (eq? p current) ncurrent p))
	       (js-object-inline-elements o))
	    (vector-map! (lambda (p)
			    (if (eq? p current) ncurrent p))
	       (js-object-noinline-elements o))
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

   (define (equal-undef? v1 v2)
      (or (equal? v1 v2) (and (eq? v1 #f) (eq? v2 #unspecified))))
   
   (define (same-value v1 v2)
      (cond
	 ((eq? v1 v2)
	  #t)
	 ((js-object? v1)
	  (when (js-object? v2)
	     (let ((len1 (js-object-length v1))
		   (len2 (js-object-length v2)))
		(when (=fx len1 len2)
		   (let loop ((i 0))
		      (cond
			 ((=fx i len1)
			  #t)
			 ((equal? (js-object-ref v1 i)
			     (js-object-ref v2 i))
			  (loop (+fx i 1)))
			 (else
			  #f)))))))
	 ((flonum? v1)
	  (or (and (flonum? v2)
		   (or (and (=fl v1 v2) (=fx (signbitfl v1) (signbitfl v2)))
		       (and (nanfl? v1) (nanfl? v2))))
	      (and (fixnum? v2) (=fl v1 (fixnum->flonum v2)))))
	 (else
	  (equal? v1 v2))))
   
   (define (same-property-descriptor? current::JsPropertyDescriptor desc::JsPropertyDescriptor)
      (with-access::JsPropertyDescriptor current
	    ((cname name) (cconf configurable) (cenum enumerable))
	 (with-access::JsPropertyDescriptor desc
	       ((dname name) (dconf configurable) (denum enumerable))
	    (and (eq? cname dname)
		 (or (not (boolean? dconf)) (eq? cconf dconf))
		 (or (not (boolean? denum)) (eq? cenum denum))))))

   (define (eq-unspecified? a b)
      (or (eq? a b)
	  (and (eq? a #f) (eq? b #unspecified))
	  (and (eq? a #unspecified) (eq? b #f))))
   
   (define (same-accessor-descriptor? current::JsAccessorDescriptor desc::JsAccessorDescriptor)
      (with-access::JsAccessorDescriptor current ((cget get) (cset set))
	 (with-access::JsAccessorDescriptor desc ((dget get) (dset set))
	    ;; see ch15/15.2/15.2.3/15.2.3.6/15.2.3.6-4-6.js
	    ;; and ch15/15.2/15.2.3/15.2.3.6/15.2.3.6-4-19.js
	    (and (eq-unspecified? cget dget) (eq-unspecified? cset dset)))))
   
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

   (define (define-own-property-extend-accessor o name desc)
      (with-access::JsObject o (cmap)
	 (with-access::JsConstructMap cmap (props lock)
	    (with-access::JsAccessorDescriptor desc
		  (value enumerable configurable)
	       (unless (boolean? enumerable)
		  (set! enumerable #f))
	       (unless (boolean? configurable)
		  (set! configurable #f))
	       (let* ((index (vector-length props))
		      (flags (property-flags
				#t enumerable configurable #t
				(<fx index (js-object-inline-length o))))
		      (nmap (cmap-find-transition cmap name desc flags)))
		  (if nmap
		      (set! cmap nmap)
		      (let ((nextmap (extend-cmap cmap name flags)))
			 (link-cmap! cmap nextmap name desc flags)
			 (set! cmap nextmap)))
		  (js-object-put-push! o index desc)
		  #t)))))
   
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
	  ((isa? desc JsAccessorDescriptor)
	   ;; MS 30mar2023
	   (define-own-property-extend-accessor o name desc))
	  (else
	   (js-object-unmap! o)
	   (define-own-property-extend-unmapped o name desc))))

   (define (define-own-property-extend-hashed o name desc)
      (when (js-object-mode-isprotoof? o)
	 (js-invalidate-pmap-pcaches! %this
	    "define-own-property-extend-hashed" name))
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
	    (prop-hashtable-put! elements (js-jsstring->string name)
	       (make-cell ndesc)))
	 #t))

   (define (define-own-property-extend-unmapped o name desc)
      (when (js-object-mode-isprotoof? o)
	 (js-invalidate-pmap-pcaches! %this
	    "define-own-property-extend-unmapped" name))
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
	 (js-assert-object o "js-define-own-property%")
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
	  ((js-object-hashed? o)
	   (if (js-jsstring? name)
	       (define-own-property-extend-hashed o name desc)
	       (begin
		  (js-object-unhash! o)
		  (define-own-property-extend-unmapped o name desc))))
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
		 (when (js-object-mapped? o)
		    (js-object-unmap! o)
		    (set! current (js-get-own-property o name %this)))
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
			       (unless (js-procedure? get)
				  (set! get (js-undefined)))
			       (unless (js-procedure? set)
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
			    ((and (set desc)
				  (not (equal-undef? (set current) (set desc))))
			     (reject
				(format "\"~a.~a\" setter mismatch"
				   (js-typeof o %this) name)))
			    ((and (get desc)
				  (not (equal-undef? (get current) (get desc))))
			     (tprint "CU=" (get current) " NW=" (get desc))
			     (reject
				(format "\"~a.~a\" getter mismatch"
				   (js-typeof o %this) name)))
			    (else
			     (propagate-accessor-descriptor! current desc)))
			 (propagate-accessor-descriptor! current desc)))
		    (else
		     (js-invalidate-pmap-pcaches! %this
			"js-define-own-property%, one accessor" name)
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
		(when (js-object-mode-isprotoof? o)
		   (js-invalidate-pmap-pcaches! %this "js-setprototypeof" "__proto__"))
		(unless (eq? cmap (js-not-a-cmap))
		   (with-access::JsConstructMap cmap (parent single)
		      (if single
			  (set! cmap
			     (duplicate::JsConstructMap cmap
				(%id (gencmapid))))
			  (set! cmap
			     (cmap-next-proto-cmap %this cmap
				   (js-object-proto o) v)))))
		(js-assert-object o "js-setprototypeof")
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
	     (property-flags writable enumerable configurable #f #f)))
	 ((isa? desc JsDataDescriptor)
	  (error "js-replace-own-property!" "not a dataproperty" new))
	 (else
	  (with-access::JsPropertyDescriptor desc (enumerable configurable)
	     (property-flags #t enumerable configurable #f #f)))))
   
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
   
   (cond
      ((js-object-mapped? o)
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
			     (loop parent)))))))))
      ((js-object-hashed? o)
       (with-access::JsObject o (elements)
	  (with-access::JsPropertyDescriptor new (name)
	     (if (js-jsstring? name)
		 (let ((e (prop-hashtable-get elements (js-jsstring->string name))))
		    (if (cell? e)
			(cell-set! e new)
			(js-replace-own-property! (js-object-proto o) old new)))
		 (js-replace-own-property! (js-object-proto o) old new)))))
      (else
       ;; update the unmapped object
       (with-access::JsObject o (elements)
	  (unless (replace-vector! elements old new)
	     (js-replace-own-property! (js-object-proto o) old new))))))

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

   (define (in-hash k c)
      (let ((p (cell-ref c)))
	 (if (isa? p JsPropertyDescriptor)
	     (with-access::JsPropertyDescriptor p (name enumerable)
		(when (js-jsstring? name)
		   (when (eq? enumerable #t)
		      (proc name %this))))
	     (proc (js-string->jsstring k) %this))))
   
   (define (in-property p)
      (when (isa? p JsPropertyDescriptor)
	 (with-access::JsPropertyDescriptor p (name enumerable)
	    (when (js-jsstring? name)
	       (when (eq? enumerable #t)
		  (unless (js-has-upto-property owner obj name %this)
		     (proc name %this)))))))
   
   (with-access::JsObject obj (cmap elements)
      (when (js-object-mode-enumerable? obj)
	 (cond
	    ((js-object-mapped? obj)
	     (with-access::JsConstructMap cmap (props)
		(vfor-in props)))
	    ((js-object-hashed? obj)
	     (with-access::JsObject obj (elements)
		(hashtable-for-each elements
		   in-hash)))
	    (else
	     (with-access::JsObject obj (elements)
		(vector-for-each in-property elements)))))
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
	     (when (js-procedure? fun)
		(js-for-of-iterator (js-call0-jsprocedure %this fun obj)
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
		       (when (js-procedure? return)
			  (js-call0-jsprocedure %this return iterator)))))
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
		  (with-access::JsFunction obj (arity info)
		     (and (>=fx arity 0) (=fx (vector-ref info 1) largs))))
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
	     ((and (or (js-object-function-tag? ,obj)
		       (js-object-procedure-tag? ,obj))
		   (=fx (js-procedure-arity ,obj) ,(+fx largs 1)))
	      (with-access::JsFunction ,obj (procedure)
		 (set! owner ,obj)
		 (set! method procedure)
		 (procedure ,this ,@args)))
	     ((js-proxy? ,obj)
	      ,(let ((call (symbol-append 'js-call-proxy/cache-miss
			      (string->symbol (integer->string largs)))))
		  `(,call ,%this ,obj ,this ,@args)))
	     ((and (or (js-object-function-tag? ,obj)
		       (js-object-procedure-tag? ,obj))
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
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
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
	     (with-access::JsConstructMap omap (vtable %id)
		(let ((vtable vtable))
		   (if (<fx vindex (vector-length vtable))
		       (let ((proc (vector-ref vtable vindex)))
			  (if (procedure? proc)
			      (apply proc obj args)
			      (js-method-jsobject-call/cache-miss %this obj name args
				 ccache ocache point ccspecs ocspecs)))
		       (js-method-jsobject-call/cache-miss %this obj name args
			  ccache ocache point ccspecs ocspecs)))))))))

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

   (with-access::JsPropertyCache ccache (pmap vindex method cntmiss vtable vmaps)
      (when (and (procedure? method)
		 (not (eq? pmap (js-not-a-pmap)))
		 (=fx (procedure-arity method) (+fx 1 (length args))))
	 (when (=fx vindex (js-not-a-index))
	    (set! vindex (js-get-vindex %this)))
	 (js-cmap-vtable-add! pmap vindex method ccache))
      (if (isa? o JsRecord)
	  (js-method-jsrecord-call/cache-fill %this o name args
	     ccache ocache point ccspecs ocspecs)
	  (js-method-jsobject-call/cache-fill %this o name args
	     ccache ocache point ccspecs ocspecs))))

;*---------------------------------------------------------------------*/
;*    js-method-jsrecord-call/cache-fill ...                           */
;*    -------------------------------------------------------------    */
;*    JsRecords are treated specially because the inline cache is      */
;*    better filled with the unsafe version instead of the generic     */
;*    one that first check the instance type.                          */
;*---------------------------------------------------------------------*/
(define (js-method-jsrecord-call/cache-fill %this::JsGlobalObject
	   o::JsObject name::obj args::pair-nil
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   point::long ccspecs::pair-nil ospecs::pair-nil)
   (let ((cmap (js-object-cmap o)))
      (with-access::JsConstructMap cmap (mntable mrtable)
	 (let loop ((i (-fx (vector-length mntable) 1)))
	    (cond
	       ((=fx i -1)
		(js-method-jsobject-call/cache-fill %this o name args
		   ccache ocache point ccspecs ospecs))
	       ((eq? (vector-ref mntable i) name)
		(let ((procedure (vector-ref mrtable i)))
		   (if (=fx (procedure-arity procedure) (+fx 1 (length args)))
		       (with-access::JsPropertyCache ccache (pmap method function)
			  ;; correct arity, put in cache
			  (js-validate-pmap-pcache! ccache)
			  (set! pmap cmap)
			  (set! method procedure)
			  (set! function procedure)
			  (apply procedure o args))
		       (js-method-jsobject-call/cache-fill %this o name args
			  ccache ocache point ccspecs ospecs))))
	       (else
		(loop (-fx i 1))))))))

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
	  (case (length args)
	     ((0) (js-call0 %this method o))
	     ((1) (js-call1 %this method o (car args)))
	     ((2) (js-call2 %this method o (car args) (cadr args)))
	     ((3) (js-call3 %this method o (car args) (cadr args) (caddr args)))
	     (else (js-calln %this method o args)))))
   
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
		  (with-access::JsObject obj ((wmap cmap))
		     (with-access::JsConstructMap wmap (methods %id)
			(let ((el-or-desc (js-object-ref obj i)))
			   (cond
			      ((or (isa? el-or-desc JsAccessorDescriptor)
				   (isa? el-or-desc JsWrapperDescriptor))
			       (jsapply (js-property-value o obj name el-or-desc %this)))
			      ((js-function? (vector-ref methods i))
			       (let ((f (funval obj el-or-desc)))
				  (cond
				     ((js-function? f)
				      (with-access::JsFunction f (len procedure arity info)
					 (cond
					    ((<fx arity 0)
					     ;; varargs functions, currently not cached...
					     (with-access::JsPropertyCache ccache (pmap emap cmap)
						(set! emap (js-not-a-pmap))
						(set! cmap (js-uncachable-pmap))
						(set! pmap (js-not-a-pmap))))
					    ((=fx (procedure-arity procedure) (+fx 1 (length args)))
					     (with-access::JsPropertyCache ccache (emap cmap pmap pindex (cmethod method) function)
						;; correct arity, put in cache
						(js-validate-pmap-pcache! ccache)
						(set! pmap omap)
						(set! function f)
						(let ((proc (if (js-method? f)
								(with-access::JsMethod f (method) method)
								procedure)))
						   (procedure-attr-set! proc f)
						   (set! cmethod proc))))
					    ((procedureN (if (js-method? f)
							     (with-access::JsMethod f (method) method)
							     procedure)
						(length args))
					     =>
					     (lambda (proc)
						(with-access::JsPropertyCache ccache (emap cmap pmap pindex (cmethod method) function)
						   ;; correct arity, put in cache
						   (js-validate-pmap-pcache! ccache)
						   (set! pmap omap)
						   (set! function f)
						   (procedure-attr-set! proc f)
						   (set! cmethod proc))))
					    (else
					     ;; arity missmatch, never cache
					     (with-access::JsPropertyCache ccache (pmap emap cmap)
						(set! emap (js-not-a-pmap))
						(set! cmap (js-uncachable-pmap))
						(set! pmap (js-not-a-pmap)))))))
				     ((procedure? f)
				      (error "js-method-jsobject-call/cache-fill" "should not be here" f)))
				  (jsapply f)))
			      ((eq? obj o)
			       (with-access::JsPropertyCache ccache (cmap pmap emap cindex)
				  ;; invalidate the call cache and update the
				  ;; object cache
				  (set! cmap omap)
				  (set! cindex i)
				  (jsapply (funval obj el-or-desc))))
			      (else
			       (with-access::JsPropertyCache ccache (pmap cmap emap)
				  ;; invalidate the call cache and update the
				  ;; object cache
				  (set! cmap (js-uncachable-pmap))
				  (set! emap (js-not-a-pmap))
				  (set! pmap (js-not-a-pmap))
				  (jsapply (funval obj el-or-desc))))))))))
	    ;; hash search
	    (lambda (obj e)
	       (let ((d (cell-ref e)))
		  (if (isa? d JsPropertyDescriptor)
		      (jsapply (js-property-value o obj name d %this))
		      (jsapply d))))
	    ;; property search
	    (lambda (obj v i)
	       (jsapply (js-property-value o obj name v %this)))
	    ;; not found
	    (lambda (o)
	       (js-raise-type-error %this "call: not a function ~s"
		  (js-undefined)))
	    ;; loop
	    loop))))

;*---------------------------------------------------------------------*/
;*    js-call-with-stack-list ...                                      */
;*    -------------------------------------------------------------    */
;*    Overriden by a macro in property.sch                             */
;*---------------------------------------------------------------------*/
(define-inline (js-call-with-stack-list lst proc)
   (proc lst))
   
;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
