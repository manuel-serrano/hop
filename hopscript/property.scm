;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/property.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 25 07:05:26 2013                          */
;*    Last change :  Tue Apr  3 17:58:16 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
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
	    "property.sch")
   
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
	   __hopscript_arithmetic)
   
   (extern ($js-make-pcache::obj (::obj ::int ::obj ::JsPropertyCache)
	      "bgl_make_pcache")
	   ($js-invalidate-pcaches-pmap!::void (::obj)
	      "bgl_invalidate_pcaches_pmap")
	   ($js-get-pcaches::pair-nil ()
	      "bgl_get_pcaches"))

   (export (generic js-debug-object ::obj #!optional (msg ""))
	   (js-debug-pcache ::obj #!optional (msg ""))
	   (js-debug-cmap ::obj #!optional (msg ""))
	   (%define-pcache ::int)
	   (js-make-pcache ::int ::obj)
	   (js-invalidate-pcaches-pmap! ::JsGlobalObject)
	   (inline js-pcache-ref ::obj ::int)
	   (inline js-pcache-imap ::JsPropertyCache)
	   (inline js-pcache-cmap ::JsPropertyCache)
	   (inline js-pcache-amap ::JsPropertyCache)
	   
	   (inline property-name::symbol ::struct)
	   
	   (js-names->cmap::JsConstructMap ::vector)
	   (js-object-literal-init! ::JsObject)

	   (js-object-unmap! ::JsObject)
	   (js-toname::obj ::obj ::JsGlobalObject)
	   (js-integer->name::obj ::long)
	   
	   (inline js-is-accessor-descriptor?::bool obj)
	   (inline js-is-data-descriptor?::bool obj)
	   (inline js-is-generic-descriptor?::bool obj)
	   (js-from-property-descriptor ::JsGlobalObject desc ::obj)
	   (js-to-property-descriptor ::JsGlobalObject desc ::obj)
	   (js-property-value ::obj ::JsPropertyDescriptor ::JsGlobalObject)
	   (js-property-value-set! obj::JsObject ::JsPropertyDescriptor v ::JsGlobalObject)
	   
	   (js-object-add! obj::JsObject index::long value)
	   (js-object-push! obj::JsObject index::long value)
	   (js-object-ctor-add! obj::JsObject index::long value)
	   (js-object-ctor-push! obj::JsObject index::long value)
	   
	   (generic js-properties-names::pair-nil ::obj ::bool ::JsGlobalObject)
	   (generic js-properties-name::vector ::obj ::bool ::JsGlobalObject)
	   (generic js-properties-symbol::vector ::obj ::JsGlobalObject)
	   
	   (generic js-has-property::bool ::obj ::obj ::JsGlobalObject)
	   (generic js-get-own-property ::obj ::obj ::JsGlobalObject)
	   
	   (generic js-get-property-value ::obj ::obj ::obj ::JsGlobalObject)
	   
	   (js-object-get-lookup ::JsObject ::obj ::bool ::JsGlobalObject
	      ::JsPropertyCache ::long ::pair-nil)
	   (js-get-property ::JsObject ::obj ::JsGlobalObject)
	   
	   (js-get-notfound ::obj ::obj ::JsGlobalObject)
	   
	   (generic js-get ::obj ::obj ::JsGlobalObject)
	   (generic js-get-length::obj ::obj ::JsGlobalObject #!optional cache)
	   (generic js-get-lengthu32::uint32 ::obj ::JsGlobalObject #!optional cache)
	   
	   (js-get/debug ::obj ::obj ::JsGlobalObject loc)
	   
	   (js-get/cache ::obj ::obj ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   (js-get-name/cache ::obj ::symbol ::bool ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   (js-object-get-name/cache ::JsObject ::obj ::bool ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   
	   (generic js-object-get-name/cache-miss ::JsObject ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   (js-object-get-name/cache-imap+ ::JsObject ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   (js-object-get-name/cache-cmap+ ::JsObject ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   
	   (js-global-object-get-name ::JsObject ::symbol ::bool
	      ::JsGlobalObject)
	   (js-global-object-get-name/cache ::JsObject ::symbol ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   
	   (js-can-put o::JsObject ::obj ::JsGlobalObject)
	   (js-unresolved-put! ::JsObject ::obj ::obj ::bool ::JsGlobalObject)
	   (js-unresolved-eval-put! ::JsObject ::obj ::obj ::bool ::JsGlobalObject)
	   (js-decl-eval-put! ::JsObject ::obj ::obj ::bool ::JsGlobalObject)
	   
	   (generic js-put! ::obj ::obj ::obj ::bool ::JsGlobalObject)
	   (generic js-put-length! ::obj ::obj ::bool ::obj ::JsGlobalObject)
	   (js-put-jsobject! ::JsObject ::obj ::obj ::bool ::bool
	      ::JsGlobalObject
	      ::obj #!optional (point -1) (cspecs '()))
	   (js-put/debug! ::obj ::obj ::obj ::bool ::JsGlobalObject loc)
	   (js-put/cache! ::obj ::obj ::obj ::bool ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   (js-put-name/cache! ::obj ::symbol ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   (js-object-put-name/cache! ::JsObject ::obj ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   
	   (js-object-put-name/cache-miss! ::JsObject ::obj ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   (js-object-put-name/cache-imap+! ::JsObject ::obj ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   (js-object-put-name/cache-cmap+! ::JsObject ::obj ::obj ::bool
	      ::JsGlobalObject
	      ::JsPropertyCache #!optional (point -1) (cspecs '()))
	   
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
	   
	   (generic js-seal ::JsObject ::obj)
	   (generic js-freeze ::JsObject ::obj)
	   (generic js-prevent-extensions ::JsObject)
	   
	   (generic js-for-in ::obj ::procedure ::JsGlobalObject)
	   (generic js-for-of ::obj ::procedure ::bool ::JsGlobalObject)
	   (js-for-of-iterator ::obj ::obj ::procedure ::bool ::JsGlobalObject)
	   
	   (js-bind! ::JsGlobalObject ::JsObject name::obj #!key
	      (value #f)
	      (get #f)
	      (set #f)
	      (writable #t)
	      (enumerable #t)
	      (configurable #t)
	      (hidden-class #t))

	   (js-define ::JsGlobalObject ::JsObject
	      ::symbol ::procedure ::obj ::obj ::obj)
	   
	   (js-method-call-name/cache ::JsGlobalObject ::obj ::obj
	      ::JsPropertyCache ::JsPropertyCache ::long ::pair-nil ::pair-nil . args)
	   (js-object-method-call-name/cache ::JsGlobalObject ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache ::long ::pair-nil ::pair-nil . args)
	   
	   (js-object-method-call/cache-miss ::JsGlobalObject
	      ::JsObject ::obj ::pair-nil
	      ::JsPropertyCache ::JsPropertyCache ::long ::pair-nil ::pair-nil)
	   
	   (js-call/cache ::JsGlobalObject ::JsPropertyCache obj this . args)
	   
	   (js-get-vindex::long ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    vtable-threshold ...                                             */
;*---------------------------------------------------------------------*/
(define (vtable-threshold) 100)

;*---------------------------------------------------------------------*/
;*    js-debug-object ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (js-debug-object obj #!optional (msg ""))
   (if (isa? obj JsObject)
       (with-access::JsObject obj (cmap elements)
	  (let ((properties (js-object-properties obj)))
	     (if (eq? cmap (js-not-a-cmap))
		 (with-access::JsConstructMap cmap (%id props)
		    (fprint (current-error-port) msg (typeof obj) " UNMAPPED"
		       " length=" (length properties)
		       "\n  prop.names="
		       (map (lambda (d)
			       (with-access::JsPropertyDescriptor d (name)
				  name))
			  properties)
		       "\n  props=" properties))
		 (with-access::JsConstructMap cmap (%id props methods size)
		    (fprint (current-error-port) msg (typeof obj) " MAPPED"
		       " length=" (vector-length elements)
		       " inline=" (js-object-inline-elements? obj)
		       " size=" size
		       " mlengths=" (vector-length methods)
		       "\n  elements=" (vector-map
					  (lambda (v)
					     (if (isa? v JsObject)
						 (typeof v)
						 v))
					  elements)
		       "\n  cmap.%id=" %id
		       " cmap.props=" props)))))
       (fprint (current-error-port) msg (typeof obj))))

;*---------------------------------------------------------------------*/
;*    js-debug-pcache ...                                              */
;*---------------------------------------------------------------------*/
(define (js-debug-pcache pcache #!optional (msg ""))
   (if (isa? pcache JsPropertyCache)
       (with-access::JsPropertyCache pcache (imap cmap pmap amap index)
	  (cond
	     ((isa? cmap JsConstructMap)
	      (fprint (current-error-port) msg (typeof pcache)
		 " index=" index)
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
	      (when (isa? amap JsConstructMap)
		 (with-access::JsConstructMap amap ((%aid %id) (aprops props))
		    (fprint (current-error-port) "  amap.%id=" %aid
		       " amap.props=" aprops))))
	     ((isa? pmap JsConstructMap)
	      (with-access::JsConstructMap pmap ((%pid %id) (pprops props))
		 (fprint (current-error-port) msg (typeof pcache)
		    " index=" index
		    "\n  pmap.%id=" %pid
		    " pmap.props=" pprops)))
	     ((isa? amap JsConstructMap)
	      (with-access::JsConstructMap amap ((%aid %id) (aprops props))
		 (fprint (current-error-port) msg (typeof pcache)
		    " index=" index
		    "\n  amap.%id=" %aid
		    " amap.props=" aprops)))
	     (else
	      (fprint (current-error-port) msg (typeof pcache)
		 " no map"))))
       (fprint (current-error-port) msg (typeof pcache))))

;*---------------------------------------------------------------------*/
;*    js-debug-cmap ...                                                */
;*---------------------------------------------------------------------*/
(define (js-debug-cmap cmap #!optional (msg ""))
   (with-access::JsConstructMap cmap (%id props methods size transitions)
      (fprint (current-error-port) msg
	 "cmap.%id=" %id
	 " size=" size
	 " plength=" (vector-length props)
	 " mlength=" (vector-length methods)
	 "\nprops.names=" (vector-map prop-name props)
	 "\nprops=" props
	 "\ntransitions="
	 (map (lambda (tr)
		 (format "~a [~a] -> ~a"
		    (if (symbol? (transition-name-or-value tr))
			(transition-name-or-value tr)
			(typeof (transition-name-or-value tr)))
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
      (let ((nels (copy-vector elements (+fx 1 idx))))
	 (cond-expand (profile (profile-cache-extension (+fx 1 idx))))
	 (vector-set! nels idx value)
	 (set! elements nels)
	 obj)))

;*---------------------------------------------------------------------*/
;*    js-object-ctor-add! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-object-ctor-add! obj::JsObject idx::long value)
   (with-access::JsObject obj (cmap)
      (with-access::JsConstructMap cmap (ctor)
	 (when (isa? ctor JsFunction)
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
	     (when (isa? ctor JsFunction)
		(with-access::JsFunction ctor (constrmap constrsize maxconstrsize)
		   (when (<fx constrsize maxconstrsize)
		      (set! constrsize (+fx 1 constrsize))
		      (set! constrmap (instantiate::JsConstructMap
					 (ctor ctor)
					 (size constrsize)))))))
	  (vector-set! elements idx value))))

;*---------------------------------------------------------------------*/
;*    function0->proc ...                                              */
;*---------------------------------------------------------------------*/
(define (function0->proc fun %this::JsGlobalObject)
   (if (isa? fun JsFunction)
       (with-access::JsFunction fun (procedure)
	  (if (correct-arity? procedure 1)
	      procedure
	      (lambda (this)
		 (js-call0 %this fun this))))
       (lambda (obj)
	  (js-undefined))))

;*---------------------------------------------------------------------*/
;*    function1->proc ...                                              */
;*---------------------------------------------------------------------*/
(define (function1->proc fun %this::JsGlobalObject)
   (if (isa? fun JsFunction)
       (with-access::JsFunction fun (procedure)
	  (if (correct-arity? procedure 2)
	      procedure
	      (lambda (this a0)
		 (js-call1 %this fun this a0))))
       (lambda (obj v)
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
;*    make-pcache ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-make-pcache len src)
   (let ((pcache ($make-vector-uncollectable len #unspecified)))
      (let loop ((i 0))
	 (if (=fx i len)
	     (register-pcache! pcache len src)
	     (begin
		(vector-set! pcache i
		   (instantiate::JsPropertyCache
		      (pcache pcache)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    *pcaches* ...                                                    */
;*---------------------------------------------------------------------*/
(define *pcaches* *pcaches*)

;*---------------------------------------------------------------------*/
;*    register-pcache! ...                                             */
;*---------------------------------------------------------------------*/
(define (register-pcache! pcache len src)
   ;; bootstrap initialization
   (when (eq? *pcaches* #unspecified) (set! *pcaches* '()))
   (set! *pcaches* (cons (vector pcache len src) *pcaches*))
   pcache)


;*---------------------------------------------------------------------*/
;*    js-invalidate-pcaches-pmap! ...                                  */
;*    -------------------------------------------------------------    */
;*    Called when:                                                     */
;*       1) a __proto__ is changed, or                                 */
;*       2) an accessor property or a non default data property is     */
;*          added to an object, or                                     */
;*       3) a property is deleted, or                                  */
;*       4) a property hidding a prototype property is added.          */
;*---------------------------------------------------------------------*/
(define (js-invalidate-pcaches-pmap! %this::JsGlobalObject)
   
   (define (invalidate-pcache-pmap! pcache)
      (with-access::JsPropertyCache pcache (pmap emap amap)
	 (when (isa? pmap JsConstructMap)
	    (reset-cmap-vtable! pmap))
	 (when (isa? amap JsConstructMap)
	    (reset-cmap-vtable! amap))
	 (set! pmap #t)
	 (set! emap #t)
	 (set! amap #t)))

   (with-access::JsGlobalObject %this (js-pmap-valid)
      (when js-pmap-valid
	 (log-pmap-invalidation!)
	 (set! js-pmap-valid #f)
	 ($js-invalidate-pcaches-pmap! invalidate-pcache-pmap!)
	 (for-each (lambda (pcache-entry)
		      (let ((vec (vector-ref pcache-entry 0)))
			 (let loop ((i (-fx (vector-ref pcache-entry 1) 1)))
			    (when (>=fx i 0)
			       (let ((pcache (vector-ref vec i)))
				  (invalidate-pcache-pmap! pcache))
			       (loop (-fx i 1))))))
	    *pcaches*))))

;*---------------------------------------------------------------------*/
;*    js-pcache-ref ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-pcache-ref pcache index)
   (vector-ref pcache index))

;*---------------------------------------------------------------------*/
;*    js-pcache-cmap ...                                               */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define-inline (js-pcache-cmap pcache)
   (with-access::JsPropertyCache pcache (cmap)
      cmap))

;*---------------------------------------------------------------------*/
;*    js-pcache-imap ...                                               */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define-inline (js-pcache-imap pcache)
   (with-access::JsPropertyCache pcache (imap)
      imap))

;*---------------------------------------------------------------------*/
;*    js-pcache-amap ...                                               */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define-inline (js-pcache-amap pcache)
   (with-access::JsPropertyCache pcache (amap)
      amap))

;*---------------------------------------------------------------------*/
;*    js-pache-invalidate! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-pache-invalidate! pcache::JsPropertyCache)
   (with-access::JsPropertyCache pcache (imap cmap pmap emap amap index owner)
      (set! imap #t)
      (set! cmap #t)
      (set! pmap #t)
      (set! emap #t)
      (set! amap #t)
      (set! owner #f)
      (set! index 0)))
   
;*---------------------------------------------------------------------*/
;*    js-pcache-update-descriptor! ...                                 */
;*    -------------------------------------------------------------    */
;*    Used to access an object's descriptor                            */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-descriptor! pcache::JsPropertyCache i o::JsObject obj)
   [assert (obj) (isa? obj JsObject)]   
   (with-access::JsObject o ((omap cmap))
      (unless (eq? omap (js-not-a-cmap))
	 (with-access::JsPropertyCache pcache (imap cmap pmap emap amap index owner)
	    (set! imap #t)
	    (set! cmap #t)
	    (set! pmap #t)
	    (set! emap #t)
	    (set! amap omap)
	    (set! owner obj)
	    (set! index i)))))

;*---------------------------------------------------------------------*/
;*    js-pcache-update-direct! ...                                     */
;*    -------------------------------------------------------------    */
;*    Used to access a direct object property.                         */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-direct! pcache::JsPropertyCache i o::JsObject)
   [assert (i) (>=fx i 0)]
   (with-access::JsObject o ((omap cmap))
      (with-access::JsPropertyCache pcache (imap cmap emap pmap amap index)
	 (if (js-object-inline-next-element? o i)
	     (begin
		(set! imap omap)
		(set! cmap omap))
	     (begin
		(set! imap #t)
		(set! cmap omap)))
	 (set! pmap #t)
	 (set! emap #t)
	 (set! amap #t)
	 (set! index i))))

;*---------------------------------------------------------------------*/
;*    js-pcache-update-owner! ...                                      */
;*    -------------------------------------------------------------    */
;*    Used to access a prototype object property.                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-owner! pcache::JsPropertyCache i o::JsObject obj)
   [assert (i) (>=fx i 0)]
   [assert (obj) (isa? obj JsObject)]
   (with-access::JsObject o ((omap cmap))
      (unless (eq? omap (js-not-a-cmap))
	 (with-access::JsPropertyCache pcache (imap cmap pmap emap amap index owner)
	    (set! imap #t)
	    (set! cmap #t)
	    (set! pmap omap)
	    (set! emap #t)
	    (set! amap #t)
	    (set! owner obj)
	    (set! index i)))))

;*---------------------------------------------------------------------*/
;*    js-pcache-next-direct! ...                                       */
;*    -------------------------------------------------------------    */
;*    Used when adding a direct property to an object.                 */
;*---------------------------------------------------------------------*/
(define (js-pcache-next-direct! pcache::JsPropertyCache o::JsObject nextmap i)
   [assert (i) (>=fx i 0)]
   (with-access::JsObject o ((omap cmap))
      (unless (eq? omap (js-not-a-cmap))
	 [assert (pcache) (or (eq? omap nextmap) (= (cmap-size nextmap) (+ 1 (cmap-size omap))))]
	 (with-access::JsPropertyCache pcache (imap pmap amap cmap emap index owner)
	    (if (js-object-inline-next-element? o i)
		(begin
		   (set! imap nextmap)
		   (set! cmap nextmap)
		   (set! emap omap))
		(begin
		   (set! imap #t)
		   (set! cmap nextmap)
		   (set! emap #t)))
	    (set! pmap omap)
	    (set! emap #t)
	    (set! amap #t)
	    (set! owner #f)
	    (set! index i)))))

;*---------------------------------------------------------------------*/
;*    cmap-size ...                                                    */
;*---------------------------------------------------------------------*/
(define (cmap-size cmap::JsConstructMap)
   (with-access::JsConstructMap cmap (props) (vector-length props)))

;*---------------------------------------------------------------------*/
;*    pmap-index ...                                                   */
;*---------------------------------------------------------------------*/
(define (pmap-index cmap::JsPropertyCache)
   (with-access::JsPropertyCache cmap (index) index))

;*---------------------------------------------------------------------*/
;*    js-object-inline-next-element? ...                               */
;*---------------------------------------------------------------------*/
(define-inline (js-object-inline-next-element? o::JsObject idx::long)
   (with-access::JsObject o (elements)
      (and (js-object-inline-elements? o) (<fx idx (vector-length elements)))))

;*---------------------------------------------------------------------*/
;*    transition ...                                                   */
;*---------------------------------------------------------------------*/
(define-struct transition name-or-value flags nextmap)

;*---------------------------------------------------------------------*/
;*    cmap-transition ...                                              */
;*---------------------------------------------------------------------*/
(define (cmap-transition name value flags nextmap)
   (if (eq? name '__proto__)
       (transition value flags nextmap)
       (transition name flags nextmap)))

;*---------------------------------------------------------------------*/
;*    cmap-find-transition ...                                         */
;*---------------------------------------------------------------------*/
(define (cmap-find-transition omap::JsConstructMap name val flags::int)
   
   (define (is-transition? t)
      (and (=fx (transition-flags t) flags)
	   (if (eq? name '__proto__)
	       (eq? (transition-name-or-value t) val)
	       (eq? (transition-name-or-value t) name))))
   
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
;*    link-cmap! ...                                                   */
;*---------------------------------------------------------------------*/
(define (link-cmap! omap::JsConstructMap nmap::JsConstructMap name value flags::int)
   (with-access::JsConstructMap omap (transitions)
      (set! transitions
	 (cons (cmap-transition name value flags nmap) transitions))
      nmap))

;*---------------------------------------------------------------------*/
;*    extend-cmap ...                                                  */
;*---------------------------------------------------------------------*/
(define (extend-cmap omap::JsConstructMap name flags)
   
   (define (vector-extend::vector vec::vector val)
      ;; extend a vector with one additional slot
      (let* ((len (vector-length vec))
	     (nvec (copy-vector vec (+fx 1 len))))
	 (vector-set! nvec len val)
	 nvec))
   
   (with-access::JsConstructMap omap (props methods ctor)
      (let ((newprops (vector-extend props (prop name flags)))
	    (newmethods (vector-extend methods #unspecified)))
	 ;;(tprint "CREATE CMAP: " name)
	 (instantiate::JsConstructMap
	    (ctor ctor)
	    (props newprops)
	    (methods newmethods)))))

;*---------------------------------------------------------------------*/
;*    extend-cmap! ...                                                 */
;*---------------------------------------------------------------------*/
(define (extend-cmap! omap::JsConstructMap name flags)
   
   (define (vector-extend::vector vec::vector val)
      ;; extend a vector with one additional slot
      (let* ((len (vector-length vec))
	     (nvec (copy-vector vec (+fx 1 len))))
	 (vector-set! nvec len val)
	 nvec))
   
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
	    (props newprops)
	    (methods newmethods)))))

;*---------------------------------------------------------------------*/
;*    reset-cmap-vtable! ...                                           */
;*---------------------------------------------------------------------*/
(define (reset-cmap-vtable! omap::JsConstructMap)
   (with-access::JsConstructMap omap (vtable vlen)
      (set! vlen 0)
      (set! vtable '#())))
   
;*---------------------------------------------------------------------*/
;*    js-cmap-vtable-add! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-cmap-vtable-add! o::JsConstructMap idx::long obj)
   (with-access::JsConstructMap o (vlen vtable)
      (when (>=fx idx (vector-length vtable))
	 (log-vtable! idx)
	 (set! vlen (+fx idx 1))
	 (set! vtable (copy-vector vtable (+fx idx 1))))
      (vector-set! vtable idx obj)
      (log-vtable-entries! vtable)
      obj))

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
(define (js-names->cmap names)
   (instantiate::JsConstructMap
      (props (vector-map (lambda (n) (prop n (property-flags #t #t #t #f))) names))
      (methods (make-vector (vector-length names) #unspecified))))
      
;*---------------------------------------------------------------------*/
;*    js-object-literal-init! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-object-literal-init! o::JsObject)
   (with-access::JsObject o ((%elements elements) cmap)
      (with-access::JsConstructMap cmap ((%methods methods) props %id)
	 (let ((elements %elements)
	       (methods %methods))
	    (let loop ((i (-fx (vector-length elements) 1)))
	       (if (=fx i -1)
		   o
		   (let ((v (vector-ref elements i)))
		      (cond
			 ((not (isa? v JsFunction))
			  (vector-set! methods i #f))
			 ((eq? (vector-ref methods i) #unspecified)
			  (vector-set! methods i #t))
			 (else
			  (vector-set! methods i #f)))
		      (loop (-fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    property-index-vector ...                                        */
;*---------------------------------------------------------------------*/
(define property-index-vector
   (list->vector
      (map (lambda (i)
	      (string->symbol (fixnum->string i)))
	 (iota 111 -10))))

;*---------------------------------------------------------------------*/
;*    fixnum->pname ...                                                */
;*---------------------------------------------------------------------*/
(define (fixnum->pname indx::long)
   (if (and (>fx indx -10) (<fx indx 100))
       (vector-ref property-index-vector (+fx indx 10))
       (string->symbol (fixnum->string indx))))

;*---------------------------------------------------------------------*/
;*    jsobject-map-find ...                                            */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-map-find o p succeed fail)
   (let ((i (gensym 'i))
	 (cmap (gensym 'cmap)))
      `(with-access::JsObject ,o ((,cmap cmap))
	  (with-access::JsConstructMap ,cmap (props)
	     (let ((props props))
		(let liip ((,i (-fx (vector-length props) 1)))
		   (cond
		      ((=fx ,i -1)
		       (,fail))
		      ((eq? (prop-name (vector-ref props ,i)) ,p)
		       (,succeed ,o ,i))
		      (else
		       (liip (-fx ,i 1))))))))))

;*---------------------------------------------------------------------*/
;*    jsobject-properties-find ...                                     */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-properties-find o p succeed fail)
   (let ((desc (gensym 'desc))
	 (name (gensym 'name))
	 (prop (gensym 'properties)))
      `(let ((,prop (js-object-properties ,o)))
	  (let ((,desc (find (lambda (d::JsPropertyDescriptor)
				(with-access::JsPropertyDescriptor d ((,name name))
				   (eq? ,p ,name)))
			  ,prop)))
	     (if ,desc (,succeed ,o ,desc) (,fail))))))

;*---------------------------------------------------------------------*/
;*    js-object-find ...                                               */
;*    -------------------------------------------------------------    */
;*    This is a general macro that walks thru the prototype chains     */
;*    (iff the optional loop argument is provided) looking of a        */
;*    property. It calls on of the successXXX hooks when found.        */
;*---------------------------------------------------------------------*/
(define-macro (jsobject-find o name foundinmap foundinprop notfound . loop)

   (define (find obj name)
      `(with-access::JsObject ,obj (cmap __proto__)
	 (if (not (eq? cmap (js-not-a-cmap)))
	     (jsobject-map-find ,obj ,name ,foundinmap
		(lambda ()
		   ,(if (pair? loop)
			`(if (isa? __proto__ JsObject)
			     (,(car loop) __proto__)
			     (,notfound))
			`(,notfound))))
	     (jsobject-properties-find ,obj ,name ,foundinprop
		(lambda ()
		   ,(if (pair? loop)
			`(if (isa? __proto__ JsObject)
			     (,(car loop) __proto__)
			     (,notfound))
			`(,notfound)))))))

   (let ((obj (gensym 'o)))
      `(let ((,obj ,o))
	  ,(if (symbol? name)
	       (find obj name)
	       (let ((nm (gensym 'name)))
		  `(let ((,nm ,name))
		      ,(find obj nm)))))))

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
   (with-access::JsObject o (cmap elements)
      (unless (eq? cmap (js-not-a-cmap))
	 (with-access::JsConstructMap cmap (props)
	    (let loop ((i (-fx (vector-length props) 1))
		       (acc '()))
	       (cond
		  ((=fx i -1)
		   (js-object-properties-set! o acc))
		  ((not (vector-ref props i))
		   (loop (-fx i 1) acc))
		  ((isa? (vector-ref elements i) JsPropertyDescriptor)
		   (loop (-fx i 1)
		      (cons (vector-ref elements i) acc)))
		  (else
		   (let* ((name (prop-name (vector-ref props i)))
			  (flags (prop-flags (vector-ref props i)))
			  (desc (instantiate::JsValueDescriptor
				   (enumerable (flags-enumerable? flags))
				   (writable (flags-writable? flags))
				   (configurable (flags-configurable? flags))
				   (name name)
				   (value (vector-ref elements i)))))
		      (loop (-fx i 1) (cons desc acc)))))))
	 (set! cmap (js-not-a-cmap))
	 (set! elements '#())))
   o)

;*---------------------------------------------------------------------*/
;*    js-toname ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-toname p %this)
   (let loop ((p p))
      (cond
	 ((symbol? p)
	  p)
	 ((string? p)
	  (string->symbol p))
	 ((js-jsstring? p)
	  (string->symbol (js-jsstring->string p)))
	 ((fixnum? p)
	  (fixnum->pname p))
	 ((uint32? p)
	  (cond-expand
	     (bint30
	      (if (<u32 p (fixnum->uint32 (bit-lsh 1 29)))
		  (fixnum->pname (uint32->fixnum p))
		  (string->symbol (llong->string (uint32->llong p)))))
	     (bint32
	      (if (<u32 p (bit-lshu32 (fixnum->uint32 1) (fixnum->uint32 30)))
		  (fixnum->pname (uint32->fixnum p))
		  (string->symbol (llong->string (uint32->llong p)))))
	     (else
	      (fixnum->pname (uint32->fixnum p)))))
	 ((int32? p)
	  (cond-expand
	     (bint30
	      (if (and (>s32 p 0) (<s32 p (fixnum->int32 (bit-lsh 1 29))))
		  (fixnum->pname (int32->fixnum p))
		  (string->symbol (llong->string (int32->llong p)))))
	     (bint32
	      (string->symbol (fixnum->string (int32->fixnum p))))
	     (else
	      (fixnum->pname (int32->fixnum p)))))
	 ((isa? p JsSymbolLiteral)
	  p)
	 ((isa? p JsSymbol)
	  (with-access::JsSymbol p (val)
	     val))
	 (else
	  (loop (js-tostring p %this))))))

;*---------------------------------------------------------------------*/
;*    js-integer->name ...                                             */
;*---------------------------------------------------------------------*/
(define (js-integer->name n)
   (string->symbol (integer->string n)))

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
(define (js-from-property-descriptor %this::JsGlobalObject desc owner)
   (if (eq? desc (js-undefined))
       desc
       (with-access::JsGlobalObject %this (js-object)
	  (let ((obj (js-new %this js-object)))
	     (cond
		((isa? desc JsValueDescriptor)
		 ;; 3
		 (with-access::JsValueDescriptor desc (writable value)
		    (js-put! obj 'value value #f %this)
		    (js-put! obj 'writable writable #f %this)))
		((isa? desc JsAccessorDescriptor)
		 ;; 4
		 (with-access::JsAccessorDescriptor desc (get set)
		    (js-put! obj 'get get #f %this)
		    (js-put! obj 'set set #f %this)))
		((isa? desc JsWrapperDescriptor)
		 ;; hop.js extension
		 (with-access::JsWrapperDescriptor desc (value writable)
		    (js-put! obj 'value value #f %this)
		    (js-put! obj 'writable writable #f %this))))
	     (with-access::JsPropertyDescriptor desc (enumerable configurable)
		;; 5
		(js-put! obj 'enumerable enumerable #f %this)
		;; 6
		(js-put! obj 'configurable configurable #f %this))
	     obj))))

;*---------------------------------------------------------------------*/
;*    js-to-property-descriptor ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.10.5       */
;*---------------------------------------------------------------------*/
(define (js-to-property-descriptor %this::JsGlobalObject obj name::obj)
   (let* ((obj (js-cast-object obj %this "ToPropertyDescriptor"))
	  (enumerable (if (js-has-property obj 'enumerable %this)
			  (js-toboolean (js-get obj 'enumerable %this))
			  (js-undefined)))
	  (configurable (if (js-has-property obj 'configurable %this)
			    (js-toboolean (js-get obj 'configurable %this))
			    (js-undefined)))
	  (hasget (js-has-property obj 'get %this))
	  (hasset (js-has-property obj 'set %this)))
      (cond
	 ((or hasget hasset)
	  (let ((get (js-get obj 'get %this))
		(set (js-get obj 'set %this)))
	     (if (or (js-has-property obj 'writable %this)
		     (js-has-property obj 'value %this)
		     (and (not (isa? get JsFunction))
			  (not (eq? get (js-undefined))))
		     (and (not (isa? set JsFunction))
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
	 ((js-has-property obj 'value %this)
	  (let ((writable (if (js-has-property obj 'writable %this)
			      (js-toboolean (js-get obj 'writable %this))
			      (js-undefined))))
	     (instantiate::JsValueDescriptor
		(name name)
		(value (js-get obj 'value %this))
		(writable writable)
		(enumerable enumerable)
		(configurable configurable))))
	 ((js-has-property obj 'writable %this)
	  (instantiate::JsDataDescriptor
	     (name name)
	     (writable (js-toboolean (js-get obj 'writable %this)))
	     (enumerable enumerable)
	     (configurable configurable)))
	 (else
	  (instantiate::JsPropertyDescriptor
	     (name name)
	     (enumerable enumerable)
	     (configurable configurable))))))

;*---------------------------------------------------------------------*/
;*    js-property-value ...                                            */
;*    -------------------------------------------------------------    */
;*    Get the value of a property.                                     */
;*---------------------------------------------------------------------*/
(define (js-property-value obj desc %this)
   (cond
      ((isa? desc JsAccessorDescriptor)
       (with-access::JsAccessorDescriptor desc (%get)
	  (%get obj)))
      ((isa? desc JsValueDescriptor)
       (with-access::JsValueDescriptor desc (value)
	  value))
      ((isa? desc JsWrapperDescriptor)
       (with-access::JsWrapperDescriptor desc (value)
	  value))
      (else
       (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-property-value-value-set! ...                                 */
;*    -------------------------------------------------------------    */
;*    Set the value of a property.                                     */
;*---------------------------------------------------------------------*/
(define (js-property-value-set! obj desc v %this)
   (cond
      ((isa? desc JsAccessorDescriptor)
       (with-access::JsAccessorDescriptor desc (%set)
	  (%set obj v)))
      ((isa? desc JsValueDescriptor)
       (with-access::JsValueDescriptor desc (value)
	  (set! value v)
	  v))
      ((isa? desc JsWrapperDescriptor)
       (with-access::JsWrapperDescriptor desc (%set)
	  (%set obj v)
	  v))
      (else
       (js-undefined))))

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

   (with-access::JsObject o (cmap)
      (cond
	 ((not (eq? cmap (js-not-a-cmap)))
	  (cmap->names cmap))
	 ((not enump)
	  (filter-map (lambda (p)
			 (with-access::JsPropertyDescriptor p (name)
			    name))
	     (js-object-properties o)))
	 (else
	  (filter-map (lambda (p)
			 (with-access::JsPropertyDescriptor p (enumerable name)
			    (when enumerable name)))
	     (js-object-properties o))))))

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
		     (when (symbol? n)
			(js-string->jsstring (symbol->string! n))))
	 (js-properties-names o enump %this))))

;*---------------------------------------------------------------------*/
;*    js-properties-symbol ...                                         */
;*    -------------------------------------------------------------    */
;*    Returns a vector of the properties symbol.                       */
;*---------------------------------------------------------------------*/
(define-generic (js-properties-symbol::vector o %this::JsGlobalObject)
   (js-raise-type-error %this "[[PROP]]: not an object ~s" o))

;*---------------------------------------------------------------------*/
;*    js-properties-symbol::vector ::JsObject ...                      */
;*---------------------------------------------------------------------*/
(define-method (js-properties-symbol::vector o::JsObject %this::JsGlobalObject)
   (apply vector
      (filter-map (lambda (n)
		     (when (isa? n JsSymbolLiteral)
			n))
	 (js-properties-names o #t %this))))

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
   (jsobject-find o name
      ;; cmap search
      (lambda (o i) #t)
      ;; property search
      (lambda (o d) #t)
      ;; failure
      (lambda () #f)
      ;; prototype search
      (lambda (__proto__) (js-has-property __proto__ name %this))))

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
;*    (when (and (isa? o JsArray) (symbol? p))                         */
;*       (tprint "MS: js-get-own-property " p " " (typeof p) " " (typeof o))) */
   (jsobject-find o (js-toname p %this)
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
      (lambda (o d) d)
      ;; not found
      (lambda () (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-get-property ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.2       */
;*    -------------------------------------------------------------    */
;*    Returns the property owned by the object (i.e., traverses the    */
;*    prototype chain if the object does not own it directly).         */
;*---------------------------------------------------------------------*/
(define (js-get-property o::JsObject p::obj %this::JsGlobalObject)
   ;; JsObject x obj x JsGlobalObject -> JsPropertyDescriptor | Undefined
   (let ((desc (js-get-own-property o p %this)))
      (if (eq? desc (js-undefined))
	  (with-access::JsObject o (__proto__)
	     (if (isa? __proto__ JsObject)
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
   ;; JsObject x obj x JsGlobalObject -> value | Absent
   (let loop ((owner o))
      (let ((desc (js-get-own-property owner p %this)))
	 (if (eq? desc (js-undefined))
	     (with-access::JsObject owner (__proto__)
		(if (isa? __proto__ JsObject)
		    (loop __proto__)
		    (js-absent)))
	     (js-property-value base desc %this)))))

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
      ((string? o)
       (js-get-string o prop %this))
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
   (cond
      ((symbol? prop)
       (js-profile-log-get prop))
      ((string? prop)
       (js-profile-log-get (string->symbol prop))))
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
;*    js-object-get-lookup ...                                         */
;*    -------------------------------------------------------------    */
;*    Look for the property, if found update the cache and return      */
;*    the property value.                                              */
;*---------------------------------------------------------------------*/
(define (js-object-get-lookup o::JsObject name::obj throw::bool %this::JsGlobalObject
	    cache::JsPropertyCache point::long cspecs::pair-nil)

   (with-access::JsPropertyCache cache (cntmiss (cname name) (cpoint point) usage)
      (set! cntmiss (+fx 1 cntmiss))
      (set! cname name)
      (set! cpoint point)
      (set! usage 'get))
   
   (define (js-pcache-vtable! omap cache i)
      (with-access::JsPropertyCache cache (cntmiss vindex)
	 (when (>=fx cntmiss (vtable-threshold))
	    (when (=fx vindex (js-not-a-index))
	       (set! vindex (js-get-vindex %this)))
	    (js-cmap-vtable-add! omap vindex i))))

   (let loop ((obj o))
      (jsobject-find obj name
	 ;; map search
	 (lambda (obj i)
	    (with-access::JsObject o ((omap cmap))
	       (with-access::JsObject obj (elements)
		  (with-access::JsPropertyCache cache (index owner cntmiss)
		     (let ((el-or-desc (vector-ref elements i)))
			(cond
			   ((isa? el-or-desc JsPropertyDescriptor)
			    (unless (eq? o obj)
			       (with-access::JsGlobalObject %this (js-pmap-valid)
				  (set! js-pmap-valid #t)))
			    ;; accessor property
			    (js-pcache-update-descriptor! cache i o obj)
			    (js-property-value o el-or-desc %this))
			   ((eq? o obj)
			    ;; direct access to the direct object
			    [assert (i) (<=fx i (vector-length elements))]
			    (js-pcache-update-direct! cache i o)
			    (js-pcache-vtable! omap cache i)
			    el-or-desc)
			   (else
			    ;; direct access to a prototype object
			    (with-access::JsGlobalObject %this (js-pmap-valid)
			       (set! js-pmap-valid #t))
			    (js-pcache-update-owner! cache i o obj)
			    el-or-desc)))))))
	 ;; property search
	 (lambda (obj v)
	    (with-access::JsPropertyCache cache (index owner)
	       (js-property-value o v %this)))
	 ;; not found
	 (lambda ()
	    (js-get-notfound name throw %this))
	 ;; loop
	 loop)))

;*---------------------------------------------------------------------*/
;*    js-get-length ::obj ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (js-get-length o::obj %this::JsGlobalObject
		   #!optional cache)
   (if cache
       (js-get-name/cache o 'length #f %this cache)
       (js-get o 'length %this)))

;*---------------------------------------------------------------------*/
;*    js-get-lengthu32::uint32 ::obj ...                               */
;*---------------------------------------------------------------------*/
(define-generic (js-get-lengthu32::uint32 o::obj %this::JsGlobalObject
		   #!optional cache)
   (js-touint32
      (if cache
	  (js-get-name/cache o 'length #f %this cache)
	  (js-get o 'length %this))
      %this))

;*---------------------------------------------------------------------*/
;*    js-get/cache ...                                                 */
;*    -------------------------------------------------------------    */
;*    Use a per site cache for the [[GET]] operation. The property     */
;*    name is not known statically.                                    */
;*---------------------------------------------------------------------*/
(define (js-get/cache o prop::obj %this::JsGlobalObject
	   cache::JsPropertyCache #!optional (point -1) (cspecs '()))
   (if (or (not (js-jsstring? prop)) (not (isa? o JsObject)))
       (js-get o prop %this)
       (let ((propname (string->symbol (js-jsstring->string prop))))
	  (with-access::JsPropertyCache cache (name)
	     (if (eq? name propname)
		 (js-get-name/cache o name #f %this cache point cspecs)
		 (js-get o propname %this))))))

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
	   %this::JsGlobalObject
	   cache::JsPropertyCache #!optional (point -1) (cspecs '()))
   (if (isa? obj JsObject)
       (js-object-get-name/cache obj name throw %this cache point cspecs)
       (js-get obj name %this)))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache ...                                     */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-object-get-name/cache o::JsObject name::obj
	   throw::bool %this::JsGlobalObject
	   cache::JsPropertyCache #!optional (point -1) (cspecs '()))
   (js-object-get-name/cache o name throw %this cache point cspecs))

;*---------------------------------------------------------------------*/
;*    js-global-object-get-name ...                                    */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*    -------------------------------------------------------------    */
;*    This is an inlined version of js-get-own-property.               */
;*---------------------------------------------------------------------*/
(define (js-global-object-get-name o::JsObject name::symbol
	   throw::bool %this::JsGlobalObject)
   (let ((pval (js-get-property-value o o name %this)))
      (if (eq? pval (js-absent))
	  (js-get-notfound name throw %this)
	  pval)))

;*---------------------------------------------------------------------*/
;*    js-global-object-get-name/cache ...                              */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-global-object-get-name/cache o::JsObject name::symbol
	   throw::bool %this::JsGlobalObject
	   cache::JsPropertyCache #!optional (point -1) (cspecs '()))
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
		   (js-property-value o desc %this))))
	    (else
	     (js-object-get-name/cache-miss o name throw %this cache point cspecs))))))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-miss ...                                */
;*    -------------------------------------------------------------    */
;*    Use a per site cache for the [[GET]] operation. The name is a    */
;*    static constant, so the actual value is not compared against     */
;*    the cache value.                                                 */
;*---------------------------------------------------------------------*/
(define-generic (js-object-get-name/cache-miss obj::JsObject name::obj
		   throw::bool %this::JsGlobalObject
		   cache::JsPropertyCache
		   #!optional (point -1) (cspecs '()))
   (js-object-get-lookup obj name throw %this cache point cspecs))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-cmap+ ...                               */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define (js-object-get-name/cache-cmap+ obj::JsObject name::obj
	   throw::bool %this::JsGlobalObject
	   cache::JsPropertyCache
	   #!optional (point -1) (cspecs '()))
   (js-object-get-name/cache obj name throw %this cache point
      '(pmap amap vtable)))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-imap+ ...                               */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define (js-object-get-name/cache-imap+ obj::JsObject name::obj
	   throw::bool %this::JsGlobalObject
	   cache::JsPropertyCache
	   #!optional (point -1) (cspecs '()))
   (js-object-get-name/cache obj name throw %this cache point
      '(cmap pmap amap vtable)))

;*---------------------------------------------------------------------*/
;*    js-can-put ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.4       */
;*---------------------------------------------------------------------*/
(define (js-can-put o::JsObject p::obj %this::JsGlobalObject)

   (define (js-get-inherited-property o::JsObject name::obj)
      (jsobject-find o name
	 (lambda (o i) i)
	 (lambda (o d) d)
	 (lambda () #f)))

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
	  (with-access::JsObject o ((proto __proto__))
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
(define (js-unresolved-put! o::JsObject p value throw::bool %this::JsGlobalObject)
   (js-put-jsobject! o p value throw #f %this #f))

;*---------------------------------------------------------------------*/
;*    js-unresolved-eval-put! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-unresolved-eval-put! scope::JsObject p value throw::bool %this::JsGlobalObject)
   (if (eq? (js-get-own-property scope p %this) (js-undefined))
       (js-put-jsobject! %this p value throw (not throw) %this #f)
       (js-put! scope p value throw %this)))

;*---------------------------------------------------------------------*/
;*    js-decl-eval-put! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-decl-eval-put! scope::JsObject p value throw::bool %this::JsGlobalObject)
   (if (eq? (js-get-own-property scope p %this) (js-undefined))
       (js-put-jsobject! %this p value throw #t %this #f)
       (js-put! scope p value throw %this)))

;*---------------------------------------------------------------------*/
;*    js-put! ...                                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.5       */
;*---------------------------------------------------------------------*/
(define-generic (js-put! _o prop v::obj throw::bool %this::JsGlobalObject)
   (cond
      ((string? _o)
       (js-put-string! _o prop v throw %this))
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
       (js-put-name/cache! o 'length v throw %this cache)
       (js-put! o 'length v throw %this)))

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
   (js-put-jsobject! o p value throw #t %this #f))

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
;*    stores values on the direct object. It might use the setter      */
;*    of one of the object's prototypes but once again, the object     */
;*    to be used in the setter, is the direct object itself.           */
;*                                                                     */
;*    At the first level, special put! form for Array, String, etc.    */
;*    are overriden by method of the js-put! function.                 */
;*---------------------------------------------------------------------*/
(define (js-put-jsobject! o p v
	   throw::bool extend::bool
	   %this::JsGlobalObject
	   cache #!optional (point -1) (cspecs '()))
   
   (define name (js-toname p %this))
   
   (define (invalidate-cache-method! v methods i)
      (when (and (isa? v JsFunction) (vector-ref methods i))
	 (vector-set! methods i #f)
	 #t))
   
   (define (validate-cache-method! v methods i)
      (when (isa? v JsFunction)
	 (vector-set! methods i #t)))
   
   (define (reject msg)
      (if throw
	  (js-raise-type-error %this
	     (format "[[PUT]], ~a ~~s" msg) (js-toname p %this))
	  v))
   
   (define (update-from-descriptor! o obj index::long v desc)
      ;; 8.12.5
      (with-access::JsAccessorDescriptor desc (set)
	 (if (isa? set JsFunction)
	     ;; 8.12.5, step 5
	     (begin
		(when (and (>=fx index 0) cache)
		   (js-pcache-update-descriptor! cache index o obj))
		(js-call1 %this set o v)
		v)
	     ;; 8.12.4, setp 2.a
	     (reject "No setter defined"))))
   
   (define (update-mapped-object! obj i)
      (with-trace 'prop "update-mapped-object"
	 (trace-item "name=" name)
	 (with-access::JsObject obj (cmap elements)
	    (with-access::JsConstructMap cmap (nextmap methods props)
	       (let ((el-or-desc (vector-ref elements i)))
		  (cond
		     ((isa? el-or-desc JsAccessorDescriptor)
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
				 (js-property-value-set! o el-or-desc v %this))
				(else
				 (when (invalidate-cache-method! v methods i)
				    (reset-cmap-vtable! cmap)
				    (set! cmap (duplicate::JsConstructMap cmap
						  (vlen 0)
						  (vtable '#()))))
				 (when cache
				    [assert (i) (<fx i (vector-length elements))]
				    (js-pcache-update-direct! cache i o))
				 (vector-set! elements i v)
				 v))))
			 ((flags-writable? (prop-flags (vector-ref props i)))
			  (when (invalidate-cache-method! v methods i)
			     (reset-cmap-vtable! cmap)
			     (set! cmap (duplicate::JsConstructMap cmap
					   (vlen 0)
					   (vtable '#()))))
			  (when cache
			     [assert (i) (<fx i (vector-length elements))]
			     (js-pcache-update-direct! cache i o))
			  (vector-set! elements i v)
			  v)
			 (else
			  (reject "Read-only property"))))
		     ((not (js-object-mode-extensible? obj))
		      ;; 8.12.9, step 3
		      (reject "sealed object"))
		     ((not (isa? el-or-desc JsPropertyDescriptor))
		      ;; 8.12.5, step 6
		      (js-invalidate-pcaches-pmap! %this)
		      (extend-object!))
		     (else
		      (with-access::JsDataDescriptor el-or-desc (writable)
			 (if writable
			     ;; 8.12.5, step 6
			     (extend-object!)
			     ;; 8.12.4, step 8.b
			     (reject "Read-only property"))))))))))
   
   (define (extend-mapped-object!)
      ;; 8.12.5, step 6
      (with-access::JsObject o (cmap elements)
	 (with-access::JsConstructMap cmap (props single)
	    (let* ((name (js-toname p %this))
		   (index (vector-length props))
		   (flags (property-flags #t #t #t #f)))
	       (let loop ()
		  (cond
		     ((cmap-find-transition cmap name v flags)
		      =>
		      (lambda (nextmap)
			 ;; follow the next map
			 (with-access::JsConstructMap nextmap (ctor)
			    (when cache
			       [assert (index p) (<=fx index (vector-length elements))]
			       (js-pcache-next-direct! cache o nextmap index))
			    [assert (o) (isa? nextmap JsConstructMap)]
			    (js-object-push/ctor! o index v ctor)
			    (set! cmap nextmap)
			    v)))
		     (single
		      (extend-cmap! cmap name flags)
		      (with-access::JsConstructMap cmap (methods)
			 (validate-cache-method! v methods index))
		      (with-access::JsConstructMap cmap (ctor)
			 (when cache
			    (js-pcache-next-direct! cache o cmap index))
			 (js-object-push/ctor! o index v ctor))
		      v)
		     (else
		      ;; create a new map
		      (let ((nextmap (extend-cmap cmap name flags)))
			 (with-access::JsConstructMap nextmap (methods)
			    (validate-cache-method! v methods index))
			 (with-access::JsConstructMap cmap (ctor)
			    (when cache
			       (js-pcache-next-direct! cache o nextmap index))
			    (link-cmap! cmap nextmap name v flags)
			    [assert (o) (isa? nextmap JsConstructMap)]
			    (js-object-push/ctor! o index v ctor))
			 (set! cmap nextmap)
			 v))))))))
   
   (define (update-properties-object! obj desc)
      (with-trace 'prop "update-properties-object!"
	 (cond
	    ((isa? desc JsAccessorDescriptor)
	     ;; 8.12.5, step 5
	     (update-from-descriptor! o o -1 v desc))
	    ((eq? o obj)
	     ;; 8.12.5, step 3
	     (let ((owndesc desc))
		(with-access::JsValueDescriptor owndesc (writable value)
		   (if (not writable)
		       ;; 8.12.4, step 2.b
		       (reject "Read-only property")
		       ;; 8.12.5, step 3,b
		       (begin
			  (set! value v)
			  v)))))
	    ((not (js-object-mode-extensible? obj))
	     ;; 8.12.9, step 3
	     (reject "sealed object"))
	    (else
	     (with-access::JsDataDescriptor desc (writable)
		(if writable
		    ;; 8.12.5, step 6
		    (extend-object!)
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
	    (js-define-own-property o name newdesc throw %this)
	    v)))
   
   (define (extend-object!)
      (with-trace 'prop "extend-object!"
	 (with-access::JsObject o (cmap)
	    (cond
	       ((not (js-object-mode-extensible? o))
		;; 8.12.9. step 3
		(reject "sealed objet"))
	       ((not extend)
		;; 11.13.1
		(js-raise-reference-error %this
		   "[[PUT]], \"~a\" is not defined" p))
	       ((not (eq? cmap (js-not-a-cmap)))
		;; 8.12.5, step 6
		(extend-mapped-object!))
	       (else
		;; 8.12.5, step 6
		(extend-properties-object!))))))

   (when (symbol? name)
      (js-profile-log-put name))

   (let loop ((obj o))
      (jsobject-find obj name
	 update-mapped-object!
	 update-properties-object!
	 extend-object!
	 loop)))

;*---------------------------------------------------------------------*/
;*    js-put/debug! ...                                                */
;*---------------------------------------------------------------------*/
(define (js-put/debug! _o prop v::obj throw::bool %this::JsGlobalObject loc)
   (cond
      ((pair? _o)
       (js-put-pair! _o (js-toname prop %this) v throw %this))
      (else
       (let ((o (js-toobject/debug %this loc _o)))
	  (js-put! _o prop v throw %this)))))

;*---------------------------------------------------------------------*/
;*    js-put/cache! ...                                                */
;*---------------------------------------------------------------------*/
(define (js-put/cache! o prop v::obj throw::bool %this
	   cache::JsPropertyCache #!optional (point -1) (cspecs '()))
   (if (or (not (string? prop)) (not (isa? o JsObject)))
       (js-put! o prop v throw %this)
       (let ((pname (js-toname prop %this)))
	  (js-put! o pname v throw %this))))

;*---------------------------------------------------------------------*/
;*    js-put-name/cache ...                                            */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-put-name/cache! o prop::symbol v::obj throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache #!optional (point -1) (cspecs '()))
   (if (isa? o JsObject)
       (js-object-put-name/cache! o prop v throw %this cache point cspecs)
       (js-put! o prop v throw %this)))

;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache! ...                                    */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache! o::JsObject prop::symbol v::obj throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache #!optional (point -1) (cspecs '()))
   ;; rewritten by macro expansion
   (js-object-put-name/cache! o prop v throw %this cache point cspecs))

;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache-miss! ...                               */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache-miss! o::JsObject prop::symbol v::obj
	   throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache #!optional (point -1) (cspecs '()))
   (with-access::JsObject o (cmap)
      (let* ((%omap cmap)
	     (tmp (js-put-jsobject! o prop v throw #t %this cache point cspecs)))
	 (with-access::JsPropertyCache cache (cntmiss name (cpoint point) usage)
	    (set! cntmiss (+fx 1 cntmiss))
	    (set! name prop)
	    (set! cpoint point)
	    (set! usage 'put))

	 (unless (eq? %omap (js-not-a-cmap))
	    (with-access::JsPropertyCache cache (index vindex cntmiss)
	       (when (=fx cntmiss (vtable-threshold))
		   (when (>=fx index 0)
		      (when (=fx vindex (js-not-a-index))
			 (set! vindex (js-get-vindex %this)))
		      (js-cmap-vtable-add! %omap vindex (cons index cmap))))))
	 tmp)))

;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache-imap+! ...                              */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache-imap+! o::JsObject prop::symbol v::obj
	   throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache #!optional (point -1) (cspecs '()))
   (js-object-put-name/cache! o prop v throw %this cache point
      '(cmap pmap amap vtable)))

;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache-cmap+! ...                              */
;*    -------------------------------------------------------------    */
;*    !!! Overriden in property_expd.sch                               */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache-cmap+! o::JsObject prop::symbol v::obj
	   throw::bool
	   %this::JsGlobalObject
	   cache::JsPropertyCache #!optional (point -1) (cspecs '()))
   (js-object-put-name/cache! o prop v throw %this cache point
      '(pmap amap vtable)))

;*---------------------------------------------------------------------*/
;*    js-bind! ...                                                     */
;*    -------------------------------------------------------------    */
;*    This is a simplified version of DefineOwnProperty used to build  */
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
      (when (isa? v JsFunction)
	 (vector-set! methods i #t)))
   
   (define (accessor-property? get set)
      (or (and get (not (eq? get (js-undefined))))
	  (and set (not (eq? set (js-undefined))))))
   
   (define (check-accessor-property! get set)
      (cond
	 ((not (or (isa? get JsFunction) (eq? get (js-undefined))))
	  (js-raise-type-error %this
	     (format "wrong getter for property \"~a\", ~~a" name)
	     get))
	 ((and set (not (eq? set (js-undefined))) (not (isa? set JsFunction)))
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
		      (when (eq? get (js-undefined)) (set! get oget))
		      (when (eq? set (js-undefined)) (set! set oset))))
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
      
      ;; 8.12.5, step 6
      (with-access::JsObject o (cmap elements)
	 (with-access::JsConstructMap cmap (props)
	    (let* ((axs (accessor-property? get set))
		   (index (vector-length props))
		   (inl (js-object-inline-next-element? o index))
		   (flags (property-flags writable enumerable configurable axs)))
	       (cond
		  ((cmap-find-transition cmap name value flags)
		   =>
		   (lambda (nextmap)
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
			  (nextmap (next-cmap o name #f flags)))
		      (check-accessor-property! get set)
		      ;; extending the elements vector is mandatory
		      (js-object-push! o index newdesc)
		      (js-undefined)))
		  (else
		   ;; create a new map with a JsIndexDescriptor
		   (let* ((newdesc (instantiate::JsValueDescriptor
				      (name name)
				      (value value)
				      (writable writable)
				      (enumerable enumerable)
				      (configurable configurable)))
			  (nextmap (next-cmap o name value flags)))
		      (with-access::JsConstructMap nextmap (methods)
			 (validate-cache-method! value methods index))
		      (js-object-push! o index newdesc)
		      value)))))))
   
   (define (update-properties-object! obj owndesc)
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
	  (let ((props (filter! (lambda (d)
				   (with-access::JsPropertyDescriptor d ((n name))
				      (not (eq? n name))))
			  (js-object-properties obj))))
	     (js-object-properties-set! obj
		(cons (instantiate::JsValueDescriptor
			 (name name)
			 (value value)
			 (writable writable)
			 (enumerable enumerable)
			 (configurable configurable))
		   props))
	     value)))
   
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
      (if (not (eq? cmap (js-not-a-cmap)))
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
(define (js-define %this obj id get set src pos)
   (let ((name (if (>=fx (bigloo-debug) 1)
		   (string-append src ":" (integer->string pos))
		   src)))
      (js-bind! %this obj id
	 :configurable #f
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
		(symbol->string! (js-toname p %this)))
	     o))
	 (else
	  #f)))
   
   (let ((n (js-toname p %this))
	 (o (js-toobject %this _o)))
      (cond
	 ((isa? o JsObject)
	  (with-access::JsObject o (cmap)
	     (if (not (eq? cmap (js-not-a-cmap)))
		 (jsobject-map-find o n
		    (lambda (o i)
		       (delete-configurable o
			  (configurable-mapped-property? o i)
			  (lambda (o)
			     (js-invalidate-pcaches-pmap! %this)
			     ;; create a new cmap for the object
			     (let ((nextmap (clone-cmap cmap)))
				(link-cmap! cmap nextmap n #f -1)
				[assert (o) (isa? nextmap JsConstructMap)]
				(set! cmap nextmap)
				(with-access::JsConstructMap nextmap (props)
				   ;; remove the prop from the cmap
				   (vector-set! props i (prop #f 0))
				   #t)))))
		    (lambda () #t))
		 (jsobject-properties-find o n
		    (lambda (o d)
		       (with-access::JsPropertyDescriptor d (configurable)
			  (delete-configurable o configurable
			     (lambda (o)
				(js-object-properties-set! o
				   (remq! d (js-object-properties o)))
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
      ;; (propagate-property-descriptor! desc current)
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
      (propagate-property-descriptor! current desc)
      (with-access::JsDataDescriptor current (writable)
	 (with-access::JsDataDescriptor desc ((dwritable writable))
	    (when (boolean? dwritable)
	       (set! writable dwritable)))
	 (when (isa? desc JsValueDescriptor)
	    (with-access::JsValueDescriptor desc ((dvalue value))
	       (cond
		  ((isa? current JsValueDescriptor)
		   (with-access::JsValueDescriptor current (value)
		      (set! value dvalue)))
		  ((isa? current JsWrapperDescriptor)
		   (with-access::JsWrapperDescriptor current (%set)
		      (%set o dvalue)))))))
      #t)
   
   (define (propagate-accessor-descriptor! current desc)
      (propagate-property-descriptor! current desc)
      (with-access::JsAccessorDescriptor current (get %get set)
	 (with-access::JsAccessorDescriptor desc ((dget get)
						  (%dget %get)
						  (dset set))
	    (when dget (set! get dget) (set! %get %dget))
	    (when dset (set! set dset))))
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
   
   (define (same-descriptor? current desc)
      (when (same-property-descriptor? current desc)
	 (cond
	    ((isa? current JsAccessorDescriptor)
	     (and (isa? desc JsAccessorDescriptor)
		  (same-accessor-descriptor? current desc)))
	    ((isa? desc JsValueDescriptor)
	     (and (same-data-descriptor? current desc)
		  (with-access::JsValueDescriptor current ((cvalue value))
		     (with-access::JsValueDescriptor desc ((dvalue value))
			(equal? cvalue dvalue)))))
	    ((isa? desc JsDataDescriptor)
	     (and (same-data-descriptor? current desc)
		  (with-access::JsValueDescriptor current (value)
		     (eq? value (js-undefined)))))
	    ((isa? desc JsAccessorDescriptor)
	     #f)
	    (else
	     (and (same-property-descriptor? current desc)
		  (with-access::JsValueDescriptor current (value writable)
		     (and (eq? value (js-undefined)) #f)))))))
   
   ;; MS CARE, to be improved
   (js-object-unmap! o)
   (let ((current (js-get-own-property o name %this)))
      ;; 1 & 2
      (cond
	 ((eq? current (js-undefined))
	  (cond
	     ((not (js-object-mode-extensible? o))
	      ;; 3
	      (reject (format "~a not extensible" (js-tostring o %this))))
	     (else
	      ;; 4
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
		 (js-object-properties-set! o
		    (append! (js-object-properties o) (list ndesc)))
		 #t))))
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
		    (format "~a.~a configurability mismatch"
		       (js-tostring o %this) name)))
		((and (boolean? (enumerable desc))
		      (not (eq? (enumerable current) (enumerable desc))))
		 ;; 7.b
		 (reject
		    (format "~a.~a enumerability mismatch"
		       (js-tostring o %this) name)))))
	  (unless rejected
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
			(format "~a.~a configurability mismatch"
			   (js-tostring o %this) name)))
		    ((isa? current JsDataDescriptor)
		     ;; 9.b
		     (when (isa? desc JsAccessorDescriptor)
			(with-access::JsAccessorDescriptor desc (set get)
			   (unless (isa? get JsFunction)
			      (set! get (js-undefined)))
			   (unless (isa? set JsFunction)
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
			    (format "~a.~a read-only"
			       (js-tostring o %this) name)))
			((eq? (writable current) #f)
			 ;; 10.a.ii
			 (if (and (isa? desc JsValueDescriptor)
				  (not (same-value (value desc) (value current))))
			     (reject
				(format "~a.~a value mismatch"
				   (js-tostring o %this) name))
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
			    (format "~a.~a setter mismatch"
			       (js-tostring o %this) name)))
			((and (get desc)
			      (not (equal? (get current) (get desc))))
			 (reject
			    (format "~a.~a getter mismatch"
			       (js-tostring o %this) name)))
			(else
			 (propagate-accessor-descriptor! current desc)))
		     (propagate-accessor-descriptor! current desc)))
		(else
		 ;; 12 & 13
		 (propagate-property-descriptor! current desc))))))))

;*---------------------------------------------------------------------*/
;*    js-replace-own-property! ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (js-replace-own-property! o::JsObject old new)
   
   (define (replace-list! lst old new)
      (let loop ((lst lst))
	 (cond
	    ((null? lst)
	     #f)
	    ((eq? (car lst) old)
	     (set-car! lst new)
	     #t)
	    (else
	     (loop (cdr lst))))))
   
   (let loop ((o o))
      (with-access::JsObject o (__proto__)
	 (unless (replace-list! (js-object-properties o) old new)
	    (loop __proto__)))))

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
(define-generic (js-prevent-extensions o::JsObject)
   (js-object-mode-extensible-set! o #f))

;*---------------------------------------------------------------------*/
;*    js-for-in ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.4       */
;*    http://www.ecma-international.org/ecma-262/6.0/#sec-13.7.5       */
;*---------------------------------------------------------------------*/
(define-generic (js-for-in obj proc %this)
   (if (or (eq? obj (js-undefined)) (eq? obj (js-null)))
       (js-undefined)
       (js-for-in (js-cast-object obj %this "for") proc %this)))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsObject ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.4       */
;*---------------------------------------------------------------------*/
(define-method (js-for-in obj::JsObject proc %this)
   
   (define env '())
   
   (define (vfor-each proc vec vecname)
      (let ((len (vector-length vecname)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (proc (vector-ref vec i) (vector-ref vecname i))
	       (loop (+fx i 1))))))
   
   (define (in-mapped-property el-or-descr prop)
      (when (and prop (flags-enumerable? (prop-flags prop)))
	 (let ((name (prop-name prop)))
	    (when (symbol? name)
	       (unless (memq name env)
		  (set! env (cons name env))
		  (proc (js-string->jsstring (symbol->string! name))))))))
   
   (define (in-property p)
      (when (isa? p JsPropertyDescriptor)
	 (with-access::JsPropertyDescriptor p (name enumerable)
	    (when (symbol? name)
	       (unless (memq name env)
		  (when (eq? enumerable #t)
		     (set! env (cons name env))
		     (proc (js-string->jsstring (symbol->string! name)))))))))

   (let loop ((o obj))
      (with-access::JsObject o (cmap __proto__ elements)
	 (if (not (eq? cmap (js-not-a-cmap)))
	     (with-access::JsConstructMap cmap (props)
		(vfor-each in-mapped-property elements props))
	     (for-each in-property (js-object-properties o)))
	 (when (isa? __proto__ JsObject)
	    (loop __proto__)))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::Object ...                                           */
;*---------------------------------------------------------------------*/
(define-method (js-for-in obj::object proc %this)
   (let ((jsobj (js-toobject %this obj)))
      (if (eq? obj jsobj)
	  (let ((fields (class-all-fields (object-class obj))))
	     (let loop ((i 0))
		(when (<fx i (vector-length fields))
		   (proc
		      (js-string->jsstring
			 (symbol->string
			    (class-field-name (vector-ref fields i)))))
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
	     (when (isa? fun JsFunction)
		(js-for-of-iterator (js-call0 %this fun obj) obj proc close %this))))))

;*---------------------------------------------------------------------*/
;*    js-for-of-iterator ...                                           */
;*---------------------------------------------------------------------*/
(define (js-for-of-iterator iterator obj proc close %this)
   
   (define (for next)
      (let loop ()
	 (let ((n (js-call0 %this next iterator)))
	    (unless (eq? (js-get n 'done %this) #t)
	       (proc (js-get n 'value %this))
	       (loop))))
      #t)
   
   (let ((next (js-get iterator 'next %this))
	 (exn #t))
      (if (isa? next JsFunction)
	  (if close
	      (unwind-protect
		 (begin
		    (for next)
		    (set! exn #f))
		 (when exn
		    (let ((return (js-get iterator 'return %this)))
		       (when (isa? return JsFunction)
			  (js-call0 %this return iterator)))))
	      (for next)))))
			 
;*---------------------------------------------------------------------*/
;*    js-call/cache ...                                                */
;*---------------------------------------------------------------------*/
(define (js-call/cache %this cache obj this . args)
   (let ((largs (length args)))
      (with-access::JsPropertyCache cache (owner method cmap)
	 (cond
	    ((eq? owner obj)
	     (apply method this args))
	    ((and (isa? obj JsFunction)
		  (with-access::JsFunction obj (len)
		     (=fx len largs)))
	     (with-access::JsFunction obj (procedure)
		(set! cmap obj)
		(set! method procedure)
		(apply procedure this args)))
	    (else
	     (js-apply %this obj this args))))))
   
;*---------------------------------------------------------------------*/
;*    js-method-call-name/cache ...                                    */
;*---------------------------------------------------------------------*/
(define (js-method-call-name/cache %this::JsGlobalObject obj::obj name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   point::long ccspecs::pair-nil ocspecs
	   . args)
   (if (isa? obj JsObject)
       (apply js-object-method-call-name/cache %this obj name
	  ccache ocache point ccspecs ocspecs args)
       (let ((o (js-toobject %this obj)))
	  (js-apply %this (js-get o name %this) o args))))

;*---------------------------------------------------------------------*/
;*    js-object-method-call-name/cache ...                             */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-object-method-call-name/cache %this::JsGlobalObject
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
	     (let ((f (js-get obj name %this)))
		(js-apply %this f obj args)))
	    (else
	     ;; cache miss
	     (with-access::JsConstructMap omap (vlen vtable %id)
		(if (and (<fx vindex vlen)
			 (procedure? (vector-ref vtable vindex)))
		    (let ((m (vector-ref vtable vindex)))
		       (apply m obj args))
		    (js-object-method-call/cache-miss %this obj name args
		       ccache ocache point ccspecs ocspecs))))))))

;*---------------------------------------------------------------------*/
;*    js-object-method-call/cache-miss ...                             */
;*    -------------------------------------------------------------    */
;*    This function is called on a true cache miss, i.e., this call    */
;*    has already been filled with another method.                     */
;*---------------------------------------------------------------------*/
(define (js-object-method-call/cache-miss %this::JsGlobalObject
	   o::JsObject name::obj args::pair-nil
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   point::long ccspecs::pair-nil ocspecs::pair-nil)
   
   (with-access::JsPropertyCache ccache (cntmiss (cname name) (cpoint point) usage)
      (set! cntmiss (+fx 1 cntmiss))
      (set! cname name)
      (set! cpoint point)
      (set! usage 'call))
   
   (with-access::JsPropertyCache ccache (pmap vindex method)
      (when (and (procedure? method)
		 (isa? pmap JsConstructMap)
		 (=fx (procedure-arity method) (+fx 1 (length args))))
	 (when (=fx vindex (js-not-a-index))
	    (set! vindex (js-get-vindex %this)))
	 (js-cmap-vtable-add! pmap vindex method))
      (js-object-method-call/cache-fill %this o name args
	 ccache ocache point ccspecs ocspecs)))

;*---------------------------------------------------------------------*/
;*    js-object-method-call/cache-fill ...                             */
;*    -------------------------------------------------------------    */
;*    This function is called when a ccache has to be filled, i.e.,    */
;*    on the first call.                                               */
;*    -------------------------------------------------------------    */
;*    .call and .apply (not yet for the former) are handled in this    */
;*    function.                                                        */
;*---------------------------------------------------------------------*/
(define (js-object-method-call/cache-fill %this::JsGlobalObject
	   o::JsObject name::obj args::pair-nil
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   point::long ccspecs::pair-nil ospecs::pair-nil)
   
   (define (jsapply method)
      (if (procedure? method)
	  (apply method args)
	  (apply js-calln %this method o args)))

   (define (funval el-or-desc)
      (if (isa? el-or-desc JsPropertyDescriptor)
	  (with-access::JsGlobalObject %this (js-call)
	     (let ((f (js-property-value o el-or-desc %this)))
		(if (eq? f js-call)
		    (with-access::JsFunction o (procedure)
		       (if (=fx (procedure-arity procedure) (length args))
			   procedure
			   f))
		    f)))
	  el-or-desc))

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
	  (lambda (this . args) (apply f args)))))

   (define (procedureN procedure largs)
      (case (procedure-arity procedure)
	 ((1)
	  (case largs
	     ((0) (lambda (this) (procedure this)))
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
	     ((1) (lambda (this a0) (procedure this a0)))
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
	     ((2) (lambda (this a0 a1) (procedure this a0 a1)))
	     ((3) (lambda (this a0 a1 a2) (procedure this a0 a1)))
	     ((4) (lambda (this a0 a1 a2 a3) (procedure this a0 a1)))
	     ((5) (lambda (this a0 a1 a2 a3 a4) (procedure this a0 a1)))
	     ((6) (lambda (this a0 a1 a2 a3 a4 a5) (procedure this a0 a1)))
	     ((7) (lambda (this a0 a1 a2 a3 a4 a5 a6) (procedure this a0 a1)))
	     (else (lambda (this a0 a1 a2 a3 a4 a5 a6 . _) (procedure this a0 a1)))))
	 (else
	  #f)))

   (js-profile-log-call name)

   (let loop ((obj o))
      (jsobject-find obj name
	 ;; map search
	 (lambda (obj i)
	    ;;(tprint "js-object-method-call/cache-miss name=" name " MAPPED")
	    (with-access::JsObject o ((omap cmap) __proto__)
	       (with-access::JsObject obj ((wmap cmap) elements)
		  (with-access::JsConstructMap wmap (methods %id)
		     (let ((el-or-desc (vector-ref elements i)))
			(cond
			   ((isa? el-or-desc JsAccessorDescriptor)
			    (with-access::JsPropertyCache ccache (pmap emap cmap)
			       (set! pmap #t)
			       (set! emap #t)
			       (set! cmap #t))
			    (jsapply (js-property-value o el-or-desc %this)))
			   ((eq? (vector-ref methods i) #t)
			    (let ((f (funval el-or-desc)))
			       (cond
				  ((procedure? f)
				   (with-access::JsPropertyCache ccache (pmap emap cmap index method)
				      ;; correct arity, put in cache
				      (set! pmap omap)
				      (set! emap #t)
				      (set! cmap #f)
				      (set! index i)
				      (set! method (method->procedure f))))
				  ((isa? f JsFunction)
				   (with-access::JsFunction f (len method)
				      (cond
					 ((=fx (procedure-arity method) (+fx 1 (length args)))
					  (with-access::JsPropertyCache ccache (pmap emap cmap index (cmethod method))
					     ;; correct arity, put in cache
					     (set! pmap omap)
					     (set! emap #t)
					     (set! cmap #f)
					     (set! index i)
					     (set! cmethod method)))
					 ((procedureN method (length args))
					  =>
					  (lambda (procedure)
					     (with-access::JsPropertyCache ccache (pmap emap cmap index (cmethod method))
						;; correct arity, put in cache
						(set! pmap omap)
						(set! emap #t)
						(set! cmap #f)
						(set! index i)
						(set! cmethod procedure))))
					 (else
					  ;; arity missmatch, never cache
					  (with-access::JsPropertyCache ccache (pmap emap cmap)
					     (set! pmap #t)
					     (set! emap #t)
					     (set! cmap #t))
					  )))))
			       (jsapply f)))
			   (else
			    (with-access::JsPropertyCache ccache (pmap cmap emap)
			       ;; invalidate the call cache and update the
			       ;; object cache
			       (set! cmap #t)
			       (set! pmap #t)
			       (set! emap #t)
			       (jsapply (funval el-or-desc))))))))))
	 ;; property search
	 (lambda (obj v)
	    (with-access::JsPropertyCache ccache (cmap emap pmap)
	       (set! pmap #t)
	       (set! emap #t)
	       (set! cmap #t)
	       (jsapply (js-property-value o v %this))))
	 ;; not found
	 (lambda ()
	    (js-raise-type-error %this "call: not a function ~s"
	       (js-undefined)))
	 ;; loop
	 loop)))

