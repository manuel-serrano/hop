;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/property.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 25 07:05:26 2013                          */
;*    Last change :  Wed Apr 19 10:43:13 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript property handling (getting, setting, defining and     */
;*    deleting).                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_property

   (library hop)

   (option  (register-srfi! 'cache-level2)
            (bigloo-compiler-debug-set! 0))
   
   (include "stringliteral.sch"
	    "property.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_worker
	   __hopscript_pair
	   __hopscript_function
	   __hopscript_lib)
   
   (extern ($js-make-pcache::obj (::obj ::int ::JsPropertyCache)
	      "bgl_make_pcache")
	   ($js-invalidate-pcaches-pmap!::void (::obj)
	      "bgl_invalidate_pcaches_pmap"))
   
   (export (%define-pcache ::int)
	   (js-make-pcache ::int)
	   (js-invalidate-pcaches-pmap!)
	   (inline js-pcache-ref ::obj ::int)
	   (inline js-pcache-cmap ::JsPropertyCache)
	   
	   (js-names->cmap::JsConstructMap ::vector)
	   (js-object-literal-init! ::JsObject)
	   
	   (js-object-unmap! ::JsObject)
	   (js-toname::obj ::obj ::JsGlobalObject)
	   
	   (inline js-is-accessor-descriptor?::bool obj)
	   (inline js-is-data-descriptor?::bool obj)
	   (inline js-is-generic-descriptor?::bool obj)
	   (js-from-property-descriptor ::JsGlobalObject desc ::obj)
	   (js-to-property-descriptor ::JsGlobalObject desc ::obj)
	   (js-property-value ::obj ::JsPropertyDescriptor ::JsGlobalObject)
	   (js-property-value-set! obj::JsObject ::JsPropertyDescriptor v ::JsGlobalObject)
	   (js-elements-push!::vector vec::vector index::long value)
	   
	   (generic js-properties-name::vector ::obj ::bool ::JsGlobalObject)
	   (generic js-properties-symbol::vector ::obj ::JsGlobalObject)
	   
	   (generic js-has-property::bool ::obj ::obj ::JsGlobalObject)
	   (generic js-get-own-property ::obj ::obj ::JsGlobalObject)
	   
	   (generic js-get-property-value ::obj ::obj ::obj ::JsGlobalObject)
	   
	   (js-get-lookup ::JsObject ::obj ::JsPropertyCache ::bool ::JsGlobalObject)
	   (js-get-property ::JsObject ::obj ::JsGlobalObject)
	   
	   (js-get-notfound ::obj ::obj ::JsGlobalObject)
	   (generic js-get ::obj ::obj ::JsGlobalObject)
	   (generic js-get-length ::obj ::obj ::JsGlobalObject)
	   (js-get/debug ::obj ::obj ::JsGlobalObject loc)
	   (js-get/cache ::obj ::obj ::JsPropertyCache ::JsGlobalObject)
	   (js-get-name/cache ::obj ::obj ::JsPropertyCache ::JsGlobalObject)
	   (js-object-get-name/cache ::JsObject ::obj ::JsPropertyCache ::JsGlobalObject)
	   (js-object-get-name/cache-level1 ::JsObject ::obj ::JsPropertyCache ::JsGlobalObject)
	   (js-object-get-name/cache-level2 ::JsObject ::obj ::JsPropertyCache ::JsGlobalObject)
	   (generic js-get-name/cache-miss ::JsObject ::obj ::JsPropertyCache ::obj ::JsGlobalObject)
	   (js-global-object-get-name ::JsObject ::symbol ::obj ::JsGlobalObject)
	   (js-global-object-get-name/cache
              ::JsObject ::symbol ::JsPropertyCache ::obj ::JsGlobalObject)
;* 	   (inline js-this-object-get-name/cache                       */
;*               ::JsObject ::symbol ::JsPropertyCache ::obj ::JsGlobalObject) */
	   
	   
	   (js-can-put o::JsObject ::obj ::JsGlobalObject)
	   (js-unresolved-put! ::JsObject ::obj ::obj ::bool ::JsGlobalObject)
	   (js-unresolved-eval-put! ::JsObject ::obj ::obj ::bool ::JsGlobalObject)
	   (js-decl-eval-put! ::JsObject ::obj ::obj ::bool ::JsGlobalObject)
	   
	   (generic js-put! ::obj ::obj ::obj ::bool ::JsGlobalObject)
	   (generic js-put-length! ::obj ::obj ::bool ::obj ::JsGlobalObject)
	   (js-put-jsobject! ::JsObject ::obj ::obj throw ::bool ::obj ::JsGlobalObject)
	   (js-put/debug! ::obj ::obj ::obj ::bool ::JsGlobalObject loc)
	   (js-put/cache! ::obj ::obj ::obj ::bool ::JsPropertyCache ::JsGlobalObject)
	   (js-put-name/cache! ::obj ::obj ::obj ::bool ::JsPropertyCache ::JsGlobalObject)
	   (js-object-put-name/cache! ::JsObject ::obj ::obj ::bool ::JsPropertyCache ::JsGlobalObject)
	   (js-object-put-name/cache-level1! ::JsObject ::obj ::obj ::bool ::JsPropertyCache ::JsGlobalObject)
	   (js-object-put-name/cache-level2! ::JsObject ::obj ::obj ::bool ::JsPropertyCache ::JsGlobalObject)
;* 	   (js-this-put-name/cache! ::JsObject ::obj ::obj ::bool ::JsPropertyCache ::JsGlobalObject) */
	   (js-object-put-name/cache-miss! ::JsObject ::obj ::obj ::bool ::JsPropertyCache ::JsGlobalObject)
	   
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
	   
	   (generic js-for-in ::obj proc::procedure ::JsGlobalObject)
	   
	   (js-bind! ::JsGlobalObject ::JsObject name::obj #!key
	      (value #f)
	      (get #f)
	      (set #f)
	      (writable #t)
	      (enumerable #t)
	      (configurable #t))

	   (js-define ::JsGlobalObject ::JsObject
	      ::symbol ::procedure ::obj ::obj ::obj)
	   
	   (js-method-call-name/cache ::JsGlobalObject ::obj ::obj
	      ::JsPropertyCache ::JsPropertyCache . args)
	   (js-object-method-call-name/cache ::JsGlobalObject ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache . args)
	   
	   (js-object-method-call0-name/cache-level2 ::JsGlobalObject
	      ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache)
	   (js-object-method-call1-name/cache-level2 ::JsGlobalObject
	      ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache
	      ::obj)
	   (js-object-method-call2-name/cache-level2 ::JsGlobalObject
	      ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache
	      ::obj ::obj)
	   (js-object-method-call3-name/cache-level2 ::JsGlobalObject
	      ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache
	      ::obj ::obj ::obj)
	   (js-object-method-call4-name/cache-level2 ::JsGlobalObject
	      ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache
	      ::obj ::obj ::obj ::obj)
	   (js-object-method-call5-name/cache-level2 ::JsGlobalObject
	      ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache
	      ::obj ::obj ::obj ::obj ::obj)
	   (js-object-method-call6-name/cache-level2 ::JsGlobalObject
	      ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache
	      ::obj ::obj ::obj ::obj ::obj ::obj)
	   (js-object-method-call7-name/cache-level2 ::JsGlobalObject
	      ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache
	      ::obj ::obj ::obj ::obj ::obj ::obj ::obj)
	   (js-object-method-call8-name/cache-level2 ::JsGlobalObject
	      ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache
	      ::obj ::obj ::obj ::obj ::obj ::obj ::obj ::obj)
	   (js-object-method-call9-name/cache-level2 ::JsGlobalObject
	      ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache
	      ::obj ::obj ::obj ::obj ::obj ::obj ::obj ::obj ::obj)
	   (js-object-method-calln-name/cache-level2 ::JsGlobalObject
	      ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache
	      . ::pair-nil)
	   
	   (js-object-method-call/cache-miss ::JsGlobalObject ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache ::pair-nil)
	   (js-object-method-call/cache-fill ::JsGlobalObject ::JsObject ::obj
	      ::JsPropertyCache ::JsPropertyCache ::pair-nil)
	   
	   (js-call/cache ::JsGlobalObject obj ::JsPropertyCache this . args)

	   (inline js-vindex-max::long)
	   (js-get-vindex::long ::JsGlobalObject)
	   (js-vtable-add!::obj ::JsConstructMap ::long ::obj)

	   (log-cache-miss!)
	   (add-cache-miss! ::symbol ::obj)
	   (show-cache-misses)))

;*---------------------------------------------------------------------*/
;*    js-elements-push! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-elements-push!::vector vec::vector index::long value)
   (let ((len (vector-length vec)))
      (if (>=fx index len)
	  (let ((nvec (copy-vector vec (+fx 1 index))))
	     (vector-set! nvec index value)
	     nvec)
	  (begin
	     (vector-set! vec index value)
	     vec))))

;*---------------------------------------------------------------------*/
;*    js-elements-push/ctor! ...                                       */
;*---------------------------------------------------------------------*/
(define (js-elements-push/ctor!::vector ctor vec::vector index::long value)
   (let ((len (vector-length vec)))
      (if (>=fx index len)
	  (let ((nvec (copy-vector vec (+fx 1 index))))
	     (when (isa? ctor JsFunction)
		(with-access::JsFunction ctor (constrsize)
		   (set! constrsize (+fx 1 constrsize))))
	     (vector-set! nvec index value)
	     nvec)
	  (begin
	     (vector-set! vec index value)
	     vec))))

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
(define (js-make-pcache len)
   (let ((pcache ($make-vector-uncollectable len #unspecified)))
      (let loop ((i 0))
	 (if (=fx i len)
	     (register-pcache! pcache len)
	     (begin
		(vector-set! pcache i (instantiate::JsPropertyCache))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    *pcaches* ...                                                    */
;*---------------------------------------------------------------------*/
(define *pcaches* *pcaches*)

;*---------------------------------------------------------------------*/
;*    register-pcache! ...                                             */
;*---------------------------------------------------------------------*/
(define (register-pcache! pcache len)
   ;; bootstrap initialization
   (when (eq? *pcaches* #unspecified) (set! *pcaches* '()))
   (set! *pcaches* (cons (cons pcache len) *pcaches*))
   pcache)

;*---------------------------------------------------------------------*/
;*    js-invalidate-pcaches-pmap! ...                                  */
;*    -------------------------------------------------------------    */
;*    Called when a __proto__ is changed or when an accessor           */
;*    property or a non default data property is added to an           */
;*    object.                                                          */
;*---------------------------------------------------------------------*/
(define (js-invalidate-pcaches-pmap!)
   
   (define (invalidate-pcache-pmap! pcache)
      (with-access::JsPropertyCache pcache (pmap)
	 (when (isa? pmap JsConstructMap)
	    (with-access::JsConstructMap pmap (vtable vlen)
	       (set! vlen 0)
	       (set! vtable '#())))
	 (set! pmap #t)))
   
   ($js-invalidate-pcaches-pmap! invalidate-pcache-pmap!)
   (for-each (lambda (pcache-entry)
		(let ((vec (car pcache-entry)))
		   (let loop ((i (-fx (cdr pcache-entry) 1)))
		      (when (>=fx i 0)
			 (let ((pcache (vector-ref vec i)))
			    (invalidate-pcache-pmap! pcache))
			 (loop (-fx i 1))))))
      *pcaches*))

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
;*    js-pache-invalidate! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-pache-invalidate! pcache::JsPropertyCache)
   (with-access::JsPropertyCache pcache (cmap pmap index owner)
      (set! cmap #t)
      (set! owner #f)
      (set! pmap #t)
      (set! index 0)))
   
;*---------------------------------------------------------------------*/
;*    js-pcache-update-descriptor! ...                                 */
;*    -------------------------------------------------------------    */
;*    Used to access an object's descriptor                            */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-descriptor! pcache::JsPropertyCache i o::JsObject obj)
   [assert (obj) (isa? obj JsObject)]   
   (with-access::JsObject o ((omap cmap))
      (when (isa? omap JsConstructMap)
	 (with-access::JsPropertyCache pcache (cmap pmap index owner)
	    (set! cmap #t)
	    (set! owner obj)
	    (set! pmap omap)
	    (set! index (-fx (negfx i) 1))))))

;*---------------------------------------------------------------------*/
;*    js-pcache-update-direct! ...                                     */
;*    -------------------------------------------------------------    */
;*    Used to access a direct object property.                         */
;*---------------------------------------------------------------------*/
(define (js-pcache-update-direct! pcache::JsPropertyCache i o::JsObject)
   [assert (i) (>=fx i 0)]
   (with-access::JsObject o ((omap cmap))
      (with-access::JsPropertyCache pcache (cmap pmap index owner)
	 (set! cmap omap)
	 (set! pmap #t)
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
      (when (isa? omap JsConstructMap)
	 (with-access::JsPropertyCache pcache (cmap pmap index owner)
	    (set! cmap #t)
	    (set! owner obj)
	    (set! pmap omap)
	    (set! index i)))))

;*---------------------------------------------------------------------*/
;*    js-pcache-next-direct! ...                                       */
;*    -------------------------------------------------------------    */
;*    Used when adding a direct property to an object.                 */
;*---------------------------------------------------------------------*/
(define (js-pcache-next-direct! pcache::JsPropertyCache o::JsObject nextmap i)
   [assert (i) (>=fx i 0)]
   (with-access::JsObject o ((omap cmap))
      (when (isa? omap JsConstructMap)
	 (with-access::JsPropertyCache pcache (pmap cmap index owner)
	    [assert (pcache) (= (cmap-size nextmap) (+ 1 (cmap-size omap)))]
	    (set! owner #f)
	    (set! pmap omap)
	    (set! cmap nextmap)
	    (set! index i)))))

;*---------------------------------------------------------------------*/
;*    cmap-size ...                                                    */
;*---------------------------------------------------------------------*/
(define (cmap-size cmap::JsConstructMap)
   (with-access::JsConstructMap cmap (names) (vector-length names)))

;*---------------------------------------------------------------------*/
;*    pmap-index ...                                                   */
;*---------------------------------------------------------------------*/
(define (pmap-index cmap::JsPropertyCache)
   (with-access::JsPropertyCache cmap (index) index))

;* {*---------------------------------------------------------------------*} */
;* {*    js-pcache-update-method! ...                                     *} */
;* {*---------------------------------------------------------------------*} */
;* (define (js-pcache-update-method! pcache::JsPropertyCache i o::JsObject met) */
;*    (with-access::JsObject o ((omap cmap))                           */
;*       (when (isa? omap JsConstructMap)                              */
;* 	 (with-access::JsPropertyCache pcache (pmap cmap index owner method) */
;* 	    (set! index i)                                             */
;* 	    (set! method met)))))                                      */

;*---------------------------------------------------------------------*/
;*    js-pcache-invalidate-method! ...                                 */
;*---------------------------------------------------------------------*/
(define (js-pcache-invalidate-method! pcache i)
   (with-access::JsPropertyCache pcache (method)
      (when method
	 (set! method #f)
	 #t)))

;*---------------------------------------------------------------------*/
;*    property-flags ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (property-flags writable enumerable configurable)
   `(bit-or (if ,writable 1 0)
       (bit-or (if ,enumerable 2 0)
	  (if ,configurable 4 0))))

;*---------------------------------------------------------------------*/
;*    cmap-transition ...                                              */
;*---------------------------------------------------------------------*/
(define (cmap-transition name value flags)
   (if (eq? name '__proto__)
       (cons value flags)
       (cons name flags)))

;*---------------------------------------------------------------------*/
;*    cmap-same-transition? ...                                        */
;*---------------------------------------------------------------------*/
(define (cmap-same-transition?::bool cmap::JsConstructMap name::obj val flags::int)
   (with-access::JsConstructMap cmap ((t1 transition))
      (and (=fx (cdr t1) flags)
	   (if (eq? name '__proto__)
	       (eq? (car t1) val)
	       (eq? (car t1) name)))))

;*---------------------------------------------------------------------*/
;*    link-cmap! ...                                                   */
;*---------------------------------------------------------------------*/
(define (link-cmap! omap::JsConstructMap nmap::JsConstructMap name value flags::int)
   (with-access::JsConstructMap omap (transition nextmap)
      ;; save the old transition
      (save-cmap-transition! omap)
      (set! transition (cmap-transition name value flags))
      (set! nextmap nmap)
      nmap))

;*---------------------------------------------------------------------*/
;*    find-old-transition ...                                          */
;*---------------------------------------------------------------------*/
(define (find-old-transition omap::JsConstructMap name flags::int)
   (unless (eq? name '__proto__)
      (with-access::JsConstructMap omap (backup)
	 (find (lambda (el)
		  (and (eq? (caar el) name) (=fx (cdar el) flags)))
	    backup))))

;*---------------------------------------------------------------------*/
;*    save-cmap-transition! ...                                        */
;*---------------------------------------------------------------------*/
(define (save-cmap-transition! cmap::JsConstructMap)
   (with-access::JsConstructMap cmap (transition nextmap backup)
      (when nextmap
	 (unless (assq transition backup)
	    (set! backup (cons (cons transition nextmap) backup))))))

;*---------------------------------------------------------------------*/
;*    restore-cmap-transition! ...                                     */
;*---------------------------------------------------------------------*/
(define (restore-cmap-transition! cmap::JsConstructMap restore::pair-nil)
   (save-cmap-transition! cmap)
   (with-access::JsConstructMap cmap (transition nextmap)
      (set! transition (car restore))
      (set! nextmap (cdr restore))))

;*---------------------------------------------------------------------*/
;*    extend-cmap ...                                                  */
;*---------------------------------------------------------------------*/
(define (extend-cmap omap::JsConstructMap name)
   
   (define (vector-extend::vector vec::vector val)
      ;; extend a vector with one additional slot
      (let* ((len (vector-length vec))
	     (nvec (copy-vector vec (+fx 1 len))))
	 (vector-set! nvec len val)
	 nvec))

   (with-access::JsConstructMap omap (names methods ctor)
      (let ((newnames (vector-extend names name))
	    (newmethods (vector-extend methods #unspecified)))
	 (instantiate::JsConstructMap
	    (ctor ctor)
	    (names newnames)
	    (methods newmethods)))))

;*---------------------------------------------------------------------*/
;*    clone-cmap ...                                                   */
;*---------------------------------------------------------------------*/
(define (clone-cmap cmap::JsConstructMap)
   (with-access::JsConstructMap cmap (names methods)
      (let ((newnames (vector-copy names))
	    (newmethods (vector-copy methods)))
	 (duplicate::JsConstructMap cmap
	    (names newnames)
	    (methods newmethods)))))

;*---------------------------------------------------------------------*/
;*    js-names->cmap ...                                               */
;*    -------------------------------------------------------------    */
;*    Used by j2sscheme to create literal objects.                     */
;*---------------------------------------------------------------------*/
(define (js-names->cmap names)
   (instantiate::JsConstructMap
      (names names)
      (methods (make-vector (vector-length names) #unspecified))))

;*---------------------------------------------------------------------*/
;*    js-object-literal-init! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-object-literal-init! o::JsObject)
   (with-access::JsObject o ((%elements elements) cmap)
      (with-access::JsConstructMap cmap ((%methods methods))
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

;* {*---------------------------------------------------------------------*} */
;* {*    js-descriptors->cmap ...                                         *} */
;* {*    -------------------------------------------------------------    *} */
;* {*    Not used but will be for builtin object creation.                *} */
;* {*---------------------------------------------------------------------*} */
;* (define (js-descriptors->cmap::JsConstructMap descrs meths)         */
;*    (let* ((len (vector-length descrs))                              */
;* 	  (names ($create-vector len)))                                */
;*       (let loop ((i 0))                                             */
;* 	 (if (=fx i len)                                               */
;* 	     (instantiate::JsConstructMap                              */
;* 		(names names)                                          */
;* 		(descriptors descrs)                                   */
;* 		(methods meths))                                       */
;* 	     (let ((descr (vector-ref descrs i)))                      */
;* 		(with-access::JsIndexDescriptor descr (name index)     */
;* 		   (set! index i)                                      */
;* 		   (vector-set! names i name)                          */
;* 		   (loop (+fx i 1))))))))                              */

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
	  (with-access::JsConstructMap ,cmap (names)
	     (let ((props names))
		(let liip ((,i (-fx (vector-length props) 1)))
		   (cond
		      ((=fx ,i -1)
		       (,fail))
		      ((eq? (vector-ref props ,i) ,p)
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
      `(with-access::JsObject ,o ((,prop properties))
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
	 (if cmap
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
   (with-access::JsObject o (cmap elements properties)
      (when cmap
	 (with-access::JsConstructMap cmap (names)
	    (let loop ((i (-fx (vector-length names) 1))
		       (acc '()))
	       (cond
		  ((=fx i -1)
		   (set! properties acc))
		  ((not (vector-ref names i))
		   (loop (-fx i 1) acc))
		  ((isa? (vector-ref elements i) JsPropertyDescriptor)
		   (loop (-fx i 1)
		      (cons (vector-ref elements i) acc)))
		  (else
		   (let ((desc (instantiate::JsValueDescriptor
				  (enumerable #t)
				  (writable #t)
				  (configurable #t)
				  (name (vector-ref names i))
				  (value (vector-ref elements i)))))
		      (loop (-fx i 1) (cons desc acc)))))))
	 (set! cmap #f)
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
	 ((isa? p JsSymbol)
	  p)
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
	 (else
	  (loop (js-tostring p %this))))))

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
		 (with-access::JsValueDescriptor desc (writable value)
		    ;; 3
		    (js-put! obj 'value value #f %this)
		    (js-put! obj 'writable writable #f %this)))
		((isa? desc JsAccessorDescriptor)
		 ;; 4
		 (with-access::JsAccessorDescriptor desc (get set)
		    (js-put! obj 'get get #f %this)
		    (js-put! obj 'set set #f %this))))
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
      ((isa? desc JsValueDescriptor)
       (with-access::JsValueDescriptor desc (value)
	  value))
      ((isa? desc JsAccessorDescriptor)
       (with-access::JsAccessorDescriptor desc (%get)
	  (%get obj)))
      (else
       (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-property-value-value-set! ...                                 */
;*    -------------------------------------------------------------    */
;*    Set the value of a property.                                     */
;*---------------------------------------------------------------------*/
(define (js-property-value-set! obj desc v %this)
   (cond
      ((isa? desc JsValueDescriptor)
       (with-access::JsValueDescriptor desc (value)
	  (set! value v)
	  v))
      ((isa? desc JsAccessorDescriptor)
       (with-access::JsAccessorDescriptor desc (%set)
	  (%set obj v)))
      (else
       (js-undefined))))

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
;*    properties-names ...                                             */
;*---------------------------------------------------------------------*/
(define (properties-names o::JsObject enump::bool)
   
   (define (cmap->names cmap)
      (with-access::JsConstructMap cmap (names)
	 (with-access::JsObject o (elements)
	    (let loop ((i (-fx (vector-length names) 1))
		       (acc '()))
	       (cond
		  ((=fx i -1)
		   acc)
		  ((vector-ref names i)
		   =>
		   (lambda (name)
		      (let ((p (vector-ref elements i)))
			 (if (and enump (isa? p JsPropertyDescriptor))
			     (with-access::JsPropertyDescriptor p (enumerable)
				(if enumerable
				    (loop (-fx i 1) (cons name acc))
				    (loop (-fx i 1) acc)))
			     (loop (-fx i 1) (cons name acc))))))
		  (else
		   (loop (-fx i 1) acc)))))))
   
   (with-access::JsObject o (cmap properties)
      (cond
	 (cmap
	  (cmap->names cmap))
	 ((not enump)
	  (map (lambda (p)
		  (with-access::JsPropertyDescriptor p (name) name))
	     properties))
	 (else
	  (filter-map (lambda (p)
			 (with-access::JsPropertyDescriptor p (enumerable name)
			    (when enumerable name)))
	     properties)))))

;*---------------------------------------------------------------------*/
;*    js-properties-name ::JsObject ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-properties-name::vector o::JsObject enump::bool %this::JsGlobalObject)
   
   (define (symbol->jsstring n)
      (js-string->jsstring (symbol->string! n)))

   (with-access::JsObject o (cmap properties)
      (apply vector
	 (filter-map (lambda (n) (when (symbol? n) (symbol->jsstring n)))
	    (properties-names o enump)))))

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
   (with-access::JsObject o (cmap properties)
      (apply vector
	 (filter-map (lambda (n) (when (isa? n JsSymbol) n))
	    (properties-names o #t)))))

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
   (jsobject-find o (js-toname p %this)
      ;; cmap search
      (lambda (owner i)
	 (with-access::JsObject owner (cmap elements)
	    (if (isa? (vector-ref elements i) JsPropertyDescriptor)
		(vector-ref elements i)
		(with-access::JsConstructMap cmap (names)
		   (instantiate::JsValueDescriptor
		      (writable #t)
		      (enumerable #t)
		      (configurable #t)
		      (name (vector-ref names i))
		      (value (vector-ref elements i)))))))
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
;*    js-get ::object ...                                              */
;*    -------------------------------------------------------------    */
;*    Accessing Bigloo objects from hopscript                          */
;*---------------------------------------------------------------------*/
(define-method (js-get o::object prop %this)
   (let* ((name (js-toname prop %this))
	  (clazz (object-class o))
	  (field (find-class-field clazz name)))
      (if (not field)
	  (case name
	     ((inspect)
	      (js-make-function %this js-inspect 1 'inspect))
	     ((constructor)
	      (js-undefined))
	     ((toString)
	      (js-make-function %this
		 (lambda (this)
		    (js-tostring this %this))
		 0
		 'toString))
	     (else
	      (js-raise-type-error %this
		 (format "no such field \"~a\" ~~a" name) o)))
	  (let ((v ((class-field-accessor field) o)))
	     (js-obj->jsobject v %this)))))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject ::JsObject ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.3       */
;*    -------------------------------------------------------------    */
;*    This function seconds JS-GET. It is required because in strict   */
;*    mode, "this" is not converted in a object, which demands         */
;*    to keep the base object (the actual receiver) available.         */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject o::JsObject base p %this)
   (let ((pval (js-get-property-value o base p %this)))
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
	  (js-get o prop %this)))))

;*---------------------------------------------------------------------*/
;*    js-get-lookup ...                                                */
;*    -------------------------------------------------------------    */
;*    Look for the property, if found update the cache and return      */
;*    the property value.                                              */
;*---------------------------------------------------------------------*/
(define (js-get-lookup o::JsObject name::obj cache::JsPropertyCache throw %this)
   (add-cache-miss! 'get name)
   (let loop ((obj o))
      (jsobject-find obj name
	 ;; map search
	 (lambda (obj i)
	    (with-access::JsObject o ((omap cmap))
	       (with-access::JsObject obj (elements)
		  (with-access::JsPropertyCache cache (cmap pmap index owner)
		     (let ((el-or-desc (vector-ref elements i)))
			(cond
			   ((isa? el-or-desc JsPropertyDescriptor)
			    ;; accessor property
			    (js-pcache-update-descriptor! cache i o obj)
			    (js-property-value o el-or-desc %this))
			   ((eq? o obj)
			    ;; direct access to the direct object
			    [assert (i) (<=fx i (vector-length elements))]
			    (js-pcache-update-direct! cache i o)
			    el-or-desc)
			   (else
			    ;; direct access to a prototype object
			    (js-pcache-update-owner! cache i o obj)
			    el-or-desc)))))))
	 ;; property search
	 (lambda (obj v)
	    ;; cache the object access
	    (with-access::JsPropertyCache cache (cmap pmap index owner)
	       (with-access::JsObject o ((omap cmap))
		  (when (eq? name 'key) (tprint "key in prop" v))
		  (js-property-value o v %this))))
	 ;; not found
	 (lambda ()
	    (when (eq? name 'key) (tprint "key not found"))
	    (js-get-notfound name throw %this))
	 ;; loop
	 loop)))

;*---------------------------------------------------------------------*/
;*    js-get-length ::obj ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (js-get-length o::obj cache %this::JsGlobalObject)
   (if cache
       (js-get-name/cache o 'length cache %this)
       (js-get o 'length %this)))

;*---------------------------------------------------------------------*/
;*    js-get/cache ...                                                 */
;*    -------------------------------------------------------------    */
;*    Use a per site cache for the [[GET]] operation. The property     */
;*    name is not known statically.                                    */
;*---------------------------------------------------------------------*/
(define (js-get/cache o prop::obj cache::JsPropertyCache %this::JsGlobalObject)
   (if (or (not (js-jsstring? prop)) (not (isa? o JsObject)))
       (js-get o prop %this)
       (let ((propname (string->symbol (js-jsstring->string prop))))
	  (with-access::JsPropertyCache cache (name)
	     (if (eq? name propname)
		 (js-get-name/cache o name cache %this)
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
(define (js-get-name/cache
	   obj name::obj cache::JsPropertyCache %this::JsGlobalObject)
   (if (isa? obj JsObject)
       (js-object-get-name/cache obj name cache %this)
       (js-get obj name %this)))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache ...                                     */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-object-get-name/cache o::JsObject
	   name::obj cache::JsPropertyCache %this::JsGlobalObject)
   (js-object-get-name/cache o name cache %this))

(define (js-object-get-name/cache-level1 o::JsObject
	   name::obj cache::JsPropertyCache %this::JsGlobalObject)
   ;; rewritten by macro expansion
   (with-access::JsObject o ((omap cmap))
      (let ((%omap omap))
	 (js-object-get-name/cache-level1 o name cache %this))))

(define (js-object-get-name/cache-level2 o::JsObject
	   name::obj cache::JsPropertyCache %this::JsGlobalObject)
   ;; rewritten by macro expansion
   (with-access::JsObject o ((omap cmap))
      (let ((%omap omap))
	 (js-object-get-name/cache-level2 o name cache %this))))
 
;*---------------------------------------------------------------------*/
;*    js-global-object-get-name ...                                    */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*    -------------------------------------------------------------    */
;*    This is an inlined version of js-get-own-property.               */
;*---------------------------------------------------------------------*/
(define (js-global-object-get-name o::JsObject name throw %this)
   (let ((pval (js-get-property-value o o name %this)))
      (if (eq? pval (js-absent))
	  (js-get-notfound name throw %this)
	  pval)))

;*---------------------------------------------------------------------*/
;*    js-global-object-get-name/cache ...                              */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-global-object-get-name/cache o::JsObject name::symbol cache::JsPropertyCache throw %this)
   (with-access::JsObject o ((omap cmap) elements)
      (with-access::JsPropertyCache cache (cmap index)
         (if (eq? cmap omap)
	     (vector-ref elements index)
	     (js-get-name/cache-miss o name cache throw %this)))))

;* {*---------------------------------------------------------------------*} */
;* {*    js-this-object-get-name/cache ...                                *} */
;* {*---------------------------------------------------------------------*} */
;* (define-inline (js-this-object-get-name/cache o::JsObject name::symbol cache::JsPropertyCache throw %this) */
;*    (js-global-object-get-name/cache o name cache throw %this))      */
;*                                                                     */
;*---------------------------------------------------------------------*/
;*    js-get-name/cache-miss ...                                       */
;*    -------------------------------------------------------------    */
;*    Use a per site cache for the [[GET]] operation. The name is a    */
;*    static constant, so the actual value is not compared against     */
;*    the cache value.                                                 */
;*---------------------------------------------------------------------*/
(define-generic (js-get-name/cache-miss obj::JsObject name::obj cache::JsPropertyCache throw %this)
   (js-get-lookup obj name cache throw %this))

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
   (js-put-jsobject! o p value throw #f #f %this))

;*---------------------------------------------------------------------*/
;*    js-unresolved-eval-put! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-unresolved-eval-put! scope::JsObject p value throw::bool %this::JsGlobalObject)
   (if (eq? (js-get-own-property scope p %this) (js-undefined))
       (js-put-jsobject! %this p value throw (not throw) #f %this)
       (js-put! scope p value throw %this)))

;*---------------------------------------------------------------------*/
;*    js-decl-eval-put! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-decl-eval-put! scope::JsObject p value throw::bool %this::JsGlobalObject)
   (if (eq? (js-get-own-property scope p %this) (js-undefined))
       (js-put-jsobject! %this p value throw #t #f %this)
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
;*    js-put! ::object ...                                             */
;*    -------------------------------------------------------------    */
;*    Mutating Bigloo objects from hopscript                           */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::object prop v::obj throw::bool %this::JsGlobalObject)
   (let* ((name (js-toname prop %this))
	  (clazz (object-class o))
	  (field (find-class-field clazz name)))
      (cond
	 ((not field)
	  (js-raise-type-error %this (format "no such field \"~a\" ~~a" name) o))
	 ((not (class-field-mutable? field))
	  (js-raise-type-error %this (format "field \"~a\" read-only ~~a" name) o))
	 (else
	  ((class-field-mutator field) o v)))))

;*---------------------------------------------------------------------*/
;*    js-put-length! ::obj ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (js-put-length! o::obj v::obj throw::bool cache %this::JsGlobalObject)
   (if cache
       (js-put-name/cache! o 'length v throw cache %this)
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
   (js-put-jsobject! o p value throw #t #f %this))

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
(define (js-put-jsobject! o p v throw extend::bool pcache %this)

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
   
   (define (update-from-descriptor! obj index::long v desc)
      ;; 8.12.5
      (with-access::JsAccessorDescriptor desc (set)
	 (if (isa? set JsFunction)
	     ;; 8.12.5, step 5
	     (begin
		(when (and (>=fx index 0) pcache)
		   (js-pcache-update-descriptor! pcache index o obj))
		(js-call1 %this set o v)
		v)
	     ;; 8.12.4, setp 2.a
	     (reject "No setter defined"))))

   (define (update-mapped-object! obj i)
      (with-access::JsObject obj (cmap elements)
	 (with-access::JsConstructMap cmap (nextmap methods vtable vlen names)
	    (let ((el-or-desc (vector-ref elements i)))
	       (cond
		  ((isa? el-or-desc JsAccessorDescriptor)
		   ;; 8.12.5, step 5
		   (update-from-descriptor! obj i v el-or-desc))
		  ((eq? o obj)
		   (if (isa? el-or-desc JsPropertyDescriptor)
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
			     (else
			      (when (invalidate-cache-method! v methods i)
				 (set! vtable '#())
				 (set! vlen 0)
				 (set! cmap (duplicate::JsConstructMap cmap)))
			      (when pcache
				 [assert (i) (<=fx i (vector-length elements))]
				 (js-pcache-update-direct! pcache i o))
			      (vector-set! elements i v)
			      v)))
		       (begin
			  (when (invalidate-cache-method! v methods i)
			     (set! vtable '#())
			     (set! vlen 0)
			     (set! cmap (duplicate::JsConstructMap cmap)))
			  (when pcache
			     [assert (i) (<=fx i (vector-length elements))]
			     (js-pcache-update-direct! pcache i o))
			  (vector-set! elements i v)
			  v)))
		  ((not (js-object-mode-extensible? obj))
		   ;; 8.12.9, step 3
		   (reject "Sealed object"))
		  ((not (isa? el-or-desc JsPropertyDescriptor))
		   ;; 8.12.5, step 6
		   (extend-object!))
		  (else
		   (with-access::JsDataDescriptor el-or-desc (writable)
		      (if writable
			  ;; 8.12.5, step 6
			  (extend-object!)
			  ;; 8.12.4, step 8.b
			  (reject "Read-only property")))))))))

   (define (extend-mapped-object!)
      ;; 8.12.5, step 6
      (with-access::JsObject o (cmap elements)
	 (with-access::JsConstructMap cmap (nextmap names)
	    (let* ((name (js-toname p %this))
		   (flags (property-flags #t #t #t))
		   (index (vector-length names)))
	       (let loop ()
		  (cond
		     ((cmap-same-transition? cmap name v flags)
		      ;; follow the next map
		      (with-access::JsConstructMap nextmap (names ctor)
			 (when (and pcache ctor)
			    [assert (index) (<=fx index (vector-length elements))]
			    (js-pcache-next-direct! pcache o nextmap index))
			 [assert (o) (isa? nextmap JsConstructMap)]
			 (set! elements
			    (js-elements-push/ctor! ctor elements index v))
			 (set! cmap nextmap)
			 v))
		     ((find-old-transition cmap name flags)
		      =>
		      (lambda (restore)
			 (restore-cmap-transition! cmap restore)
			 (loop)))
		     (else
		      
		      ;; create a new map
		      (let ((nextmap (extend-cmap cmap name)))
			 (with-access::JsConstructMap nextmap (methods)
			    (validate-cache-method! v methods index))
			 (with-access::JsConstructMap cmap (ctor)
			    (when (and pcache ctor)
			       [assert (index) (<=fx index (vector-length elements))]
			       (js-pcache-next-direct! pcache o nextmap index))
			    (link-cmap! cmap nextmap name v flags)
			    [assert (o) (isa? nextmap JsConstructMap)]
			    (set! elements
			       (js-elements-push/ctor! ctor elements index v)))
			 (set! cmap nextmap)
			 v))))))))

   (define (update-properties-object! obj desc)
      (cond
	 ((isa? desc JsAccessorDescriptor)
	  ;; 8.12.5, step 5
	  (update-from-descriptor! o -1 v desc))
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
	  (reject "Sealed object"))
	 (else
	  (with-access::JsDataDescriptor desc (writable)
	     (if writable
		 ;; 8.12.5, step 6
		 (extend-object!)
		 ;; 8.12.4, step 8.b
		 (reject "Read-only property"))))))
   
   (define (extend-properties-object!)
      (with-access::JsObject o (properties)
	 (let ((newdesc (instantiate::JsValueDescriptor
			   (name name)
			   (value v)
			   (writable #t)
			   (enumerable #t)
			   (configurable #t))))
	    (js-define-own-property o name newdesc throw %this)
	    v)))
   
   (define (extend-object!)
      (with-access::JsObject o (cmap properties)
	 (cond
	    ((not (js-object-mode-extensible? o))
	     ;; 8.12.9. step 3
	     (reject "Sealed objet"))
	    ((not extend)
	     ;; 11.13.1
	     (js-raise-reference-error %this
		"[[PUT]], \"~a\" is not defined" p))
	    (cmap
	     ;; 8.12.5, step 6
	     (extend-mapped-object!))
	    (else
	     ;; 8.12.5, step 6
	     (extend-properties-object!)))))

   (assert (o p)
      (with-access::JsObject o (elements cmap)
	 (or (not cmap)
	     (with-access::JsConstructMap cmap (names)
		(>=fx (vector-length elements) (vector-length names))))))

   (add-cache-miss! 'put name)
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
(define (js-put/cache! o prop v::obj throw::bool cache::JsPropertyCache %this)
   (if (or (not (string? prop)) (not (isa? o JsObject)))
       (js-put! o prop v throw %this)
       (let ((pname (js-toname prop %this)))
	  (js-put! o pname v throw %this))))

;*---------------------------------------------------------------------*/
;*    js-put-name/cache ...                                            */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-put-name/cache! o prop::obj v::obj throw::bool pcache::JsPropertyCache %this)
   (if (isa? o JsObject)
       (js-object-put-name/cache! o prop v throw pcache %this)
       (js-put! o prop v throw %this)))

;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache! ...                                    */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache! o::JsObject prop::obj v::obj throw::bool pcache::JsPropertyCache %this)
   ;; rewritten by macro expansion
   (js-object-put-name/cache! o prop v throw pcache %this))

(define (js-object-put-name/cache-level1! o::JsObject prop::obj v::obj throw::bool pcache::JsPropertyCache %this)
   ;; rewritten by macro expansion
   (with-access::JsObject o ((omap cmap))
      (let ((%omap omap))
	 (js-object-put-name/cache-level1! o prop v throw pcache %this))))

(define (js-object-put-name/cache-level2! o::JsObject prop::obj v::obj throw::bool pcache::JsPropertyCache %this)
   ;; rewritten by macro expansion
   (with-access::JsObject o ((omap cmap))
      (let ((%omap omap))
	 (js-object-put-name/cache-level2! o prop v throw pcache %this))))

;* {*---------------------------------------------------------------------*} */
;* {*    js-this-put-name/cache! ...                                      *} */
;* {*---------------------------------------------------------------------*} */
;* (define (js-this-put-name/cache! o::JsObject prop::obj v::obj throw::bool cache::JsPropertyCache %this) */
;*    (js-object-put-name/cache! o prop v throw cache %this))          */
;*                                                                     */
;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache-miss! ...                               */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache-miss! o::JsObject prop::obj v::obj throw::bool cache::JsPropertyCache %this)
   (with-access::JsObject o ((omap cmap))
      (let* ((%omap omap)
	     (tmp (js-put-jsobject! o prop v throw #t cache %this)))
	 (with-access::JsPropertyCache cache (index cmap vindex)
	    (when (>=fx index 0)
	       (unless (<fx vindex (js-vindex-max))
		  (set! vindex (js-get-vindex %this)))
	       (js-vtable-add! %omap vindex (cons index cmap))))
	 tmp)))

;*---------------------------------------------------------------------*/
;*    js-bind! ...                                                     */
;*    -------------------------------------------------------------    */
;*    This is a simplified version of DefineOwnProperty used to build  */
;*    the library objects. This function always binds the value in the */
;*    mentionned object. It does not follow the prototype chain. It    */
;*    does not check the extensibility flags.                          */
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
	   (configurable #t))
   
   (define (validate-cache-method! v methods i)
      (when (isa? v JsFunction)
	 (vector-set! methods i #t)))
   
   (define (accessor-property? get set)
      (or (and get (not (eq? get (js-undefined))))
	  (and set (not (eq? set (js-undefined))))))
   
   (define (assert-accessor-property! get set)
      (cond
	 ((not (or (isa? get JsFunction) (eq? get (js-undefined))))
	  (js-raise-type-error %this
	     (format "wrong getter for property \"~a\", ~~a" name)
	     get))
	 ((and set (not (eq? set (js-undefined))) (not (isa? set JsFunction)))
	  (js-raise-type-error %this
	     (format "wrong setter for property \"~a\", ~~a" name) set))))
   
   (define (plain-data-property? flags)
      (=fx flags (property-flags #t #t #t)))
   
   (define (update-mapped-object! obj i)
      (cond
	 ((not (eq? obj o))
	  (error "js-bind!"
	     (format "cannot rebind mapped prototype property \"~a\"" name)
	     (typeof o)))
	 ((or get set)
	  (with-access::JsObject o (elements)
	     (vector-set! elements i
		(instantiate::JsAccessorDescriptor
		   (name name)
		   (get get)
		   (set set)
		   (%get (function0->proc get %this))
		   (%set (function1->proc set %this))
		   (enumerable enumerable)
		   (configurable configurable)))))
	 ((or (not writable) (not enumerable) (not configurable))
	  (with-access::JsObject o (elements)
	     (vector-set! elements i
		(instantiate::JsValueDescriptor
		   (name name)
		   (value value)
		   (writable writable)
		   (enumerable enumerable)
		   (configurable configurable)))
	     value))
	 (else
	  (with-access::JsObject o (elements)
	     (vector-set! elements i value)
	     value))))
   
   (define (extend-mapped-object!)
      ;; 8.12.5, step 6
      (with-access::JsObject o (cmap elements)
	 (with-access::JsConstructMap cmap (nextmap names)
	    (let ((flags (property-flags writable enumerable configurable))
		  (index (vector-length names)))
	       (cond
		  ((cmap-same-transition? cmap name value flags)
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
		      (with-access::JsConstructMap nextmap (names)
			 (set! cmap nextmap)
			 (set! elements
			    (js-elements-push! elements index val-or-desc))
			 value)))
		  ((find-old-transition cmap name flags)
		   =>
		   (lambda (restore)
		      (restore-cmap-transition! cmap restore)
		      (extend-mapped-object!)))
		  ((accessor-property? get set)
		   ;; create a new map with a JsAccessorDescriptor
		   (let* ((newdesc (instantiate::JsAccessorDescriptor
				      (name name)
				      (get get)
				      (set set)
				      (%get (function0->proc get %this))
				      (%set (function1->proc set %this))
				      (enumerable enumerable)
				      (configurable configurable)))
			  (nextmap (extend-cmap cmap name)))
		      (assert-accessor-property! get set)
		      (link-cmap! cmap nextmap name #f flags)
		      [assert (o) (isa? nextmap JsConstructMap)]
		      (set! cmap nextmap)
		      ;; extending the elements vector is mandatory
		      (set! elements (js-elements-push! elements index newdesc))
		      (js-undefined)))
		  ((plain-data-property? flags)
		   (let ((nextmap (extend-cmap cmap name)))
		      (with-access::JsConstructMap nextmap (methods)
			 (validate-cache-method! value methods index))
		      (link-cmap! cmap nextmap name value flags)
		      [assert (o) (isa? nextmap JsConstructMap)]
		      (set! cmap nextmap)
		      ;; store in the obj
		      (set! elements (js-elements-push! elements index value))
		      value))
		  (else
		   ;; create a new map with a JsIndexDescriptor
		   (let* ((newdesc (instantiate::JsValueDescriptor
				      (name name)
				      (value value)
				      (writable writable)
				      (enumerable enumerable)
				      (configurable configurable)))
			  (nextmap (extend-cmap cmap name)))
		      (with-access::JsConstructMap nextmap (methods)
			 (validate-cache-method! value methods index))
		      (link-cmap! cmap nextmap name value flags)
		      [assert (o) (isa? nextmap JsConstructMap)]
		      (set! cmap nextmap)
		      (set! elements (js-elements-push! elements index newdesc))
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
	  (with-access::JsObject obj (properties)
	     (let ((props (filter! (lambda (d)
				      (with-access::JsPropertyDescriptor d ((n name))
					 (not (eq? n name))))
			     properties)))
		(set! properties
		   (cons (instantiate::JsValueDescriptor
			    (name name)
			    (value value)
			    (writable writable)
			    (enumerable enumerable)
			    (configurable configurable))
		      props)))
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
      (if cmap
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
      (with-access::JsObject obj (elements)
	 (let ((el (vector-ref elements i)))
	    (if (isa? el JsPropertyDescriptor)
		(with-access::JsPropertyDescriptor el (configurable)
		   configurable)
		#t))))
   
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
	     (if cmap
		 (jsobject-map-find o n
		    (lambda (o i)
		       (delete-configurable o
			  (configurable-mapped-property? o i)
			  (lambda (o)
			     (js-invalidate-pcaches-pmap!)
			     ;; create a new cmap for the object
			     (let ((nextmap (clone-cmap cmap)))
				(link-cmap! cmap nextmap n #f -1)
				[assert (o) (isa? nextmap JsConstructMap)]
				(set! cmap nextmap)
				(with-access::JsConstructMap nextmap (names)
				   ;; remove the prop from the cmap
				   (vector-set! names i #f)
				   #t)))))
		    (lambda () #t))
		 (jsobject-properties-find o n
		    (lambda (o d)
		       (with-access::JsPropertyDescriptor d (configurable)
			  (delete-configurable o configurable
			     (lambda (o)
				(with-access::JsObject o (properties)
				   (set! properties (remq! d properties))
				   #t)))))
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
      (with-access::JsValueDescriptor current (value writable)
	 (with-access::JsDataDescriptor desc ((dwritable writable))
	    (when (boolean? dwritable)
	       (set! writable dwritable)))
	 (when (isa? desc JsValueDescriptor)
	    (with-access::JsValueDescriptor desc ((dvalue value))
	       (set! value dvalue))))
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
      ;; 1
      (with-access::JsObject o (properties)
	 ;; 2
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
		    (set! properties (append! properties (list ndesc)))
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
		    (propagate-property-descriptor! current desc)))))))))

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
      (with-access::JsObject o (properties __proto__)
	 (unless (replace-list! properties old new)
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
;*---------------------------------------------------------------------*/
(define-generic (js-for-in obj proc %this)
   (if (or (eq? obj (js-undefined)) (eq? obj (js-null)))
       (js-undefined)
       (js-for-in (js-cast-object obj %this "for") proc %this)))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsObject ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-for-in obj::JsObject proc %this)
   
   (define env '())
   
   (define (vfor-each proc vec vecname)
      (let ((len (vector-length vecname)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (proc (vector-ref vec i) (vector-ref vecname i))
	       (loop (+fx i 1))))))
   
   (define (in-mapped-property el-or-descr name)
      (when name
	 (cond
	    ((isa? el-or-descr JsPropertyDescriptor)
	     (with-access::JsPropertyDescriptor el-or-descr (enumerable)
		(unless (memq name env)
		   (when (eq? enumerable #t)
		      (set! env (cons name env))
		      (proc (js-string->jsstring (symbol->string! name)))))))
	    ((not (memq name env))
	     (set! env (cons name env))
	     (proc (js-string->jsstring (symbol->string! name)))))))
   
   (define (in-property p)
      (when (isa? p JsPropertyDescriptor)
	 (with-access::JsPropertyDescriptor p (name enumerable)
	    (unless (memq name env)
	       (when (eq? enumerable #t)
		  (set! env (cons name env))
		  (proc (js-string->jsstring (symbol->string! name))))))))
   
   (let loop ((o obj))
      (with-access::JsObject o (cmap properties __proto__ elements)
	 (if cmap
	     (with-access::JsConstructMap cmap (names)
		(vfor-each in-mapped-property elements names))
	     (for-each in-property properties))
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
;*    js-call/cache ...                                                */
;*---------------------------------------------------------------------*/
(define (js-call/cache %this obj cache this . args)
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
	   ccache::JsPropertyCache ocache::JsPropertyCache . args)
   (if (isa? obj JsObject)
       (apply js-object-method-call-name/cache %this
	  obj name ccache ocache args)
       (js-raise-type-error %this "call: not a function ~s" obj)))

;*---------------------------------------------------------------------*/
;*    js-object-method-call-name/cache ...                             */
;*    -------------------------------------------------------------    */
;*    !!! Overriden by a macro in property.sch                         */
;*---------------------------------------------------------------------*/
(define (js-object-method-call-name/cache %this::JsGlobalObject obj::JsObject
	   name::obj ccache::JsPropertyCache ocache::JsPropertyCache . args)
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
	     (with-access::JsConstructMap omap (vtable)
		(if (and (>fx (vector-length vtable) vindex)
			 (procedure? (vector-ref vtable vindex)))
		    (apply (vector-ref vtable vindex) obj args)
		    (js-object-method-call/cache-miss %this obj name
		       ccache ocache args))))))))

(define (js-object-method-call0-name/cache-level2 %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache)
   (with-access::JsObject obj ((omap cmap))
      (let ((%omap omap))
	 (js-object-method-call-name/cache-level2 %this obj name ccache ocache))))

(define (js-object-method-call1-name/cache-level2 %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   a0)
   (with-access::JsObject obj ((omap cmap))
      (let ((%omap omap))
	 (js-object-method-call-name/cache-level2 %this obj name ccache ocache
	    a0))))

(define (js-object-method-call2-name/cache-level2 %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   a0 a1)
   (with-access::JsObject obj ((omap cmap))
      (let ((%omap omap))
	 (js-object-method-call-name/cache-level2 %this obj name ccache ocache
	    a0 a1))))

(define (js-object-method-call3-name/cache-level2 %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   a0 a1 a2)
   (with-access::JsObject obj ((omap cmap))
      (let ((%omap omap))
	 (js-object-method-call-name/cache-level2 %this obj name ccache ocache
	    a0 a1 a2))))

(define (js-object-method-call4-name/cache-level2 %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   a0 a1 a2 a3)
   (with-access::JsObject obj ((omap cmap))
      (let ((%omap omap))
	 (js-object-method-call-name/cache-level2 %this obj name ccache ocache
	    a0 a1 a2 a3))))

(define (js-object-method-call5-name/cache-level2 %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   a0 a1 a2 a3 a4)
   (with-access::JsObject obj ((omap cmap))
      (let ((%omap omap))
	 (js-object-method-call-name/cache-level2 %this obj name ccache ocache
	    a0 a1 a2 a3 a4))))

(define (js-object-method-call6-name/cache-level2 %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   a0 a1 a2 a3 a4 a5)
   (with-access::JsObject obj ((omap cmap))
      (let ((%omap omap))
	 (js-object-method-call-name/cache-level2 %this obj name ccache ocache
	    a0 a1 a2 a3 a4 a5))))

(define (js-object-method-call7-name/cache-level2 %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   a0 a1 a2 a3 a4 a5 a6)
   (with-access::JsObject obj ((omap cmap))
      (let ((%omap omap))
	 (js-object-method-call-name/cache-level2 %this obj name ccache ocache
	    a0 a1 a2 a3 a4 a5 a6))))

(define (js-object-method-call8-name/cache-level2 %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   a0 a1 a2 a3 a4 a5 a6 a7)
   (with-access::JsObject obj ((omap cmap))
      (let ((%omap omap))
	 (js-object-method-call-name/cache-level2 %this obj name ccache ocache
	    a0 a1 a2 a3 a4 a5 a6 a7))))

(define (js-object-method-call9-name/cache-level2 %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   a0 a1 a2 a3 a4 a5 a6 a7 a8)
   (with-access::JsObject obj ((omap cmap))
      (let ((%omap omap))
	 (js-object-method-call-name/cache-level2 %this obj name ccache ocache
	    a0 a1 a2 a3 a4 a5 a6 a7 a8))))

(define (js-object-method-calln-name/cache-level2 %this::JsGlobalObject
	   obj::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache
	   . args)
   (with-access::JsObject obj ((omap cmap))
      (let ((%omap omap))
	 (with-access::JsPropertyCache ccache (cmap pmap method vindex)
	    (cond
	       ((not %omap)
		;; uncachable
		(let ((f (js-get obj name %this)))
		   (js-apply %this f obj args)))
	       (else
		;; cache miss
		(with-access::JsConstructMap %omap (vtable)
		   (if (and (>fx (vector-length vtable) vindex)
			    (procedure? (vector-ref vtable vindex)))
		       (apply (vector-ref vtable vindex) obj args)
		       (js-object-method-call/cache-miss %this obj name
			  ccache ocache args)))))))))

;*---------------------------------------------------------------------*/
;*    js-object-method-call/cache-miss ...                             */
;*    -------------------------------------------------------------    */
;*    This function is called on a true cache miss, i.e., this call    */
;*    has already been filled with another method.                     */
;*---------------------------------------------------------------------*/
(define (js-object-method-call/cache-miss %this::JsGlobalObject
	   o::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache args::pair-nil)
   (with-access::JsPropertyCache ccache (pmap vtable vindex method)
      (when (and (procedure? method) (isa? pmap JsConstructMap))
	 (when (=fx vindex (js-vindex-max))
	    (set! vindex (js-get-vindex %this)))
	 (js-vtable-add! pmap vindex method))
      (js-object-method-call/cache-fill %this o name ccache ocache args)))

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
	   o::JsObject name::obj
	   ccache::JsPropertyCache ocache::JsPropertyCache args::pair-nil)
   
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
      (if (=fx (procedure-arity procedure) 1)
	  (case largs
	     ((0) (lambda (this) (procedure this)))
	     ((1) (lambda (this a0) (procedure this)))
	     ((2) (lambda (this a0 a1) (procedure this)))
	     ((3) (lambda (this a0 a1 a2) (procedure this)))
	     ((4) (lambda (this a0 a1 a2 a3) (procedure this)))
	     ((5) (lambda (this a0 a1 a2 a3 a4) (procedure this)))
	     ((6) (lambda (this a0 a1 a2 a3 a4 a5) (procedure this)))
	     ((7) (lambda (this a0 a1 a2 a3 a4 a5 a6) (procedure this)))
	     (else (lambda (this a0 a1 a2 a3 a4 a5 a6 . _) (procedure this))))
	  #f))

   (add-cache-miss! 'call name)

   (let loop ((obj o))
      (jsobject-find obj name
	 ;; map search
	 (lambda (obj i)
	    (with-access::JsObject o ((omap cmap) __proto__)
	       (with-access::JsObject obj ((wmap cmap) elements)
		  (with-access::JsConstructMap wmap (methods)
		     (let ((el-or-desc (vector-ref elements i)))
			(cond
			   ((isa? el-or-desc JsAccessorDescriptor)
			    (with-access::JsPropertyCache ccache (pmap cmap)
			       (set! pmap #t)
			       (set! cmap #t))
			    (jsapply (js-property-value o el-or-desc %this)))
			   ((eq? (vector-ref methods i) #t)
			    (let ((f (funval el-or-desc)))
			       (cond
				  ((procedure? f)
				   (with-access::JsPropertyCache ccache (pmap cmap index method)
				      ;; correct arity, put in cache
				      (set! pmap omap)
				      (set! cmap #t)
				      (set! index i)
				      (set! method (method->procedure f))))
				  ((isa? f JsFunction)
				   (with-access::JsFunction f (len procedure)
				      (cond
					 ((=fx len (length args))
					  (with-access::JsPropertyCache ccache (pmap cmap index method)
					     ;; correct arity, put in cache
					     (set! pmap omap)
					     (set! cmap #t)
					     (set! index i)
					     (set! method procedure)))
					 ((procedureN procedure (length args))
					  =>
					  (lambda (procedure)
					     (with-access::JsPropertyCache ccache (pmap cmap index method)
						;; correct arity, put in cache
						(set! pmap omap)
						(set! cmap #t)
						(set! index i)
						(set! method procedure))))
					 (else
					  ;; arity missmatch, never cache
					  (with-access::JsPropertyCache ccache (cmap)
					     (set! cmap #t)))))))
			       (jsapply f)))
			   (else
			    (with-access::JsPropertyCache ccache (pmap cmap)
			       ;; invalidate the call cache and update the
			       ;; object cache
			       (set! cmap #t)
			       (set! pmap #t)
			       (jsapply (funval el-or-desc))))))))))
	 ;; property search
	 (lambda (obj v)
	    (with-access::JsPropertyCache ccache (cmap pmap)
	       (set! pmap #t)
	       (set! cmap #t)
	       (jsapply (js-property-value o v %this))))
	 ;; not found
	 (lambda ()
	    (js-raise-type-error %this "call: not a function ~s"
	       (js-undefined)))
	 ;; loop
	 loop)))

;*---------------------------------------------------------------------*/
;*    js-vindex-max ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-vindex-max::long)
   (bit-lsh 1 28))

;*---------------------------------------------------------------------*/
;*    js-get-vindex ...                                                */
;*---------------------------------------------------------------------*/
(define (js-get-vindex %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-vindex)
      (let ((i js-vindex))
	 (set! js-vindex (+fx 1 js-vindex))
	 i)))

;*---------------------------------------------------------------------*/
;*    js-vtable-add! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-vtable-add! o::JsConstructMap idx::long obj)
   (with-access::JsConstructMap o (vtable vlen)
      (when (<=fx (vector-length vtable) idx)
	 (set! vtable (copy-vector vtable (+fx idx 1))))
      (vector-set! vtable idx obj)
      (when (>=fx idx vlen) (set! vlen (+fx 1 idx)))
      obj))

;*---------------------------------------------------------------------*/
;*    *misses* ...                                                     */
;*---------------------------------------------------------------------*/
(define *misses* '())
(define *log-misses* #f)
(define *log-miss-threshold* 100)

;*---------------------------------------------------------------------*/
;*    log-cache-miss! ...                                              */
;*---------------------------------------------------------------------*/
(define (log-cache-miss!)
   (set! *log-misses* #t))

;*---------------------------------------------------------------------*/
;*    add-cache-miss! ...                                              */
;*---------------------------------------------------------------------*/
(define (add-cache-miss! what name)
   (when *log-misses*
      (let ((ow (assq what *misses*)))
	 (if (not ow)
	     (set! *misses*
		(cons (cons what (cons 1 (list (cons name 1)))) *misses*))
	     (let ((on (assq name (cddr ow))))
		(set-car! (cdr ow) (+fx 1 (cadr ow)))
		(if (not on)
		    (set-cdr! (cdr ow) (cons (cons name 1) (cddr ow)))
		    (set-cdr! on (+fx 1 (cdr on)))))))))

;*---------------------------------------------------------------------*/
;*    show-cache-misses ...                                            */
;*---------------------------------------------------------------------*/
(define (show-cache-misses)
   (let ((m (pregexp-match "hopscript:cache=([0-9]+)" (getenv "HOPTRACE"))))
      (when m
	 (set! *log-miss-threshold* (string->integer (cadr m)))))
   (for-each (lambda (what)
		(fprint (current-error-port) (car what) ": " (cadr what))
		(for-each (lambda (e)
			     (when (>=fx (cdr e) *log-miss-threshold*)
				(fprint (current-error-port) "   "
				   (car e) ": " (cdr e))))
		   (sort (lambda (e1 e2)
			    (cond
			       ((>fx (cdr e1) (cdr e2)) #t)
			       ((<fx (cdr e1) (cdr e2)) #f)
			       (else
				(string<=? (symbol->string! (car e1))
				   (symbol->string! (car e2))))))
		      (cddr what)))
		(newline (current-error-port)))
      *misses*)
   (print "total cache misses: " (apply + (map cadr *misses*))))
