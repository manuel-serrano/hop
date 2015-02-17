;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/property.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 25 07:05:26 2013                          */
;*    Last change :  Sat Feb 14 06:57:25 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript property handling (getting, setting, defining and     */
;*    deleting).                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_property

   (library hop)

   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_worker
	   __hopscript_pair
	   __hopscript_function)

   (export (js-object-unmap! ::JsObject)
	   (js-toname::symbol ::obj ::JsGlobalObject)
	   
	   (inline js-is-accessor-descriptor?::bool obj)
	   (inline js-is-data-descriptor?::bool obj)
	   (inline js-is-generic-descriptor?::bool obj)
	   (js-from-property-descriptor ::JsGlobalObject desc ::obj)
	   (js-to-property-descriptor ::JsGlobalObject desc ::symbol)
	   (js-property-value ::obj ::JsObject
	      ::JsPropertyDescriptor ::JsGlobalObject)
	   
	   (generic js-properties-name::vector ::obj ::bool ::JsGlobalObject)
	   (generic js-has-property::bool ::obj ::symbol ::JsGlobalObject)
	   (generic js-get-own-property ::obj ::obj ::JsGlobalObject)
	   (generic js-get-property-value ::obj ::obj ::obj ::JsGlobalObject)

	   (js-get-property ::JsObject ::obj ::JsGlobalObject)

	   (js-get-notfound ::obj ::obj ::JsGlobalObject)
	   (generic js-get ::obj ::obj ::JsGlobalObject)
	   (js-get/debug ::obj ::obj ::JsGlobalObject loc)
	   (js-get/cache ::obj ::obj ::JsPropertyCache ::JsGlobalObject)
	   (js-get-name/cache ::obj ::symbol ::JsPropertyCache ::JsGlobalObject)
	   (js-get-name/cache-miss ::JsObject ::symbol ::JsPropertyCache ::obj ::JsGlobalObject)
	   
	   (js-can-put o::JsObject ::symbol ::JsGlobalObject)
	   (js-unresolved-put! ::JsObject ::obj ::obj ::bool ::JsGlobalObject)
	   (js-unresolved-eval-put! ::JsObject ::obj ::obj ::bool ::JsGlobalObject)
	   (js-decl-eval-put! ::JsObject ::obj ::obj ::bool ::JsGlobalObject)

	   (generic js-put! ::obj ::obj ::obj ::bool ::JsGlobalObject)
	   (js-put/debug! ::obj ::obj ::obj ::bool ::JsGlobalObject loc)
	   (js-put/cache! ::obj ::obj ::obj ::bool ::JsPropertyCache ::JsGlobalObject)
	   (inline js-put-name/cache! ::obj ::symbol ::obj ::bool ::JsPropertyCache ::JsGlobalObject)
	   (js-put-name/cache-miss! ::JsObject ::symbol ::obj ::bool ::JsPropertyCache ::JsGlobalObject)

	   (generic js-delete! ::obj ::obj ::bool ::JsGlobalObject)
	   
	   (generic js-replace-own-property! ::JsObject
	      old::JsPropertyDescriptor
	      new::JsPropertyDescriptor)
	   
	   (js-define-own-property%::bool o::JsObject
	      name::symbol desc::JsPropertyDescriptor throw::bool
	      ::JsGlobalObject)
	   
	   (generic js-define-own-property::bool o::JsObject p
	      desc::JsPropertyDescriptor throw::bool
	      ::JsGlobalObject)
	   
	   (generic js-seal ::JsObject ::obj)
	   (generic js-freeze ::JsObject ::obj)

	   (generic js-for-in ::obj proc::procedure ::JsGlobalObject)

	   (js-bind! ::JsGlobalObject ::JsObject name::symbol #!key
	      (value #f)
	      (get #f)
	      (set #f)
	      (writable #t)
	      (enumerable #t)
	      (configurable #t))))

(define-macro (vector-ref-ur v i)
   `(vector-ref ,v ,i))

(define-macro (vector-set-ur! v i o)
   `(vector-set! ,v ,i ,o))

;*---------------------------------------------------------------------*/
;*    property-flags ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (property-flags writable enumerable configurable get)
   `(bit-or (if ,writable 1 0)
       (bit-or (if ,enumerable 2 0)
	  (bit-or (if ,configurable 4 0)
	     (if ,get 8 0)))))

;*---------------------------------------------------------------------*/
;*    cmap-transition ...                                              */
;*---------------------------------------------------------------------*/
(define (cmap-transition name flags)
   (cons name flags))

;*---------------------------------------------------------------------*/
;*    cmap-descriptors ...                                             */
;*---------------------------------------------------------------------*/
(define (cmap-descriptors omap)
   (when omap
      (with-access::JsConstructMap omap (descriptors)
	 descriptors)))

;*---------------------------------------------------------------------*/
;*    link-cmap! ...                                                   */
;*---------------------------------------------------------------------*/
(define (link-cmap! omap::JsConstructMap nmap::JsConstructMap t)
   (with-access::JsConstructMap omap (transition nextmap)
      (set! transition t)
      (set! nextmap nmap)
      nmap))

;*---------------------------------------------------------------------*/
;*    extend-cmap ...                                                  */
;*---------------------------------------------------------------------*/
(define (extend-cmap omap::JsConstructMap name descr::JsPropertyDescriptor)
   (with-access::JsConstructMap omap (names descriptors)
      (let ((newnames (vector-extend names name))
	    (newdescriptors (vector-extend descriptors descr)))
	 (instantiate::JsConstructMap
	    (names newnames)
	    (descriptors newdescriptors)))))

;*---------------------------------------------------------------------*/
;*    clone-cmap ...                                                   */
;*---------------------------------------------------------------------*/
(define (clone-cmap cmap::JsConstructMap)
   (with-access::JsConstructMap cmap (names descriptors)
      (let ((newnames (vector-copy names))
	    (newdescriptors (vector-copy descriptors)))
	 (duplicate::JsConstructMap cmap
	    (names newnames)
	    (descriptors newdescriptors)))))

;*---------------------------------------------------------------------*/
;*    cmap-same-transition? ...                                        */
;*---------------------------------------------------------------------*/
(define (cmap-same-transition? cmap::JsConstructMap name::symbol flags::int)
   (with-access::JsConstructMap cmap ((t1 transition))
      (and (eq? (car t1) name) (=fx (cdr t1) flags))))

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
       (vector-ref-ur property-index-vector (+fx indx 10))
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
		      ((eq? (vector-ref-ur props ,i) ,p)
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
   (let ((obj (gensym 'o)))
      `(let ((,obj ,o))
	  (with-access::JsObject ,obj (cmap __proto__)
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
			    `(,notfound)))))))))

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

   (define (mapped-descriptor->descriptor odesc)
      (cond
	 ((isa? odesc JsAccessorDescriptor)
	  odesc)
	 ((isa? odesc JsIndexDescriptor)
	  (with-access::JsIndexDescriptor odesc (name configurable enumerable writable index)
	     (with-access::JsObject o (elements)
		(instantiate::JsValueDescriptor
		   (name name)
		   (enumerable enumerable)
		   (configurable configurable)
		   (writable writable)
		   (value (vector-ref-ur elements index))))))
	 ((isa? odesc JsValueDescriptor)
	  (error "js-object-unmap!" "Unexpected JsValueDescriptor" odesc))
	 (else
	  (duplicate::JsDataDescriptor odesc))))
   
   (with-access::JsObject o (cmap elements properties)
      (when cmap
	 (with-access::JsConstructMap cmap (names descriptors)
	    (set! properties
	       (let loop ((i (-fx (vector-length names) 1))
			  (acc '()))
		  (if (=fx i -1)
		      acc
		      (let ((odesc (vector-ref-ur descriptors i)))
			 (if (eq? odesc (js-undefined))
			     (loop (-fx i 1) acc)
			     (let ((desc (mapped-descriptor->descriptor odesc)))
				(loop (-fx i 1) (cons desc acc)))))))))
	 (set! cmap #f)
	 (set! elements '#())))
   o)

;*---------------------------------------------------------------------*/
;*    js-toname ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-toname p %this)
   (let loop ((p p))
      (cond
	 ((string? p)
	  (string->symbol p))
	 ((js-jsstring? p)
	  (string->symbol (js-jsstring->string p)))
	 ((symbol? p)
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
		((isa? desc JsIndexDescriptor)
		 (with-access::JsIndexDescriptor desc (writable index)
		    (with-access::JsObject owner (elements)
		       ;; 3
		       (js-put! obj 'value (vector-ref-ur elements index) #f %this)
		       (js-put! obj 'writable writable #f %this))))
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
(define (js-to-property-descriptor %this::JsGlobalObject obj name::symbol)
   (let* ((obj (js-cast-object %this obj "[[ToPropertyDescriptor]]"))
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
;*    Get the value of a property. Owner, is the object owning the     */
;*    property. Obj, is the object used to access the property.        */
;*    Owner != Obj if the property is defined in the prototype chain.  */
;*    Obj is the "this" value of AccessorDescriptors.                  */
;*---------------------------------------------------------------------*/
(define (js-property-value obj owner desc %this)
   (cond
      ((isa? desc JsIndexDescriptor)
       (with-access::JsIndexDescriptor desc (index)
	  (with-access::JsObject owner (elements)
	     (vector-ref-ur elements index))))
      ((isa? desc JsValueDescriptor)
       (with-access::JsValueDescriptor desc (value name)
	  value))
      ((isa? desc JsAccessorDescriptor)
       (with-access::JsAccessorDescriptor desc (get)
	  (if (isa? get JsFunction)
	      (js-call0 %this get obj)
	      (js-undefined))))
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
;*    js-properties-name ::JsObject ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-properties-name::vector o::JsObject enump::bool %this::JsGlobalObject)
   (with-access::JsObject o (cmap properties)
      (apply vector
	 (filter-map (lambda (p)
			(unless (eq? p (js-undefined))
			   ;; not a descriptor if deleted property
			   ;; see cmap case in js-delete!
			   (with-access::JsPropertyDescriptor p (name enumerable)
			      (when (or (not enump) enumerable)
				 (js-string->jsstring
				    (symbol->string! name))))))
	    (if cmap
		(with-access::JsConstructMap cmap (descriptors)
		   (vector->list descriptors))
		properties)))))

;*---------------------------------------------------------------------*/
;*    js-has-property ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.6       */
;*---------------------------------------------------------------------*/
(define-generic (js-has-property::bool o name::symbol %this)
   #f)

;*---------------------------------------------------------------------*/
;*    js-has-property ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.6       */
;*---------------------------------------------------------------------*/
(define-method (js-has-property::bool o::JsObject name::symbol %this)
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
	 (with-access::JsObject owner (cmap)
	    (with-access::JsConstructMap cmap (descriptors)
	       (vector-ref-ur descriptors i))))
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
	     (js-property-value base owner desc %this)))))

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
;*    js-get ::JsStringLiteral ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsStringLiteral prop %this)
   (let ((obj (js-toobject %this o)))
      (js-get-jsobject obj o prop %this)))

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
		    (js-object-tostring this %this))
		 0
		 'toString))
	     (else
;* 	      (when (isa? o &exception)                                */
;* 		 (exception-notify o))                                 */
	      (js-raise-type-error %this
		 (format "no such field \"~a\" ~~a" name) o)))
	  (let ((v ((class-field-accessor field) o)))
	     (if (string? v)
		 (js-string->jsstring v)
		 v)))))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject ::JsObject ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.3       */
;*    -------------------------------------------------------------    */
;*    This function seconds JS-GET. It is required because in strict   */
;*    mode, "this" is not converted in a object, which demands         */
;*    to keep the base object (the actual receiver) avaiable.          */
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
	  (js-get _o prop %this)))))

;*---------------------------------------------------------------------*/
;*    js-get/cache ...                                                 */
;*    -------------------------------------------------------------    */
;*    Use a per site cache for the [[GET]] operation. The property     */
;*    name is not know statically.                                     */
;*---------------------------------------------------------------------*/
(define (js-get/cache o prop::obj cache::JsPropertyCache %this::JsGlobalObject)
   (if (or (not (symbol? prop)) (not (isa? o JsObject)))
       (js-get o prop %this)
       (with-access::JsPropertyCache cache (name)
	  (if (eq? name prop)
	      (js-get-name/cache o name cache %this)
	      (js-get-lookup o name cache #f %this)))))

;*---------------------------------------------------------------------*/
;*    js-get-lookup ...                                                */
;*    -------------------------------------------------------------    */
;*    Look for the property, if found update the cache and return      */
;*    the property value.                                              */
;*    -------------------------------------------------------------    */
;*    As this is only used when the property is a name (i.e., a        */
;*    symbol), it is never used to access indexed object. Hence,       */
;*    it does not need to be generic function.                         */
;*---------------------------------------------------------------------*/
(define (js-get-lookup obj::JsObject name::symbol cache::JsPropertyCache throw %this)
   (let loop ((owner obj))
      (jsobject-find owner name
	 ;; map search
	 (lambda (owner i)
	    (with-access::JsObject owner ((omap cmap) elements)
	       (with-access::JsConstructMap omap (descriptors)
		  (with-access::JsPropertyCache cache (cmap index)
		     (let ((descr (vector-ref-ur descriptors i)))
			(if (isa? descr JsAccessorDescriptor)
			    (begin
			       (set! cmap descriptors)
			       (set! index i)
			       (js-property-value obj owner descr %this))
			    (begin
			       (set! cmap omap)
			       (set! index i)
			       (vector-ref-ur elements i))))))))
	 ;; property search
	 (lambda (owner v)
	    (js-property-value obj owner v %this))
	 ;; not found
	 (lambda ()
	    (js-get-notfound name throw %this))
	 ;; loop
	 loop)))

;*---------------------------------------------------------------------*/
;*    js-get-name/cache ...                                            */
;*    -------------------------------------------------------------    */
;*    Use a per site cache for the [[GET]] operation. The name is a    */
;*    static constant, so the actual value is not compared against     */
;*    the cache value.                                                 */
;*---------------------------------------------------------------------*/
(define (js-get-name/cache obj
		  name::symbol cache::JsPropertyCache %this::JsGlobalObject)
   (if (not (isa? obj JsObject))
       (js-get obj name %this)
       (with-access::JsObject obj ((omap cmap) elements __proto__)
	  (with-access::JsPropertyCache cache (cmap index)
	     (if (eq? cmap omap)
		 (vector-ref-ur elements index)
		 (js-get-name/cache-miss obj name cache #f %this))))))

;*---------------------------------------------------------------------*/
;*    js-get-name/cache-miss ...                                       */
;*    -------------------------------------------------------------    */
;*    Use a per site cache for the [[GET]] operation. The name is a    */
;*    static constant, so the actual value is not compared against     */
;*    the cache value.                                                 */
;*---------------------------------------------------------------------*/
(define (js-get-name/cache-miss obj::JsObject name::symbol cache::JsPropertyCache throw %this)
   (let loop ((owner obj))
      (with-access::JsObject owner ((omap cmap) elements __proto__)
	 (with-access::JsPropertyCache cache (cmap index)
	    (cond
	       ((not omap)
		(jsobject-properties-find owner name
		   (lambda (owner d)
		      (js-property-value obj owner d %this))
		   (lambda ()
		      ;; not found
		      (if (isa? __proto__ JsObject)
			  (loop __proto__)
			  (js-get-lookup obj name cache throw %this)))))
	       ((eq? cmap omap)
		;; we got the map
		(vector-ref-ur elements index))
	       ((eq? cmap (cmap-descriptors omap))
		;; check if we are accessing a prop via a cached accessor
		(js-property-value obj owner (vector-ref-ur cmap index) %this))
	       (else
		(js-get-lookup obj name cache throw %this)))))))

;*---------------------------------------------------------------------*/
;*    js-can-put ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.4       */
;*---------------------------------------------------------------------*/
(define (js-can-put o::JsObject p::symbol %this::JsGlobalObject)

   (define (js-get-inherited-property o::JsObject name::symbol)
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
	  (with-access::JsObject o ((proto __proto__)
				    extensible)
	     ;; 3
	     (if (eq? proto (js-null))
		 ;; 4
		 extensible
		 ;; 5
		 (let ((inherited (js-get-inherited-property proto p)))
		    (cond
		       ((eq? inherited #f)
			;; 6
			extensible)
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
			   (when extensible
			      writable)))))))))))

;*---------------------------------------------------------------------*/
;*    js-unresolved-put! ...                                           */
;*---------------------------------------------------------------------*/
(define (js-unresolved-put! o::JsObject p value throw::bool %this::JsGlobalObject)
   (js-put-jsobject! o p value throw #f %this))


;*---------------------------------------------------------------------*/
;*    js-unresolved-eval-put! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-unresolved-eval-put! scope::JsObject p value throw::bool %this::JsGlobalObject)
   (if (eq? (js-get-own-property scope p %this) (js-undefined))
       (js-put-jsobject! %this p value throw (not throw) %this)
       (js-put! scope p value throw %this)))

;*---------------------------------------------------------------------*/
;*    js-decl-eval-put! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-decl-eval-put! scope::JsObject p value throw::bool %this::JsGlobalObject)
   (if (eq? (js-get-own-property scope p %this) (js-undefined))
       (js-put-jsobject! %this p value throw #t %this)
       (js-put! scope p value throw %this)))

;*---------------------------------------------------------------------*/
;*    js-put! ...                                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.5       */
;*---------------------------------------------------------------------*/
(define-generic (js-put! _o prop v::obj throw::bool %this::JsGlobalObject)
   (if (pair? _o)
       (js-put-pair! _o prop v throw %this)
       (let ((o (js-toobject %this _o)))
	  (if o
	      (js-put! o prop v throw %this)
	      (js-raise-type-error %this "[[PUT]]: not an object ~s" _o)))))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsStringLiteral ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-put! _o::JsStringLiteral prop v throw %this)
   (let ((o (js-toobject %this _o)))
      (js-put! o prop v throw %this)))

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
   (js-put-jsobject! o p value throw #t %this))

;*---------------------------------------------------------------------*/
;*    js-put-jsobject! ...                                             */
;*    -------------------------------------------------------------    */
;*    This function does not need to be generic because PUT only       */
;*    stores values on the direct object. It might use the setter      */
;*    of one of the object's prototypes but once again, the object     */
;*    to be used in the setter, is the direct object itself.           */
;*                                                                     */
;*    At the first level, special put! form for Array, String, etc.    */
;*    are overriden by method of the js-put! function.                 */
;*---------------------------------------------------------------------*/
(define (js-put-jsobject! o p value throw extend::bool %this)
   
   (define (reject msg)
      (if throw
	  (js-raise-type-error %this
	     (format "[[PUT]], ~a ~~s" msg) (js-toname p %this))
	  value))

   (define (update-accessor-descriptor! obj value desc)
      ;; 8.12.5
      (with-access::JsAccessorDescriptor desc (set)
	 (if (isa? set JsFunction)
	     ;; 8.12.5, step 5
	     (begin (js-call1 %this set obj value) value)
	     ;; 8.12.4, setp 2.a
	     (reject "No setter defined"))))

   (define (update-mapped-object! obj i)
      (with-access::JsObject obj (cmap elements extensible)
	 (with-access::JsConstructMap cmap (descriptors transition nextmap)
	    (let ((desc (vector-ref-ur descriptors i)))
	       (cond
		  ((isa? desc JsAccessorDescriptor)
		   ;; 8.12.5, step 5
		   ;;(update-accessor-descriptor! obj value desc)
		   (update-accessor-descriptor! o value desc))
		  ((eq? o obj)
		   ;; 8.12.5, step 3
		   (let ((owndesc desc))
		      (with-access::JsDataDescriptor owndesc (writable)
			 
			 (if (not writable)
			     ;; 8.12.4, step 2.b
			     (reject "Read-only property")
			     ;; 8.12.5, step 3,b
			     (begin
				(vector-set-ur! elements i value)
				value)))))
		  ((not extensible)
		   ;; 8.12.9, step 3
		   (reject "Sealed object"))
		  (else
		   (with-access::JsDataDescriptor desc (writable)
		      (if writable
			  ;; 8.12.5, step 6
			  (extend-object!)
			  ;; 8.12.4, step 8.b
			  (reject "Read-only property")))))))))

   (define (extend-mapped-object!)
      ;; 8.12.5, step 6
      (with-access::JsObject o (elements cmap)
	 (with-access::JsConstructMap cmap (nextmap names)
	    (let* ((name (js-toname p %this))
		   (flags (property-flags #t #t #t #f))
		   (index (vector-length names)))
	       (if (cmap-same-transition? cmap name flags)
		   ;; follow the next map 
		   (with-access::JsConstructMap nextmap (names)
		      (set! cmap nextmap))
		   ;; create a new map
		   (let* ((newdesc (instantiate::JsIndexDescriptor
				      (name name)
				      (index index)
				      (writable #t)
				      (enumerable #t)
				      (configurable #t)))
			  (nextmap (extend-cmap cmap name newdesc))
			  (trans (cmap-transition name flags)))
		      (link-cmap! cmap nextmap trans)
		      (set! cmap nextmap)))
	       ;; store in the obj
	       (if (>=fx index (vector-length elements))
		   (set! elements (vector-extend elements value))
		   (vector-set-ur! elements index value))
	       (if (not (>=fx (vector-length elements) (vector-length names)))
		   (error "js-put!" "assert not maintained" p))
	       value))))
   
   (define (update-properties-object! obj desc)
      (with-access::JsObject obj (extensible)
	 (cond
	    ((isa? desc JsAccessorDescriptor)
	     ;; 8.12.5, step 5
	     ;;(update-accessor-descriptor! obj value desc)
	     (update-accessor-descriptor! o value desc))
	    ((eq? o obj)
	     ;; 8.12.5, step 3
	     (let ((owndesc desc))
		(with-access::JsValueDescriptor owndesc (writable (v value))
		   (if (not writable)
		       ;; 8.12.4, step 2.b
		       (reject "Read-only property")
		       ;; 8.12.5, step 3,b
		       (begin
			  (set! v value)
			  value)))))
	    ((not extensible)
	     ;; 8.12.9, step 3
	     (reject "Sealed object"))
	    (else
	     (with-access::JsDataDescriptor desc (writable)
		(if writable
		    ;; 8.12.5, step 6
		    (extend-object!)
		    ;; 8.12.4, step 8.b
		    (reject "Read-only property")))))))
   
   (define (extend-properties-object!)
      (with-access::JsObject o (properties)
	 (let* ((name (js-toname p %this))
		(newdesc (instantiate::JsValueDescriptor
			    (name name)
			    (value value)
			    (writable #t)
			    (enumerable #t)
			    (configurable #t))))
	    (js-define-own-property o name newdesc throw %this)
	    value)))
   
   (define (extend-object!)
      (with-access::JsObject o (cmap properties extensible)
	 (cond
	    ((not extensible)
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

   (let loop ((obj o))
      (jsobject-find obj (js-toname p %this)
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
;*    js-bind! ...                                                     */
;*    -------------------------------------------------------------    */
;*    This is a simplified version of DefineOwnProperty used to build  */
;*    the library objects. This function always bind the value in the  */
;*    mentionned object. It does not follow the prototype chain. It    */
;*    does not check the extensible flags.                             */
;*    -------------------------------------------------------------    */
;*    [[Put]]                                                          */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.4    */
;*    [[CanPut]]                                                       */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.5    */
;*    [[DefineOwnProperty]]                                            */
;*       http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.9    */
;*---------------------------------------------------------------------*/
(define (js-bind! %this::JsGlobalObject o::JsObject name::symbol
	   #!key
	   (value #f)
	   (get #f)
	   (set #f)
	   (writable #t)
	   (enumerable #t)
	   (configurable #t))
   
   (define (update-mapped-object! obj i)
      (with-access::JsObject obj (cmap)
	 (with-access::JsConstructMap cmap (descriptors transition nextmap)
	    (let ((owndesc (vector-ref-ur descriptors i)))
	       (cond
		  ((or (isa? owndesc JsAccessorDescriptor) get)
		   (let ((newdesc (instantiate::JsAccessorDescriptor
				     (name name)
				     (get get)
				     (set set)
				     (enumerable enumerable)
				     (configurable configurable))))
		      (vector-set-ur! descriptors i newdesc)
		      (js-undefined)))
		  (else
		   ;; rebind the descriptor
		   (js-put! o name value #f %this)))))))

   (define (extend-mapped-object!)
      ;; 8.12.5, step 6
      (with-access::JsObject o (elements cmap)
	 (with-access::JsConstructMap cmap (nextmap names descriptors)
	    (let ((flags (property-flags writable enumerable configurable get))
		  (index (vector-length names)))
	       (cond
		  ((cmap-same-transition? cmap name flags)
		   ;; follow the next map 
		   (with-access::JsConstructMap nextmap (names)
		      (set! cmap nextmap)
		      (if (>=fx index (vector-length elements))
			  (set! elements (vector-extend elements value))
			  (vector-set-ur! elements index value))
		      value))
		  ((or (and get (not (eq? get (js-undefined))))
		       (and set (not (eq? set (js-undefined)))))
		   ;; create a new map with a JsAccessorDescriptor
		   (let* ((newdesc (instantiate::JsAccessorDescriptor
				      (name name)
				      (get get)
				      (set set)
				      (enumerable enumerable)
				      (configurable configurable)))
			  (nextmap (extend-cmap cmap name newdesc))
			  (trans (cmap-transition name flags)))
		      (cond
			 ((not (or (isa? get JsFunction) (eq? get (js-undefined))))
			  (js-raise-type-error %this
			     (format "wrong getter for property \"~a\", ~~a"
				name)
			     get))
			 ((and set
			       (not (eq? set (js-undefined)))
			       (not (isa? set JsFunction)))
			  (js-raise-type-error %this
			     (format "wrong setter for property \"~a\", ~~a"
				name)
			     set)))
		      (link-cmap! cmap nextmap trans)
		      (set! cmap nextmap)
		      ;; extending the elements vector is mandatory
		      (when (>=fx index (vector-length elements))
			 (set! elements (vector-extend elements value)))
		      (js-undefined)))
		  (else
		   ;; create a new map with a JsIndexDescriptor
		   (let* ((newdesc (instantiate::JsIndexDescriptor
				      (name name)
				      (index index)
				      (writable writable)
				      (enumerable enumerable)
				      (configurable configurable)))
			  (nextmap (extend-cmap cmap name newdesc))
			  (trans (cmap-transition name flags)))
		      (link-cmap! cmap nextmap trans)
		      (set! cmap nextmap)
		      ;; store in the obj
		      (if (>=fx index (vector-length elements))
			  (set! elements (vector-extend elements value))
			  (vector-set-ur! elements index value))
		      value)))))))
   
   (define (update-properties-object! obj owndesc)
      (if (or (isa? owndesc JsAccessorDescriptor) get)
	  (let ((newdesc (instantiate::JsAccessorDescriptor
			    (name name)
			    (get get)
			    (set set)
			    (enumerable enumerable)
			    (configurable configurable))))
	     (js-define-own-property o name newdesc #f %this)
	     (js-undefined))
	  (js-put! o name value #f %this)))
   
   (define (extend-properties-object!)
      (with-access::JsObject o (properties)
	 (if get
	     (let ((newdesc (instantiate::JsAccessorDescriptor
			       (name name)
			       (get get)
			       (set set)
			       (enumerable enumerable)
			       (configurable configurable))))
		(js-define-own-property o name newdesc #f %this)
		(js-undefined))
	     (let ((newdesc (instantiate::JsValueDescriptor
			       (name name)
			       (value value)
			       (writable writable)
			       (enumerable enumerable)
			       (configurable configurable))))
		(js-define-own-property o name newdesc #f %this)
		value))))

   (with-access::JsObject o (cmap)
      (if cmap
          (jsobject-map-find o name
             update-mapped-object!
	     extend-mapped-object!)
          (jsobject-properties-find o name
	     update-properties-object!
	     extend-properties-object!))))

;*---------------------------------------------------------------------*/
;*    vector-extend ...                                                */
;*---------------------------------------------------------------------*/
(define (vector-extend::vector vec::vector val)
   ;; extend a vector with one additional slot
   (let* ((len (vector-length vec))
	  (nvec ($create-vector (+fx 1 len))))
      (vector-copy! nvec 0 vec 0)
      (vector-set-ur! nvec len val)
      nvec))

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
;*---------------------------------------------------------------------*/
(define-inline (js-put-name/cache! o prop::symbol v::obj throw::bool cache::JsPropertyCache %this)
   (if (not (isa? o JsObject))
       (js-put! o prop v throw %this)
       (js-put-name/cache-miss! o prop v throw cache %this)))

;*---------------------------------------------------------------------*/
;*    js-put-name/cache-miss! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-put-name/cache-miss! o::JsObject prop::symbol v::obj throw::bool cache::JsPropertyCache %this)
   (js-put! o prop v throw %this))

;*---------------------------------------------------------------------*/
;*    js-delete! ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.7       */
;*---------------------------------------------------------------------*/
(define-generic (js-delete! _o p throw %this)
   
   (define (delete-configurable o d proc)
      (with-access::JsPropertyDescriptor d (configurable)
	 (cond
	    (configurable
	     (proc o d)
	     #t)
	    (throw 
	     (js-raise-type-error %this
		(string-append "delete: cannot delete ~s."
		   (symbol->string! (js-toname p %this)))
		o))
	    (else
	     #f))))
   
   (let ((n (js-toname p %this))
	 (o (js-toobject %this _o)))
      (cond
	 ((isa? o JsObject)
	  (with-access::JsObject o (cmap)
	     (if cmap
		 (jsobject-map-find o n
		    (lambda (o i)
		       (with-access::JsConstructMap cmap (descriptors)
			  (let ((d (vector-ref-ur descriptors i)))
			     (delete-configurable o d
				(lambda (o d)
				   ;; create a new cmap for the object
				   (let* ((trans (cmap-transition n -1))
					  (nextmap (clone-cmap cmap)))
				      (link-cmap! cmap nextmap trans)
				      (set! cmap nextmap)
				      (with-access::JsConstructMap nextmap
					    (names descriptors)
					 ;; remove the prop from the cmap
					 (vector-set-ur! names i #f)
					 (vector-set-ur! descriptors i
					    (js-undefined))
					 #t)))))))
		    (lambda () #t))
		 (jsobject-properties-find o n
		    (lambda (o d)
		       (delete-configurable o d
			  (lambda (o d)
			     (with-access::JsObject o (properties)
				(set! properties (remq! d properties))))))
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
(define (js-define-own-property%::bool o::JsObject name::symbol desc::JsPropertyDescriptor throw %this::JsGlobalObject)

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
      (with-access::JsAccessorDescriptor current (get set)
	 (with-access::JsAccessorDescriptor desc ((dget get) (dset set))
	    (when dget (set! get dget))
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
      (with-access::JsObject o (extensible properties)
	 ;; 2
	 (cond
	    ((eq? current (js-undefined))
	     (cond
		((not extensible)
		 ;; 3
		 (reject (format "~a not extensible" (js-tostring o %this))))
		((eq? extensible #t)
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
;*    js-for-in ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.4       */
;*---------------------------------------------------------------------*/
(define-generic (js-for-in obj proc %this)
   (if (or (eq? obj (js-undefined)) (eq? obj (js-null)))
       (js-undefined)
       (js-for-in (js-cast-object %this obj "for") proc %this)))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsObject ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-for-in obj::JsObject proc %this)
   
   (define env '())
   
   (define (vfor-each proc vec)
      (let ((len (vector-length vec)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (proc (vector-ref-ur vec i))
	       (loop (+fx i 1))))))
   
   (define (in-property p)
      (when (isa? p JsPropertyDescriptor)
	 (with-access::JsPropertyDescriptor p (name enumerable)
	    (unless (memq name env)
	       (when (eq? enumerable #t)
		  (set! env (cons name env))
		  (proc (js-string->jsstring (symbol->string! name))))))))
   
   (let loop ((o obj))
      (with-access::JsObject o (cmap properties __proto__)
	 (if cmap
	     (with-access::JsConstructMap cmap (descriptors)
		(vfor-each in-property descriptors))
	     (for-each in-property properties))
	 (when (isa? __proto__ JsObject)
	    (loop __proto__)))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::Object ...                                           */
;*---------------------------------------------------------------------*/
(define-method (js-for-in obj::object proc %this)
   (let ((fields (class-all-fields (object-class obj))))
      (let loop ((i 0))
	 (when (<fx i (vector-length fields))
	    (proc
	       (js-string->jsstring
		  (symbol->string (class-field-name (vector-ref-ur fields i)))))
	    (loop (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    js-for-in ::Object ...                                           */
;*---------------------------------------------------------------------*/
(define-method (js-for-in obj::object proc %this)
   (js-for-in (js-toobject %this obj) proc %this))

