;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arguments.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 14 09:14:55 2013                          */
;*    Last change :  Sat Jun 10 07:09:43 2023 (serrano)                */
;*    Copyright   :  2013-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript arguments objects            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-10.6         */
;*---------------------------------------------------------------------*/
(module __hopscript_arguments

   (library hop)

   (include "types.sch" "stringliteral.sch" "property.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_worker
	   __hopscript_array)

   (export (js-init-arguments! ::JsGlobalObject)
	   (js-arguments-define-own-property ::JsArguments ::int ::JsPropertyDescriptor)
	   (js-sloppy-arguments ::JsGlobalObject ::vector)
	   (js-strict-arguments ::JsGlobalObject ::vector)
	   (js-arguments->list ::JsArguments ::JsGlobalObject)
	   (js-arguments->vector ::JsArguments ::JsGlobalObject)
	   (js-arguments->jsarray ::JsArguments ::JsGlobalObject)
	   (js-arguments-ref ::JsArguments ::obj ::JsGlobalObject)
	   (js-arguments-index-ref ::JsArguments ::uint32 ::JsGlobalObject)
	   (js-arguments-set! ::JsArguments ::obj ::JsGlobalObject ::obj)
	   (js-arguments-index-set! ::JsArguments ::uint32 ::obj ::JsGlobalObject)
	   (js-arguments-length::obj ::JsArguments ::JsGlobalObject)
	   (js-arguments-slice ::JsArguments start end ::JsGlobalObject)
	   (js-arguments-slice1 ::JsArguments start ::JsGlobalObject)
	   (js-arguments-vector-slice ::vector start end ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsArguments ...                              */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsArguments
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-arguments->vector o ctx)
	  (error "obj->string ::JsArguments" "Not a JavaScript context" ctx)))
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-vector->jsarray o ctx)
	  (error "string->obj ::JsArguments" "Not a JavaScript context" ctx))))

;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsArguments ...                                     */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack obj::JsArguments ctx)
   (if (isa? ctx JsGlobalObject)
       (with-access::JsArguments obj (vec)
	  (map! (lambda (v)
		   (unless (eq? v (js-absent))
		      (if (isa? v JsPropertyDescriptor)
			  (with-access::JsPropertyDescriptor v (name)
			     (js-property-value obj obj name v ctx))
			  v)))
	     (vector->list vec)))
       (error "xml-unpack ::JsArguments" "Not a JavaScript context" ctx)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsArguments ...                                */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsArguments op compile isexpr ctx)
   (js-with-context ctx "hop->javascript"
      (lambda (%this)
	 (let ((len::uint32 (js-touint32 (js-get o (& "length") %this) %this)))
	    (if (=u32 len (fixnum->uint32 0))
		(display "sc_vector2array([])" op)
		(begin
		   (display "sc_vector2array([" op)
		   (hop->javascript
		      (js-get o (js-toname 0 %this) %this) op compile isexpr ctx)
		   (let loop ((i (fixnum->uint32 1)))
		      (if (=u32 i len)
			  (display "])" op)
			  (let ((n (js-integer-name->jsstring (uint32->fixnum i))))
			     (display "," op)
			     (when (js-has-property o n %this)
				(hop->javascript (js-get o n %this)
				   op compile isexpr ctx))
			     (loop (+u32 i (fixnum->uint32 1))))))))))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsArguments ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-donate o::JsArguments worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (js-raise-type-error %this "[[DonationTypeError]] ~a" o)))

;*---------------------------------------------------------------------*/
;*    js-debug-object ::JsArguments ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-debug-object obj::JsArguments #!optional (msg ""))
   (call-next-method)
   (with-access::JsArguments obj (vec)
      (fprint (current-error-port)
	 "   vec=" vec)))

;*---------------------------------------------------------------------*/
;*    properties                                                       */
;*---------------------------------------------------------------------*/
(define strict-caller-property #f)
(define strict-callee-property #f)

;*---------------------------------------------------------------------*/
;*    js-init-arguments! ...                                           */
;*---------------------------------------------------------------------*/
(define (js-init-arguments! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-arguments-cmap js-strict-arguments-cmap)
      ;; local constant strings
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      ;; properties
      (let ((throwget (lambda (o)
			 (js-raise-type-error %this
			    "[[ThrowTypeError]] ~a" o)))
	    (throwset (lambda (o v)
			 (js-raise-type-error %this
			    "[[ThrowTypeError]] ~a" o))))
	 (set! strict-caller-property
	    (instantiate::JsAccessorDescriptor
	       (name (& "caller"))
	       (get thrower-get)
	       (set thrower-set)
	       (%get throwget)
	       (%set throwset)
	       (enumerable #f)
	       (configurable #f)))
	 (set! strict-callee-property
	    (instantiate::JsAccessorDescriptor
	       (name (& "callee"))
	       (get thrower-get)
	       (set thrower-set)
	       (%get throwget)
	       (%set throwset)
	       (enumerable #f)
	       (configurable #f))))
      ;; arguments cmap
      (let ((arguments-cmap-props
	       `#(,(prop (& "length") (property-flags #t #f #t #f #f))
		  ,(prop (& "callee") (property-flags #t #f #t #f #f)))))
	 (set! js-arguments-cmap
	    (js-make-jsconstructmap
	       :methods (make-vector (vector-length arguments-cmap-props))
	       :props arguments-cmap-props)))
      ;; strict arguments cmap
      (let ((strict-arguments-cmap-props
	       `#(,(prop (& "length") (property-flags #t #f #t #f #f))
		  ,(prop (& "callee") (property-flags #t #f #f #t #f))
		  ,(prop (& "caller") (property-flags #t #f #f #t #f)))))
	 (set! js-strict-arguments-cmap
	    (js-make-jsconstructmap
	       :methods (make-vector (vector-length strict-arguments-cmap-props))
	       :props strict-arguments-cmap-props)))))

;*---------------------------------------------------------------------*/
;*    jsarguments-fields ...                                           */
;*---------------------------------------------------------------------*/
(define jsarguments-fields (vector (find-class-field JsObject 'vec)))

;*---------------------------------------------------------------------*/
;*    javascript-class-all-fields ::JsArguments ...                    */
;*---------------------------------------------------------------------*/
(define-method (javascript-class-all-fields obj::JsArguments)
   jsarguments-fields)
   
;*---------------------------------------------------------------------*/
;*    js-arguments-define-own-property ...                             */
;*---------------------------------------------------------------------*/
(define (js-arguments-define-own-property arguments indx prop)
   (with-access::JsArguments arguments (vec)
      (js-object-mode-inline-set! arguments #f)
      (vector-set! vec indx prop)))

;*---------------------------------------------------------------------*/
;*    js-arguments-length ...                                          */
;*---------------------------------------------------------------------*/
(define (js-arguments-length arr::JsArguments %this)
   (if (js-object-mode-inline? arr)
       (with-access::JsArguments arr (vec)
	  (vector-length vec))
       (js-get arr (& "length") %this)))

;*---------------------------------------------------------------------*/
;*    js-get-length ::JsArguments ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-get-length arr::JsArguments %this #!optional cache)
   (js-arguments-length arr %this))

;*---------------------------------------------------------------------*/
;*    js-arguments-ref ...                                             */
;*---------------------------------------------------------------------*/
(define (js-arguments-ref arr::JsArguments idx %this)
   (with-access::JsArguments arr (vec)
      (if (and (js-object-mode-inline? arr)
	       (and (fixnum? idx) (>=fx idx 0) (<fx idx (vector-length vec))))
	  (let ((v (vector-ref vec idx)))
	     (if (and (object? v) (eq? (object-class v) JsValueDescriptor))
		 (with-access::JsValueDescriptor v (value)
		    value)
		 v))
	  (js-get arr idx %this))))

;*---------------------------------------------------------------------*/
;*    js-arguments-index-ref ...                                       */
;*---------------------------------------------------------------------*/
(define (js-arguments-index-ref arr::JsArguments idx::uint32 %this)
   (with-access::JsArguments arr (vec)
      (if (and (js-object-mode-inline? arr)
	       (<u32 idx (fixnum->uint32 (vector-length vec))))
	  (let ((v (u32vref vec idx)))
	     (if (and (object? v) (eq? (object-class v) JsValueDescriptor))
		 (with-access::JsValueDescriptor v (value)
		    value)
		 v))
	  (js-get arr (js-uint32-tointeger idx) %this))))

;*---------------------------------------------------------------------*/
;*    js-arguments-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-arguments-set! arr::JsArguments idx val %this)
   (js-put! arr idx val #f %this))

;*---------------------------------------------------------------------*/
;*    js-arguments-index-set! ...                                      */
;*---------------------------------------------------------------------*/
(define (js-arguments-index-set! arr::JsArguments idx val %this)
   (js-put! arr (js-uint32-tointeger idx) val #f %this))

;*---------------------------------------------------------------------*/
;*    js-put-length! ...                                               */
;*---------------------------------------------------------------------*/
(define-method (js-put-length! o::JsArguments v::obj throw::bool cache %this)
   (js-put! o (& "length") v throw %this))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsArguments ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-10.6         */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsArguments p v throw %this)
   (with-access::JsArguments o (vec)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (when (eq? (js-toname p %this) (& "length"))
		(js-object-mode-inline-set! o #f))
	     (call-next-method))
	    ((<uint32 i (vector-length vec))
	     (let ((desc (u32vref vec i)))
		(cond
		   ((eq? desc (js-absent))
		    (call-next-method))
		   ((isa? desc JsAccessorDescriptor)
		    (with-access::JsAccessorDescriptor desc (set)
		       (js-call1 %this set o v)))
		   ((isa? desc JsValueDescriptor)
		    (with-access::JsValueDescriptor desc (value writable)
		       (if writable
			   (begin
			      (set! value v)
			      v)
			   (js-undefined))))
		   (else
		    (vector-set! vec (uint32->fixnum i) v)
		    v))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-delete! ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-10.6         */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsArguments p throw %this)
   (with-access::JsArguments o (vec properties)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((<uint32 i (vector-length vec))
	     (let ((desc (u32vref vec i)))
		(if (isa? desc JsPropertyDescriptor)
		    (with-access::JsPropertyDescriptor desc (configurable)
		       (if configurable
			   (begin
			      (js-object-mode-inline-set! o #f)
			      (u32vset! vec i (js-absent))
			      #t)
			   #f))
		    (begin
		       (js-object-mode-inline-set! o #f)
		       (u32vset! vec i (js-absent))
		       #t))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-replace-own-property! ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-replace-own-property! o::JsArguments old new)
   (with-access::JsPropertyDescriptor old ((p name))
      (with-access::JsArguments o (vec)
	 (let ((i::uint32 (js-toindex p)))
	    (cond
	       ((not (js-isindex? i))
		(call-next-method))
	       ((and (<uint32 i (vector-length vec))
		     (not (eq? (u32vref vec i) (js-absent))))
		(js-object-mode-inline-set! o #f)
		(u32vset! vec i new))
	       (else
		(call-next-method)))))))

;*---------------------------------------------------------------------*/
;*    get-mapped-property ...                                          */
;*---------------------------------------------------------------------*/
(define (get-mapped-property o::JsArguments p)
   (with-access::JsArguments o (vec)
      (let ((i::uint32 (js-toindex p)))
	 (when (and (js-isindex? i) (<uint32 i (vector-length vec)))
	    (let ((v (u32vref vec i)))
	       (unless (eq? v (js-absent))
		  (if (isa? v JsPropertyDescriptor)
		      v
		      (let ((nv (instantiate::JsValueDescriptor
				   (name (js-index-name (uint32->fixnum i)))
				   (value v)
				   (writable #t)
				   (configurable #t)
				   (enumerable #t))))
			 ;; lazy creation of arguments property descriptor
			 (u32vset! vec i nv)
			 nv))))))))

;*---------------------------------------------------------------------*/
;*    function1->proc ...                                              */
;*---------------------------------------------------------------------*/
(define (function1->proc fun %this::JsGlobalObject)
   (if (js-procedure? fun)
       (with-access::JsProcedure fun (procedure)
	  (if (correct-arity? procedure 2)
	      (lambda (this v owner pname %this)
		 (procedure this v))
	      (lambda (this v owner pname %this)
		 (js-call1 %this fun this v))))
       (lambda (obj v owner pname %this)
	  (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-define-own-property ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-10.6         */
;*---------------------------------------------------------------------*/
(define-method (js-define-own-property::bool o::JsArguments p desc throw %this)
   ;; step 2
   (let* ((ismapped (get-mapped-property o p))
	  (allowed (js-define-own-property% o (js-toname p %this) desc #f %this)))
      ;; 3
      (cond
	 ((not allowed)
	  ;; 4
	  (js-raise-type-error %this "Illegal arguments property override ~s"
	     (with-access::JsPropertyDescriptor desc (name) name)))
	 (ismapped
	  ;; 5
	  (if (isa? desc JsAccessorDescriptor)
	      ;; 5.a (don't need to delete as define-own-property%
	      ;; has replace the property) but we need to propagate
	      ;; the set attribute of the new desc
	      (with-access::JsAccessorDescriptor desc ((dset set))
		 (when (isa? ismapped JsAccessorDescriptor)
		    (with-access::JsAccessorDescriptor ismapped (set %set)
		       (if (js-procedure? dset)
			   (begin
			      (set! set dset)
			      (set! %set (function1->proc dset %this)))
			   (set! set (js-undefined))))))
	      (begin
		 ;; 5.b
		 (when (isa? desc JsValueDescriptor)
		    ;; 5.b.i
		    (with-access::JsValueDescriptor desc (value)
		       (cond
			  ((isa? ismapped JsAccessorDescriptor)
			   (with-access::JsAccessorDescriptor ismapped (set)
			      (when (js-procedure? set)
				 (js-call1 %this set o value))))
			  ((isa? ismapped JsValueDescriptor)
			   (with-access::JsValueDescriptor ismapped
				 ((avalue value) writable)
			      (when writable
				 (set! avalue value)))))))
		 (when (isa? desc JsDataDescriptor)
		    (with-access::JsDataDescriptor desc (writable)
		       (when (eq? writable #f)
			  ;; 5.b.ii (don't need to delete as
			  ;; define-own-property%
			  ;; has replace the property)
			  #unspecified))))))
	 ((eq? p (& "length"))
	  (js-object-mode-inline-set! o #f)
	  (call-next-method))
	 (else
	  #t))))

;*---------------------------------------------------------------------*/
;*    js-properties-names ::JsArray ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-properties-names obj::JsArguments enump %this)
   (with-access::JsArguments obj (vec)
      (append!
	 (map! js-integer->jsstring (iota (vector-length vec)))
	 (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-ownkeys ::JsArguments ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-ownkeys obj::JsArguments %this)
   (js-vector->jsarray (js-properties-name obj #t %this) %this))

;*---------------------------------------------------------------------*/
;*    js-has-property ::JsArguments ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.5.2     */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsArguments p %this)
   (let ((index::uint32 (js-toindex p)))
      (if (js-isindex? index)
	  (with-access::JsArguments o (vec)
	     (let ((len (vector-length vec)))
		(cond
		   ((<=u32 (fixnum->uint32 len) index)
		    (call-next-method))
		   ((eq? (vector-ref vec (uint32->fixnum index)) (js-absent))
		    #f)
		   (else
		    #t))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-has-own-property ::JsArray ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-has-own-property o::JsArguments p %this::JsGlobalObject)
   (not (eq? (js-get-own-property o p %this) (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsArguments p %this)
   (or (get-mapped-property o p) (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-get-own-property-descriptor ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property-descriptor o::JsArguments p %this)
   (let ((desc (get-mapped-property o p)))
      (if desc
	  (js-from-property-descriptor %this p desc o)
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get-property-value ::JsArguments ...                          */
;*    -------------------------------------------------------------    */
;*    This method is optional. It could be removed without changing    */
;*    the programs behaviors. It merely optimizes access to strings.   */
;*---------------------------------------------------------------------*/
(define-method (js-get-property-value o::JsArguments base p %this)
   (let ((index::uint32 (js-toindex p)))
      (if (js-isindex? index)
	  (with-access::JsArguments o (vec)
	     (let ((len (vector-length vec)))
		(if (<=u32 (fixnum->uint32 len) index)
		    (call-next-method)
		    (let ((d (u32vref vec index)))
		       (cond
			  ((eq? d (js-absent))
			   (call-next-method))
			  ((js-object-mode-inline? o)
			   d)
			  ((isa? d JsPropertyDescriptor)
			   (js-property-value o o p d %this))
			  (else
			   d))))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsArguments ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-10.6         */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsArguments p %this)
   (with-access::JsArguments o (vec properties)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (if (eq? p (& "length"))
		 (if (js-object-mode-inline? o)
		     (with-access::JsArguments o (vec)
			(vector-length vec))
		     (call-next-method))
		 (call-next-method)))
	    ((<uint32 i (vector-length vec))
	     (let ((desc (u32vref vec i)))
		(cond
		   ((eq? desc (js-absent))
		    (call-next-method))
		   ((js-object-mode-inline? o)
		    desc)
		   ((isa? desc JsPropertyDescriptor)
		    (js-property-value o o p desc %this))
		   (else
		    desc))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject-name/cache-miss ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-get-jsobject-name/cache-miss o::JsArguments p::obj
		  throw::bool %this::JsGlobalObject
		  cache::JsPropertyCache)
   (js-get o p %this))

;*---------------------------------------------------------------------*/
;*    js-sloppy-arguments ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-10.6         */
;*---------------------------------------------------------------------*/
(define (js-sloppy-arguments %this::JsGlobalObject vec::vector)
   (with-access::JsGlobalObject %this (js-arguments-cmap)
      (instantiateJsArguments
	 (vec vec)
	 (cmap js-arguments-cmap)
	 (elements (vector (vector-length vec) (js-undefined)))
	 (__proto__ (js-object-proto %this)))))

;*---------------------------------------------------------------------*/
;*    js-strict-arguments ...                                          */
;*---------------------------------------------------------------------*/
(define (js-strict-arguments %this::JsGlobalObject vec::vector)
   
   (define (value->descriptor v i)
      (instantiate::JsValueDescriptor
	 (name (js-integer->jsstring i))
	 (value v)
	 (writable #t)
	 (configurable #t)
	 (enumerable #t)))
   
   (let ((len (vector-length vec)))
      ;; build the arguments object
      (with-access::JsGlobalObject %this (js-strict-arguments-cmap)
	 (instantiateJsArguments
	    (vec vec)
	    (cmap js-strict-arguments-cmap)
	    (elements (vector (vector-length vec)
			 strict-callee-property
			 strict-caller-property))
	    (__proto__ (js-object-proto %this))))))

;*---------------------------------------------------------------------*/
;*    js-arguments->vector..                                           */
;*---------------------------------------------------------------------*/
(define (js-arguments->vector obj::JsArguments %this)
   (with-access::JsArguments obj (vec)
      (vector-map (lambda (d)
		     (if (isa? d JsPropertyDescriptor)
			 (with-access::JsPropertyDescriptor d (name)
			    (js-property-value obj obj name d %this))
			 d))
	 vec)))

;*---------------------------------------------------------------------*/
;*    js-arguments->list ...                                           */
;*---------------------------------------------------------------------*/
(define (js-arguments->list obj::JsArguments %this)
   (with-access::JsArguments obj (vec)
      (map! (lambda (d)
		     (if (isa? d JsPropertyDescriptor)
			 (with-access::JsPropertyDescriptor d (name)
			    (js-property-value obj obj name d %this))
			 d))
	 (vector->list vec))))

;*---------------------------------------------------------------------*/
;*    js-arguments->jsarray ...                                        */
;*---------------------------------------------------------------------*/
(define (js-arguments->jsarray obj::JsArguments %this)
   (with-access::JsArguments obj (vec)
      (js-vector->jsarray
	 (vector-map (lambda (d)
			(if (isa? d JsPropertyDescriptor)
			    (with-access::JsPropertyDescriptor d (name)
			       (js-property-value obj obj name d %this))
			    d))
	    vec)
	 %this)))

;*---------------------------------------------------------------------*/
;*    js-jsobject->jsarray ::JsArray ...                               */
;*---------------------------------------------------------------------*/
(define-method (js-jsobject->jsarray o::JsArguments %this)
   (js-arguments->jsarray o %this))

;*---------------------------------------------------------------------*/
;*    js-freeze ::JsArguments ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-freeze o::JsArguments obj)
   (with-access::JsArguments o (vec)
      (let loop ((i (-fx (vector-length vec) 1)))
	 (when (>=fx i 0)
	    (let ((desc (vector-ref vec i)))
	       (cond
		  ((isa? desc JsPropertyDescriptor)
		   (js-freeze-property! desc))
		  ((not (eq? desc (js-absent)))
		   (let ((desc (instantiate::JsValueDescriptor
				  (enumerable #t)
				  (writable #t)
				  (configurable #t)
				  (name (js-index-name i))
				  (value (vector-ref vec i)))))
		      (vector-set! vec i desc)
		      (js-freeze-property! desc)))))
	    (loop (-fx i 1))))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-for-in ::JsArguments ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::JsArguments proc %this)
   (with-access::JsArguments o (vec)
      (if (>fx (vector-length vec) 0)
	  (let ((len (minfx (vector-length vec)
			(uint32->fixnum
			   (js-touint32 (js-get o (& "length") %this) %this)))))
	     (let loop ((i 0))
		(if (<fx i len)
		    (begin
		       (proc (js-integer->jsstring i) %this)
		       (loop (+fx i 1)))
		    (call-next-method))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-for-of ::JsArguments ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-for-of o::JsArguments proc close %this)
   (with-access::JsGlobalObject %this (js-symbol-iterator)
      (let ((fun (js-get o js-symbol-iterator %this)))
	 (if (js-procedure? fun)
	     (js-for-of-iterator (js-call0 %this fun o) o proc close %this)
	     (with-access::JsArguments o (vec)
		(let ((len (minfx (vector-length vec)
			      (uint32->fixnum
				 (js-touint32 (js-get o (& "length") %this) %this)))))
		   (let loop ((i 0))
		      (when (<fx i len)
			 (let ((v (vector-ref vec i)))
			    (if (isa? v JsPropertyDescriptor)
				(proc (js-property-value o o i v %this) %this)
				(proc v %this)))
			 (loop (+fx i 1))))))))))

;*---------------------------------------------------------------------*/
;*    vector-slice! ...                                                */
;*---------------------------------------------------------------------*/
(define (vector-slice! o val::vector k::long final::long %this)
   (let* ((len (fixnum->uint32 (-fx final k)))
	  (arr (js-array-construct-alloc/lengthu32 %this len)))
      (with-access::JsArray arr (vec ilen length)
	 (vector-copy! vec 0 val k final)
	 (set! ilen len)
	 (set! length len)
	 arr)))

;*---------------------------------------------------------------------*/
;*    js-arguments-slice1 ...                                          */
;*---------------------------------------------------------------------*/
(define (js-arguments-slice1 this::JsArguments start %this)
   (js-arguments-slice this start (js-arguments-length this %this) %this))

;*---------------------------------------------------------------------*/
;*    js-arguments-slice ...                                           */
;*    -------------------------------------------------------------    */
;*    Arguments slicing is optimized because the pattern               */
;*    Array.prototype.slice.call( arguments, ... ) is very             */
;*    frequent to extract optional arguments.                          */
;*---------------------------------------------------------------------*/
(define (js-arguments-slice this::JsArguments start end %this)
   
   (define (array-copy! o len::long arr k::obj final::obj)
      (let loop ((i len))
	 (cond
	    ((= k final)
	     (js-put-length! arr i #f #f %this)
	     arr)
	    ((eq? (js-get-property o (js-toname k %this) %this) (js-undefined))
	     (set! k (+ 1 k))
	     (loop (+fx i 1)))
	    (else
	     (js-put! arr i (js-get o k %this) #f %this)
	     (set! k (+ 1 k))
	     (loop (+fx i 1))))))
   
   (define (array-slice! o k::obj final::obj)
      (let ((arr (js-array-construct-alloc/lengthu32 %this
		    (fixnum->uint32 (- final k)))))
	 (array-copy! o 0 arr k final)))
   
   (if (js-object-mode-inline? this)
       (with-access::JsArguments this (vec)
	  (let* ((len (vector-length vec))
		 (end (if (eq? end (js-undefined)) len end)))
	     (if (and (fixnum? start) (fixnum? end))
		 ;; optimal case, arguments is inlined
		 ;; and indexes are all small integers
		 (let* ((relstart start)
			(k (if (<fx relstart 0) (maxfx (+fx len relstart) 0) (minfx relstart len)))
			(relend end)
			(final (if (<fx relend 0) (maxfx (+fx len relend) 0) (minfx relend len))))
		    (cond
		       ((<=fx final k)
			(js-empty-vector->jsarray %this))
		       ((<=fx final len)
			(vector-slice! this vec k final %this))
		       ((>fx len 0)
			(let* ((arr (vector-slice! this vec k len %this))
			       (vlen (-fx len k)))
			   (array-copy! this vlen arr (-fx len vlen) final)))
		       (else
			(array-slice! this k final))))
		 ;; less optimized case, there is something
		 ;; weird with the indexes that are not
		 ;; small integers
		 (let* ((relstart (if (fixnum? start) start (js-tointeger start %this)))
			(k (if (< relstart 0) (max (+ len relstart) 0) (min relstart len)))
			(relend (if (fixnum? end) end (js-tointeger end %this)))
			(final (if (< relend 0) (max (+ len relend) 0) (min relend len))))
		    (cond
		       ((<= final k)
			(js-empty-vector->jsarray %this))
		       ((<= final len)
			(vector-slice! this vec (->fixnum k) (->fixnum final) %this))
		       ((>fx len 0)
			(let* ((arr (vector-slice! this vec (->fixnum k) len %this))
			       (vlen (->fixnum (js-get-length arr %this))))
			   (array-copy! this vlen arr (- len vlen) final)))
		       (else
			(array-slice! this k final)))))))
       ;; generic slow case
       (let* ((len (js-arguments-length this %this))
	      (relstart (js-tointeger start %this))
	      (k (if (< relstart 0) (max (+ len relstart) 0) (min relstart len)))
	      (relend (if (eq? end (js-undefined)) len (js-tointeger end %this)))
	      (final (if (< relend 0) (max (+ len relend) 0) (min relend len))))
	  (array-slice! this k final))))

;*---------------------------------------------------------------------*/
;*    js-arguments-vector-slice ...                                    */
;*---------------------------------------------------------------------*/
(define (js-arguments-vector-slice this::vector start end %this)
   (let* ((vec this)
	  (len (vector-length vec))
	  (end (if (eq? end (js-undefined)) len end)))
      (cond
	 ((=fx len 0)
	  (js-empty-vector->jsarray %this))
	 ((and (fixnum? start) (fixnum? end))
	  ;; optimal case, arguments is inlined
	  ;; and indexes are all small integers
	  (let* ((relstart start)
		 (k (if (<fx relstart 0) (maxfx (+fx len relstart) 0) (minfx relstart len)))
		 (relend end)
		 (final (if (<fx relend 0) (maxfx (+fx len relend) 0) (minfx relend len))))
	     (cond
		((<=fx final k)
		 (js-empty-vector->jsarray %this))
		((<=fx final len)
		 (vector-slice! this vec k final %this))
		(else
		 (vector-slice! this vec k len %this)))))
	 (else
	  ;; less optimized case, there is something
	  ;; weird with the indexes that are not
	  ;; small integers
	  (let* ((relstart (if (fixnum? start) start (js-tointeger start %this)))
		 (k (if (< relstart 0) (max (+ len relstart) 0) (min relstart len)))
		 (relend (if (fixnum? end) end (js-tointeger end %this)))
		 (final (if (< relend 0) (max (+ len relend) 0) (min relend len))))
	     (cond
		((<= final k)
		 (js-empty-vector->jsarray %this))
		((<= final len)
		 (vector-slice! this vec (->fixnum k) (->fixnum final) %this))
		(else
		 (vector-slice! this vec (->fixnum k) len %this))))))))
  
;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
