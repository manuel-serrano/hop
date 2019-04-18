;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arguments.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 14 09:14:55 2013                          */
;*    Last change :  Thu Apr 18 08:10:38 2019 (serrano)                */
;*    Copyright   :  2013-19 Manuel Serrano                            */
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
	   (js-arguments ::JsGlobalObject ::vector)
	   (js-strict-arguments ::JsGlobalObject ::pair-nil)
	   (js-arguments->list ::JsArguments ::JsGlobalObject)
	   (js-arguments->jsarray ::JsArguments ::JsGlobalObject)
	   (js-arguments-ref ::JsArguments ::obj ::JsGlobalObject)
	   (js-arguments-index-ref ::JsArguments ::uint32 ::JsGlobalObject)
	   (js-arguments-length::obj ::JsArguments ::JsGlobalObject)))

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
	  (map! (lambda (desc)
		   (unless (eq? desc (js-absent))
		      (with-access::JsPropertyDescriptor desc (name)
			 (js-property-value obj obj name desc ctx))))
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
      (lambda ()
	 (let* ((%this ctx)
		(len::uint32 (js-touint32 (js-get o (& "length") %this) %this)))
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
;*    js-init-arguments! ...                                           */
;*---------------------------------------------------------------------*/
(define (js-init-arguments! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-arguments-cmap)
      ;; local constant strings
      (set! __js_strings (&init!))
      ;; arguments cmap
      (let ((arguments-cmap-props
	       `#(,(prop (& "length") (property-flags #t #f #t #f))
		  ,(prop (& "callee") (property-flags #t #f #t #f)))))
	 (set! js-arguments-cmap
	    (instantiate::JsConstructMap
	       (methods (make-vector (vector-length arguments-cmap-props)))
	       (props arguments-cmap-props))))))

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
	  (vector-ref vec idx)
	  (js-get arr idx %this))))

;*---------------------------------------------------------------------*/
;*    js-arguments-index-ref ...                                       */
;*---------------------------------------------------------------------*/
(define (js-arguments-index-ref arr::JsArguments idx::uint32 %this)
   (with-access::JsArguments arr (vec)
      (if (and (js-object-mode-inline? arr)
	       (<u32 idx (fixnum->uint32 (vector-length vec))))
	  (u32vref vec idx)
	  (js-get arr idx %this))))

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
		    (js-undefined)))))
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
	     (with-access::JsPropertyDescriptor (u32vref vec i) (configurable)
		(if configurable
		    (begin
		       (u32vset! vec i (js-absent))
		       #t)
		    #f)))
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
	    (let ((o (u32vref vec i)))
	       (unless (eq? o (js-absent))
		  o))))))

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
		       (if (isa? dset JsFunction)
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
			   (with-access::JsAccessorDescriptor ismapped
				 (set)
			      (when (isa? set JsFunction)
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
;*    js-has-property ::JsArguments ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.5.2     */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsArguments p %this)
   (let ((index::uint32 (js-toindex p)))
      (if (js-isindex? index)
	  (with-access::JsArguments o (vec)
	     (let ((len (vector-length vec)))
		(if (<=u32 (fixnum->uint32 len) index)
		    (call-next-method)
		    #t)))
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
		       (if (eq? d (js-absent))
			   (call-next-method)
			   (js-property-value o o p d %this))))))
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
	     (call-next-method))
	    ((<uint32 i (vector-length vec))
	     (let ((desc (u32vref vec i)))
		(if (eq? desc (js-absent))
		    (call-next-method)
		    (js-property-value o o p desc %this))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-miss ...                                */
;*---------------------------------------------------------------------*/
(define-method (js-object-get-name/cache-miss o::JsArguments p::JsStringLiteral
		  throw::bool %this::JsGlobalObject
		  cache::JsPropertyCache #!optional (point -1) (cspecs '()))
   (js-get o p %this))

;*---------------------------------------------------------------------*/
;*    js-arguments ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-10.6         */
;*---------------------------------------------------------------------*/
(define (js-arguments %this::JsGlobalObject vec::vector)
   (with-access::JsGlobalObject %this (js-arguments-cmap)
      (with-access::JsObject %this (__proto__)
	 (instantiateJsArguments
	    (vec vec)
	    (cmap js-arguments-cmap)
	    (elements (vector (vector-length vec) (js-undefined)))
	    (__proto__ __proto__)))))

;*---------------------------------------------------------------------*/
;*    js-strict-arguments ...                                          */
;*---------------------------------------------------------------------*/
(define (js-strict-arguments %this::JsGlobalObject lst::pair-nil)
   
   (define (value->descriptor v i)
      (instantiate::JsValueDescriptor
	 (name (js-integer->jsstring i))
	 (value v)
	 (writable #t)
	 (configurable #t)
	 (enumerable #t)))
   
   (let* ((len (length lst))
	  (vec (make-vector len)))
      ;; initialize the vector of descriptor
      (let loop ((i 0)
		 (lst lst))
	 (when (pair? lst)
	    (vector-set! vec i (value->descriptor (car lst) i))
	    (loop (+fx i 1) (cdr lst))))
      ;; build the arguments object
      (with-access::JsObject %this (__proto__)
	 (let ((obj (instantiateJsArguments
		       (vec vec)
		       (elements ($create-vector 3))
		       (__proto__ __proto__))))
	    (js-bind! %this obj (& "length")
	       :value len
	       :enumerable #f :configurable #t :writable #t
	       :hidden-class #t)
	    (js-bind! %this obj (& "caller")
	       :get thrower-get
	       :set thrower-set
	       :enumerable #f :configurable #f
	       :hidden-class #t)
	    (js-bind! %this obj (& "callee")
	       :get thrower-get
	       :set thrower-set
	       :enumerable #f :configurable #f
	       :hidden-class #t)
	    obj))))

;*---------------------------------------------------------------------*/
;*    js-arguments->vector..                                           */
;*---------------------------------------------------------------------*/
(define (js-arguments->vector obj::JsArguments %this)
   (with-access::JsArguments obj (vec)
      (vector-map (lambda (d)
		     (with-access::JsPropertyDescriptor d (name)
			(js-property-value obj obj name d %this)))
	 vec)))

;*---------------------------------------------------------------------*/
;*    js-arguments->list ...                                           */
;*---------------------------------------------------------------------*/
(define (js-arguments->list obj::JsArguments %this)
   (with-access::JsArguments obj (vec)
      (map! (lambda (d)
	       (with-access::JsPropertyDescriptor d (name)
		  (js-property-value obj obj name d %this)))
	 (vector->list vec))))

;*---------------------------------------------------------------------*/
;*    js-arguments->jsarray ...                                        */
;*---------------------------------------------------------------------*/
(define (js-arguments->jsarray obj::JsArguments %this)
   (with-access::JsArguments obj (vec)
      (js-vector->jsarray
	 (vector-map (lambda (d)
			(with-access::JsPropertyDescriptor d (name)
			   (js-property-value obj obj name d %this)))
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
	       (unless (eq? desc (js-absent))
		  (js-freeze-property! desc)))
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
	 (if (isa? fun JsFunction)
	     (js-for-of-iterator (js-call0 %this fun o) o proc close %this)
	     (with-access::JsArguments o (vec)
		(let ((len (minfx (vector-length vec)
			      (uint32->fixnum
				 (js-touint32 (js-get o (& "length") %this) %this)))))
		   (let loop ((i 0))
		      (when (<fx i len)
			 (proc (js-property-value o o i (vector-ref vec i) %this)
			    %this)
			 (loop (+fx i 1))))))))))


;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
