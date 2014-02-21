;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/hopscript/array.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Wed Jan 29 14:13:54 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript arrays                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_array

   (library hop)
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_number)

   (export js-array
	   js-array-prototype
	   (js-init-array! ::JsObject)
	   (js-vector->jsarray::JsArray vec::vector)
	   
	   (%js-array this . items)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsArray ...                                    */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsArray op compile isexpr)
   (let ((len (js-get o 'length)))
      (if (= len 0)
	  (display "[]" op)
	  (begin
	     (display "[" op)
	     (hop->javascript (js-get o (js-toname 0)) op compile isexpr)
	     (let loop ((i 1))
		(if (= i len)
		    (display "]" op)
		    (begin
		       (display "," op)
		       (when (js-has-property o (js-toname i))
			  (hop->javascript (js-get o i)
			     op compile isexpr))
		       (loop (+ i 1)))))))))

;*---------------------------------------------------------------------*/
;*    js-array ...                                                     */
;*---------------------------------------------------------------------*/
(define js-array #f)
(define js-array-prototype #f)

;*---------------------------------------------------------------------*/
;*    xml-body ::JsArray ...                                           */
;*---------------------------------------------------------------------*/
(define-method (xml-body obj::JsArray)
   (with-access::JsArray obj (vec)
      (vector->list vec)))

;*---------------------------------------------------------------------*/
;*    js-init-array! ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4         */
;*---------------------------------------------------------------------*/
(define (js-init-array! %this)
   ;; create the builtin prototype
   (set! js-array-prototype
      (instantiate::JsArray
	 (vec '#())
	 (__proto__ js-object-prototype)
	 (properties (list
			(instantiate::JsValueDescriptor
			   (name 'length)
			   (value 0)
			   (configurable #f)
			   (writable #t))))
	 (extensible #t)))
   ; Create a HopScript array object constructor
   (let* ((obj (js-make-function %js-array 1 "JsArray"
		  :prototype js-array-prototype
		  :__proto__ js-function-prototype
		  :alloc js-array-alloc
		  :construct js-array-construct))
	  (isarray (instantiate::JsValueDescriptor
		      (name 'isArray)
		      (writable #f)
		      (enumerable #f)
		      (configurable #f)
		      (value (js-make-function
				(lambda (this arg) (isa? arg JsArray))
				1 "isArray")))))
      (with-access::JsObject obj (properties)
	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.5.1
	 (set! properties (cons isarray properties)))
      (set! js-array obj)
      ;; init the prototype properties
      (init-builtin-array-prototype! js-array-prototype)
      ;; bind Array in the global object
      (js-bind! %this 'Array :configurable #f :enumerable #f :value js-array)
      js-array))

;*---------------------------------------------------------------------*/
;*    %js-sarray ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.1       */
;*---------------------------------------------------------------------*/
(define (%js-array this . items)
   (apply js-array-construct (js-array-alloc js-array) items))

;*---------------------------------------------------------------------*/
;*    js-array-alloc ...                                               */
;*---------------------------------------------------------------------*/
(define (js-array-alloc constructor::JsFunction)
   (instantiate::JsArray
      (cmap #f)
      (__proto__ (js-get constructor 'prototype))))

;*---------------------------------------------------------------------*/
;*    js-array-construct ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.2.1     */
;*---------------------------------------------------------------------*/
(define (js-array-construct this::JsArray . items)
   
   (define (array-set! v::vector #!optional (len (vector-length v)))
      (let ((lenproperty (instantiate::JsValueDescriptor
			    (name 'length)
			    (value len)
			    (configurable #f)
			    (writable #t))))
	 (with-access::JsArray this (vec properties)
	    (set! vec v)
	    (set! properties (list lenproperty))))
      this)

   (if (and (pair? items) (null? (cdr items)))
       (let ((i (car items)))
	  (cond
	     ((not (number? i))
	      (array-set! (apply vector items)))
	     ((not (= (js-touint32 i) i))
	      (js-raise
		 (js-new js-range-error
		    (format "index out of range ~a" i))))
	     ((< i (bit-lsh 1 16))
	      ;; MS CARE: the max boundary for a concrete vector
	      ;; is pure heuristic. This should be confirmed by
	      ;; actual tests
	      (array-set! (make-vector (->fixnum i) (js-absent))))
	     (else
	      (array-set! (make-vector 10 (js-absent)) i))))
       (array-set! (apply vector items))))

;*---------------------------------------------------------------------*/
;*    js-vector->jsarray ...                                           */
;*---------------------------------------------------------------------*/
(define (js-vector->jsarray::JsArray vec::vector)
   (let ((lenproperty (instantiate::JsValueDescriptor
			 (name 'length)
			 (value (vector-length vec))
			 (configurable #f)
			 (writable #t))))
      (instantiate::JsArray
	 (extensible #t)
	 (__proto__ js-array-prototype)
	 (properties (list lenproperty))
	 (vec vec))))

;*---------------------------------------------------------------------*/
;*    js-property-names ::JsArray ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-property-names obj::JsArray enump)
   (with-access::JsArray obj (vec)
      (let ((len (max 0 (js-touint32 (js-get obj 'length)))))
	 (let loop ((i (-fx (->fixnum (min len (vector-length vec))) 1))
		    (acc '()))
	    (if (=fx i -1)
		(vector-append (apply vector acc) (call-next-method))
		(let ((v (vector-ref vec i)))
		   (if (eq? v (js-absent))
		       (loop (-fx i 1) acc)
		       (loop (-fx i 1) (cons (integer->string i) acc)))))))))

;*---------------------------------------------------------------------*/
;*    js-array-vector-properties ...                                   */
;*    -------------------------------------------------------------    */
;*    Returns the subset of the array properties which are stored      */
;*    in its inline vector.                                            */
;*---------------------------------------------------------------------*/
(define (js-array-vector-properties obj::JsArray)
   (with-access::JsArray obj (vec)
      (let ((len (max 0 (js-touint32 (js-get obj 'length)))))
	 (let loop ((i (-fx (->fixnum (min len (vector-length vec))) 1))
		    (acc '()))
	    (if (=fx i -1)
		acc
		(let ((v (vector-ref vec i)))
		   (if (eq? v (js-absent))
		       (loop (-fx i 1) acc)
		       (let ((desc (instantiate::JsValueDescriptor
				      (name (js-toname i))
				      (value v)
				      (writable #t)
				      (enumerable #t)
				      (configurable #t))))
			  (loop (-fx i 1) (cons desc acc))))))))))


;*---------------------------------------------------------------------*/
;*    js-has-property ::JsArray ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.5.2     */
;*---------------------------------------------------------------------*/
(define-method (js-has-property o::JsArray p)
   (let ((index (js-toindex p)))
      (if index
	  (with-access::JsArray o (vec)
	     (let ((len (vector-length vec))
		   (index (->fixnum index)))
		(if (or (<=fx len index)
			(eq? (vector-ref-ur vec index) (js-absent)))
		    (call-next-method)
		    #t)))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get-property ::JsArray ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-get-property o::JsArray p)
   (let ((index (js-toindex p)))
      (if index
	  (with-access::JsArray o (vec)
	     (let ((len (vector-length vec))
		   (index (->fixnum index)))
		(if (or (<=fx len index)
			(eq? (vector-ref-ur vec index) (js-absent)))
		    (call-next-method)
		    (instantiate::JsValueDescriptor
		       (name (js-toname index))
		       (value (vector-ref-ur vec index))
		       (writable #t)
		       (enumerable #t)
		       (configurable #t)))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsArray ...                                             */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsArray p)
   (with-access::JsArray o (vec properties)
      (let ((i (js-toindex p)))
	 (cond
	    ((not i)
	     (call-next-method))
	    ((and (vector? vec)
		  (< i (vector-length vec))
		  (< i (js-touint32 (js-get o 'length))))
	     (let ((v (vector-ref vec (->fixnum i))))
		(if (eq? v (js-absent))
		    (call-next-method)
		    v)))
	    (else
	     (call-next-method))))))
       
;*---------------------------------------------------------------------*/
;*    js-put! ::JsArray ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.5       */
;*---------------------------------------------------------------------*/
(define-method (js-put! o::JsArray p v throw)

   (define (js-put-array! o::JsArray p v)
      (if (not (js-can-put o p))
	  ;; 1
	  (js-undefined)
	  (let ((owndesc (js-get-own-property o p)))
	     ;; 2
	     (if (js-is-data-descriptor? owndesc)
		 ;; 3
		 (if (eq? (js-toname p) 'length)
		     (let ((newdesc (duplicate::JsValueDescriptor owndesc
				       (value v))))
			(js-define-own-property o p newdesc throw))
		     (with-access::JsValueDescriptor owndesc ((valuedesc value))			
			(set! valuedesc v)
			(js-define-own-property o p owndesc throw)))
		 (let ((desc (js-get-property o p)))
		    ;; 4
		    (if (js-is-accessor-descriptor? desc)
			;; 5
			(with-access::JsAccessorDescriptor desc ((setter set))
			   (if (isa? setter JsFunction)
			       (js-call1 setter o v)
			       (js-undefined)))
			(let ((newdesc (instantiate::JsValueDescriptor
					  (name (js-toname p))
					  (value v)
					  (writable #t)
					  (enumerable #t)
					  (configurable #t))))
			   ;; 6
			   (js-define-own-property o p newdesc throw)))))
	     v)))

   (with-access::JsArray o (vec inline frozen)
      (let ((i (js-toindex p)))
	 (cond
	    ((not i)
	     (js-put-array! o (js-toname p) v))
	    ((and (< i (vector-length vec))
		  (< i (js-touint32 (js-get o 'length))))
	     (vector-set! vec (->fixnum i) v))
	    (else
	     (js-put-array! o (js-toname p) v))))))

;*---------------------------------------------------------------------*/
;*    js-delete! ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsArray p throw)
   (with-access::JsArray o (vec inline properties frozen)
      (let ((i (js-toindex p)))
	 (cond
	    ((not i)
	     (call-next-method))
	    ((< i (vector-length vec))
	     (unless frozen
		(vector-set! vec (->fixnum i) (js-absent))
		#t))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsArray p)
   (with-access::JsArray o (vec frozen)
      (let ((i (js-toindex p)))
	 (cond
	    ((not i)
	     (call-next-method))
	    ((< i (vector-length vec))
	     (let ((len (max (js-touint32 (js-get o 'length)) 0))
		   (fi (->fixnum i)))
		(if (or (>= i len) (eq? (vector-ref vec fi) (js-absent)))
		    (call-next-method)
		    (instantiate::JsValueDescriptor
		       (name (js-toname p))
		       (value (vector-ref vec fi))
		       (enumerable #t)
		       (writable (not frozen))
		       (configurable (not frozen))))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-define-own-property ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.5.1     */
;*---------------------------------------------------------------------*/
(define-method (js-define-own-property a::JsArray p desc throw)
   
   (define (reject msg)
      (when throw
	 (js-raise-type-error msg p)))

   (define (newwritable! oldlendesc newlendesc)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.5.1
      (or (not (isa? newlendesc JsValueDescriptor))
	  (with-access::JsValueDescriptor newlendesc (writable)
	     (not (eq? writable #f)))))

   (define (js-array-property-names arr)
      (let loop ((o arr))
	 (with-access::JsObject o (cmap properties __proto__)
	    (append (if cmap
			(with-access::JsConstructMap cmap (names)
			   (vector->list names))
			(map (lambda (d)
				(with-access::JsPropertyDescriptor d (name)
				   name))
			   properties))
	       (if (isa? __proto__ JsObject)
		   (loop __proto__)
		   '())))))
      
   (define (delete-out-of-range! a newlendesc newwritable oldlen newlen)
      ;; delete all the properties that are at an index greater that the
      ;; new length
      ;; the cannonical implementation would be as follows:
      ;;    (let while ()
      ;;      ;; 3.l
      ;;      (when (< newlen oldlen)
      ;;         (set! oldlen (- oldlen 1))
      ;;         (let ((r (js-get-delete! a oldlen #f)))
      ;;            (if (eq? r #f)
      ;;                ;; 3.l.iii
      ;;                (with-access::JsValueDescriptor newlendesc
      ;;                      (value writable)
      ;;                   ;; 3.l.iii.1
      ;;                   (set! value (+ oldlen 1))
      ;;                   ;; 3.l.iii.2
      ;;                   (set! writable newwritable)
      ;;                   ;; 3.l.iii.3
      ;;                   (js-define-own-property% a "length" newlendesc #f)
      ;;                   ;; 3.l.iii.4
      ;;                   (reject))
      ;;                (while)))))
      ;; however, this implementation is not practical for sparse vectors
      (with-access::JsArray a (vec)
	 (if (>fx (vector-length vec) 0)
	     ;; an inline vector
	     (let ((stop (->fixnum newlen)))
		(let loop ((i (-fx (vector-length vec) 1)))
		   (when (>=fx i stop)
		      ;; 3.l
		      (vector-set! vec i  (js-absent))
		      (loop (-fx i 1)))))
	     (for-each (lambda (name)
			  (let ((num (string->number (symbol->string! name))))
			     (when (and num (>= num newlen))
				(let ((r (js-delete! a name #f)))
				   (if (eq? r #f)
				       ;; 3.l.iii
				       (with-access::JsValueDescriptor newlendesc (value writable)
					  ;; 3.l.iii.1
					  (set! value (+ num 1))
					  ;; 3.l.iii.2
					  (set! writable newwritable)
					  ;; 3.l.iii.3
					  (js-define-own-property% a 'length newlendesc #f)
					  ;; 3.l.iii.4
					  (reject (format "Cannot delete element ~a" num))))))))
		(sort (lambda (n1 n2)
			 (string>? (symbol->string! n1) (symbol->string! n2)))
		   (js-array-property-names a))))))
   
   (define (define-own-property/length oldlendesc)
      (with-access::JsValueDescriptor oldlendesc (value writable)
	 (let ((oldlen value))
	    (if (not (isa? desc JsValueDescriptor))
		;; 3.a
		(call-next-method)
		(with-access::JsValueDescriptor desc (value)
		   (let ((newlendesc (duplicate::JsValueDescriptor desc))
			 (newlen (js-touint32 value)))
		      (unless (= newlen (js-tonumber value))
			 ;; 3.d
			 (js-raise
			    (js-new js-range-error
			       (format "Illegal length: ~s"
				  (js-tostring value)))))
		      (with-access::JsValueDescriptor newlendesc (value)
			 ;; 3.e
			 (set! value newlen))
		      (if (>= newlen oldlen)
			  ;; 3.f
			  (js-define-own-property% a 'length newlendesc throw)
			  (if (not writable)
			      ;; 3.g
			      (reject "property read-only \"~a\"")
			      ;; 3.h
			      (let* ((newwritable (newwritable!
						     oldlendesc newlendesc))
				     (_ (with-access::JsValueDescriptor newlendesc (writable)
					   (set! writable #t)))
				     (desc (js-define-own-property%
					      a 'length newlendesc throw)))
				 (if (not desc)
				     ;; 3.k
				     desc
				     (begin
					;; 3.l
					(delete-out-of-range!
					   a newlendesc newwritable
					   oldlen newlen)
					;; 3.m
					(if (not newwritable)
					    ;; 3.m.i
					    (js-define-own-property% a 'length
					       (instantiate::JsDataDescriptor
						  (name 'length)
						  (writable #f))
					       #f)
					    ;; 3.n
					    #t))))))))))))

   (define (js-default-array-property! desc)
      (with-access::JsPropertyDescriptor desc (enumerable configurable)
	 ;; complete array property with default attribute value
	 (unless (boolean? configurable)
	    (set! configurable #t))
	 (unless (boolean? enumerable)
	    (set! enumerable #t))))

   (define (js-default-array-data-property! desc)
      (js-default-array-property! desc)
      (with-access::JsDataDescriptor desc (writable)
	 (unless (boolean? writable)
	    (set! writable #t))))

   (define (js-default-array-accessor-property! desc)
      (js-default-array-property! desc)
      (with-access::JsAccessorDescriptor desc (get set)
	 (unless (isa? get JsFunction) (set! get (js-undefined)))
	 (unless (isa? set JsFunction) (set! set (js-undefined)))))

   (define (js-default-array-generic-property! desc)
      (cond
	 ((isa? desc JsDataDescriptor) (js-default-array-data-property! desc))
	 ((isa? desc JsAccessorDescriptor) (js-default-array-accessor-property! desc))
	 (else (js-default-array-property! desc))))

   (define (inline-value-descriptor? desc)
      (when (isa? desc JsValueDescriptor)
	 (with-access::JsValueDescriptor desc (enumerable writable configurable)
	    (and (eq? enumerable #t)
		 (eq? writable #t)
		 (not (eq? configurable #f))))))
   
   (define (uninline-array! arr::JsArray)
      ;; this function switch from a fast inlined array representation
      ;; to a slow inefficient object representation
      (with-access::JsArray arr (vec properties cmap)
	 (when (>fx (vector-length vec) 0)
	    (set! cmap #f)
	    (set! properties
	       (append (js-array-vector-properties arr) properties))
	    (set! vec '#()))
	 arr))

   (define (js-define-own-property-array a p desc throw)
      (with-access::JsArray a (vec)
	 (cond
	    ((eq? (vector-ref vec (->fixnum p)) (js-absent))
	     (let ((r (js-define-own-property% a (js-toname p) desc #f)))
		(unless (inline-value-descriptor? desc)
		   (uninline-array! a))
		r))
	    ((isa? desc JsValueDescriptor)
	     (with-access::JsValueDescriptor desc (value)
		(if (inline-value-descriptor? desc)
		    (vector-set! vec (->fixnum p) value)
		    (begin
		       (uninline-array! a)
		       (js-define-own-property% a (js-toname p) desc #f)))))
	    ((isa? desc JsAccessorDescriptor)
	     (uninline-array! a)
	     (js-define-own-property% a (js-toname p) desc #f))
	    (else
	     (uninline-array! a)
	     (js-define-own-property% a (js-toname p) desc #f)))))

   (let ((oldlendesc (js-get-own-property a 'length)))
      (cond
	 ((eq? p 'length)
	  ;; 3
	  (define-own-property/length oldlendesc))
	 ((js-toindex p)
	  ;; 4
	  (with-access::JsValueDescriptor oldlendesc ((oldlen value) writable)
	     (with-access::JsArray a (vec)
		(let ((index (js-touint32 p)))
		   (if (and (>= index oldlen) (not (eq? writable #t)))
		       ;; 4.b
		       (reject "wrong index ~a")
		       ;; 4.c
		       (let ((s (if (< index (vector-length vec))
				    (js-define-own-property-array
				       a index desc #f)
				    (begin
				       (uninline-array! a)
				       (js-define-own-property%
					  a (js-toname p) desc #f)))))
			  (cond
			     ((not s)
			      (reject "wrong index \"~a\""))
			     ((>= index oldlen)
			      ;; 4.e.i, 
			      (set! oldlen (+ index 1))
			      ;; 4.e.ii
			      (js-define-own-property a 'length oldlendesc #f)
			      ;; 4.f
			      #t))))))))
	 (else
	  ;; 5
	  (call-next-method)))))
   
;*---------------------------------------------------------------------*/
;*    init-builtin-array-prototype! ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.3       */
;*---------------------------------------------------------------------*/
(define (init-builtin-array-prototype! obj)
   
   (define (not-implemented . l)
      (error "hopscript" "array method not implemented" l))
   
   ;; prototype fields
   (js-bind! obj 'constructor
      :value js-array
      :enumerable #f)
   (js-bind! obj 'toString
      :value (js-make-function array-prototype-tostring 0 "toString"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'toLocaleString
      :value (js-make-function array-prototype-tolocalestring 0 "toLocaleString"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'concat
      :value (js-make-function array-prototype-concat 1 "concat"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'join
      :value (js-make-function array-prototype-join 1 "join"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'pop
      :value (js-make-function array-prototype-pop 0 "pop"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'push
      :value (js-make-function array-prototype-push 1 "push"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'reverse
      :value (js-make-function array-prototype-reverse 0 "reverse"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'shift
      :value (js-make-function array-prototype-shift 0 "shift"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'slice
      :value (js-make-function array-prototype-slice 2 "slice"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'sort
      :value (js-make-function array-prototype-sort 1 "sort"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'splice
      :value (js-make-function array-prototype-splice 2 "splice"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'unshift
      :value (js-make-function array-prototype-unshift 1 "unshift"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'indexOf
      :value (js-make-function array-prototype-indexof 1 "indexOf"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'lastIndexOf
      :value (js-make-function array-prototype-lastindexof 1 "lastIndexOf"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'every
      :value (js-make-function array-prototype-every 1 "every"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'some
      :value (js-make-function array-prototype-some 1 "some"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'forEach
      :value (js-make-function array-prototype-foreach 1 "forEach"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'map
      :value (js-make-function array-prototype-map 1 "map"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'filter
      :value (js-make-function array-prototype-filter 1 "filter"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'reduce
      :value (js-make-function array-prototype-reduce 1 "reduce"
		:prototype (js-undefined))
      :enumerable #f)
   (js-bind! obj 'reduceRight
      :value (js-make-function array-prototype-reduceright 1 "reduceRight"
		:prototype (js-undefined))
      :enumerable #f))

;*---------------------------------------------------------------------*/
;*    array-prototype-tostring ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.2     */
;*---------------------------------------------------------------------*/
(define (array-prototype-tostring this::obj)
   (let* ((o (js-toobject this))
	  (func (js-get this 'join)))
      (if (isa? func JsFunction)
	  (js-call1 func this (js-undefined))
	  (js-tostring this))))

;*---------------------------------------------------------------------*/
;*    array-prototype-tolocalestring ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.3     */
;*---------------------------------------------------------------------*/
(define (array-prototype-tolocalestring this::obj)
   
   (define (el->string el)
      (if (or (eq? el (js-undefined)) (eq? el (js-null)))
	  ""
	  (let ((obj (js-toobject el)))
	     ;; MS CARE: I'm not sure that the conversion js-tostring is
	     ;; correct as I don't see where it is demanded by the spec
	     (js-tostring (js-call0 (js-get obj 'toLocaleString) obj)))))
   
   (let* ((o (js-toobject this))
	  (lenval (js-touint32 (js-get o 'length))))
      (if (= lenval 0)
	  ""
	  (let* ((sep ",")
		 (el0 (el->string (js-get o 0))))
	     (let loop ((r (list el0))
			(i 1))
		(if (= i lenval)
		    (apply string-append (reverse! r))
		    (loop (cons* (el->string (js-get o i)) sep r)
		       (+ i 1))))))))

;*---------------------------------------------------------------------*/
;*    array-prototype-concat ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.4     */
;*---------------------------------------------------------------------*/
(define (array-prototype-concat this::obj . l)
   
   (define (copy-array-slow target tstart src sstart send)
      ;; slow copy, elements by elements
      (let loop ((i sstart)
		 (j tstart))
	 (if (= i send)
	     j
	     (begin
		(js-put! target j (js-get src i) #f)
		(loop (+ i 1) (+ j 1))))))
   
   (define (copy-array src dst i)
      (with-access::JsArray src ((vsrc vec))
	 (with-access::JsArray dst ((vdst vec))
	    ;; try to use a vector copy
	    (if (and (>fx (vector-length vdst) 0)
		     (>fx (vector-length vsrc) 0))
		(let ((lsrc (vector-length vsrc))
		      (slen (js-get src 'length)))
		   ;; fast vector-copy
		   (vector-copy! vdst i vsrc 0 lsrc)
		   (if (> slen lsrc)
		       (copy-array-slow dst (+fx i lsrc) src lsrc slen)
		       (+fx i lsrc)))
		;; slow copy
		(copy-array-slow dst i src 0 (js-get src 'length))))))
   
   (let* ((l (cons (js-toobject this) l))
	  (new-len (let loop ((l l)
			      (len 0))
		      (cond
			 ((null? l)
			  len)
			 ((isa? (car l) JsArray)
			  (loop (cdr l) (js+ len (js-get (car l) 'length))))
			 (else
			  (loop (cdr l) (+ 1 len))))))
	  (arr (js-array-construct (js-array-alloc js-array) new-len)))
      ;; fill the vector
      (let loop ((l l)
		 (i 0))
	 (cond
	    ((null? l)
	     arr)
	    ((isa? (car l) JsArray)
	     (loop (cdr l) (copy-array (car l) arr i)))
	    (else
	     (js-put! arr i (car l) #f)
	     (loop (cdr l) (+fx 1 i)))))))

;*---------------------------------------------------------------------*/
;*    array-prototype-join ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.5     */
;*---------------------------------------------------------------------*/
(define (array-prototype-join this::obj separator)

   (define (el->string el)
      (if (or (eq? el (js-undefined)) (eq? el (js-null)))
	  ""
	  (js-tostring el)))
   
   (let* ((o (js-toobject this))
	  (lenval (js-touint32 (js-get o 'length)))
	  (sep (if (eq? separator (js-undefined)) "," (js-tostring separator))))
      (if (= lenval 0)
	  ""
	  (let* (
		 (el0 (el->string (js-get o 0))))
	     (let loop ((r (list el0))
			(i 1))
		(if (= i lenval)
		    (apply string-append (reverse! r))
		    (loop (cons* (el->string (js-get o i)) sep r)
		       (+ i 1))))))))
	 
;*---------------------------------------------------------------------*/
;*    array-prototype-pop ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.6     */
;*---------------------------------------------------------------------*/
(define (array-prototype-pop this::obj)
   (let* ((o (js-toobject this))
	  (len (js-touint32 (js-get o 'length))))
      (cond
	 ((= len 0)
	  (js-put! o 'length 0 #f)
	  (js-undefined))
	 ((> len 0)
	  (let* ((indx (- len 1))
		 (el (js-get o indx)))
	     (js-delete! o indx #t)
	     (js-put! o 'length indx #f)
	     el)))))
	  
;*---------------------------------------------------------------------*/
;*    array-prototype-push ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.7     */
;*---------------------------------------------------------------------*/
(define (array-prototype-push this::obj . items)
   (let ((o (js-toobject this)))
      (let loop ((n (js-touint32 (js-get o 'length)))
		 (items items))
	 (if (pair? items)
	     (begin
		(js-put! o n (car items) #f)
		(loop (+ n 1) (cdr items)))
	     (begin
		(js-put! o 'length n #f)
		n)))))

;*---------------------------------------------------------------------*/
;*    array-prototype-reverse ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.8     */
;*---------------------------------------------------------------------*/
(define (array-prototype-reverse this::obj)
   
   (define (vector-reverse! val len)
      (let ((len/2 (/fx len 2)))
	 (let loop ((i 0))
	    (unless (=fx i len/2)
	       (let ((t (vector-ref val i))
		     (ni (+fx i 1)))
		  (vector-set! val i (vector-ref val (-fx len ni)))
		  (vector-set! val (-fx len ni) t)
		  (loop ni))))))

   (define (array-reverse! o)
      (let* ((len (js-touint32 (js-get o 'length)))
	     (len/2 (floor (/ len 2))))
	 (let loop ((i 0))
	    (if (= i len/2)
		o
		(let* ((t (js-get o i))
		       (ni (+ i 1))
		       (rni (- len ni)))
		   (js-put! o i (js-get o rni) #f)
		   (js-put! o rni t #f)
		   (loop ni))))))

   (let ((o (js-toobject this)))
      (if (isa? o JsArray)
	  (with-access::JsArray o (vec)
	     (let ((len (js-touint32 (js-get o 'length))))
		(if (<= len (vector-length vec))
		    ;; fast path
		    (begin
		       (vector-reverse! vec (->fixnum len))
		       o)
		    (array-reverse! o))))
	  (array-reverse! o))))

;*---------------------------------------------------------------------*/
;*    array-prototype-shift ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.9     */
;*---------------------------------------------------------------------*/
(define (array-prototype-shift this::obj)
   
   (define (vector-shift! o val len)
      (let ((first (vector-ref val 0)))
	 (vector-copy! val 0 val 1)
	 (vector-set! val (-fx len 1) (js-absent))
	 (js-put! o 'length (-fx len 1) #f)
	 first))
   
   (define (array-shift! o len)
      (let ((first (js-get o 0)))
	 (let loop ((i 1))
	    (cond
	       ((= i len)
		(js-delete! o (- i 1) #t)
		(js-put! o 'length (- i 1) #f)
		first)
	       ((eq? (js-get-property o (js-toname i)) (js-undefined))
		(js-delete! o (- i 1) #t)
		(loop (+ i 1)))
	       (else
		(let ((v (js-get o i)))
		   (js-put! o (- i 1) v #f)
		   (loop (+ i 1))))))))
   
   (let* ((o (js-toobject this))
	  (len (js-touint32 (js-get o 'length))))
      (cond
	 ((= len 0)
	  (js-put! o 'length 0 #f)
	  (js-undefined))
	 ((isa? o JsArray)
	  (with-access::JsArray o (vec inline)
	     (if (<= len (vector-length vec))
		 ;; fast path
		 (vector-shift! o vec
		    (minfx (vector-length vec) (->fixnum len)))
		 ;; fast path
		 (array-shift! o len))))
	 (else
	  (array-shift! o len)))))

;*---------------------------------------------------------------------*/
;*    array-prototype-slice ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.10    */
;*---------------------------------------------------------------------*/
(define (array-prototype-slice this::obj start end)
   
   (define (vector-slice! o val k final)
      (let* ((vec (vector-copy val k final))
	     (arr (js-vector->jsarray vec)))
	 (let loop ((i (-fx (vector-length vec) 1)))
	    (cond
	       ((=fx i -1)
		arr)
	       ((not (eq? (vector-ref vec i) (js-absent)))
		(unless (=fx i (-fx (vector-length vec) 1))
		   (js-put! arr 'length i #f))
		arr)
	       (else
		(loop (-fx i 1)))))))

   (define (array-copy! o len arr k final)
      (let loop ((i len))
	 (cond
	    ((= k final)
	     (js-put! arr 'length i #f)
	     arr)
	    ((eq? (js-get-property o (js-toname k)) (js-undefined))
	     (set! k (+ 1 k))
	     (loop (+fx i 1)))
	    (else
	     (js-put! arr i (js-get o k) #f)
	     (set! k (+ 1 k))
	     (loop (+fx i 1))))))
   
   (define (array-slice! o k final)
      (let ((arr (js-array-construct (js-array-alloc js-array) (- final k))))
	 (array-copy! o 0 arr k final)))
   
   (let* ((o (js-toobject this))
	  (len (js-touint32 (js-get o 'length)))
	  (relstart (js-tointeger start))
	  (k (if (< relstart 0) (max (+ len relstart) 0) (min relstart len)))
	  (relend (if (eq? end (js-undefined)) len (js-tointeger end)))
	  (final (if (< relend 0) (max (+ len relend) 0) (min relend len))))
      (cond
	 ((<= final k)
	  (js-vector->jsarray '#()))
	 ((not (isa? o JsArray))
	  (array-slice! o k final))
	 (else
	  (with-access::JsArray o (vec)
	     (let ((vlen (vector-length vec)))
		(cond
		   ((<= final vlen)
		    (vector-slice! o vec (->fixnum k) (->fixnum final)))
		   ((>fx vlen 0)
		    (let* ((arr (vector-slice! o vec (->fixnum k) vlen))
			   (vlen (->fixnum (js-get arr 'length))))
		       (array-copy! o vlen arr (- len vlen) final)))
		   (else
		    (array-slice! o k final)))))))))

;*---------------------------------------------------------------------*/
;*    array-prototype-sort ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.11    */
;*---------------------------------------------------------------------*/
(define (array-prototype-sort this::obj comparefn)
   
   (define (default-compare x y)
      (let ((nothasj (eq? x (js-absent)))
	    (nothask (eq? y (js-absent))))
	 (cond
	    (nothasj nothask)
	    (nothask #t)
	    ((eq? x (js-undefined)) (eq? y (js-undefined)))
	    ((eq? y (js-undefined)) #t)
	    ((and (integer? x) (integer? y)) (< x y))
	    (else (string<? (js-tostring x) (js-tostring y))))))
   
   (define (make-compare comparefn)
      (lambda (x y)
	 (let ((nothasj (eq? x (js-absent)))
	       (nothask (eq? y (js-absent))))
	    (cond
	       (nothasj nothask)
	       (nothask #t)
	       ((eq? x (js-undefined)) (eq? y (js-undefined)))
	       ((eq? y (js-undefined)) #t)
	       (else (<= (js-call2 comparefn (js-undefined) x y) 0))))))
   
   (define (get-compare comparefn)
      (cond
	 ((eq? comparefn (js-undefined))
	  default-compare)
	 ((not (isa? comparefn JsFunction))
	  (js-raise
	     (js-new js-type-error
		(format "sort: argument not a function ~s"
		   comparefn))))
	 (else
	  (with-access::JsFunction comparefn (proc)
	     (make-compare comparefn)))))
   
   (define (vector-sort this cmp)
      (with-access::JsArray this (vec)
	 ($sort-vector vec cmp)))

   (define (partition arr cmp left right pivotindex)
      (let ((pivotvalue (js-get arr pivotindex)))
	 (js-put! arr pivotindex (js-get arr right) #f)
	 (js-put! arr right pivotvalue #f)
	 (let loop ((i left)
		    (s left))
	    (if (< i right)
		(let ((vi (js-get arr i)))
		   (if (cmp vi pivotvalue)
		       (begin
			  (unless (= i s)
			     (let ((vi (js-get arr i)))
				(js-put! arr i (js-get arr s) #f)
				(js-put! arr s vi #f)))
			  (loop (+ i 1) (+ s 1)))
		       (loop (+ i 1) s)))
		(let ((si (js-get arr s)))
		   (js-put! arr s (js-get arr right) #f)
		   (js-put! arr right si #f)
		   s)))))
	 
	 
   (define (quicksort arr cmp left right)
      ;; http://en.wikipedia.org/wiki/Quicksort
      (when (< left right)
	 (let ((pivotindex (+ left
			      (inexact->exact (round (/ (- right left) 2))))))
	    (let ((pivotnewindex (partition arr cmp left right pivotindex)))
	       (quicksort arr cmp left (- pivotnewindex 1))
	       (quicksort arr cmp (+ pivotnewindex 1) right)))))
   
   (define (array-sort arr cmp)
      (let ((len (js-touint32 (js-get arr 'length))))
	 (unless (< len 2)
	    (quicksort arr cmp 0 (- len 1)))
	 arr))
   
   (let ((o (js-toobject this)))
      (if (not (isa? this JsArray))
	  (array-sort this (get-compare comparefn))
	  (with-access::JsArray this (vec)
	     (cond
		((>fx (vector-length vec) 0)
		 (vector-sort this (get-compare comparefn)))
		((= (js-touint32 (js-get o 'length)) 0)
		 this)
		(else
		 (array-sort this (get-compare comparefn))))))))

;*---------------------------------------------------------------------*/
;*    array-get-elements ...                                           */
;*---------------------------------------------------------------------*/
(define (array-get-elements arr start len)
   (let loop ((i (- len 1))
	      (acc '()))
      (if (< i start)
	  acc
	  (loop (- i 1) (cons (js-get arr i) acc)))))

;*---------------------------------------------------------------------*/
;*    array-prototype-splice ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.12    */
;*---------------------------------------------------------------------*/
(define (array-prototype-splice this::obj start deletecount . items)

   (define (vector-splice this actualstart actualdeletecount)
      (with-access::JsArray this (vec)
	 (let* ((len (vector-length vec))
		(litems (length items))
		(rlen (+fx actualstart actualdeletecount))
		(nlen (+fx len (-fx litems actualdeletecount)))
		(res (js-vector->jsarray (vector-copy vec actualstart rlen))))
	    (when (>fx nlen len)
	       ;; enargle the vector if needed
	       (let ((tmp (make-vector nlen)))
		  (vector-copy! tmp 0 vec 0 actualstart)
		  (let ((cstart (+fx actualstart actualdeletecount)))
		     (vector-copy! tmp (-fx nlen (-fx len cstart)) vec cstart len))
		  (set! vec tmp)))
	    ;; insert the new items
	    (let loop ((k actualstart)
		       (items items))
	       (if (pair? items)
		   (begin
		      (vector-set! vec k (car items))
		      (loop (+fx k 1) (cdr items)))
		   (begin
		      (when (< k len)
			 (vector-copy! vec k vec rlen len))
		      (js-put! this 'length nlen #f))))
	    res)))

   (define (array-splice arr len actualstart actualdeletecount)
      (let* ((els (array-get-elements arr actualstart (+ actualstart actualdeletecount)))
	     (res (js-vector->jsarray (list->vector els)))
	     (rest (array-get-elements arr (+ actualstart actualdeletecount) len)))
	 ;; add all the new elements
	 (for-each (lambda (el)
		      (js-put! arr actualstart el #f)
		      (set! actualstart (+ 1 actualstart)))
	    items)
	 (for-each (lambda (el)
		      (js-put! arr actualstart el #f)
		      (set! actualstart (+ 1 actualstart)))
	    rest)
	 ;; remove all the remaining elements
	 (let loop ()
	    (when (< actualstart len)
	       (js-delete! arr actualstart #f)
	       (set! len (- len 1))))
	 ;; shrink the vector
	 (js-put! arr 'length actualstart #f)
	 res))

   (let* ((o (js-toobject this))
	  (relstart (js-tointeger start))
	  (len (js-touint32 (js-get o 'length)))
	  (actualstart (if (< relstart 0) (max (+ len relstart) 0) (min relstart len)))
	  (actualdeletecount (min (max (js-tointeger deletecount) 0) (- len actualstart))))
      (if (not (isa? this JsArray))
	  (array-splice this len actualstart actualdeletecount)
	  (with-access::JsArray this (vec)
	     (cond
		((>fx (vector-length vec) 0)
		 (vector-splice this
		    (->fixnum actualstart) (->fixnum actualdeletecount)))
		((= len 0)
		 (js-vector->jsarray '#()))
		(else
		 (array-splice this len actualstart actualdeletecount)))))))

;*---------------------------------------------------------------------*/
;*    array-prototype-unshift ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.13    */
;*---------------------------------------------------------------------*/
(define (array-prototype-unshift this::obj . items)

   (define (vector-unshift arr)
      (with-access::JsArray arr (vec)
	 (let* ((litems (length items))
		(len (vector-length vec))
		(nvec (make-vector (+fx len litems))))
	    (vector-copy! nvec litems vec)
	    (let ((i 0))
	       (for-each (lambda (el)
			    (vector-set! nvec i el))
		  items)
	       (set! vec nvec)
	       (js-put! arr 'length (+ len litems) #f)
	       (+fx litems len)))))
   
   (define (array-unshift arr len)
      (let ((rest (array-get-elements arr 0 len))
	    (i 0))
	 ;; add all the new elements
	 (for-each (lambda (el)
		      (js-put! arr i el #f)
		      (set! i (+ 1 i)))
	    items)
	 (for-each (lambda (el)
		      (js-put! arr i el #f)
		      (set! i (+ 1 i)))
	    rest)
	 ;; shrink the vector
	 (js-put! arr 'length i #f)
	 i))

   (let* ((o (js-toobject this))
	  (len (js-touint32 (js-get o 'length))))
      (if (null? items)
	  (begin
	     ;; override the length as the conversion touin32 might have
	     ;; changed it
	     (js-put! o 'length len #f)
	     len)
	  (if (not (isa? this JsArray))
	      (array-unshift this len)
	      (with-access::JsArray this (vec)
		 (cond
		    ((>fx (vector-length vec) 0)
		     (vector-unshift this))
		    (else
		     (array-unshift this len))))))))
   
;*---------------------------------------------------------------------*/
;*    array-prototype-indexof ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.14    */
;*---------------------------------------------------------------------*/
(define (array-prototype-indexof this::obj el . indx)
   
   (define (vector-indexof arr vec k len)
      (let loop ((k k))
	 (cond
	    ((>=fx k len)
	     -1)
	    ((js-strict-equal? (vector-ref vec k) el)
	     k)
	    ((and (eq? (vector-ref vec k) (js-absent))
		  (let ((name (js-toname k)))
		     (and (js-has-property arr name)
			  (js-strict-equal? (js-get arr name) el))))
	     k)
	    (else
	     (loop (+fx k 1))))))
   
   (define (array-indexof arr k len)
      (let loop ((k k))
	 (cond
	    ((>= k len)
	     -1)
	    ((let ((name (js-toname k)))
		(and (js-has-property arr name)
		     (js-strict-equal? (js-get arr name) el)))
	     k)
	    (else
	     (loop (+ k 1))))))
   
   (let* ((o (js-toobject this))
	  (len (js-touint32 (js-get o 'length))))
      (if (= len 0)
	  -1
	  (let ((n (if (pair? indx) (js-tointeger (car indx)) 0)))
	     (if (> n len)
		 -1
		 (let ((k (if (< n 0)
			      (let ((k (- len (abs n))))
				 (if (< k 0) 0 k))
			      n)))
		    (if (isa? o JsArray)
			(with-access::JsArray o (vec)
			   (if (>fx (vector-length vec) 0)
			       (if (> k (vector-length vec))
				   -1
				   (vector-indexof o vec (->fixnum k) (vector-length vec)))
			       (array-indexof this k len)))
			(array-indexof o k len))))))))
		    
;*---------------------------------------------------------------------*/
;*    array-prototype-lastindexof ...                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.15    */
;*---------------------------------------------------------------------*/
(define (array-prototype-lastindexof this::obj el . indx)

   (define (vector-lastindexof arr vec k)
      (let loop ((k k))
	 (cond
	    ((< k 0)
	     -1)
	    ((js-strict-equal? (vector-ref vec k) el)
	     k)
	    ((and (eq? (vector-ref vec k) (js-absent))
		  (let ((name (js-toname k)))
		     (and (js-has-property arr name)
			  (js-strict-equal? (js-get arr name) el))))
	     k)
	    (else
	     (loop (- k 1))))))
      
   (define (array-lastindexof arr k)
      (let loop ((k k))
	 (cond
	    ((< k 0)
	     -1)
	    ((let ((name (js-toname k)))
		(and (js-has-property arr name)
		     (js-strict-equal? (js-get arr name) el)))
	     k)
	    (else
	     (loop (- k 1))))))
      
   (let* ((o (js-toobject this))
	  (len (js-touint32 (js-get o 'length))))
      (if (= len 0)
	  -1
	  (let* ((n (if (pair? indx) (js-tointeger (car indx)) (- len 1)))
		 (k (if (< n 0) (- len (abs n)) (if (< n (- len 1)) n (- len 1)))))
	     (if (isa? o JsArray)
		 (with-access::JsArray o (vec)
		    (if (and (>fx (vector-length vec) 0) (> k 0))
			(vector-lastindexof o vec (->fixnum (min k (vector-length vec))))
			(array-lastindexof o k)))
		 (array-lastindexof o k))))))

;*---------------------------------------------------------------------*/
;*    array-prototype-iterator ...                                     */
;*---------------------------------------------------------------------*/
(define (array-prototype-iterator this proc t array-iterator vector-iterator)
   (let* ((o (js-toobject this))
	  (len (js-touint32 (js-get o 'length))))
      (if (not (isa? proc JsFunction))
	  (js-raise-type-error "Not a procedure ~s" proc)
	  (if (not (isa? o JsArray))
	      (array-iterator o len proc t)
	      (with-access::JsArray o (vec)
		 (if (and (>fx (vector-length vec) 0)
			  (<= len (vector-length vec)))
		     (vector-iterator o (->fixnum len) proc t)
		     (array-iterator o len proc t)))))))

;*---------------------------------------------------------------------*/
;*    array-prototype-every ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.16    */
;*---------------------------------------------------------------------*/
(define (array-prototype-every this::obj proc t)

   (define (test-val proc t v i o)
      (js-totest (js-call3 proc t v i o)))
   
   (define (vector-every o len::long proc t)
      (with-access::JsArray o (vec)
	 (letrec ((loop-in-property
		     (lambda (i)
			(let ((p (js-get-property o (js-toname i))))
			   (cond
			      ((eq? p (js-undefined))
			       (loop-in-vec (+fx i 1)))
			      ((test-val proc t (js-property-value o p) i o)
			       (loop-in-vec (+fx i 1)))
			      (else
			       #f)))))
		  (loop-in-vec
		     (lambda (i)
			(cond
			   ((>=fx i len)
			    #t)
			   ((=fx (vector-length vec) 0)
			    (loop-in-property i))
			   (else
			    (let ((v (vector-ref vec i)))
			       (cond
				  ((eq? v (js-absent))
				   (loop-in-property i))
				  ((test-val proc t v i o)
				   (loop-in-vec (+fx i 1)))
				  (else
				   #f))))))))
	    (loop-in-vec 0))))
   
   (define (array-every o len proc t)
      (let loop ((i 0))
	 (if (< i len)
	     (let* ((n (js-toname i))
		    (p (js-get-property o n)))
		(cond
		   ((eq? p (js-undefined))
		    (loop (+ i 1)))
		   ((test-val proc t (js-property-value o p) i o)
		    (loop (+ i 1)))
		   (else
		    #f)))
	     #t)))

   (array-prototype-iterator this proc t array-every vector-every))

;*---------------------------------------------------------------------*/
;*    array-prototype-some ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.16    */
;*---------------------------------------------------------------------*/
(define (array-prototype-some this::obj proc t)
   
   (define (test-val proc t v i o)
      (js-totest (js-call3 proc t v i o)))

   (define (vector-some o len::long proc t)
      (with-access::JsArray o (vec)
	 (letrec ((loop-in-property
		     (lambda (i)
			(let ((p (js-get-property o (js-toname i))))
			   (cond
			      ((eq? p (js-undefined))
			       (loop-in-vec (+fx i 1)))
			      ((test-val proc t (js-property-value o p) i o)
			       #t)
			      (else
			       (loop-in-vec (+fx i 1)))))))
		  (loop-in-vec
		     (lambda (i)
			(cond
			   ((>=fx i len)
			    #f)
			   ((=fx (vector-length vec) 0)
			    (loop-in-property i))
			   (else
			    (let ((v (vector-ref vec i)))
			       (cond
				  ((eq? v (js-absent))
				   (loop-in-property i))
				  ((test-val proc t v i o)
				   #t)
				  (else
				   (loop-in-vec (+fx i 1))))))))))
	    (loop-in-vec 0))))
   
   (define (array-some o len proc t)
      (let loop ((i 0))
	 (if (< i len)
	     (let ((p (js-get-property o (js-toname i))))
		(cond
		   ((eq? p (js-undefined))
		    (loop (+ i 1)))
		   ((test-val proc t (js-property-value o p) i o)
		    #t)
		   (else
		    (loop (+ i 1)))))
	     #f)))

   (array-prototype-iterator this proc t array-some vector-some))

;*---------------------------------------------------------------------*/
;*    array-prototype-foreach ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.18    */
;*---------------------------------------------------------------------*/
(define (array-prototype-foreach this::obj proc t)
   
   (define (vector-foreach o len::long proc t)
      (with-access::JsArray o (vec)
	 (let loop ((i 0))
	    (when (<fx i len)
	       (if (=fx (vector-length vec) 0)
		   (let ((p (js-get-property o (js-toname i))))
		      (unless (eq? p (js-undefined))
			 (js-call3 proc t (js-property-value o p) i o)))
		   (let ((v (vector-ref vec i)))
		      (if (eq? v (js-absent))
			  (let ((p (js-get-property o (js-toname i))))
			     (unless (eq? p (js-undefined))
				(js-call3 proc t (js-property-value o p) i o)))
			 (js-call3 proc t v i o))))
	       (loop (+fx i 1))))))
   
   (define (array-foreach o len proc t)
      (let loop ((i 0))
	 (when (< i len)
	    (let ((p (js-get-property o (js-toname i))))
	       (unless (eq? p (js-undefined))
		  (js-call3 proc t (js-property-value o p) i o))
	       (loop (+ i 1))))))
   
   (array-prototype-iterator this proc t array-foreach vector-foreach)
   (js-undefined))

;*---------------------------------------------------------------------*/
;*    array-prototype-map ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.19    */
;*---------------------------------------------------------------------*/
(define (array-prototype-map this::obj proc t)

   (define (vector-map o len::long proc t)
      (with-access::JsArray o (vec)
	 (let ((a (make-vector (vector-length vec) (js-absent))))
	    (let loop ((i 0))
	       (cond
		  ((>=fx i len)
		   (js-vector->jsarray a))
		  ((=fx (vector-length vec) 0)
		   (let ((p (js-get-property o (js-toname i))))
		      (unless (eq? p (js-undefined))
			 (vector-set! a i
			    (js-call3 proc t (js-property-value o p) i o)))
		      (loop (+fx i 1))))
		  (else
		   (let ((v (vector-ref vec i)))
		      (if (eq? v (js-absent))
			  (let ((p (js-get-property o (js-toname i))))
			     (unless (eq? p (js-undefined))
				(vector-set! a i
				   (js-call3 proc t (js-property-value o p) i o))))
			  (vector-set! a i (js-call3 proc t v i o)))
		      (loop (+fx i 1)))))))))
   
   (define (array-map o len proc t)
      (let ((a (js-array-construct (js-array-alloc js-array) len)))
	 (let loop ((i 0))
	    (if (< i len)
		(let ((p (js-get-property o (js-toname i))))
		   (unless (eq? p (js-undefined))
		      (js-put! a i
			 (js-call3 proc t (js-property-value o p) i o)
			 #f))
		   (loop (+ i 1)))
		a))))
   
   (array-prototype-iterator this proc t array-map vector-map))

;*---------------------------------------------------------------------*/
;*    array-prototype-filter ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.20    */
;*---------------------------------------------------------------------*/
(define (array-prototype-filter this::obj proc t)
   
   (define (vector-filter o len::long proc t)
      (with-access::JsArray o (vec)
	 (let ((a (make-vector (vector-length vec) (js-absent))))
	    (let loop ((i 0)
		       (j 0))
	       (cond
		  ((>=fx i len)
		   (let ((res (js-vector->jsarray a)))
		      (js-put! res 'length j #f)
		      res))
		  ((=fx (vector-length vec) 0)
		   ;; the vector has been modified by the callback...
		   (let ((p (js-get-property o (js-toname i))))
		      (if (eq? p (js-undefined))
			  (loop (+fx i 1) j)
			  (let ((v (js-property-value o p)))
			     (if (js-totest (js-call3 proc t v i o))
				 (begin
				    (vector-set! a j v)
				    (loop (+fx i 1) (+fx j 1)))
				 (loop (+fx i 1) j))))))
		  (else
		   (let ((v (vector-ref vec i)))
		      (cond
			 ((eq? v (js-absent))
			  (let ((p (js-get-property o (js-toname i))))
			     (if (eq? p (js-undefined))
				 (loop (+fx i 1) j)
				 (let ((v (js-property-value o p)))
				    (if (js-totest (js-call3 proc t v i o))
					(begin
					   (vector-set! a j v)
					   (loop (+fx i 1) (+fx j 1)))
					(loop (+fx i 1) j))))))
			 ((js-totest (js-call3 proc t v i o))
			  (vector-set! a j v)
			  (loop (+fx i 1) (+fx j 1)))
			 (else
			  (loop (+fx i 1) j))))))))))
   
   (define (array-filter o len proc t)
      (let ((a (js-vector->jsarray '#())))
	 (let loop ((i 0)
		    (j 0))
	    (if (< i len)
		(let ((p (js-get-property o (js-toname i))))
		   (if (eq? p (js-undefined))
		       (loop (+ i 1) j)
		       (let ((v (js-property-value o p))
			     (nj (js-toname j)))
			  (if (js-totest (js-call3 proc t v i o))
			      (let ((newdesc (instantiate::JsValueDescriptor
						(name nj)
						(value v)
						(writable #t)
						(enumerable #t)
						(configurable #t))))
				 ;; 6
				 (js-define-own-property a nj newdesc #f)
				 (loop (+ i 1) (+ j 1)))
			      (loop (+ i 1) j)))))
		a))))
   
   (array-prototype-iterator this proc t array-filter vector-filter))

;*---------------------------------------------------------------------*/
;*    array-prototype-reduce ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.21    */
;*---------------------------------------------------------------------*/
(define (array-prototype-reduce this::obj proc . init)

   (define (reduce/accumulator o len i accumulator)
      (let loop ((i i)
		 (acc accumulator))
	 (if (< i len)
	     (let ((p (js-get-property o (js-toname i))))
		(if (eq? p (js-undefined))
		    (loop (+ i 1) acc)
		    (let ((v (js-property-value o p)))
		       (loop (+ i 1) (js-call4 proc (js-undefined) acc v i o)))))
	     acc)))
      
   (let* ((o (js-toobject this))
	  (len (js-touint32 (js-get o 'length))))
      (if (not (isa? proc JsFunction))
	  (js-raise-type-error "Not a procedure ~s" proc)
	  ;; find the accumulator init value
	  (if (null? init)
	      (let loop ((i 0))
		 (if (< i len)
		     (let ((p (js-get-property o (js-toname i))))
			(if (eq? p (js-undefined))
			    (loop (+ i 1))
			    (reduce/accumulator o len (+ i 1)
			       (js-property-value o p))))
		     (js-raise-type-error
			"reduce: cannot find accumulator ~s" this)))
	      (reduce/accumulator o len 0 (car init))))))

;*---------------------------------------------------------------------*/
;*    array-prototype-reduceright ...                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.22    */
;*---------------------------------------------------------------------*/
(define (array-prototype-reduceright this::obj proc . init)

   (define (reduce/accumulator o len i accumulator)
      (let loop ((i i)
		 (acc accumulator))
	 (if (>= i 0)
	     (let ((p (js-get-property o (js-toname i))))
		(if (eq? p (js-undefined))
		    (loop (- i 1) acc)
		    (let ((v (js-property-value o p)))
		       (loop (- i 1) (js-call4 proc (js-undefined) acc v i o)))))
	     acc)))
      
   (let* ((o (js-toobject this))
	  (len (js-touint32 (js-get o 'length))))
      (if (not (isa? proc JsFunction))
	  (js-raise-type-error "Not a procedure ~s" proc)
	  ;; find the accumulator init value
	  (if (null? init)
	      (let loop ((k (- len 1)))
		 (if (>= k 0)
		     (let ((p (js-get-property o (js-toname k))))
			(if (eq? p (js-undefined))
			    (loop (- k 1))
			    (let ((v (js-property-value o p)))
			       (reduce/accumulator o len (- k 1) v))))
		     (js-raise-type-error
			"reduce: cannot find accumulator ~s" this)))
	      (reduce/accumulator o len (- len 1) (car init))))))

;*---------------------------------------------------------------------*/
;*    js-seal ::JsArray ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-seal o::JsArray obj)
   (with-access::JsArray o (sealed)
      (set! sealed #t)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    js-freeze ::JsArray ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-freeze o::JsArray obj)
   (with-access::JsArray o (frozen)
      (set! frozen #t)
      (call-next-method)))
 
;*---------------------------------------------------------------------*/
;*    js-for-in ::JsArray ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-for-in o::JsArray proc)
   (with-access::JsArray o (vec)
      (if (>fx (vector-length vec) 0)
	  (let ((len (minfx (vector-length vec)
			(->fixnum (js-touint32 (js-get o 'length))))))
	     (let loop ((i 0))
		(if (<fx i len)
		    (begin
		       (proc (integer->string i))
		       (loop (+fx i 1)))
		    (call-next-method))))
	  (call-next-method))))
  
