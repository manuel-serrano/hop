;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/array.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Thu Dec 10 19:37:54 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript arrays                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_array
   
   (library hop)
   
   (include "../nodejs/nodejs_debug.sch"
	    "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_number
	   __hopscript_worker
	   __hopscript_string)
   
   (export (js-init-array! ::JsGlobalObject)
	   (js-vector->jsarray::JsArray ::vector ::JsGlobalObject)
	   (jsarray->list::pair-nil ::JsArray ::JsGlobalObject)
	   (jsarray->vector::vector ::JsArray ::JsGlobalObject)
	   (js-array-comprehension ::JsGlobalObject ::obj ::procedure
	      ::obj ::pair ::bstring ::bstring ::pair)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsArray ...                                  */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsArray
   (lambda (o)
      (jsarray->vector o (js-initial-global-object)))
   (lambda (o %this)
      (js-vector->jsarray o (or %this (js-initial-global-object)))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsArray ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsArray worker %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-array)
	 (with-access::JsArray obj (vec frozen inline sealed)
	    (let ((nobj (js-vector->jsarray
			   (vector-map (lambda (e)
					  (js-donate e worker %_this))
			      vec)
			   %this)))
	       ;; donate the value of the array
	       (js-for-in obj
		  (lambda (k)
		     (js-put! nobj k
			(js-donate (js-get obj k %_this) worker %_this)
			#f %this))
		  %this)
	       nobj)))))
	    
;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsArray ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack obj::JsArray)
   (with-access::JsArray obj (vec)
      (if (>fx (vector-length vec) 0)
	  (filter! (lambda (x) (not (eq? x (js-absent)))) (vector->list vec))
	  (let* ((%this (js-initial-global-object))
		 (len (uint32->fixnum
			 (js-touint32 (js-get obj 'length %this) %this))))
	     (let loop ((i 0))
		(cond
		   ((=u32 i len)
		    '())
		   ((js-has-property obj (js-toname i %this) %this)
		    (cons (js-get obj i %this) (loop (+fx i 1))))
		   (else
		    (loop (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    xml-body-element ::JsArray ...                                   */
;*---------------------------------------------------------------------*/
(define-method (xml-body-element obj::JsArray)
   (xml-unpack obj))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsArray ...                                    */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsArray op compile isexpr)
   (let* ((%this (js-initial-global-object))
	  (len::uint32 (js-touint32 (js-get o 'length %this) %this)))
      (if (=u32 len (fixnum->uint32 0))
	  (display "sc_vector2array([])" op)
	  (begin
	     (display "sc_vector2array([" op)
	     (hop->javascript (js-get o (js-toname 0 %this) %this) op compile isexpr)
	     (let loop ((i (fixnum->uint32 1)))
		(if (=u32 i len)
		    (display "])" op)
		    (begin
		       (display "," op)
		       (when (js-has-property o (js-toname i %this) %this)
			  (hop->javascript (js-get o i %this)
			     op compile isexpr))
		       (loop (+u32 i (fixnum->uint32 1))))))))))

;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::JsArray ...                                */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute o::JsArray id op backend)
   (let ((v (xml-unpack o)))
      (when (pair? v)
	 (display (keyword->string! id) op)
	 (display "='" op)
	 (display (xml-attribute-encode (car v)) op)
	 (let loop ((v (cdr v)))
	    (when (pair? v)
	       (display " " op)
	       (display (xml-attribute-encode (car v)) op)
	       (loop (cdr v))))
	 (display "'" op))))

;*---------------------------------------------------------------------*/
;*    jsarray->list ...                                                */
;*---------------------------------------------------------------------*/
(define (jsarray->list o::JsArray %this)
   (let* ((%this (js-initial-global-object))
	  (len::uint32 (js-touint32 (js-get o 'length %this) %this)))
      (if (=u32 len (fixnum->uint32 0))
	  '()
	  (let loop ((i #u32:0))
	     (cond
		((=u32 i len)
		 '())
		((js-has-property o (js-toname i %this) %this)
		 (cons (cons (js-get o i %this) i) (loop (+u32 i #u32:1))))
		(else
		 (loop (+u32 i #u32:1))))))))

;*---------------------------------------------------------------------*/
;*    jsarray->vector ...                                              */
;*---------------------------------------------------------------------*/
(define (jsarray->vector o::JsArray %this)
   (let* ((%this (js-initial-global-object))
	  (len::uint32 (js-touint32 (js-get o 'length %this) %this)))
      (if (=u32 len (fixnum->uint32 0))
	  '#()
	  (let ((res (make-vector (uint32->fixnum len) #unspecified)))
	     (let loop ((i #u32:0))
		(cond
		   ((=u32 i len)
		    res)
		   ((js-has-property o (js-toname i %this) %this)
		    (vector-set! res (uint32->fixnum i) (js-get o i %this))
		    (loop (+u32 i #u32:1)))
		   (else
		    (loop (+u32 i #u32:1)))))))))
   

;*---------------------------------------------------------------------*/
;*    jsarray-fields ...                                               */
;*---------------------------------------------------------------------*/
(define jsarray-fields (vector (find-class-field JsObject 'vec)))

;*---------------------------------------------------------------------*/
;*    javascript-class-all-fields ::JsArray ...                        */
;*    -------------------------------------------------------------    */
;*    JSON serialization, see runtime/json.scm                         */
;*---------------------------------------------------------------------*/
(define-method (javascript-class-all-fields obj::JsArray)
   jsarray-fields)
   
;*---------------------------------------------------------------------*/
;*    js-init-array! ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4         */
;*---------------------------------------------------------------------*/
(define (js-init-array! %this)
   (with-access::JsGlobalObject %this (__proto__ js-array js-array-prototype
					 js-function)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 ;; builtin prototype
	 (set! js-array-prototype
	    (instantiate::JsArray
	       (vec '#())
	       (__proto__ __proto__)
	       (extensible #t)
	       (properties (list
			      ;; cannot be defined with js-bind! because
			      ;; of bootstrap specificities
			      (instantiate::JsValueDescriptor
				 (name 'length)
				 (value 0)
				 (configurable #f)
				 (writable #t))))))
	 
	 ;; create the array object constructor
	 (set! js-array
	    (js-make-function %this (%js-array %this) 1 'Array
	       :__proto__ js-function-prototype
	       :prototype js-array-prototype
	       :alloc (lambda (ctor) (js-array-alloc ctor %this))
	       :construct (lambda (this . items)
			     (apply js-array-construct %this this items))))
	 ;; other properties of the Array constructor
	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.5.1
	 (js-bind! %this js-array 'isArray
	    :value (js-make-function %this
		      (lambda (this arg) (isa? arg JsArray))
		      1 'isArray)
	    :writable #t
	    :enumerable #f)

	 ;; init the prototype properties
	 (init-builtin-array-prototype! %this js-array js-array-prototype)
	 
	 ;; bind Array in the global object
	 (js-bind! %this %this 'Array
	    :configurable #f :enumerable #f :value js-array)
	 
	 js-array)))

;*---------------------------------------------------------------------*/
;*    init-builtin-array-prototype! ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.3       */
;*---------------------------------------------------------------------*/
(define (init-builtin-array-prototype! %this js-array js-array-prototype)
   
   ;; constructor
   (js-bind! %this js-array-prototype 'constructor
      :value js-array :enumerable #f)
   
   ;; tostring
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.2
   (define (array-prototype-tostring this::obj)
      (let* ((o (js-toobject %this this))
	     (func (js-get this 'join %this)))
	 (if (isa? func JsFunction)
	     (js-call1 %this func this (js-undefined))
	     (js-tojsstring this %this))))
   
   (js-bind! %this js-array-prototype 'toString
      :value (js-make-function %this array-prototype-tostring 0 'toString
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; tolocaleString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.3
   (define (array-prototype-tolocalestring this::obj)
      
      (define (el->string el)
	 (if (or (eq? el (js-undefined)) (eq? el (js-null)))
	     ""
	     (let ((obj (js-toobject %this el)))
		;; MS CARE: I'm not sure that the conversion js-tojsstring
		;; is %this correct as I don't see where it is
		;; demanded by the spec
		(js-tostring
		   (js-call0 %this (js-get obj 'toLocaleString %this) obj)
		   %this))))
      
      (let* ((o (js-toobject %this this))
	     (lenval::uint32 (js-touint32 (js-get o 'length %this) %this)))
	 (if (=u32 lenval #u32:0)
	     (js-string->jsstring "")
	     (let* ((sep ",")
		    (el0 (el->string (js-get o 0 %this))))
		(let loop ((r (list el0))
			   (i 1))
		   (if (=u32 i lenval)
		       (js-stringlist->jsstring (reverse! r))
		       (loop (cons* (el->string (js-get o (uint32->fixnum i) %this))
				sep r)
			  (+u32 i #u32:1))))))))
   
   (js-bind! %this js-array-prototype 'toLocaleString
      :value (js-make-function %this array-prototype-tolocalestring 0 'toLocaleString
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; concat
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.4
   (define (array-prototype-concat this::obj . l)
      
      (define (copy-array-slow target tstart src sstart send)
	 ;; slow copy, elements by elements
	 (let loop ((i sstart)
		    (j tstart))
	    (if (= i send)
		j
		(begin
		   (js-put! target j (js-get src i %this) #f %this)
		   (loop (+ i 1) (+ j 1))))))
      
      (define (copy-array src dst i)
	 (with-access::JsArray src ((vsrc vec))
	    (with-access::JsArray dst ((vdst vec))
	       ;; try to use a vector copy
	       (if (and (>fx (vector-length vdst) 0)
			(>fx (vector-length vsrc) 0))
		   (let* ((lsrc (vector-length vsrc))
			  (slen (js-get src 'length %this))
			  (alen (minfx slen lsrc)))
		      ;; fast vector-copy
		      (vector-copy! vdst i vsrc 0 alen)
		      (if (> slen lsrc)
			  (copy-array-slow dst (+fx i lsrc) src lsrc slen)
			  (+fx i alen)))
		   ;; slow copy
		   (copy-array-slow dst i src 0 (js-get src 'length %this))))))
      
      (let* ((l (cons (js-toobject %this this) l))
	     (new-len (let loop ((l l)
				 (len 0))
			 (cond
			    ((null? l)
			     len)
			    ((isa? (car l) JsArray)
			     (loop (cdr l) (js+ len (js-get (car l) 'length %this) %this)))
			    (else
			     (loop (cdr l) (+ 1 len))))))
	     (arr (with-access::JsGlobalObject %this (js-array)
		     (js-array-construct %this
			(js-array-alloc js-array %this) new-len))))
	 ;; fill the vector
	 (let loop ((l l)
		    (i 0))
	    (cond
	       ((null? l)
		arr)
	       ((isa? (car l) JsArray)
		(loop (cdr l) (copy-array (car l) arr i)))
	       (else
		(js-put! arr i (car l) #f %this)
		(loop (cdr l) (+fx 1 i)))))))
   
   (js-bind! %this js-array-prototype 'concat
      :value (js-make-function %this array-prototype-concat 1 'concat
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; join
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.5
   (define (array-prototype-join this::obj separator)
      
      (define (el->string el)
	 (if (or (eq? el (js-undefined)) (eq? el (js-null)))
	     ""
	     (js-tostring el %this)))

      (let* ((o (js-toobject %this this))
	     (lenval::uint32 (js-touint32 (js-get o 'length %this) %this))
	     (sep (if (eq? separator (js-undefined))
		      ","
		      (js-tostring separator %this))))
	 (if (=u32 lenval #u32:0)
	     (js-string->jsstring "")
	     (let* ((el0 (el->string (js-get o 0 %this))))
		(let loop ((r (list el0))
			   (i #u32:1))
		   (if (=u32 i lenval)
		       (js-stringlist->jsstring (reverse! r))
		       (loop (cons* (el->string (js-get o i %this)) sep r)
			  (+u32 i #u32:1))))))))
   
   (js-bind! %this js-array-prototype 'join
      :value (js-make-function %this array-prototype-join 1 'join
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; pop
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.6
   (define (array-prototype-pop this::obj)
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get o 'length %this) %this)))
	 (cond
	    ((=u32 len #u32:0)
	     (js-put! o 'length 0 #f %this)
	     (js-undefined))
	    (else
	     (let* ((indx (-u32 len #u32:1))
		    (el (js-get o (uint32->integer indx) %this)))
		(js-delete! o indx #t %this)
		(js-put! o 'length (uint32->integer indx) #f %this)
		el)))))
   
   (js-bind! %this js-array-prototype 'pop
      :value (js-make-function %this array-prototype-pop 0 'pop
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; push
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.7
   (define (array-prototype-push this::obj . items)
      (let ((o (js-toobject %this this)))
	 (let loop ((n::uint32 (js-touint32 (js-get o 'length %this) %this)))
	    (if (not (isa? o JsArray))
		(let ((n (uint32->integer n)))
		   (for-each (lambda (item)
				(js-put! o n item #f %this)
				(set! n (+ 1 n)))
		      items)
		   (js-put! o 'length n #f %this)
		   n)
		(begin
		   (for-each (lambda (item)
				(js-put! o (uint32->integer n) item #f %this)
				(set! n (+u32 n #u32:1))
				(when (=u32 n #u32:0)
				   (js-raise-range-error %this
				      "Illegal length: ~s"
				      (js-tostring #l4294967296 %this))))
		      items)
		   (let ((ni (uint32->integer n)))
		      (js-put! o 'length ni  #f %this)
		      ni))))))
   
   (js-bind! %this js-array-prototype 'push
      :value (js-make-function %this array-prototype-push 1 'push
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; reverse
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.8
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
	 (let* ((len::uint32 (js-touint32 (js-get o 'length %this) %this))
		(len/2::uint32 (/u32 len #u32:2)))
	    (let loop ((i #u32:0))
	       (if (=u32 i len/2)
		   o
		   (let* ((t (js-get o (uint32->fixnum i) %this))
			  (ni (+u32 i (fixnum->uint32 1)))
			  (rni (uint32->integer (-u32 len ni))))
		      (js-put! o i (js-get o rni %this) #f %this)
		      (js-put! o rni t #f %this)
		      (loop ni))))))
      
      (let ((o (js-toobject %this this)))
	 (if (isa? o JsArray)
	     (with-access::JsArray o (vec)
		(let ((len::uint32 (js-touint32 (js-get o 'length %this) %this)))
		   (if (<=u32 len (fixnum->uint32 (vector-length vec)))
		       ;; fast path
		       (begin
			  (vector-reverse! vec (uint32->fixnum len))
			  o)
		       (array-reverse! o))))
	     (array-reverse! o))))
   
   (js-bind! %this js-array-prototype 'reverse
      :value (js-make-function %this array-prototype-reverse 0 'reverse
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; shift
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.9
   (define (array-prototype-shift this::obj)
      
      (define (vector-shift! o val len::long)
	 (let ((first (vector-ref val 0)))
	    (vector-copy! val 0 val 1)
	    (vector-set! val (-fx len 1) (js-absent))
	    (js-put! o 'length (-fx len 1) #f %this)
	    (if (eq? first (js-absent))
		(js-undefined)
		first)))
      
      (define (array-shift! o len::uint32)
	 (let ((first (js-get o 0 %this))
	       (len (uint32->fixnum len)))
	    (let loop ((i 1))
	       (cond
		  ((=fx i len)
		   (js-delete! o (-fx i 1) #t %this)
		   (js-put! o 'length (-fx i 1) #f %this)
		   first)
		  ((eq? (js-get-property o (js-toname i %this) %this) (js-undefined))
		   (js-delete! o (-fx i 1) #t %this)
		   (loop (+fx i 1)))
		  (else
		   (let ((v (js-get o i %this)))
		      (js-put! o (-fx i 1) v #f %this)
		      (loop (+fx i 1))))))))

      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get o 'length %this) %this)))
	 (cond
	    ((=u32 len (fixnum->uint32 0))
	     (js-put! o 'length 0 #f %this)
	     (js-undefined))
	    ((isa? o JsArray)
	     (with-access::JsArray o (vec)
		(if (<=u32 len (u32vlen vec))
		    ;; fast path
		    (vector-shift! o vec (uint32->fixnum len))
		    ;; fast path
		    (array-shift! o len))))
	    (else
	     (array-shift! o len)))))
   
   (js-bind! %this js-array-prototype 'shift
      :value (js-make-function %this array-prototype-shift 0 'shift
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; slice
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.10
   (define (array-prototype-slice this::obj start end)
      
      (define (vector-slice! o val k::long final::long)
	 (let* ((vec (vector-copy val k final))
		(arr (js-vector->jsarray vec %this))
		(len (vector-length vec)))
	    (let loop ((i (-fx len 1)))
	       (cond
		  ((=fx i -1)
		   arr)
		  ((not (eq? (vector-ref vec i) (js-absent)))
		   (unless (=fx i (-fx len 1))
		      (js-put! arr 'length i #f %this))
		   arr)
		  (else
		   (loop (-fx i 1)))))))
      
      (define (array-copy! o len::long arr k::obj final::obj)
	 (let loop ((i len))
	    (cond
	       ((= k final)
		(js-put! arr 'length i #f %this)
		arr)
	       ((eq? (js-get-property o (js-toname k %this) %this) (js-undefined))
		(set! k (+ 1 k))
		(loop (+fx i 1)))
	       (else
		(js-put! arr i (js-get o k %this) #f %this)
		(set! k (+ 1 k))
		(loop (+fx i 1))))))
      
      (define (array-slice! o k::obj final::obj)
	 (let ((arr (js-array-construct %this (js-array-alloc js-array %this)
		       (- final k))))
	    (array-copy! o 0 arr k final)))
      
      (let* ((o (js-toobject %this this))
	     (len (uint32->integer (js-touint32 (js-get o 'length %this) %this)))
	     (relstart (js-tointeger start %this))
	     (k (if (< relstart 0) (max (+ len relstart) 0) (min relstart len)))
	     (relend (if (eq? end (js-undefined)) len (js-tointeger end %this)))
	     (final (if (< relend 0) (max (+ len relend) 0) (min relend len))))
	 (cond
	    ((<= final k)
	     (js-vector->jsarray '#() %this))
	    ((not (isa? o JsArray))
	     (array-slice! o k final))
	    (else
	     (with-access::JsArray o (vec)
		(let ((vlen (js-array-vector-length o %this)))
		   (cond
		      ((<= final vlen)
		       (vector-slice! o vec (->fixnum k) (->fixnum final)))
		      ((>fx vlen 0)
		       (let* ((arr (vector-slice! o vec (->fixnum k) vlen))
			      (vlen (->fixnum (js-get arr 'length %this))))
			  (array-copy! o vlen arr (- len vlen) final)))
		      (else
		       (array-slice! o k final)))))))))
   
   (js-bind! %this js-array-prototype 'slice
      :value (js-make-function %this array-prototype-slice 2 'slice
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; sort
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.11
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
	       (else (string<? (js-tostring x %this) (js-tostring y %this))))))
      
      (define (make-compare comparefn)
	 (lambda (x y)
	    (let ((nothasj (eq? x (js-absent)))
		  (nothask (eq? y (js-absent))))
	       (cond
		  (nothasj nothask)
		  (nothask #t)
		  ((eq? x (js-undefined)) (eq? y (js-undefined)))
		  ((eq? y (js-undefined)) #t)
		  (else (<= (js-tointeger (js-call2 %this comparefn (js-undefined) x y) %this) 0))))))
      
      (define (get-compare comparefn)
	 (cond
	    ((eq? comparefn (js-undefined))
	     default-compare)
	    ((not (isa? comparefn JsFunction))
	     (js-raise-type-error %this
		"sort: argument not a function ~s" comparefn))
	    (else
	     (with-access::JsFunction comparefn (proc)
		(make-compare comparefn)))))
      
      (define (vector-sort this cmp)
	 (with-access::JsArray this (vec)
	    ($sort-vector vec cmp)
	    this))
      
      (define (partition arr cmp left right pivotindex)
	 (let ((pivotvalue (js-get arr pivotindex %this)))
	    (js-put! arr pivotindex (js-get arr right %this) #f %this)
	    (js-put! arr right pivotvalue #f %this)
	    (let loop ((i left)
		       (s left))
	       (if (< i right)
		   (let ((vi (js-get arr i %this)))
		      (if (cmp vi pivotvalue)
			  (begin
			     (unless (= i s)
				(let ((vi (js-get arr i %this)))
				   (js-put! arr i (js-get arr s %this) #f %this)
				   (js-put! arr s vi #f %this)))
			     (loop (+ i 1) (+ s 1)))
			  (loop (+ i 1) s)))
		   (let ((si (js-get arr s %this)))
		      (js-put! arr s (js-get arr right %this) #f %this)
		      (js-put! arr right si #f %this)
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
	 (let ((len (uint32->integer (js-touint32 (js-get arr 'length %this) %this))))
	    (unless (< len 2)
	       (quicksort arr cmp 0 (- len 1)))
	    arr))
      
      (let ((o (js-toobject %this this)))
	 (if (not (isa? this JsArray))
	     (array-sort this (get-compare comparefn))
	     (with-access::JsArray this (vec)
		(cond
		   ((>fx (vector-length vec) 0)
		    (vector-sort this (get-compare comparefn)))
		   ((=u32 (js-touint32 (js-get o 'length %this) %this) #u32:0)
		    this)
		   (else
		    (array-sort this (get-compare comparefn))))))))
   
   (js-bind! %this js-array-prototype 'sort
      :value (js-make-function %this array-prototype-sort 1 'sort
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; splice
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.12
   (define (array-prototype-splice this::obj start deletecount . items)
      
      (define (vector-splice this len actualstart actualdeletecount)
	 (with-access::JsArray this (vec)
	    (let* ((vlen (vector-length vec))
		   (litems (length items))
		   (rlen (+fx actualstart actualdeletecount))
		   (nlen (+fx len (-fx litems actualdeletecount)))
		   (cstart (+fx actualstart actualdeletecount))
		   (vres (make-vector actualdeletecount))
		   (res (js-vector->jsarray vres %this)))
	       ;; populate the result vector
	       (when (<fx actualstart vlen)
		  ;; from the inlined vector
		  (vector-copy! vres 0 vec actualstart
		     (minfx vlen (+fx actualstart actualdeletecount))))
 	       (when (>fx actualdeletecount (-fx vlen actualstart))
		  ;;  from the protype object
		  (let loop ((k (+fx actualstart (-fx vlen actualstart))))
		     (when (<fx k actualdeletecount)
			(let ((o (js-get this k %this)))
			   (vector-set! vres k o)
			   (loop (+fx k 1))))))
	       ;; modify the source array
	       (cond
		  ((>fx nlen len)
		   ;; enargle the vector if needed
		   (let ((tmp (make-vector nlen)))
		      (vector-copy! tmp 0 vec 0 actualstart)
		      (vector-copy! tmp (-fx nlen (-fx len cstart))
			 vec cstart len)
		      (set! vec tmp)))
		  ((<=fx nlen 0)
		   (set! vec '#()))
		  ((<fx nlen len)
		   ;; shift the vector
		   (vector-copy! vec (-fx nlen (-fx len cstart))
		      vec cstart len)))
	       ;; insert the new items
	       (let loop ((k actualstart)
			  (items items))
		  (if (pair? items)
		      (begin
			 (vector-set! vec k (car items))
			 (loop (+fx k 1) (cdr items)))
		      (js-put! this 'length nlen #f %this)))
	       res)))
      
      (define (array-splice arr len actualstart actualdeletecount)
	 (let* ((els (array-get-elements arr actualstart
			(+ actualstart actualdeletecount) %this))
		(res (js-vector->jsarray (list->vector els) %this))
		(rest (array-get-elements arr (+ actualstart actualdeletecount)
			 len %this)))
	    ;; add all the new elements
	    (for-each (lambda (el)
			 (js-put! arr actualstart el #f %this)
			 (set! actualstart (+ 1 actualstart)))
	       items)
	    (for-each (lambda (el)
			 (js-put! arr actualstart el #f %this)
			 (set! actualstart (+ 1 actualstart)))
	       rest)
	    ;; remove all the remaining elements
	    (let loop ()
	       (when (< actualstart len)
		  (js-delete! arr actualstart #f %this)
		  (set! len (- len 1))))
	    ;; shrink the vector
	    (js-put! arr 'length actualstart #f %this)
	    res))
      
      (let* ((o (js-toobject %this this))
	     (relstart (js-tointeger start %this))
	     (len (uint32->integer (js-touint32 (js-get o 'length %this) %this)))
	     (actualstart (if (< relstart 0) (max (+ len relstart) 0) (min relstart len)))
	     (actualdeletecount (min (max (js-tointeger deletecount %this) 0)
				   (- len actualstart))))
	 (if (not (isa? this JsArray))
	     (array-splice this len actualstart actualdeletecount)
	     (with-access::JsArray this (vec inline)
		(cond
		   (inline
		    (vector-splice this len
		       (->fixnum actualstart) (->fixnum actualdeletecount)))
		   (else
		    (array-splice this len actualstart actualdeletecount)))))))
   
   (js-bind! %this js-array-prototype 'splice
      :value (js-make-function %this array-prototype-splice 2 'splice
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; unshift
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.13
   (define (array-prototype-unshift this::obj . items)
      
      (define (vector-unshift arr len)
	 (with-access::JsArray arr (vec)
	    (let* ((litems (length items))
		   (vlen (js-array-vector-length arr %this))
		   (nvec (make-vector (+fx vlen litems)))
		   (nlen (+fx litems (uint32->fixnum len))))
	       (vector-copy! nvec litems vec)
	       (let ((i 0))
		  (for-each (lambda (el)
			       (vector-set! nvec i el)
			       (set! i (+fx i 1)))
		     items)
		  (set! vec nvec)
		  (js-put! arr 'length nlen #f %this)
		  nlen))))
      
      (define (array-unshift arr len)
	 (let ((rest (array-get-elements arr 0 len %this))
	       (i 0))
	    ;; add all the new elements
	    (for-each (lambda (el)
			 (js-put! arr i el #f %this)
			 (set! i (+fx 1 i)))
	       items)
	    (for-each (lambda (el)
			 (js-put! arr i el #f %this)
			 (set! i (+fx 1 i)))
	       rest)
	    ;; shrink the vector
	    (js-put! arr 'length i #f %this)
	    i))
      
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get o 'length %this) %this)))
	 (if (null? items)
	     (let ((nlen (uint32->integer len)))
		;; override the length as the conversion touin32 might have
		;; changed it
		(js-put! o 'length nlen #f %this)
		nlen)
	     (if (not (isa? this JsArray))
		 (array-unshift this (uint32->integer len))
		 (with-access::JsArray this (vec)
		    (cond
		       ((>fx (vector-length vec) 0)
			(vector-unshift this len))
		       (else
			(array-unshift this (uint32->integer len)))))))))
   
   (js-bind! %this js-array-prototype 'unshift
      :value (js-make-function %this array-prototype-unshift 1 'unshift
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; indexOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.14
   (define (array-prototype-indexof this::obj el . indx)
      
      (define (vector-indexof::int arr vec k::int len::int)
	 (let loop ((k k))
	    (cond
	       ((>=fx k len)
		-1)
	       ((js-strict-equal? (vector-ref-ur vec k) el)
		k)
	       ((and (eq? (vector-ref-ur vec k) (js-absent))
		     (let ((name (js-toname k %this)))
			(and (js-has-property arr name %this)
			     (js-strict-equal? (js-get arr name %this) el))))
		k)
	       (else
		(loop (+fx k 1))))))
      
      (define (array-indexof::int arr k::uint32 len::uint32)
	 (let ((k (uint32->integer k))
	       (len (uint32->integer len)))
	    (let loop ((k k))
	       (cond
		  ((>= k len)
		   -1)
		  ((let ((name (js-toname k %this)))
		      (and (js-has-property arr name %this)
			   (js-strict-equal? (js-get arr name %this) el)))
		   k)
		  (else
		   (loop (+ k 1)))))))
      
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get o 'length %this) %this)))
	 (if (=u32 len #u32:0)
	     -1
	     (let ((n (if (pair? indx) (js-tointeger (car indx) %this) 0)))
		(if (<=uint32 len n)
		    -1
		    (let ((k (if (< n 0)
				 (let ((absn (abs n)))
				    (if (<=uint32 len absn)
					#u32:0
					(-u32 len (->uint32 absn))))
				 (->uint32 n))))
		       (if (isa? o JsArray)
			   (with-access::JsArray o (vec)
			      (if (>fx (vector-length vec) 0)
				  (if (>uint32 k (vector-length vec))
				      -1
				      (vector-indexof o vec (uint32->fixnum k)
					 (vector-length vec)))
				  (array-indexof this k len)))
			   (array-indexof o k len))))))))
   
   (js-bind! %this js-array-prototype 'indexOf
      :value (js-make-function %this array-prototype-indexof 1 'indexOf
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; lastIndexOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.15
   (define (array-prototype-lastindexof this::obj el . indx)
      
      (define (vector-lastindexof::int arr vec k::int)
	 (let loop ((k k))
	    (cond
	       ((<fx k 0)
		-1)
	       ((js-strict-equal? (vector-ref vec k) el)
		k)
	       ((and (eq? (vector-ref vec k) (js-absent))
		     (let ((name (js-toname k %this)))
			(and (js-has-property arr name %this)
			     (js-strict-equal? (js-get arr name %this) el))))
		k)
	       (else
		(loop (-fx k 1))))))
      
      (define (array-lastindexof::int arr k)
	 (let loop ((k k))
	    (cond
	       ((< k 0)
		-1)
	       ((let ((name (js-toname k %this)))
		   (and (js-has-property arr name %this)
			(js-strict-equal? (js-get arr name %this) el)))
		k)
	       (else
		(loop (- k 1))))))
      
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get o 'length %this) %this)))
	 (if (=u32 len #u32:0)
	     -1
	     (let* ((n (if (pair? indx)
			   (js-tointeger (car indx) %this)
			   (uint32->integer (-u32 len #u32:1))))
		    (k (if (< n 0)
			   (let ((absn (abs n)))
			      (if (>uint32 len absn)
				  (uint32->integer (-u32 len (->uint32 (abs n))))
				  -1))
			   (if (>=uint32 (-u32 len #u32:1) n)
			       n
			       (uint32->integer (-u32 len #u32:1))))))
		(if (isa? o JsArray)
		    (with-access::JsArray o (vec)
		       (if (and (>fx (vector-length vec) 0) (> k 0))
			   (vector-lastindexof o vec
			      (minfx (->fixnum k)
				 (-fx (js-array-vector-length o %this) 1)))
			   (array-lastindexof o k)))
		    (array-lastindexof o k))))))
   
   (js-bind! %this js-array-prototype 'lastIndexOf
      :value (js-make-function %this array-prototype-lastindexof 1 'lastIndexOf
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; every
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.16
   (define (array-prototype-every this::obj proc t)
      
      (define (test-val proc t v i o)
	 (js-totest (js-call3 %this proc t v i o)))
      
      (define (vector-every o len::long proc t)
	 (with-access::JsArray o (vec)
	    (letrec ((loop-in-property
			(lambda (i)
			   (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
			      (cond
				 ((eq? pv (js-absent))
				  (loop-in-vec (+fx i 1)))
				 ((test-val proc t pv i o)
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
		(let* ((n (js-toname i %this))
		       (pv (js-get-property-value o o n %this)))
		   (cond
		      ((eq? pv (js-absent))
		       (loop (+ i 1)))
		      ((test-val proc t pv i o)
		       (loop (+ i 1)))
		      (else
		       #f)))
		#t)))
      
      (array-prototype-iterator %this this proc t array-every vector-every))
   
   (js-bind! %this js-array-prototype 'every
      :value (js-make-function %this array-prototype-every 1 'every
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.16
   (define (array-prototype-some this::obj proc t)
      
      (define (test-val proc t v i o)
	 (js-totest (js-call3 %this proc t v i o)))
      
      (define (vector-some o len::long proc t)
	 (with-access::JsArray o (vec)
	    (letrec ((loop-in-property
			(lambda (i)
			   (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
			      (cond
				 ((eq? pv (js-absent))
				  (loop-in-vec (+fx i 1)))
				 ((test-val proc t pv i o)
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
		(let ((pv (js-get-property-value o o (js-toname i %this) %this)))
		   (cond
		      ((eq? pv (js-absent))
		       (loop (+ i 1)))
		      ((test-val proc t pv i o)
		       #t)
		      (else
		       (loop (+ i 1)))))
		#f)))
      
      (array-prototype-iterator %this this proc t array-some vector-some))
   
   (js-bind! %this js-array-prototype 'some
      :value (js-make-function %this
		array-prototype-some 1 'some
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; forEach
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.18
   (define (array-prototype-foreach this::obj proc t)
      
      (define (vector-foreach o len::long proc t)
	 (with-access::JsArray o (vec)
	    (let loop ((i 0))
	       (when (<fx i len)
		  (if (=fx (vector-length vec) 0)
		      (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
			 (unless (eq? pv (js-absent))
			    (js-call3 %this proc t pv i o)))
		      (let ((v (vector-ref vec i)))
			 (if (eq? v (js-absent))
			     (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
				(unless (eq? pv (js-absent))
				   (js-call3 %this proc t pv i o)))
			     (js-call3 %this proc t v i o))))
		  (loop (+fx i 1))))))
      
      (define (array-foreach o len proc t)
	 (let loop ((i 0))
	    (when (< i len)
	       (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
		  (unless (eq? pv (js-absent))
		     (js-call3 %this proc t pv i o))
		  (loop (+ i 1))))))

      (array-prototype-iterator %this this proc t array-foreach vector-foreach)
      (js-undefined))
   
   (js-bind! %this js-array-prototype 'forEach
      :value (js-make-function %this array-prototype-foreach 1 'forEach
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; map 
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.19
   (define (make-array-prototype-map %this::JsGlobalObject)
      (lambda (this::obj proc t)
	 
	 (define (vector-map o len::long proc t)
	    (with-access::JsArray o (vec)
	       (let ((a (make-vector (minfx len (vector-length vec))
			   (js-absent))))
		  (let loop ((i 0))
		     (cond
			((>=fx i len)
			 (js-vector->jsarray a %this))
			((=fx (vector-length vec) 0)
			 (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
			    (unless (eq? pv (js-absent))
			       (vector-set! a i
				  (js-call3 %this proc t pv i o)))
			    (loop (+fx i 1))))
			(else
			 (let ((v (vector-ref vec i)))
			    (if (eq? v (js-absent))
				(let ((pv (js-get-property-value o o (js-toname i %this) %this)))
				   (unless (eq? pv (js-absent))
				      (vector-set! a i
					 (js-call3 %this proc t pv i o))))
				(vector-set! a i (js-call3 %this proc t v i o)))
			    (loop (+fx i 1)))))))))
	 
	 (define (array-map o len proc t)
	    (let ((a (js-array-construct %this (js-array-alloc js-array %this)
			len)))
	       (let loop ((i 0))
		  (if (< i len)
		      (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
			 (unless (eq? pv (js-absent))
			    (js-put! a i
			       (js-call3 %this proc t pv i o)
			       #f %this))
			 (loop (+ i 1)))
		      a))))

	 (array-prototype-iterator %this this proc t array-map vector-map)))
   
   (js-bind! %this js-array-prototype 'map
      :value (js-make-function %this
		(make-array-prototype-map %this) 1 'map
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; filter
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.20
   (define (array-prototype-filter this::obj proc t)
      
      (define (vector-filter o len::long proc t)
	 (with-access::JsArray o (vec)
	    (let ((a (make-vector (minfx len (vector-length vec))
			(js-absent))))
	       (let loop ((i 0)
			  (j 0))
		  (cond
		     ((>=fx i len)
		      (let ((res (js-vector->jsarray a %this)))
			 (js-put! res 'length j #f %this)
			 res))
		     ((=fx (vector-length vec) 0)
		      ;; the vector has been modified by the callback...
		      (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
			 (if (eq? pv (js-absent))
			     (loop (+fx i 1) j)
			     (let ((v pv))
				(if (js-totest (js-call3 %this proc t v i o))
				    (begin
				       (vector-set! a j v)
				       (loop (+fx i 1) (+fx j 1)))
				    (loop (+fx i 1) j))))))
		     (else
		      (let ((v (vector-ref vec i)))
			 (cond
			    ((eq? v (js-absent))
			     (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
				(if (eq? pv (js-absent))
				    (loop (+fx i 1) j)
				    (let ((v pv))
				       (if (js-totest (js-call3 %this proc t v i o))
					   (begin
					      (vector-set! a j v)
					      (loop (+fx i 1) (+fx j 1)))
					   (loop (+fx i 1) j))))))
			    ((js-totest (js-call3 %this proc t v i o))
			     (vector-set! a j v)
			     (loop (+fx i 1) (+fx j 1)))
			    (else
			     (loop (+fx i 1) j))))))))))
      
      (define (array-filter o len proc t)
	 (let ((a (js-vector->jsarray '#() %this)))
	    (let loop ((i 0)
		       (j 0))
	       (if (< i len)
		   (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
		      (if (eq? pv (js-absent))
			  (loop (+ i 1) j)
			  (let ((v pv)
				(nj (js-toname j %this)))
			     (if (js-totest (js-call3 %this proc t v i o))
				 (let ((newdesc (instantiate::JsValueDescriptor
						   (name nj)
						   (value v)
						   (writable #t)
						   (enumerable #t)
						   (configurable #t))))
				    ;; 6
				    (js-define-own-property a nj newdesc #f %this)
				    (loop (+ i 1) (+ j 1)))
				 (loop (+ i 1) j)))))
		   a))))

      (array-prototype-iterator %this this proc t array-filter vector-filter))
   
   (js-bind! %this js-array-prototype 'filter
      :value (js-make-function %this
		array-prototype-filter 1 'filter
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; reduce
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.21
   (define (array-prototype-reduce this::obj proc . init)
      
      (define (reduce/accumulator o len::uint32 i::uint32 accumulator)
	 (let loop ((i i)
		    (acc accumulator))
	    (if (<u32 i len)
		(let ((pv (js-get-property-value o o (js-toname i %this) %this)))
		   (if (eq? pv (js-absent))
		       (loop (+u32 i #u32:1) acc)
		       (let ((v pv))
			  (loop (+u32 i #u32:1)
			     (js-call4 %this proc (js-undefined) acc v
				(uint32->integer i) o)))))
		acc)))
      
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get o 'length %this) %this)))
	 (if (not (isa? proc JsFunction))
	     (js-raise-type-error %this "Not a procedure ~s" proc)
	     ;; find the accumulator init value
	     (if (null? init)
		 (let loop ((i #u32:0))
		    (if (<u32 i len)
			(let ((pv (js-get-property-value o o (js-toname i %this) %this)))
			   (if (eq? pv (js-absent))
			       (loop (+u32 i #u32:1))
			       (reduce/accumulator o len (+u32 i #u32:1) pv)))
			(js-raise-type-error %this
			   "reduce: cannot find accumulator ~s" this)))
		 (reduce/accumulator o len #u32:0 (car init))))))
   
   (js-bind! %this js-array-prototype 'reduce
      :value (js-make-function %this
		array-prototype-reduce 1 'reduce
		:prototype (js-undefined))
      :enumerable #f)
   
   ;; reduceRight
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.4.22
   (define (array-prototype-reduceright this::obj proc . init)
      
      (define (reduce/accumulator o len::uint32 i::uint32 accumulator)
	 (let loop ((i i)
		    (acc accumulator))
	    (let ((pv (js-get-property-value o o (js-toname i %this) %this)))
	       (if (<u32 i len)
		   (if (eq? pv (js-absent))
		       (loop (-u32 i #u32:1) acc)
		       (let* ((v pv)
			      (acc (js-call4 %this proc (js-undefined) acc v
				      (uint32->integer i) o)))
			  (loop (-u32 i #u32:1) acc)))
		   acc))))
      
      (let* ((o (js-toobject %this this))
	     (len::uint32 (js-touint32 (js-get o 'length %this) %this)))
	 (if (not (isa? proc JsFunction))
	     (js-raise-type-error %this "Not a procedure ~s" proc)
	     ;; find the accumulator init value
	     (if (null? init)
		 (let loop ((k (-u32 len #u32:1)))
		    (if (<u32 k len)
			(let ((pv (js-get-property-value o o (js-toname k %this) %this)))
			   (if (eq? pv (js-absent))
			       (loop (-u32 k #u32:1))
			       (let ((v pv))
				  (reduce/accumulator o len (-u32 k #u32:1) v))))
			(js-raise-type-error %this
			   "reduce: cannot find accumulator ~s" this)))
		 (reduce/accumulator o len (-u32 len #u32:1) (car init))))))
   
   (js-bind! %this js-array-prototype 'reduceRight
      :value (js-make-function %this array-prototype-reduceright 1 'reduceRight
		:prototype (js-undefined))
      :enumerable #f)

   ;; arrayComprehension
   ;; http://wiki.ecmascript.org/doku.php?id=harmony:array_comprehensions
   ;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Array_comprehensions
   (define (array-prototype-comprehension iterables::pair fun test _names
	      _astp _aste _astd)

      (define (prod2 l1 l2)
	 (append-map (lambda (a)
			(map (lambda (b) (list a b)) l2))
	    l1))

      (define (prod l)
	 (cond
	    ((null? l)
	     '())
	    ((null? (cdr l))
	     (car l))
	    ((null? (cddr l))
	     (prod2 (car l) (cadr l)))
	    (else
	     (let ((r (prod (cdr l))))
		(append-map (lambda (a) (map (lambda (r) (cons a r)) r))
		   (car l))))))

      
      (define (iterables->lists iterables)
	 
	 (define jsid (js-make-function %this (lambda (this n) n) 1 "id"))
	 
	 (map (lambda (el)
		 (if (isa? el JsArray)
		     (vector->list (jsarray->vector el %this))
		     (let* ((jsmap (js-get el 'map %this))
			    (arr (js-call1 %this jsmap el jsid)))
			(vector->list (jsarray->vector el %this)))))
	    iterables))

      (define (fast-comprehension)
	 (let ((this (car iterables)))
	    (if (eq? test #t)
		(let ((jsmap (js-get this 'map %this)))
		   (js-call1 %this jsmap this fun))
		(let* ((jsfilter (js-get this 'filter %this))
		       (arr (js-call1 %this jsfilter this test))
		       (jsmap (js-get arr 'map %this)))
		   (js-call1 %this jsmap arr fun)))))

      (if (null? (cdr iterables))
	  ;; fast path, only one array
	  (fast-comprehension)
	  ;; full path, multiple iterables
	  (let* ((this (car iterables))
		 (lsts (prod (iterables->lists iterables)))
		 (res (if (eq? test #t)
			  (map (lambda (l)
				  (js-apply %this fun this l))
			     lsts)
			  (filter-map (lambda (l)
					 (let ((t (js-apply %this test this l)))
					    (when (js-totest t)
					       (js-apply %this fun this l))))
			     lsts))))
	     (js-vector->jsarray (list->vector res) %this))))

   (js-bind! %this js-array-prototype 'comprehension
      :value (js-make-function %this array-prototype-comprehension 6
		'comprehension
		:prototype (js-undefined))
      :enumerable #f))

;*---------------------------------------------------------------------*/
;*    %js-array ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.1       */
;*---------------------------------------------------------------------*/
(define (%js-array %this::JsGlobalObject)
   (lambda (this . items)
      (with-access::JsGlobalObject %this (js-array)
	 (apply js-array-construct %this (js-array-alloc js-array %this)
	    items))))

;*---------------------------------------------------------------------*/
;*    js-array-alloc ...                                               */
;*---------------------------------------------------------------------*/
(define (js-array-alloc constructor::JsFunction %this)
   (instantiate::JsArray
      (cmap #f)
      (__proto__ (js-get constructor 'prototype %this))))

;*---------------------------------------------------------------------*/
;*    js-array-construct ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.2.1     */
;*---------------------------------------------------------------------*/
(define (js-array-construct %this::JsGlobalObject this::JsArray . items)
   
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
	     ((not (=uint32 (js-touint32 i %this) i))
	      (js-raise-range-error %this "index out of range ~a" i))
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
(define (js-vector->jsarray::JsArray vec::vector %this::JsGlobalObject)
   (let ((lenproperty (instantiate::JsValueDescriptor
			 (name 'length)
			 (value (vector-length vec))
			 (configurable #f)
			 (writable #t))))
      (with-access::JsGlobalObject %this (js-array js-array-prototype)
	 (instantiate::JsArray
	    (extensible #t)
	    (__proto__ js-array-prototype)
	    (properties (list lenproperty))
	    (vec vec)))))

;*---------------------------------------------------------------------*/
;*    js-properties-name ::JsArray ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-properties-name obj::JsArray enump %this)
   (with-access::JsArray obj (vec)
      (let ((len::uint32 (js-touint32 (js-get obj 'length %this) %this)))
	 (let loop ((i (-fx (uint32->fixnum (minu32 len (u32vlen vec))) 1))
		    (acc '()))
	    (if (=fx i -1)
		(vector-append (apply vector acc) (call-next-method))
		(let ((v (vector-ref vec i)))
		   (if (eq? v (js-absent))
		       (loop (-fx i 1) acc)
		       (loop (-fx i 1) (cons (js-integer->jsstring i) acc)))))))))

;*---------------------------------------------------------------------*/
;*    js-array-vector-properties ...                                   */
;*    -------------------------------------------------------------    */
;*    Returns the subset of the array properties which are stored      */
;*    in its inline vector.                                            */
;*---------------------------------------------------------------------*/
(define (js-array-vector-properties obj::JsArray %this)
   (with-access::JsArray obj (vec)
      (let ((len::uint32 (js-touint32 (js-get obj 'length %this) %this)))
	 (let loop ((i (-fx (uint32->fixnum (minu32 len (u32vlen vec))) 1))
		    (acc '()))
	    (if (=fx i -1)
		acc
		(let ((v (vector-ref vec i)))
		   (if (eq? v (js-absent))
		       (loop (-fx i 1) acc)
		       (let ((desc (instantiate::JsValueDescriptor
				      (name (js-toname i %this))
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
(define-method (js-has-property o::JsArray p %this)
   (with-access::JsArray o (vec)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((<uint32 i (vector-length vec))
	     ;; the length is always an integer in the range [0..2^32-1]
	     (let ((len::uint32 (js-touint32 (js-get o 'length %this) %this)))
		(if (or (>=u32 i len) (eq? (u32vref vec i) (js-absent)))
		    (call-next-method)
		    #t)))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get-own-property ...                                          */
;*---------------------------------------------------------------------*/
(define-method (js-get-own-property o::JsArray p %this::JsGlobalObject)
   (with-access::JsArray o (vec frozen)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((<uint32 i (vector-length vec))
	     ;; the length is always an integer in the range [0..2^32-1]
	     (let ((len::uint32 (js-touint32 (js-get o 'length %this) %this)))
		(if (or (>=u32 i len) (eq? (u32vref vec i) (js-absent)))
		    (call-next-method)
		    (instantiate::JsValueDescriptor
		       (name (js-toname p %this))
		       (value (u32vref vec i))
		       (enumerable #t)
		       (writable (not frozen))
		       (configurable (not frozen))))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get-property-value ::JsArray ...                              */
;*    -------------------------------------------------------------    */
;*    This method is optional. It could be removed without changing    */
;*    the programs behaviors. It merely optimizes access to strings.   */
;*---------------------------------------------------------------------*/
(define-method (js-get-property-value o::JsArray base p %this)
   (with-access::JsArray o (vec)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((<uint32 i (vector-length vec))
	     ;; the length is always an integer in the range [0..2^32-1]
	     (let ((len::uint32 (js-touint32 (js-get o 'length %this) %this)))
		(if (or (>=u32 i len) (eq? (u32vref vec i) (js-absent)))
		    (call-next-method)
		    (u32vref vec i))))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsArray ...                                             */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsArray p %this)
   (with-access::JsArray o (vec properties)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((and (vector? vec)
		  (<uint32 i (vector-length vec))
		  (<uint32 i (js-get o 'length %this)))
	     (let ((v (u32vref vec i)))
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
(define-method (js-put! o::JsArray p v throw %this)
   
   (define (js-put-array! o::JsArray p::obj v)
      (if (not (js-can-put o p %this))
	  ;; 1
	  (js-undefined)
	  (let ((owndesc (js-get-own-property o p %this)))
	     ;; 2
	     (if (js-is-data-descriptor? owndesc)
		 ;; 3
		 (if (eq? p 'length)
		     (let ((newdesc (duplicate::JsValueDescriptor owndesc
				       (value v))))
			(js-define-own-property o p newdesc throw %this))
		     (with-access::JsValueDescriptor owndesc ((valuedesc value))			
			(set! valuedesc v)
			(js-define-own-property o p owndesc throw %this)))
		 (let ((desc (js-get-property o p %this)))
		    ;; 4
		    (if (js-is-accessor-descriptor? desc)
			;; 5
			(with-access::JsAccessorDescriptor desc ((setter set))
			   (if (isa? setter JsFunction)
			       (js-call1 %this setter o v)
			       (js-undefined)))
			(let ((newdesc (instantiate::JsValueDescriptor
					  (name p)
					  (value v)
					  (writable #t)
					  (enumerable #t)
					  (configurable #t))))
			   ;; 6
			   (js-define-own-property o p newdesc throw %this)))))
	     v)))

   (with-access::JsArray o (vec)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (js-put-array! o (js-toname p %this) v))
	    ((and (<u32 i (fixnum->uint32 (vector-length vec)))
		  (<uint32 i (js-get o 'length %this)))
	     (vector-set-ur! vec (uint32->fixnum i) v)
	     v)
	    (else
	     (js-put-array! o (js-toname p %this) v))))))

;*---------------------------------------------------------------------*/
;*    js-delete! ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (js-delete! o::JsArray p throw %this)
   (with-access::JsArray o (vec properties frozen)
      (let ((i::uint32 (js-toindex p)))
	 (cond
	    ((not (js-isindex? i))
	     (call-next-method))
	    ((<uint32 i (vector-length vec))
	     (unless frozen
		(u32vset! vec i (js-absent))
		#t))
	    (else
	     (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    js-define-own-property ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.5.1     */
;*---------------------------------------------------------------------*/
(define-method (js-define-own-property a::JsArray p desc throw %this::JsGlobalObject)
   
   (define rejected #f)

   (define (reject fmt)
      (if throw
	  (js-raise-type-error %this fmt p)
	  (set! rejected #t)))

   (define (newwritable! oldlendesc newlendesc)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.4.5.1
      (or (not (isa? newlendesc JsValueDescriptor))
	  (with-access::JsValueDescriptor newlendesc (writable)
	     (let ((r (not (eq? writable #f))))
		(set! writable #t)
		r))))

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
      
   (define (delete-out-of-range! a newlendesc newwritable oldlen newlen::uint32)
      ;; delete all the properties that are at an index greater that the
      ;; new length
      ;; the cannonical implementation would be as follows:
      ;;    (let while ()
      ;;      ;; 3.l
      ;;      (when (< newlen oldlen)
      ;;         (set! oldlen (- oldlen 1))
      ;;         (let ((r (js-get-delete! a %this oldlen #f)))
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
	     (let ((stop (uint32->fixnum newlen)))
		(let loop ((i (-fx (vector-length vec) 1)))
		   (when (>=fx i stop)
		      ;; 3.l
		      (vector-set-ur! vec i  (js-absent))
		      (loop (-fx i 1)))))
	     (for-each (lambda (name)
			  (unless rejected
			     (let ((num (js-string->number (symbol->string! name))))
				(when (and num (<=uint32 newlen num))
				   (let ((r (js-delete! a name #f %this)))
				      (unless r
					 ;; 3.l.iii
					 (with-access::JsValueDescriptor newlendesc (value writable)
					    ;; 3.l.iii.1
					    (set! value (+ num 1))
					    ;; 3.l.iii.2
					    (set! writable newwritable)
					    ;; 3.l.iii.3
					    (js-define-own-property% a 'length
					       newlendesc #f %this)
					    ;; 3.l.iii.4
					    (reject (format "Cannot delete element ~a" num)))))))))
		(sort (lambda (n1 n2)
			 (string>? (symbol->string! n1) (symbol->string! n2)))
		   (js-array-property-names a))))))
   
   (define (define-own-property/length oldlendesc)
      (with-access::JsValueDescriptor oldlendesc (value (owritable writable))
	 (let ((oldlen value))
	    (if (not (isa? desc JsValueDescriptor))
		;; 3.a
		(call-next-method)
		(with-access::JsValueDescriptor desc (value)
		   (let ((newlendesc (duplicate::JsValueDescriptor desc))
			 (newlen (js-touint32 value %this))
			 (nvalue (js-tonumber value %this)))
		      (unless (=uint32 newlen nvalue)
			 ;; 3.d
			 (js-raise-range-error %this
			    "Illegal length: ~s" (js-tostring value %this)))
		      (with-access::JsValueDescriptor newlendesc (value)
			 ;; 3.e
			 (set! value (uint32->integer newlen)))
		      (if (>=uint32 newlen oldlen)
			  ;; 3.f
			  (js-define-own-property% a 'length
			     newlendesc throw %this)
			  (if (not owritable)
			      ;; 3.g
			      (reject "property read-only \"~a\"")
			      ;; 3.h
			      (let* ((deferredwritable (newwritable!
						     oldlendesc newlendesc))
				     (desc (js-define-own-property% a 'length
					      newlendesc throw %this)))
				 (if (not desc)
				     ;; 3.k
				     desc
				     (begin
					;; 3.l
					(delete-out-of-range!
					   a newlendesc deferredwritable
					   oldlen newlen)
					(unless rejected
					   ;; 3.m
					   (if (not deferredwritable)
					       ;; 3.m.i
					       (js-define-own-property% a 'length
						  (instantiate::JsDataDescriptor
						     (name 'length)
						     (writable #f))
						  #f %this)
					       ;; 3.n
					       #t)))))))))))))

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
		 (eq? configurable #t)))))
   
   (define (uninline-array! arr::JsArray)
      ;; this function switch from a fast inlined array representation
      ;; to a slow inefficient object representation
      (with-access::JsArray arr (vec properties cmap inline)
	 (set! inline #f)
	 (when (>fx (vector-length vec) 0)
	    (set! cmap #f)
	    (set! properties
	       (append (js-array-vector-properties arr %this) properties))
	    (set! vec '#()))
	 arr))

   (define (js-define-own-property-array a p::uint32 desc throw)
      (with-access::JsArray a (vec extensible)
	 (cond
	    ;; MS CARE 26 sep 2014: I'm not sure if impossible (not extensible)
	    ;; is correct, let's remove this warning if all the test262 pass
	    ((and (eq? (u32vref vec p) (js-absent)) (not extensible))
	     (let ((r (js-define-own-property% a (js-toname p %this) desc #f %this)))
		(unless (inline-value-descriptor? desc)
		   (uninline-array! a))
		r))
	    ((isa? desc JsValueDescriptor)
	     (with-access::JsValueDescriptor desc (value)
		(if (inline-value-descriptor? desc)
		    (u32vset! vec p value)
		    (begin
		       (uninline-array! a)
		       (js-define-own-property% a (js-toname p %this) desc #f %this)))))
	    ((isa? desc JsAccessorDescriptor)
	     (uninline-array! a)
	     (js-define-own-property% a (js-toname p %this) desc #f %this))
	    (else
	     (uninline-array! a)
	     (js-define-own-property% a (js-toname p %this) desc #f %this)))))

   (define max-expandable-array-size::uint32
      (bit-lshu32 #u32:1 20))
   (define max-expandable-array-size/2::uint32
      (/u32 max-expandable-array-size #u32:2))
   (define max-expandable-array-size/8::uint32
      (/u32 max-expandable-array-size #u32:8))
   
  (define (expandable-array vec::JsArray index::uint32 len::uint32)
     ;; Check is an inline array can be expanded. To take the descision,
     ;; a basic heuristic (to be improved) to decide when an array
     ;; should be expanded
     (with-access::JsArray vec (inline)
	(when inline
	   ;; the vector is inlined, make the real check
	   (when (<u32 index max-expandable-array-size)
	      (cond
		 ((=u32 index #u32:0)
		  #u32:2)
		 ((<u32 (*u32 #u32:2 index)
		     (/u32  max-expandable-array-size #u32:2))
		  (*u32 index #u32:2))
		 ((<u32 (+u32 max-expandable-array-size/8 index)
		     max-expandable-array-size)
		  (+u32 index max-expandable-array-size/8))
		 (else
		  max-expandable-array-size))))))

  (let ((oldlendesc (js-get-own-property a 'length %this)))
     (if (eq? p 'length)
	 ;; 3
	 (define-own-property/length oldlendesc)
	 (let ((index::uint32 (js-toindex p)))
	    (if (js-isindex? index)
		;; 4
		(with-access::JsValueDescriptor oldlendesc ((oldlen value) writable)
		   (with-access::JsArray a (vec)
		      (if (and (>=uint32 index oldlen) (not (eq? writable #t)))
			  ;; 4.b
			  (reject "wrong index ~a")
			  ;; 4.c
			  (let ((s (cond
				      ((<u32 index (u32vlen vec))
				       ;; fast access, inline vector
				       (js-define-own-property-array
					  a index desc #f))
				      ((expandable-array a index (u32vlen vec))
				       ;; expand the vector
				       =>
				       (lambda (len)
					  (let ((olen (vector-length vec))
						(nlen (uint32->fixnum len)))
					     (set! vec (copy-vector vec nlen))
					     (vector-fill! vec (js-absent) olen ))
					  (js-define-own-property-array
					     a index desc #f)))
				      (else
				       ;; slow access
				       (uninline-array! a)
				       (js-define-own-property%
					  a (js-toname p %this) desc #f
					  %this)))))
			     (cond
				((not s)
				 (reject "wrong index \"~a\""))
				((>=uint32 index oldlen)
				 ;; 4.e.i,
				 (set! oldlen
				    (uint32->integer (+u32 index #u32:1)))
				 ;; 4.e.ii
				 (js-define-own-property a 'length
				    oldlendesc #f %this)
				 ;; 4.f
				 #t))))))
		;; 5
		(call-next-method))))))
   
;*---------------------------------------------------------------------*/
;*    array-get-elements ...                                           */
;*---------------------------------------------------------------------*/
(define (array-get-elements arr start len %this)
   (let loop ((i (- len 1))
	      (acc '()))
      (if (< i start)
	  acc
	  (loop (- i 1) (cons (js-get arr i %this) acc)))))

;*---------------------------------------------------------------------*/
;*    array-prototype-iterator ...                                     */
;*---------------------------------------------------------------------*/
(define (array-prototype-iterator %this::JsGlobalObject
	   this proc t array-iterator vector-iterator)
   (let* ((o (js-toobject %this this))
	  (len::uint32 (js-touint32 (js-get o 'length %this) %this)))
      (if (not (isa? proc JsFunction))
	  (js-raise-type-error %this "Not a procedure ~s" proc)
	  (if (not (isa? o JsArray))
	      (array-iterator o (uint32->integer len) proc t)
	      (with-access::JsArray o (vec)
		 (if (and (>fx (vector-length vec) 0)
			  (<=u32 len (fixnum->uint32 (vector-length vec))))
		     (vector-iterator o (uint32->fixnum len) proc t)
		     (array-iterator o (uint32->integer len) proc t)))))))

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
(define-method (js-for-in o::JsArray proc %this)
   (with-access::JsArray o (vec)
      (if (>fx (vector-length vec) 0)
	  (let ((len (js-array-vector-length o %this)))
	     (let loop ((i 0))
		(if (<fx i len)
		    (begin
		       (proc (js-integer->jsstring i))
		       (loop (+fx i 1)))
		    (call-next-method))))
	  (call-next-method))))
  
;*---------------------------------------------------------------------*/
;*    js-array-vector-length ...                                       */
;*---------------------------------------------------------------------*/
(define (js-array-vector-length o::JsArray %this::JsGlobalObject)
   (with-access::JsArray o (vec)
      (minfx (vector-length vec)
	 (uint32->fixnum (js-touint32 (js-get o 'length %this) %this)))))

;*---------------------------------------------------------------------*/
;*    js-array-comprehension ...                                       */
;*---------------------------------------------------------------------*/
(define (js-array-comprehension %this iterables fun test _names _astp _aste _astd)
   (let ((jscomp (js-get (car iterables) 'comprehension %this))
	 (len (length iterables)))
      (js-call6 %this jscomp iterables
	 (js-make-function %this fun len "comprehension-expr")
	 (if (eq? test #t)
	     #t
	     (js-make-function %this test len "comprehension-test"))
	 _names _astp _aste _astd)))
	
;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
