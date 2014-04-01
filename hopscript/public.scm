;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/public.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:10:39 2013                          */
;*    Last change :  Sat Mar 22 16:25:14 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Public (i.e., exported outside the lib) hopscript functions      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_public
   
   (include "hopscript.sch")
   
   (library js2scheme)
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_error
	   __hopscript_string
	   __hopscript_boolean
	   __hopscript_number
	   __hopscript_property
	   __hopscript_private)
   
   (export (js-new f . args)
	   (js-new0 f)
	   (js-new1 f a0)
	   (js-new2 f a0 a1)
	   (js-new3 f a0 a1 a2)
	   (js-new4 f a0 a1 a2 a3)

	   (js-object-alloc ::JsFunction)

	   (js-apply fun::obj this args)
	   
	   (js-call0 fun::obj this)
	   (js-call1 fun::obj this a0)
	   (js-call2 fun::obj this a0 a1)
	   (js-call3 fun::obj this a0 a1 a2)
	   (js-call4 fun::obj this a0 a1 a2 a3)
	   (js-call5 fun::obj this a0 a1 a2 a3 a4)
	   (js-call6 fun::obj this a0 a1 a2 a3 a4 a5)
	   (js-call7 fun::obj this a0 a1 a2 a3 a4 a5 a6)
	   (js-call8 fun::obj this a0 a1 a2 a3 a4 a5 a6 a7)
	   (js-calln fun::obj this . args)

	   (js-call0/debug loc fun::obj this)
	   (js-call1/debug loc fun::obj this a0)
	   (js-call2/debug loc fun::obj this a0 a1)
	   (js-call3/debug loc fun::obj this a0 a1 a2)
	   (js-call4/debug loc fun::obj this a0 a1 a2 a3)
	   (js-call5/debug loc fun::obj this a0 a1 a2 a3 a4)
	   (js-call6/debug loc fun::obj this a0 a1 a2 a3 a4 a5)
	   (js-call7/debug loc fun::obj this a0 a1 a2 a3 a4 a5 a6)
	   (js-call8/debug loc fun::obj this a0 a1 a2 a3 a4 a5 a6 a7)
	   (js-calln/debug loc fun::obj this . args)

	   (js-instanceof?::bool v f)
	   (js-in?::bool f obj)
	   (inline js-totest::bool ::obj)
	   (js-toboolean::bool ::obj)
	   (generic js-tonumber ::obj)
	   (js-touint16::uint16 ::obj)
	   (js-touint32::uint32 ::obj)
	   (js-toint32::int32 ::obj)
	   (js-tostring::bstring obj)
	   (js-toobject::JsObject ::obj)

	   (inline js-equal? ::obj ::obj)
	   (js-equality? ::obj ::obj)
	   (inline js-strict-equal? ::obj ::obj)
	   (js-eq? ::obj ::obj)

	   (make-pcache ::int)
	   (inline pcache-ref ::vector ::int)

	   (%js-eval-strict ::bstring)
	   (js-raise ::JsError)))

;*---------------------------------------------------------------------*/
;*    js-new ...                                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.2.2       */
;*---------------------------------------------------------------------*/
(define (js-new f . args)
   (if (isa? f JsFunction)
       (with-access::JsFunction f (construct alloc)
	  (let ((o (alloc f)))
	     (let ((r (js-apply% construct (procedure-arity construct)
			 o args)))
		(if (isa? r JsObject) r o))))
       (js-raise-type-error "new: object is not a function ~s" f)))

;*---------------------------------------------------------------------*/
;*    js-new-return ...                                                */
;*---------------------------------------------------------------------*/
(define (js-new-return f r o)
   (with-access::JsFunction f (constrsize)
      (if (isa? r JsObject)
	  (with-access::JsObject r (elements)
	     (when (vector? elements)
		(set! constrsize (vector-length elements)))
	     r)
	  (with-access::JsObject o (elements)
	     (when (vector? elements)
		(set! constrsize (vector-length elements)))
	     o))))

;*---------------------------------------------------------------------*/
;*    js-new0 ...                                                      */
;*---------------------------------------------------------------------*/
(define (js-new0 f)
   (if (isa? f JsFunction)
       (with-access::JsFunction f (construct alloc name constrsize)
	  (let ((o (alloc f)))
	     (let ((r (js-call0% construct (procedure-arity construct)
			 o)))
		(js-new-return f r o))))
       (js-raise-type-error "new: object is not a function ~s" f)))

(define (js-new1 f a0)
   (if (isa? f JsFunction)
       (with-access::JsFunction f (construct alloc)
	  (let ((o (alloc f)))
	     (let ((r (js-call1% construct (procedure-arity construct)
			 o a0)))
		(js-new-return f r o))))
       (js-raise-type-error "new: object is not a function ~s" f)))

(define (js-new2 f a0 a1)
   (if (isa? f JsFunction)
       (with-access::JsFunction f (construct alloc)
	  (let ((o (alloc f)))
	     (let ((r (js-call2% construct (procedure-arity construct)
			 o a0 a1)))
		(js-new-return f r o))))
       (js-raise-type-error "new: object is not a function ~s" f)))

(define (js-new3 f a0 a1 a2)
   (if (isa? f JsFunction)
       (with-access::JsFunction f (construct alloc name)
	  (let ((o (alloc f)))
	     (let ((r (js-call3% construct (procedure-arity construct)
			 o a0 a1 a2)))
		(js-new-return f r o))))
       (js-raise-type-error "new: object is not a function ~s" f)))

(define (js-new4 f a0 a1 a2 a3)
   (if (isa? f JsFunction)
       (with-access::JsFunction f (construct alloc)
	  (let ((o (alloc f)))
	     (let ((r (js-call4% construct (procedure-arity construct)
			 o a0 a1 a2 a3)))
		(js-new-return f r o))))
       (js-raise-type-error "new: object is not a function ~s" f)))

;*---------------------------------------------------------------------*/
;*    js-object-alloc ...                                              */
;*---------------------------------------------------------------------*/
(define (js-object-alloc constructor::JsFunction)
   (with-access::JsFunction constructor (constrsize constrmap)
      (let ((cproto (js-get constructor 'prototype)))
	 (instantiate::JsObject
	    (cmap constrmap)
	    (elements (make-vector constrsize (js-undefined)))
	    (__proto__ (if (isa? cproto JsObject) cproto js-object-prototype))))))

;*---------------------------------------------------------------------*/
;*    js-apply ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-apply fun obj args)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error"apply: argument not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-apply% procedure (procedure-arity procedure) obj args))))

;*---------------------------------------------------------------------*/
;*    js-apply% ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-apply% proc::procedure arity::int obj args)
   (let ((len (+fx 1 (length args))))
      (if (<fx arity 0)
	  (let ((-arity (-fx (negfx arity) 1)))
	     (if (<=fx -arity len)
		 (apply proc obj args)
		 (let ((rest (make-list (-fx -arity len) (js-undefined))))
		    (apply proc obj (append args rest)))))
	  (cond
	     ((=fx arity len)
	      (apply proc obj args))
	     ((<fx arity len)
	      (apply proc obj (take args (-fx arity 1))))
	     (else
	      (let ((rest (make-list (-fx arity len) (js-undefined))))
		 (apply proc obj (append args rest))))))))

;*---------------------------------------------------------------------*/
;*    gen-calln ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (gen-calln . args)
   (let ((n (+fx 1 (length args))))
      `(case arity
	  ((-1)
	   (proc this ,@args))
	  ,@(map (lambda (i)
		    `((,i) 
		      ,(cond
			  ((=fx i n)
			   `(proc this ,@args))
			  ((<fx i n)
			   `(proc this ,@(take args (-fx i 1))))
			  (else
			   `(proc this ,@args
			       ,@(make-list (-fx i n)
				    '(js-undefined)))))))
	     (iota 8 1))
	  (else
	   (if (<fx arity 0)
	       (let ((min (-fx (negfx arity) 1)))
		  (apply proc this ,@args
		     (make-list (-fx min ,n) (js-undefined))))
	       (apply proc this ,@args
		  (make-list (-fx arity 8) (js-undefined))))))))

;*---------------------------------------------------------------------*/
;*    js-calln ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-call0% proc::procedure arity::int this)
   (gen-calln))
(define (js-call1% proc::procedure arity::int this a0)
   (gen-calln a0))
(define (js-call2% proc::procedure arity::int this a0 a1)
   (gen-calln a0 a1))
(define (js-call3% proc::procedure arity::int this a0 a1 a2)
   (gen-calln a0 a1 a2))
(define (js-call4% proc::procedure arity::int this a0 a1 a2 a3)
   (gen-calln a0 a1 a2 a3))
(define (js-call5% proc::procedure arity::int this a0 a1 a2 a3 a4)
   (gen-calln a0 a1 a2 a3 a4))
(define (js-call6% proc::procedure arity::int this a0 a1 a2 a3 a4 a5)
   (gen-calln a0 a1 a2 a3 a4 a5))
(define (js-call7% proc::procedure arity::int this a0 a1 a2 a3 a4 a5 a6)
   (gen-calln a0 a1 a2 a3 a4 a5 a6))
(define (js-call8% proc::procedure arity::int this a0 a1 a2 a3 a4 a5 a6 a7)
   (gen-calln a0 a1 a2 a3 a4 a5 a6 a7))

(define (js-call0 fun this)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call0% procedure (procedure-arity procedure)
	     this))))
(define (js-call1 fun this a0)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call1% procedure (procedure-arity procedure)
	     this a0))))
(define (js-call2 fun this a0 a1)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call2% procedure (procedure-arity procedure)
	     this a0 a1))))
(define (js-call3 fun this a0 a1 a2)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call3% procedure (procedure-arity procedure)
	     this a0 a1 a2))))
(define (js-call4 fun this a0 a1 a2 a3)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call4% procedure (procedure-arity procedure)
	     this a0 a1 a2 a3))))
(define (js-call5 fun this a0 a1 a2 a3 a4)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call5% procedure (procedure-arity procedure)
	     this a0 a1 a2 a3 a4))))
(define (js-call6 fun this a0 a1 a2 a3 a4 a5)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call6% procedure (procedure-arity procedure)
	     this a0 a1 a2 a3 a4 a5))))
(define (js-call7 fun this a0 a1 a2 a3 a4 a5 a6)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call7% procedure (procedure-arity procedure)
	     this a0 a1 a2 a3 a4 a5 a6))))
(define (js-call8 fun this a0 a1 a2 a3 a4 a5 a6 a7)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure arity)
	  (js-call8% procedure (procedure-arity procedure)
	     this a0 a1 a2 a3 a4 a5 a6 a7))))

(define (js-calln fun this . args)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (let ((arity (procedure-arity procedure)))
	     (if (<fx arity 0)
		 (apply procedure this args)
		 (let ((len (length args)))
		    (cond
		       ((=fx arity len)
			(apply procedure this args))
		       ((<fx arity len)
			(apply procedure this (take args (-fx arity len))))
		       (else
			(apply procedure this
			   (append args
			      (make-list (-fx arity len))
			      (js-undefined)))))))))))

(define (js-call0/debug loc fun this)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc loc "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call0% procedure (procedure-arity procedure)
	     this))))
(define (js-call1/debug loc fun this a0)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc loc "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call1% procedure (procedure-arity procedure)
	     this a0))))
(define (js-call2/debug loc fun this a0 a1)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc loc "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call2% procedure (procedure-arity procedure)
	     this a0 a1))))
(define (js-call3/debug loc fun this a0 a1 a2)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc loc "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call3% procedure (procedure-arity procedure)
	     this a0 a1 a2))))
(define (js-call4/debug loc fun this a0 a1 a2 a3)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc loc "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call4% procedure (procedure-arity procedure)
	     this a0 a1 a2 a3))))
(define (js-call5/debug loc fun this a0 a1 a2 a3 a4)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc loc "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call5% procedure (procedure-arity procedure)
	     this a0 a1 a2 a3 a4))))
(define (js-call6/debug loc fun this a0 a1 a2 a3 a4 a5)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc loc "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call6% procedure (procedure-arity procedure)
	     this a0 a1 a2 a3 a4 a5))))
(define (js-call7/debug loc fun this a0 a1 a2 a3 a4 a5 a6)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc loc "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call7% procedure (procedure-arity procedure)
	     this a0 a1 a2 a3 a4 a5 a6))))
(define (js-call8/debug loc fun this a0 a1 a2 a3 a4 a5 a6 a7)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc loc "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure arity)
	  (js-call8% procedure (procedure-arity procedure)
	     this a0 a1 a2 a3 a4 a5 a6 a7))))

(define (js-calln/debug loc fun this . args)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc loc "call: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (let ((arity (procedure-arity procedure)))
	     (if (<fx arity 0)
		 (apply procedure this args)
		 (let ((len (length args)))
		    (cond
		       ((=fx arity len)
			(apply procedure this args))
		       ((<fx arity len)
			(apply procedure this (take args (-fx arity len))))
		       (else
			(apply procedure this
			   (append args
			      (make-list (-fx arity len))
			      (js-undefined)))))))))))

;*---------------------------------------------------------------------*/
;*    js-instanceof? ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.6       */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.5.3   */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.5.3     */
;*---------------------------------------------------------------------*/
(define (js-instanceof? v f)
   (if (not (isa? f JsFunction))
       (js-raise-type-error "instanceof: not a function ~s" f)
       (when (isa? v JsObject)
	  (let ((o (js-getvalue f f 'prototype #f)))
	     (if (not (isa? o JsObject))
		 (js-raise-type-error "instanceof: no prototype ~s" v)
		 (let loop ((v v))
		    (with-access::JsObject v ((v __proto__))
		       (cond
			  ((eq? o v) #t)
			  ((eq? v (js-null)) #f)
			  (else (loop v))))))))))

;*---------------------------------------------------------------------*/
;*    js-in? ...                                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.7       */
;*---------------------------------------------------------------------*/
(define (js-in? field obj)
   (if (not (isa? obj JsObject))
       (js-raise-type-error "in: not a object ~s" obj)
       (js-has-property obj (js-toname field))))

;*---------------------------------------------------------------------*/
;*    js-totest ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.5         */
;*---------------------------------------------------------------------*/
(define-inline (js-totest obj)
   (if (boolean? obj) obj (js-toboolean obj)))
      
;*---------------------------------------------------------------------*/
;*    js-toboolean ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.2          */
;*---------------------------------------------------------------------*/
(define (js-toboolean obj)
   (cond
      ((boolean? obj) obj)
      ((eq? obj (js-undefined)) #f)
      ((eq? obj (js-null)) #f)
      ((number? obj) (not (or (= obj 0) (and (flonum? obj) (nanfl? obj)))))
      ((string? obj) (>fx (string-length obj) 0))
      (else #t)))

;*---------------------------------------------------------------------*/
;*    js-tonumber ::obj ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-generic (js-tonumber obj)
   (cond
      ((number? obj)
       obj)
      ((eq? obj (js-undefined))
       +nan.0)
      ((eq? obj (js-null))
       0)
      ((eq? obj #t)
       1)
      ((eq? obj #f)
       0)
      ((string? obj)
       (let ((str (trim-whitespaces+ obj :left #t :right #t :plus #t)))
	  (cond
	     ((string=? str "Infinity")
	      +inf.0)
	     ((string=? str "+Infinity")
	      +inf.0)
	     ((string=? str "-Infinity")
	      -inf.0)
	     ((string=? str "NaN")
	      +nan.0)
	     ((string-null? str)
	      0)
	     ((or (string-prefix? "0x" str) (string-prefix? "0X" str))
	      (js-parseint str 16 #t))
	     ((string-index str "eE.")
	      (js-parsefloat str #t))
	     (else
	      (js-parseint str 10 #t)))))
      ((symbol? obj)
       (js-tonumber (symbol->string! obj)))
      (else
       (bigloo-type-error "toNumber" "JsObject" obj))))
   
;*---------------------------------------------------------------------*/
;*    js-touint16 ::obj ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.7          */
;*---------------------------------------------------------------------*/
(define (js-touint16::uint16 obj)
   
   (define 2^16 (exptfl 2. 16.))
   
   (define (uint32->uint16 o)
      (fixnum->uint16 (uint32->fixnum o)))
   
   (define (positive-double->uint16::uint16 obj::double)
      (uint32->uint16
	 (if (<fl obj 2^16)
	     (flonum->uint32 obj)
	     (flonum->uint32 (remainderfl obj 2^16)))))
   
   (define (double->uint16::uint16 obj::double)
      (cond
	 ((or (= obj +inf.0) (= obj -inf.0) (not (= obj obj)))
	  #u16:0)
	 ((<fl obj 0.)
	  (positive-double->uint16 (+fl 2^16 (*fl -1. (floor (abs obj))))))
	 (else
	  (positive-double->uint16 obj))))
   
   (cond
      ((fixnum? obj) (modulofx obj (bit-lsh 1 16)))
      ((flonum? obj) (double->uint16 obj))
      (else (js-touint16 (js-tointeger obj)))))

;*---------------------------------------------------------------------*/
;*    js-touint32 ::obj ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.6          */
;*---------------------------------------------------------------------*/
(define (js-touint32::uint32 obj)
   
   (define 2^32 (exptfl 2. 32.))
   
   (define (positive-double->uint32::uint32 obj::double)
      (if (<fl obj 2^32)
	  (flonum->uint32 obj)
	  (flonum->uint32 (remainderfl obj 2^32))))

   (define (double->uint32::uint32 obj::double)
      (cond
	 ((or (= obj +inf.0) (= obj -inf.0) (not (= obj obj)))
	   #u32:0)
	 ((<fl obj 0.)
	  (positive-double->uint32 (+fl 2^32 (*fl -1. (floor (abs obj))))))
	 (else
	  (positive-double->uint32 obj))))
   
   (cond
      ((uint32? obj)
       (let ((r::uint32 obj)) r))
      ((int32? obj)
       (int32->uint32 obj))
      ((fixnum? obj)
       (cond-expand
	  (bint30
	   (fixnum->uint32 obj))
	  (bint32
	   (int32->uint32 (fixnum->int32 obj)))
	  (else
	   (if (<=fx obj (-fx (bit-lsh 1 32) 1))
	       (fixnum->uint32 obj)
	       (let* ((^31 (bit-lsh 1 31))
		      (^32 (bit-lsh 1 32))
		      (posint (if (<fx obj 0) (+fx ^32 obj) obj))
		      (int32bit (modulofx posint ^32)))
		  (fixnum->uint32 int32bit))))))
      ((flonum? obj)
       (double->uint32 obj))
      (else
       (js-touint32 (js-tointeger obj)))))
		  
;*---------------------------------------------------------------------*/
;*    js-toint32 ::obj ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.5          */
;*---------------------------------------------------------------------*/
(define (js-toint32::int32 obj)

   (define (int64->int32::int32 obj::int64)
      (cond-expand
	 ((or bint30 bint32)
	  (let* ((i::llong (int64->llong obj))
		 (^31 (*llong #l8 (fixnum->llong (bit-lsh 1 28))))
		 (^32 (*llong #l2 ^31))
		 (posint (if (<llong i #l0) (+llong ^32 i) i))
		 (int32bit (modulollong posint ^32))
		 (n (if (>=llong int32bit ^31)
			(-llong int32bit ^32)
			int32bit)))
	     (llong->int32 n)))
	 (else
	  (let* ((i::elong (int64->elong obj))
		 (^31 (fixnum->elong (bit-lsh 1 31)))
		 (^32 (fixnum->elong (bit-lsh 1 32)))
		 (posint (if (<elong i #e0) (+elong ^32 i) i))
		 (int32bit (moduloelong posint ^32))
		 (n (if (>=elong int32bit ^31)
			(-elong int32bit ^32)
			int32bit)))
	     (elong->int32 n)))))

   (cond
      ((int32? obj)
       obj)
      ((uint32? obj)
       (uint32->int32 obj))
      ((fixnum? obj)
       (cond-expand
	  ((or bint30 bint32)
	   (fixnum->int32 obj))
	  (bint61
	   (if (and (<=fx obj (-fx (bit-lsh 1 31) 1))
		    (>=fx obj (negfx (bit-lsh 1 31))))
	       (fixnum->int32 obj)
	       (let* ((^31 (bit-lsh 1 31))
		      (^32 (bit-lsh 1 32))
		      (posint (if (<fx obj 0) (+fx ^32 obj) obj))
		      (int32bit (modulofx posint ^32))
		      (n (if (>=fx int32bit ^31)
			     (-fx int32bit ^32)
			     int32bit)))
		  (fixnum->int32 n))))
	  (else
	   (int64->int32 (fixnum->int64 obj)))))
      ((flonum? obj)
       (cond
	  ((or (= obj +inf.0) (= obj -inf.0) (nanfl? obj))
	   (fixnum->int32 0))
	  ((<fl obj 0.)
	   (let ((i (*fl -1. (floor (abs obj)))))
	      (if (>=fl i (negfl (exptfl 2. 31.)))
		  (fixnum->int32 (flonum->fixnum i))
		  (int64->int32 (flonum->int64 i)))))
	  (else
	   (let ((i (floor obj)))
	      (if (<=fl i (-fl (exptfl 2. 31.) 1.))
		  (fixnum->int32 (flonum->fixnum i))
		  (int64->int32 (flonum->int64 i)))))))
      (else
       (js-toint32 (js-tonumber obj)))))

;*---------------------------------------------------------------------*/
;*    js-tostring ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.8          */
;*---------------------------------------------------------------------*/
(define (js-tostring obj)
   (cond
      ((isa? obj JsObject)
       (js-tostring (js-toprimitive obj 'string)))
      ((string? obj)
       obj)
      ((eq? obj (js-undefined))
       "undefined")
      ((eq? obj #t)
       "true")
      ((eq? obj #f)
       "false")
      ((eq? obj (js-null))
       "null")
      ((number? obj)
       (js-number->string obj))
      ((symbol? obj)
       (symbol->string! obj)) 
      (else
       (typeof obj))))

;*---------------------------------------------------------------------*/
;*    js-toobject ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.9          */
;*---------------------------------------------------------------------*/
(define (js-toobject o)
   (cond
      ((isa? o JsObject)
       o)
      ((string? o)
       (js-new1 js-string o))
      ((number? o)
       (js-new1 js-number o))
      ((boolean? o)
       (js-new1 js-boolean o))
      (else
       (js-raise-type-error "toObject: cannot convert ~s" o))))

;*---------------------------------------------------------------------*/
;*    js-equal? ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.1       */
;*---------------------------------------------------------------------*/
(define-inline (js-equal? o1 o2)
   (or (and (not (flonum? o1)) (eq? o1 o2)) (js-equality? o1 o2)))

;*---------------------------------------------------------------------*/
;*    js-equality? ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.3       */
;*---------------------------------------------------------------------*/
(define (js-equality? x y)
   (cond
      ((eq? x y)
       (not (and (flonum? x) (nanfl? x))))
      ((eq? x (js-null))
       (eq? y (js-undefined)))
      ((eq? x (js-undefined))
       (eq? y (js-null)))
      ((number? x)
       (cond
	  ((number? y)
	   (= x y))
	  ((string? y)
	   (if (= x 0)
	       (or (string-null? y) (js-equality? x (js-tonumber y)))
	       (js-equality? x (js-tonumber y))))
	  ((isa? y JsObject)
	   (js-equality? x (js-toprimitive y 'any)))
	  ((boolean? y)
	   (js-equality? x (js-tonumber y)))
	  (else #f)))
      ((string? x)
       (cond
	  ((string? y)
	   (string=? x y))
	  ((number? y)
	   (if (= y 0)
	       (or (string-null? x) (js-equality? (js-tonumber x) y))
	       (js-equality? (js-tonumber x) y)))
	  ((isa? y JsObject)
	   (js-equality? x (js-toprimitive y 'any)))
	  ((eq? y #f)
	   (string-null? x))
	  ((boolean? y)
	   (js-equality? x (js-tonumber y)))
	  (else #f)))
      ((boolean? x)
       (cond
	  ((boolean? y)
	   #f)
	  (else
	   (js-equality? (js-tonumber x) y))))
      ((boolean? y)
       (js-equality? x (js-tonumber y)))
      ((isa? x JsObject)
       (cond
	  ((string? y) (js-equality? (js-toprimitive x 'any) y))
	  ((number? y) (js-equality? (js-toprimitive x 'any) y))
	  (else #f)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    js-strict-equal?                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.4       */
;*---------------------------------------------------------------------*/
(define-inline (js-strict-equal? o1 o2)
   (or (and (eq? o1 o2) (not (flonum? o1))) (js-eq? o1 o2)))

;*---------------------------------------------------------------------*/
;*    js-eq? ...                                                       */
;*---------------------------------------------------------------------*/
(define (js-eq? x y)
   (cond
      ((number? x) (and (number? y) (= x y)))
      ((string? x) (and (string? y) (string=? x y)))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    make-pcache ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-pcache len)
   (let ((pcache ($make-vector-uncollectable len #unspecified)))
      (let loop ((i 0))
	 (if (=fx i len)
	     pcache
	     (begin
		(vector-set-ur! pcache i (instantiate::JsPropertyCache))
		(loop (+fx i 1)))))))
      
;*---------------------------------------------------------------------*/
;*    pcache-ref ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (pcache-ref pcache index)
   (vector-ref-ur pcache index))

;*---------------------------------------------------------------------*/
;*    %js-eval-strict ...                                              */
;*---------------------------------------------------------------------*/
(define (%js-eval-strict string)
   (%js-eval (string-append "\"use strict\";\n" string)))

;*---------------------------------------------------------------------*/
;*    js-raise ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-raise err)
   (with-access::JsError err (stack)
      (set! stack (get-trace-stack))
      (raise err)))
