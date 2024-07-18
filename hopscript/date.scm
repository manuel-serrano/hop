;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/date.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Thu Jul 18 08:57:53 2024 (serrano)                */
;*    Copyright   :  2013-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript dates                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_date
   
   (library hop)
   
   (include "types.sch" "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_regexp
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_error
	   __hopscript_names)
   
   (cond-expand
      ((or bint30 bint32)
       (import __hopscript_arithmetic32))
      (else
       (import __hopscript_arithmetic64)))
   
   (export (js-init-date! ::JsObject)
	   (inline js-date-alloc ::JsGlobalObject)
	   (js-date->jsdate::JsDate ::date ::JsGlobalObject)
	   (js-new-date0 ::JsGlobalObject)
	   (js-new-date1 ::JsGlobalObject ::obj)
	   (js-new-date2 ::JsGlobalObject ::obj ::obj)
	   (js-new-date3 ::JsGlobalObject ::obj ::obj ::obj)
	   (js-new-date4 ::JsGlobalObject ::obj ::obj ::obj ::obj)
	   (js-new-date5 ::JsGlobalObject ::obj ::obj ::obj ::obj ::obj)
	   (js-new-date6 ::JsGlobalObject ::obj ::obj ::obj ::obj ::obj ::obj)
	   (js-new-date7 ::JsGlobalObject ::obj ::obj ::obj ::obj ::obj ::obj ::obj)
	   (js-date-utc0 ::JsGlobalObject)
	   (js-date-utc1 ::JsGlobalObject ::obj)
	   (js-date-utc2 ::JsGlobalObject ::obj ::obj)
	   (js-date-utc3 ::JsGlobalObject ::obj ::obj ::obj)
	   (js-date-utc4 ::JsGlobalObject ::obj ::obj ::obj ::obj)
	   (js-date-utc5 ::JsGlobalObject ::obj ::obj ::obj ::obj ::obj)
	   (js-date-utc6 ::JsGlobalObject ::obj ::obj ::obj ::obj ::obj ::obj)
	   (js-date-utc7 ::JsGlobalObject ::obj ::obj ::obj ::obj ::obj ::obj ::obj)
	   (js-date-now)
	   (js-date-gettime ::JsDate)
	   (js-date-maybe-gettime ::obj ::JsGlobalObject ::obj)
	   (js-date-getfullyear ::JsDate)
	   (js-date-maybe-getfullyear ::obj ::JsGlobalObject ::obj)
	   (js-date-getmonth ::JsDate)
	   (js-date-maybe-getmonth ::obj ::JsGlobalObject ::obj)
	   (js-date-getdate ::JsDate)
	   (js-date-maybe-getdate ::obj ::JsGlobalObject ::obj)
	   (js-date-getutcdate ::JsDate)
	   (js-date-maybe-getutcdate ::obj ::JsGlobalObject ::obj)
	   (js-date-gethours ::JsDate)
	   (js-date-maybe-gethours ::obj ::JsGlobalObject ::obj)
	   (js-date-getminutes ::JsDate)
	   (js-date-maybe-getminutes ::obj ::JsGlobalObject ::obj)
	   (js-date-getutcminutes ::JsDate)
	   (js-date-maybe-getutcminutes ::obj ::JsGlobalObject ::obj)
	   (js-date-getseconds ::JsDate)
	   (js-date-maybe-getseconds ::obj ::JsGlobalObject ::obj)
	   (js-date-getmilliseconds ::JsDate)
	   (js-date-maybe-getmilliseconds ::obj ::JsGlobalObject ::obj)
	   (js-date-setminutes ::JsDate min sec ms ::JsGlobalObject)
	   (js-date-maybe-setminutes ::obj min sec ms ::JsGlobalObject ::obj)
	   (js-date-setutcminutes ::JsDate min sec ms ::JsGlobalObject)
	   (js-date-maybe-setutcminutes ::obj min sec ms ::JsGlobalObject ::obj)
	   ))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-date-val ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-date-val o::JsDate)
   (with-access::JsDate o (time %val)
      (or %val
	  (let ((val (milliseconds->date time)))
	     (set! %val val)
	     val))))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsDate ...                                   */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsDate
   (lambda (o ctx)
      (js-date-val o))
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-date->jsdate o ctx)
	  (error "string-obj ::JsData" "Not a JavaScript context" ctx))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsDate ...                                           */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsDate worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-date)
	 (let ((nobj (call-next-method)))
	    (with-access::JsDate nobj (%val time)
	       (with-access::JsDate obj ((_%val %val) (_time time))
		  (js-object-proto-set! nobj (js-get js-date (& "prototype") %this))
		  (set! time _time)
		  (set! %val (js-donate _%val worker %_this))))
	    nobj))))

;*---------------------------------------------------------------------*/
;*    scheme->response ::JsDate ...                                    */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::JsDate req ctx)
   (with-access::JsDate obj (time)
      (instantiate::http-response-hop
	 (server (hop-server-name))
	 (backend (hop-xml-backend-secure))
	 (content-type "text/string")
	 (header '((Hop-Serialize: . "date") (Cache-Control: . "no-cache") (Pragma: . "no-cache")))
	 (bodyp #t)
	 (value (number->string time)))))

;*---------------------------------------------------------------------*/
;*    xml-write ::JsDate ...                                           */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::JsDate p backend)
   (let ((val (js-date-val obj)))
      (display val p)))
		  
;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsDate ...                                          */
;*    -------------------------------------------------------------    */
;*    Used when an JS object is to pack the arguments sent to          */
;*    an XML constructor.                                              */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack o::JsDate ctx)
   (let ((val (js-date-val o)))
      (list val)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsDate ...                                     */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsDate op compile isexpr ctx)
   (display "new Date(" op)
   (with-access::JsDate o (time)
      (if time
	  (display (llong->flonum time) op)
	  (display "undefined" op)))
   (display ")" op))

;*---------------------------------------------------------------------*/
;*    js-toprimitive ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.8       */
;*---------------------------------------------------------------------*/
(define-method (js-toprimitive o::JsDate preferredtype %this)
   (case preferredtype
      ((any)
       (set! preferredtype 'string)
       (call-next-method))
      (else
       (call-next-method))))


;*---------------------------------------------------------------------*/
;*    tofixnum ...                                                     */
;*---------------------------------------------------------------------*/
(define (tofixnum val %this)
   (cond
      ((fixnum? val) val)
      ((flonum? val) (if (nanfl? val) val (flonum->fixnum val)))
      ((eq? val (js-undefined)) #f)
      (else (tofixnum (js-tonumber val %this) %this))))

;*---------------------------------------------------------------------*/
;*    set-date-val! ...                                                */
;*---------------------------------------------------------------------*/
(define (set-date-val! this v)
   (with-access::JsDate this (%val time)
      (if (date? v)
	  (begin
	     (set! %val v)
	     (set! time (date->milliseconds v)))
	  (begin
	     (set! %val #f)
	     (set! time #f)))
      this))

;*---------------------------------------------------------------------*/
;*    set-date-time! ...                                               */
;*---------------------------------------------------------------------*/
(define (set-date-time! this v::llong)
   (with-access::JsDate this (%val time)
      (set! time v)
      (set! %val #f)
      this))

;*---------------------------------------------------------------------*/
;*    js-init-date! ...                                                */
;*---------------------------------------------------------------------*/
(define (js-init-date! %this)
   ;; local constant strings
   (unless (vector? __js_strings) (set! __js_strings (&init!)))

   ($js-init-jsalloc-date
      (bit-andu32 (js-object-default-mode)
	 (bit-notu32 (JS-OBJECT-MODE-INLINE))))
   
   ;; first, bind the builtin date prototype
   (with-access::JsGlobalObject %this (js-date js-date-cmap
					 js-function js-date-prototype)

      (set! js-date-cmap (js-make-jsconstructmap))
      
      (set! js-date-prototype
	 (let ((dt (current-date)))
	    (instantiateJsDate
	       (%val dt)
	       (time (date->milliseconds dt))
	       (cmap (js-make-jsconstructmap))
	       (__proto__ (js-object-proto %this))
	       (elements ($create-vector 47)))))
      
      (define (js-date-ctor-alloc %this constructor::JsFunction)
	 (with-access::JsGlobalObject %this (js-date)
	    (js-new-target-push! %this constructor)
	    (instantiateJsDate
	       (cmap js-date-cmap)
	       (__proto__ (if (eq? constructor js-date)
			      js-date-prototype
			      (js-get constructor (& "prototype") %this))))))
      
      (define (parse-date-arguments args)
	 (match-case args
	    ((?year ?month ?date ?hours ?minutes ?seconds ?ms)
	     (js-date-value7 %this year month date hours minutes seconds ms))
	    ((?year ?month ?date ?hours ?minutes ?seconds)
	     (js-date-value6 %this year month date hours minutes seconds))
	    ((?year ?month ?date ?hours ?minutes)
	     (js-date-value5 %this year month date hours minutes))
	    ((?year ?month ?date ?hours)
	     (js-date-value4 %this year month date hours))
	    ((?year ?month ?date)
	     (js-date-value3 %this year month date))
	    ((?year ?month)
	     (js-date-value2 %this year month))
	    ((?value)
	     (js-date-value1 %this value))
	    (else
	     (js-date-value0 %this))))
      
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9
      (define (%js-date this . args)
	 (cond
	    ((eq? (js-new-target-pop! %this) (js-undefined))
	     (js-date->jsstring (current-date)))
	    ((any (lambda (a) (eq? a (js-undefined))) args)
	     this)
	    (else
	     (let ((val (parse-date-arguments args)))
		(set-date-val! this val)
		this))))
      
      (set! js-date
	 (js-make-function %this %js-date
	    (js-function-arity 0 -1 'scheme)
	    (js-function-info :name "Date" :len 7)
	    :__proto__ (js-object-proto js-function)
	    :prototype js-date-prototype
	    :alloc js-date-ctor-alloc
	    :size 3
	    :shared-cmap #f))
      
      ;; parse
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.4.2
      (define (js-date-parse this str)
	 (let ((d (parse-date (js-tostring str %this))))
	    (if (date? d)
		(js-date->milliseconds d)
		+nan.0)))
      
      (js-bind! %this js-date (& "parse")
	 :value (js-make-function %this js-date-parse
		   (js-function-arity js-date-parse)
		   (js-function-info :name "parse" :len 1))
	 :writable #t :configurable #t :enumerable #f :hidden-class #t)
      
      ;; UTC
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.4.3
      (define (js-date-utc this . args)
	 (if (any (lambda (a) (eq? a (js-undefined))) args)
	     +nan.0
	     (match-case args
		(()
		 (js-date-utc0 %this))
		((?a0)
		 (js-date-utc1 %this a0))
		((?a0 ?a1)
		 (js-date-utc2 %this a0 a1))
		((?a0 ?a1 ?a2)
		 (js-date-utc3 %this a0 a1 a2))
		((?a0 ?a1 ?a2 ?a3)
		 (js-date-utc4 %this a0 a1 a2 a3))
		((?a0 ?a1 ?a2 ?a3 ?a4)
		 (js-date-utc5 %this a0 a1 a2 a3 a4))
		((?a0 ?a1 ?a2 ?a3 ?a4 ?a5)
		 (js-date-utc6 %this a0 a1 a2 a3 a4 a5))
		((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6)
		 (js-date-utc7 %this a0 a1 a2 a3 a4 a5 a6))
		(else
		 (let ((dt (parse-date-arguments args)))
		    (if (date? dt)
			(let ((ctz (date-timezone dt)))
			   (js-flonum->integer
			      (llong->flonum
				 (+llong (date->milliseconds dt)
				    (*llong (fixnum->llong ctz) #l1000)))))
			0))))))
      
      (js-bind! %this js-date (& "UTC")
	 :value (js-make-function %this js-date-utc
		   (js-function-arity js-date-utc)
		   (js-function-info :name "UTC" :len 7))
	 :writable #t :configurable #t :enumerable #f :hidden-class #f)
      
      (js-bind! %this js-date (& "now")
	 :value (js-make-function %this (lambda (this) (js-date-now))
		   (js-function-arity 0 0)
		   (js-function-info :name "now" :len 0))
	 :writable #t :configurable #t :enumerable #f :hidden-class #f)
      
      ;; prototype properties
      (init-builtin-date-prototype! %this js-date js-date-prototype)
      ;; bind Date in the global object
      (js-bind! %this %this (& "Date")
	 :configurable #f :enumerable #f :value js-date :hidden-class #t)
      js-date))

;*---------------------------------------------------------------------*/
;*    js-date-alloc ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-date-alloc %this)
   (with-access::JsGlobalObject %this (js-date-prototype js-date-cmap)
      ($js-make-jsdate js-date-cmap js-date-prototype)))

;*---------------------------------------------------------------------*/
;*    js-date-value0 ...                                               */
;*---------------------------------------------------------------------*/
(define (js-date-value0 %this)
   (current-date))

;*---------------------------------------------------------------------*/
;*    js-date-value1 ...                                               */
;*---------------------------------------------------------------------*/
(define (js-date-value1 %this value)
   (if (isa? value JsDate)
       (js-date-val value)
       (let ((v (js-toprimitive value 'any %this)))
	  (cond
	     ((js-jsstring? v)
	      (parse-date (js-jsstring->string v)))
	     ((js-number? v)
	      (if (flonum? v)
		  (if (or (nanfl? v) (=fl v +inf.0) (=fl v -inf.0))
		      +nan.0
		      (milliseconds->date
			 (flonum->llong (floorfl v))))
		  (let ((n (cond
			      ((fixnum? v) (fixnum->llong v))
			      ((llong? v) v)
			      ((elong? v) (elong->llong v))
			      (else #l0))))
		     (milliseconds->date n))))
	     (else
	      v)))))

;*---------------------------------------------------------------------*/
;*    js-date-value2 ...                                               */
;*---------------------------------------------------------------------*/
(define (js-date-value2 %this year month)
   (let* ((y (tofixnum year %this))
	  (m (tofixnum month %this)))
      (when (and (fixnum? y) (>fx y 0)
		 (fixnum? m))
	 (make-date
	    :year y :month (+ m 1)
	    :day 1 :hour 0 :min 0 :sec 0))))

;*---------------------------------------------------------------------*/
;*    js-date-value3 ...                                               */
;*---------------------------------------------------------------------*/
(define (js-date-value3 %this year month date)
   (let* ((y (tofixnum year %this))
	  (m (tofixnum month %this))
	  (d (tofixnum date %this)))
      (when (and (fixnum? y) (>fx y 0)
		 (fixnum? m) (fixnum? d))
	 (make-date
	    :year y :month (+ m 1) :day d
	    :hour 0 :min 0 :sec 0))))

;*---------------------------------------------------------------------*/
;*    js-date-value4 ...                                               */
;*---------------------------------------------------------------------*/
(define (js-date-value4 %this year month date hours)
   (let* ((y (tofixnum year %this))
	  (m (tofixnum month %this))
	  (d (tofixnum date %this))
	  (h (tofixnum hours %this)))
      (when (and (fixnum? y) (>fx y 0)
		 (fixnum? m) (fixnum? d) (fixnum? h))
	 (make-date
	    :year y :month (+ m 1) :day d
	    :hour h :min 0 :sec 0))))

;*---------------------------------------------------------------------*/
;*    js-date-value5 ...                                               */
;*---------------------------------------------------------------------*/
(define (js-date-value5 %this year month date hours minutes)
   (let* ((y (tofixnum year %this))
	  (m (tofixnum month %this))
	  (d (tofixnum date %this))
	  (h (tofixnum hours %this))
	  (mi (tofixnum minutes %this)))
      (when (and (fixnum? y) (>fx y 0)
		 (fixnum? m) (fixnum? d) (fixnum? h)
		 (fixnum? mi))
	 (make-date
	    :year y :month (+ m 1) :day d
	    :hour h :min mi :sec 0))))

;*---------------------------------------------------------------------*/
;*    js-date-value6 ...                                               */
;*---------------------------------------------------------------------*/
(define (js-date-value6 %this year month date hours minutes seconds)
   (let* ((y (tofixnum year %this))
	  (m (tofixnum month %this))
	  (d (tofixnum date %this))
	  (h (tofixnum hours %this))
	  (mi (tofixnum minutes %this))
	  (se (tofixnum seconds %this)))
      (when (and (fixnum? y) (>fx y 0)
		 (fixnum? m) (fixnum? d) (fixnum? h)
		 (fixnum? mi) (fixnum? se))
	 (make-date
	    :year y :month (+ m 1) :day d
	    :hour h :min mi :sec se))))

;*---------------------------------------------------------------------*/
;*    js-date-value7 ...                                               */
;*---------------------------------------------------------------------*/
(define (js-date-value7 %this year month date hours minutes seconds ms)
   (let* ((y (tofixnum year %this))
	  (m (tofixnum month %this))
	  (d (tofixnum date %this))
	  (h (tofixnum hours %this))
	  (mi (tofixnum minutes %this))
	  (se (tofixnum seconds %this))
	  (us (tofixnum ms %this))
	  (ns (cond
		 ((fixnum? us)
		  (*llong #l1000000 (fixnum->llong us)))
		 ((flonum? us)
		  (*llong #l1000000
		     (fixnum->llong (flonum->fixnum us))))
		 (else
		  #l0))))
      (when (and (fixnum? y) (>fx y 0)
		 (fixnum? m) (fixnum? d) (fixnum? h)
		 (fixnum? mi) (fixnum? se) (fixnum? us) (llong? ns))
	 (make-date
	    :year y :month (+ m 1) :day d
	    :hour h :min mi :sec se :nsec ns))))

;*---------------------------------------------------------------------*/
;*    js-new-date0 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-new-date0 %this)
   (let ((this (js-date-alloc %this)))
      (set-date-val! this (js-date-value0 %this))
      this))

;*---------------------------------------------------------------------*/
;*    js-new-date1 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-new-date1 %this value)
   (let ((this (js-date-alloc %this)))
      (if (js-number? value)
	  (with-access::JsDate this (time)
	     (cond
		((fixnum? value)
		 (set! time (fixnum->llong value)))
		((flonum? value)
		 (unless (or (nanfl? value) (=fl value +inf.0) (=fl value -inf.0))
		    (set! time (flonum->llong (floorfl value)))))
		(else
		 (set! time #l0))))
	  (set-date-val! this (js-date-value1 %this value)))
      this))

;*---------------------------------------------------------------------*/
;*    js-new-date2 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-new-date2 %this year month)
   (let ((this (js-date-alloc %this)))
      (set-date-val! this (js-date-value2 %this year month))
      this))

;*---------------------------------------------------------------------*/
;*    js-new-date3 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-new-date3 %this year month date)
   (let ((this (js-date-alloc %this)))
      (set-date-val! this (js-date-value3 %this year month date))
      this))

;*---------------------------------------------------------------------*/
;*    js-new-date4 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-new-date4 %this year month date hours)
   (let ((this (js-date-alloc %this)))
      (set-date-val! this (js-date-value4 %this year month date hours))
      this))

;*---------------------------------------------------------------------*/
;*    js-new-date5 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-new-date5 %this year month date hours minutes)
   (let ((this (js-date-alloc %this)))
      (set-date-val! this (js-date-value5 %this year month date hours minutes))
      this))

;*---------------------------------------------------------------------*/
;*    js-new-date6 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-new-date6 %this year month date hours minutes seconds)
   (let ((this (js-date-alloc %this)))
      (set-date-val! this (js-date-value6 %this year month date hours minutes seconds))
      this))

;*---------------------------------------------------------------------*/
;*    js-new-date7 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-new-date7 %this year month date hours minutes seconds ms)
   (let ((this (js-date-alloc %this)))
      (set-date-val! this (js-date-value7 %this year month date hours minutes seconds ms))
      this))
 
;*---------------------------------------------------------------------*/
;*    js-date-utc0 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-date-utc0 %this)
   (let ((dt (js-date-value0 %this)))
      (if (date? dt)
	  (let ((ctz (date-timezone dt)))
	     (js-flonum->integer
		(llong->flonum
		   (+llong (date->milliseconds dt)
		      (*llong (fixnum->llong ctz) #l1000)))))
	  0)))

;*---------------------------------------------------------------------*/
;*    js-date-utc1 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-date-utc1 %this year)
   (let ((dt (make-date :year year)))
      (if (date? dt)
	  (let ((ctz (date-timezone dt)))
	     (js-flonum->integer
		(llong->flonum
		   (+llong (date->milliseconds dt)
		      (*llong (fixnum->llong ctz) #l1000)))))
	  0)))

;*---------------------------------------------------------------------*/
;*    js-date-utc2 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-date-utc2 %this year month)
   (let ((dt (js-date-value2 %this year month)))
      (if (date? dt)
	  (let ((ctz (date-timezone dt)))
	     (js-flonum->integer
		(llong->flonum
		   (+llong (date->milliseconds dt)
		      (*llong (fixnum->llong ctz) #l1000)))))
	  0)))

;*---------------------------------------------------------------------*/
;*    js-date-utc3 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-date-utc3 %this year month date)
   (let ((dt (js-date-value3 %this year month date)))
      (if (date? dt)
	  (let ((ctz (date-timezone dt)))
	     (js-flonum->integer
		(llong->flonum
		   (+llong (date->milliseconds dt)
		      (*llong (fixnum->llong ctz) #l1000)))))
	  0)))

;*---------------------------------------------------------------------*/
;*    js-date-utc4 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-date-utc4 %this year month date hours)
   (let ((dt (js-date-value4 %this year month date hours)))
      (if (date? dt)
	  (let ((ctz (date-timezone dt)))
	     (js-flonum->integer
		(llong->flonum
		   (+llong (date->milliseconds dt)
		      (*llong (fixnum->llong ctz) #l1000)))))
	  0)))

;*---------------------------------------------------------------------*/
;*    js-date-utc5 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-date-utc5 %this year month date hours minutes)
   (let ((dt (js-date-value5 %this year month date hours minutes)))
      (if (date? dt)
	  (let ((ctz (date-timezone dt)))
	     (js-flonum->integer
		(llong->flonum
		   (+llong (date->milliseconds dt)
		      (*llong (fixnum->llong ctz) #l1000)))))
	  0)))

;*---------------------------------------------------------------------*/
;*    js-date-utc6 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-date-utc6 %this year month date hours minutes seconds)
   (let ((dt (js-date-value6 %this year month date hours minutes seconds)))
      (if (date? dt)
	  (let ((ctz (date-timezone dt)))
	     (js-flonum->integer
		(llong->flonum
		   (+llong (date->milliseconds dt)
		      (*llong (fixnum->llong ctz) #l1000)))))
	  0)))

;*---------------------------------------------------------------------*/
;*    js-date-utc7 ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-date-utc7 %this year month date hours minutes seconds ms)
   (let ((dt (js-date-value7 %this year month date hours minutes seconds ms)))
      (if (date? dt)
	  (let ((ctz (date-timezone dt)))
	     (js-flonum->integer
		(llong->flonum
		   (+llong (date->milliseconds dt)
		      (*llong (fixnum->llong ctz) #l1000)))))
	  0)))

;*---------------------------------------------------------------------*/
;*    parse-date ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15    */
;*---------------------------------------------------------------------*/
(define (parse-date v::bstring)
   ;; JS dates are weird, if no time nor timezone is give, the string
   ;; representation denotes an UTC date, otherwise, a local time zone date!
   ;; For extra details, see:
   ;; https://maggiepint.com/2017/04/11/fixing-javascript-date-web-compatibility-and-reality/
   (match-case (pregexp-match "^([0-9]{4})(?:-([0-9]{2})(?:-([0-9]{2}))?)?$" v)
      ((?- ?year #f #f)
       ;; utc date because to time nor time zone
       (make-date :year (string->integer year)
	  :timezone 0))
      ((?- ?year ?month #f)
       ;; utc date because to time nor time zone
       (make-date :year (string->integer year)
	  :month (string->integer month)
	  :timezone 0))
      ((?- ?year ?month ?day)
       ;; utc date because to time nor time zone
       (make-date :year (string->integer year)
	  :month (string->integer month)
	  :day (string->integer day)
	  :timezone 0))
      (else
       (let ((ip (open-input-string v)))
	  (unwind-protect
	     (with-handler
		(lambda (e)
		   (input-port-reopen! ip)
		   (with-handler
		      (lambda (e) "Invalid Date")
		      (iso8601-parse-date ip)))
		(rfc2822-parse-date ip))
	     (close-input-port ip))))))

;*---------------------------------------------------------------------*/
;*    init-builtin-date-prototype! ...                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5       */
;*---------------------------------------------------------------------*/
(define (init-builtin-date-prototype! %this js-date obj)
   
   ;; constructor
   (js-bind! %this obj (& "constructor")
      :value js-date
      :writable #t :configurable #t :enumerable #f :hidden-class #t)
   
   ;; toString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.2
   (js-bind! %this obj (& "toString")
      :value (js-make-function %this date-prototype-tostring
		(js-function-arity date-prototype-tostring)
		(js-function-info :name "toString" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; toDateString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.3
   (define (date-prototype-todatestring this::JsDate)
      (with-access::JsDate this (time)
	 (if time
	     (let ((val (js-date-val this)))
		(js-ascii->jsstring
		   (format "~a ~a ~2,0d ~d"
		      (day-aname (date-wday val))
		      (month-aname (date-month val))
		      (date-day val)
		      (date-year val))))
	     (js-ascii->jsstring "Invalid date"))))

   (js-bind! %this obj (& "toDateString")
      :value (js-make-function %this date-prototype-todatestring
		(js-function-arity date-prototype-todatestring)
		(js-function-info :name "toDateString" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; toTimeString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.4
   (define (date-prototype-totimestring this::JsDate)
      (with-access::JsDate this (time)
	 (if (date? time)
	     (let ((val (js-date-val this)))
		(js-ascii->jsstring
		   (format "~2,0d:~2,0d:~2,0d ~a"
		      (date-hour val)
		      (date-minute val)
		      (date-second val)
		      (date-timezone val))))
	     (js-ascii->jsstring "Invalid date"))))

   (js-bind! %this obj (& "toTimeString")
      :value (js-make-function %this date-prototype-totimestring
		(js-function-arity date-prototype-totimestring)
		(js-function-info :name "toTimeString" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; toLocaleString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.5
   (define (date-prototype-tolocalestring this::JsDate)
      (date-prototype-tostring this))

   (js-bind! %this obj (& "toLocaleString")
      :value (js-make-function %this date-prototype-tolocalestring
		(js-function-arity date-prototype-tolocalestring)
		(js-function-info :name "toLocaleString" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; toLocaleDateString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.6
   (define (date-prototype-tolocaledatestring this::JsDate)
      (date-prototype-todatestring this))

   (js-bind! %this obj (& "toLocaleDateString")
      :value (js-make-function %this date-prototype-tolocaledatestring
		(js-function-arity date-prototype-tolocaledatestring)
		(js-function-info :name "toLocaleDateString" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; toLocaleTimeString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.7
   (define (date-prototype-tolocaletimestring this::JsDate)
      (date-prototype-totimestring this))

   (js-bind! %this obj (& "toLocaleTimeString")
      :value (js-make-function %this date-prototype-tolocaletimestring
		(js-function-arity date-prototype-tolocaletimestring)
		(js-function-info :name "toLocaleTimeString" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; toUTCString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.42
   (define (date-prototype-toutcstring this::JsDate)
      (with-access::JsDate this (time)
	 (if time
	     (let ((val (js-date-val this)))
		(js-ascii->jsstring (date->utc-string val)))
	     "Invalid date")))

   (js-bind! %this obj (& "toUTCString")
      :value (js-make-function %this date-prototype-toutcstring
		(js-function-arity date-prototype-toutcstring)
		(js-function-info :name "toUTCString" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; toISOString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.43
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15
   (define (date-prototype-toisostring this)
      
      (define (utc-isostring val)
	 (let ((buf (make-string 24 #\0)))
	    ;; equivalent to
	    ;; (format "~4,0d-~2,0d-~2,0dT~2,0d:~2,0d:~2,0d.~3,0dZ"
	    ;;    (date-year val)
	    ;;    (date-month val)
	    ;;    (date-day val)
	    ;;    (date-hour val)
	    ;;    (date-minute val)
	    ;;    (date-second val)
	    ;;    (llong->fixnum (date-millisecond val))
	    (string-set! buf 4 #\-)
	    (string-set! buf 7 #\-)
	    (string-set! buf 10 #\T)
	    (string-set! buf 13 #\:)
	    (string-set! buf 16 #\:)
	    (string-set! buf 19 #\.)
	    (string-set! buf 23 #\Z)
	    (blit-fixnum! buf (date-year val) 0 4)
	    (blit-fixnum! buf (date-month val) 5 2)
	    (blit-fixnum! buf (date-day val) 8 2)
	    (blit-fixnum! buf (date-hour val) 11 2)
	    (blit-fixnum! buf (date-minute val) 14 2)
	    (blit-fixnum! buf (date-second val) 17 2)
	    (let ((ms (llong->fixnum (date-millisecond val))))
	       (blit-fixnum! buf ms 20 3))
	    (js-ascii->jsstring buf)))
      
      (if (not (isa? this JsDate))
	  (js-raise-type-error %this "Not a date ~s" (typeof this))
	  (with-access::JsDate this (time)
	     (if time
		 (let ((val (js-date-val this)))
		    (let loop ((val val))
		       (if (=fx (date-timezone val) 0)
			   (utc-isostring val)
			   (utc-isostring (date->utc-date val)))))
		 (js-raise-range-error %this "Invalid date ~s" time)))))
   
   (js-bind! %this obj (& "toISOString")
      :value (js-make-function %this date-prototype-toisostring
		(js-function-arity date-prototype-toisostring)
		(js-function-info :name "toISOString" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; toJSON
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.44
   (define (date-prototype-toJSON this)
      (let* ((o (js-toobject %this this))
	     (tv (js-toprimitive o 'number %this)))
	 (if (and (js-number? tv) (not (integer? tv)))
	     (js-null)
	     (let ((p (js-get o (& "toISOString") %this)))
		(if (js-procedure? p)
		    (js-call0 %this p o)
		    (js-raise-type-error %this
		       "toJSON: argument not a function ~s" p))))))

   (js-bind! %this obj (& "toJSON")
      :value (js-make-function %this date-prototype-toJSON
		(js-function-arity date-prototype-toJSON)
		(js-function-info :name "toJSON" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)

   ;; valueOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.8
   (define (date-prototype-valueof this::JsDate)
      (with-access::JsDate this (time)
	 (if time
	     (js-time->milliseconds time)
	     +nan.0)))
	 
   (js-bind! %this obj (& "valueOf")
      :value (js-make-function %this date-prototype-valueof
		(js-function-arity date-prototype-valueof)
		(js-function-info :name "valueOf" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getTime
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.9
   (js-bind! %this obj (& "getTime")
      :value (js-make-function %this
		(lambda (this)
		   (if (js-date? this)
		       (js-date-gettime this)
		       (js-raise-type-error %this "Not a date ~s" (typeof this))))
		(js-function-arity js-date-gettime)
		(js-function-info :name "getTime" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getFullYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.10
   (js-bind! %this obj (& "getFullYear")
      :value (js-make-function %this js-date-getfullyear
		(js-function-arity js-date-getfullyear)
		(js-function-info :name "getFullYear" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCFullYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.11
   (define (date-prototype-getutcfullyear this::JsDate)
      (with-access::JsDate this (time)
	 (if time
	     (let* ((val (js-date-val this))
		    (tz (date-timezone val))
		    (d (seconds->date (-elong (date->seconds val) tz))))
		(date-year d))
	     +nan.0)))
	 
   (js-bind! %this obj (& "getUTCFullYear")
      :value (js-make-function %this date-prototype-getutcfullyear
		(js-function-arity date-prototype-getutcfullyear)
		(js-function-info :name "getUTCFullYear" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getMonth
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.12
   (js-bind! %this obj (& "getMonth")
      :value (js-make-function %this js-date-getmonth
		(js-function-arity js-date-getmonth)
		(js-function-info :name "getMonth" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCMonth
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.13
   (define (date-prototype-getutcmonth this::JsDate)
      (with-access::JsDate this (time)
	 (if time
	     (let* ((val (js-date-val this))
		    (tz (date-timezone val))
		    (d (seconds->date (-elong (date->seconds val) tz))))
		(-fx (date-month d) 1))
	     +nan.0)))
	 
   (js-bind! %this obj (& "getUTCMonth")
      :value (js-make-function %this date-prototype-getutcmonth
		(js-function-arity date-prototype-getutcmonth)
		(js-function-info :name "getUTCMonth" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getDate
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.14
   (js-bind! %this obj (& "getDate")
      :value (js-make-function %this js-date-getdate
		(js-function-arity js-date-getdate)
		(js-function-info :name "getDate" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCDate
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.15
   (js-bind! %this obj (& "getUTCDate")
      :value (js-make-function %this js-date-getutcdate
		(js-function-arity js-date-getutcdate)
		(js-function-info :name "getUTCDate" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getDay
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.16
   (define (date-prototype-getday this::JsDate)
      (if (js-date? this)
	  (with-access::JsDate this (time)
	     (if time
		 (let ((val (js-date-val this)))
		    (-fx (date-wday val) 1))
		 +nan.0))
	  (js-raise-type-error %this "Not a date ~s" (typeof this))))
	 
   (js-bind! %this obj (& "getDay")
      :value (js-make-function %this date-prototype-getday
		(js-function-arity date-prototype-getday)
		(js-function-info :name "getDay" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCDay
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.17
   (define (date-prototype-getutcday this::JsDate)
      (with-access::JsDate this (time)
	 (if time
	     (let ((val (js-date-val this)))
		(-fx (date-wday val) 1))
	     +nan.0)))
	 
   (js-bind! %this obj (& "getUTCDay")
      :value (js-make-function %this date-prototype-getutcday
		(js-function-arity date-prototype-getutcday)
		(js-function-info :name "getUTCDay" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getHours
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.18
   (js-bind! %this obj (& "getHours")
      :value (js-make-function %this js-date-gethours
		(js-function-arity js-date-gethours)
		(js-function-info :name "getHours" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCHours
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.19
   (define (date-prototype-getutchours this::JsDate)
      (with-access::JsDate this (time)
	 (if time
	     (let* ((val (js-date-val this))
		    (tz (date-timezone val))
		    (tzh (/fx tz 3600))
		    (n (-fx (date-hour val) tzh)))
		(cond
		   ((<fx n 0) (+fx 24 n))
		   ((>fx n 23) (-fx n 23))
		   (else n)))
	     +nan.0)))
	 
   (js-bind! %this obj (& "getUTCHours")
      :value (js-make-function %this date-prototype-getutchours
		(js-function-arity date-prototype-getutchours)
		(js-function-info :name "getUTCHours" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getMinutes
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.20
   (js-bind! %this obj (& "getMinutes")
      :value (js-make-function %this js-date-getminutes
		(js-function-arity js-date-getminutes)
		(js-function-info :name "getMinutes" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)

   ;; getUTCMinutes
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.21
   (js-bind! %this obj (& "getUTCMinutes")
      :value (js-make-function %this js-date-getutcminutes
		(js-function-arity js-date-getutcminutes)
		(js-function-info :name "getUTCMinutes" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getSeconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.22
   (js-bind! %this obj (& "getSeconds")
      :value (js-make-function %this js-date-getseconds
		(js-function-arity js-date-getseconds)
		(js-function-info :name "getSeconds" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCSeconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.23
   (define (date-prototype-getutcseconds this::JsDate)
      (with-access::JsDate this (time)
	 (if time
	     (date-second (js-date-val this))
	     +nan.0)))
	 
   (js-bind! %this obj (& "getUTCSeconds")
      :value (js-make-function %this date-prototype-getutcseconds
		(js-function-arity date-prototype-getutcseconds)
		(js-function-info :name "getUTCSeconds" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getMilliseconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.24
   (js-bind! %this obj (& "getMilliseconds")
      :value (js-make-function %this js-date-getmilliseconds
		(js-function-arity js-date-getmilliseconds)
		(js-function-info :name "getMilliseconds" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCMilliseconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.25
   (define (date-prototype-getutcmilliseconds this::JsDate)
      (with-access::JsDate this (time)
	 (if time
	     (date-millisecond (js-date-val this))
	     +nan.0)))
	 
   (js-bind! %this obj (& "getUTCMilliseconds")
      :value (js-make-function %this date-prototype-getutcmilliseconds
		(js-function-arity date-prototype-getutcmilliseconds)
		(js-function-info :name "getUTCMilliseconds" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getTimezoneOffset
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.26
   (define (date-prototype-gettimezoneoffset this::JsDate)
      (with-access::JsDate this (time)
	 (if time
	     (let ((val (js-date-val this)))
		(negfx (/fx (date-timezone val) 60)))
	     +nan.0)))

   (js-bind! %this obj (& "getTimezoneOffset")
      :value (js-make-function %this date-prototype-gettimezoneoffset
		(js-function-arity date-prototype-gettimezoneoffset)
		(js-function-info :name "getTimezoneOffset" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setTime
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.27
   (define (date-prototype-settime this::JsDate tm)
      (with-access::JsDate this (time %val)
	 (let ((s (js-tonumber tm %this)))
	    (cond
	       ((fixnum? s)
		(set! %val #f)
		(set! time (fixnum->llong s))
		s)
	       ((flonum? s)
		(if (nanfl? s)
		    (begin
		       (set! %val #f)
		       (set! time #f)
		       s)
		    (begin
		       (set! %val #f)
		       (set! time (flonum->llong s))
		       s)))))))

   (js-bind! %this obj (& "setTime")
      :value (js-make-function %this date-prototype-settime
		(js-function-arity date-prototype-settime)
		(js-function-info :name "setTime" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.4
   (define (date-prototype-getyear this::JsDate)
      (with-access::JsDate this (time)
	 (if time
	     (let ((val (js-date-val this)))
		(-fx (date-year val) 1900))
	     +nan.0)))
   
   (js-bind! %this obj (& "getYear")
      :value (js-make-function %this date-prototype-getyear
		(js-function-arity date-prototype-getyear)
		(js-function-info :name "getYear" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.5
   (define (date-prototype-setyear this::JsDate year)
      (with-access::JsDate this (time %val)
	 (let ((r2 (js-tonumber year %this)))
	    (if (and (real? r2) (nanfl? r2))
		(begin
		   (set! %val #f)
		   (set! time #f)
		   r2)
		(let* ((val (js-date-val this))
		       (r2i (js-tointeger r2 %this))
		       (r4 (if (and (>=fx r2i 0) (<=fx r2i 99))
			       (+fx r2i 1900)
			       r2))
		       (r (if (date? val)
			      (date-update! val :year r4)
			      (make-date :year r4))))
		   (set! %val r)
		   (set! time (date->milliseconds r))
		   (js-time->milliseconds time))))))

   (js-bind! %this obj (& "setYear")
      :value (js-make-function %this date-prototype-setyear
		(js-function-arity date-prototype-setyear)
		(js-function-info :name "setYear" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)

   ;; setMilliseconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.28
   (define (date-prototype-setmilliseconds this::JsDate ms)
      (with-access::JsDate this (time %val)
	 (if time
	     (begin
		(set! %val (date-setmilliseconds! (js-date-val this) ms %this))
		(if (date? %val)
		    (begin
		       (set! time (date->milliseconds %val))
		       (js-time->milliseconds time))
		    (begin
		       (set! time #f)
		       (set! %val #f)
		       +nan.0)))
	     time)))

   (js-bind! %this obj (& "setMilliseconds")
      :value (js-make-function %this date-prototype-setmilliseconds
		(js-function-arity date-prototype-setmilliseconds)
		(js-function-info :name "setMilliseconds" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setUTCMilliseconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.29
   (define (date-prototype-setutcmilliseconds this::JsDate ms)
      (with-access::JsDate this (time %val)
	 (if time
	     (begin
		(set! %val (date-setmilliseconds! (js-date-val this) ms %this))
		(if (date? %val)
		    (begin
		       (set! time (date->milliseconds %val))
		       (js-time->milliseconds time))
		    (begin
		       (set! %val #f)
		       (set! time #f)
		       +nan.0)))
	     time)))

   (js-bind! %this obj (& "setUTCMilliseconds")
      :value (js-make-function %this date-prototype-setutcmilliseconds
		(js-function-arity date-prototype-setutcmilliseconds)
		(js-function-info :name "setUTCMilliseconds" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)

   ;; setSeconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.30
   (define (date-prototype-setseconds this::JsDate sec ms)
      (with-access::JsDate this (time %val)
	 (if time
	     (if (eq? ms (js-undefined))
		 (begin
		    (set! %val (date-setseconds! (js-date-val this) sec %this))
		    (if (date? %val)
			(begin
			   (set! time (date->milliseconds %val))
			   (js-time->milliseconds time))
			(begin
			   (set! %val #f)
			   (set! time #f)
			   +nan.0)))
		 (begin
		    (set! %val (date-setmilliseconds! (js-date-val this) ms %this))
		    (if (date? %val)
			(begin
			   (set! %val (date-setseconds! %val sec %this))
			   (set! time (date->milliseconds %val))
			   (js-time->milliseconds time))
			(begin
			   (set! %val #f)
			   (set! time #f)
			   +nan.0))))
	     time)))

   (js-bind! %this obj (& "setSeconds")
      :value (js-make-function %this date-prototype-setseconds
		(js-function-arity date-prototype-setseconds)
		(js-function-info :name "setSeconds" :len 2))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setUTCSeconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.31
   (define (date-prototype-setutcseconds this::JsDate sec ms)
      (with-access::JsDate this (time %val)
	 (if time
	     (if (eq? ms (js-undefined))
		 (begin
		    (set! %val (date-setseconds! (js-date-val this) sec %this))
		    (if (date? %val)
			(begin
			   (set! time (date->milliseconds %val))
			   (js-time->milliseconds time))
			(begin
			   (set! %val #f)
			   (set! time #f)
			   +nan.0)))
		 (begin
		    (set! %val (date-setmilliseconds! (js-date-val this) ms %this))
		    (if (date? %val)
			(begin
			   (set! %val (date-setseconds! %val sec %this))
			   (set! time (date->milliseconds %val))
			   (js-time->milliseconds time))
			(begin
			   (set! %val #f)
			   (set! time #f)
			   +nan.0))))
	     time)))

   (js-bind! %this obj (& "setUTCSeconds")
      :value (js-make-function %this date-prototype-setutcseconds
		(js-function-arity date-prototype-setutcseconds)
		(js-function-info :name "setUTCSeconds" :len 2))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   
   
   ;; setMinutes
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.32
   (js-bind! %this obj (& "setMinutes")
      :value (js-make-function %this
		(lambda (this min sec ms)
		   (js-date-setminutes this min sec ms %this))
		(js-function-arity 3 0)
		(js-function-info :name "setMinutes" :len 3))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setUTCMinutes
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.33
   (js-bind! %this obj (& "setUTCMinutes")
      :value (js-make-function %this
		(lambda (this min sec ms)
		   (js-date-setutcminutes this min sec ms %this))
		(js-function-arity 3 0)
		(js-function-info :name "setUTCMinutes" :len 3))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setHours
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.34
   (define (date-prototype-sethours this::JsDate hour min sec ms)
      (with-access::JsDate this (time %val)
	 (if time
	     (let ((hour (js-tonumber hour %this))
		   (min (unless (eq? min (js-undefined)) (js-tonumber min %this)))
		   (sec (unless (eq? sec (js-undefined)) (js-tonumber sec %this))))
		(if (and (flonum? hour) (nanfl? hour))
		    (begin
		       (set! %val #f)
		       (set! time #f)
		       hour)
		    (begin
		       (set! %val (date-update! (js-date-val this)
				     :hour (->fixnum-safe hour)
				     :min (->fixnum-safe min)
				     :sec (->fixnum-safe sec)))
		       (set! time (date->milliseconds %val))
		       (js-time->milliseconds time))))
	     time)))

   (js-bind! %this obj (& "setHours")
      :value (js-make-function %this date-prototype-sethours
		(js-function-arity date-prototype-sethours)
		(js-function-info :name "setHours" :len 4))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setUTCHours
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.35
   (define (date-prototype-setutchours this::JsDate hour min sec ms)
      (with-access::JsDate this (time %val)
	 (if time
	     (let ((hour (js-tonumber hour %this)))
		(if (and (flonum? hour) (nanfl? hour))
		    (begin
		       (set! %val #f)
		       (set! time #f)
		       hour)
		    (let* ((min (unless (eq? min (js-undefined)) (js-tonumber min %this)))
			   (sec (unless (eq? sec (js-undefined)) (js-tonumber sec %this)))
			   (dt (date-update! (js-date-val this)
				  :hour (->fixnum-safe hour)
				  :min (->fixnum-safe min)
				  :sec (->fixnum-safe sec)))
			   (ms (date->milliseconds dt)))
		       (set! %val
			  (milliseconds->date
			     (+llong ms
				(*llong #l1000
				   (fixnum->llong (date-timezone dt))))))
		       (set! time (date->milliseconds %val))
		       (js-time->milliseconds time))))
	     time)))

   (js-bind! %this obj (& "setUTCHours")
      :value (js-make-function %this date-prototype-setutchours
		(js-function-arity date-prototype-setutchours)
		(js-function-info :name "setUTCHours" :len 4))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setDate
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.36
   (define (date-prototype-setdate this::JsDate date)
      (with-access::JsDate this (time %val)
	 (if time
	     (let* ((date (js-tonumber date %this))
		    (day (cond
			    ((fixnum? date) date)
			    ((flonum? date) (flonum->fixnum date))
			    (else 1))))
		(if (and (flonum? date) (nanfl? date))
		    (begin
		       (set! %val #f)
		       (set! time #f)
		       date)
		    (begin
		       (set! %val (date-update! (js-date-val this)
				     :day (->fixnum-safe day)))
		       (set! time (date->milliseconds %val))
		       (js-time->milliseconds time))))
	     time)))

   (js-bind! %this obj (& "setDate")
      :value (js-make-function %this date-prototype-setdate
		(js-function-arity date-prototype-setdate)
		(js-function-info :name "setDate" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setUTCDate
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.37
   (define (date-prototype-setutcdate this::JsDate date)
      (with-access::JsDate this (time %val)
	 (if time
	     (let ((date (js-tonumber date %this)))
		(if (and (flonum? date) (nanfl? date))
		    (begin
		       (set! %val #f)
		       (set! time #f)
		       date)
		    (begin
		       (set! %val (date-update! (js-date-val this)
				     :day (->fixnum-safe date)))
		       (set! time (date->milliseconds %val))
		       (js-time->milliseconds time))))
	     time)))

   (js-bind! %this obj (& "setUTCDate")
      :value (js-make-function %this date-prototype-setutcdate
		(js-function-arity date-prototype-setutcdate)
		(js-function-info :name "setUTCDate" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setMonth
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.38
   (define (date-prototype-setmonth this::JsDate month date)
      (with-access::JsDate this (time %val)
	 (if time
	     (let* ((val (js-date-val this))
		    (month (js-tonumber month %this))
		    (day (if (eq? date (js-undefined))
			     (date-day val)
			     (js-tonumber date %this))))
		(if (and (flonum? month) (nanfl? month))
		    (begin
		       (set! %val #f)
		       (set! time #f)
		       month)
		    (let ((nval (date-update! val
				   :month (->fixnum-safe (+ 1 month))
				   :day (->fixnum-safe day))))
		       (set! %val nval)
		       (set! time (date->milliseconds %val))
		       (js-time->milliseconds time))))
	     time)))
   
   (js-bind! %this obj (& "setMonth")
      :value (js-make-function %this date-prototype-setmonth
		(js-function-arity date-prototype-setmonth)
		(js-function-info :name "setMonth" :len 2))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setUTCMonth
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.39
   (define (date-prototype-setutcmonth this::JsDate month date)
      (with-access::JsDate this (time %val)
	 (if time
	     (let* ((val (js-date-val this))
		    (month (js-tonumber month %this))
		    (date (if (eq? date (js-undefined))
			      (date-day val)
			      (js-tonumber date %this))))
		(if (and (flonum? month) (nanfl? month))
		    (begin
		       (set! %val #f)
		       (set! time #f)
		       month)
		    (let ((val (date->gmtdate! val)))
		       (set! %val (date-update! val
				    :month (->fixnum-safe (+ 1 month))
				    :day (->fixnum-safe date)))
		       (set! time (date->milliseconds %val))
		       (js-time->milliseconds time))))
	     time)))
   
   (js-bind! %this obj (& "setUTCMonth")
      :value (js-make-function %this date-prototype-setutcmonth
		(js-function-arity date-prototype-setutcmonth)
		(js-function-info :name "setUTCMonth" :len 2))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setFullYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.40
   (define (date-prototype-setfullyear this::JsDate year month date)
      (with-access::JsDate this (time %val)
	 (if time
	     (let ((year (js-tonumber year %this))
		   (month (unless (eq? month (js-undefined))
			     (js-tonumber month %this)))
		   (date (unless (eq? date (js-undefined))
			    (js-tonumber date %this))))
		(if (and (flonum? year) (nanfl? year))
		    (begin
		       (set! %val #f)
		       (set! time #f)
		       year)
		    (begin
		       (set! %val (date-update! (js-date-val this)
				     :year (->fixnum-safe year)
				     :month (->fixnum-safe month)
				     :day (->fixnum-safe date)))
		       (set! time (date->milliseconds %val))
		       (js-time->milliseconds time))))
	     ;; 1. Let t be the result of LocalTime(this time value)
	     ;; but if this time value is NaN, let t be +0.
	     (let ((year (js-tonumber year %this))
		   (month (if (eq? month (js-undefined))
			      1
			      (js-tonumber month %this)))
		   (date (if (eq? date (js-undefined))
			     1
			     (js-tonumber date %this))))
		(if (and (flonum? year) (nanfl? year))
		    (begin
		       (set! %val #f)
		       (set! time #f)
		       year)
		    (begin
		       (set! %val (make-date
				     :year (->fixnum-safe year)
				     :month (->fixnum-safe month)
				     :day (->fixnum-safe date)
				     :hour 0 :min 0 :sec 0))
		       (set! time (date->milliseconds %val))
		       (js-time->milliseconds time)))))))

   (js-bind! %this obj (& "setFullYear")
      :value (js-make-function %this date-prototype-setfullyear
		(js-function-arity date-prototype-setfullyear)
		(js-function-info :name "setFullYear" :len 3))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setUTCFullYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.41
   (define (date-prototype-setutcfullyear this::JsDate year month date)
      (with-access::JsDate this (time %val)
	 (if time
	     (let ((year (js-tonumber year %this))
		   (month (unless (eq? month (js-undefined)) (js-tonumber month %this)))
		   (date (unless (eq? date (js-undefined)) (js-tonumber date %this))))
		(if (and (flonum? year) (nanfl? year))
		    (begin
		       (set! %val #f)
		       (set! time #f)
		       year)
		    (begin
		       (set! %val (date-update! (js-date-val this)
				    :year (->fixnum-safe year)
				    :month (->fixnum-safe month)
				    :day (->fixnum-safe date)))
		       (set! time (date->milliseconds %val))
		       (js-time->milliseconds time))))
	     time)))

   (js-bind! %this obj (& "setUTCFullYear")
      :value (js-make-function %this date-prototype-setutcfullyear
		(js-function-arity date-prototype-setutcfullyear)
		(js-function-info :name "setUTCFullYear" :len 3))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; toGMTString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.6
   (define (date-prototype-togmtstring this::JsDate)
      (date-prototype-toutcstring this))

   (js-bind! %this obj (& "toGMTString")
      :value (js-make-function %this date-prototype-togmtstring
		(js-function-arity date-prototype-togmtstring)
		(js-function-info :name "toGMTString" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   obj)

;*---------------------------------------------------------------------*/
;*    js-time->milliseconds ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-time->milliseconds ms::llong)
   (cond-expand
      ((or bint61 bint64)
       (llong->fixnum ms))
      (else
       (llong->flonum ms))))

;*---------------------------------------------------------------------*/
;*    js-date->milliseconds ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-date->milliseconds dt::date)
   (js-time->milliseconds (date->milliseconds dt)))

;*---------------------------------------------------------------------*/
;*    js-date->jsstring ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-date->jsstring val)
   (js-ascii->jsstring (date->rfc2822-date val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-tostring ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.2     */
;*---------------------------------------------------------------------*/
(define (date-prototype-tostring this::JsDate)
   (with-access::JsDate this (time)
      (if time
	  (let ((val (js-date-val this)))
	     (js-date->jsstring val))
	  (js-ascii->jsstring "Invalid Date"))))

;*---------------------------------------------------------------------*/
;*    date->local-date ...                                             */
;*---------------------------------------------------------------------*/
(define (date->local-date dt::date)
   (milliseconds->date (date->milliseconds dt)))

;*---------------------------------------------------------------------*/
;*    date->utc-date ...                                               */
;*---------------------------------------------------------------------*/
(define (date->utc-date dt::date)
   (milliseconds->gmtdate (date->milliseconds dt)))

;*---------------------------------------------------------------------*/
;*    js-date->jsdate ...                                              */
;*---------------------------------------------------------------------*/
(define (js-date->jsdate val::date %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-date js-date-cmap js-date-prototype)
      (let ((dt (instantiateJsDate
		   (cmap js-date-cmap)
		   (__proto__ js-date-prototype))))
	 (with-access::JsDate dt (time %val)
	    (set! %val val)
	    (set! time (date->milliseconds val))
	    dt))))

;*---------------------------------------------------------------------*/
;*    ->fixnum-safe ...                                                */
;*---------------------------------------------------------------------*/
(define (->fixnum-safe num)
   (when num (->fixnum num)))

;*---------------------------------------------------------------------*/
;*    blit-fixnum! ...                                                 */
;*---------------------------------------------------------------------*/
(define (blit-fixnum! buffer::bstring n idx padding::long)
   
   (define (digit->char n) (integer->char (+fx n (char->integer #\0))))
   
   (let loop ((n n)
	      (p (-fx padding 1)))
      (string-set! buffer (+fx idx p) (digit->char (remainderfx n 10)))
      (when (>fx p 0) (loop (/fx n 10) (-fx p 1)))))

;*---------------------------------------------------------------------*/
;*    js-date-now ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.4.4     */
;*---------------------------------------------------------------------*/
(define (js-date-now)
   (cond-expand
      ((or bint61 bint64)
       (llong->fixnum (current-milliseconds)))
      (else
       (llong->flonum (current-milliseconds)))))

;*---------------------------------------------------------------------*/
;*    js-plain-date? ...                                               */
;*---------------------------------------------------------------------*/
(define (js-plain-date? this)
   (and (js-date? this) (or (js-object-mode-plain? this) #t)))

;*---------------------------------------------------------------------*/
;*    js-date-gettime ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.9     */
;*---------------------------------------------------------------------*/
(define (js-date-gettime this::JsDate)
   (with-access::JsDate this (time)
      (if time
	  (js-time->milliseconds time)
	  +nan.0)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-gettime ...                                        */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-gettime this::obj %this cache)
   (if (js-plain-date? this)
       (js-date-gettime this)
       (js-call0 %this
	  (js-get-name/cache this (& "getTime") #f %this cache)
	  this)))

;*---------------------------------------------------------------------*/
;*    js-date-getfullyear ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.10    */
;*---------------------------------------------------------------------*/
(define (js-date-getfullyear this::JsDate)
   (with-access::JsDate this (time)
      (if time
	  (let ((val (js-date-val this)))
	     (date-year val))
	  +nan.0)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-getfullyear ...                                    */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-getfullyear this::obj %this cache)
   (if (js-plain-date? this)
       (js-date-getfullyear this)
       (js-call0 %this
	  (js-get-name/cache this (& "getFullYear") #f %this cache)
	  this)))

;*---------------------------------------------------------------------*/
;*    js-date-getmonth ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.12    */
;*---------------------------------------------------------------------*/
(define (js-date-getmonth this::JsDate)
   (with-access::JsDate this (time)
      (if time
	  (let ((val (js-date-val this)))
	     (-fx (date-month val) 1))
	  +nan.0)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-getmonth ...                                       */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-getmonth this::obj %this cache)
   (if (js-plain-date? this)
       (js-date-getmonth this)
       (js-call0 %this
	  (js-get-name/cache this (& "getMonth") #f %this cache)
	  this)))

;*---------------------------------------------------------------------*/
;*    js-date-getdate ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.14    */
;*---------------------------------------------------------------------*/
(define (js-date-getdate this::JsDate)
   (with-access::JsDate this (time)
      (if time
	  (let ((val (js-date-val this)))
	     (date-day val))
	  +nan.0)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-getdate ...                                        */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-getdate this::obj %this cache)
   (if (js-plain-date? this)
       (js-date-getdate this)
       (js-call0 %this
	  (js-get-name/cache this (& "getDate") #f %this cache)
	  this)))

;*---------------------------------------------------------------------*/
;*    js-date-getutcdate ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.15    */
;*---------------------------------------------------------------------*/
(define (js-date-getutcdate this::JsDate)
   (with-access::JsDate this (time)
      (if time
	  (let* ((val (js-date-val this))
		 (tz (date-timezone val))
		 (d (seconds->date (-elong (date->seconds val) tz))))
	     (date-day d))
	  +nan.0)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-getutcdate ...                                     */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-getutcdate this::obj %this cache)
   (if (js-plain-date? this)
       (js-date-getutcdate this)
       (js-call0 %this
	  (js-get-name/cache this (& "getUTCDate") #f %this cache)
	  this)))

;*---------------------------------------------------------------------*/
;*    js-date-gethours ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.18    */
;*---------------------------------------------------------------------*/
(define (js-date-gethours this::JsDate)
   (with-access::JsDate this (time)
      (if time
	  (let ((val (js-date-val this)))
	     (date-hour val))
	  +nan.0)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-gethours ...                                       */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-gethours this::obj %this cache)
   (if (js-plain-date? this)
       (js-date-gethours this)
       (js-call0 %this
	  (js-get-name/cache this (& "getHours") #f %this cache)
	  this)))

;*---------------------------------------------------------------------*/
;*    js-date-getminutes ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.20    */
;*---------------------------------------------------------------------*/
(define (js-date-getminutes this::JsDate)
   (with-access::JsDate this (time)
      (if time
	  (let ((val (js-date-val this)))
	     (date-minute val))
	  +nan.0)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-getminutes ...                                     */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-getminutes this::obj %this cache)
   (if (js-plain-date? this)
       (js-date-getminutes this)
       (js-call0 %this
	  (js-get-name/cache this (& "getminutes") #f %this cache)
	  this)))

;*---------------------------------------------------------------------*/
;*    js-date-getutcminutes ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.21    */
;*---------------------------------------------------------------------*/
(define (js-date-getutcminutes this::JsDate)
   (with-access::JsDate this (time)
      (if time
	  (let ((val (js-date-val this)))
	     (date-minute val))
	  +nan.0)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-getutcminutes ...                                  */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-getutcminutes this::obj %this cache)
   (if (js-plain-date? this)
       (js-date-getutcminutes this)
       (js-call0 %this
	  (js-get-name/cache this (& "getUTCminutes") #f %this cache)
	  this)))

;*---------------------------------------------------------------------*/
;*    js-date-getseconds ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.22    */
;*---------------------------------------------------------------------*/
(define (js-date-getseconds this::JsDate)
   (with-access::JsDate this (time)
      (if time
	  (let ((val (js-date-val this)))
	     (date-second val))
	  +nan.0)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-getseconds ...                                     */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-getseconds this::obj %this cache)
   (if (js-plain-date? this)
       (js-date-getseconds this)
       (js-call0 %this
	  (js-get-name/cache this (& "getseconds") #f %this cache)
	  this)))

;*---------------------------------------------------------------------*/
;*    js-date-getmilliseconds ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.24    */
;*---------------------------------------------------------------------*/
(define (js-date-getmilliseconds this::JsDate)
   (with-access::JsDate this (time)
      (if time
	  (let ((val (js-date-val this)))
	     (llong->fixnum (date-millisecond val)))
	  +nan.0)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-getmilliseconds ...                                */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-getmilliseconds this::obj %this cache)
   (if (js-plain-date? this)
       (js-date-getmilliseconds this)
       (js-call0 %this
	  (js-get-name/cache this (& "getmilliseconds") #f %this cache)
	  this)))

;*---------------------------------------------------------------------*/
;*    date-setmilliseconds! ...                                        */
;*---------------------------------------------------------------------*/
(define (date-setmilliseconds! val ms %this)
   (let ((ms (js-tonumber ms %this)))
      (if (flonum? ms)
	  (if (nanfl? ms)
	      ms
	      (date-update-millisecond! val (flonum->fixnum ms)))
	  (date-update-millisecond! val ms))))

;*---------------------------------------------------------------------*/
;*    date-setseconds! ...                                             */
;*---------------------------------------------------------------------*/
(define (date-setseconds! val sec %this)
   (let ((sec (js-tonumber sec %this)))
      (if (flonum? sec)
	  (if (nanfl? sec)
	      sec
	      (date-update-second! val (flonum->fixnum sec)))
	  (date-update-second! val sec))))

;*---------------------------------------------------------------------*/
;*    date-setminutes! ...                                             */
;*---------------------------------------------------------------------*/
(define (date-setminutes! val min %this)
   (let ((min (js-tonumber min %this)))
      (if (flonum? min)
	  (if (nanfl? min)
	      min
	      (date-update-minute! val (flonum->fixnum min)))
	  (date-update-minute! val min))))

;*---------------------------------------------------------------------*/
;*    js-date-setminutes ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.32    */
;*---------------------------------------------------------------------*/
(define (js-date-setminutes this::JsDate min sec ms %this)
   (with-access::JsDate this (time %val)
      (if time
	  (let ((val (js-date-val this)))
	     (unless (eq? ms (js-undefined))
		(set! val (date-setmilliseconds! val ms %this)))
	     (when (and (date? val) (not (eq? sec (js-undefined))))
		(set! val (date-setseconds! val sec %this)))
	     (when (date? val)
		(set! val (date-setminutes! val min %this)))
	     (if (date? val)
		 (begin
		    (set! %val val)
		    (set! time (date->milliseconds %val))
		    (js-time->milliseconds time))
		 (begin
		    (set! %val #f)
		    (set! time #f)
		    +nan.0)))
	  time)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-setminutes ...                                     */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-setminutes this min sec ms %this cache)
   (if (js-plain-date? this)
       (js-date-setminutes this min sec ms %this)
       (js-call3 %this
	  (js-get-name/cache this (& "setMinutes") #f %this cache)
	  this min sec ms)))

;*---------------------------------------------------------------------*/
;*    js-date-setutcminutes ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.33    */
;*---------------------------------------------------------------------*/
(define (js-date-setutcminutes this::JsDate min sec ms %this)
   (with-access::JsDate this (time %val)
      (if time
	  (let ((val (js-date-val this)))
	     (unless (eq? ms (js-undefined))
		(set! val (date-setmilliseconds! val ms %this)))
	     (when (and (date? val) (not (eq? sec (js-undefined))))
		(set! val (date-setseconds! val sec %this)))
	     (when (date? val)
		(set! val (date-setminutes! val min %this)))
	     (if (date? val)
		 (begin
		    (set! %val val)
		    (set! time (date->milliseconds val))
		    (js-time->milliseconds time))
		 (begin
		    (set! %val #f)
		    (set! time #f)
		 +nan.0)))
	  time)))

;*---------------------------------------------------------------------*/
;*    js-date-maybe-setutcminutes ...                                  */
;*---------------------------------------------------------------------*/
(define (js-date-maybe-setutcminutes this min sec ms %this cache)
   (if (js-plain-date? this)
       (js-date-setutcminutes this min sec ms %this)
       (js-call3 %this
	  (js-get-name/cache this (& "setUTCMinutes") #f %this cache)
	  this min sec ms)))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
