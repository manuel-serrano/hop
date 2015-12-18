;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/date.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Fri Dec 18 08:58:13 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
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
   
   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_regexp
	   __hopscript_private
	   __hopscript_public
	   __hopscript_error)
   
   (export (js-init-date! ::JsObject)
	   (js-date->jsdate::JsDate ::date ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsDate ...                                   */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsDate
   (lambda (o)
      (with-access::JsDate o (val) val))
   (lambda (o %this)
      (js-date->jsdate o (or %this (js-initial-global-object)))))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsDate ...                                           */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsDate worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-date)
	 (let ((nobj (call-next-method)))
	    (with-access::JsDate nobj (__proto__ val)
	       (with-access::JsDate obj ((_val val))
		  (set! __proto__ (js-get js-date 'prototype %this))
		  (set! val (js-donate _val worker %_this))))
	    nobj))))

;*---------------------------------------------------------------------*/
;*    xml-write ::JsDate ...                                           */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::JsDate p backend)
   (with-access::JsDate obj (val)
      (display val p)))
		  
;*---------------------------------------------------------------------*/
;*    xml-unpack ::JsDate ...                                          */
;*    -------------------------------------------------------------    */
;*    Used when an JS object is to pack the arguments sent to          */
;*    an XML constructor.                                              */
;*---------------------------------------------------------------------*/
(define-method (xml-unpack o::JsDate)
   (with-access::JsDate o (val)
      (list val)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsDate ...                                     */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsDate op compile isexpr)
   (display "new Date(" op)
   (with-access::JsDate o (val)
      (if (date? val)
	  (display (llong->flonum (/llong (date->nanoseconds val) #l1000000)) op)
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
;*    js-init-date! ...                                                */
;*---------------------------------------------------------------------*/
(define (js-init-date! %this)
   ;; first, bind the builtin date prototype
   (with-access::JsGlobalObject %this (__proto__ js-date js-function)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 (define js-date-prototype
	    (instantiate::JsDate
	       (val (current-date))
	       (__proto__ __proto__)))
	 
	 (define (js-date-alloc constructor::JsFunction)
	    (instantiate::JsDate
	       (__proto__ (js-get constructor 'prototype %this))))

	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9
	 (define (js-date-construct this::JsDate . args)
	    
	    (define (js-date->jsdate v)
	       (with-access::JsDate this (val)
		  (set! val v)
		  this))

	    (if (any (lambda (a) (eq? a (js-undefined))) args)
		(instantiate::JsDate
		   (__proto__ js-date-prototype))
		(match-case args
		   ((?year ?month ?date ?hours ?minutes ?seconds ?ms)
		    (let* ((y (js-tonumber year %this))
			   (m (js-tonumber month %this))
			   (d (js-tonumber date %this))
			   (h (js-tonumber hours %this))
			   (mi (js-tonumber minutes %this))
			   (se (js-tonumber seconds %this))
			   (us (js-tonumber ms %this))
			   (ns (cond
				  ((fixnum? us)
				   (*llong #l1000000 (fixnum->llong us)))
				  ((flonum? us)
				   (*llong #l1000000
				      (fixnum->llong (flonum->fixnum us))))
				  (else
				   #l0))))
		       (js-date->jsdate
			  (when (and (fixnum? y) (>fx y 0)
				     (fixnum? m) (fixnum? d) (fixnum? h)
				     (fixnum? mi) (fixnum? se) (llong? ns))
			     (make-date
				:year y :month (+ m 1) :day d
				:hour h :min mi :sec se :nsec ns)))))
		   ((?year ?month ?date ?hours ?minutes ?seconds)
		    (let* ((y (js-tonumber year %this))
			   (m (js-tonumber month %this))
			   (d (js-tonumber date %this))
			   (h (js-tonumber hours %this))
			   (mi (js-tonumber minutes %this))
			   (se (js-tonumber seconds %this)))
		       (js-date->jsdate
			  (when (and (fixnum? y) (>fx y 0)
				     (fixnum? m) (fixnum? d) (fixnum? h)
				     (fixnum? mi) (fixnum? se))
			     (make-date
				:year y :month (+ m 1) :day d
				:hour h :min mi :sec se)))))
		   ((?year ?month ?date ?hours ?minutes)
		    (let* ((y (js-tonumber year %this))
			   (m (js-tonumber month %this))
			   (d (js-tonumber date %this))
			   (h (js-tonumber hours %this))
			   (mi (js-tonumber minutes %this)))
		       (js-date->jsdate
			  (when (and (fixnum? y) (>fx y 0)
				     (fixnum? m) (fixnum? d) (fixnum? h)
				     (fixnum? mi))
			     (make-date
				:year y :month (+ m 1) :day d
				:hour h :min mi :sec 0)))))
		   ((?year ?month ?date ?hours)
		    (let* ((y (js-tonumber year %this))
			   (m (js-tonumber month %this))
			   (d (js-tonumber date %this))
			   (h (js-tonumber hours %this)))
		       (js-date->jsdate
			  (when (and (fixnum? y) (>fx y 0)
				     (fixnum? m) (fixnum? d) (fixnum? h))
			     (make-date
				:year y :month (+ m 1) :day d
				:hour h :min 0 :sec 0)))))
		   ((?year ?month ?date)
		    (let* ((y (js-tonumber year %this))
			   (m (js-tonumber month %this))
			   (d (js-tonumber date %this)))
		       (js-date->jsdate
			  (when (and (fixnum? y) (>fx y 0)
				     (fixnum? m) (fixnum? d))
			     (make-date
				:year y :month (+ m 1) :day d
				:hour 0 :min 0 :sec 0)))))
		   ((?year ?month)
		    (let* ((y (js-tonumber year %this))
			   (m (js-tonumber month %this)))
		       (js-date->jsdate
			  (when (and (fixnum? y) (>fx y 0)
				     (fixnum? m))
			     (make-date
				:year y :month (+ m 1)
				:day 1 :hour 0 :min 0 :sec 0)))))
		   ((?value)
		    (let ((v (js-toprimitive value 'any %this)))
		       (cond
			  ((js-jsstring? v)
			   (js-date->jsdate
			      (parse-date (js-jsstring->string v))))
			  ((number? v)
			   (if (flonum? v)
			       (js-date->jsdate 
				  (if (or (nanfl? v) (=fl v +inf.0) (=fl v -inf.0))
				      +nan.0
				      (nanoseconds->date
					 (*llong
					    #l1000000
					    (flonum->llong (round v))))))
			       (let ((n (cond
					   ((fixnum? v) (fixnum->llong v))
					   ((llong? v) v)
					   ((elong? v) (elong->llong v))
					   (else #l0))))
				  (js-date->jsdate
				     (nanoseconds->date
					(*llong #l1000000 n))))))
			  (else
			   (js-date->jsdate v)))))
		   (else
		    (js-date->jsdate (current-date))))))

	 ;; create a HopScript object
	 (define (%js-date this . args)
	    (let ((dt (js-date-construct (js-date-alloc js-date))))
	       (js-call0 %this (js-get dt 'toString %this) dt)))
	 
	 (set! js-date
	    (js-make-function %this %js-date 7 'Date
	       :__proto__ js-function-prototype
	       :prototype js-date-prototype
	       :alloc js-date-alloc
	       :construct js-date-construct))

	 ;; parse
	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.4.2
	 (define (js-date-parse this str)
	    (*fl 1000.
	       (elong->flonum
		  (date->seconds (parse-date (js-tostring str %this))))))

	 (js-bind! %this js-date 'parse
	    :value (js-make-function %this js-date-parse 1 'parse)
	    :writable #t
	    :configurable #t
	    :enumerable #f)

	 ;; UTC
	 (define (js-date-utc this . l)
	    (apply js-date-construct (js-date-alloc js-date) l))
	 
	 (js-bind! %this js-date 'UTC
	    :value (js-make-function %this js-date-utc 7 'UTC)
	    :writable #t
	    :configurable #t
	    :enumerable #f)

	 ;; now
	 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.4.4
	 (define (js-date-now this)
	    (let ((ns (llong->flonum (current-nanoseconds))))
	       (roundfl (/fl ns 1000000.))))
	 
	 (js-bind! %this js-date 'now
	    :value (js-make-function %this js-date-now 0 'now)
	    :writable #t
	    :configurable #t
	    :enumerable #f)
	 
	 ;; prototype properties
	 (init-builtin-date-prototype! %this js-date js-date-prototype)
	 ;; bind Date in the global object
	 (js-bind! %this %this 'Date
	    :configurable #f :enumerable #f :value js-date)
	 js-date)))

;*---------------------------------------------------------------------*/
;*    parse-date ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15    */
;*---------------------------------------------------------------------*/
(define (parse-date v::bstring)
   (let ((ip (open-input-string v)))
      (unwind-protect
	 (with-handler
	    (lambda (e)
	       (input-port-reopen! ip)
	       (with-handler
		  (lambda (e)
		     (current-date))
		  (iso8601-parse-date ip)))
	    (rfc2822-parse-date ip))
	 (close-input-port ip))))

;*---------------------------------------------------------------------*/
;*    init-builtin-date-prototype! ...                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5       */
;*---------------------------------------------------------------------*/
(define (init-builtin-date-prototype! %this js-date obj)
   
   ;; constructor
   (js-bind! %this obj 'constructor
      :value js-date
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; toString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.2
   (define (date-prototype-tostring this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (js-string->jsstring
		(date->rfc2822-date (seconds->date (date->seconds val))))
	     (js-string->jsstring "Invalid date"))))
   
   (js-bind! %this obj 'toString
      :value (js-make-function %this date-prototype-tostring 0 'toString)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; toDateString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.3
   (define (date-prototype-todatestring this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (js-string->jsstring
		(format "~a ~a ~2,0d ~d"
		   (day-aname (date-wday val))
		   (month-aname (date-month val))
		   (date-day val)
		   (date-year val)))
	     (js-string->jsstring "Invalid date"))))

   (js-bind! %this obj 'toDateString
      :value (js-make-function %this date-prototype-todatestring
		0 'toDateString)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; toTimeString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.4
   (define (date-prototype-totimestring this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (js-string->jsstring
		(format "~2,0d:~2,0d:~2,0d ~a"
		   (date-hour val)
		   (date-minute val)
		   (date-second val)
		   (date-timezone val)))
	     (js-string->jsstring "Invalid date"))))

   (js-bind! %this obj 'toTimeString
      :value (js-make-function %this date-prototype-totimestring
		0 'toTimeString)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; toLocaleString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.5
   (define (date-prototype-tolocalestring this::JsDate)
      (date-prototype-tostring this))

   (js-bind! %this obj 'toLocaleString
      :value (js-make-function %this date-prototype-tolocalestring
		0 'toLocaleString)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; toLocaleDateString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.6
   (define (date-prototype-tolocaledatestring this::JsDate)
      date-prototype-todatestring this)

   (js-bind! %this obj 'toLocaleDateString
      :value (js-make-function %this date-prototype-tolocaledatestring
		0 'toLocaleDateString)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; toLocaleTimeString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.7
   (define (date-prototype-tolocaletimestring this::JsDate)
      (date-prototype-totimestring this))

   (js-bind! %this obj 'toLocaleTimeString
      :value (js-make-function %this date-prototype-tolocaletimestring
		0 'toLocaleTimeString)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; toUTCString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.42
   (define (date-prototype-toutcstring this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (js-string->jsstring (date->utc-string val))
	     "Invalid date")))

   (js-bind! %this obj 'toUTCString
      :value (js-make-function %this date-prototype-toutcstring 0 'toUTCString)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; toISOString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.43
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15
   (define (date-prototype-toisostring this)
      (if (not (isa? this JsDate))
	  (js-raise-type-error %this "Not a date ~s" (typeof this))
	  (with-access::JsDate this (val ms)
	     (if (date? val)
		 (let loop ((val val))
		    (if (=fx (date-timezone val) 0)
			(js-string->jsstring
			   (format "~4,0d-~2,0d-~2,0dT~2,0d:~2,0d:~2,0d.~3,0dZ"
			      (date-year val)
			      (date-month val)
			      (date-day val)
			      (date-hour val)
			      (date-minute val)
			      (date-second val)
			      (llong->fixnum
				 (/llong (date-nanosecond val) #l1000000))))
			(loop (date->utc-date val))))
		 (js-raise-range-error %this "Invalid date ~s" val)))))
   
   (js-bind! %this obj 'toISOString
      :value (js-make-function %this date-prototype-toisostring 0 'toISOString)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; toJSON
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.44
   (define (date-prototype-toJSON this::JsDate)
      (let* ((o (js-toobject %this this))
	     (tv (js-toprimitive o 'number %this)))
	 (if (and (number? tv) (not (integer? tv)))
	     (js-null)
	     (let ((p (js-get o 'toISOString %this)))
		(if (isa? p JsFunction)
		    (js-call0 %this p o)
		    (js-raise-type-error %this
		       "toJSON: argument not a function ~s" p))))))

   (js-bind! %this obj 'toJSON
      :value (js-make-function %this date-prototype-toJSON 1 'toJSON)
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; valueOf
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.8
   (define (date-prototype-valueof this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (llong->flonum (/llong (date->nanoseconds val) #l1000000))
	     +nan.0)))
	 
   (js-bind! %this obj 'valueOf
      :value (js-make-function %this date-prototype-valueof 0 'valueOf)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getTime
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.9
   (define (date-prototype-gettime this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date->milliseconds val)
	     +nan.0)))
 	 
   (js-bind! %this obj 'getTime
      :value (js-make-function %this date-prototype-gettime 0 'getTime)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getFullYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.10
   (define (date-prototype-getfullyear this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-year val)
	     +nan.0)))
	 
   (js-bind! %this obj 'getFullYear
      :value (js-make-function %this date-prototype-getfullyear 0 'getFullYear)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getUTCFullYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.11
   (define (date-prototype-getutcfullyear this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let* ((tz (date-timezone val))
		    (d (seconds->date (-elong (date->seconds val) tz))))
		(date-year d))
	     +nan.0)))
	 
   (js-bind! %this obj 'getUTCFullYear
      :value (js-make-function %this date-prototype-getutcfullyear 0 'getUTCFullYear)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getMonth
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.12
   (define (date-prototype-getmonth this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (-fx (date-month val) 1)
	     +nan.0)))
	 
   (js-bind! %this obj 'getMonth
      :value (js-make-function %this date-prototype-getmonth 0 'getMonth)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getUTCMonth
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.13
   (define (date-prototype-getutcmonth this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let* ((tz (date-timezone val))
		    (d (seconds->date (-elong (date->seconds val) tz))))
		(-fx (date-month d) 1))
	     +nan.0)))
	 
   (js-bind! %this obj 'getUTCMonth
      :value (js-make-function %this date-prototype-getutcmonth 0 'getUTCMonth)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getDate
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.14
   (define (date-prototype-getdate this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-day val)
	     +nan.0)))
	 
   (js-bind! %this obj 'getDate
      :value (js-make-function %this date-prototype-getdate 0 'getDate)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getUTCDate
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.15
   (define (date-prototype-getutcdate this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let* ((tz (date-timezone val))
		    (d (seconds->date (-elong (date->seconds val) tz))))
		(date-day d))
	     +nan.0)))
	 
   (js-bind! %this obj 'getUTCDate
      :value (js-make-function %this date-prototype-getutcdate 0 'getUTCDate)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getDay
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.16
   (define (date-prototype-getday this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (-fx (date-wday val) 1)
	     +nan.0)))
	 
   (js-bind! %this obj 'getDay
      :value (js-make-function %this date-prototype-getday 0 'getDay)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getUTCDay
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.17
   (define (date-prototype-getutcday this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (-fx (date-wday val) 1)
	     +nan.0)))
	 
   (js-bind! %this obj 'getUTCDay
      :value (js-make-function %this date-prototype-getutcday 0 'getUTCDay)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getHours
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.18
   (define (date-prototype-gethours this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-hour val)
	     +nan.0)))
	 
   (js-bind! %this obj 'getHours
      :value (js-make-function %this date-prototype-gethours 0 'getHours)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getUTCHours
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.19
   (define (date-prototype-getutchours this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let* ((tz (date-timezone val))
		    (tzh (/fx tz 3600))
		    (n (-fx (date-hour val) tzh)))
		(cond
		   ((<fx n 0) (+fx 24 n))
		   ((>fx n 23) (-fx n 23))
		   (else n)))
	     +nan.0)))
	 
   (js-bind! %this obj 'getUTCHours
      :value (js-make-function %this date-prototype-getutchours 0 'getUTCHours)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getMinutes
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.20
   (define (date-prototype-getminutes this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-minute val)
	     +nan.0)))
	 
   (js-bind! %this obj 'getMinutes
      :value (js-make-function %this date-prototype-getminutes 0 'getMinutes)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getUTCMinutes
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.21
   (define (date-prototype-getutcminutes this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-minute val)
	     +nan.0)))
	 
   (js-bind! %this obj 'getUTCMinutes
      :value (js-make-function %this date-prototype-getutcminutes 0 'getUTCMinutes)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getSeconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.22
   (define (date-prototype-getseconds this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-second val)
	     +nan.0)))
	 
   (js-bind! %this obj 'getSeconds
      :value (js-make-function %this date-prototype-getseconds 0 'getSeconds)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getUTCSeconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.23
   (define (date-prototype-getutcseconds this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-second val)
	     +nan.0)))
	 
   (js-bind! %this obj 'getUTCSeconds
      :value (js-make-function %this date-prototype-getutcseconds 0 'getUTCSeconds)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getMilliseconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.24
   (define (date-prototype-getmilliseconds this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (llong->fixnum (/llong (date-nanosecond val) #l1000000))
	     +nan.0)))
	 
   (js-bind! %this obj 'getMilliseconds
      :value (js-make-function %this date-prototype-getmilliseconds 0 'getMilliseconds)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getUTCMilliseconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.25
   (define (date-prototype-getutcmilliseconds this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (llong->fixnum (/llong (date-nanosecond val) #l1000000))
	     +nan.0)))
	 
   (js-bind! %this obj 'getUTCMilliseconds
      :value (js-make-function %this date-prototype-getutcmilliseconds 0 'getUTCMilliseconds)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getTimezoneOffset
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.26
   (define (date-prototype-gettimezoneoffset this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (negfx (/fx (date-timezone (seconds->date (date->seconds val))) 60))
	     +nan.0)))

   (js-bind! %this obj 'getTimezoneOffset
      :value (js-make-function %this date-prototype-gettimezoneoffset 0 'getTimezoneOffset)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setTime
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.27
   (define (date-prototype-settime this::JsDate time)
      (with-access::JsDate this (val)
	 (let ((s (js-tonumber time %this)))
	    (cond
	       ((fixnum? s)
		(set! val (seconds->date (fixnum->elong s)))
		(date->milliseconds val))
	       ((elong? s)
		(set! val (seconds->date s))
		(date->milliseconds val))
	       ((flonum? s)
		(if (nanfl? s)
		    (begin
		       (set! val s)
		       time)
		    (begin
		       (set! val (seconds->date (flonum->fixnum s)))
		       (date->milliseconds val))))))))

   (js-bind! %this obj 'setTime
      :value (js-make-function %this date-prototype-settime 1 'setTime)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; getYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.4
   (define (date-prototype-getyear this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (-fx (date-year val) 1900)
	     +nan.0)))
   
   (js-bind! %this obj 'getYear
      :value (js-make-function %this date-prototype-getyear 0 'getYear)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.5
   (define (date-prototype-setyear this::JsDate year)
      (with-access::JsDate this (val)
	 (let ((r2 (js-tonumber year %this)))
	    (if (and (real? r2) (nanfl? r2))
		(begin
		   (set! val #f)
		   r2)
		(let* ((r2i (js-tointeger r2 %this))
		       (r4 (if (and (>=fx r2i 0) (<=fx r2i 99))
			       (+fx r2i 1900)
			       r2))
		       (r (if (date? val)
			      (date-copy val :year r4)
			      (make-date :year r4))))
		   (set! val r)
		   r)))))

   (js-bind! %this obj 'setYear
      :value (js-make-function %this date-prototype-setyear 1 'setYear)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setMilliseconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.28
   (define (date-prototype-setmilliseconds this::JsDate ms)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((ms (js-tonumber ms %this)))
		(cond
		   ((flonum? ms)
		    (if (nanfl? ms)
			(begin
			   (set! val ms)
			   ms)
			(begin
			   (set! val (date-copy val :nsec (*llong #l1000000 (flonum->llong ms))))
			   (date->milliseconds val))))
		   ((fixnum? ms)
		    (set! val (date-copy val :nsec (*llong #l1000000 (fixnum->llong ms))))
		    (date->milliseconds val))
		   (else
		    (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setMilliseconds
      :value (js-make-function %this date-prototype-setmilliseconds 1 'setMilliseconds)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setUTCMilliseconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.29
   (define (date-prototype-setutcmilliseconds this::JsDate ms)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((ms (js-tonumber ms %this)))
		(cond
		   ((flonum? ms)
		    (if (nanfl? ms)
			(begin
			   (set! val ms)
			   ms)
			(begin
			   (set! val (date-copy val :nsec (*llong #l1000000 (flonum->llong ms))))
			   (date->milliseconds val))))
		   ((fixnum? ms)
		    (set! val (date-copy val :nsec (*llong #l1000000 (fixnum->llong ms))))
		    (date->milliseconds val))
		   (else
		    (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setUTCMilliseconds
      :value (js-make-function %this date-prototype-setutcmilliseconds 1 'setUTCMilliseconds)
      :writable #t
      :configurable #t
      :enumerable #f)

   ;; setSeconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.30
   (define (date-prototype-setseconds this::JsDate sec ms)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((sec (js-tonumber sec %this)))
		(if (and (flonum? sec) (nanfl? sec))
		    (begin
		       (set! val sec)
		       sec)
		    (begin
		       (set! val (date-copy val :sec sec))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setSeconds
      :value (js-make-function %this date-prototype-setseconds 2 'setSeconds)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setUTCSeconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.31
   (define (date-prototype-setutcseconds this::JsDate sec ms)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((sec (js-tonumber sec %this)))
		(if (and (flonum? sec) (nanfl? sec))
		    (begin
		       (set! val sec)
		       sec)
		    (begin
		       (set! val (date-copy val :sec sec))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setUTCSeconds
      :value (js-make-function %this date-prototype-setutcseconds 2 'setUTCSeconds)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setMinutes
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.32
   (define (date-prototype-setminutes this::JsDate min sec ms)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((min (js-tonumber min %this))
		   (sec (unless (eq? sec (js-undefined)) (js-tonumber sec %this))))
		(if (and (flonum? min) (nanfl? min))
		    (begin
		       (set! val min)
		       min)
		    (begin
		       (set! val (date-copy val :min min :sec sec))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setMinutes
      :value (js-make-function %this date-prototype-setminutes 3 'setMinutes)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setUTCMinutes
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.33
   (define (date-prototype-setutcminutes this::JsDate min sec ms)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((min (js-tonumber min %this))
		   (sec (unless (eq? sec (js-undefined)) (js-tonumber sec %this))))
		(if (and (flonum? min) (nanfl? min))
		    (begin
		       (set! val min)
		       min)
		    (begin
		       (set! val (date-copy val :min min :sec sec))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setUTCMinutes
      :value (js-make-function %this date-prototype-setutcminutes 3 'setUTCMinutes)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setHours
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.34
   (define (date-prototype-sethours this::JsDate hour min sec ms)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((hour (js-tonumber hour %this))
		   (min (unless (eq? min (js-undefined)) (js-tonumber min %this)))
		   (sec (unless (eq? sec (js-undefined)) (js-tonumber sec %this))))
		(if (and (flonum? hour) (nanfl? hour))
		    (begin
		       (set! val hour)
		       hour)
		    (begin
		       (set! val (date-copy val :hour hour :min min :sec sec))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setHours
      :value (js-make-function %this date-prototype-sethours 4 'setHours)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setUTCHours
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.35
   (define (date-prototype-setutchours this::JsDate hour min sec ms)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((hour (js-tonumber hour %this)))
		(if (and (flonum? hour) (nanfl? hour))
		    (begin
		       (set! val hour)
		       hour)
		    (let* ((min (unless (eq? min (js-undefined)) (js-tonumber min %this)))
			   (sec (unless (eq? sec (js-undefined)) (js-tonumber sec %this)))
			   (dt (date-copy val :hour hour :min min :sec sec))
			   (ns (date->nanoseconds dt)))
		       (set! val
			  (nanoseconds->date
			     (+llong ns
				(*llong #l1000000000
				   (fixnum->llong (date-timezone dt))))))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setUTCHours
      :value (js-make-function %this date-prototype-setutchours 4 'setUTCHours)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setDate
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.36
   (define (date-prototype-setdate this::JsDate date)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let* ((date (js-tonumber date %this))
		    (day (cond
			    ((fixnum? date) date)
			    ((flonum? date) (flonum->fixnum date))
			    (else 1))))
		(if (and (flonum? date) (nanfl? date))
		    (begin
		       (set! val date)
		       date)
		    (begin
		       (set! val (date-copy val :day day))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setDate
      :value (js-make-function %this date-prototype-setdate 1 'setDate)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setUTCDate
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.37
   (define (date-prototype-setutcdate this::JsDate date)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((date (js-tonumber date %this)))
		(if (and (flonum? date) (nanfl? date))
		    (begin
		       (set! val date)
		       date)
		    (begin
		       (set! val (date-copy val :day date))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setUTCDate
      :value (js-make-function %this date-prototype-setutcdate 1 'setUTCDate)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setMonth
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.38
   (define (date-prototype-setmonth this::JsDate month date)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((month (js-tonumber month %this))
		   (day (unless (eq? date (js-undefined)) (js-tonumber date %this))))
		(if (and (flonum? month) (nanfl? month))
		    (begin
		       (set! val month)
		       month)
		    (begin
		       (set! val
			  (date-copy val :timezone 0
			     :month (+ 1 month)
			     :day day))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setMonth
      :value (js-make-function %this date-prototype-setmonth 2 'setMonth)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setUTCMonth
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.39
   (define (date-prototype-setutcmonth this::JsDate month date)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((month (js-tonumber month %this))
		   (date (unless (eq? date (js-undefined)) (js-tonumber date %this))))
		(if (and (flonum? month) (nanfl? month))
		    (begin
		       (set! val month)
		       month)
		    (begin
		       (set! val (date-copy val :month (+ 1 month) :day date))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setUTCMonth
      :value (js-make-function %this date-prototype-setutcmonth 2 'setUTCMonth)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setFullYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.40
   (define (date-prototype-setfullyear this::JsDate year month date)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((year (js-tonumber year %this))
		   (month (unless (eq? month (js-undefined)) (js-tonumber month %this)))
		   (date (unless (eq? date (js-undefined)) (js-tonumber date %this))))
		(if (and (flonum? year) (nanfl? year))
		    (begin
		       (set! val year)
		       year)
		    (begin
		       (set! val (date-copy val :year year :month month :day date))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setFullYear
      :value (js-make-function %this date-prototype-setfullyear 3 'setFullYear)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; setUTCFullYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.41
   (define (date-prototype-setutcfullyear this::JsDate year month date)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((year (js-tonumber year %this))
		   (month (unless (eq? month (js-undefined)) (js-tonumber month %this)))
		   (date (unless (eq? date (js-undefined)) (js-tonumber date %this))))
		(if (and (flonum? year) (nanfl? year))
		    (begin
		       (set! val year)
		       year)
		    (begin
		       (set! val (date-copy val :year year :month month :day date))
		       (date->milliseconds val))))
	     val)))

   (js-bind! %this obj 'setUTCFullYear
      :value (js-make-function %this date-prototype-setutcfullyear 3 'setUTCFullYear)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   ;; toGMTString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.6
   (define (date-prototype-togmtstring this::JsDate)
      (date-prototype-toutcstring this))

   (js-bind! %this obj 'toGMTString
      :value (js-make-function %this date-prototype-togmtstring 0 'toGMTString)
      :writable #t
      :configurable #t
      :enumerable #f)
   
   obj)

;*---------------------------------------------------------------------*/
;*    date->milliseconds ...                                           */
;*---------------------------------------------------------------------*/
(define (date->milliseconds dt::date)
   (llong->flonum (/llong (date->nanoseconds dt) #l1000000)))

;*---------------------------------------------------------------------*/
;*    date->utc-date ...                                               */
;*---------------------------------------------------------------------*/
(define (date->utc-date dt::date)
   (let ((tz (date-timezone dt))
	 (ctz (date-timezone (date-copy dt))))
      (date-copy
	 (nanoseconds->date
	    (- (date->nanoseconds dt)
	       (*llong (fixnum->llong ctz)
		  #l1000000000)))
	 :timezone 0)))

;*---------------------------------------------------------------------*/
;*    js-date->jsdate ...                                              */
;*---------------------------------------------------------------------*/
(define (js-date->jsdate val::date %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-date)
      (let ((dt (js-new0 %this js-date)))
	 (with-access::JsDate dt ((dval val))
	    (set! dval val)
	    dt))))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
