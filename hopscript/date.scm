;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/date.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Wed Apr  8 08:27:30 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
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
	   (js-date->jsdate::JsDate ::date ::JsGlobalObject)
	   (js-date-now)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsDate ...                                   */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsDate
   (lambda (o ctx)
      (with-access::JsDate o (val) val))
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
	    (with-access::JsDate nobj (val)
	       (with-access::JsDate obj ((_val val))
		  (js-object-proto-set! nobj (js-get js-date (& "prototype") %this))
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
(define-method (xml-unpack o::JsDate ctx)
   (with-access::JsDate o (val)
      (list val)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsDate ...                                     */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsDate op compile isexpr ctx)
   (display "new Date(" op)
   (with-access::JsDate o (val)
      (if (date? val)
	  (display (llong->flonum (date->milliseconds val)) op)
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
   ;; local constant strings
   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   
   ;; first, bind the builtin date prototype
   (with-access::JsGlobalObject %this (js-date js-function)
      
      (define js-date-prototype
	 (instantiateJsDate
	    (val (current-date))
	    (cmap (instantiate::JsConstructMap))
	    (elements ($create-vector 47))
	    (__proto__ (js-object-proto %this))))
      
      (define (js-date-alloc %this constructor::JsFunction)
	 (with-access::JsGlobalObject %this (js-new-target js-date)
	    (set! js-new-target constructor)
	    (instantiateJsDate
	       (__proto__ (if (eq? constructor js-date)
			      js-date-prototype
			      (js-get constructor (& "prototype") %this))))))
      
      (define (parse-date-arguments args)
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
		(when (and (fixnum? y) (>fx y 0)
			   (fixnum? m) (fixnum? d) (fixnum? h)
			   (fixnum? mi) (fixnum? se) (llong? ns))
		   (make-date
		      :year y :month (+ m 1) :day d
		      :hour h :min mi :sec se :nsec ns))))
	    ((?year ?month ?date ?hours ?minutes ?seconds)
	     (let* ((y (js-tonumber year %this))
		    (m (js-tonumber month %this))
		    (d (js-tonumber date %this))
		    (h (js-tonumber hours %this))
		    (mi (js-tonumber minutes %this))
		    (se (js-tonumber seconds %this)))
		(when (and (fixnum? y) (>fx y 0)
			   (fixnum? m) (fixnum? d) (fixnum? h)
			   (fixnum? mi) (fixnum? se))
		   (make-date
		      :year y :month (+ m 1) :day d
		      :hour h :min mi :sec se))))
	    ((?year ?month ?date ?hours ?minutes)
	     (let* ((y (js-tonumber year %this))
		    (m (js-tonumber month %this))
		    (d (js-tonumber date %this))
		    (h (js-tonumber hours %this))
		    (mi (js-tonumber minutes %this)))
		(when (and (fixnum? y) (>fx y 0)
			   (fixnum? m) (fixnum? d) (fixnum? h)
			   (fixnum? mi))
		   (make-date
		      :year y :month (+ m 1) :day d
		      :hour h :min mi :sec 0))))
	    ((?year ?month ?date ?hours)
	     (let* ((y (js-tonumber year %this))
		    (m (js-tonumber month %this))
		    (d (js-tonumber date %this))
		    (h (js-tonumber hours %this)))
		(when (and (fixnum? y) (>fx y 0)
			   (fixnum? m) (fixnum? d) (fixnum? h))
		   (make-date
		      :year y :month (+ m 1) :day d
		      :hour h :min 0 :sec 0))))
	    ((?year ?month ?date)
	     (let* ((y (js-tonumber year %this))
		    (m (js-tonumber month %this))
		    (d (js-tonumber date %this)))
		(when (and (fixnum? y) (>fx y 0)
			   (fixnum? m) (fixnum? d))
		   (make-date
		      :year y :month (+ m 1) :day d
		      :hour 0 :min 0 :sec 0))))
	    ((?year ?month)
	     (let* ((y (js-tonumber year %this))
		    (m (js-tonumber month %this)))
		(when (and (fixnum? y) (>fx y 0)
			   (fixnum? m))
		   (make-date
		      :year y :month (+ m 1)
		      :day 1 :hour 0 :min 0 :sec 0))))
	    ((?value)
	     (if (isa? value JsDate)
		 (with-access::JsDate value (val)
		    val)
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
	    (else
	     (current-date))))
      
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9
      (define (%js-date this . args)
	 
	 (define (set-date! this v)
	    (with-access::JsDate this (val)
	       (set! val v)
	       this))
	 
	 (with-access::JsGlobalObject %this (js-new-target)
	    (cond
	       ((eq? js-new-target (js-undefined))
		(js-date->jsstring (current-date)))
	       ((any (lambda (a) (eq? a (js-undefined))) args)
		(set! js-new-target (js-undefined))
		this)
	       (else
		(let ((val (parse-date-arguments args)))
		   (set-date! this val)
		   (set! js-new-target (js-undefined))
		   this)))))
      
      (set! js-date
	 (js-make-function %this %js-date
	    (js-function-arity 0 -1 'scheme)
	    (js-function-info :name "Date" :len 7)
	    :__proto__ (js-object-proto js-function)
	    :prototype js-date-prototype
	    :alloc js-date-alloc
	    :size 3
	    :shared-cmap #f))
      
      ;; parse
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.4.2
      (define (js-date-parse this str)
	 (let ((d (parse-date (js-tostring str %this))))
	    (if (date? d)
		(*fl 1000. (elong->flonum (date->seconds d)))
		+nan.0)))
      
      (js-bind! %this js-date (& "parse")
	 :value (js-make-function %this js-date-parse
		   (js-function-arity js-date-parse)
		   (js-function-info :name "parse" :len 1))
	 :writable #t :configurable #t :enumerable #f :hidden-class #t)
      
      ;; UTC
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.4.3
      (define (js-date-utc this . args)
	 (match-case args
	    (()
	     0)
	    ((?year)
	     (let ((d (parse-date-arguments (list (js-tonumber year %this)))))
		(js-flonum->integer
		   (*fl 1000.
		      (elong->flonum
			 (- (date->seconds d) (date-timezone d)))))))
	    (else
	     (let* ((dt (parse-date-arguments args))
		    (ctz (date-timezone dt)))
		(js-flonum->integer
		   (llong->flonum
		      (+llong (date->milliseconds dt)
			 (*llong (fixnum->llong ctz) #l1000))))))))
      
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
		  (lambda (e) "Invalid Date")
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
      (with-access::JsDate this (val)
	 (if (date? val)
	     (js-ascii->jsstring
		(format "~a ~a ~2,0d ~d"
		   (day-aname (date-wday val))
		   (month-aname (date-month val))
		   (date-day val)
		   (date-year val)))
	     (js-ascii->jsstring "Invalid date"))))

   (js-bind! %this obj (& "toDateString")
      :value (js-make-function %this date-prototype-todatestring
		(js-function-arity date-prototype-todatestring)
		(js-function-info :name "toDateString" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; toTimeString
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.4
   (define (date-prototype-totimestring this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (js-ascii->jsstring
		(format "~2,0d:~2,0d:~2,0d ~a"
		   (date-hour val)
		   (date-minute val)
		   (date-second val)
		   (date-timezone val)))
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
      date-prototype-todatestring this)

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
      (with-access::JsDate this (val)
	 (if (date? val)
	     (js-ascii->jsstring (date->utc-string val))
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
      (if (not (isa? this JsDate))
	  (js-raise-type-error %this "Not a date ~s" (typeof this))
	  (with-access::JsDate this (val)
	     (if (date? val)
		 (let loop ((val val))
		    (if (=fx (date-timezone val) 0)
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
			   (js-ascii->jsstring buf))
			(loop (date->utc-date val))))
		 (js-raise-range-error %this "Invalid date ~s" val)))))
   
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
      (with-access::JsDate this (val)
	 (if (date? val)
	     (begin
		;; MS care to replace !!!
		;; (js-date->milliseconds val)
		(llong->flonum (date->milliseconds val)))
	     +nan.0)))
	 
   (js-bind! %this obj (& "valueOf")
      :value (js-make-function %this date-prototype-valueof
		(js-function-arity date-prototype-valueof)
		(js-function-info :name "valueOf" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getTime
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.9
   (define (date-prototype-gettime this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (js-date->milliseconds val)
	     +nan.0)))
 	 
   (js-bind! %this obj (& "getTime")
      :value (js-make-function %this date-prototype-gettime
		(js-function-arity date-prototype-gettime)
		(js-function-info :name "getTime" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getFullYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.10
   (define (date-prototype-getfullyear this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-year val)
	     +nan.0)))
	 
   (js-bind! %this obj (& "getFullYear")
      :value (js-make-function %this date-prototype-getfullyear
		(js-function-arity date-prototype-getfullyear)
		(js-function-info :name "getFullYear" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCFullYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.11
   (define (date-prototype-getutcfullyear this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let* ((tz (date-timezone val))
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
   (define (date-prototype-getmonth this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (-fx (date-month val) 1)
	     +nan.0)))
	 
   (js-bind! %this obj (& "getMonth")
      :value (js-make-function %this date-prototype-getmonth
		(js-function-arity date-prototype-getmonth)
		(js-function-info :name "getMonth" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCMonth
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.13
   (define (date-prototype-getutcmonth this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let* ((tz (date-timezone val))
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
   (define (date-prototype-getdate this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-day val)
	     +nan.0)))
	 
   (js-bind! %this obj (& "getDate")
      :value (js-make-function %this date-prototype-getdate
		(js-function-arity date-prototype-getdate)
		(js-function-info :name "getDate" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCDate
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.15
   (define (date-prototype-getutcdate this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let* ((tz (date-timezone val))
		    (d (seconds->date (-elong (date->seconds val) tz))))
		(date-day d))
	     +nan.0)))
	 
   (js-bind! %this obj (& "getUTCDate")
      :value (js-make-function %this date-prototype-getutcdate
		(js-function-arity date-prototype-getutcdate)
		(js-function-info :name "getUTCDate" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getDay
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.16
   (define (date-prototype-getday this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (-fx (date-wday val) 1)
	     +nan.0)))
	 
   (js-bind! %this obj (& "getDay")
      :value (js-make-function %this date-prototype-getday
		(js-function-arity date-prototype-getday)
		(js-function-info :name "getDay" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCDay
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.17
   (define (date-prototype-getutcday this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (-fx (date-wday val) 1)
	     +nan.0)))
	 
   (js-bind! %this obj (& "getUTCDay")
      :value (js-make-function %this date-prototype-getutcday
		(js-function-arity date-prototype-getutcday)
		(js-function-info :name "getUTCDay" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getHours
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.18
   (define (date-prototype-gethours this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-hour val)
	     +nan.0)))
	 
   (js-bind! %this obj (& "getHours")
      :value (js-make-function %this date-prototype-gethours
		(js-function-arity date-prototype-gethours)
		(js-function-info :name "getHours" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
	 
   (js-bind! %this obj (& "getUTCHours")
      :value (js-make-function %this date-prototype-getutchours
		(js-function-arity date-prototype-getutchours)
		(js-function-info :name "getUTCHours" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getMinutes
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.20
   (define (date-prototype-getminutes this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-minute val)
	     +nan.0)))
	 
   (js-bind! %this obj (& "getMinutes")
      :value (js-make-function %this date-prototype-getminutes
		(js-function-arity date-prototype-getminutes)
		(js-function-info :name "getMinutes" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCMinutes
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.21
   (define (date-prototype-getutcminutes this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-minute val)
	     +nan.0)))
	 
   (js-bind! %this obj (& "getUTCMinutes")
      :value (js-make-function %this date-prototype-getutcminutes
		(js-function-arity date-prototype-getutcminutes)
		(js-function-info :name "getUTCMinutes" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getSeconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.22
   (define (date-prototype-getseconds this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-second val)
	     +nan.0)))
	 
   (js-bind! %this obj (& "getSeconds")
      :value (js-make-function %this date-prototype-getseconds
		(js-function-arity date-prototype-getseconds)
		(js-function-info :name "getSeconds" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCSeconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.23
   (define (date-prototype-getutcseconds this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (date-second val)
	     +nan.0)))
	 
   (js-bind! %this obj (& "getUTCSeconds")
      :value (js-make-function %this date-prototype-getutcseconds
		(js-function-arity date-prototype-getutcseconds)
		(js-function-info :name "getUTCSeconds" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getMilliseconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.24
   (define (date-prototype-getmilliseconds this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (llong->fixnum (date-millisecond val))
	     +nan.0)))
	 
   (js-bind! %this obj (& "getMilliseconds")
      :value (js-make-function %this date-prototype-getmilliseconds
		(js-function-arity date-prototype-getmilliseconds)
		(js-function-info :name "getMilliseconds" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getUTCMilliseconds
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.25
   (define (date-prototype-getutcmilliseconds this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (llong->fixnum (date-millisecond val))
	     +nan.0)))
	 
   (js-bind! %this obj (& "getUTCMilliseconds")
      :value (js-make-function %this date-prototype-getutcmilliseconds
		(js-function-arity date-prototype-getutcmilliseconds)
		(js-function-info :name "getUTCMilliseconds" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getTimezoneOffset
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.26
   (define (date-prototype-gettimezoneoffset this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (negfx (/fx (date-timezone (date->local-date val)) 60))
	     +nan.0)))

   (js-bind! %this obj (& "getTimezoneOffset")
      :value (js-make-function %this date-prototype-gettimezoneoffset
		(js-function-arity date-prototype-gettimezoneoffset)
		(js-function-info :name "getTimezoneOffset" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setTime
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.27
   (define (date-prototype-settime this::JsDate time)
      (with-access::JsDate this (val)
	 (let ((s (js-tonumber time %this)))
	    (cond
	       ((fixnum? s)
		(set! val (milliseconds->date (fixnum->llong s)))
		(js-date->milliseconds val))
	       ((flonum? s)
		(if (nanfl? s)
		    (begin
		       (set! val s)
		       s)
		    (begin
		       (set! val (milliseconds->date (flonum->llong s)))
		       (js-date->milliseconds val))))))))

   (js-bind! %this obj (& "setTime")
      :value (js-make-function %this date-prototype-settime
		(js-function-arity date-prototype-settime)
		(js-function-info :name "setTime" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; getYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.4
   (define (date-prototype-getyear this::JsDate)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (-fx (date-year val) 1900)
	     +nan.0)))
   
   (js-bind! %this obj (& "getYear")
      :value (js-make-function %this date-prototype-getyear
		(js-function-arity date-prototype-getyear)
		(js-function-info :name "getYear" :len 0))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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

   (js-bind! %this obj (& "setYear")
      :value (js-make-function %this date-prototype-setyear
		(js-function-arity date-prototype-setyear)
		(js-function-info :name "setYear" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
			   (js-date->milliseconds val))))
		   ((fixnum? ms)
		    (set! val (date-copy val :nsec (*llong #l1000000 (fixnum->llong ms))))
		    (js-date->milliseconds val))
		   (else
		    (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setMilliseconds")
      :value (js-make-function %this date-prototype-setmilliseconds
		(js-function-arity date-prototype-setmilliseconds)
		(js-function-info :name "setMilliseconds" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
			   (js-date->milliseconds val))))
		   ((fixnum? ms)
		    (set! val (date-copy val :nsec (*llong #l1000000 (fixnum->llong ms))))
		    (js-date->milliseconds val))
		   (else
		    (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setUTCMilliseconds")
      :value (js-make-function %this date-prototype-setutcmilliseconds
		(js-function-arity date-prototype-setutcmilliseconds)
		(js-function-info :name "setUTCMilliseconds" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)

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
		       (set! val (date-copy val :sec (->fixnum-safe sec)))
		       (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setSeconds")
      :value (js-make-function %this date-prototype-setseconds
		(js-function-arity date-prototype-setseconds)
		(js-function-info :name "setSeconds" :len 2))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
		       (set! val (date-copy val :sec (->fixnum-safe sec)))
		       (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setUTCSeconds")
      :value (js-make-function %this date-prototype-setutcseconds
		(js-function-arity date-prototype-setutcseconds)
		(js-function-info :name "setUTCSeconds" :len 2))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
		       (set! val
			  (date-copy val :min (->fixnum-safe min) :sec sec))
		       (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setMinutes")
      :value (js-make-function %this date-prototype-setminutes
		(js-function-arity date-prototype-setminutes)
		(js-function-info :name "setMinutes" :len 3))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
		       (set! val (date-copy val
				    :min (->fixnum-safe min) :sec (->fixnum-safe sec)))
		       (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setUTCMinutes")
      :value (js-make-function %this date-prototype-setutcminutes
		(js-function-arity date-prototype-setutcminutes)
		(js-function-info :name "setUTCMinutes" :len 3))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
		       (set! val (date-copy val
				    :hour (->fixnum-safe hour)
				    :min (->fixnum-safe min)
				    :sec (->fixnum-safe sec)))
		       (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setHours")
      :value (js-make-function %this date-prototype-sethours
		(js-function-arity date-prototype-sethours)
		(js-function-info :name "setHours" :len 4))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
			   (dt (date-copy val
				  :hour (->fixnum-safe hour)
				  :min (->fixnum-safe min)
				  :sec (->fixnum-safe sec)))
			   (ms (date->milliseconds dt)))
		       (set! val
			  (milliseconds->date
			     (+llong ms
				(*llong #l1000
				   (fixnum->llong (date-timezone dt))))))
		       (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setUTCHours")
      :value (js-make-function %this date-prototype-setutchours
		(js-function-arity date-prototype-setutchours)
		(js-function-info :name "setUTCHours" :len 4))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
		       (set! val (date-copy val :day (->fixnum-safe day)))
		       (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setDate")
      :value (js-make-function %this date-prototype-setdate
		(js-function-arity date-prototype-setdate)
		(js-function-info :name "setDate" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
		       (set! val (date-copy val :day (->fixnum-safe date)))
		       (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setUTCDate")
      :value (js-make-function %this date-prototype-setutcdate
		(js-function-arity date-prototype-setutcdate)
		(js-function-info :name "setUTCDate" :len 1))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
		    (let* ((hour (date-hour val))
			   (tz (date-timezone val))
			   (nval (date-copy val
				    :hour hour
				    :month (->fixnum-safe (+ 1 month))
				    :day (->fixnum-safe day)))
			   (ntz (date-timezone nval)))
		       (if (=fx ntz tz)
			   (set! val nval)
			   (set! val
			      (milliseconds->date
				 (-llong (date->milliseconds nval)
				    (*llong #l1000
				       (fixnum->llong (- ntz tz)))))))
		       (js-date->milliseconds val))))
	     val)))
   
   (define (date-prototype-setmonth-wrong this::JsDate month date)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((month (js-tonumber month %this))
		   (day (if (eq? date (js-undefined))
			    (date-day val)
			    (->fixnum-safe (js-tonumber date %this)))))
		(if (and (flonum? month) (nanfl? month))
		    (begin
		       (set! val month)
		       month)
		    (let ((utcval (date->utc-date val))
			  (hour (date-hour val)))
		       (set! val
			  (date-copy
			     (date->local-date
				(date-copy utcval
				   :timezone 0
				   :month (+fx (->fixnum-safe month) 1)
				   :day day))
			     :hour hour))
		       (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setMonth")
      :value (js-make-function %this date-prototype-setmonth
		(js-function-arity date-prototype-setmonth)
		(js-function-info :name "setMonth" :len 2))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
		       (set! val (date-copy val
				    :month (->fixnum-safe (+ 1 month))
				    :day (->fixnum-safe date)))
		       (js-date->milliseconds val))))
	     val)))
   
   (define (date-prototype-setutcmonth-wrong this::JsDate month date)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((month (js-tonumber month %this))
		   (day (if (eq? date (js-undefined))
			    (date-day val)
			    (->fixnum-safe (js-tonumber date %this)))))
		(if (and (flonum? month) (nanfl? month))
		    (begin
		       (set! val month)
		       month)
		    (let ((utcval val)
			  (hour (date-hour val)))
		       (set! val
			  (date-copy utcval
			     :month (+fx (->fixnum-safe month) 1)
			     :hour hour
			     :day day))
		       (js-date->milliseconds val))))
	     val)))

   (js-bind! %this obj (& "setUTCMonth")
      :value (js-make-function %this date-prototype-setutcmonth
		(js-function-arity date-prototype-setutcmonth)
		(js-function-info :name "setUTCMonth" :len 2))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
   ;; setFullYear
   ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.40
   (define (date-prototype-setfullyear this::JsDate year month date)
      (with-access::JsDate this (val)
	 (if (date? val)
	     (let ((year (js-tonumber year %this))
		   (month (unless (eq? month (js-undefined))
			     (js-tonumber month %this)))
		   (date (unless (eq? date (js-undefined))
			    (js-tonumber date %this))))
		(if (and (flonum? year) (nanfl? year))
		    (begin
		       (set! val year)
		       year)
		    (begin
		       (set! val (date-copy val
				    :year (->fixnum-safe year)
				    :month (->fixnum-safe month)
				    :day (->fixnum-safe date)))
		       (js-date->milliseconds val))))
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
		       (set! val year)
		       year)
		    (begin
		       (set! val (make-date
				    :year (->fixnum-safe year)
				    :month (->fixnum-safe month)
				    :day (->fixnum-safe date)
				    :hour 0 :min 0 :sec 0))
		       (js-date->milliseconds val)))))))

   (js-bind! %this obj (& "setFullYear")
      :value (js-make-function %this date-prototype-setfullyear
		(js-function-arity date-prototype-setfullyear)
		(js-function-info :name "setFullYear" :len 3))
      :writable #t :configurable #t :enumerable #f :hidden-class #f)
   
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
		       (set! val (date-copy val
				    :year (->fixnum-safe year)
				    :month (->fixnum-safe month)
				    :day (->fixnum-safe date)))
		       (js-date->milliseconds val))))
	     val)))

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
;*    js-date->milliseconds ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-date->milliseconds dt)
   (cond-expand
      ((or bint61 bint64)
       (llong->fixnum (date->milliseconds dt)))
      (else
       (llong->flonum (date->milliseconds dt)))))

;*---------------------------------------------------------------------*/
;*    js-date->jsstring ...                                            */
;*---------------------------------------------------------------------*/
(define (js-date->jsstring val)
   (if (date? val)
       (js-ascii->jsstring
	  (date->rfc2822-date (seconds->date (date->seconds val))))
       (js-ascii->jsstring "Invalid Date")))

;*---------------------------------------------------------------------*/
;*    date-prototype-tostring ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.2     */
;*---------------------------------------------------------------------*/
(define (date-prototype-tostring this::JsDate)
   (with-access::JsDate this (val)
      (js-date->jsstring val)))

;*---------------------------------------------------------------------*/
;*    date->local-date ...                                             */
;*---------------------------------------------------------------------*/
(define (date->local-date dt::date)
   (milliseconds->date (date->milliseconds dt)))

;*---------------------------------------------------------------------*/
;*    date->utc-date ...                                               */
;*---------------------------------------------------------------------*/
(define (date->utc-date dt::date)
   (let ((tz (date-timezone dt))
	 (ctz (date-timezone (date-copy dt))))
      (date-copy
	 (milliseconds->date
	    (-llong (date->milliseconds dt) (*llong (fixnum->llong ctz) #l1000)))
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
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
