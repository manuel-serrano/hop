;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/date.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Tue Apr  1 11:21:04 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
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
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_regexp
	   __hopscript_private
	   __hopscript_public
	   __hopscript_error)
   
   (export js-date
	   js-date-prototype
	   (js-init-date! ::JsObject)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsDate ...                                     */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsDate op compile isexpr)
   (display "new Date(\"" op)
   (display (js-tostring o) op)
   (display "\")" op))

;*---------------------------------------------------------------------*/
;*    js-date ...                                                      */
;*---------------------------------------------------------------------*/
(define js-date #f)
(define js-date-prototype #f)

;*---------------------------------------------------------------------*/
;*    js-toprimitive ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.8       */
;*---------------------------------------------------------------------*/
(define-method (js-toprimitive o::JsDate preferredtype)
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
   (set! js-date-prototype
      (instantiate::JsDate
	 (val (current-date))
	 (__proto__ js-object-prototype)))
   ;; then, create a HopScript object
   (let ((obj (js-make-function %js-date 7 "JsDate"
		 :__proto__ js-function-prototype
		 :prototype js-date-prototype
		 :alloc js-date-alloc
		 :construct js-date-construct)))
      (set! js-date obj)
      ;; other properties of the Date constructor
      (js-bind! obj 'parse
	 :value (js-make-function js-date-parse 1 "parse")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'UTC
	 :value (js-make-function js-date-utc 7 "UTC")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'now
	 :value (js-make-function js-date-now 0 "now")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; prototype properties
      (init-builtin-date-prototype! js-date-prototype)
      ;; bind Date in the global object
      (js-bind! %this 'Date :configurable #f :enumerable #f :value js-date)
      js-date))

;*---------------------------------------------------------------------*/
;*    %js-date ...                                                     */
;*---------------------------------------------------------------------*/
(define (%js-date this . args)
   (let ((dt (js-date-construct (js-date-alloc js-date))))
      (js-call0 (js-get dt 'toString) dt)))

;*---------------------------------------------------------------------*/
;*    js-date-alloc ...                                                */
;*---------------------------------------------------------------------*/
(define (js-date-alloc constructor::JsFunction)
   (instantiate::JsDate
      (__proto__ (js-get constructor 'prototype))))

;*---------------------------------------------------------------------*/
;*    js-date-construct ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9         */
;*---------------------------------------------------------------------*/
(define (js-date-construct this::JsDate . args)
   
   (define (js-date->jsdate v)
      (with-access::JsDate this (val)
	 (set! val v)
	 this))

   (match-case args
      ((?year ?month ?date ?hours ?minutes ?seconds ?ms)
       (let* ((y (js-tonumber year))
	      (m (js-tonumber month))
	      (d (js-tonumber date))
	      (h (js-tonumber hours))
	      (mi (js-tonumber minutes))
	      (se (js-tonumber seconds))
	      (ns (* 1000 (js-tonumber ms))))
	  (js-date->jsdate
	     (when (and (fixnum? y) (>fx y 0)
			(fixnum? m) (fixnum? d) (fixnum? h)
			(fixnum? mi) (fixnum? se) (fixnum? ns))
		(make-date
		   :year y :month (+ m 1) :day d
		   :hour h :min mi :sec se :nsec ns)))))
      ((?year ?month ?date ?hours ?minutes ?seconds)
       (let* ((y (js-tonumber year))
	      (m (js-tonumber month))
	      (d (js-tonumber date))
	      (h (js-tonumber hours))
	      (mi (js-tonumber minutes))
	      (se (js-tonumber seconds)))
	  (js-date->jsdate
	     (when (and (fixnum? y) (>fx y 0)
			(fixnum? m) (fixnum? d) (fixnum? h)
			(fixnum? mi) (fixnum? se))
		(make-date
		   :year y :month (+ m 1) :day d
		   :hour h :min mi :sec se)))))
      ((?year ?month ?date ?hours ?minutes)
       (let* ((y (js-tonumber year))
	      (m (js-tonumber month))
	      (d (js-tonumber date))
	      (h (js-tonumber hours))
	      (mi (js-tonumber minutes)))
	  (js-date->jsdate
	     (when (and (fixnum? y) (>fx y 0)
			(fixnum? m) (fixnum? d) (fixnum? h)
			(fixnum? mi))
		(make-date
		   :year y :month (+ m 1) :day d
		   :hour h :min mi :sec 0)))))
      ((?year ?month ?date ?hours)
       (let* ((y (js-tonumber year))
	      (m (js-tonumber month))
	      (d (js-tonumber date))
	      (h (js-tonumber hours)))
	  (js-date->jsdate
	     (when (and (fixnum? y) (>fx y 0)
			(fixnum? m) (fixnum? d) (fixnum? h))
		(make-date
		   :year y :month (+ m 1) :day d
		   :hour h :min 0 :sec 0)))))
      ((?year ?month ?date)
       (let* ((y (js-tonumber year))
	      (m (js-tonumber month))
	      (d (js-tonumber date)))
	  (js-date->jsdate
	     (when (and (fixnum? y) (>fx y 0)
			(fixnum? m) (fixnum? d))
		(make-date
		   :year y :month (+ m 1) :day d
		   :hour 0 :min 0 :sec 0)))))
      ((?year ?month)
       (let* ((y (js-tonumber year))
	      (m (js-tonumber month)))
	  (js-date->jsdate
	     (when (and (fixnum? y) (>fx y 0)
			(fixnum? m))
		(make-date
		   :year y :month (+ m 1)
		   :day 1 :hour 0 :min 0 :sec 0)))))
      ((?value)
       (let ((v (js-toprimitive value 'any)))
	  (cond
	     ((string? v)
	      (js-date->jsdate (parse-date v)))
	     ((number? v)
	      (if (flonum? v)
		  (js-date->jsdate 
		     (if (or (nanfl? v) (=fl v +inf.0) (=fl v -inf.0))
			 +nan.0
			 (fixnum->elong (flonum->fixnum (round v)))))
		  (let ((n (cond
			      ((fixnum? v)
			       (fixnum->elong (/fx v 1000)))
			      ((llong? v)
			       (llong->elong (/llong v #l1000)))
			      ((elong? v)
			       (/elong v #e1000))
			      (else
			       v))))
		     (js-date->jsdate (seconds->date n)))))
	     (else
	      (js-date->jsdate v)))))
      (else
       (js-date->jsdate (current-date)))))

;*---------------------------------------------------------------------*/
;*    parse-date ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15    */
;*---------------------------------------------------------------------*/
(define (parse-date v)
   (with-handler
      (lambda (e)
	 (cond
	    ((pregexp-match "([0-9]{1,2})/([0-9]{1,2})/[0-9]{4} ([0-9]{2}):([0-9]{2})" v)
	     =>
	     (match-lambda
		((?- ?day ?month ?year ?hour ?minute)
		 (make-date
		    :timezone 0
		    :year (string->integer year)
		    :month (string->integer month)
		    :day (string->integer day)
		    :hour (string->integer hour)
		    :min (string->integer minute)))
		(else
		 (current-seconds))))
	    ((pregexp-match "[0-9]{4}" v)
	     (make-date
		:timezone 0
		:year (string->integer v)))
	    ((pregexp-match "([0-9]{4}):([0-9]{2})" v)
	     =>
	     (match-lambda
		((?yyyy ?mm)
		 (make-date
		    :timezone 0
		    :year (string->integer yyyy)
		    :min (string->integer mm)))))
	    ((pregexp-match "([0-9]{4})-([0-9]{2})" v)
	     =>
	     (match-lambda
		((?yyyy ?mm)
		 (make-date
		    :timezone 0
		    :year (string->integer yyyy)
		    :month (string->integer mm)))))
	    ((pregexp-match "([0-9]{4})-([0-9]{2})-([0-9]{2})" v)
	     =>
	     (match-lambda
		((?yyyy ?mm ?dd)
		 (make-date
		    :timezone 0
		    :year (string->integer yyyy)
		    :month (string->integer mm)
		    :day (string->integer dd)))))
	    ((pregexp-match "T([0-9]{2})" v)
	     =>
	     (match-lambda
		((?HH)
		 (make-date
		    :timezone 0
		    :hour (string->integer HH)))))
	    ((pregexp-match "T([0-9]{2}):([0-9]{2})" v)
	     =>
	     (match-lambda
		((?HH ?mm)
		 (make-date
		    :timezone 0
		    :hour (string->integer HH)
		    :min (string->integer mm)))))
	    ((pregexp-match "T([0-9]{2}):([0-9]{2}):([0-9]{2})" v)
	     =>
	     (match-lambda
		((?HH ?mm ?ss)
		 (make-date
		    :timezone 0
		    :hour (string->integer HH)
		    :min (string->integer mm)
		    :sec (string->integer ss)))))
	    ((pregexp-match "T([0-9]{2}):([0-9]{2}):([0-9]{2}).:([0-9]{3})" v)
	     =>
	     (match-lambda
		((?HH ?mm ?ss ?sss)
		 (make-date
		    :timezone 0
		    :hour (string->integer HH)
		    :min (string->integer mm)
		    :sec (string->integer ss)
		    :nsec (*fx 1000 (string->integer sss))))))
	    (else
	     (current-date))))
      (rfc2822-date->date v)))

;*---------------------------------------------------------------------*/
;*    js-date-parse ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.4.2     */
;*---------------------------------------------------------------------*/
(define (js-date-parse this str)
   (date->seconds (parse-date (js-tostring str))))

;*---------------------------------------------------------------------*/
;*    js-date-utc ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-date-utc this . l)
   (apply js-date-construct (js-date-alloc js-date) l))

;*---------------------------------------------------------------------*/
;*    js-date-now ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.4.4     */
;*---------------------------------------------------------------------*/
(define (js-date-now this)
   (current-seconds))

;*---------------------------------------------------------------------*/
;*    init-builtin-date-prototype! ...                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5       */
;*---------------------------------------------------------------------*/
(define (init-builtin-date-prototype! obj)
   ;; prototype fields
   (js-bind! obj 'constructor
      :value js-date
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; toString
   (js-bind! obj 'toString
      :value (js-make-function date-prototype-tostring 0 "toString")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; toDateString
   (js-bind! obj 'toDateString
      :value (js-make-function date-prototype-todatestring 0 "toDateString")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; toTimeString
   (js-bind! obj 'toTimeString
      :value (js-make-function date-prototype-totimestring 0 "toTimeString")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; toLocaleString
   (js-bind! obj 'toLocaleString
      :value (js-make-function date-prototype-tolocalestring 0 "toLocaleString")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; toLocaleDateString
   (js-bind! obj 'toLocaleDateString
      :value (js-make-function date-prototype-tolocaledatestring 0 "toLocaleDateString")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; toLocaleTimeString
   (js-bind! obj 'toLocaleTimeString
      :value (js-make-function date-prototype-tolocaletimestring 0 "toLocaleTimeString")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; toUTCString
   (js-bind! obj 'toUTCString
      :value (js-make-function date-prototype-toutcstring 0 "toUTCString")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; toISOString
   (js-bind! obj 'toISOString
      :value (js-make-function date-prototype-toisostring 0 "toISOString")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; toJSON
   (js-bind! obj 'toJSON
      :value (js-make-function date-prototype-toJSON 1 "toJSON")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; valueOf
   (js-bind! obj 'valueOf
      :value (js-make-function date-prototype-valueof 0 "valueOf")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getTime
   (js-bind! obj 'getTime
      :value (js-make-function date-prototype-gettime 0 "getTime")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getFullYear
   (js-bind! obj 'getFullYear
      :value (js-make-function date-prototype-getfullyear 0 "getFullYear")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getUTCFullYear
   (js-bind! obj 'getUTCFullYear
      :value (js-make-function date-prototype-getutcfullyear 0 "getUTCFullYear")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getMonth
   (js-bind! obj 'getMonth
      :value (js-make-function date-prototype-getmonth 0 "getMonth")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getUTCMonth
   (js-bind! obj 'getUTCMonth
      :value (js-make-function date-prototype-getutcmonth 0 "getUTCMonth")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getDate
   (js-bind! obj 'getDate
      :value (js-make-function date-prototype-getdate 0 "getDate")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getUTCDate
   (js-bind! obj 'getUTCDate
      :value (js-make-function date-prototype-getutcdate 0 "getUTCDate")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getDay
   (js-bind! obj 'getDay
      :value (js-make-function date-prototype-getday 0 "getDay")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getUTCDay
   (js-bind! obj 'getUTCDay
      :value (js-make-function date-prototype-getutcday 0 "getUTCDay")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getHours
   (js-bind! obj 'getHours
      :value (js-make-function date-prototype-gethours 0 "getHours")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getUTCHours
   (js-bind! obj 'getUTCHours
      :value (js-make-function date-prototype-getutchours 0 "getUTCHours")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getMinutes
   (js-bind! obj 'getMinutes
      :value (js-make-function date-prototype-getminutes 0 "getMinutes")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getUTCMinutes
   (js-bind! obj 'getUTCMinutes
      :value (js-make-function date-prototype-getutcminutes 0 "getUTCMinutes")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getSeconds
   (js-bind! obj 'getSeconds
      :value (js-make-function date-prototype-getseconds 0 "getSeconds")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getUTCSeconds
   (js-bind! obj 'getUTCSeconds
      :value (js-make-function date-prototype-getutcseconds 0 "getUTCSeconds")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getMilliseconds
   (js-bind! obj 'getMilliseconds
      :value (js-make-function date-prototype-getmilliseconds 0 "getMilliseconds")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getUTCMilliseconds
   (js-bind! obj 'getUTCMilliseconds
      :value (js-make-function date-prototype-getutcmilliseconds 0 "getUTCMilliseconds")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getTimezoneOffset
   (js-bind! obj 'getTimezoneOffset
      :value (js-make-function date-prototype-gettimezoneoffset 0 "getTimezoneOffset")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setTime
   (js-bind! obj 'setTime
      :value (js-make-function date-prototype-settime 1 "setTime")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; getYear
   (js-bind! obj 'getYear
      :value (js-make-function date-prototype-getyear 0 "getYear")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setYear
   (js-bind! obj 'setYear
      :value (js-make-function date-prototype-setyear 1 "setYear")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setMilliseconds
   (js-bind! obj 'setMilliseconds
      :value (js-make-function date-prototype-setmilliseconds 1 "setMilliseconds")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setUTCMilliseconds
   (js-bind! obj 'setUTCMilliseconds
      :value (js-make-function date-prototype-setutcmilliseconds 1 "setUTCMilliseconds")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setSeconds
   (js-bind! obj 'setSeconds
      :value (js-make-function date-prototype-setseconds 2 "setSeconds")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setUTCSeconds
   (js-bind! obj 'setUTCSeconds
      :value (js-make-function date-prototype-setutcseconds 2 "setUTCSeconds")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setMinutes
   (js-bind! obj 'setMinutes
      :value (js-make-function date-prototype-setminutes 3 "setMinutes")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setUTCMinutes
   (js-bind! obj 'setUTCMinutes
      :value (js-make-function date-prototype-setutcminutes 3 "setUTCMinutes")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setHours
   (js-bind! obj 'setHours
      :value (js-make-function date-prototype-sethours 4 "setHours")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setUTCHours
   (js-bind! obj 'setUTCHours
      :value (js-make-function date-prototype-setutchours 4 "setUTCHours")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setDate
   (js-bind! obj 'setDate
      :value (js-make-function date-prototype-setdate 1 "setDate")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setUTCDate
   (js-bind! obj 'setUTCDate
      :value (js-make-function date-prototype-setutcdate 1 "setUTCDate")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setMonth
   (js-bind! obj 'setMonth
      :value (js-make-function date-prototype-setmonth 2 "setMonth")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setUTCMonth
   (js-bind! obj 'setUTCMonth
      :value (js-make-function date-prototype-setutcmonth 2 "setUTCMonth")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setFullYear
   (js-bind! obj 'setFullYear
      :value (js-make-function date-prototype-setfullyear 3 "setFullYear")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; setUTCFullYear
   (js-bind! obj 'setUTCFullYear
      :value (js-make-function date-prototype-setutcfullyear 3 "setUTCFullYear")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; toGMTString
   (js-bind! obj 'toGMTString
      :value (js-make-function date-prototype-togmtstring 0 "toGMTString")
      :writable #t
      :configurable #t
      :enumerable #f)
   obj)

;*---------------------------------------------------------------------*/
;*    date-prototype-tostring ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.2     */
;*---------------------------------------------------------------------*/
(define (date-prototype-tostring this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date->string val)
	  "Invalid date")))

;*---------------------------------------------------------------------*/
;*    date-prototype-todatestring ...                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.3     */
;*---------------------------------------------------------------------*/
(define (date-prototype-todatestring this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (format "~a ~a ~2,0d ~d"
	     (day-aname (date-wday val))
	     (month-aname (date-month val))
	     (date-day val)
	     (date-year val))
	  "Invalid date")))

;*---------------------------------------------------------------------*/
;*    date-prototype-totimestring ...                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.4     */
;*---------------------------------------------------------------------*/
(define (date-prototype-totimestring this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (format "~2,0d:~2,0d:~2,0d ~a"
	     (date-hour val)
	     (date-minute val)
	     (date-second val)
	     (date-timezone val))
	  "Invalid date")))

;*---------------------------------------------------------------------*/
;*    date-prototype-tolocalestring ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.5     */
;*---------------------------------------------------------------------*/
(define (date-prototype-tolocalestring this::JsDate)
   (date-prototype-tostring this))

;*---------------------------------------------------------------------*/
;*    date-prototype-tolocaledatestring ...                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.6     */
;*---------------------------------------------------------------------*/
(define (date-prototype-tolocaledatestring this::JsDate)
   date-prototype-todatestring this)

;*---------------------------------------------------------------------*/
;*    date-prototype-tolocaletimestring ...                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.7     */
;*---------------------------------------------------------------------*/
(define (date-prototype-tolocaletimestring this::JsDate)
   date-prototype-totimestring this)

;*---------------------------------------------------------------------*/
;*    date-prototype-valueof ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.8     */
;*---------------------------------------------------------------------*/
(define (date-prototype-valueof this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date->seconds val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-gettime ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.9     */
;*---------------------------------------------------------------------*/
(define (date-prototype-gettime this::JsDate)
   
   (define (date->milliseconds date::date)
      (let ((seconds (*llong #l1000 (elong->llong (date->seconds date)))))
         (+llong seconds
            (quotientllong (elong->llong (date-nanosecond date))
               #l1000000))))
   
   (with-access::JsDate this (val)
      (if (date? val)
	  (date->milliseconds val)
 	  +nan.0)))
 	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getfullyear ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.10    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getfullyear this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-year val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getutcfullyear ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.11    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getutcfullyear this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-year val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getmonth ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.12    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getmonth this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (-fx (date-month val) 1)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getutcmonth ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.13    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getutcmonth this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-month val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getdate ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.14    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getdate this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-day val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getutcdate ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.15    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getutcdate this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-day val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getday ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.16    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getday this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-wday val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getutcday ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.17    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getutcday this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-wday val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-gethours ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.18    */
;*---------------------------------------------------------------------*/
(define (date-prototype-gethours this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-hour val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getutchours ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.19    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getutchours this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-hour val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getminutes ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.20    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getminutes this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-minute val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getutcminutes ...                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.21    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getutcminutes this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-minute val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getseconds ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.22    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getseconds this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-second val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getutcseconds ...                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.23    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getutcseconds this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date-second val)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getmilliseconds ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.24    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getmilliseconds this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (* 1000 (date-nanosecond val))
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-getutcmilliseconds ...                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.25    */
;*---------------------------------------------------------------------*/
(define (date-prototype-getutcmilliseconds this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (* 1000 (date-nanosecond val))
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-gettimezoneoffset ...                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.26    */
;*---------------------------------------------------------------------*/
(define (date-prototype-gettimezoneoffset this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (/fx (date-timezone val) 60)
	  +nan.0)))

;*---------------------------------------------------------------------*/
;*    date-prototype-settime ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.27    */
;*---------------------------------------------------------------------*/
(define (date-prototype-settime this::JsDate time)
   (with-access::JsDate this (val)
      (set! val (seconds->date time))
      (date->seconds val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setmilliseconds ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.28    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setmilliseconds this::JsDate ms)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date->seconds val)
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setutcmilliseconds ...                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.29    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setutcmilliseconds this::JsDate ms)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date->seconds val)
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setseconds ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.30    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setseconds this::JsDate sec ms)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((sec (js-tonumber sec)))
	     (set! val (date-copy val :sec sec))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setutcseconds ...                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.31    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setutcseconds this::JsDate sec ms)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((sec (js-tonumber sec)))
	     (set! val (date-copy val :sec sec))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setminutes ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.32    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setminutes this::JsDate min sec ms)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((min (js-tonumber min))
		(sec (unless (eq? sec (js-undefined)) (js-tonumber sec))))
	     (set! val (date-copy val :min min :sec sec))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setutcminutes ...                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.33    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setutcminutes this::JsDate min sec ms)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((min (js-tonumber min))
		(sec (unless (eq? sec (js-undefined)) (js-tonumber sec))))
	     (set! val (date-copy val :min min :sec sec))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-sethours ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.34    */
;*---------------------------------------------------------------------*/
(define (date-prototype-sethours this::JsDate hour min sec ms)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((hour (js-tonumber hour))
		(min (unless (eq? min (js-undefined)) (js-tonumber min)))
		(sec (unless (eq? sec (js-undefined)) (js-tonumber sec))))
	     (set! val (date-copy val :hour hour :min min :sec sec))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setutchours ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.35    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setutchours this::JsDate hour min sec ms)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((hour (js-tonumber hour))
		(min (unless (eq? min (js-undefined)) (js-tonumber min)))
		(sec (unless (eq? sec (js-undefined)) (js-tonumber sec))))
	     (set! val (date-copy val :hour hour :min min :sec sec))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setdate ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.36    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setdate this::JsDate date)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((date (js-tonumber date)))
	     (set! val (date-copy val :day date))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setutcdate ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.37    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setutcdate this::JsDate date)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((date (js-tonumber date)))
	     (set! val (date-copy val :day date))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setmonth ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.38    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setmonth this::JsDate month date)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((month (js-tonumber month))
		(date (unless (eq? date (js-undefined)) (js-tonumber date))))
	     (set! val (date-copy val :month month :day date))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setutcmonth ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.39    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setutcmonth this::JsDate month date)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((month (js-tonumber month))
		(date (when date (js-tonumber date))))
	     (set! val (date-copy val :month month :day date))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setfullyear ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.40    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setfullyear this::JsDate year month date)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((year (js-tonumber year))
		(month (unless (eq? month (js-undefined)) (js-tonumber month)))
		(date (unless (eq? date (js-undefined)) (js-tonumber date))))
	     (set! val (date-copy val :year year :month month :day date))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-setutcfullyear ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.41    */
;*---------------------------------------------------------------------*/
(define (date-prototype-setutcfullyear this::JsDate year month date)
   (with-access::JsDate this (val)
      (if (date? val)
	  (let ((year (js-tonumber year))
		(month (unless (eq? month (js-undefined)) (js-tonumber month)))
		(date (unless (eq? date (js-undefined)) (js-tonumber date))))
	     (set! val (date-copy val :year year :month month :day date))
	     (date->seconds val))
	  val)))

;*---------------------------------------------------------------------*/
;*    date-prototype-toutcstring ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.42    */
;*---------------------------------------------------------------------*/
(define (date-prototype-toutcstring this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (date->utc-string val)
	  "Invalid date")))

;*---------------------------------------------------------------------*/
;*    date->utc-date ...                                               */
;*---------------------------------------------------------------------*/
(define (date->utc-date dt::date)
   (let ((tz (date-timezone dt)))
      (date-copy (seconds->date (+ (date->seconds dt) tz)) :timezone 0)))

;*---------------------------------------------------------------------*/
;*    date-prototype-toisostring ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.43    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15    */
;*---------------------------------------------------------------------*/
(define (date-prototype-toisostring this)
   (if (not (isa? this JsDate))
       (js-raise
	  (js-new js-type-error
	     (format "Not a date ~s" (typeof this))))
       (with-access::JsDate this (val ms)
	  (if (date? val)
	      (let loop ((val val))
		 (if (=fx (date-timezone val) 0)
		     (format "~4,0d-~2,0d-~2,0dT~2,0d:~2,0d:~2,0d.~3,0dZ"
			(date-year val)
			(date-month val)
			(date-day val)
			(date-hour val)
			(date-minute val)
			(date-second val)
			(/fx (date-nanosecond val) 1000))
		     (loop (date->utc-date val))))
	      (js-raise
		 (js-new js-range-error
		    (format "Invalide date ~s" val)))))))

;*---------------------------------------------------------------------*/
;*    date-prototype-toJSON ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.5.44    */
;*---------------------------------------------------------------------*/
(define (date-prototype-toJSON this::JsDate)
   (let* ((o (js-toobject this))
	  (tv (js-toprimitive o 'number)))
      (if (and (number? tv) (not (integer? tv)))
	  (js-null)
	  (let ((p (js-get o 'toISOString)))
	     (if (isa? p JsFunction)
		 (js-call0 p o)
		 (js-raise
		    (js-new js-type-error
		       (format "toJSON: argument not a function ~s" p))))))))

;*---------------------------------------------------------------------*/
;*    date-prototype-getyear ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.4        */
;*---------------------------------------------------------------------*/
(define (date-prototype-getyear this::JsDate)
   (with-access::JsDate this (val)
      (if (date? val)
	  (-fx (date-year val) 1900)
	  +nan.0)))
	 
;*---------------------------------------------------------------------*/
;*    date-prototype-setyear ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.5        */
;*---------------------------------------------------------------------*/
(define (date-prototype-setyear this::JsDate year)
   (with-access::JsDate this (val)
      (let ((r2 (js-tonumber year)))
	 (if (and (real? r2) (nanfl? r2))
	     (begin
		(set! val #f)
		r2)
	     (let* ((r2i (js-tointeger r2))
		    (r4 (if (and (>=fx r2i 0) (<=fx r2i 99))
			    (+fx r2i 1900)
			    r2))
		    (r (if (date? val)
			   (date-copy val :year r4)
			   (make-date :year r4))))
		(set! val r)
		r)))))

;*---------------------------------------------------------------------*/
;*    date-prototype-togmtstring ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.6        */
;*---------------------------------------------------------------------*/
(define (date-prototype-togmtstring this::JsDate)
   (date-prototype-toutcstring this))

