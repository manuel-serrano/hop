;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/hopscript/json.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Mon Feb  3 14:10:31 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript Json                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.12        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_json

   (library web)
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_function
	   __hopscript_error
	   __hopscript_array)

   (export js-json
	   (js-init-json! ::JsObject)))

;*---------------------------------------------------------------------*/
;*    js-json ...                                                      */
;*---------------------------------------------------------------------*/
(define js-json #f)

;*---------------------------------------------------------------------*/
;*    js-init-json! ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.12        */
;*---------------------------------------------------------------------*/
(define (js-init-json! %this)
   (set! js-json
      (instantiate::JsJSON
	 (extensible #t)
	 (__proto__ js-object-prototype)))
   ;; parse
   (js-bind! js-json 'parse
      :value (js-make-function js-json-parse 2 "parse")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; stringify
   (js-bind! js-json 'stringify
      :value (js-make-function js-json-stringify 3 "stringify")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; bind the global object
   (js-bind! %this 'JSON :configurable #t :value js-json)
   js-json)

;*---------------------------------------------------------------------*/
;*    js-json-parse ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.12.2      */
;*---------------------------------------------------------------------*/
(define (js-json-parse this text reviver)
   (call-with-input-string text
      (lambda (ip)
	 (json-parse ip
	    :array-alloc (lambda () (js-new js-array 10))
	    :array-set (lambda (a i val) (js-put! a i val #f))
	    :array-return (lambda (a i) (js-put! a 'length i #f) a)
	    :object-alloc (lambda () (js-new js-object))
	    :object-set (lambda (o p val) (js-put! o (js-toname p) val #f))
	    :object-return (lambda (o) o)
	    :parse-error (lambda (msg token loc)
			    (js-raise (js-new js-syntax-error msg)))
	    :reviver (when (isa? reviver JsFunction)
			(lambda (this key val)
			   (let ((res (js-call2 reviver this key val)))
			      (unless (eq? res (js-undefined))
				 res))))))))

;*---------------------------------------------------------------------*/
;*    js-json-escape ...                                               */
;*---------------------------------------------------------------------*/
;* (define js-json-escape                                              */
;*    (pregexp "[\\\"\x7f-\x9f\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]" 'JAVASCRIPT_COMPAT 'UTF8)) */

;*---------------------------------------------------------------------*/
;*    js-json-stringify ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.12.3      */
;*---------------------------------------------------------------------*/
(define (js-json-stringify this value replacer space)

   (define (toVALUE value)
      (if (or (isa? value JsNumber)
	      (isa? value JsString)
	      (isa? value JsBoolean)
	      (isa? value JsDate))
	  (js-valueof value)
	  value))

   (define (toJSON value key)
      (if (isa? value JsObject)
	  (let ((tojson (js-get value 'toJSON)))
	     (if (isa? tojson JsFunction)
		 (toVALUE (js-call1 tojson value key))
		 (toVALUE value)))
	  value))

   (define (string-count str)
      (let ((len (string-length str)))
	 (let loop ((i 0)
		    (n 0))
	    (if (=fx i len)
		n
		(let ((c (string-ref-ur str i)))
		   (case c
		      ((#a008 #\tab #\newline #a012 #a013 #\" #\' #\\)
		       (loop (+fx i 1) (+fx n 2)))
		      (else
		       (cond
			  ((char<=? c #a031)
			   (loop (+fx i 1) (+fx n 6)))
			  (else
			   (loop (+fx i 1) (+fx n 1)))))))))))
      
   (define (string-quote str)
      (let ((len (string-length str))
	    (count (string-count str)))
	 (if (=fx len count)
	     (string-append "\"" str "\"")
	     (let ((nstr (make-string (+fx count 2))))
		(string-set! nstr 0 #\")
		(string-set! nstr (+fx count 1) #\")
		(let loop ((r 0)
			   (w 1))
		   (if (=fx r len)
		       nstr
		       (let ((c (string-ref-ur str r)))
			  (case c
			     ((#a008)
			      (string-set! nstr w #\\)
			      (string-set! nstr (+fx w 1) #\b)
			      (loop (+fx r 1) (+fx w 2)))
			     ((#\tab)
			      (string-set! nstr w #\\)
			      (string-set! nstr (+fx w 1) #\t)
			      (loop (+fx r 1) (+fx w 2)))
			     ((#\newline)
			      (string-set! nstr w #\\)
			      (string-set! nstr (+fx w 1) #\n)
			      (loop (+fx r 1) (+fx w 2)))
			     ((#a012)
			      (string-set! nstr w #\\)
			      (string-set! nstr (+fx w 1) #\f)
			      (loop (+fx r 1) (+fx w 2)))
			     ((#a013)
			      (string-set! nstr w #\\)
			      (string-set! nstr (+fx w 1) #\r)
			      (loop (+fx r 1) (+fx w 2)))
			     ((#\")
			      (string-set! nstr w #\\)
			      (string-set! nstr (+fx w 1) #\")
			      (loop (+fx r 1) (+fx w 2)))
			     ((#\')
			      (string-set! nstr w #\\)
			      (string-set! nstr (+fx w 1) #\')
			      (loop (+fx r 1) (+fx w 2)))
			     ((#\\)
			      (string-set! nstr w #\\)
			      (string-set! nstr (+fx w 1) #\\)
			      (loop (+fx r 1) (+fx w 2)))
			     (else
			      (if (char<=? c #a031)
				  (begin
				     (string-set! nstr w #\\)
				     (string-set! nstr (+fx w 1) #\u)
				     (blit-string! (format "~4,0x" (char->integer c)) 0
					nstr (+fx w 2) 4)
				     (loop (+fx r 1) (+fx w 6)))
				  (begin
				   (string-set! nstr w c)
				   (loop (+fx r 1) (+fx w 1)))))))))))))

   (define indent
      (let loop ((space space))
	 (cond
	    ((number? space)
	     (cond
		((> space 10)
		 "          ")
		((< space 1)
		 "")
		(else
		 (make-string (->fixnum (js-tointeger space)) #\space))))
	    ((string? space)
	     (if (>fx (string-length space) 10)
		 (substring space 0 10)
		 space))
	    ((isa? space JsNumber)
	     (loop (js-valueof space)))
	    ((isa? space JsString)
	     (loop (js-valueof space)))
	    (else
	     ""))))

   (define rep
      (cond
	 ((isa? replacer JsFunction) replacer)
	 ((isa? replacer JsArray) replacer)
	 (else #f)))

   (define gap "")

   (define (lst key holder value mind opar cpar proc)
      (let ((len (js-get value 'length)))
	 (if (= len 0)
	     (string-append opar cpar)
	     (call-with-output-string
		(lambda (op)
		   (display opar op)
		   (unless (string-null? gap)
		      (newline op)
		      (display gap op))
		   (display (proc 0) op)
		   (let liip ((i 1))
		      (if (= i len)
			  (begin
			     (unless (string-null? gap)
				(display "\n" op)
				(display mind op))
			     (display cpar op))
			  (begin
			     (if (string-null? gap)
				 (display "," op)
				 (begin
				    (display ",\n" op)
				    (display gap op)))
			     (display (proc i) op)
			     (liip (+fx i 1)))))
		   (set! gap mind))))))
   
   (define (str key holder stack)
      (let* ((value (js-get holder key))
	     (value (toJSON value key))
	     (value (if (isa? rep JsFunction)
			(toVALUE (js-call2 rep holder key value))
			value))
	     (mind gap))
	 (cond
	    ((memq value stack)
	     (js-raise-type-error "Converting circular structure to JSON ~s" (typeof value)))
	    ((string? value)
	     (string-quote value))
	    ((number? value)
	     (number->string value))
	    ((eq? (js-null) value)
	     "null")
	    ((eq? value #t)
	     "true")
	    ((eq? value #f)
	     "false")
	    ((eq? value (js-undefined))
	     value)
	    (else
	     (set! gap (string-append gap indent))
	     (cond
		((isa? value JsFunction)
		 (js-undefined))
		((isa? value JsArray)
		 ;; value is an array
		 (let ((res (lst key holder value mind "[" "]"
			       (lambda (i)
				  (str i value stack)))))
		    (set! gap mind)
		    res))
		((and (isa? rep JsObject) (not (isa? rep JsFunction)))
		 ;; rep is an array
		 (let ((res (lst key holder value mind "{" "}"
			       (lambda (i)
				  (let ((k (js-get rep i)))
				     (if (string? k)
					 (let ((v (str k value stack)))
					    (when (js-totest v)
					       (format "~a~a~a"
						  (string-quote k)
						  (if gap ": " ":")
						  v)))))))))
		    (set! gap mind)
		    res))
		(else
		 (let ((i 0)
		       (nstack (cons value stack)))
		    (call-with-output-string
		       (lambda (op)
			  (js-for-in value
			     (lambda (k)
				(when (js-object-prototype-hasownproperty value k)
				   (let ((v (str k value nstack)))
				      (when (js-totest v)
					 (if (=fx i 0)
					     (begin
						(set! i (+fx i 1))
						(if (string-null? gap)
						    (display "{" op)
						    (begin
						       (display "{\n" op)
						       (display gap op))))
					     (if (string-null? gap)
						 (display "," op)
						 (begin
						    (display ",\n" op)
						    (display gap op))))
					 (display (string-quote k) op)
					 (display
					    (if (string-null? gap) ":" ": ")
					    op)
					 (display v op))))))
			  (cond
			     ((=fx i 0)
			      (display "{}" op))
			     ((string-null? gap)
			      (display "}" op))
			     (else
			      (display "\n" op)
			      (display mind op)
			      (display "}" op)))
			  (set! gap mind))))))))))

   (let ((holder (js-new js-object)))
      (js-put! holder '|| value #f)
      (str '|| holder '())))

;*---------------------------------------------------------------------*/
;*    js-json-stringify ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.12.3      */
;*---------------------------------------------------------------------*/
(define (js-json-stringify-old this value replacer space)
   (if (eq? value (js-undefined))
       (js-undefined)
       (call-with-output-string
	  (lambda (op)
	     (let loop ((value value))
		(cond
		   ((string? value)
		    (display "\"" op)
		    (display value op)
		    (display "\"" op))
		   ((number? value)
		    (display value op))
		   ((eq? (js-null) value)
		    (display "null" op))
		   ((eq? value #t)
		    (display "true" op))
		   ((eq? value #f)
		    (display "false" op))
		   ((isa? value JsArray)
		    (let ((len (js-get value 'length)))
		       (if (= len 0)
			   (display "[]" op)
			   (begin
			      (display "[" op)
			      (loop (js-get value (js-toname 0)))
			      (let liip ((i 1))
				 (if (= i len)
				     (display "]" op)
				     (begin
					(display "," op)
					(when (js-has-property value (js-toname i))
					   (loop (js-get value i)))
					(liip (+ i 1)))))))))
		   ((isa? value JsObject)
		    (display "{" op)
		    (js-for-in value
		       (lambda (pname)
			  (let ((val (js-get value pname)))
			     (unless (eq? val (js-undefined))
				(display "\"" op)
				(display pname op)
				(display "\":" op)
				(loop val)
				(display "," op)))))
		    (display "}" op))
		   (else
		    (js-raise-type-error "Illegal value ~s" (typeof value)))))))))
