;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/json.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Thu Oct 10 18:48:06 2019 (serrano)                */
;*    Copyright   :  2013-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript Json                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.12        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_json

   (library web hop)
   
   (include "types.sch" "stringliteral.sch" "property.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_function
	   __hopscript_error
	   __hopscript_array)

   (export (js-init-json! ::JsObject)
	   (js-json-stringify ::obj ::obj ::obj ::obj ::JsGlobalObject)
	   (js-json-parser ::input-port ::obj ::bool ::bool ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsJSON ...                                           */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsJSON worker::WorkerHopThread %_this)
   (js-undefined))

;*---------------------------------------------------------------------*/
;*    js-init-json! ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.12        */
;*---------------------------------------------------------------------*/
(define (js-init-json! %this)
   (with-access::JsGlobalObject %this (__proto__ js-json js-json-pcache)
      ;; json pcache
      (set! js-json-pcache
	 ((@ js-make-pcache-table __hopscript_property) 2 "json"))
      ;; local constant strings
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      (set! js-json
	 (instantiateJsJSON
	    (__proto__ __proto__)))
      ;; parse
      (js-bind! %this js-json (& "parse")
	 :value (js-make-function %this (js-json-parse %this) 2 "parse")
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #t)
      ;; stringify
      (js-bind! %this js-json (& "stringify")
	 :value (js-make-function %this
		   (lambda (this value replacer space)
		      (js-json-stringify this value replacer space %this))
		   3 "stringify")
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #t)
      ;; bind the global object
      (js-bind! %this %this (& "JSON")
	 :configurable #t :value js-json :enumerable #f
	 :hidden-class #t)
      js-json))

;*---------------------------------------------------------------------*/
;*    js-json-parse ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.12.2      */
;*---------------------------------------------------------------------*/
(define (js-json-parse %this::JsGlobalObject)
   (lambda (this text reviver)
      (call-with-input-string (js-tostring text %this)
	 (lambda (ip)
	    (js-json-parser ip reviver #f #f %this)))))

;*---------------------------------------------------------------------*/
;*    js-json-parser ...                                               */
;*---------------------------------------------------------------------*/
(define (js-json-parser ip::input-port reviver expr undefined %this::JsGlobalObject)
   (json-parse ip
      :expr expr
      :undefined undefined
      :string-alloc js-string->jsstring
      :constant-alloc (lambda (n)
			 (cond
			    ((elong? n) (elong->flonum n))
			    ((llong? n) (llong->flonum n))
			    (else n)))
      :array-alloc (lambda ()
		      (with-access::JsGlobalObject %this (js-array)
			 (make-cell '())))
      :array-set (lambda (a i val)
		    (cell-set! a (cons val (cell-ref a))))
      :array-return (lambda (a i)
		       (js-vector->jsarray
			  (list->vector (reverse! (cell-ref a))) %this))
      :object-alloc (lambda ()
		       (with-access::JsGlobalObject %this (js-object)
			  (js-new0 %this js-object)))
      :object-set (lambda (o p val)
		     (let* ((name (js-toname p %this))
			    (desc (instantiate::JsValueDescriptor
				     (name name)
				     (value val)
				     (enumerable #t)
				     (configurable #t)
				     (writable #t))))
			(js-define-own-property o name desc #f %this)))
      :object-return (lambda (o) o)
      :parse-error (lambda (msg fname loc)
		      (js-raise-syntax-error %this msg #f
			 (if (string? fname) (js-string->jsstring fname) ip)
			 loc))
      :reviver (when (js-function? reviver)
		  (lambda (this key val)
		     (let ((res (js-call2 %this reviver this key val)))
			(unless (eq? res (js-undefined))
			   res))))))

;*---------------------------------------------------------------------*/
;*    js-json-stringify ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.12.3      */
;*---------------------------------------------------------------------*/
(define (js-json-stringify this value replacer space %this::JsGlobalObject)
   
   (define (toVALUE value)
      (if (or (isa? value JsNumber)
	      (isa? value JsString)
	      (isa? value JsBoolean)
	      (isa? value JsDate))
	  (js-valueof value %this)
	  value))
   
   (define (toJSON value key)
      (with-access::JsGlobalObject %this (js-json-pcache)
	 (if (js-object? value)
	     (let ((tojson (js-get-name/cache value (& "toJSON") #f %this
			      (js-pcache-ref js-json-pcache 0))))
		(if (js-function? tojson)
		    (toVALUE (js-call1 %this tojson value key))
		    (toVALUE value)))
	     value)))
   
   (define (string-count str)
      (let ((len (string-length str)))
	 (let loop ((i 0)
		    (n 0))
	    (if (=fx i len)
		n
		(let ((c (string-ref-ur str i)))
		   (case c
		      ((#a008 #\tab #\newline #a012 #a013 #\" #\\)
		       (loop (+fx i 1) (+fx n 2)))
		      (else
		       (cond
			  ((char<=? c #a031)
			   (loop (+fx i 1) (+fx n 6)))
			  (else
			   (loop (+fx i 1) (+fx n 1)))))))))))
   
   (define (string-quote::obj s::JsStringLiteral)
      (let* ((str (js-jsstring->string s))
	     (len (string-length str))
	     (count (string-count str)))
	 (if (=fx len count)
	     (js-jsstring-append (& "\"") (js-jsstring-append s (& "\"")))
	     (let ((nstr (make-string (+fx count 2))))
		(string-set! nstr 0 #\")
		(string-set! nstr (+fx count 1) #\")
		(let loop ((r 0)
			   (w 1))
		   (if (=fx r len)
		       (js-string->jsstring nstr)
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
	    ((js-number? space)
	     (cond
		((> space 10)
		 "          ")
		((< space 1)
		 "")
		(else
		 (make-string
		    (->fixnum (js-tointeger space %this)) #\space))))
	    ((js-jsstring? space)
	     (let ((space (js-jsstring->string space)))
		(if (>fx (string-length space) 10)
		    (substring space 0 10)
		    space)))
	    ((isa? space JsNumber)
	     (loop (js-valueof space %this)))
	    ((isa? space JsString)
	     (loop (js-valueof space %this)))
	    (else
	     ""))))
   
   (define rep
      (cond
	 ((js-function? replacer) replacer)
	 ((js-array? replacer) replacer)
	 ((isa? replacer JsTypedArray) replacer)
	 ((isa? replacer JsArrayBufferView) replacer)
	 (else #f)))
   
   (define gap "")
   
   (define (lst::obj holder value mind opar::bstring cpar::bstring proc)
      (with-access::JsGlobalObject %this (js-json-pcache)
	 (let ((len (js-get-name/cache value (& "length") #f %this
		       (js-pcache-ref js-json-pcache 1))))
	    (js-string->jsstring 
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
			 (set! gap mind))))))))
   
   (define (for-in obj proc)
      
      (define (vfor-each2 proc vec)
	 (let ((len (vector-length vec)))
	    (let loop ((i 0))
	       (when (<fx i len)
		  (proc (vector-ref-ur vec i))
		  (loop (+fx i 1))))))
      
      (define (vfor-each3 proc vec vecname)
	 (let ((len (vector-length vecname)))
	    (let loop ((i 0))
	       (when (<fx i len)
		  (proc (vector-ref vec i) (vector-ref vecname i))
		  (loop (+fx i 1))))))
      
      (define (in-mapped-property el-or-descr prop)
	 (when (and (js-jsstring? (prop-name prop))
		    (flags-enumerable? (prop-flags prop)))
	    (cond
	       ((isa? el-or-descr JsPropertyDescriptor)
		(with-access::JsPropertyDescriptor el-or-descr (enumerable)
		   (when (eq? enumerable #t)
		      (proc (prop-name prop)))))
	       (else
		(proc (prop-name prop))))))
      
      (define (in-property p)
	 (when (isa? p JsPropertyDescriptor)
	    (with-access::JsPropertyDescriptor p (name enumerable)
	       (when (and (js-jsstring? name) enumerable)
		  (proc name)))))
      
      (cond
	 ((js-object? obj)
	  (let loop ((o obj))
	     (with-access::JsObject o (elements cmap __proto__)
		(if (not (eq? cmap (js-not-a-cmap)))
		    (with-access::JsConstructMap cmap (props)
		       (vfor-each3 in-mapped-property elements props))
		    (for-each in-property (js-object-properties o))))))
	 ((object? obj)
	  (vfor-each2 (lambda (f) (proc (class-field-name f)))
	     (class-all-fields (object-class obj))))
	 (else
	  '#())))
   
   (define (str key holder stack #!optional (symbol (js-undefined)))
      (let* ((value (js-object-get/name-cache holder key %this))
	     (value (toJSON value key))
	     (value (if (js-function? rep)
			(toVALUE (js-call2 %this rep holder key value))
			value))
	     (mind gap))
	 (cond
	    ((memq value stack)
	     (js-raise-type-error %this
		"Converting circular structure to JSON ~a"
		(format "[~a: ~a]" key (js-tostring value %this))))
	    ((js-jsstring? value)
	     (string-quote value))
	    ((js-number? value)
	     (cond
		((fixnum? value)
		 (js-ascii->jsstring (fixnum->string value)))
		((not (infinitefl? value))
		 (js-ascii->jsstring (number->string value)))
		((= value +inf.0)
		 (& "Infinity"))
		(else
		 (& "-Infinity"))))
	    ((eq? (js-null) value)
	     (& "null"))
	    ((eq? value #t)
	     (& "true"))
	    ((eq? value #f)
	     (& "false"))
	    ((eq? value (js-undefined))
	     symbol)
	    ((isa? value JsSymbolLiteral)
	     symbol)
	    (else
	     (set! gap (string-append gap indent))
	     (cond
		((js-function? value)
		 (js-undefined))
		((or (js-array? value) (js-proxy-array? value))
		 (let ((res (lst holder value mind "[" "]"
			       (lambda (i)
				  (str i value stack
				     (js-ascii->jsstring "null"))))))
		    (set! gap mind)
		    res))
		((and (js-object? rep) (not (js-function? rep)))
		 (let ((res (lst holder value mind "{" "}"
			       (lambda (i)
				  (let ((k (js-get rep i %this)))
				     (when (js-jsstring? k)
					(let ((v (str k value stack)))
					   (when (js-totest v)
					      (js-jsstring-append
						 (string-quote k)
						 (js-jsstring-append
						    (if gap
							(& ": ")
							(& ":"))
						    v))))))))))
		    (set! gap mind)
		    res))
		(else
		 (let ((i 0)
		       (nstack (cons value stack)))
		    (js-string->jsstring
		       (call-with-output-string
			  (lambda (op)
			     (for-in value
				(lambda (k)
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
					 (display v op)))))
			     (cond
				((=fx i 0)
				 (display "{}" op))
				((string-null? gap)
				 (display "}" op))
				(else
				 (display "\n" op)
				 (display mind op)
				 (display "}" op)))
			     (set! gap mind)))))))))))
   
   (define (default)
      (with-access::JsGlobalObject %this (js-object)
	 (let ((holder (js-new0 %this js-object)))
	    (let* ((name (& ""))
		   (desc (instantiate::JsValueDescriptor
			    (name name)
			    (value value)
			    (enumerable #t)
			    (configurable #t)
			    (writable #t))))
		(js-define-own-property holder name desc #f %this))
	    (str (& "") holder '()))))

   (cond
      ((isa? replacer JsFunction)
       (default))
      ((js-jsstring? value)
       (string-quote value))
      ((js-number? value)
       (cond
	  ((fixnum? value)
	   (js-ascii->jsstring (fixnum->string value)))
	  ((not (infinitefl? value))
	   (js-ascii->jsstring (number->string value)))
	  ((= value +inf.0)
	   (& "Infinity"))
	  (else
	   (& "-Infinity"))))
      ((eq? (js-null) value)
       (& "null"))
      ((eq? value #t)
       (& "true"))
      ((eq? value #f)
       (& "false"))
      (else
       (default))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
