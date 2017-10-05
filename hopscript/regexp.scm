;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/regexp.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Thu Oct  5 05:40:38 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript regexps                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_regexp

   (library hop)
   
   (include "stringliteral.sch"
	    "property.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_array
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_string
	   __hopscript_stringliteral
	   __hopscript_error)

   (export (js-init-regexp! ::JsGlobalObject)
	   (inline js-regexp?::bool ::obj)
	   (js-regexp->jsregexp ::regexp ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    property caches ...                                              */
;*---------------------------------------------------------------------*/
(%define-pcache 3)
(define %pcache (js-make-pcache 3))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsRegExp ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsRegExp
   (lambda (o)
      (with-access::JsRegExp o (rx) rx))
   (lambda (o %this)
      (js-regexp->jsregexp o (or %this (js-initial-global-object)))))

;*---------------------------------------------------------------------*/
;*    js-regexp? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-regexp? obj)
   (isa? obj JsRegExp))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsRegExp ...                                         */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsRegExp worker::WorkerHopThread %_this)
   (with-access::WorkerHopThread worker (%this)
      (with-access::JsGlobalObject %this (js-regexp)
	 (let ((nobj (call-next-method)))
	    (with-access::JsRegExp nobj (__proto__ rx)
	       (with-access::JsRegExp obj ((_rx rx))
		  (set! __proto__ (js-get js-regexp 'prototype %this))
		  (set! rx (js-donate _rx worker %_this))))
	    nobj))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsRegexp ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsRegExp op compile isexpr)
   (let ((%this (js-initial-global-object)))
      (display "/" op)
      (display (js-tostring (js-get o 'source %this) %this) op)
      (display "/" op)
      (when (js-totest (js-get o 'global %this)) (display "g" op))
      (when (js-totest (js-get o 'ignoreCase %this)) (display "i" op))
      (when (js-totest (js-get o 'multiline %this)) (display "m" op))))

;*---------------------------------------------------------------------*/
;*    js-init-regexp! ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10        */
;*---------------------------------------------------------------------*/
(define (js-init-regexp! %this::JsGlobalObject)
   ;; first, create the builtin prototype
   (with-access::JsGlobalObject %this (__proto__ js-regexp js-function
					 js-regexp-prototype)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))
	 
	 (set! js-regexp-prototype
	    (instantiate::JsRegExp
	       (lastindex (instantiate::JsValueDescriptor
			     (name 'lastIndex)
			     (writable #t)
			     (value 0)))
	       (rx (pregexp ""))
	       (__proto__ __proto__)))
	 
	 ;; create a HopScript regexp object constructor
	 (set! js-regexp
	    (js-make-function %this
	       (%js-regexp %this) 2 'RegExp
	       :__proto__ js-function-prototype
	       :prototype js-regexp-prototype
	       :construct (js-regexp-construct %this)))
	 (init-builtin-regexp-prototype! %this js-regexp js-regexp-prototype)
	 ;; bind Regexp in the global object
	 (js-bind! %this %this 'RegExp
	    :configurable #f :enumerable #f :value js-regexp
	    :hidden-class #t)
	 js-regexp)))
   
;*---------------------------------------------------------------------*/
;*    %js-regexp ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.3.1    */
;*---------------------------------------------------------------------*/
(define (%js-regexp %this::JsGlobalObject)
   (lambda (this pattern flags)
      (with-access::JsGlobalObject %this (js-regexp)
	 (if (and (isa? pattern JsRegExp) (eq? flags (js-undefined)))
	     pattern
	     (js-new %this js-regexp pattern flags)))))

;*---------------------------------------------------------------------*/
;*    make-js-regexp-pattern ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-7.8.5        */
;*---------------------------------------------------------------------*/
(define (make-js-regexp-pattern %this str::bstring)
   
   (define (err fmt str)
      (js-raise-syntax-error %this fmt str))
   
   (define (char-alpha c)
      (cond
	 ((char>=? c #\a)
	  (when (char<=? c #\f)
	     (+fx 10 (-fx (char->integer c) (char->integer #\a)))))
	 ((char>=? c #\A)
	  (when (char<=? c #\F)
	     (+fx 10 (-fx  (char->integer c) (char->integer #\A)))))
	 ((char>=? c #\0)
	  (when (char<=? c #\9)
	     (-fx (char->integer c) (char->integer #\0))))
	 (else
	  #f)))
   
   (define (hex2 str j)
      (let ((n1 (char-alpha (string-ref str j))))
	 (when n1
	    (let ((n2 (char-alpha (string-ref str (+fx j 1)))))
	       (when n2
		  (+fx (*fx n1 16) n2))))))
   
   (define (hex4 str j)
      (let ((n1 (hex2 str j)))
	 (when n1
	    (let ((n2 (hex2 str (+fx j 2))))
	       (when n2
		  (+fx (*fx n1 256) n2))))))
   
   (define (integer->utf8 n)
      (cond
	 ((and (>=fx n #xD800) (<=fx n #xdbff))
	  ;; MS 9feb2016: don't know what to do as PCRE cannot handle
	  ;; "red" cells https://en.wikipedia.org/wiki/UTF-8
	  (ucs2-string->utf8-string
	     (make-ucs2-string 1 (integer->ucs2 #xd7ff))))
	 (else
	  (let ((u (make-ucs2-string 1 (integer->ucs2 n))))
	     (ucs2-string->utf8-string u)))))
   
   (let* ((len (string-length str))
	  (res (make-string (*fx 2 len))))
      (let loop ((i 0)
		 (w 0)
		 (inrange #f))
	 (let ((j (string-index str "\\[]" i)))
	    (if (not j)
		(begin
		   (blit-string! str i res w (-fx len i))
		   (string-shrink! res (+fx w (-fx len i)))
		   res)
		(let ((tag (string-ref str j)))
		   (when (>fx j i)
		      (blit-string! str i res w (-fx j i))
		      (set! w (+fx w (-fx j i))))
		   (cond
		      ((char=? tag #\[)
		       (string-set! res w tag)
		       (loop (+fx j 1) (+fx w 1) #t))
		      ((char=? tag #\])
		       (if inrange
			   (begin
			      (string-set! res w tag)
			      (loop (+fx j 1) (+fx w 1) #f))
			   (begin
			      (string-set! res w #\\)
			      (string-set! res (+fx w 1) tag)
			      (loop (+fx j 1) (+fx w 2) #f))))
		      ((=fx j (-fx len 1))
		       (err "wrong pattern \"~a\"" str))
		      (else
		       (let ((c (string-ref str (+fx j 1))))
			  (case c
			     ((#\x)
			      (if (>=fx j (-fx len 3))
				  (err "wrong \"\\x\" pattern \"~a\"" str)
				  (let ((n (hex2 str (+fx j 2))))
				     (cond
					((not n)
					 (err "wrong \"\\x\" pattern \"~a\"" str))
					((=fx n 0)
					 (blit-string! "\\000" 0 res w 4)
					 (loop (+fx j 4) (+fx w 4) inrange))
					((=fx n #x5d)
					 ;; "]" character
					 ;; https://lists.exim.org/lurker/message/20130111.082459.a6aa1d5b.fr.html
					 (string-set! res w #\\)
					 (string-set! res (+fx 1 w) #\])
					 (loop (+fx j 4) (+fx w 2) inrange))
					(n
					 (let* ((s (integer->utf8 n))
						(l (string-length s)))
					    (blit-string! s 0 res w l)
					    (loop (+fx j 4) (+fx w l) inrange))
					 )))))
			     ((#\u)
			      (if (>=fx j (-fx len 5))
				  (err "wrong \"\\u\" pattern \"~a\"" str)
				  (let ((n (hex4 str (+fx j 2))))
				     (cond
					((not n)
					 (err "wrong \"\\u\" pattern \"~a\"" str))
					((=fx n 0)
					 (blit-string! "\\000" 0 res w 4)
					 (loop (+fx j 6) (+fx w 4) inrange))
					((=fx n #x5d)
					 ;; "]" character
					 ;; https://lists.exim.org/lurker/message/20130111.082459.a6aa1d5b.fr.html
					 (string-set! res w #\\)
					 (string-set! res (+fx 1 w) #\])
					 (loop (+fx j 6) (+fx w 2) inrange))
					(else
					 (let* ((s (integer->utf8 n))
						(l (string-length s)))
					    (blit-string! s 0 res w l)
					    (loop (+fx j 6) (+fx w l) inrange)))))))
			     ((#\t)
			      (string-set! res w #\Tab)
			      (loop (+fx j 2) (+fx w 1) inrange))
			     ((#\n)
			      (string-set! res w #\Newline)
			      (loop (+fx j 2) (+fx w 1) inrange))
			     ((#\v)
			      (string-set! res w #a011)
			      (loop (+fx j 2) (+fx w 1) inrange))
			     ((#\f)
			      (string-set! res w #a012)
			      (loop (+fx j 2) (+fx w 1) inrange))
			     ((#\r)
			      (string-set! res w #\Return)
			      (loop (+fx j 2) (+fx w 1) inrange))
			     ((#\")
			      (string-set! res w c)
			      (loop (+fx j 2) (+fx w 1) inrange))
			     (else
			      (string-set! res w #\\)
			      (string-set! res (+fx w 1) c)
			      (loop (+fx j 2) (+fx w 2) inrange))))))))))))

;*---------------------------------------------------------------------*/
;*    js-regexp-construct ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.4.1    */
;*---------------------------------------------------------------------*/
(define (js-regexp-construct %this::JsGlobalObject)
   (lambda (_ . l)
      (let ((pattern (if (null? l) (js-undefined) (car l)))
	    (flags (if (or (null? l) (null? (cdr l))) (js-undefined) (cadr l)))
	    (i #f)
	    (m #f)
	    (g #f))
	 (cond
	    ((isa? pattern JsRegExp)
	     (if (eq? flags (js-undefined))
		 (begin
		    (when (js-totest (js-get pattern 'global %this))
		       (set! g -1))
		    (when (js-totest (js-get pattern 'ignoreCase %this))
		       (set! i -1))
		    (when (js-totest (js-get pattern 'multiline %this))
		       (set! m -1)))
		 (js-raise-type-error %this "wrong regexp flags ~s" flags))
	     (set! pattern (js-tostring (js-get pattern 'source %this) %this)))
	    (else
	     (if (eq? pattern (js-undefined))
		 (set! pattern "")
		 (set! pattern (js-tostring pattern %this)))
	     (unless (eq? flags (js-undefined))
		(let ((f (js-tostring flags %this)))
		   (set! i (string-index f #\i))
		   (set! m (string-index f #\m))
		   (set! g (string-index f #\g))
		   (when (or (string-skip f "igm")
			     (and (integer? i) (string-index f #\i (+fx 1 i)))
			     (and (integer? m) (string-index f #\m (+fx 1 m)))
			     (and (integer? g) (string-index f #\g (+fx 1 g))))
		      (js-raise-syntax-error %this "Illegal flags \"~a\"" f))))))
	 (let ((lastindex (instantiate::JsValueDescriptor
			     (name 'lastIndex)
			     (writable #t)
			     (value 0)))
	       (global (instantiate::JsValueDescriptor
			  (name 'global)
			  (value (fixnum? g))))
	       (icase (instantiate::JsValueDescriptor
			 (name 'ignoreCase)
			 (value (fixnum? i))))
	       (mline (instantiate::JsValueDescriptor
			 (name 'multiline)
			 (value (fixnum? m))))
	       (source (instantiate::JsValueDescriptor
			  (name 'source)
			  (value (js-string->jsstring pattern)))))
	    (with-handler
	       (lambda (e)
		  (if (isa? e &io-parse-error)
		      (with-access::&io-parse-error e (msg)
			 (js-raise-syntax-error %this
			    (format "~a \"~a\"" msg pattern) ""))
		      (raise e)))
	       (with-access::JsGlobalObject %this (js-regexp js-regexp-prototype)
		  (instantiate::JsRegExp
		     (__proto__ js-regexp-prototype)
		     (rx (pregexp (make-js-regexp-pattern %this pattern)
				     (when (fixnum? i) 'CASELESS)
				     'JAVASCRIPT_COMPAT
				     'UTF8
				     (when (fixnum? m) 'MULTILINE)))
		     (lastindex lastindex)
		     (properties (list lastindex global icase mline source)))))))))
       
;*---------------------------------------------------------------------*/
;*    init-builtin-regexp-prototype! ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6      */
;*---------------------------------------------------------------------*/
(define (init-builtin-regexp-prototype! %this::JsGlobalObject js-regexp obj)
   ;; prototype fields
   (js-bind! %this obj 'constructor
      :value js-regexp
      :enumerable #f
      :hidden-class #t)
   ;; toString
   (js-bind! %this obj 'toString
      :value (js-make-function %this
		(lambda (this)
		   (js-string->jsstring
		      (string-append "/"
			 (js-tostring (js-get this 'source %this) %this) "/"
			 (if (js-totest (js-get this 'global %this)) "g" "")
			 (if (js-totest (js-get this 'ignoreCase %this)) "i" "")
			 (if (js-totest (js-get this 'multiline %this)) "m" ""))))

		0 'toString)
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t)
   ;; exec
   (js-bind! %this obj 'exec
      :value (js-make-function %this
		(lambda (this string::obj)
		   (regexp-prototype-exec %this this string))
		1 'exec)
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t)
   ;; test
   (js-bind! %this obj 'test
      :value (js-make-function %this (make-regexp-prototype-test %this) 1 'test)
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t)
   ;; source
   (js-bind! %this obj 'source
      :value ""
      :writable #f
      :configurable #f
      :enumerable #f
      :hidden-class #t)
   ;; global
   (js-bind! %this obj 'global
      :value #f
      :writable #f
      :configurable #f
      :enumerable #f
      :hidden-class #t)
   ;; ignoreCase
   (js-bind! %this obj 'ignoreCase
      :value #f
      :writable #f
      :configurable #f
      :enumerable #f
      :hidden-class #t)
   ;; multiline
   (js-bind! %this obj 'multiline
      :value #f
      :writable #f
      :configurable #f
      :enumerable #f
      :hidden-class #t)
   ;; lastindex
   (js-bind! %this obj 'lastIndex
      :value 0
      :writable #t
      :configurable #f
      :enumerable #f
      :hidden-class #t)
   ;; compile
   (js-bind! %this obj 'compile
      :value (js-make-function %this regexp-prototype-compile 1 'compile)
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    regexp-prototype-exec ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6.2    */
;*---------------------------------------------------------------------*/
(define (regexp-prototype-exec %this::JsGlobalObject this string::obj)
   
   (define (js-substring s start end)
      (if (=fx end (+fx start 1))
	  (js-jsstring-fromcharcode %this (char->integer (string-ref s start)) %this)
	  (js-string->jsstring (substring s start end))))
      
   (if (not (isa? this JsRegExp))
       (js-raise-type-error %this "Not a RegExp ~s" this)
       (with-access::JsRegExp this (rx lastindex)
	  (with-access::JsValueDescriptor lastindex ((lastindex value))
	     (let* ((jss (js-tojsstring string %this))
		    (s (js-jsstring->string jss))
		    (len (string-length s))
		    (i (js-tointeger lastindex %this)) 
		    (global (js-get this 'global %this)))
		(unless global (set! i 0))
		(let loop ()
		   (cond
		      ((or (<fx i 0) (>fx i len))
		       (set! lastindex 0)
		       (js-null))
		      ((pregexp-match-positions rx s i)
		       =>
		       (lambda (r)
			  ;; 10
			  (let ((e (cdar r)))
			     ;; 11
			     (when global (set! lastindex e))
			     (let* ((n (length r))
				    (a (with-access::JsGlobalObject %this (js-array)
					  (js-new %this js-array n)))
				    (matchindex (caar r)))
				;; 15
				(js-define-own-property a 'index
				   (instantiate::JsValueDescriptor
				      (name 'index)
				      (value matchindex)
				      (writable #t)
				      (enumerable #t)
				      (configurable #t))
				   #t %this)
				;; 16
				(js-define-own-property a 'input
				   (instantiate::JsValueDescriptor
				      (name 'input)
				      (value jss)
				      (writable #t)
				      (enumerable #t)
				      (configurable #t))
				   #t %this)
				;; 17
				(js-define-own-property a 'length
				   (instantiate::JsValueDescriptor
				      (name 'length)
				      (value n)
				      (writable #t)
				      (enumerable #f)
				      (configurable #f))
				   #t %this)
				;; 19
				(js-define-own-property a 0
				   (instantiate::JsValueDescriptor
				      (name (js-toname 0 %this))
				      (value (js-substring s (caar r) (cdar r)))
				      (writable #f)
				      (enumerable #t)
				      (configurable #t))
				   #t %this)
				;; 20
				(let loop ((c (cdr r))
					   (i 1))
				   (when (pair? c)
				      (let* ((r (car c))
					     (v (if (pair? r)
						    (js-substring s (car r) (cdr r))
						    (js-undefined))))
					 (js-define-own-property a i
					    (instantiate::JsValueDescriptor
					       (name (js-toname i %this))
					       (value v)
					       (writable #f)
					       (enumerable #t)
					       (configurable #t))
					    #t %this))
				      (loop (cdr c) (+fx i 1))))
				a))))
		      (else
		       (set! lastindex 0)
		       (js-null)))))))))

;*---------------------------------------------------------------------*/
;*    make-regexp-prototype-test ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6.3    */
;*---------------------------------------------------------------------*/
(define (make-regexp-prototype-test %this::JsGlobalObject)
   (lambda (this string::obj)
      (not (eq? (regexp-prototype-exec %this this string) (js-null)))))
   
;*---------------------------------------------------------------------*/
;*    regexp-prototype-compile ...                                     */
;*---------------------------------------------------------------------*/
(define (regexp-prototype-compile this)
   (tprint "CANNOT FIND THE SPEC..."))

;*---------------------------------------------------------------------*/
;*    js-regexp->jsregexp ...                                          */
;*---------------------------------------------------------------------*/
(define (js-regexp->jsregexp val::regexp %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-regexp)
      (js-new1 %this js-regexp (js-string->jsstring (regexp-pattern val)))))

;*---------------------------------------------------------------------*/
;*    JsStringLiteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
