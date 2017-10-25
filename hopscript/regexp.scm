;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/regexp.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Wed Oct 25 14:52:11 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript regexps                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_regexp

   (library hop)
   
   (include "types.sch" "stringliteral.sch" "property.sch")
   
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
   (with-access::JsRegExp o (global)
      (with-access::JsValueDescriptor global ((global value))
	 (let ((%this (js-initial-global-object)))
	    (display "/" op)
	    (display (js-tostring (js-get o 'source %this) %this) op)
	    (display "/" op)
	    (when (js-totest global) (display "g" op))
	    (when (js-totest (js-get o 'ignoreCase %this)) (display "i" op))
	    (when (js-totest (js-get o 'multiline %this)) (display "m" op))))))

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
	 (let* ((global (instantiate::JsValueDescriptor
			   (name 'global)
			   (value #f)))
		(lastindex (instantiate::JsValueDescriptor
			      (name 'lastIndex)
			      (writable #t)
			      (value 0)))
		(proto (instantiate-JsRegExp
			  (lastindex lastindex)
			  (global global)
			  (rx (pregexp ""))
			  (__proto__ __proto__))))
	    (js-object-properties-set! proto (list lastindex global))
	    (set! js-regexp-prototype proto))
	 
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
   
   (let* ((len (string-length str))
	  (res (make-string (*fx 3 len))))
      (let loop ((i 0)
		 (w 0)
		 (ascii #t))
	 (let ((j (string-index str "\\[]" i)))
	    (if (not j)
		(begin
		   (blit-string! str i res w (-fx len i))
		   (string-shrink! res (+fx w (-fx len i)))
		   (values res (if ascii 'ascii 'utf8)))
		(let ((tag (string-ref str j)))
		   (when (>fx j i)
		      (blit-string! str i res w (-fx j i))
		      (set! w (+fx w (-fx j i))))
		   (cond
		      ((char=? tag #\[)
		       (multiple-value-bind (pat rascii j)
			  (make-js-regexp-range %this str j)
			  (blit-string! pat 0 res w (string-length pat))
			  (loop (+fx j 1) (+fx w (string-length pat))
			     (and ascii rascii))))
		      ((char=? tag #\])
		       (string-set! res w #\\)
		       (string-set! res (+fx w 1) tag)
		       (loop (+fx j 1) (+fx w 2) ascii))
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
					 (loop (+fx j 4) (+fx w 4) ascii))
					((=fx n #x5d)
					 ;; "]" character
					 ;; https://lists.exim.org/lurker/message/20130111.082459.a6aa1d5b.fr.html
					 (string-set! res w #\\)
					 (string-set! res (+fx 1 w) #\])
					 (loop (+fx j 4) (+fx w 2) ascii))
					(else
					 (let* ((s (integer->utf8 n))
						(l (string-length s)))
					    (blit-string! s 0 res w l)
					    (loop (+fx j 4) (+fx w l)
					       (and ascii (<=fx n 127)))))))))
			     ((#\u)
			      (if (>=fx j (-fx len 5))
				  (err "wrong \"\\u\" pattern \"~a\"" str)
				  (let ((n (hex4 str (+fx j 2))))
				     (cond
					((not n)
					 (err "wrong \"\\u\" pattern \"~a\"" str))
					((=fx n 0)
					 (blit-string! "\\000" 0 res w 4)
					 (loop (+fx j 6) (+fx w 4) ascii))
					((=fx n #x5d)
					 ;; "]" character
					 ;; https://lists.exim.org/lurker/message/20130111.082459.a6aa1d5b.fr.html
					 (string-set! res w #\\)
					 (string-set! res (+fx 1 w) #\])
					 (loop (+fx j 6) (+fx w 2) ascii))
					(else
					 (let* ((s (integer->utf8 n))
						(l (string-length s)))
					    (blit-string! s 0 res w l)
					    (loop (+fx j 6) (+fx w l)
					       (and ascii (<fx n 127)))))))))
			     ((#\t)
			      (string-set! res w #\Tab)
			      (loop (+fx j 2) (+fx w 1) ascii))
			     ((#\n)
			      (string-set! res w #\Newline)
			      (loop (+fx j 2) (+fx w 1) ascii))
			     ((#\v)
			      (string-set! res w #a011)
			      (loop (+fx j 2) (+fx w 1) ascii))
			     ((#\f)
			      (string-set! res w #a012)
			      (loop (+fx j 2) (+fx w 1) ascii))
			     ((#\r)
			      (string-set! res w #\Return)
			      (loop (+fx j 2) (+fx w 1) ascii))
			     ((#\")
			      (string-set! res w c)
			      (loop (+fx j 2) (+fx w 1) ascii))
			     (else
			      (string-set! res w #\\)
			      (string-set! res (+fx w 1) c)
			      (loop (+fx j 2) (+fx w 2) #f))))))))))))

;*---------------------------------------------------------------------*/
;*    make-js-regexp-range ...                                         */
;*---------------------------------------------------------------------*/
(define (make-js-regexp-range %this str::bstring i0)
   
   (define (ending-range? str w)
      (and (>fx w 0) (char=? (string-ref str (-fx w 1)) #\-)))
   
   (define (starting-range? str i)
      (and (<fx i (string-length str)) (char=? (string-ref str i) #\-)))
   
   (define (err fmt str)
      (js-raise-syntax-error %this fmt str))
   
   ;; (string-ref str j) == #\[
   (let* ((len (string-length str))
	  (res (make-string (*fx 3 len))))
      (let loop ((i (+fx i0 1))
		 (w 0)
		 (chars '())
		 (ascii #t))
	 (let ((j (string-index str "\\]" i)))
	    (if (not j)
		(err "wrong pattern \"~a\"" (substring str j))
		(let ((tag (string-ref str j)))
		   (when (>fx j i)
		      (blit-string! str i res w (-fx j i))
		      (set! w (+fx w (-fx j i))))
		   (cond
		      ((char=? tag #\])
		       (let ((r (string-shrink! res w)))
			  (if (null? chars)
			      (values
				 (string-append "[" r "]") ascii j)
			      (values
				 (format "(?:[~a]|~(|))" r chars) ascii j))))
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
					 (loop (+fx j 4) (+fx w 4) chars ascii))
					((=fx n #x5d)
					 (string-set! res w #\\)
					 (string-set! res (+fx 1 w) #\])
					 (loop (+fx j 4) (+fx w 2) chars ascii))
					((and (>=fx n 128) ascii)
					 (if (or (ending-range? res w)
						 (starting-range? str (+fx j 3)))
					     (let* ((s (integer->utf8 n))
						    (l (string-length s)))
						(blit-string! s 0 res w l)
						(loop (+fx j 4) (+fx w l) chars #f))
					     (let ((s (integer->utf8 n)))
						(loop (+fx j 4) w
						   (cons s chars) ascii))))
					(else
					 (let* ((s (integer->utf8 n))
						(l (string-length s)))
					    (blit-string! s 0 res w l)
					    (loop (+fx j 4) (+fx w l)
					       chars ascii)))))))
			     ((#\u)
			      (if (>=fx j (-fx len 5))
				  (err "wrong \"\\u\" pattern \"~a\"" str)
				  (let ((n (hex4 str (+fx j 2))))
				     (cond
					((not n)
					 (err "wrong \"\\u\" pattern \"~a\"" str))
					((=fx n 0)
					 (blit-string! "\\000" 0 res w 4)
					 (loop (+fx j 6) (+fx w 4) chars ascii))
					((=fx n #x5d)
					 (string-set! res w #\\)
					 (string-set! res (+fx 1 w) #\])
					 (loop (+fx j 6) (+fx w 2) chars ascii))
					((and (>=fx n 128) ascii)
					 (if (or (ending-range? res w)
						 (starting-range? str (+fx j 5)))
					     (let* ((s (integer->utf8 n))
						    (l (string-length s)))
						(blit-string! s 0 res w l)
						(loop (+fx j 6) (+fx w l) chars #f))
					     (let ((s (integer->utf8 n)))
						(loop (+fx j 6) w
						   (cons s chars) ascii))))
					(else
					 (let* ((s (integer->utf8 n))
						(l (string-length s)))
					    (blit-string! s 0 res w l)
					    (loop (+fx j 6) (+fx w l)
					       chars (and ascii (<fx n 127)))))))))
			     ((#\t)
			      (string-set! res w #\Tab)
			      (loop (+fx j 2) (+fx w 1) chars ascii))
			     ((#\n)
			      (string-set! res w #\Newline)
			      (loop (+fx j 2) (+fx w 1) chars ascii))
			     ((#\v)
			      (string-set! res w #a011)
			      (loop (+fx j 2) (+fx w 1) chars ascii))
			     ((#\f)
			      (string-set! res w #a012)
			      (loop (+fx j 2) (+fx w 1) chars ascii))
			     ((#\r)
			      (string-set! res w #\Return)
			      (loop (+fx j 2) (+fx w 1) chars ascii))
			     ((#\")
			      (string-set! res w c)
			      (loop (+fx j 2) (+fx w 1) chars ascii))
			     (else
			      (string-set! res w #\\)
			      (string-set! res (+fx w 1) c)
			      (loop (+fx j 2) (+fx w 2) chars ascii))))))))))))

;*---------------------------------------------------------------------*/
;*    hex2 ...                                                         */
;*---------------------------------------------------------------------*/
(define (hex2 str j)
   (let ((n1 (char-alpha (string-ref str j))))
      (when n1
	 (let ((n2 (char-alpha (string-ref str (+fx j 1)))))
	    (when n2
	       (+fx (*fx n1 16) n2))))))

;*---------------------------------------------------------------------*/
;*    hex4 ...                                                         */
;*---------------------------------------------------------------------*/
(define (hex4 str j)
   (let ((n1 (hex2 str j)))
      (when n1
	 (let ((n2 (hex2 str (+fx j 2))))
	    (when n2
	       (+fx (*fx n1 256) n2))))))

;*---------------------------------------------------------------------*/
;*    char-alpha ...                                                   */
;*---------------------------------------------------------------------*/
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

;*---------------------------------------------------------------------*/
;*    integer->utf8 ...                                                */
;*---------------------------------------------------------------------*/
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
	     (with-access::JsRegExp pattern (global)
		(with-access::JsValueDescriptor global ((global value))
		   (if (eq? flags (js-undefined))
		       (begin
			  (when (js-totest global)
			     (set! g -1))
			  (when (js-totest (js-get pattern 'ignoreCase %this))
			     (set! i -1))
			  (when (js-totest (js-get pattern 'multiline %this))
			     (set! m -1)))
		       (js-raise-type-error %this "wrong regexp flags ~s" flags))))
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
		  (multiple-value-bind (pat enc)
		     (make-js-regexp-pattern %this pattern)
		     (let ((nobj (instantiate-JsRegExp
				    (__proto__ js-regexp-prototype)
				    (rx (pregexp pat
					   (when (fixnum? i) 'CASELESS)
					   'JAVASCRIPT_COMPAT
					   (if (eq? enc 'ascii)
					       'JAVASCRIPT_COMPAT
					       'UTF8)
					   (when (fixnum? m) 'MULTILINE)))
				    (lastindex lastindex)
				    (global global))))
			(js-object-properties-set! nobj
			   (list lastindex global icase mline source))
			nobj))))))))
       
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
       (with-access::JsRegExp this (rx lastindex global properties)
	  (with-access::JsValueDescriptor lastindex ((lastindex value))
	     (with-access::JsValueDescriptor global ((global value))
		(let* ((jss (js-tojsstring string %this))
		       (s (js-jsstring->string jss))
		       (len (string-length s))
		       (i (js-tointeger lastindex %this)) )
		   (unless global (set! i 0))
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
