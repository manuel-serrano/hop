;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/regexp.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Thu Apr 25 18:48:11 2019 (serrano)                */
;*    Copyright   :  2013-19 Manuel Serrano                            */
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
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_function
	   __hopscript_array
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_string
	   __hopscript_stringliteral
	   __hopscript_error)

   (export (js-init-regexp! ::JsGlobalObject)
	   js-regexp-cmap
	   (inline js-regexp?::bool ::obj)
	   (js-regexp->jsregexp ::regexp ::JsGlobalObject)
	   (js-regexp-literal-test::bool ::JsRegExp ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    property caches ...                                              */
;*---------------------------------------------------------------------*/
(%define-pcache 1)
(define %pcache (js-make-pcache-table 1 "hopscript/regexp.scm"))

;*---------------------------------------------------------------------*/
;*    js-regexp-cmap ...                                               */
;*---------------------------------------------------------------------*/
(define js-regexp-cmap
   (js-not-a-cmap))

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
(define-method (hop->javascript o::JsRegExp op compile isexpr _)
   (with-access::JsRegExp o (global)
      (let ((%this (js-initial-global-object)))
	 (display "/" op)
	 (display (js-get o 'source %this) op)
	 (display "/" op)
	 (when (js-totest (js-get o 'global %this)) (display "g" op))
	 (when (js-totest (js-get o 'ignoreCase %this)) (display "i" op))
	 (when (js-totest (js-get o 'multiline %this)) (display "m" op)))))

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
	 
	 ;; default regexp cmap
	 (set! js-regexp-cmap
	    (instantiate::JsConstructMap
	       (methods (make-vector 1))
	       (props `#(,(prop 'lastIndex (property-flags #t #f #f #f))))))
	 
	 (set! js-regexp-prototype
	    (instantiateJsRegExp
	       (cmap js-regexp-cmap)
	       (__proto__ __proto__)
	       (elements ($create-vector 10))
	       (rx (pregexp ""))
	       (source "")
	       (flags #u32:0)))
	 
	 ;; create a HopScript regexp object constructor
	 (set! js-regexp
	    (js-make-function %this
	       (%js-regexp %this) 2 "RegExp"
	       :__proto__ js-function-prototype
	       :prototype js-regexp-prototype
	       :alloc js-no-alloc
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
   (lambda (this pattern flags loc)
      (with-access::JsGlobalObject %this (js-regexp)
	 (if (and (isa? pattern JsRegExp) (eq? flags (js-undefined)))
	     pattern
	     (js-new3 %this js-regexp pattern flags loc)))))

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
	  (res (make-string (*fx 4 len))))
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
						 (starting-range? str (+fx j 6)))
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
      ((and (>=fx n #xD800) (<=fx n #xdfff))
       ;; MS 9feb2016: don't know what to do as PCRE cannot handle
       ;; "red" cells https://en.wikipedia.org/wiki/UTF-8
       (ucs2-string->utf8-string
	  (make-ucs2-string 1 (integer->ucs2 #xe000))))
      (else
       (let ((u (make-ucs2-string 1 (integer->ucs2 n))))
	  (ucs2-string->utf8-string u)))))

;*---------------------------------------------------------------------*/
;*    js-regexp-construct ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.4.1    */
;*---------------------------------------------------------------------*/
(define (js-regexp-construct %this::JsGlobalObject)
   (lambda (_ pattern uflags loc)
      (let ((flags #u32:0))
	 (cond
	    ((isa? pattern JsRegExp)
	     (with-access::JsRegExp pattern ((rxflags flags) source)
		(if (eq? uflags (js-undefined))
		    (set! flags rxflags)
		    (js-raise-type-error %this "wrong regexp flags ~s" uflags))
		(set! pattern source)))
	    (else
	     (if (eq? pattern (js-undefined))
		 (set! pattern "")
		 (set! pattern (js-tostring pattern %this)))
	     (unless (eq? uflags (js-undefined))
		(let* ((f (js-tostring uflags %this))
		       (i (string-index f #\i))
		       (m (string-index f #\m))
		       (g (string-index f #\g)))
		   (set! flags
		      (bit-oru32
			 (if (integer? i)
			     (JS-REGEXP-FLAG-IGNORECASE)
			     #u32:0)
			 (bit-oru32
			    (if (integer? m)
				(JS-REGEXP-FLAG-MULTILINE)
				#u32:0)
			    (if (integer? g)
				(JS-REGEXP-FLAG-GLOBAL)
				#u32:0))))
		   (when (or (string-skip f "igm")
			     (and (integer? i)
				  (string-index f #\i (+fx 1 i)))
			     (and (integer? m)
				  (string-index f #\m (+fx 1 m)))
			     (and (integer? g)
				  (string-index f #\g (+fx 1 g))))
		      (js-raise-syntax-error %this "Illegal flags \"~a\"" f))))))
	 (with-handler
	    (lambda (e)
	       (if (isa? e &io-parse-error)
		   (with-access::&io-parse-error e (msg)
		      (js-raise-syntax-error/loc %this loc
			 (format "~a \"~a\"" msg pattern) ""))
		   (raise e)))
	    (multiple-value-bind (pat enc)
	       (make-js-regexp-pattern %this pattern)
	       (let ((rx (pregexp pat
			    (when (js-regexp-flags-ignorecase? flags) 'CASELESS)
			    'JAVASCRIPT_COMPAT
			    (if (eq? enc 'ascii) 'JAVASCRIPT_COMPAT 'UTF8)
			    (when (js-regexp-flags-multiline? flags) 'MULTILINE))))
		  (with-access::JsGlobalObject %this (js-regexp js-regexp-prototype)
		     (instantiateJsRegExp
			(cmap js-regexp-cmap)
			(__proto__ js-regexp-prototype)
			(elements (vector 0))
			(rx rx)
			(source pattern)
			(flags flags)))))))))

;*---------------------------------------------------------------------*/
;*    init-builtin-regexp-prototype! ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6      */
;*---------------------------------------------------------------------*/
(define (init-builtin-regexp-prototype! %this::JsGlobalObject js-regexp obj)
   ;; constructor
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
		
		0 "toString"
		:prototype (js-undefined))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t)
   ;; ignoreCase
   (js-bind! %this obj 'ignoreCase
      :get (js-make-function %this
	      (lambda (this)
		 (if (isa? this JsRegExp)
		     (with-access::JsRegExp this (flags)
			(js-regexp-flags-ignorecase? flags))
		     (js-raise-type-error %this "Not a regexp" this)))
	      0 "ignoreCase"
	      :prototype (js-undefined))
      :set (js-undefined)
      :writable #f
      :enumerable #f
      :configurable #t)
   ;; multiline
   (js-bind! %this obj 'multiline
      :get (js-make-function %this
	      (lambda (this)
		 (if (isa? this JsRegExp)
		     (with-access::JsRegExp this (flags)
			(js-regexp-flags-multiline? flags))
		     (js-raise-type-error %this "Not a regexp" this)))
	      0 "multiline"
	      :prototype (js-undefined))
      :set (js-undefined)
      :writable #f
      :enumerable #f
      :configurable #t)
   ;; global
   (js-bind! %this obj 'global
      :get (js-make-function %this
	      (lambda (this)
		 (if (isa? this JsRegExp)
		     (with-access::JsRegExp this (flags)
			(js-regexp-flags-global? flags))
		     (js-raise-type-error %this "Not a regexp" this)))
	      0 "global"
	      :prototype (js-undefined))
      :set (js-undefined)
      :writable #f
      :enumerable #f
      :configurable #t)
   ;; source
   (js-bind! %this obj 'source
      :get (js-make-function %this
	      (lambda (this)
		 (if (isa? this JsRegExp)
		     (with-access::JsRegExp this (source)
			(js-string->jsstring source))
		     (js-raise-type-error %this "Not a regexp" this)))
	      0 "source"
	      :prototype (js-undefined))
      :set (js-undefined)
      :writable #f
      :enumerable #f
      :configurable #t)
   ;; exec
   (js-bind! %this obj 'exec
      :value (js-make-function %this
		(lambda (this string::obj)
		   (regexp-prototype-exec %this this string))
		1 "exec"
		:prototype (js-undefined))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t)
   ;; test
   (js-bind! %this obj 'test
      :value (js-make-function %this (make-regexp-prototype-test %this) 1 "test"
		:prototype (js-undefined))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t)
   ;; compile
   (js-bind! %this obj 'compile
      :value (js-make-function %this regexp-prototype-compile 1 "compile"
		:prototype (js-undefined))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    js-regexp-exec-cmap ...                                          */
;*---------------------------------------------------------------------*/
(define js-regexp-exec-cmap
   (instantiate::JsConstructMap
      (methods '#())
      (props `#(,(prop 'index (property-flags #t #t #t #f))
		,(prop 'input (property-flags #t #t #t #f))))))

;*---------------------------------------------------------------------*/
;*    regexp-prototype-exec ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6.2    */
;*---------------------------------------------------------------------*/
(define (regexp-prototype-exec %this::JsGlobalObject this string::obj)
   
   (define (js-substring s start end utf8)
      (cond
	 ((=fx end (+fx start 1))
	  (js-jsstring-fromcharcode %this
	     (char->integer (string-ref s start)) %this))
	 (utf8
	  (js-utf8->jsstring (substring s start end)))
	 (else
	  (js-ascii->jsstring (substring s start end)))))
   
   (if (not (isa? this JsRegExp))
       (js-raise-type-error %this "Not a RegExp ~s" this)
       (with-access::JsRegExp this (rx flags)
	  (let ((lastindex (js-object-get-name/cache this 'lastIndex
			      #f %this (js-pcache-ref %pcache 0)))
		(global (js-regexp-flags-global? flags)))
	     (let* ((jss (js-tojsstring string %this))
		    (s (js-jsstring->string jss))
		    (len (string-length s))
		    (i (js-tointeger lastindex %this))
		    (enc (isa? s JsStringLiteralUTF8)))
		(unless global (set! i 0))
		(cond
		   ((or (<fx i 0) (>fx i len))
		    (set! lastindex 0)
		    (js-object-put-name/cache! this 'lastIndex lastindex
		       #f %this (js-pcache-ref %pcache 0))
		    (js-null))
		   ((pregexp-match-positions rx s i)
		    =>
		    (lambda (r)
		       ;; 10
		       (let ((e (cdar r)))
			  ;; 11
			  (when global
			     (set! lastindex e)
			     (js-object-put-name/cache! this 'lastIndex lastindex
				#f %this (js-pcache-ref %pcache 0)))
			  (let* ((n (length r))
				 (vec ($create-vector n))
				 (a (js-vector->jsarray vec %this))
				 (matchindex (caar r))
				 (els ($create-vector 2)))
			     ;; bind the result cmap and add the elements
			     (with-access::JsArray a (elements cmap)
				(set! cmap js-regexp-exec-cmap)
				(set! elements els))
			     ;; 15
			     (vector-set! els 0 matchindex)
			     ;; (js-bind! %this a 'index :value matchindex)
			     ;; 16
			     (vector-set! els 1 jss)
			     ;; (js-bind! %this a 'input :value jss)
			     ;; 17
			     ;; no need as already automatically set
			     ;; 19
			     (vector-set! vec 0
				(js-substring s (caar r) (cdar r) enc))
			     ;; 20
			     (let loop ((c (cdr r))
					(i 1))
				(when (pair? c)
				   (let ((r (car c)))
				      (vector-set! vec i
					 (if (pair? r)
					     (js-substring s (car r) (cdr r) enc)
					     (js-undefined))))
				   (loop (cdr c) (+fx i 1))))
			     a))))
		   (else
		    (set! lastindex 0)
		    (js-object-put-name/cache! this 'lastIndex lastindex
		       #f %this (js-pcache-ref %pcache 0))
		    (js-null))))))))

;*---------------------------------------------------------------------*/
;*    make-regexp-prototype-test ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6.3    */
;*---------------------------------------------------------------------*/
(define (make-regexp-prototype-test %this::JsGlobalObject)
   (lambda (this string::obj)
      (not (eq? (regexp-prototype-exec %this this string) (js-null)))))

;*---------------------------------------------------------------------*/
;*    literal-test-pos ...                                             */
;*---------------------------------------------------------------------*/
(define literal-test-pos (vector -1 -1))

;*---------------------------------------------------------------------*/
;*    js-regexp-literal-test-string ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6.3    */
;*    -------------------------------------------------------------    */
;*    This function is used when the REGEXP is given as a literal.     */
;*    In that particular, there is no need to store all the            */
;*    EXEC variables.                                                  */
;*---------------------------------------------------------------------*/
(define (js-regexp-literal-test-string::bool this::JsRegExp str::obj %this)
   (with-access::JsRegExp this (rx flags)
      (let* ((lastindex (js-object-get-name/cache this 'lastIndex
			   #f %this (js-pcache-ref %pcache 0)))
	     (global (js-regexp-flags-global? flags))
	     (s (js-jsstring->string str))
	     (i (cond
		   ((not global)
		    0)
		   ((or (<fx lastindex 0) (>fx lastindex (string-length s)))
		    0)
		   (else
		    lastindex))))
	 (=fx (pregexp-match-n-positions! rx
		 s literal-test-pos i (string-length s))
	    1))))

;*---------------------------------------------------------------------*/
;*    js-regexp-literal-test ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6.3    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6.3    */
;*    This function is used when the REGEXP is given as a literal.     */
;*    In that particular, there is no need to store all the            */
;*    EXEC variables.                                                  */
;*---------------------------------------------------------------------*/
(define (js-regexp-literal-test::bool this::JsRegExp string::obj %this::JsGlobalObject)
   (let ((jss (js-tojsstring string %this)))
      (js-regexp-literal-test-string this jss %this)))

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
