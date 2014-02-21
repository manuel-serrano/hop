;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/hopscript/regexp.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Tue Feb 11 17:51:03 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript regexps                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_regexp

   (library hop)
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_array
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_error)

   (export js-regexp
	   js-regexp-prototype
	   (js-init-regexp! ::JsObject)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsRegexp ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsRegExp op compile isexpr)
   (display "/" op)
   (display (js-tostring (js-get o 'source)) op)p
   (display "/" op)
   (when (js-totest (js-get o 'global)) (display "g" op))
   (when (js-totest (js-get o 'multiline)) (display "m" op))
   (when (js-totest (js-get o 'ignoreCase)) (display "i" op)))

;*---------------------------------------------------------------------*/
;*    js-regexp ...                                                    */
;*---------------------------------------------------------------------*/
(define js-regexp #f)
(define js-regexp-prototype #f)

;*---------------------------------------------------------------------*/
;*    js-init-regexp! ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10        */
;*---------------------------------------------------------------------*/
(define (js-init-regexp! %this)
   ;; first, create the builtin prototype
   (set! js-regexp-prototype
      (instantiate::JsRegExp
	 (rx (pregexp ""))
	 (__proto__ js-object-prototype)
	 (extensible #t)))
   ; then, Create a HopScript regexp object constructor
   (let ((obj (js-make-function %js-regexp 2 "JsRegExp"
		 :__proto__ js-function-prototype
		 :prototype js-regexp-prototype
		 :construct js-regexp-construct)))
      (set! js-regexp obj)
      (init-builtin-regexp-prototype! js-regexp-prototype)
      ;; bind Regexp in the global object
      (js-bind! %this 'RegExp :configurable #f :enumerable #f :value js-regexp)
      js-regexp))
   
;*---------------------------------------------------------------------*/
;*    %js-regexp ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.3.1    */
;*---------------------------------------------------------------------*/
(define (%js-regexp this pattern flags)
   (if (and (isa? pattern JsRegExp) (eq? flags (js-undefined)))
       pattern
       (js-new js-regexp pattern flags)))

;*---------------------------------------------------------------------*/
;*    make-js-regexp-pattern ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-7.8.5        */
;*---------------------------------------------------------------------*/
(define (make-js-regexp-pattern str)
   
   (define (err fmt str)
      (js-raise
	 (js-new js-syntax-error (format fmt str))))
   
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
      (let ((u (make-ucs2-string 1 (integer->ucs2 n))))
	 (ucs2-string->utf8-string u)))
   
   (let* ((len (string-length str))
	  (res (make-string len)))
      (let loop ((i 0)
		 (w 0))
	 (let ((j (string-index str #\\ i)))
	    (cond
	       ((not j)
		(blit-string! str i res w (-fx len i))
		(string-shrink! res (+fx w (-fx len i)))
		res)
	       ((=fx j (-fx len 1))
		(err "wrong pattern \"~a\"" str))
	       (else
		(when (>fx j i)
		   (blit-string! str i res w (-fx j i))
		   (set! w (+fx w (-fx j i))))
		(let ((c (string-ref str (+fx j 1))))
		   (case c
		      ((#\x)
		       (if (>=fx j (-fx len 3))
			   (err "wrong \"\\x\" pattern \"~a\"" str)
			   (let ((n (hex2 str (+fx j 2))))
			      (if n
				  (let* ((s (integer->utf8 n))
					 (l (string-length s)))
				     (blit-string! s 0 res w l)
				     (loop (+fx j 4) (+fx w l)))
				  (err "wrong \"\\x\" pattern \"~a\"" str)))))
		      ((#\u)
		       (if (>=fx j (-fx len 5))
			   (err "wrong \"\\u\" pattern \"~a\"" str)
			   (let ((n (hex4 str (+fx j 2))))
			      (if n
				  (let* ((s (integer->utf8 n))
					 (l (string-length s)))
				     (blit-string! s 0 res w l)
				     (loop (+fx j 6) (+fx w l)))
				  (err "wrong \"\\u\" pattern \"~a\"" str)))))
		      (else
		       (string-set! res w #\\)
		       (loop (+fx j 1) (+fx w 1)))))))))))

;*---------------------------------------------------------------------*/
;*    js-regexp-construct ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.4.1    */
;*---------------------------------------------------------------------*/
(define (js-regexp-construct _ . l)
   (let ((pattern (if (null? l) "" (car l)))
	 (flags (if (or (null? l) (null? (cdr l))) (js-undefined) (cadr l)))
	 (i #f)
	 (m #f)
	 (g #f))
      (cond
	 ((isa? pattern JsRegExp)
	  (if (eq? flags (js-undefined))
	      (begin
		 (when (js-totest (js-get pattern 'global)) (set! g -1))
		 (when (js-totest (js-get pattern 'ignoreCase)) (set! i -1))
		 (when (js-totest (js-get pattern 'multiline)) (set! m -1)))
	      (js-raise-type-error "wrong regexp flags ~s" flags))
	  (set! pattern (js-tostring (js-get pattern 'source))))
	 (else
	  (if (eq? pattern (js-undefined))
	     (set! pattern "")
	     (set! pattern (js-tostring pattern)))
	  (unless (eq? flags (js-undefined))
	     (let ((f (js-tostring flags)))
		(set! i (string-index f #\i))
		(set! m (string-index f #\m))
		(set! g (string-index f #\g))
		(when (or (string-skip f "igm")
			  (and (integer? i) (string-index f #\i (+fx 1 i)))
			  (and (integer? m) (string-index f #\m (+fx 1 m)))
			  (and (integer? g) (string-index f #\g (+fx 1 g))))
		   (js-raise
		      (js-new js-syntax-error "Illegal flags")))))))
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
		       (value pattern))))
	 (with-handler
	    (lambda (e)
	       (if (isa? e &io-parse-error)
		   (with-access::&io-parse-error e (msg)
		      (js-raise (js-new js-syntax-error msg)))
		   (raise e)))
	    (instantiate::JsRegExp
	       (extensible #t)
	       (__proto__ js-regexp-prototype)
	       (rx (pregexp (make-js-regexp-pattern pattern)
		      (when (fixnum? i) 'CASELESS)
		      'JAVASCRIPT_COMPAT
		      'UTF8
		      (when (fixnum? m) 'MULTILINE)))
	       (properties (list lastindex global icase mline source)))))))
       
;*---------------------------------------------------------------------*/
;*    init-builtin-regexp-prototype! ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6      */
;*---------------------------------------------------------------------*/
(define (init-builtin-regexp-prototype! obj)
   ;; prototype fields
   (js-bind! obj 'constructor
      :value js-regexp
      :enumerable #f)
   ;; toString
   (js-bind! obj 'toString
      :value (js-make-function
		(lambda (this)
		   (string-append "/" (js-tostring (js-get this 'source)) "/"))
		0 "toString")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; exec
   (js-bind! obj 'exec
      :value (js-make-function regexp-prototype-exec 1 "exec")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; test
   (js-bind! obj 'test
      :value (js-make-function regexp-prototype-test 1 "test")
      :writable #t
      :configurable #t
      :enumerable #f)
   ;; source
   (js-bind! obj 'source
      :value ""
      :writable #f
      :configurable #f
      :enumerable #f)
   ;; global
   (js-bind! obj 'global
      :value #f
      :writable #f
      :configurable #f
      :enumerable #f)
   ;; ignoreCase
   (js-bind! obj 'ignoreCase
      :value #f
      :writable #f
      :configurable #f
      :enumerable #f)
   ;; multiline
   (js-bind! obj 'multiline
      :value #f
      :writable #f
      :configurable #f
      :enumerable #f)
   ;; lastindex
   (js-bind! obj 'lastIndex
      :value 0
      :writable #t
      :configurable #f
      :enumerable #f)
   ;; compile
   (js-bind! obj 'compile
      :value (js-make-function regexp-prototype-compile 1 "compile")
      :writable #t
      :configurable #t
      :enumerable #f))

;*---------------------------------------------------------------------*/
;*    regexp-prototype-exec ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6.2    */
;*---------------------------------------------------------------------*/
(define (regexp-prototype-exec this string::obj)
   (if (not (isa? this JsRegExp))
       (js-raise-type-error "Not a RegExp ~s" this)
       (with-access::JsRegExp this (rx)
	  (let* ((s (js-tostring string))
		 (len (string-length s))
		 (lastindex (js-get this 'lastIndex))
		 (i (js-tointeger lastindex)) 
		 (global (js-get this 'global)))
	     (unless global (set! i 0))
	     (let loop ()
		(cond
		   ((or (<fx i 0) (>fx i len))
		    (js-put! this 'lastIndex 0 #f)
		    (js-null))
		   ((pregexp-match-positions rx s i)
		    =>
		    (lambda (r)
		       ;; 10
		       (let ((e (cdar r)))
			  ;; 11
			  (when global (js-put! this 'lastIndex e #f))
			  (let* ((n (length r))
				 (a (js-new js-array n))
				 (matchindex (caar r)))
			     ;; 15
			     (js-define-own-property a 'index
				(instantiate::JsValueDescriptor
				   (name 'index)
				   (value matchindex)
				   (writable #t)
				   (enumerable #t)
				   (configurable #t))
				#t)
			     ;; 16
			     (js-define-own-property a 'input
				(instantiate::JsValueDescriptor
				   (name 'input)
				   (value s)
				   (writable #t)
				   (enumerable #t)
				   (configurable #t))
				#t)
			     ;; 17
			     (js-define-own-property a 'length
				(instantiate::JsValueDescriptor
				   (name 'length)
				   (value n)
				   (writable #t)
				   (enumerable #f)
				   (configurable #f))
				#t)
			     ;; 19
			     (js-define-own-property a 0
				(instantiate::JsValueDescriptor
				   (name (js-toname 0))
				   (value (substring s (caar r) (cdar r)))
				   (writable #f)
				   (enumerable #t)
				   (configurable #t))
				#t)
			     ;; 20
			     (let loop ((c (cdr r))
					(i 1))
				(when (pair? c)
				   (let* ((r (car c))
					  (v (if (pair? r)
						 (substring s (car r) (cdr r))
						 (js-undefined))))
				      (js-define-own-property a i
					 (instantiate::JsValueDescriptor
					    (name (js-toname i))
					    (value v)
					    (writable #f)
					    (enumerable #t)
					    (configurable #t))
					 #t))
				   (loop (cdr c) (+fx i 1))))
			     a))))
		   (else
		    (set! i (+fx i 1))
		    (loop))))))))

;*---------------------------------------------------------------------*/
;*    regexp-prototype-test ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6.3    */
;*---------------------------------------------------------------------*/
(define (regexp-prototype-test this string::obj)
   (not (eq? (regexp-prototype-exec this string) (js-null))))
   
;*---------------------------------------------------------------------*/
;*    regexp-prototype-compile ...                                     */
;*---------------------------------------------------------------------*/
(define (regexp-prototype-compile this)
   (tprint "CANNOT FIND THE SPEC..."))

	 

