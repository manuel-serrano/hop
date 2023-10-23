;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/regexp.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:41:39 2013                          */
;*    Last change :  Sun Oct 22 18:48:10 2023 (serrano)                */
;*    Copyright   :  2013-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript regexps                      */
;*    -------------------------------------------------------------    */
;*    Warning: the flags d, s, v, and y are not supported.             */
;*    See: doc/lang/01-es.md                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_regexp

   (library hop)
   
   (include "types.sch" "stringliteral.sch" "property.sch" "array.sch")
   
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
	   __hopscript_error
	   __hopscript_proxy
	   __hopscript_profile)

   (export (js-init-regexp! ::JsGlobalObject)
	   (js-init-regexp-caches! ::JsGlobalObject ::long)
	   (js-new-regexp1 ::JsGlobalObject ::obj)
	   (js-new-regexp1/cache ::JsGlobalObject ::obj ::struct)
	   (inline js-regexp?::bool ::obj)
	   (js-regexp->jsregexp ::regexp ::JsGlobalObject)
	   (inline js-regexp-test this::JsRegExp string %this)
	   (js-regexp-literal-test::bool ::JsRegExp ::obj ::JsGlobalObject)
	   (js-regexp-prototype-exec-string-global ::JsRegExp ::JsStringLiteral ::long ::JsGlobalObject)
	   (js-regexp-prototype-exec-no-global ::JsRegExp ::obj ::JsGlobalObject)
	   (js-regexp-prototype-exec-sticky ::JsRegExp ::obj ::JsGlobalObject)
	   (js-regexp-prototype-exec ::JsRegExp ::obj ::JsGlobalObject)
	   (js-regexp-prototype-maybe-exec ::obj ::obj ::JsGlobalObject cache)
	   (inline js-regexp-prototype-exec-as-bool::bool ::JsRegExp ::obj ::JsGlobalObject)
	   (js-regexp-prototype-maybe-exec-as-bool::bool ::JsRegExp ::obj ::JsGlobalObject cache)
	   (js-regexp-prototype-exec-for-match-string ::JsGlobalObject ::JsRegExp ::obj)

	   (js-regexp-right-context ::JsGlobalObject)
	   (js-regexp-left-context ::JsGlobalObject)
	   (js-regexp-last-match ::JsGlobalObject))

   ;; export for memory profiling
   (export (js-regexp-construct ::JsGlobalObject pattern uflags loc)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    regexp-cache ...                                                 */
;*---------------------------------------------------------------------*/
(define-struct regexp-cache index patterns regexps)

(define regexp-cache-default-pattern "")
(define regexp-cache-default-regexp (pregexp ""))

(define regexp-cache-size-default 5)

;*---------------------------------------------------------------------*/
;*    js-init-regexp-caches! ...                                       */
;*---------------------------------------------------------------------*/
(define (js-init-regexp-caches! %this len)
   (let ((caches (make-vector len)))
      (let loop ((i 0))
	 (if (=fx i len)
	     caches
	     (let ((cache (js-make-regexp-cache regexp-cache-size-default)))
		(vector-set! caches i cache)
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    js-make-regexp-cache ...                                         */
;*---------------------------------------------------------------------*/
(define (js-make-regexp-cache sz)
   (regexp-cache 0
      (make-vector sz regexp-cache-default-pattern)
      (make-vector sz regexp-cache-default-regexp)))

;*---------------------------------------------------------------------*/
;*    js-regexp-cache-get ...                                          */
;*---------------------------------------------------------------------*/
(define (js-regexp-cache-get cache pat)
   (let ((idx (regexp-cache-index cache))
	 (pats (regexp-cache-patterns cache)))
      (let loop ((i 0))
	 (cond
	    ((=fx i idx)
	     #f)
	    ((string=? pat (vector-ref pats i))
	     (vector-ref (regexp-cache-regexps cache) i))
	    (else
	     (loop (+fx i 1)))))))
      
;*---------------------------------------------------------------------*/
;*    js-regexp-cache-put! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-regexp-cache-put! cache pat regexp)
   (let ((idx (regexp-cache-index cache))
	 (pats (regexp-cache-patterns cache))
	 (rxs (regexp-cache-regexps cache)))
      (if (<fx idx (vector-length pats))
	  (begin
	     (vector-set! pats idx pat)
	     (vector-set! rxs idx regexp)
	     (regexp-cache-index-set! cache (+fx idx 1)))
	  (let ((idx (random (vector-length pats))))
	     (vector-set! pats idx pat)
	     (vector-set! rxs idx regexp)))))
	     
;*---------------------------------------------------------------------*/
;*    object-serializer ::JsRegExp ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsRegExp
   (lambda (o)
      (with-access::JsRegExp o (rx) rx))
   (lambda (o ctx)
      (if (isa? ctx JsGlobalObject)
	  (js-regexp->jsregexp o ctx)
	  (error "obj->string ::JsRegExp" "Not a JavaScript context" ctx))))

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
	    (with-access::JsRegExp nobj (rx)
	       (with-access::JsRegExp obj ((_rx rx))
		  (js-object-proto-set! nobj (js-get js-regexp (& "prototype") %this))
		  (set! rx (js-donate _rx worker %_this))))
	    nobj))))

;*---------------------------------------------------------------------*/
;*    js-inspect-object ...                                            */
;*---------------------------------------------------------------------*/
(define-method (js-inspect-object obj::JsRegExp #!optional (msg ""))
   (call-next-method)
   (with-access::JsRegExp obj (flags)
      (fprint (current-error-port) "   flags=" flags)
      (fprint (current-error-port) "     indices"
	 (js-regexp-flags-indices? flags))
      (fprint (current-error-port) "     global="
	 (js-regexp-flags-global? flags))
      (fprint (current-error-port) "     icase="
	 (js-regexp-flags-ignorecase? flags))
      (fprint (current-error-port) "     multi="
	 (js-regexp-flags-dotall? flags))
      (fprint (current-error-port) "     dotall="
	 (js-regexp-flags-multiline? flags))
      (fprint (current-error-port) "     uni="
	 (js-regexp-flags-unicode? flags))
      (fprint (current-error-port) "     uniset="
	 (js-regexp-flags-unicodesets? flags))
      (fprint (current-error-port) "     sticky"
	 (js-regexp-flags-sticky? flags))))
   
;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsRegexp ...                                   */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsRegExp op compile isexpr ctx)
   (js-with-context ctx "hop->javascript"
      (lambda (%this)
	 (display "/" op)
	 (display (js-get o (& "source") %this) op)
	 (display "/" op)
	 (when (js-totest (js-get o (& "hasIndices") %this))
	    (display "d" op))
	 (when (js-totest (js-get o (& "global") %this))
	    (display "g" op))
	 (when (js-totest (js-get o (& "ignoreCase") %this))
	    (display "i" op))
	 (when (js-totest (js-get o (& "multiline") %this))
	    (display "m" op))
	 (when (js-totest (js-get o (& "dotAll") %this))
	    (display "s" op))
	 (when (js-totest (js-get o (& "unicode") %this))
	    (display "u" op))
	 (when (js-totest (js-get o (& "unicodeSets") %this))
	    (display "v" op))
	 (when (js-totest (js-get o (& "sticky") %this))
	    (display "y" op)))))

;*---------------------------------------------------------------------*/
;*    js-init-regexp! ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10        */
;*---------------------------------------------------------------------*/
(define (js-init-regexp! %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-regexp js-function
					 js-regexp-prototype
					 js-regexp-cmap
					 js-regexp-exec-cmap
					 js-regexp-pcache)
      ;; local constant strings
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      
      ;; regexp pcache
      (set! js-regexp-pcache
	 ((@ js-make-pcache-table __hopscript_property) 7 "regexp"))
      
      ;; default regexp cmap
      (set! js-regexp-cmap
	 (js-make-jsconstructmap
	    :methods (make-vector 1)
	    :props `#(,(prop (& "lastIndex") (property-flags #t #f #f #f #f)))))
      
      (let ((props `#(,(prop (& "index") (property-flags #t #t #t #f #f))
		      ,(prop (& "input") (property-flags #t #t #t #f #f)))))
	 (set! js-regexp-exec-cmap
	    (js-make-jsconstructmap
	       :methods (make-vector (vector-length props))
	       :props props)))
      
      (set! js-regexp-prototype
	 (instantiateJsRegExp
	    (cmap js-regexp-cmap)
	    (__proto__ (js-object-proto %this))
	    (elements ($create-vector 10))
	    (rx (pregexp ""))
	    (source "")
	    (flags #u32:0)))

      ;; create a HopScript regexp object constructor
      (set! js-regexp
	 (let ((proc (%js-regexp %this)))
	    (js-make-function %this proc
	       (js-function-arity proc)
	       (js-function-info :name "RegExp" :len 2)
	       :__proto__ (js-object-proto js-function)
	       :prototype js-regexp-prototype
	       :alloc js-no-alloc)))
      (init-builtin-regexp-prototype! %this js-regexp js-regexp-prototype)
      
      ;; deprecated (unsupported features)
      (js-bind! %this js-regexp (& "rightContext")
	 :configurable #f :enumerable #f :value (& "")
	 :hidden-class #t)
      (js-bind! %this js-regexp (& "leftContext")
	 :configurable #f :enumerable #f :value (& "")
	 :hidden-class #t)
	 
      ;; bind Regexp in the global object
      (js-bind! %this %this (& "RegExp")
	 :configurable #f :enumerable #f :value js-regexp
	 :hidden-class #t)
      js-regexp))
   
;*---------------------------------------------------------------------*/
;*    %js-regexp ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.3.1    */
;*---------------------------------------------------------------------*/
(define (%js-regexp %this::JsGlobalObject)
   (lambda (this pattern flags loc)
      (with-access::JsGlobalObject %this (js-regexp)
	 (if (and (js-regexp? pattern) (eq? flags (js-undefined)))
	     pattern
	     (js-regexp-construct %this pattern flags loc)))))

;*---------------------------------------------------------------------*/
;*    js-new-regexp1 ...                                               */
;*---------------------------------------------------------------------*/
(define (js-new-regexp1 %this::JsGlobalObject pattern)
   (if (js-regexp? pattern)
       pattern
       (js-regexp-construct %this pattern (js-undefined) (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-new-regexp1/cache ...                                         */
;*---------------------------------------------------------------------*/
(define (js-new-regexp1/cache %this::JsGlobalObject pattern cache)
   (if (js-regexp? pattern)
       pattern
       (let* ((pat (js-tostring pattern %this))
	      (oldrx (js-regexp-cache-get cache pat)))
	  (if oldrx
	      (js-regexp-construct/rx %this oldrx pat #u32:0)
	      (let ((new (js-regexp-construct %this pattern (js-undefined) (js-undefined))))
		 (with-access::JsRegExp new (rx)
		    (js-regexp-cache-put! cache pat rx)
		    new))))))

;*---------------------------------------------------------------------*/
;*    make-js-regexp-pattern ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-7.8.5        */
;*---------------------------------------------------------------------*/
(define (make-js-regexp-pattern %this str::bstring)
   
   (define (err fmt str)
      (js-raise-syntax-error %this fmt str))

   (define (need-new-string? str)
      (let ((len (string-length str)))
	 (let loop ((i 0))
	    (let ((j (string-index str "\\[]" i)))
	       (if (not j)
		   #f
		   (let ((tag (string-ref str j)))
		      (cond
			 ((char=? tag #\[)
			  #t)
			 ((char=? tag #\])
			  #t)
			 ((=fx j (-fx len 1))
			  (err "wrong pattern \"~a\"" str))
			 (else
			  (let ((c (string-ref str (+fx j 1))))
			     (case c
				((#\x) #t)
				((#\u) #t)
				((#\t) #t)
				((#\n) #t)
				((#\v) #t)
				((#\f) #t)
				((#\r) #t)
				((#\") #t)
				(else (loop (+fx j 2)))))))))))))

   (if (not (need-new-string? str))
       str
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
				  (loop (+fx j 2) (+fx w 2) #f)))))))))))))

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
	 (let ((j (string-index str "\\[]" i)))
	    (if (not j)
		(err "wrong pattern \"~a\"" (substring str i0))
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
		      ((char=? tag #\[)
		       (if (and (<fx j len)
				(char=? (string-ref str (+fx j 1)) #\:))
			   ;; a character name e.g., [:alnum:]
			   (let ((k (string-index str #\: (+fx j 2))))
			      (if (or (not k)
				      (>=fx k (-fx len 1))
				      (not (char=? (string-ref str (+fx k 1)) #\])))
				  (err "wrong-pattern \"~a\"" (substring str j))
				  (let ((len (+fx 2 (-fx k j))))
				     (blit-string! str j res w len)
				     (loop (+fx j len) (+fx w len)
					chars ascii))))
			   (begin
			      (string-set! res w #\\)
			      (string-set! res (+fx w 1) #\[)
			      (loop (+fx j 1) (+fx w 2) chars ascii))))
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
						 (starting-range? str (+fx j 4)))
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
;*    js-regexp-construct/rx ...                                       */
;*---------------------------------------------------------------------*/
(define (js-regexp-construct/rx %this::JsGlobalObject rx pattern::bstring flags::uint32)
   (with-access::JsGlobalObject %this (js-regexp js-regexp-cmap js-regexp-prototype)
      (instantiateJsRegExp
	 (cmap js-regexp-cmap)
	 (__proto__ js-regexp-prototype)
	 (elements (vector 0))
	 (rx rx)
	 (source pattern)
	 (flags flags))))

;*---------------------------------------------------------------------*/
;*    js-regexp-construct ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.4.1    */
;*---------------------------------------------------------------------*/
(define (js-regexp-construct %this::JsGlobalObject pattern uflags loc)
   (let ((flags #u32:0))
      (cond
	 ((js-regexp? pattern)
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
		    (d (string-index f #\d))
		    (g (string-index f #\g))
		    (i (string-index f #\i))
		    (m (string-index f #\m))
		    (s (string-index f #\s))
		    (u (string-index f #\u))
		    (v (string-index f #\v))
		    (y (string-index f #\y)))
		(set! flags
		   (bit-oru32
		      (if (integer? i)
			  (JS-REGEXP-FLAG-IGNORECASE)
			  #u32:0)
		      (bit-oru32
			 (if (integer? m)
			     (JS-REGEXP-FLAG-MULTILINE)
			     #u32:0)
			 (bit-oru32
			    (if (integer? g)
				(JS-REGEXP-FLAG-GLOBAL)
				#u32:0)
			    (bit-oru32
			       (if (integer? u)
				   (JS-REGEXP-FLAG-UNICODE)
				   #u32:0)
			       (if (integer? y)
				   (JS-REGEXP-FLAG-STICKY)
				   #u32:0))))))
		(when (or (string-skip f "dgimsuvy")
			  (and (integer? d) (string-index f #\d (+fx 1 d)))
			  (and (integer? g) (string-index f #\g (+fx 1 g)))
			  (and (integer? i) (string-index f #\i (+fx 1 i)))
			  (and (integer? m) (string-index f #\m (+fx 1 m)))
			  (and (integer? s) (string-index f #\m (+fx 1 s)))
			  (and (integer? u) (string-index f #\u (+fx 1 u)))
			  (and (integer? v) (string-index f #\v (+fx 1 v)))
			  (and (integer? y) (string-index f #\y (+fx 1 y))))
		   (js-raise-syntax-error %this "Illegal flags \"~a\"" f))))))
      (multiple-value-bind (pat enc)
	 (make-js-regexp-pattern %this pattern)
	 (let ((rx (pregexp pat
		      'NORAISE
		      (when (js-regexp-flags-ignorecase? flags) 'CASELESS)
		      'JAVASCRIPT_COMPAT
		      (if (and (eq? enc 'ascii)
			       (not (js-regexp-flags-unicode? flags)))
			  'JAVASCRIPT_COMPAT 'UTF8)
		      (when (js-regexp-flags-multiline? flags) 'MULTILINE))))
	    (if (string? rx)
		(js-raise-syntax-error/loc %this loc rx "")
		(js-regexp-construct/rx %this rx pattern flags))))))

;*---------------------------------------------------------------------*/
;*    init-builtin-regexp-prototype! ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6      */
;*---------------------------------------------------------------------*/
(define (init-builtin-regexp-prototype! %this::JsGlobalObject js-regexp obj)
   ;; constructor
   (js-bind! %this obj (& "constructor")
      :value js-regexp
      :enumerable #f
      :hidden-class #t)
   ;; toString
   (js-bind! %this obj (& "toString")
      :value (js-make-function %this
		(lambda (this)
		   (js-string->jsstring
		      (string-append "/"
			 (js-tostring (js-get this (& "source") %this) %this) "/"
			 (if (js-totest (js-get this (& "global") %this)) "g" "")
			 (if (js-totest (js-get this (& "ignoreCase") %this)) "i" "")
			 (if (js-totest (js-get this (& "multiline") %this)) "m" ""))))
		(js-function-arity 0 0)
		(js-function-info :name "toString" :len 0)
		:prototype (js-undefined))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t)
   ;; ignoreCase
   (js-bind! %this obj (& "ignoreCase")
      :get (js-make-function %this
	      (lambda (this)
		 (if (js-regexp? this)
		     (with-access::JsRegExp this (flags)
			(js-regexp-flags-ignorecase? flags))
		     (js-raise-type-error %this "Not a regexp" this)))
	      (js-function-arity 0 0)
	      (js-function-info :name "ignoreCase" :len 0)
	      :prototype (js-undefined))
      :set (js-undefined)
      :writable #f
      :enumerable #f
      :configurable #t)
   ;; multiline
   (js-bind! %this obj (& "multiline")
      :get (js-make-function %this
	      (lambda (this)
		 (if (js-regexp? this)
		     (with-access::JsRegExp this (flags)
			(js-regexp-flags-multiline? flags))
		     (js-raise-type-error %this "Not a regexp" this)))
	      (js-function-arity 0 0)
	      (js-function-info :name "multiline" :len 0)
	      :prototype (js-undefined))
      :set (js-undefined)
      :writable #f
      :enumerable #f
      :configurable #t)
   ;; global
   (js-bind! %this obj (& "global")
      :get (js-make-function %this
	      (lambda (this)
		 (if (js-regexp? this)
		     (with-access::JsRegExp this (flags)
			(js-regexp-flags-global? flags))
		     (js-raise-type-error %this "Not a regexp" this)))
	      (js-function-arity 0 0)
	      (js-function-info :name "global" :len 0)
	      :prototype (js-undefined))
      :set (js-undefined)
      :writable #f
      :enumerable #f
      :configurable #t)
   ;; source
   (js-bind! %this obj (& "source")
      :get (js-make-function %this
	      (lambda (this)
		 (if (js-regexp? this)
		     (with-access::JsRegExp this (source)
			(js-string->jsstring source))
		     (js-raise-type-error %this "Not a regexp" this)))
	      (js-function-arity 0 0)
	      (js-function-info :name "source" :len 0)
	      :prototype (js-undefined))
      :set (js-undefined)
      :writable #f
      :enumerable #f
      :configurable #t)
   ;; exec
   (js-bind! %this obj (& "exec")
      :value (js-make-function %this
		(lambda (this string::obj)
		   (if (not (js-regexp? this))
		       (js-raise-type-error %this "Not a RegExp ~s" this)
		       (js-regexp-prototype-exec this string %this)))
		(js-function-arity 1 0)
		(js-function-info :name "exec" :len 1)
		:prototype (js-undefined))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t)
   ;; test
   (js-bind! %this obj (& "test")
      :value (js-make-function %this (make-regexp-prototype-test %this)
		(js-function-arity 1 0)
		(js-function-info :name "test" :len 1)
		:prototype (js-undefined))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t)
   ;; compile
   (js-bind! %this obj (& "compile")
      :value (js-make-function %this regexp-prototype-compile
		(js-function-arity 1 0)
		(js-function-info :name "compile" :len 1)
		:prototype (js-undefined))
      :writable #t
      :configurable #t
      :enumerable #f
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    js-sbustring ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-substring s start end utf8 %this)
   (cond
      ((=fx start end)
       (& ""))
      ((and (=fx end (+fx start 1)) (not utf8))
       (js-jsstring-fromcharcode
	  (char->integer (string-ref s start)) %this))
;*       (utf8                                                         */
;*        (js-utf8->jsstring (utf8-substring s start end)))            */
      (utf8
       (js-utf8->jsstring (substring s start end)))
      (else
       (js-substring->jsstring s start (-fx end start)))))

;*---------------------------------------------------------------------*/
;*    js-regexp-prototype-exec-no-global-at ...                        */
;*---------------------------------------------------------------------*/
(define (js-regexp-prototype-exec-no-global-at this::JsRegExp string::obj
	   start sticky::bool %this::JsGlobalObject)
   
   (define (match/vector r l clen enc jss s offset)
      (with-access::JsGlobalObject %this (js-regexp-exec-cmap js-regexp-pcache)
	 (if (<fx l 0)
	     (begin
		(when sticky
		   (js-put-jsobject-name/cache! this (& "lastIndex") 0
		      #f %this (js-pcache-ref js-regexp-pcache 5)))
		(js-null))
	     ;; 10
	     (let ((e (vector-ref r 1)))
		;; 11
		(let* ((vec ($create-vector (maxfx l clen)))
		       (a (js-vector->jsarray vec %this))
		       (matchindex (vector-ref r 0))
		       (els ($create-vector 2)))
		   (when sticky
		      (js-put-jsobject-name/cache! this (& "lastIndex")
			 (vector-ref r 1)
			 #f %this (js-pcache-ref js-regexp-pcache 6)))
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
		   ;; 19 & 20
		   ;; 20
		   (let loop ((i 0))
		      (when (<fx i l)
			 (let ((j (*fx i 2)))
			    (vector-set! vec i
			       (if (>=fx (vector-ref r j) 0)
				   (js-substring s
				      (+fx offset (vector-ref r j))
				      (+fx offset (vector-ref r (+fx j 1)))
				      enc
				      %this)
				   (js-undefined)))
			    (loop (+fx i 1)))))
		   (let loop ((i l))
		      (when (<fx i clen)
			 (vector-set! vec i (js-undefined))
			 (loop (+fx i 1))))
		   (with-access::JsGlobalObject %this (js-regexp-last-match)
		      (let ((lm js-regexp-last-match))
			 (vector-set! lm 0 jss)
			 (vector-set! lm 1 matchindex)
			 (vector-set! lm 2 e)))
		   a)))))
   
   (define (exec-string this jss s beg len offset enc)
      (with-access::JsGlobalObject %this (js-regexp-pcache js-regexp-positions)
	 (with-access::JsRegExp this (rx flags)
	    (let ((clen (regexp-capture-count rx)))
	       (cond
		  ((>=fx clen 0)
		   (when (>=fx (*fx (+fx clen 1) 2)
			    (vector-length js-regexp-positions))
		      (set! js-regexp-positions
			 (make-vector (* (+fx clen 2) 2))))
		   (let* ((r js-regexp-positions)
			  (l (pregexp-match-n-positions! rx s r beg len offset)))
		      (match/vector r l clen enc jss s offset)))
		  (else
		   (let ((p (pregexp-match-positions rx s beg len offset)))
		      (if (pair? p)
			  (let* ((v (match-positions->vector p))
				 (l (/fx (vector-length v) 2)))
			     (match/vector v l l enc jss s offset))
			  (begin
			     (when sticky
				(js-put-jsobject-name/cache! this (& "lastIndex") 0
				   #f %this (js-pcache-ref js-regexp-pcache 5)))
			     (js-null))))))))))
   
   (if (and (js-jsstring? string) (js-jsstring-substring? string))
       (with-access::JsStringLiteralSubstring string (left right length)
	  (let ((len (uint32->fixnum length)))
	     (if (and sticky (or (<fx start 0) (>=fx start len)))
		 (with-access::JsGlobalObject %this (js-regexp-pcache)
		    (js-put-jsobject-name/cache! this (& "lastIndex") 0
		       #f %this (js-pcache-ref js-regexp-pcache 5))
		    '())
		 (exec-string this string left start len right #f))))
       (let* ((jss (js-tojsstring string %this))
	      (s (js-jsstring->string jss))
	      (len (string-length s))
	      (enc (isa? jss JsStringLiteralUTF8)))
	  (if (and sticky (or (<fx start 0) (>=fx start len)))
	      (with-access::JsGlobalObject %this (js-regexp-pcache)
		 (js-put-jsobject-name/cache! this (& "lastIndex") 0
		    #f %this (js-pcache-ref js-regexp-pcache 5))
		 '())
	      (exec-string this jss s start len 0 enc)))))

;*---------------------------------------------------------------------*/
;*    js-regexp-prototype-exec-no-global ...                           */
;*---------------------------------------------------------------------*/
(define (js-regexp-prototype-exec-no-global this::JsRegExp string::obj %this::JsGlobalObject)
   (js-regexp-prototype-exec-no-global-at this string 0 #f %this))

;*---------------------------------------------------------------------*/
;*    js-regexp-prototype-exec-sticky ...                              */
;*---------------------------------------------------------------------*/
(define (js-regexp-prototype-exec-sticky this::JsRegExp string::obj %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-regexp-pcache)
      (let ((lastindex (js-tointeger
			  (js-get-jsobject-name/cache this (& "lastIndex")
			     #f %this (js-pcache-ref js-regexp-pcache 4))
			  %this)))
	 (js-regexp-prototype-exec-no-global-at this string lastindex #t %this))))

;*---------------------------------------------------------------------*/
;*    match-positions->vector ...                                      */
;*---------------------------------------------------------------------*/
(define (match-positions->vector l)
   (let* ((len (*fx (length l) 2))
          (vec (make-vector len)))
      (let loop ((i 0)
                 (l l))
         (if (null? l)
             vec
             (begin
                (vector-set! vec i (caar l))
                (vector-set! vec (+fx i 1) (cdar l))
                (loop (+fx i 2) (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    js-regexp-prototype-exec-string-global ...                       */
;*    -------------------------------------------------------------    */
;*    !!! WARNING: If the result shape changes, modify                 */
;*    JS-JSSTRING-MATCH-ALL                                            */
;*---------------------------------------------------------------------*/
(define (js-regexp-prototype-exec-string-global this::JsRegExp
	   jss::JsStringLiteral lastindex::long
	   %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-regexp-pcache js-regexp-positions)
      (with-access::JsRegExp this (rx flags)
	 (let* ((s (js-jsstring->string jss))
		(len (string-length s))
		(enc (isa? jss JsStringLiteralUTF8))
		(clen (regexp-capture-count rx)))
	    (cond
	       ((or (<fx lastindex 0) (>fx lastindex len))
		(js-put-jsobject-name/cache! this (& "lastIndex") 0
		   #f %this (js-pcache-ref js-regexp-pcache 0))
		(js-null))
	       ((>=fx clen 0)
		(when (>=fx (*fx (+fx clen 1) 2)
			 (vector-length js-regexp-positions))
		   (set! js-regexp-positions
		      (make-vector (* (+fx clen 1) 2))))
		(let* ((r js-regexp-positions)
		       (l (pregexp-match-n-positions! rx s r lastindex len)))
		   (if (<fx l 0)
		       (begin
			  (js-put-jsobject-name/cache! this (& "lastIndex") 0
			     #f %this (js-pcache-ref js-regexp-pcache 0))
			  (js-null))
		       ;; 10
		       (let ((e (vector-ref r 1)))
			  ;; 11
			  (js-put-jsobject-name/cache! this (& "lastIndex") e
			     #f %this (js-pcache-ref js-regexp-pcache 0))
			  (let* ((vec ($create-vector (maxfx l clen)))
				 (a (js-vector->jsarray vec %this))
				 (matchindex (vector-ref r 0))
				 (els ($create-vector 2)))
			     ;; bind the result cmap and add the elements
			     (with-access::JsArray a (elements cmap)
				(with-access::JsGlobalObject %this (js-regexp-exec-cmap)
				   (set! cmap js-regexp-exec-cmap))
				(set! elements els))
			     ;; 15
			     (vector-set! els 0 matchindex)
			     ;; (js-bind! %this a 'index :value matchindex)
			     ;; 16
			     (vector-set! els 1 jss)
			     ;; (js-bind! %this a 'input :value jss)
			     ;; 17
			     ;; no need as already automatically set
			     ;; 19 & 20
			     ;; 20
			     (let loop ((i 0))
				(when (<fx i l)
				   (let ((j (*fx i 2)))
				      (vector-set! vec i
					 (if (>=fx (vector-ref r j) 0)
					     (js-substring s
						(vector-ref r j)
						(vector-ref r (+fx j 1))
						enc
						%this)
					     (js-undefined)))
				      (loop (+fx i 1)))))
			     (let loop ((i l))
				(when (<fx i clen)
				   (vector-set! vec i (js-undefined))
				   (loop (+fx i 1))))
			     a)))))
	       ((pregexp-match-positions rx s lastindex)
		=>
		(lambda (r)
		   ;; 10
		   (let ((e (cdar r)))
		      ;; 11
		      (js-put-jsobject-name/cache! this (& "lastIndex") e
			 #f %this (js-pcache-ref js-regexp-pcache 0))
		      (let* ((n (length r))
			     ;;(_ (tprint "match-positions n=" n))
			     (vec ($create-vector n))
			     (a (js-vector->jsarray vec %this))
			     (matchindex (caar r))
			     (els ($create-vector 2)))
			 ;; bind the result cmap and add the elements
			 (with-access::JsArray a (elements cmap)
			    (with-access::JsGlobalObject %this (js-regexp-exec-cmap)
			       (set! cmap js-regexp-exec-cmap))
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
			    (js-substring s (caar r) (cdar r) enc %this))
			 ;; 20
			 (let loop ((c (cdr r))
				    (i 1))
			    (when (pair? c)
			       (let ((r (car c)))
				  (vector-set! vec i
				     (if (pair? r)
					 (js-substring s (car r) (cdr r) enc %this)
					 (js-undefined))))
			       (loop (cdr c) (+fx i 1))))
			 a))))
	       (else
		(js-put-jsobject-name/cache! this (& "lastIndex") 0
		   #f %this (js-pcache-ref js-regexp-pcache 0))
		(js-null)))))))

;*---------------------------------------------------------------------*/
;*    js-regexp-prototype-exec-global ...                              */
;*---------------------------------------------------------------------*/
(define (js-regexp-prototype-exec-global this::JsRegExp string::obj %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-regexp-pcache js-regexp-positions)
      (with-access::JsRegExp this (rx flags)
	 (let ((lastindex (js-tointeger
			     (js-get-jsobject-name/cache this (& "lastIndex")
				#f %this (js-pcache-ref js-regexp-pcache 0))
			     %this))
	       (jss (js-tojsstring string %this)))
	    (js-regexp-prototype-exec-string-global this jss lastindex %this)))))

;*---------------------------------------------------------------------*/
;*    js-regexp-prototype-exec ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6.2    */
;*---------------------------------------------------------------------*/
(define (js-regexp-prototype-exec this::JsRegExp string::obj %this::JsGlobalObject)
   (with-access::JsRegExp this (flags)
      (cond
	 ((js-regexp-flags-global? flags)
	  (js-regexp-prototype-exec-global this string %this))
	 ((js-regexp-flags-sticky? flags)
	  (js-regexp-prototype-exec-sticky this string %this))
	 (else
	  (js-regexp-prototype-exec-no-global this string %this)))))

;*---------------------------------------------------------------------*/
;*    js-regexp-prototype-maybe-exec ...                               */
;*---------------------------------------------------------------------*/
(define (js-regexp-prototype-maybe-exec this string::obj %this::JsGlobalObject cache)
   (if (js-regexp? this)
       (js-regexp-prototype-exec this string %this)
       (with-access::JsGlobalObject %this (js-regexp-pcache)
	  (let ((exec (js-get-name/cache this (& "exec") #f %this
			 (or cache (js-pcache-ref js-regexp-pcache 1))
			 -1 '(imap+))))
	     (js-call1 %this exec this string)))))

;*---------------------------------------------------------------------*/
;*    js-regexp-prototype-exec-for-match-string ...                    */
;*    -------------------------------------------------------------    */
;*    This function is used when the regexp is executed on behalf of   */
;*    string.match method and when regexp is built from a string.      */
;*    In that particular case, the regexp is not global and there is   */
;*    no need to manage the lastIndex property.                        */
;*---------------------------------------------------------------------*/
(define (js-regexp-prototype-exec-for-match-string %this::JsGlobalObject this::JsRegExp string::obj)
   (with-access::JsGlobalObject %this (js-regexp-pcache js-regexp-positions)
      (with-access::JsRegExp this (rx)
	 (let* ((jss (js-tojsstring string %this))
		(s (js-jsstring->string jss))
		(len (string-length s))
		(enc (isa? jss JsStringLiteralUTF8))
		(clen (regexp-capture-count rx)))
	    (cond
	       ((>=fx clen 0)
		(when (>=fx (*fx (+fx clen 1) 2)
			 (vector-length js-regexp-positions))
		   (set! js-regexp-positions
		      (make-vector (*fx (+fx clen 1) 2))))
		(let* ((r js-regexp-positions)
		       (l (pregexp-match-n-positions! rx s r 0 len)))
		   (if (<fx l 0)
		       (js-null)
		       ;; 10 & 11
		       (let* ((vec ($create-vector (maxfx l clen)))
			      (a (js-vector->jsarray vec %this))
			      (matchindex (vector-ref r 0))
			      (els ($create-vector 2)))
			  ;; bind the result cmap and add the elements
			  (with-access::JsArray a (elements cmap)
			     (with-access::JsGlobalObject %this (js-regexp-exec-cmap)
				(set! cmap js-regexp-exec-cmap))
			     (set! elements els))
			  ;; 15
			  (vector-set! els 0 matchindex)
			  ;; (js-bind! %this a 'index :value matchindex)
			  ;; 16
			  (vector-set! els 1 jss)
			  ;; (js-bind! %this a 'input :value jss)
			  ;; 17
			  ;; no need as already automatically set
			  ;; 19 & 20
			  (let loop ((i 0))
			     (when (<fx i l)
				(let ((j (*fx i 2)))
				   (vector-set! vec i
				      (if (>=fx (vector-ref r j) 0)
					  (js-substring s
					     (vector-ref r j)
					     (vector-ref r (+fx j 1))
					     enc
					     %this)
					  (js-undefined)))
				   (loop (+fx i 1)))))
			  (let loop ((i l))
			     (when (<fx i clen)
				(vector-set! vec i (js-undefined))
				(loop (+fx i 1))))
			  a))))
	       ((pregexp-match-positions rx s 0)
		=>
		(lambda (r)
		   ;; 10 & 11
		   (let* ((n (length r))
			  (vec ($create-vector n))
			  (a (js-vector->jsarray vec %this))
			  (matchindex (caar r))
			  (els ($create-vector 2)))
		      ;; bind the result cmap and add the elements
		      (with-access::JsArray a (elements cmap)
			 (with-access::JsGlobalObject %this (js-regexp-exec-cmap)
			    (set! cmap js-regexp-exec-cmap))
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
			 (js-substring s (caar r) (cdar r) enc %this))
		      ;; 20
		      (let loop ((c (cdr r))
				 (i 1))
			 (when (pair? c)
			    (let ((r (car c)))
			       (vector-set! vec i
				  (if (pair? r)
				      (js-substring s (car r) (cdr r) enc %this)
				      (js-undefined))))
			    (loop (cdr c) (+fx i 1))))
		      a)))
	       (else
		(js-null)))))))

;*---------------------------------------------------------------------*/
;*    js-regexp-prototype-exec-as-bool ...                             */
;*    -------------------------------------------------------------    */
;*    This version is directly invoked in compiled code when the       */
;*    RX is not global and when the expression is used for its         */
;*    boolean result.                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-regexp-prototype-exec-as-bool this::JsRegExp string::obj %this::JsGlobalObject)
   (with-access::JsRegExp this (rx flags)
      (let* ((s (js-tostring string %this))
	     (len (string-length s)))
	 (>=fx (pregexp-match-n-positions! rx s '#() 0 len) 0))))

;*---------------------------------------------------------------------*/
;*    js-regexp-prototype-maybe-exec-as-bool ...                       */
;*    -------------------------------------------------------------    */
;*    This version is directly invoked in compiled code when the       */
;*    RX is not global and when the expression is used for its         */
;*    boolean result.                                                  */
;*---------------------------------------------------------------------*/
(define (js-regexp-prototype-maybe-exec-as-bool this string::obj %this::JsGlobalObject cache)
   (if (js-regexp? this)
       (js-regexp-prototype-exec-as-bool this string %this)
       (with-access::JsGlobalObject %this (js-regexp-pcache)
	  (let ((exec (js-get-name/cache this (& "exec") #f %this
			 (or cache (js-pcache-ref js-regexp-pcache 2))
			 -1 '(imap+))))
	     (js-totest (js-call1 %this exec this string))))))

;*---------------------------------------------------------------------*/
;*    make-regexp-prototype-test ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.10.6.3    */
;*---------------------------------------------------------------------*/
(define (make-regexp-prototype-test %this::JsGlobalObject)
   (lambda (this string::obj)
      (if (not (js-regexp? this))
	  (js-raise-type-error %this "Not a RegExp ~s" this)
	  (js-regexp-test this string %this))))

;*---------------------------------------------------------------------*/
;*    js-regexp-test ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-regexp-test this::JsRegExp string %this)
   (not (eq? (js-regexp-prototype-exec this string %this) (js-null))))

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
;*    In that particular situation, there is no need to store all the  */
;*    EXEC variables.                                                  */
;*---------------------------------------------------------------------*/
(define (js-regexp-literal-test-string::bool this::JsRegExp str::obj %this)
   (with-access::JsGlobalObject %this (js-regexp-pcache js-regexp-last-match)
      (with-access::JsRegExp this (rx flags)
	 (let* ((lastindex (js-get-jsobject-name/cache this (& "lastIndex")
			      #f %this (js-pcache-ref js-regexp-pcache 3)))
		(global (js-regexp-flags-global? flags))
		(s (js-jsstring->string str))
		(i (cond
		      ((not global)
		       0)
		      ((or (<fx lastindex 0) (>fx lastindex (string-length s)))
		       0)
		      (else
		       lastindex))))
	    (js-call-with-stack-vector
	       (make-vector 2)
	       (lambda (v)
		  (let ((m (pregexp-match-n-positions! rx s v i (string-length s))))
		     (when (>=fx m 0)
			(let ((lm js-regexp-last-match))
			   (vector-set! lm 0 str)
			   (vector-set! lm 1 (vector-ref v 0))
			   (vector-set! lm 2 (vector-ref v 1))
			   #t)))))))))

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

;*---------------------------------------------------------------------*/
;*    js-regexp-right-context ...                                      */
;*---------------------------------------------------------------------*/
(define (js-regexp-right-context %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-regexp-last-match)
      (let* ((lm js-regexp-last-match)
	     (s (vector-ref lm 0)))
	 (if (js-jsstring? s)
	     (js-jsstring-substring s (vector-ref lm 2) (js-jsstring-length s) %this)
	     (& "")))))

;*---------------------------------------------------------------------*/
;*    js-regexp-left-context ...                                       */
;*---------------------------------------------------------------------*/
(define (js-regexp-left-context %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-regexp-last-match)
      (let* ((lm js-regexp-last-match)
	     (s (vector-ref lm 0)))
	 (if (js-jsstring? s)
	     (js-jsstring-substring s 0 (vector-ref lm 1) %this)
	     (& "")))))

;*---------------------------------------------------------------------*/
;*    js-regexp-last-match ...                                         */
;*---------------------------------------------------------------------*/
(define (js-regexp-last-match %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-regexp-last-match)
      (let* ((lm js-regexp-last-match)
	     (s (vector-ref lm 0)))
	 (if (js-jsstring? s)
	     (js-jsstring-substring s (vector-ref lm 1) (vector-ref lm 2) %this)
	     (& "")))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
