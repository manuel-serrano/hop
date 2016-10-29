;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/stringliteral.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 21 14:13:28 2014                          */
;*    Last change :  Wed Oct 26 17:51:27 2016 (serrano)                */
;*    Copyright   :  2014-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Internal implementation of literal strings                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_stringliteral

   (library hop)
   
   (import __hopscript_types
	   __hopscript_public
	   __hopscript_private
	   __hopscript_property)
   
   (export (inline js-ascii->jsstring::bstring ::bstring)
	   (inline js-utf8->jsstring::JsStringLiteralUTF8 ::bstring)
	   (js-string->jsstring::obj ::bstring)
	   (js-stringlist->jsstring ::pair-nil)
	   (inline js-jsstring->string::bstring ::obj)
	   (inline js-jsstring?::bool ::obj)
	   (js-jsstring-ref ::obj ::uint32)
	   (js-jsstring-length::long ::obj)
	   (js-jsstring-character-length::long ::obj)
	   (inline js-jsstring-null? ::obj)
	   (inline js-jsstring=?::bool ::obj ::obj)
	   (inline js-jsstring<?::bool ::obj ::obj)
	   (inline js-jsstring<=?::bool ::obj ::obj)
	   (inline js-jsstring>?::bool ::obj ::obj)
	   (inline js-jsstring>=?::bool ::obj ::obj)
	   (js-string->number::obj ::bstring ::JsGlobalObject)
	   (js-integer->jsstring ::long)
	   (js-jsstring->bool::bool ::obj)
	   (generic js-jsstring-normalize!::bstring ::JsStringLiteral)
	   
	   (inline js-jsstring-append::JsStringLiteral ::obj ::obj)
	   (utf8-codeunit-ref::long ::bstring ::long)
	   (utf8-codeunit-length::long ::bstring)
	   (js-utf8-ref ::bstring ::long)
	   (js-get-string ::obj ::obj ::obj)
	   (js-put-string! ::bstring ::obj ::obj ::bool ::obj)
	   (js-jsstring-indexof ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-indexof ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-lastindexof ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-lastindexof ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-charcodeat ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-charcodeat ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-charat ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-charat ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-substring ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-substring ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-substr ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-substr ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-tolowercase ::obj)
	   (js-jsstring-maybe-tolowercase ::obj ::JsGlobalObject)
	   (js-jsstring-touppercase ::obj)
	   (js-jsstring-maybe-touppercase ::obj ::JsGlobalObject)
	   (js-jsstring-split ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-split ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-replace ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-replace ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-match ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-match ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-naturalcompare ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-naturalcompare ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-localecompare ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-localecompare ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-trim ::obj)
	   (js-jsstring-maybe-trim ::obj ::JsGlobalObject)
	   ))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsString ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsStringLiteral
   js-jsstring->string
   (lambda (o %this)
      (if (eq? %this 'hop)
	  o
	  (js-string->jsstring o))))

;*---------------------------------------------------------------------*/
;*    object-print ::JsStringLiteral ...                               */
;*---------------------------------------------------------------------*/
(define-method (object-print obj::JsStringLiteral op proc)
   (display-js-string obj op))

;*---------------------------------------------------------------------*/
;*    js-tostring::bstring ::JsStringLiteral ...                       */
;*---------------------------------------------------------------------*/
(define-method (js-tostring::bstring obj::JsStringLiteral %this)
   (js-jsstring->string obj))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsStringLiteral ...                            */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsStringLiteral op compile isexpr)
   (hop->javascript (js-jsstring->string o) op compile isexpr))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsStringLiteral ...                        */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value o::JsStringLiteral)
   (js-jsstring->string o))

;*---------------------------------------------------------------------*/
;*    xml-write ::JsStringLiteral ...                                  */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::JsStringLiteral op backend)
   (display-js-string obj op))

;*---------------------------------------------------------------------*/
;*    js-inspect ::JsStringLiteral ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-inspect s::JsStringLiteral cnt)
   s)

;*---------------------------------------------------------------------*/
;*    xml-attribute-encode ::JsStringLiteral ...                       */
;*---------------------------------------------------------------------*/
(define-method (xml-attribute-encode obj::JsStringLiteral)
   (xml-attribute-encode (js-jsstring->string obj)))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsStringLiteral ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsStringLiteral worker %this)
   (js-jsstring-normalize! obj))

;*---------------------------------------------------------------------*/
;*    scheme->response ::JsStringLiteral ...                           */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::JsStringLiteral req)
   (scheme->response (js-jsstring->string obj) req))

;*---------------------------------------------------------------------*/
;*    string-call ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (string-call fun this . args)
   `(cond
       ((string? ,this)
	(,(symbol-append 'ascii- fun) ,this ,@args))
       ((isa? ,this JsStringLiteralASCII)
	(,(symbol-append 'ascii- fun) (js-jsstring->string ,this) ,@args))
       (else
	(,(symbol-append 'utf8- fun) (js-jsstring->string ,this) ,@args))))

;*---------------------------------------------------------------------*/
;*    isascii? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (isascii? val)
   (or (string? val) (isa? val JsStringLiteralASCII)))

;*---------------------------------------------------------------------*/
;*    js-ascii->jsstring ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-ascii->jsstring::bstring val::bstring)
   val)

;*---------------------------------------------------------------------*/
;*    js-ascii->jsstring ...                                           */
;*    -------------------------------------------------------------    */
;*    For local module optimization                                    */
;*---------------------------------------------------------------------*/
(define-macro (js-ascii->jsstring val)
   val)

;*---------------------------------------------------------------------*/
;*    js-utf8->jsstring ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-utf8->jsstring::JsStringLiteralUTF8 val::bstring)
   (instantiate::JsStringLiteralUTF8
      (weight (string-length val))
      (left val)
      (right #f)))

;*---------------------------------------------------------------------*/
;*    js-string->jsstring ...                                          */
;*    -------------------------------------------------------------    */
;*    Create a JsStringLiteral from a Scheme string literal.           */
;*---------------------------------------------------------------------*/
(define (js-string->jsstring::obj val::bstring)
   (case (string-minimal-charset val)
      ((ascii) (js-ascii->jsstring val))
      ((latin1 utf8) (js-utf8->jsstring val))
      (else (error "string->jsstring" "unsupported encoding"
	       (string-minimal-charset val)))))

;*---------------------------------------------------------------------*/
;*    js-stringlist->jsstring ...                                      */
;*    -------------------------------------------------------------    */
;*    Create a JsStringLiteral from a list of Scheme string literals.  */
;*---------------------------------------------------------------------*/
(define (js-stringlist->jsstring val::pair-nil)
   (if (null? val)
       (js-ascii->jsstring "")
       (let loop ((val val))
	  (cond
	     ((null? (cdr val))
	      (js-string->jsstring (car val)))
	     ((eq? (string-minimal-charset (car val)) 'ascii)
	      (instantiate::JsStringLiteralASCII
		 (left (car val))
		 (weight (string-length (car val)))
		 (right (loop (cdr val)))))
	     (else
	      (instantiate::JsStringLiteralUTF8
		 (left (car val))
		 (weight (string-length (car val)))
		 (right (loop (cdr val)))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring->string ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring->string::bstring js::obj)
   (if (string? js)
       js
       (js-jsstring-normalize! js)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize! ...                                       */
;*    -------------------------------------------------------------    */
;*    Tailrec normalization (with explicit stack management).          */
;*---------------------------------------------------------------------*/
(define-generic (js-jsstring-normalize!::bstring js::JsStringLiteral))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize! ...                                       */
;*    -------------------------------------------------------------    */
;*    Tailrec normalization (with explicit stack management).          */
;*---------------------------------------------------------------------*/
(define-method (js-jsstring-normalize!::bstring js::JsStringLiteralASCII)
   (with-access::JsStringLiteral js (left right weight)
      (if (and (string? left) (not right))
	  left
	  (let ((buffer (make-string (js-string-literal-length js))))
	     (let loop ((i 0)
			(stack (list js)))
		(if (null? stack)
		    (begin
		       (set! weight i)
		       (set! left buffer)
		       (set! right #f)
		       buffer)
		    (let ((s (car stack)))
		       (if (string? s)
			   (let ((len (string-length s)))
			      (blit-string! s 0 buffer i len)
			      (loop (+fx i len) (cdr stack)))
			   (with-access::JsStringLiteral s (left right)
			      (if right
				  (loop i (cons* left right (cdr stack)))
				  (loop i (cons left (cdr stack)))))))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize! ...                                       */
;*    -------------------------------------------------------------    */
;*    Tailrec normalization (with explicit stack management).          */
;*---------------------------------------------------------------------*/
(define-method (js-jsstring-normalize!::bstring js::JsStringLiteralUTF8)
   (with-access::JsStringLiteral js (left right weight)
      (if (and (string? left) (not right))
	  left
	  (let ((buffer (make-string (js-string-literal-length js))))
	     (let loop ((i 0)
			(stack (list js)))
		(cond
		   ((null? stack)
		    (set! weight i)
		    (set! left (string-shrink! buffer i))
		    (set! right #f)
		    left)
		   ((string? (car stack))
		    (let ((len (string-length (car stack))))
		       (blit-string! (car stack) 0 buffer i len)
		       (loop (+fx i len) (cdr stack))))
		   ((isa? (car stack) JsStringLiteralASCII)
		    (with-access::JsStringLiteralASCII (car stack) (left right)
		       (if right
			   (loop i (cons* left right (cdr stack)))
			   (loop i (cons left (cdr stack))))))
		   (else
		    (with-access::JsStringLiteral (car stack) (left right)
		       (cond
			  ((string? left)
			   (let ((ni (utf8-string-append-fill! buffer i left)))
			      (if right
				  (loop ni (cons right (cdr stack)))
				  (loop ni (cdr stack)))))
			  (right
			   (loop i (cons* left right (cdr stack))))
			  (else
			   (loop i (cons left (cdr stack)))))))))))))

;* (define-method (js-jsstring-normalize-old!::bstring js::JsStringLiteralUTF8) */
;*    (with-access::JsStringLiteral js (left right weight)             */
;*       (if (and (string? left) (not right))                          */
;* 	  left                                                         */
;* 	  (let ((buffer (make-string (js-string-literal-length js))))  */
;* 	     (let loop ((i 0)                                          */
;* 			(stack (list js)))                             */
;* 		(cond                                                  */
;* 		   ((null? stack)                                      */
;* 		    (set! weight i)                                    */
;* 		    (set! left (string-shrink! buffer i))              */
;* 		    (set! right #f)                                    */
;* 		    left)                                              */
;* 		   ((string? (car stack))                              */
;* 		    (let ((ni (utf8-string-append-fill! buffer i (car stack)))) */
;* 		       (loop ni (cdr stack))))                         */
;* 		   (else                                               */
;* 		    (with-access::JsStringLiteral (car stack) (left right) */
;* 		       (if right                                       */
;* 			   (loop i (cons* left right (cdr stack)))     */
;* 			   (loop i (cons left (cdr stack))))))))))))   */

;*---------------------------------------------------------------------*/
;*    js-string-literal-length ...                                     */
;*---------------------------------------------------------------------*/
(define (js-string-literal-length::long js::JsStringLiteral)
   (let loop ((len 0)
	      (js js))
      (cond
	 ((string? js)
	  (+fx len (string-length js)))
	 ((not js)
	  len)
	 (else
	  (with-access::JsStringLiteral js (weight right)
	     (loop (+fx len weight) right))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-length ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsstring-length js)
   (if (string? js)
       (string-length js)
       (js-string-literal-length js)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-character-length ...                                 */
;*---------------------------------------------------------------------*/
(define (js-jsstring-character-length js)
   (cond
      ((string? js)
       (string-length js))
      ((isa? js JsStringLiteralASCII)
       (with-access::JsStringLiteralASCII js (right left weight)
	  (if (not right)
	      weight
	      (js-string-literal-length js))))
      (else
       (utf8-codeunit-length (js-jsstring-normalize! js)))))

;*---------------------------------------------------------------------*/
;*    display-js-string ...                                            */
;*---------------------------------------------------------------------*/
(define (display-js-string jstr::JsStringLiteral op)
   (display (js-jsstring->string jstr) op))

;*---------------------------------------------------------------------*/
;*    js-jsstring? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring? obj)
   (or (string? obj) (isa? obj JsStringLiteral)))

;*---------------------------------------------------------------------*/
;*    hop-register-value ::JsStringLiteral ...                         */
;*---------------------------------------------------------------------*/
(define-method (hop-register-value s::JsStringLiteral register)
   #t)

;*---------------------------------------------------------------------*/
;*    js-jsstring-null? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-null? js)
   (string-null? (js-jsstring->string js)))

;*---------------------------------------------------------------------*/
;*    js-jsstring=? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring=?::bool left right)
   (string=? (js-jsstring->string left) (js-jsstring->string right)))

;*---------------------------------------------------------------------*/
;*    js-jsstring>? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring>?::bool left right)
   (string>? (js-jsstring->string left) (js-jsstring->string right)))

;*---------------------------------------------------------------------*/
;*    js-jsstring>=? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring>=?::bool left right)
   (string>=? (js-jsstring->string left) (js-jsstring->string right)))

;*---------------------------------------------------------------------*/
;*    js-jsstring<? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring<?::bool left right)
   (string<? (js-jsstring->string left) (js-jsstring->string right)))

;*---------------------------------------------------------------------*/
;*    js-jsstring<=? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring<=?::bool left right)
   (string<=? (js-jsstring->string left) (js-jsstring->string right)))

;*---------------------------------------------------------------------*/
;*    make-integer-table ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (make-integer-table num)
   `(vector
       ,@(map (lambda (i)
		 (js-ascii->jsstring (integer->string i)))
	    (iota num))))

;*---------------------------------------------------------------------*/
;*    integers ...                                                     */
;*---------------------------------------------------------------------*/
(define integers (make-integer-table 100))

;*---------------------------------------------------------------------*/
;*    js-string->number ...                                            */
;*---------------------------------------------------------------------*/
(define (js-string->number::obj str::bstring %this::JsGlobalObject)
   (let ((str (trim-whitespaces+ str :left #t :right #t :plus #t)))
      (cond
	 ((string=? str "Infinity")
	  +inf.0)
	 ((string=? str "+Infinity")
	  +inf.0)
	 ((string=? str "-Infinity")
	  -inf.0)
	 ((string=? str "NaN")
	  +nan.0)
	 ((string-null? str)
	  0)
	 ((or (string-prefix? "0x" str) (string-prefix? "0X" str))
	  (js-parseint str 16 #t %this))
	 ((string-index str "eE.")
	  (js-parsefloat str #t %this))
	 (else
	  (js-parseint str 10 #t %this)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-tonumber ...                                         */
;*---------------------------------------------------------------------*/
(define (js-jsstring-tonumber this %this)
   (js-string->number (js-jsstring->string this) %this))

;*---------------------------------------------------------------------*/
;*    js-tonumber ::JsStringLiteral ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tonumber this::JsStringLiteral %this)
   (js-jsstring-tonumber this %this))

;*---------------------------------------------------------------------*/
;*    js-tointeger ::JsStringLiteral ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tointeger this::JsStringLiteral %this)
   (js-tointeger (js-jsstring-tonumber this %this) %this))

;*---------------------------------------------------------------------*/
;*    js-integer->jsstring ...                                         */
;*---------------------------------------------------------------------*/
(define (js-integer->jsstring num::long)
   (if (or (<fx num 0) (>=fx num (vector-length integers)))
       (js-string->jsstring (integer->string num))
       (vector-ref-ur integers num)))

;*---------------------------------------------------------------------*/
;*    js-jsstring->bool ...                                            */
;*---------------------------------------------------------------------*/
(define (js-jsstring->bool::bool s)
   (>fx (string-length (js-jsstring->string s)) 0))

;*---------------------------------------------------------------------*/
;*    js-jsstring-append ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-append::JsStringLiteral left::obj right::obj)
   (if (or (isa? left JsStringLiteralUTF8) (isa? right JsStringLiteralUTF8))
       (instantiate::JsStringLiteralUTF8
	  (weight (js-jsstring-length left))
	  (left left)
	  (right right))
       (instantiate::JsStringLiteralASCII
	  (weight (js-jsstring-length left))
	  (left left)
	  (right right))))

;*---------------------------------------------------------------------*/
;*    utf8-codeunit-length ...                                         */
;*    -------------------------------------------------------------    */
;*    Returns the number of code points required to encode that        */
;*    UTF8 string (might be bigger than the UTF8 length).              */
;*---------------------------------------------------------------------*/
(define (utf8-codeunit-length::long str::bstring)
   (let ((len (string-length str)))
      (let loop ((r 0)
		 (l 0))
	 (if (>=fx r len)
	     l
	     (let* ((c (string-ref str r))
		    (s (utf8-char-size c)))
		(if (and (=fx s 4)
			 (or (=fx (char->integer c) #xf0)
			     (=fx (char->integer c) #xf4)))
		    (loop (+fx r s) (+fx l 2))
		    (loop (+fx r s) (+fx l 1))))))))

;*---------------------------------------------------------------------*/
;*    utf8-left-replacement-codeunit ...                               */
;*    -------------------------------------------------------------    */
;*    See UCS2-STRING->UTF8-STRING.                                    */
;*---------------------------------------------------------------------*/
(define (utf8-left-replacement-codeunit str r)
   (let* ((b1 (char->integer (string-ref str (+fx 1 r))))
	  (b2 (char->integer (string-ref str (+fx 2 r))))
	  (b3 (char->integer (string-ref str (+fx 3 r))))
	  (u4u3 (bit-lsh (bit-and b3 #x3) 2))
	  (xx (bit-and (bit-rsh b2 4) #x3))
	  (wwww (bit-and b1 #xf))
	  (u2u1 (bit-and (bit-rsh b1 4) #x3))
	  (uuuu (bit-or u4u3 u2u1))
	  (vvvv (-fx uuuu 1))
	  (hi #b110110))
      (bit-or xx
	 (bit-or
	    (bit-lsh wwww 2)
	    (bit-or (bit-lsh vvvv 6) (bit-lsh hi 10))))))
   
;*---------------------------------------------------------------------*/
;*    utf8-right-replacement-codeunit ...                              */
;*    -------------------------------------------------------------    */
;*    See UCS2-STRING->UTF8-STRING.                                    */
;*---------------------------------------------------------------------*/
(define (utf8-right-replacement-codeunit str r)
   (let* ((b1 (char->integer (string-ref str (+fx 1 r))))
	  (b2 (char->integer (string-ref str (+fx 2 r))))
	  (b3 (char->integer (string-ref str (+fx 3 r))))
	  (zzzzzz (bit-and b3 #x3f))
	  (yyyy (bit-and b2 #xf))
	  (hi #b110111))
      (bit-or zzzzzz (bit-or (bit-lsh yyyy 6) (bit-lsh hi 10)))))
   
;*---------------------------------------------------------------------*/
;*    codepoint-length ...                                             */
;*    -------------------------------------------------------------    */
;*    Returns the number of code units of this code point.             */
;*---------------------------------------------------------------------*/
(define (codepoint-length s c)
   (case (char->integer c)
      ((#xf0 #xf4 #xf8 #xfc) 2)
      (else 1)))

;*---------------------------------------------------------------------*/
;*    utf8-string-codeunit-ref ...                                     */
;*    -------------------------------------------------------------    */
;*    Returns the ith code unit (UTF16 code unit) of the UTF8 source   */
;*    string.                                                          */
;*---------------------------------------------------------------------*/
(define (utf8-codeunit-ref str i::long)
   (let ((len (string-length str)))
      (let loop ((r 0) (i i))
	 (let* ((c (string-ref str r))
		(s (utf8-char-size c))
		(u (codepoint-length s c)))
	    (cond
	       ((>=fx i u)
		(loop (+fx r s) (-fx i u)))
	       ((=fx s 1)
		(char->integer (string-ref str r)))
	       ((char=? c (integer->char #xf8))
		(utf8-left-replacement-codeunit str r))
	       ((char=? c (integer->char #xfc))
		(utf8-right-replacement-codeunit str r))
	       (else
		(let* ((utf8 (substring str r (+fx r s)))
		       (ucs2 (utf8-string->ucs2-string utf8)))
		   (ucs2->integer (ucs2-string-ref ucs2 i)))))))))

;*---------------------------------------------------------------------*/
;*    js-utf8-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-utf8-ref str::bstring index::long)

   (let ((n (utf8-codeunit-ref str index)))
      (if (<= n 127)
	  (js-ascii->jsstring
	     (make-string 1 (integer->char n)))
	  (let ((ns (utf8-char-size (string-ref str index))))
	     [assert (str) 
                (equal?
		   (ucs2-string->utf8-string
		      (ucs2-string
			 (integer->ucs2 (utf8-codeunit-ref str index))))
		   (js-utf8->jsstring
		      (substring str index (+fx index ns))))]
	     (js-utf8->jsstring
		(substring str index (+fx index ns)))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-ref ...                                              */
;*---------------------------------------------------------------------*/
(define (js-jsstring-ref o index::uint32)
   
   (define (ascii-jsstring-ref val fxpos)
      (if (or (<fx fxpos 0) (>=fx fxpos (string-length val)))
	  (js-undefined)
	  (js-ascii->jsstring (make-string 1 (string-ref-ur val fxpos)))))
   
   (define (utf8-jsstring-ref val fxpos)
      (if (or (<fx fxpos 0) (>=fx fxpos (utf8-codeunit-length val)))
	  (js-undefined)
	  (js-utf8-ref val fxpos)))
   
   (string-call jsstring-ref o (uint32->fixnum index)))

;*---------------------------------------------------------------------*/
;*    js-get ::JsStringLiteral ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsStringLiteral prop %this)
   (js-get-string o prop %this))

;*---------------------------------------------------------------------*/
;*    js-get-string ...                                                */
;*---------------------------------------------------------------------*/
(define (js-get-string o prop %this)
   (let ((i (js-toindex prop)))
      (if (js-isindex? i)
	  (js-jsstring-ref o i)
	  (let ((p (js-toname prop %this)))
	     (cond
		((eq? prop 'length)
		 (js-jsstring-character-length o))
		((memq prop '(indexOf lastIndexOf charCodeAt charAt
			      substring substr
			      toLowerCase ))
		 ;; (tprint "JS_GET_PROTO: " prop " " (typeof prop))
		 (with-access::JsGlobalObject %this (js-string)
		    (let ((proto (js-get js-string 'prototype %this)))
		       (js-get proto prop %this))))
		(else
		 ;; see js-get-jsobject@property.scm
		 ;; (tprint "JS_GET_STRING: " prop " " (typeof prop))
		 (let* ((obj (js-toobject %this o))
			(pval (js-get-property-value obj o prop %this)))
		    (if (eq? pval (js-absent))
			(js-undefined)
			pval))))))))

;*---------------------------------------------------------------------*/
;*    js-put-string! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-put-string! _o::bstring prop v throw %this)
   (let ((o (js-toobject %this _o)))
      (js-put! o prop v throw %this)))

;*---------------------------------------------------------------------*/
;*    js-put! ::JsStringLiteral ...                                    */
;*---------------------------------------------------------------------*/
(define-method (js-put! _o::JsStringLiteral prop v throw %this)
   (let ((o (js-toobject %this _o)))
      (js-put! o prop v throw %this)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-indexof ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.7     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-indexof this search position %this)
   
   (define (ascii-indexof s::bstring)
      (if (not (isascii? search))
	  -1
	  (let* ((ulen (string-length s))
		 (pos (if (eq? position (js-undefined))
			  0
			  (js-tointeger position %this)))
		 (search (js-jsstring->string search))
		 (start (inexact->exact (min (max pos 0) ulen))))
	     (if (=fx (string-length search) 0)
		 -1
		 (let ((i start))
		    (if (<fx i 0)
			i
			(let ((j (string-char-index s (string-ref search 0) i)))
			   (if j
			       (let ((kt (bm-table search)))
				  (bm-string kt s j))
			       -1))))))))
   
   (define (utf8-indexof s::bstring)
      (let* ((ulen (utf8-string-length s))
	     (pos (if (eq? position (js-undefined))
		      0
		      (js-tointeger position %this)))
	     (search (js-jsstring->string search))
	     (start (inexact->exact (min (max pos 0) ulen))))
	 (if (=fx (string-length search) 0)
	     -1
	     (let ((i (utf8-string-index->string-index s start)))
		(if (<fx i 0)
		    i
		    (let ((j (string-char-index s (string-ref search 0) i)))
		       (if j
			   (let ((kt (bm-table search)))
			      (string-index->utf8-string-index s (bm-string kt s j)))
			   -1)))))))

   (string-call indexof this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-indexof ...                                    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-indexof this search position %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-indexof this (js-tostring search %this) position %this))
	 ((isa? this JsObject)
	  (js-call2 %this (js-get this 'indexOf %this) this search position))
	 (else
	  (loop (js-toobject %this this))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-lastindexof ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.8     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-lastindexof this search position %this)
   
   (define (ascii-lastindexof s)
      (if (not (isascii? search))
	  -1
	  (let* ((searchlen (string-length search))
		 (len (string-length s))
		 (numpos (js-tonumber position %this))
		 (pos (if (and (flonum? numpos) (nanfl? numpos))
			  (+ len 1)
			  (js-tointeger numpos %this)))
		 (start (inexact->exact (min (max pos 0) len))))
	     ;; utf-8 imposes a left-to-right parsing
	     (let loop ((i 0)
			(u 0)
			(r -1))
		(cond
		   ((or (=fx i len) (>fx u start))
		    r)
		   ((substring-at? s search i)
		    (loop (+fx searchlen i) (+fx u searchlen) u))
		   (else
		    (let ((c (string-ref s i)))
		       (loop (+fx i 1) (+fx u 1) r))))))))
   
   (define (utf8-lastindexof s)
      (let* ((searchlen (string-length search))
	     (usearchlen (utf8-string-length search))
	     (len (string-length s))
	     (ulen (utf8-string-length s))
	     (numpos (js-tonumber position %this))
	     (pos (if (and (flonum? numpos) (nanfl? numpos))
		      (+ ulen 1)
		      (js-tointeger numpos %this)))
	     (start (inexact->exact (min (max pos 0) ulen))))
	 ;; utf-8 imposes a left-to-right parsing
	 (let loop ((i 0)
		    (u 0)
		    (r -1))
	    (cond
	       ((or (=fx i len) (>fx u start))
		r)
	       ((substring-at? s search i)
		(loop (+fx searchlen i) (+fx u usearchlen) u))
	       (else
		(let ((c (string-ref s i)))
		   (loop (+fx i (utf8-char-size c)) (+fx u 1) r)))))))

   (string-call lastindexof this))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-lastindexof ...                                */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-lastindexof this search position %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-lastindexof this (js-tostring search %this) position %this))
	 ((isa? this JsObject)
	  (js-call2 %this (js-get this 'lastIndexOf %this) this search position))
	 (else
	  (loop (js-toobject %this this))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-charcodeat ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.5     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-charcodeat this position %this)
   
   (define (ascii-charcodeat val::bstring)
      (if (fixnum? position)
	  (cond
	     ((<fx position 0)
	      +nan.0)
	     ((>=fx position (string-length val))
	      +nan.0)
	     (else
	      (char->integer (string-ref-ur val position))))
	  (let ((pos (js-tointeger position %this)))
	     (if (or (< pos 0) (>= pos (string-length val)))
		 +nan.0
		 (char->integer (string-ref val (->fixnum pos)))))))
   
   (define (utf8-charcodeat val::bstring)
      (if (fixnum? position)
	  (cond
	     ((<fx position 0)
	      +nan.0)
	     ((>=fx position (utf8-codeunit-length val))
	      +nan.0)
	     ((ascii-string? val)
	      (char->integer (string-ref-ur val position)))
	     (else
	      (utf8-codeunit-ref val position)))
	  (let ((pos (js-tointeger position %this)))
	     (if (or (< pos 0) (>= pos (utf8-codeunit-length val)))
		 +nan.0
		 (utf8-codeunit-ref val (->fixnum pos))))))

   (string-call charcodeat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-charcodeat ...                                 */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-charcodeat this index %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-charcodeat this index %this))
	 ((isa? this JsObject)
	  (js-call1 %this (js-get this 'charCodeAt %this) this index))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-charat ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.4     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-charat this position %this)

   (define (ascii-charat val::bstring)
      (if (fixnum? position)
	  (cond
	     ((<fx position 0)
	      (js-ascii->jsstring ""))
	     ((>=fx position (string-length val))
	      (js-ascii->jsstring ""))
	     (else
	      (js-ascii->jsstring (make-string 1 (string-ref val position)))))
	  (let ((pos (js-tointeger position %this)))
	     (if (or (< pos 0) (>= pos (utf8-codeunit-length val)))
		 (js-ascii->jsstring "")
		 (js-ascii->jsstring
		    (make-string 1 (string-ref val (->fixnum pos))))))))

   (define (utf8-charat val::bstring)
      (if (fixnum? position)
	  (cond
	     ((<fx position 0)
	      (js-ascii->jsstring ""))
	     ((>=fx position (utf8-codeunit-length val))
	      (js-ascii->jsstring ""))
	     (else
	      (js-utf8-ref val position)))
	  (let ((pos (js-tointeger position %this)))
	     (if (or (< pos 0) (>= pos (utf8-codeunit-length val)))
		 (js-ascii->jsstring "")
		 (js-utf8-ref val (->fixnum pos))))))

   (string-call charat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-charat ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-charat this index %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-charat this index %this))
	 ((isa? this JsObject)
	  (js-call1 %this (js-get this 'charAt %this) this index))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-substring ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.15    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-substring this start end %this)
   
   (define (ascii-substr s)
      (let* ((len (string-length s))
	     (intstart (js-tointeger start %this))
	     (intend (if (eq? end (js-undefined)) len (js-tointeger end %this)))
	     (finalstart (->fixnum (min (max intstart 0) len)))
	     (finalend (->fixnum (min (max intend 0) len)))
	     (from (minfx finalstart finalend))
	     (to (maxfx finalstart finalend)))
	 (js-ascii->jsstring (substring s from to))))
   
   (define (utf8-substr s)
      (let* ((len (utf8-string-length s))
	     (intstart (js-tointeger start %this))
	     (intend (if (eq? end (js-undefined)) len (js-tointeger end %this)))
	     (finalstart (->fixnum (min (max intstart 0) len)))
	     (finalend (->fixnum (min (max intend 0) len)))
	     (from (minfx finalstart finalend))
	     (to (maxfx finalstart finalend)))
	 (js-string->jsstring (utf8-substring s from to))))
   
   (string-call substr this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-substring ...                                  */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-substring this start end %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-substring this start end %this))
	 ((isa? this JsObject)
	  (js-call2 %this (js-get this 'substring %this) this start end))
	 (else
	  (loop (js-toobject %this this))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-substr ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.3        */
;*---------------------------------------------------------------------*/
(define (js-jsstring-substr this start length %this)
   
   (define (ascii-substr r1)
      (let* ((r2 (js-tointeger start %this))
	     (r3 (if (eq? length (js-undefined))
		     (maxvalfx)
		     (js-tointeger length %this)))
	     (r4 (string-length r1))
	     (r5 (if (>=fx r2 0) r2 (maxfx (+fx r4 r2) 0)))
	     (r6 (minfx (maxfx r3 0) (-fx r4 r5))))
	 (if (<=fx r6 0)
	     (js-ascii->jsstring "")
	     (js-ascii->jsstring (substring r1 r5 (+fx r5 r6))))))
   
   (define (utf8-substr r1)
      (let* ((r2 (js-tointeger start %this))
	     (r3 (if (eq? length (js-undefined))
		     (maxvalfx)
		     (js-tointeger length %this)))
	     (r4 (utf8-string-length r1))
	     (r5 (if (>=fx r2 0) r2 (maxfx (+fx r4 r2) 0)))
	     (r6 (minfx (maxfx r3 0) (-fx r4 r5))))
	 (if (<=fx r6 0)
	     (js-ascii->jsstring "")
	     (js-string->jsstring (utf8-substring r1 r5 (+fx r5 r6))))))
   
   (string-call substr (js-jsstring->string this)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-substr ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-substr this start length %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-substr this start length %this))
	 ((isa? this JsObject)
	  (js-call2 %this (js-get this 'substr %this) this start length))
	 (else
	  (loop (js-toobject %this this))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-tolowercase ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.16    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-tolowercase this)

   (define (ascii-tolowercase s)
      (js-ascii->jsstring (string-downcase s)))

   (define (utf8-tolowercase s)
      (js-utf8->jsstring
	 (ucs2-string->utf8-string
	    (ucs2-string-downcase (utf8-string->ucs2-string s)))))

   (string-call tolowercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-tolowercase ...                                */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-tolowercase this %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-tolowercase this))
	 ((isa? this JsObject)
	  (js-call0 %this (js-get this 'toLowerCase %this) this))
	 (else
	  (loop (js-toobject %this this))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-touppercase ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.18    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-touppercase this)
   
   (define (ascii-touppercase s)
      (js-ascii->jsstring (string-upcase s)))

   (define (utf8-touppercase s)
      (js-string->jsstring
	 (ucs2-string->utf8-string
	    (ucs2-string-upcase (utf8-string->ucs2-string s)))))

   (string-call touppercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-touppercase ...                                */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-touppercase this %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-touppercase this))
	 ((isa? this JsObject)
	  (js-call0 %this (js-get this 'toUpperCase %this) this))
	 (else
	  (loop (js-toobject %this this))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-split ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.14    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-split this::obj separator limit %this)
   
   (define (minelong2::elong n1::elong n2::elong)
      (if (<elong n1 n2) n1 n2))
   
   (define (split-match S::bstring q R)
      (if (isa? R JsRegExp)
	  (with-access::JsRegExp R (rx)
	     (or (pregexp-match-positions rx S q) 'failure))
	  (let ((r (string-length R))
		(s (string-length S)))
	     (cond
		((>fx (+fx q r) s) 'failure)
		((substring-at? S R q) (list (cons q (+fx q r))))
		(else 'failure)))))
   
   (with-access::JsGlobalObject %this (js-array)
      (let* ((jsS this)
	     (S (js-jsstring->string jsS))
	     (A (js-new %this js-array 0))
	     (lim (if (eq? limit (js-undefined))
		      (+fx (string-length S) 1)
		      (elong->fixnum
			 (minelong2
			    (uint32->elong (js-touint32 limit %this))
			    (fixnum->elong (+fx 1 (string-length S)))))))
	     (s (string-length S))
	     (p 0)
	     (R (if (isa? separator JsRegExp)
		    separator
		    (js-tostring separator %this))))
	 (cond
	    ((=fx lim 0)
	     ;; 9
	     A)
	    ((eq? separator (js-undefined))
	     ;; 10
	     (js-define-own-property A 0
		(instantiate::JsValueDescriptor
		   (name (js-toname 0 %this))
		   (value jsS)
		   (writable #t)
		   (enumerable #t)
		   (configurable #t))
		#f %this)
	     A)
	    ((=fx s 0)
	     ;; 11
	     (let ((z (split-match S 0 R)))
		(when (eq? z 'failure)
		   (js-define-own-property A 0
		      (instantiate::JsValueDescriptor
			 (name (js-toname 0 %this))
			 (value jsS)
			 (writable #t)
			 (enumerable #t)
			 (configurable #t))
		      #f %this))
		A))
	    (else
	     ;; 13
	     (let loop ((q p)
			(p p))
		(if (not (=fx q s))
		    (let ((z (split-match S q R)))
		       (if (eq? z 'failure)
			   (loop (+fx q (utf8-char-size (string-ref S q))) p)
			   ;; 13.c.i
			   (let ((e (cdar z))
				 (q (caar z))
				 (cap (cdr z)))
			      (if (=fx e p)
				  ;; 13.c.ii
				  (loop (+fx q (utf8-char-size (string-ref S q))) p)
				  ;; 13.c.iii.1
				  (let ((T (substring S p q))
					(l (->fixnum (js-get A 'length %this))))
				     ;; 13.c.iii.2
				     (js-define-own-property A l
					(instantiate::JsValueDescriptor
					   (name (js-toname l %this))
					   (value (js-string->jsstring T))
					   (writable #t)
					   (enumerable #t)
					   (configurable #t))
					#f %this)
				     (if (=fx (+fx l 1) lim)
					 ;; 13.c.iii.4
					 A
					 ;; 13.c.iii.5
					 (let ((p e))
					    (let repeat ((cap cap)
							 (l (+fx l 1)))
					       (if (pair? cap)
						   (begin
						      ;; 13.c.iii.7.b
						      (js-define-own-property A l
							 (instantiate::JsValueDescriptor
							    (name (js-toname l %this))
							    (value (car cap))
							    (writable #t)
							    (enumerable #t)
							    (configurable #t))
							 #f %this)
						      (if (=fx (+fx l 1) lim)
							  ;; 13.c.iii.7.d
							  A
							  ;; 13.c.iii.8
							  (repeat (cdr cap) (+fx l 1))))
						   (loop p e))))))))))
		    ;; 14
		    (let ((T (substring S p s))
			  (l (js-get A 'length %this)))
		       ;; 15
		       (js-define-own-property A l
			  (instantiate::JsValueDescriptor
			     (name (js-toname l %this))
			     (value (js-string->jsstring T))
			     (writable #t)
			     (enumerable #t)
			     (configurable #t))
			  #f %this)
		       ;;16
		       A))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-split ...                                      */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-split this separator limit %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-split this separator limit %this))
	 ((isa? this JsObject)
	  (js-call2 %this (js-get this 'split %this) this separator limit))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-replace ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-replace this::obj searchvalue replacevalue %this)
   
   (define (digit->number c)
      (-fx (char->integer c) (char->integer #\0)))
   
   (define (digit10->number c1 c2)
      (+fx (*fx (digit->number c1) 10) (digit->number c2)))
   
   (define (table22::pair-nil fmt::bstring match string::bstring init::bstring)
      (let ((stop (-fx (string-length fmt) 1)))
	 (let loop ((i 0)
		    (j 0)
		    (segments '()))
	    (cond
	       ((>=fx i stop)
		(reverse (cons* init (substring fmt j (+fx stop 1)) segments)))
	       ((not (char=? (string-ref fmt i) #\$))
		(loop (+fx i 1) j segments))
	       (else
		(let ((segments (cons (substring fmt j i) segments)))
		   (case (string-ref fmt (+fx i 1))
		      ((#\$)
		       (loop (+fx i 2) (+fx i 2)
			  (cons "$" segments)))
		      ((#\&)
		       (let ((seg (js-get match (js-toname 0 %this) %this)))
			  (loop (+fx i 2) (+fx i 2)
			     (cons (js-jsstring->string seg) segments))))
		      ((#\`)
		       (let* ((k (js-get match 'index %this))
			      (portion (substring string 0 k)))
			  (loop (+fx i 2) (+fx i 2)
			     (cons portion segments))))
		      ((#\')
		       (let* ((k (js-get match 'index %this))
			      (s (js-get match (js-toname 0 %this) %this))
			      (l (js-jsstring-length s))
			      (portion (substring string (+fx k l))))
			  (loop (+fx i 2) (+fx i 2)
			     (cons portion segments))))
		      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8\ #\9)
		       (let ((len (-fx (js-get match 'length %this) 1)))
			  (if (or (=fx i (-fx stop 1))
				  (not (char-numeric? (string-ref fmt (+fx i 2)))))
			      (let ((n (digit->number (string-ref fmt (+fx i 1)))))
				 (if (>fx n len)
				     (loop (+fx i 2) j segments)
				     (let ((s (js-get match n %this)))
					(loop (+fx i 2) (+fx i 2)
					   (if (js-jsstring? s)
					       (cons (js-jsstring->string s)
						  segments)
					       segments)))))
			      (let ((n (digit10->number
					  (string-ref fmt (+fx i 1))
					  (string-ref fmt (+fx i 2)))))
				 (if (>fx n len)
				     (let ((n (digit->number (string-ref fmt (+fx i 1)))))
					(if (>=fx n len)
					    (loop (+fx i 3) j segments)
					    (let ((s (js-get match n %this)))
					       (loop (+fx i 2) (+fx i 2)
						  (if (js-jsstring? s)
						      (cons (js-jsstring->string s)
							 segments)
						      segments)))))
				     (let ((s (js-get match n %this)))
					(loop (+fx i 3) (+fx i 3)
					   (if (js-jsstring? s)
					       (cons (js-jsstring->string s)
						  segments)
					       segments))))))))
		      (else
		       (loop (+fx i 2) j segments)))))))))
   
   (define (matches->string-list::pair-nil a)
      (let ((len (js-get a 'length %this)))
	 (let loop ((i 1)
		    (l '()))
	    (if (=fx i len)
		(reverse! l)
		(let ((v (js-get a (js-toname i %this) %this)))
		   (loop (+fx i 1)
		      (cons (if (eq? v (js-undefined)) (js-string->jsstring "") v)
			 l)))))))
   
   (with-access::JsGlobalObject %this (js-regexp js-array)
      (let ((string (js-jsstring->string this)))
	 (cond
	    ((not (isa? searchvalue JsRegExp))
	     (let* ((searchstr (js-tojsstring searchvalue %this))
		    (i (string-contains (js-jsstring->string string)
			  (js-jsstring->string searchstr) 0)))
		(cond
		   ((not i)
		    string)
		   ((isa? replacevalue JsFunction)
		    (let ((str (js-jsstring->string string)))
		       (js-stringlist->jsstring
			  (list
			     (substring str 0 i)
			     (js-tostring
				(js-call3 %this replacevalue (js-undefined)
				   searchstr i string) %this)
			     (substring str
				(+fx i (js-jsstring-length searchstr)))))))
		   (else
		    (let ((newstring (js-tostring replacevalue %this))
			  (a (js-new %this js-array 1))
			  (str (js-jsstring->string string)))
		       (js-put! a 'input string #f %this)
		       (js-put! a (js-toname 0 %this) searchstr #f %this)
		       (js-stringlist->jsstring
			  (cons (substring str 0 i)
			     (table22 newstring a str
				(substring str (+fx i (js-jsstring-length searchstr)))))))))))
	    ((not (js-get searchvalue 'global %this))
	     (let* ((exec (js-get (js-get js-regexp 'prototype %this)
			     'exec %this))
		    (res (js-call1 %this exec searchvalue string)))
		(cond
		   ((eq? res (js-null))
		    string)
		   ((isa? replacevalue JsFunction)
		    (let ((i (js-get res 'index %this))
			  (str (js-jsstring->string string)))
		       (js-stringlist->jsstring
			  (list
			     (substring str 0 i)
			     (js-tostring
				(js-apply %this replacevalue (js-undefined)
				   (cons (js-get res (js-toname 0 %this) %this)
				      (append (matches->string-list res)
					 (list i str))))
				%this)
			     (substring str
				(+fx i (js-jsstring-length (js-get res (js-toname 0 %this) %this))))))))
		   (else
		    (let ((newstring (js-tostring replacevalue %this))
			  (i (js-get res 'index %this))
			  (str (js-jsstring->string string)))
		       (js-stringlist->jsstring 
			  (cons (substring str 0 i)
			     (table22 newstring res str
				(substring str
				   (+fx i (js-jsstring-length (js-get res (js-toname 0 %this) %this))))))))))))
	    (else
	     (let* ((previousLastIndex 0)
		    (exec (js-get (js-get js-regexp 'prototype %this)
			     'exec %this)))
		(js-put! searchvalue 'lastIndex 0 #f %this)
		(let loop ((n 0)
			   (ms '()))
		   (let ((result (js-call1 %this exec searchvalue string)))
		      (if (eq? result (js-null))
			  (cond
			     ((null? ms)
			      string)
			     ((isa? replacevalue JsFunction)
			      (let loop ((matches (reverse! ms))
					 (res string)
					 (offset 0))
				 (if (null? matches)
				     res
				     (let* ((m (car matches))
					    (i (js-get m 'index %this))
					    (l (js-jsstring-length (js-get m (js-toname 0 %this) %this)))
					    (v (js-tostring
						  (js-apply %this replacevalue
						     (js-undefined)
						     (cons
							(js-get m (js-toname 0 %this) %this)
							(append (matches->string-list m)
							   (list i string))))
						  %this)))
					(let ((str (js-jsstring->string res)))
					   (loop (cdr matches)
					      (js-stringlist->jsstring
						 (list (substring str 0 (+fx offset i))
						    v
						    (substring str (+fx offset (+fx i l)))))
					      (+fx offset (-fx (string-length v) l))))))))
			     (else
			      (let ((newstring (js-tostring replacevalue %this))
				    (str (js-jsstring->string string)))
				 (let loop ((matches (reverse! ms))
					    (res string)
					    (offset 0))
				    (if (null? matches)
					res
					(let* ((m (car matches))
					       (i (js-get m 'index %this))
					       (l (js-jsstring-length (js-get m (js-toname 0 %this) %this)))
					       (sres (js-jsstring->string res))
					       (v (apply utf8-string-append* (table22 newstring m str ""))))
					   (loop (cdr matches)
					      (js-stringlist->jsstring
						 (list (substring sres 0 (+fx offset i))
						    v
						    (substring sres (+fx offset (+fx i l)))))
					      (+fx offset (-fx (string-length v) l)))))))))
			  (let ((thisIndex (js-get searchvalue 'lastIndex %this)))
			     (if (= thisIndex previousLastIndex)
				 (begin
				    (js-put! searchvalue 'lastIndex (+ thisIndex 1) #f %this)
				    (set! previousLastIndex (+ 1 thisIndex)))
				 (set! previousLastIndex thisIndex))
			     (let ((matchStr (js-get result 0 %this)))
				(loop (+fx 1 n) (cons result ms)))))))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-replace ...                                    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-replace this searchvalue replacevalue %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-replace this searchvalue replacevalue %this))
	 ((isa? this JsObject)
	  (js-call2 %this (js-get this 'replace %this) this
	     searchvalue replacevalue))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-match ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.10    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-match this regexp %this)
   (with-access::JsGlobalObject %this (js-regexp js-array)
      (let* ((s (js-jsstring->string this))
	     (rx (if (isa? regexp JsRegExp)
		     regexp
		     (js-new %this js-regexp regexp)))
	     (exec (js-get (js-get js-regexp 'prototype %this) 'exec %this))
	     (global (js-get rx 'global %this)))
	 ;; 7
	 (if (not global)
	     (js-call1 %this exec rx s)
	     ;; 8
	     (let ((previousLastIndex 0)
		   (a (js-null)))
		(js-put! rx 'lastIndex 0  #f %this)
		(let loop ((n 0))
		   (let ((result (js-call1 %this exec rx s)))
		      (if (eq? result (js-null))
			  a
			  (let ((thisIndex (js-get rx 'lastIndex %this)))
			     (if (= thisIndex previousLastIndex)
				 (begin
				    (js-put! rx 'lastIndex
				       (+ thisIndex 1) #f %this)
				    (set! previousLastIndex (+ 1 thisIndex)))
				 (set! previousLastIndex thisIndex))
			     (when (eq? a (js-null))
				(set! a (js-new %this js-array 1)))
			     (let ((matchStr (js-get result 0 %this)))
				(js-define-own-property a n
				   (instantiate::JsValueDescriptor
				      (name (js-toname n %this))
				      (value matchStr)
				      (writable #t)
				      (enumerable #t)
				      (configurable #t))
				   #f %this))
			     (loop (+fx 1 n)))))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-match ...                                      */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-match this regexp %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-match this regexp %this))
	 ((isa? this JsObject)
	  (js-call1 %this (js-get this 'match %this) this regexp))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-naturalcompare ...                                   */
;*---------------------------------------------------------------------*/
(define (js-jsstring-naturalcompare this that %this)
   (let ((s (js-jsstring->string this))
	 (t (js-tostring that %this)))
      (string-natural-compare3 s t)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-naturalcompare ...                             */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-naturalcompare this that %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-naturalcompare this that %this))
	 ((isa? this JsObject)
	  (js-call1 %this (js-get this 'naturalCompare %this) this that))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-localecompare ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.9     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-localecompare this that %this)
   (let ((s (js-jsstring->string this))
	 (t (js-tostring that %this)))
      (utf8-string-locale-compare3 s t)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-localecompare ...                              */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-localecompare this that %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-localecompare this that %this))
	 ((isa? this JsObject)
	  (js-call1 %this (js-get this 'localeCompare %this) this that))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-trim ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.20    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-trim this)
   (js-string->jsstring
      (trim-whitespaces+ (js-jsstring->string this) :left #t :right #t)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-trim ...                                       */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-trim this %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-trim this))
	 ((isa? this JsObject)
	  (js-call0 %this (js-get this 'trim %this) this))
	 (else
	  (loop (js-toobject %this this))))))



	 
   
