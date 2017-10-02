;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/stringliteral.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 21 14:13:28 2014                          */
;*    Last change :  Mon Oct  2 19:23:38 2017 (serrano)                */
;*    Copyright   :  2014-17 Manuel Serrano                            */
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
	   (js-jsstring-character-ref ::obj ::uint32)
	   (js-jsstring-ref ::obj ::uint32)
	   (js-jsstring-length::uint32 ::obj)
	   (inline js-jsstring-lengthfx::long ::obj)
	   (js-jsstring-character-length::uint32 ::obj)
	   (js-jsstring-codeunit-length::uint32 ::obj)
	   (inline js-jsstring-null? ::obj)
	   (inline js-jsstring=?::bool ::obj ::obj)
	   (inline js-jsstring<?::bool ::obj ::obj)
	   (inline js-jsstring<=?::bool ::obj ::obj)
	   (inline js-jsstring>?::bool ::obj ::obj)
	   (inline js-jsstring>=?::bool ::obj ::obj)
	   (js-string->number::obj ::bstring ::JsGlobalObject)
	   (js-integer->jsstring ::long)
	   (js-jsstring->bool::bool ::obj)
	   (generic js-jsstring-normalize!::JsStringLiteral ::JsStringLiteral)
	   
	   (js-jsstring-normalize-ASCII! ::JsStringLiteral)
	   (js-jsstring-normalize-UTF8! ::JsStringLiteral)
	   (inline js-jsstring-append::JsStringLiteral ::obj ::obj)
	   (utf8-codeunit-length::long ::bstring)
	   (js-utf8-ref ::JsStringLiteralUTF8 ::bstring ::long)
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
	   (js-jsstring-tolocalelowercase ::obj)
	   (js-jsstring-maybe-tolocalelowercase ::obj ::JsGlobalObject)
	   (js-jsstring-touppercase ::obj)
	   (js-jsstring-maybe-touppercase ::obj ::JsGlobalObject)
	   (js-jsstring-tolocaleuppercase ::obj)
	   (js-jsstring-maybe-tolocaleuppercase ::obj ::JsGlobalObject)
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
	   (js-jsstring-fromcharcode ::JsObject ::obj ::JsGlobalObject)
	   (js-jsstring-escape ::obj)
	   (js-jsstring-unescape ::obj ::JsGlobalObject))

   (cond-expand
      ((not bigloo4.3a)
       (pragma (js-string->jsstring default-inline)))))

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
;*    prealloc-strings ...                                             */
;*---------------------------------------------------------------------*/
(define prealloc-strings
   (let ((vec (make-vector 256)))
      (let loop ((i 0))
	 (when (<=fx i 127)
	    (vector-set! vec i
	       (js-ascii->jsstring
		  (make-string 1 (integer->char i))))
	    (loop (+fx i 1))))
      (let loop ((i 128))
	 (when (<=fx i 255)
	    (vector-set! vec i
	       (js-utf8->jsstring
		  (ucs2-string->utf8-string
		     (make-ucs2-string 1 (integer->ucs2 i)))))
	    (loop (+fx i 1))))
      vec))

;*---------------------------------------------------------------------*/
;*    string-dipatch ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (string-dipatch fun this . args)
   `(cond
       ((string? ,this)
	(,(symbol-append 'ascii- fun) ,this ,@args))
       ((isa? ,this JsStringLiteralASCII)
	(,(symbol-append 'ascii- fun) (js-jsstring-normalize-ASCII! ,this) ,@args))
       (else
	(,(symbol-append 'utf8- fun) (js-jsstring-normalize-UTF8! ,this) ,@args))))

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
   (let ((enc (string-minimal-charset val)))
      (case enc
	 ((ascii) val)
	 ((latin1 utf8) (js-utf8->jsstring val))
	 (else (error "string->jsstring" "unsupported encoding" enc)))))

;*---------------------------------------------------------------------*/
;*    js-stringlist->jsstring ...                                      */
;*    -------------------------------------------------------------    */
;*    Create a JsStringLiteral from a list of Scheme string literals.  */
;*---------------------------------------------------------------------*/
(define (js-stringlist->jsstring  val::pair-nil)
   (cond
      ((null? val)
       (js-ascii->jsstring ""))
      ((null? (cdr val))
       (js-string->jsstring (car val)))
      ((null? (cddr val))
       (js-jsstring-append
	  (js-string->jsstring (car val))
	  (js-string->jsstring (cadr val))))
      (else
       (let* ((str (car val))
	      (res (instantiate::JsStringLiteralUTF8
		      (left str)
		      (weight (string-length str)))))
	  (let loop ((val (cdr val))
		     (parent res))
	     (if (null? val)
		 res
		 (with-access::JsStringLiteral parent (right)
		    (let* ((str (car val))
			   (child (instantiate::JsStringLiteralUTF8
				     (left (car val))
				     (weight (string-length str)))))
		       (set! right child)
		       (loop (cdr val) child)))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring->string ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring->string::bstring js::obj)
   (cond
      ((string? js) js)
      ((isa? js JsStringLiteralASCII) (js-jsstring-normalize-ASCII! js))
      ((isa? js JsStringLiteralUTF8) (js-jsstring-normalize-UTF8! js))
      (else
       (js-jsstring-normalize! js)
       (with-access::JsStringLiteral js (left) left))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize-ASCII! ...                                 */
;*---------------------------------------------------------------------*/
(define (js-jsstring-normalize-ASCII! js::JsStringLiteral)
   (with-access::JsStringLiteral js (left right weight)
      (if (not right)
	  left
	  (let ((buffer (make-string
			   (uint32->fixnum (js-string-literal-length js)))))
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
;*    js-jsstring-normalize-UTF8! ...                                  */
;*---------------------------------------------------------------------*/
(define (js-jsstring-normalize-UTF8! js::JsStringLiteral)
   (with-access::JsStringLiteralUTF8 js (left right weight %idxutf8 %idxstr)
      (if (and (string? left) (not right))
	  left
	  (let ((buffer (make-string
			   (uint32->fixnum (js-string-literal-length js)))))
	     (let loop ((i 0)
			(stack (list js)))
		(cond
		   ((null? stack)
		    (set! weight i)
		    (set! left (string-shrink! buffer i))
		    (set! right #f)
		    (set! %idxutf8 0)
		    (set! %idxstr 0)
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

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize! ...                                       */
;*    -------------------------------------------------------------    */
;*    Tailrec normalization (with explicit stack management).          */
;*---------------------------------------------------------------------*/
(define-generic (js-jsstring-normalize!::JsStringLiteral js::JsStringLiteral))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize! ...                                       */
;*    -------------------------------------------------------------    */
;*    Tailrec normalization (with explicit stack management).          */
;*---------------------------------------------------------------------*/
(define-method (js-jsstring-normalize!::JsStringLiteral js::JsStringLiteralASCII)
   (js-jsstring-normalize-ASCII! js)
   js)

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize! ...                                       */
;*    -------------------------------------------------------------    */
;*    Tailrec normalization (with explicit stack management).          */
;*---------------------------------------------------------------------*/
(define-method (js-jsstring-normalize!::JsStringLiteral js::JsStringLiteralUTF8)
   (js-jsstring-normalize-UTF8! js)
   js)

;*---------------------------------------------------------------------*/
;*    js-string-literal-length ...                                     */
;*---------------------------------------------------------------------*/
(define (js-string-literal-length::uint32 js::JsStringLiteral)
   (let loop ((len #u32:0)
	      (js js))
      (cond
	 ((string? js)
	  (+u32 len (fixnum->uint32 (string-length js))))
	 ((not js)
	  len)
	 (else
	  (with-access::JsStringLiteral js (weight right)
	     (loop (+u32 len weight) right))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-length ...                                           */
;*    -------------------------------------------------------------    */
;*    Returns the number of scheme caracters (i.e., bytes) forming the */
;*    string. For an UTF* string, this number differs from the number  */
;*    of letters of that string.                                       */
;*---------------------------------------------------------------------*/
(define (js-jsstring-length js)
   (if (string? js)
       (fixnum->uint32 (string-length js))
       (js-string-literal-length js)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-lengthfx ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-lengthfx js)
   (uint32->fixnum (js-jsstring-length js)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-character-length ...                                 */
;*    -------------------------------------------------------------    */
;*    Returns the number of unicode characters of that strings.        */
;*---------------------------------------------------------------------*/
(define (js-jsstring-character-length js)
   (cond
      ((string? js)
       (fixnum->uint32 (string-length js)))
      ((isa? js JsStringLiteralASCII)
       (with-access::JsStringLiteralASCII js (right left weight)
	  (if (not right)
	      weight
	      (js-string-literal-length js))))
      (else
       (fixnum->uint32 (utf8-string-length (js-jsstring->string js))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-codeunit-length ...                                  */
;*    -------------------------------------------------------------    */
;*    This function implements the JavaScript string length, which     */
;*    counts the number of UCS2 characters of the string.              */
;*---------------------------------------------------------------------*/
(define (js-jsstring-codeunit-length js)
   (cond
      ((string? js)
       (fixnum->uint32 (string-length js)))
      ((isa? js JsStringLiteralASCII)
       (with-access::JsStringLiteralASCII js (right left weight)
	  (if (not right)
	      weight
	      (js-string-literal-length js))))
      (else
       (with-access::JsStringLiteralUTF8 js (%culen)
	  (when (=u32 %culen #u32:0)
	     (set! %culen (utf8-codeunit-length (js-jsstring->string js))))
	  %culen))))

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
       (js-ascii->jsstring (integer->string num))
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
(define (codepoint-length c)
   (case (char->integer c)
      ((#xf0 #xf4 #xf8 #xfc) 2)
      (else 1)))

;*---------------------------------------------------------------------*/
;*    utf8-string-codeunit-ref ...                                     */
;*    -------------------------------------------------------------    */
;*    Returns the ith code unit (UTF16 code unit) of the UTF8 source   */
;*    string.                                                          */
;*---------------------------------------------------------------------*/
(define (utf8-codeunit-ref this::JsStringLiteralUTF8 str i::long)
   (let ((len (string-length str)))
      (with-access::JsStringLiteralUTF8 this (%idxutf8 %idxstr)
	 ;; adjust with respect to the last position
	 (let loop ((r (if (>=fx i %idxutf8) %idxstr 0))
		    (j (if (>=fx i %idxutf8) (-fx i %idxutf8) i)))
	    (if (>=fx r len)
		+nan.0
		(let* ((c (string-ref-ur str r))
		       (s (utf8-char-size c))
		       (u (codepoint-length c)))
		   (cond
		      ((>=fx j u)
		       (loop (+fx r s) (-fx j u)))
		      (else
		       (set! %idxutf8 (-fx i j))
		       (set! %idxstr r)
		       (cond
			  ((=fx s 1)
			   (char->integer c))
			  ((char=? c (integer->char #xf8))
			   (utf8-left-replacement-codeunit str r))
			  ((char=? c (integer->char #xfc))
			   (utf8-right-replacement-codeunit str r))
			  (else
			   (let* ((utf8 (substring str r (+fx r s)))
				  (ucs2 (utf8-string->ucs2-string utf8)))
			      (ucs2->integer (ucs2-string-ref ucs2 j)))))))))))))

;*---------------------------------------------------------------------*/
;*    js-utf8-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-utf8-ref this::JsStringLiteralUTF8 str::bstring index::long)
   (let ((n (utf8-codeunit-ref this str index)))
      (if (<=fx n 255)
	  (vector-ref prealloc-strings n)
	  (js-utf8->jsstring
	     (ucs2-string->utf8-string
		(ucs2-string
		   (integer->ucs2 n)))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-ref ...                                              */
;*---------------------------------------------------------------------*/
(define (js-jsstring-ref o index::uint32)
   
   (define (ascii-string-ref val fxpos)
      (if (>=fx fxpos (string-length val))
	  (js-undefined)
	  (vector-ref prealloc-strings
	     (char->integer (string-ref-ur val fxpos)))))
   
   (define (utf8-string-ref val fxpos)
      (if (>=fx fxpos (utf8-codeunit-length val))
	  (js-undefined)
	  (js-utf8-ref o val fxpos)))

   (string-dipatch string-ref o (uint32->fixnum index)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-character-ref ...                                    */
;*    -------------------------------------------------------------    */
;*    Returns a unicode character (not a ucs2 char), used by           */
;*    string iterator.                                                 */
;*---------------------------------------------------------------------*/
(define (js-jsstring-character-ref o index::uint32)
   
   (define (ascii-string-character-ref val fxpos)
      (if (>=fx fxpos (string-length val))
	  (js-undefined)
	  (js-ascii->jsstring (make-string 1 (string-ref-ur val fxpos)))))
   
   (define (utf8-string-character-ref val fxpos)
      (if (>=fx fxpos (utf8-codeunit-length val))
	  (js-undefined)
	  (utf8-string-ref val fxpos)))
   
   (string-dipatch string-character-ref o (uint32->fixnum index)))

;*---------------------------------------------------------------------*/
;*    js-get ::JsStringLiteral ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsStringLiteral prop %this)
   (js-get-string o prop %this))

;*---------------------------------------------------------------------*/
;*    js-get-string ...                                                */
;*---------------------------------------------------------------------*/
(define (js-get-string o prop %this)
   (if (eq? prop 'length)
       (uint32->fixnum (js-jsstring-codeunit-length o))
       (let ((i (js-toindex prop)))
	  (if (js-isindex? i)
	      (js-jsstring-ref o i)
	      (let ((p (js-toname prop %this)))
		 (cond
		    ((eq? prop 'length)
		     (uint32->fixnum (js-jsstring-codeunit-length o)))
		    ((memq prop '(indexOf lastIndexOf charCodeAt charAt
				  substring substr
				  toLowerCase toLocaleLowerCase
				  toUpperCase toLocaleUpperCase
				  split replace match
				  compare naturalCompare localeCompare trim))
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
			    pval)))))))))

;*---------------------------------------------------------------------*/
;*    js-get-length ::JsStringLiteral ...                              */
;*---------------------------------------------------------------------*/
(define-method (js-get-length o::JsStringLiteral cache %this)
   (uint32->fixnum (js-jsstring-codeunit-length o)))

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
   
   (define (ascii-indexof str::bstring)
      (let* ((pat (js-jsstring->string search))
	     (patlen (string-length pat))
	     (ulen (string-length str))
	     (n (-fx ulen patlen))
	     (c0 (string-ref pat 0))
	     (idx (if (eq? position (js-undefined))
		      0
		      (inexact->exact
			 (min (max (js-tointeger position %this) 0) ulen)))))
	 (let loop ((i idx)
		    (badness (-fx -10 (*fx patlen 4))))
	    (if (>fx i n)
		-1
		(let ((j (string-char-index-ur str c0 i (+fx 1 (-fx n i)))))
		   (if (not j)
		       -1
		       (let liip ((k 1))
			  (cond
			     ((=fx k patlen)
			      j)
			     ((char=? (string-ref pat k)
				 (string-ref str (+fx k j)))
			      (liip (+fx k 1)))
			     ((<fx badness 0)
			      (loop (+fx j 1) (+fx badness k)))
			     (else
			      (let ((t (bm-table pat)))
				 (bm-string t str j)))))))))))
   
   (define (utf8-indexof str::bstring)
      (let* ((pat (js-jsstring->string search))
	     (patlen (string-length pat))
	     (ulen (string-length str))
	     (n (-fx ulen patlen))
	     (c0 (string-ref pat 0))
	     (idx (if (eq? position (js-undefined))
		      0
		      (inexact->exact
			 (min (max (js-tointeger position %this) 0) ulen)))))
	 (let loop ((i (utf8-string-index->string-index str idx))
		    (badness (-fx -10 (*fx patlen 4))))
	    (if (>fx i n)
		-1
		(let ((j (string-char-index-ur str c0 i (+fx 1 (-fx n i)))))
		   (if (not j)
		       -1
		       (let liip ((k 1))
			  (cond
			     ((=fx k patlen)
			      (string-index->utf8-string-index str j))
			     ((char=? (string-ref pat k)
				 (string-ref str (+fx k j)))
			      (liip (+fx k 1)))
			     ((<fx badness 0)
			      (loop (+fx j 1) (+fx badness k)))
			     (else
			      (let ((t (bm-table pat)))
				 (string-index->utf8-string-index str
				    (bm-string t str j))))))))))))

   (string-dipatch indexof this))

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
	  (let* ((ulen (string-length s))
		 (numpos (js-tonumber position %this))
		 (pos (if (and (flonum? numpos) (nanfl? numpos))
		      ulen
		      (+ 1 (js-tointeger numpos %this))))
		 (search (js-jsstring->string search))
		 (start (inexact->exact (min (max pos 0) ulen))))
	     (if (=fx (string-length search) 0)
		 -1
		 (let loop ((i start))
		    (if (<fx i 0)
			-1
			(let ((j (string-index-right s (string-ref search 0) i)))
			   (if j
			       (if (substring-at? s search j)
				   j
				   (loop j))
			       -1))))))))
   
   (define (utf8-lastindexof s)
      (let* ((search (js-jsstring->string search))
	     (searchlen (string-length search))
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

   (string-dipatch lastindexof this))
   
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
	  (utf8-codeunit-ref this val position)
	  (let ((pos (js-tointeger position %this)))
	     (utf8-codeunit-ref this val (->fixnum pos)))))

   (string-dipatch charcodeat this))

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
;*    charat-table ...                                                 */
;*---------------------------------------------------------------------*/
(define charat-table
   (make-vector 128 #f))

;*---------------------------------------------------------------------*/
;*    js-jsstring-charat ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.4     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-charat this position %this)

   (define (ascii-charat/table val position)
      (let* ((c (string-ref-ur val position))
	     (i (char->integer c))
	     (o (vector-ref-ur charat-table i)))
	 (or o
	     (let ((ns (js-ascii->jsstring (make-string 1 c))))
		(vector-set-ur! charat-table i ns)
		ns))))
   
   (define (ascii-charat val::bstring)
      (if (fixnum? position)
	  (cond
	     ((<fx position 0)
	      (js-ascii->jsstring ""))
	     ((>=fx position (string-length val))
	      (js-ascii->jsstring ""))
	     (else
	      (ascii-charat/table val position)))
	  (let ((pos (js-tointeger position %this)))
	     (if (or (< pos 0) (>= pos (string-length val)))
		 (js-ascii->jsstring "")
		 (ascii-charat/table val (->fixnum pos))))))

   (define (utf8-charat val::bstring)
      (if (fixnum? position)
	  (cond
	     ((<fx position 0)
	      (js-ascii->jsstring ""))
	     ((>=fx position (utf8-codeunit-length val))
	      (js-ascii->jsstring ""))
	     (else
	      (js-utf8-ref this val position)))
	  (let ((pos (js-tointeger position %this)))
	     (if (or (< pos 0) (>= pos (utf8-codeunit-length val)))
		 (js-ascii->jsstring "")
		 (js-utf8-ref this val (->fixnum pos))))))

   (string-dipatch charat this))

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
   
   (string-dipatch substr this))

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
   
   (string-dipatch substr (js-jsstring->string this)))

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

   (string-dipatch tolowercase this))

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
;*    js-jsstring-tolocalelowercase ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.17    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-tolocalelowercase this)

   (define (ascii-tolowercase s)
      (js-ascii->jsstring (string-downcase s)))

   (define (utf8-tolowercase s)
      (js-utf8->jsstring (utf8-string-locale-downcase s)))

   (string-dipatch tolowercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-tolocalelowercase ...                          */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-tolocalelowercase this %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-tolocalelowercase this))
	 ((isa? this JsObject)
	  (js-call0 %this (js-get this 'toLocaleLowerCase %this) this))
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

   (string-dipatch touppercase this))

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
;*    js-jsstring-tolocaleuppercase ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.18    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-tolocaleuppercase this)

   (define (ascii-touppercase s)
      (js-ascii->jsstring (string-upcase s)))

   (define (utf8-touppercase s)
      (js-utf8->jsstring (utf8-string-locale-upcase s)))

   (string-dipatch touppercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-tolocaleuppercase ...                          */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-tolocaleuppercase this %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-tolocaleuppercase this))
	 ((isa? this JsObject)
	  (js-call0 %this (js-get this 'toLocaleUpperCase %this) this))
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
					(l (->fixnum (js-get-length A #f %this))))
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
			  (l (js-get-length A #f %this)))
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
			      (l (js-jsstring-lengthfx s))
			      (portion (substring string (+fx k l))))
			  (loop (+fx i 2) (+fx i 2)
			     (cons portion segments))))
		      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		       (let ((len (-fx (js-get-length match #f %this) 1)))
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
      (let ((len (js-get-length a #f %this)))
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
				(+fx i (js-jsstring-lengthfx searchstr)))))))
		   (else
		    (let ((newstring (js-tostring replacevalue %this))
			  (a (js-new %this js-array 1))
			  (str (js-jsstring->string string)))
		       (js-put! a 'input string #f %this)
		       (js-put! a (js-toname 0 %this) searchstr #f %this)
		       (js-stringlist->jsstring
			  (cons (substring str 0 i)
			     (table22 newstring a str
				(substring str (+fx i (js-jsstring-lengthfx searchstr)))))))))))
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
				(+fx i (js-jsstring-lengthfx (js-get res (js-toname 0 %this) %this))))))))
		   (else
		    (let ((newstring (js-tostring replacevalue %this))
			  (i (js-get res 'index %this))
			  (str (js-jsstring->string string)))
		       (js-stringlist->jsstring 
			  (cons (substring str 0 i)
			     (table22 newstring res str
				(substring str
				   (+fx i (js-jsstring-lengthfx (js-get res (js-toname 0 %this) %this))))))))))))
	    (else
;* 	     (tprint "string=" (map (lambda (c) (integer->string (char->integer c) 16)) (string->list string))) */
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
			      this)
			     ((isa? replacevalue JsFunction)
			      (let loop ((matches (reverse! ms))
					 (res this)
					 (offset 0))
				 (if (null? matches)
				     res
				     (let* ((m (car matches))
					    (i (js-get m 'index %this))
					    (l (js-jsstring-lengthfx (js-get m (js-toname 0 %this) %this)))
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
					    (res this)
					    (offset 0))
				    (if (null? matches)
					res
					(let* ((m (car matches))
					       (i (js-get m 'index %this))
					       (l (js-jsstring-lengthfx (js-get m (js-toname 0 %this) %this)))
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

;*---------------------------------------------------------------------*/
;*    js-jsstring-fromcharcode ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-fromcharcode this code %this)
   (let loop ((code code))
      (cond
	 ((not (fixnum? code))
	  (loop (uint16->fixnum (js-touint16 code %this))))
	 ((and (>=fx code 0) (<=fx code 255))
	  (vector-ref prealloc-strings code))
	 (else
	  (js-utf8->jsstring
	     (ucs2-string->utf8-string
		(ucs2-string (integer->ucs2 code))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-escape ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsstring-escape this)
   
   (define (ascii-escape str::bstring)
      (js-ascii->jsstring (url-path-encode str)))
   
   (define (utf8-escape str::bstring)
      (js-ascii->jsstring (utf8-path-encode str)))
   
   (string-dipatch escape this))
   
;*---------------------------------------------------------------------*/
;*    utf8-path-encode ...                                             */
;*---------------------------------------------------------------------*/
(define (utf8-path-encode str)
   
   (define set "# \"'`&=%?:\n^[]\\<>;,{|}()~$!")
   
   (define (ucs2-string-index? set n)
      (when (<fx n 127)
	 (string-index set (integer->char n))))
   
   (define (int->char c)
      (cond
	 ((<fx c 10)
	  (integer->char (+fx c (char->integer #\0))))
	 ((<fx c 16)
	  (integer->char (+fx (-fx c 10) (char->integer #\A))))))
   
   (define (encode-char res j n)
      (string-set! res j #\%)
      (cond
	 ((<fx n 16)
	  (string-set! res (+fx j 1) #\0)
	  (string-set! res (+fx j 2) (int->char n)))
	 ((<fx n 256)
	  (let ((n1 (/fx n 16))
		(n2 (remainderfx n 16)))
	     (string-set! res (+fx j 1) (int->char n1))
	     (string-set! res (+fx j 2) (int->char n2))))
	 (else
	  (let ((n2 (bit-rsh (bit-and n #xf000) 12))
		(n3 (bit-rsh (bit-and n #xf00) 8))
		(n4 (bit-rsh (bit-and n #xf0) 4))
		(n5 (bit-and n #xf)))
	     (string-set! res (+fx j 1) #\u)
	     (string-set! res (+fx j 2) (int->char n2))
	     (string-set! res (+fx j 3) (int->char n3))
	     (string-set! res (+fx j 4) (int->char n4))
	     (string-set! res (+fx j 5) (int->char n5))))))
   
   (define (count str ol)
      (let loop ((i 0)
		 (k 0))
	 (if (=fx i ol)
	     k
	     (let ((n (ucs2->integer (ucs2-string-ref str i))))
		(cond
		   ((ucs2-string-index? set n)
		    (loop (+fx i 1) (+fx k 3)))
		   ((>=fx n 256)
		    (loop (+fx i 1) (+fx k 6)))
		   ((or (<fx n 32) (>=fx n 127))
		    (loop (+fx i 1) (+fx k 3)))
		   (else
		    (loop (+fx i 1) (+fx k 1))))))))
   
   (define (encode str ol nl)
      (if (=fx nl ol)
	  str
	  (let ((res (make-string nl)))
	     (let loop ((i 0)
			(j 0))
		(if (=fx j nl)
		    res
		    (let ((n (ucs2->integer (ucs2-string-ref str i))))
		       (cond
			  ((ucs2-string-index? set n)
			   (encode-char res j n)
			   (loop (+fx i 1) (+fx j 3)))
			  ((>=fx n 256)
			   (encode-char res j n)
			   (loop (+fx i 1) (+fx j 6)))
			  ((or (<fx n 32) (>=fx n 127))
			   (encode-char res j n)
			   (loop (+fx i 1) (+fx j 3)))
			  (else
			   (string-set! res j (integer->char n))
			   (loop (+fx i 1) (+fx j 1))))))))))
   (let* ((ustr (utf8-string->ucs2-string str))
	  (ol (ucs2-string-length ustr)))
      (encode ustr ol (count ustr ol))))
      
;*---------------------------------------------------------------------*/
;*    js-jsstring-unescape ...                                         */
;*---------------------------------------------------------------------*/
(define (js-jsstring-unescape this %this)
   
   (define (ascii-unescape str::bstring)
      (if (string-index str #\%)
	  (let ((s (url-decode-extended str)))
	     (if s 
		 (js-string->jsstring s)
		 (js-raise-type-error %this "unescape, illegal value ~s" str)))
	  (js-string->jsstring str)))
   
   (define (utf8-unescape str::bstring)
      (js-raise-type-error %this "unescape, illegal value ~s" str))

   (string-dipatch unescape this))

;*---------------------------------------------------------------------*/
;*    url-decode-extended ...                                          */
;*---------------------------------------------------------------------*/
(define (url-decode-extended str)
   
   (define (char-value c)
      (cond
	 ((char-numeric? c)
	  (-fx (char->integer c) (char->integer #\0)))
	 ((char<=? c #\F)
	  (+fx 10 (-fx (char->integer c) (char->integer #\A))))
	 (else
	  (+fx 10 (-fx (char->integer c) (char->integer #\a))))))
   
   (define (char-hexnumeric? c)
      (or (char-numeric? c)
	  (and (char>=? c #\A) (char<=? c #\F))
	  (and (char>=? c #\a) (char<=? c #\f))))
   
   (let* ((ol (string-length str))
	  (buf (make-string (*fx ol 4)))) ;; 4: max utf8 sequence
      (let loop ((i 0)
		 (j 0))
	 (cond
	    ((=fx i ol)
	     (string-shrink! buf j))
	    ((string-index str #\% i)
	     =>
	     (lambda (ni)
		(cond
		   ((>fx ni (-fx ol 3))
		    (blit-string! str i buf j (-fx ol i)))
		   ((char=? (string-ref str (+fx ni 1)) #\u)
		    (blit-string! str i buf j (-fx ni i))
		    (set! j (+fx j (-fx ni i)))
		    (unless (>fx ni (-fx ol 5))
		       (let ((c1 (string-ref str (+fx ni 2)))
			     (c2 (string-ref str (+fx ni 3)))
			     (c3 (string-ref str (+fx ni 4)))
			     (c4 (string-ref str (+fx ni 5))))
			  (when (and (char-hexnumeric? c1) (char-hexnumeric? c2)
				     (char-hexnumeric? c3) (char-hexnumeric? c4))
			     (let* ((v1 (char-value c1))
				    (v2 (char-value c2))
				    (v3 (char-value c3))
				    (v4 (char-value c4))
				    (d (integer->ucs2
					  (+fx (bit-lsh v1 12)
					     (+fx (bit-lsh v2 8)
						(+fx (bit-lsh v3 4) v4)))))
				    (ucs2 (ucs2-string d))
				    (utf8 (ucs2-string->utf8-string ucs2))
				    (l (string-length utf8)))
				
				(blit-string! utf8 0 buf j l)
				(loop (+fx ni 6) (+fx j l)))))))
		   (else
		    (blit-string! str i buf j (-fx ni i))
		    (set! j (+fx j (-fx ni i)))
		    (unless (>fx ni (-fx ol 3))
		       (let ((c1 (string-ref str (+fx ni 1)))
			     (c2 (string-ref str (+fx ni 2))))
			  (when (and (char-hexnumeric? c1) (char-hexnumeric? c2))
			     (let* ((v1 (char-value c1))
				    (v2 (char-value c2))
				    (d (+fx (bit-lsh v1 4) v2)))
				(if (<fx d 128)
				    (begin
				       (string-set! buf j (integer->char d))
				       (loop (+fx i 3) (+fx j 1)))
				    (let* ((ucs2 (ucs2-string (integer->ucs2 d)))
					   (utf8 (ucs2-string->utf8-string ucs2))
					   (l (string-length utf8)))
				       
				       (blit-string! utf8 0 buf j l)
				       (loop (+fx ni 3) (+fx j l))))))))))))
	    (else
	     (let ((l (-fx ol i)))
		(blit-string! str i buf j l)
		(loop ol (+fx j l))))))))
