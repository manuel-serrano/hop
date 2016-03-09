;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/stringliteral.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 21 14:13:28 2014                          */
;*    Last change :  Wed Mar  9 13:35:15 2016 (serrano)                */
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
   
   (export (inline js-string->jsstring::bstring ::bstring)
	   (js-stringlist->jsstring ::pair-nil)
	   (inline js-jsstring->string::bstring ::obj)
	   (inline js-jsstring?::bool ::obj)
	   (js-jsstring-length::long ::obj)
	   (inline js-jsstring-null? ::obj)
	   (inline js-jsstring=?::bool ::obj ::obj)
	   (inline js-jsstring<?::bool ::obj ::obj)
	   (inline js-jsstring<=?::bool ::obj ::obj)
	   (inline js-jsstring>?::bool ::obj ::obj)
	   (inline js-jsstring>=?::bool ::obj ::obj)
	   (js-string->number::obj ::bstring ::JsGlobalObject)
	   (js-integer->jsstring ::long)
	   (js-jsstring->bool::bool ::obj)
	   (js-jsstring-normalize!::bstring ::JsStringLiteral)
	   
	   (inline js-jsstring-append::JsStringLiteral ::obj ::obj)
	   (utf8-codeunit-ref::long ::bstring ::long)
	   (utf8-codeunit-length::long ::bstring)
	   (js-string-ref ::bstring ::long)
	   (js-get-string ::bstring ::obj ::obj)
	   (js-put-string! ::bstring ::obj ::obj ::bool ::obj)))

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
;*    js-string->jsstring ...                                          */
;*    -------------------------------------------------------------    */
;*    Create a JsStringLiteral from a Scheme string literal.           */
;*---------------------------------------------------------------------*/
(define-inline (js-string->jsstring::bstring val::bstring)
   val)
;*    (instantiate::JsStringLiteral                                    */
;* {*       (weight (string-length val))                                  *} */
;*       (left val)))                                                  */

;*---------------------------------------------------------------------*/
;*    js-stringlist->jsstring ...                                      */
;*    -------------------------------------------------------------    */
;*    Create a JsStringLiteral from a list of Scheme string literals.  */
;*---------------------------------------------------------------------*/
(define (js-stringlist->jsstring val::pair-nil)
   (if (null? val)
       (js-string->jsstring "")
       (let loop ((val val))
	  (if (null? (cdr val))
	      (js-string->jsstring (car val))
	      (instantiate::JsStringLiteral
		 (left (car val))
;* 		 (weight (string-length (car val)))                    */
		 (right (loop (cdr val))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring->string ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring->string::bstring js::obj)
   (if (string? js)
       js
       (js-jsstring-normalize! js)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize! ...                                       */
;*---------------------------------------------------------------------*/
(define (js-jsstring-normalize!::bstring js::JsStringLiteral)
   (with-access::JsStringLiteral js (left right)
      (if (and (string? left) (not right))
	  left
	  (let* ((buffer (make-string (js-string-literal-length js)))
		 (nlen (let loop ((i 0)
				  (js js))
			  (if (string? js)
			      (utf8-string-append-fill! buffer i js)
			      (with-access::JsStringLiteral js (left right)
				 (let ((ni (if (string? left)
					       (utf8-string-append-fill!
						  buffer i left)
					       (loop i left))))
				    (if (not right)
					ni
					(loop ni right))))))))
	     (set! left (string-shrink! buffer nlen))
	     (set! right #f)
	     left))))

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
	  (with-access::JsStringLiteral js (left right)
	     (loop (loop len left) right))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-length ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsstring-length js)
   (if (string? js)
       (string-length js)
       (js-string-literal-length js)))

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
		 (let ((s (integer->string i)))
		    `(instantiate::JsStringLiteral
			(left ,s)
			#;(weight ,(string-length s)))))
	    (iota num))))

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
;*    integers ...                                                     */
;*---------------------------------------------------------------------*/
(define integers (make-integer-table 100))

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
;*    -------------------------------------------------------------    */
;*    This append function optimizes the case where either the         */
;*    two strings are normalized or when the left string is            */
;*    normalized and the right string is a string list.                */
;*    -------------------------------------------------------------    */
;*    Abandon optimization if the consed string is a utf8 replacement. */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-append::JsStringLiteral left::obj right::obj)
   (instantiate::JsStringLiteral
      (left left)
      (right right)))

;*---------------------------------------------------------------------*/
;*    utf8-codeunit-length ...                                         */
;*    -------------------------------------------------------------    */
;*    Returns the number of code points required to encode that        */
;*    UTF8 string (might be bigger than the UTF8 length).              */
;*---------------------------------------------------------------------*/
(define (utf8-codeunit-length::long str::bstring)
   (let ((len (string-length str))
	 (sen (string-ascii-sentinel str)))
      (if (>=fx sen len)
	  len
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
			(loop (+fx r s) (+fx l 1)))))))))

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
   (let ((sentinel (string-ascii-sentinel str)))
      (if (<fx i sentinel)
	  (char->integer (string-ref str i))
	  (let ((len (string-length str)))
	     (let loop ((r sentinel) (i (-fx i sentinel)))
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
			  (ucs2->integer (ucs2-string-ref ucs2 i)))))))))))

;*---------------------------------------------------------------------*/
;*    js-string-ref ...                                                */
;*---------------------------------------------------------------------*/
(define (js-string-ref str::bstring index::long)
   (if (<fx index (string-ascii-sentinel str))
       (js-string->jsstring
	  (string-ascii-sentinel-set!
	     (string (string-ref str index))
	     1))
       (js-string->jsstring
	  (ucs2-string->utf8-string
	     (ucs2-string
		(integer->ucs2 (utf8-codeunit-ref str index)))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-ref ...                                              */
;*---------------------------------------------------------------------*/
(define (js-jsstring-ref o index::uint32)
   (let* ((val (js-jsstring->string o))
	  (fxpos (uint32->fixnum index)))
      (if (or (<fx fxpos 0) (>=fx fxpos (utf8-codeunit-length val)))
	  (js-undefined)
	  (js-string-ref val fxpos))))

;*---------------------------------------------------------------------*/
;*    js-get-string ...                                                */
;*---------------------------------------------------------------------*/
(define (js-get-string o::bstring prop %this)
   (let ((i (js-toindex prop)))
      (if (not (js-isindex? i))
	  ;; see js-get-jsobject@property.scm
	  (let* ((obj (js-toobject %this o))
		 (pval (js-get-property-value obj o prop %this)))
	     (if (eq? pval (js-absent))
		 (js-undefined)
		 pval))
	  (js-jsstring-ref o i))))

;*---------------------------------------------------------------------*/
;*    js-get ::JsStringLiteral ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsStringLiteral prop %this)
   (let ((i (js-toindex prop)))
      (if (not (js-isindex? i))
	  ;; see js-get-jsobject@property.scm
	  (let* ((obj (js-toobject %this o))
		 (pval (js-get-property-value obj o prop %this)))
	     (if (eq? pval (js-absent))
		 (js-undefined)
		 pval))
	  (js-jsstring-ref o i))))

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

