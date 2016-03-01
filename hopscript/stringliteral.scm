;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/stringliteral.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 21 14:13:28 2014                          */
;*    Last change :  Mon Feb 29 19:38:58 2016 (serrano)                */
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
   
   (export (inline js-string->jsstring::JsStringLiteral ::bstring)
	   (inline js-stringlist->jsstring::JsStringLiteral ::pair-nil)
	   (inline js-jsstring->string::bstring ::JsStringLiteral)
	   (inline js-jsstring?::bool ::obj)
	   (inline js-jsstring-length::long ::JsStringLiteral)
	   (inline js-jsstring-null? ::JsStringLiteral)
	   (inline js-jsstring=?::bool ::JsStringLiteral ::JsStringLiteral)
	   (inline js-jsstring<?::bool ::JsStringLiteral ::JsStringLiteral)
	   (inline js-jsstring<=?::bool ::JsStringLiteral ::JsStringLiteral)
	   (inline js-jsstring>?::bool ::JsStringLiteral ::JsStringLiteral)
	   (inline js-jsstring>=?::bool ::JsStringLiteral ::JsStringLiteral)
	   (js-integer->jsstring::JsStringLiteral ::long)
	   (js-jsstring->bool::bool ::JsStringLiteral)
	   (js-jsstring-normalize!::bstring ::JsStringLiteral)
	   
	   (js-jsstring-append::JsStringLiteral ::JsStringLiteral ::JsStringLiteral)
	   (utf8-codeunit-ref::long ::bstring ::long)
	   (utf8-codeunit-length::long ::bstring)
	   (js-string-ref::JsStringLiteral ::bstring ::long)))

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
   (js-jsstring-normalize! obj)
   obj)

;*---------------------------------------------------------------------*/
;*    scheme->response ::JsStringLiteral ...                           */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::JsStringLiteral req)
   (scheme->response (js-jsstring->string obj) req))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize! ...                                       */
;*    -------------------------------------------------------------    */
;*    0: S     ;; "foo" = "foo"                                        */
;*    1: L     ;; "foobar" = ("foo" "bar")                             */
;*    2: R     ;; "foobar" = ("bar" "foo")                             */
;*---------------------------------------------------------------------*/
(define (js-jsstring-normalize!::bstring js::JsStringLiteral)
   (with-access::JsStringLiteral js (state val)
      (case (uint8->fixnum state)
	 ((0)
	  val)
	 ((1)
	  (let ((v (apply utf8-string-append* val)))
	     (set! state #u8:0)
	     (set! val v)
	     v))
	 ((2)
	  (let ((v (apply utf8-string-append* (reverse val))))
	     (set! state #u8:0)
	     (set! val v)
	     v))
	 (else
	  (error "js-jsstring-normalize!" "internal error" state)))))

;*---------------------------------------------------------------------*/
;*    display-js-string ...                                            */
;*---------------------------------------------------------------------*/
(define (display-js-string jstr::JsStringLiteral op)
   (with-access::JsStringLiteral jstr (state val)
      (case (uint8->fixnum state)
	 ((0)
	  (display-string val op))
	 ((1)
	  (for-each (lambda (str) (display-string str op)) val))
	 ((2)
	  (with-access::JsStringLiteral jstr (state val)
	     (let ((v (reverse val)))
		(set! state #u8:1)
		(set! val v)
		(for-each (lambda (str) (display-string str op)) v)))))))

;*---------------------------------------------------------------------*/
;*    js-string->jsstring ...                                          */
;*    -------------------------------------------------------------    */
;*    Create a JsStringLiteral from a Scheme string literal.           */
;*---------------------------------------------------------------------*/
(define-inline (js-string->jsstring::JsStringLiteral val::bstring)
   (instantiate::JsStringLiteral
      (state #u8:0)
      (val val)))

;*---------------------------------------------------------------------*/
;*    js-stringlist->jsstring ...                                      */
;*    -------------------------------------------------------------    */
;*    Create a JsStringLiteral from a list of Scheme string literals.  */
;*---------------------------------------------------------------------*/
(define-inline (js-stringlist->jsstring::JsStringLiteral val::pair-nil)
   (instantiate::JsStringLiteral
      (state #u8:1)
      (val val)))

;*---------------------------------------------------------------------*/
;*    js-jsstring->string ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring->string::bstring js::JsStringLiteral)
   (with-access::JsStringLiteral js (state val)
      (if (=u8 state #u8:0)
	  val
	  (js-jsstring-normalize! js))))

;*---------------------------------------------------------------------*/
;*    js-jsstring? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring? obj)
   (isa? obj JsStringLiteral))

;*---------------------------------------------------------------------*/
;*    js-jsstring-length ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-length s::JsStringLiteral)
   (string-length (js-jsstring->string s)))

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
(define-inline (js-jsstring=?::bool left::JsStringLiteral right::JsStringLiteral)
   (string=? (js-jsstring->string left) (js-jsstring->string right)))

;*---------------------------------------------------------------------*/
;*    js-jsstring>? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring>?::bool left::JsStringLiteral right::JsStringLiteral)
   (string>? (js-jsstring->string left) (js-jsstring->string right)))

;*---------------------------------------------------------------------*/
;*    js-jsstring>=? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring>=?::bool left::JsStringLiteral right::JsStringLiteral)
   (string>=? (js-jsstring->string left) (js-jsstring->string right)))

;*---------------------------------------------------------------------*/
;*    js-jsstring<? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring<?::bool left::JsStringLiteral right::JsStringLiteral)
   (string<? (js-jsstring->string left) (js-jsstring->string right)))

;*---------------------------------------------------------------------*/
;*    js-jsstring<=? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring<=?::bool left::JsStringLiteral right::JsStringLiteral)
   (string<=? (js-jsstring->string left) (js-jsstring->string right)))

;*---------------------------------------------------------------------*/
;*    make-integer-table ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (make-integer-table num)
   `(vector
       ,@(map (lambda (i)
		 `(instantiate::JsStringLiteral
		     (val ,(integer->string i))
		     (state #u8:0)))
	    (iota num))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-tonumber ...                                         */
;*---------------------------------------------------------------------*/
(define (js-jsstring-tonumber this %this)
   (let ((str (trim-whitespaces+ (js-jsstring->string this)
		 :left #t :right #t :plus #t)))
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
(define (js-jsstring->bool::bool s::JsStringLiteral)
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
(define (js-jsstring-append::JsStringLiteral left::JsStringLiteral right::JsStringLiteral)
   (with-access::JsStringLiteral left ((lstate state) (lval val))
      (with-access::JsStringLiteral right ((rstate state) (rval val))
;* 	 (tprint "append left " lstate "=" (format "~s" lval)          */
;* 	    " right " rstate "=" (format "~s" rval))                   */
	 (case (uint8->fixnum lstate)
	    ((0)
	     (case (uint8->fixnum rstate)
		((0)
		 (instantiate::JsStringLiteral
		    (state #u8:1)
		    (val (list lval rval))))
		((1)
		 (instantiate::JsStringLiteral
		    (state #u8:1)
		    (val (cons lval rval))))
		((2)
		 (instantiate::JsStringLiteral
		    (state #u8:1)
		    (val (cons lval (reverse rval)))))
		(else
		 left)))
	    ((1)
	     (case (uint8->fixnum rstate)
		((0)
		 (instantiate::JsStringLiteral
		    (state #u8:2)
		    (val (cons rval (reverse lval)))))
		((1)
		 (instantiate::JsStringLiteral
		    (state #u8:1)
		    (val (append lval rval))))
		((2)
		 ;; reverse the smallest list
		 (if (<fx (length lval) (length rval))
		     (instantiate::JsStringLiteral
			(state #u8:2)
			(val (append rval (reverse lval))))
		     (instantiate::JsStringLiteral
			(state #u8:1)
			(val (append lval (reverse rval))))))
		(else
		 left)))
	    ((2)
	     (case (uint8->fixnum rstate)
		((0)
		 (instantiate::JsStringLiteral
		    (state #u8:2)
		    (val (cons rval lval))))
		((1)
		 ;; reverse the smallest list
		 (if (<fx (length lval) (length rval))
		     (instantiate::JsStringLiteral
			(state #u8:1)
			(val (append (reverse lval) rval)))
		     (instantiate::JsStringLiteral
			(state #u8:2)
			(val (append (reverse rval) lval)))))
		((2)
		 (instantiate::JsStringLiteral
		    (state #u8:2)
		    (val (append rval lval))))
		(else
		 left)))
	    (else
	     left)))))

;*---------------------------------------------------------------------*/
;*    utf8-codeunit-length ...                                         */
;*    -------------------------------------------------------------    */
;*    Returns the number of code points required to encode that        */
;*    UTF8 string (might be bigger than the UTF8 length).              */
;*---------------------------------------------------------------------*/
(define (utf8-codeunit-length str)
   (let ((len (string-length str))
	 (sen (string-ascii-sentinel str)))
      (if (>fx sen len)
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
(define (js-string-ref::JsStringLiteral str::bstring index::long)
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
(define (js-jsstring-ref o::JsStringLiteral index::uint32)
   (let* ((val (js-jsstring->string o))
	  (fxpos (uint32->fixnum index)))
      (if (or (<fx fxpos 0) (>=fx fxpos (utf8-codeunit-length val)))
	  (js-undefined)
	  (js-string-ref val fxpos))))

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
