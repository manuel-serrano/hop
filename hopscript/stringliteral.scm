;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/stringliteral.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 21 14:13:28 2014                          */
;*    Last change :  Sat Dec 13 08:02:36 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Internal implementation of literal strings                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_stringliteral

   (library hop)
   
   (import __hopscript_types
	   __hopscript_public)
   
   (export (inline string->js-string::JsStringLiteral ::bstring)
	   (inline string-list->js-string::JsStringLiteral ::pair-nil)
	   (inline js-string->string::bstring ::JsStringLiteral)
	   (inline js-string?::bool ::obj)
	   (inline js-string-length::long ::JsStringLiteral)
	   (inline js-string-null? ::JsStringLiteral)
	   (inline js-string=?::bool ::JsStringLiteral ::JsStringLiteral)
	   (inline js-string<?::bool ::JsStringLiteral ::JsStringLiteral)
	   (inline js-string<=?::bool ::JsStringLiteral ::JsStringLiteral)
	   (inline js-string>?::bool ::JsStringLiteral ::JsStringLiteral)
	   (inline js-string>=?::bool ::JsStringLiteral ::JsStringLiteral)
	   (integer->js-string::JsStringLiteral ::long)
	   (js-string->bool::bool ::JsStringLiteral)
	   (js-string-normalize!::bstring ::JsStringLiteral)
	   
	   (js-string-append::JsStringLiteral ::JsStringLiteral ::JsStringLiteral)))

;*---------------------------------------------------------------------*/
;*    object-print ::JsStringLiteral ...                               */
;*---------------------------------------------------------------------*/
(define-method (object-print obj::JsStringLiteral op proc)
   (display-js-string obj op))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsStringLiteral ...                            */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsStringLiteral op compile isexpr)
   (hop->javascript (js-string->string o) op compile isexpr))

;* {*---------------------------------------------------------------------*} */
;* {*    object-serializer ::JsStringLiteral ...                          *} */
;* {*---------------------------------------------------------------------*} */
;* (register-class-serialization! JsStringLiteral                      */
;*    js-string->string                                                */
;*    string->js-string)                                               */
;*                                                                     */
;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsStringLiteral ...                        */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value o::JsStringLiteral)
   (js-string->string o))

;*---------------------------------------------------------------------*/
;*    xml-write ::JsStringLiteral ...                                  */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::JsStringLiteral op backend)
   (display-js-string obj op))

;*---------------------------------------------------------------------*/
;*    js-inspect ::JsStringLiteral ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-inspect s::JsStringLiteral cnt)
   (js-string->string s))

;*---------------------------------------------------------------------*/
;*    xml-attribute-encode ::JsStringLiteral ...                       */
;*---------------------------------------------------------------------*/
(define-method (xml-attribute-encode obj::JsStringLiteral)
   (xml-attribute-encode (js-string->string obj)))

;*---------------------------------------------------------------------*/
;*    scheme->response ::JsStringLiteral ...                           */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::JsStringLiteral req)
   (scheme->response (js-string->string obj) req))

;*---------------------------------------------------------------------*/
;*    js-string-normalize! ...                                         */
;*    -------------------------------------------------------------    */
;*    0: S     ;; "foo" = "foo"                                        */
;*    1: L     ;; "foobar" = ("foo" "bar")                             */
;*    2: R     ;; "foobar" = ("bar" "foo")                             */
;*    3: S*    ;; utf8-left-replacement                                */
;*    4: S*    ;; utf8-right-replacement                               */
;*---------------------------------------------------------------------*/
(define (js-string-normalize!::bstring js::JsStringLiteral)
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
	  (error "js-string-normalize!" "internal error" state)))))

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
;*    string->js-string ...                                            */
;*    -------------------------------------------------------------    */
;*    Create a JsStringLiteral from a Scheme string literal.           */
;*---------------------------------------------------------------------*/
(define-inline (string->js-string::JsStringLiteral val::bstring)
   (instantiate::JsStringLiteral
      (state #u8:0)
      (val val)))

;*---------------------------------------------------------------------*/
;*    string-list->js-string ...                                       */
;*    -------------------------------------------------------------    */
;*    Create a JsStringLiteral from a list of Scheme string literals.  */
;*---------------------------------------------------------------------*/
(define-inline (string-list->js-string::JsStringLiteral val::pair-nil)
   (instantiate::JsStringLiteral
      (state #u8:1)
      (val val)))

;*---------------------------------------------------------------------*/
;*    js-string->string ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-string->string::bstring js::JsStringLiteral)
   (with-access::JsStringLiteral js (state val)
      (if (=u8 state #u8:0)
	  val
	  (js-string-normalize! js))))

;*---------------------------------------------------------------------*/
;*    js-string? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-string? obj)
   (isa? obj JsStringLiteral))

;*---------------------------------------------------------------------*/
;*    js-string-length ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-string-length s::JsStringLiteral)
   (string-length (js-string->string s)))

;*---------------------------------------------------------------------*/
;*    hop-register-value ::JsStringLiteral ...                         */
;*---------------------------------------------------------------------*/
(define-method (hop-register-value s::JsStringLiteral register)
   #t)

;*---------------------------------------------------------------------*/
;*    js-string-null? ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-string-null? js)
   (string-null? (js-string->string js)))

;*---------------------------------------------------------------------*/
;*    js-string=? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-string=?::bool left::JsStringLiteral right::JsStringLiteral)
   (string=? (js-string->string left) (js-string->string right)))

;*---------------------------------------------------------------------*/
;*    js-string>? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-string>?::bool left::JsStringLiteral right::JsStringLiteral)
   (string>? (js-string->string left) (js-string->string right)))

;*---------------------------------------------------------------------*/
;*    js-string>=? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-string>=?::bool left::JsStringLiteral right::JsStringLiteral)
   (string>=? (js-string->string left) (js-string->string right)))

;*---------------------------------------------------------------------*/
;*    js-string<? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-string<?::bool left::JsStringLiteral right::JsStringLiteral)
   (string<? (js-string->string left) (js-string->string right)))

;*---------------------------------------------------------------------*/
;*    js-string<=? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-string<=?::bool left::JsStringLiteral right::JsStringLiteral)
   (string<=? (js-string->string left) (js-string->string right)))

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
;*    integers ...                                                     */
;*---------------------------------------------------------------------*/
(define integers (make-integer-table 100))

;*---------------------------------------------------------------------*/
;*    integer->js-string ...                                           */
;*---------------------------------------------------------------------*/
(define (integer->js-string num::long)
   (if (or (<fx num 0) (>fx num (vector-length integers)))
       (string->js-string (integer->string num))
       (vector-ref-ur integers num)))

;*---------------------------------------------------------------------*/
;*    js-string->bool ...                                              */
;*---------------------------------------------------------------------*/
(define (js-string->bool::bool s::JsStringLiteral)
   (>fx (string-length (js-string->string s)) 0))

;*---------------------------------------------------------------------*/
;*    js-string-append ...                                             */
;*    -------------------------------------------------------------    */
;*    This append function optimizes the case where either the         */
;*    two strings are normalized or when the left string is            */
;*    normalized and the right string is a string list.                */
;*    -------------------------------------------------------------    */
;*    Abandon optimization if the consed string is a utf8 replacement. */
;*---------------------------------------------------------------------*/
(define (js-string-append::JsStringLiteral left::JsStringLiteral right::JsStringLiteral)
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
