;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/stringliteral.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 21 14:13:28 2014                          */
;*    Last change :  Fri Mar 13 16:24:02 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
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
	   
	   (js-jsstring-append::JsStringLiteral ::JsStringLiteral ::JsStringLiteral)))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsString ...                                 */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsStringLiteral
   js-jsstring->string
   (lambda (s) s))

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
