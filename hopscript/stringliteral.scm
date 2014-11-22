;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/stringliteral.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 21 14:13:28 2014                          */
;*    Last change :  Sat Nov 22 07:59:33 2014 (serrano)                */
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
	   
	   (js-string-append::JsStringLiteral ::JsStringLiteral ::JsStringLiteral)
	   (js-string-appendN::JsStringLiteral ::JsStringLiteral ::pair-nil ::JsGlobalObject)))


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
      (state #u8:2)
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
;*    hop->javascript ::JsStringLiteral ...                            */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsStringLiteral op compile isexpr)
   (hop->javascript (js-string->string o) op compile isexpr))

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsStringLiteral ...                          */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsStringLiteral
   js-string->string
   string->js-string)

;*---------------------------------------------------------------------*/
;*    xml-id? ::JsStringLiteral ...                                    */
;*---------------------------------------------------------------------*/
(define-method (xml-id? o::JsStringLiteral)
   #t)

;*---------------------------------------------------------------------*/
;*    xml-write-id ::JsStringLiteral ...                               */
;*---------------------------------------------------------------------*/
(define-method (xml-write-id obj::JsStringLiteral op)
   (display (js-string->string obj) op))

;*---------------------------------------------------------------------*/
;*    xml-write ::JsStringLiteral ...                                  */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::JsStringLiteral op backend)
   (display (js-string->string obj) op))

;*---------------------------------------------------------------------*/
;*    object-print ::JsStringLiteral ...                               */
;*---------------------------------------------------------------------*/
(define-method (object-print obj::JsStringLiteral op proc)
   (display (js-string->string obj) op))

;*---------------------------------------------------------------------*/
;*    js-inspect ::JsStringLiteral ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-inspect s::JsStringLiteral cnt)
   (js-string->string s))

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
;*    js-string-normalize! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-string-normalize! js::JsStringLiteral)
   ;; assumed a unnormalized string
   (with-access::JsStringLiteral js (state val)
      (case (uint8->fixnum state)
	 ((1)
	  (let ((nval (utf8-string-append
			 (js-string->string (car val))
			 (js-string->string (cdr val)))))
	     (set! state #u8:0)
	     (set! val nval)
	     nval))
	 ((2)
	  (let ((nval (apply utf8-string-append* val)))
	     (set! state #u8:0)
	     (set! val nval)
	     nval))
	 (else
	  (error "js-string-normalize!" "internal error" state)))))

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
;*    string-append3 ...                                               */
;*---------------------------------------------------------------------*/
(define (string-append3::JsStringLiteral left::bstring middle::bstring right::bstring)
   (instantiate::JsStringLiteral
      (state #u8:0)
      (val (string-append left middle right))))

;*---------------------------------------------------------------------*/
;*    js-string-appendN ...                                            */
;*---------------------------------------------------------------------*/
(define (js-string-appendN::JsStringLiteral left::JsStringLiteral rest::pair-nil %this)
   (instantiate::JsStringLiteral
      (state #u8:1)
      (val (cons left (map (lambda (r) (js-tojsstring r %this)) rest)))))

;*---------------------------------------------------------------------*/
;*    js-string-append ...                                             */
;*---------------------------------------------------------------------*/
(define (js-string-append::JsStringLiteral left::JsStringLiteral right::JsStringLiteral)
   (instantiate::JsStringLiteral
      (state #u8:1)
      (val (cons left right))))


