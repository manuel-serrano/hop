;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/names.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar 30 06:29:09 2019                          */
;*    Last change :  Sat Apr 13 06:34:17 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Property names (see stringliteral.scm)                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_names

   (include "types.sch" "constants.sch")

   (library hop)

   (import __hopscript_types)
   
   (use    __hopscript_stringliteral
	   __hopscript_lib
	   __hopscript_public)

   (export (js-init-names! ::JsGlobalObject)
	   (inline js-name-pcache::obj ::JsStringLiteral)
	   (inline js-name-pcache-set! ::JsStringLiteral ::JsPropertyCache)
	   (inline js-jsstring-toname::JsStringLiteral ::JsStringLiteral ::JsGlobalObject)
	   (js-toname::obj ::obj ::JsGlobalObject)
	   (inline js-jsstring-name ::JsStringLiteral)
	   (inline js-name->string::bstring ::JsStringLiteral)
	   (js-jsstring->name!::JsStringLiteral ::JsStringLiteral ::JsGlobalObject)
	   (js-ascii-name->jsstring::JsStringLiteralASCII ::bstring ::JsGlobalObject)
	   (js-utf8-name->jsstring::JsStringLiteralUTF8 ::bstring ::JsGlobalObject)
	   (js-integer-name->jsstring::JsStringLiteralASCII ::long ::JsGlobalObject)
	   (js-name->jsstring::JsStringLiteral ::bstring ::JsGlobalObject))
   
   (static js-names js-integer-names js-string-names)
   
   (cond-expand
      ((config thread-local-storage #t)
       (pragma (js-names thread-local)
	  (js-integer-names thread-local)
	  (js-string-names thread-local)))))

;*---------------------------------------------------------------------*/
;*    name tables                                                      */
;*---------------------------------------------------------------------*/
(define js-names #f)
(define js-integer-names #f)
(define js-string-names #f)

;*---------------------------------------------------------------------*/
;*    js-init-names! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-init-names! %this::JsGlobalObject)
   (unless js-names
   ;;   (with-access::JsGlobalObject %this (js-names js-integer-names js-string-names)
      (set! js-names
	 (create-hashtable :eqtest string=? :hash string-hash-number))
      (set! js-integer-names
	 (list->vector
	    (append
	       (map (lambda (i)
		       (js-ascii-name->jsstring (fixnum->string i) %this))
		  (iota 10 -10))
	       (map (lambda (i)
		       (js-index-name->jsstring (fixnum->uint32 i) %this))
		  (iota 100)))))
      (set! js-string-names
	 (vector-map! (lambda (val)
			 (js-ascii-name->jsstring val %this))
	    (& strings)))))

;*---------------------------------------------------------------------*/
;*    js-name-pcache ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-name-pcache::obj o::JsStringLiteral)
   (with-access::JsStringLiteral o (pcache)
      pcache))

;*---------------------------------------------------------------------*/
;*    js-name-pcache-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-name-pcache-set! o::JsStringLiteral c::JsPropertyCache)
   (with-access::JsStringLiteral o (pcache)
      (set! pcache c)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-name ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-name o::JsStringLiteral)
   (object-widening o))

;*---------------------------------------------------------------------*/
;*    js-jsstring-name-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-name-set! o::JsStringLiteral name::JsStringLiteral)
   (object-widening-set! o name))

;*---------------------------------------------------------------------*/
;*    js-jsstring-toname ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-toname::JsStringLiteral p::JsStringLiteral %this)
   (or (js-jsstring-name p) (js-jsstring->name! p %this)))

;*---------------------------------------------------------------------*/
;*    js-toname ...                                                    */
;*    -------------------------------------------------------------    */
;*    www.ecma-international.org/ecma-262/7.0/#sec-topropertykey       */
;*---------------------------------------------------------------------*/
(define (js-toname p %this)
   (cond
      ((isa? p JsStringLiteral)
       (js-jsstring-toname p %this))
      ((fixnum? p)
       (js-integer-name->jsstring p %this))
      ((uint32? p)
       (cond-expand
	  (bint30
	   (if (<u32 p (fixnum->uint32 (bit-lsh 1 29)))
	       (js-integer-name->jsstring (uint32->fixnum p))
	       (js-ascii-name->jsstring (llong->string (uint32->llong p)) %this)))
	  (bint32
	   (if (<u32 p (bit-lshu32 (fixnum->uint32 1) 31))
	       (js-integer-name->jsstring (uint32->fixnum p))
	       (js-ascii-name->jsstring (llong->string (uint32->llong p)) %this)))
	  (else
	   (js-integer-name->jsstring (uint32->fixnum p) %this))))
      ((int32? p)
       (cond-expand
	  (bint30
	   (if (and (>s32 p 0) (<s32 p (fixnum->int32 (bit-lsh 1 29))))
	       (js-integer-name->jsstring (int32->fixnum p))
	       (js-ascii-name->jsstring (llong->string (int32->llong p)) %this)))
	  (bint32
	   (js-ascii-name->jsstring (fixnum->string (int32->fixnum p)) %this))
	  (else
	   (js-integer-name->jsstring (int32->fixnum p) %this))))
      ((isa? p JsSymbolLiteral)
       p)
      ((isa? p JsSymbol)
       (with-access::JsSymbol p (val)
	  val))
      ((symbol? p)
       (error "js-toname" "Illegal `symbol'" p))
      ((string? p)
       (error "js-toname" "Illegal `string'" p))
      ((number? p)
       (js-ascii-name->jsstring (js-tostring p %this) %this))
      (else
       (js-name->jsstring (js-tostring p %this) %this))))

;*---------------------------------------------------------------------*/
;*    js-name->string ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-name->string::bstring o::JsStringLiteral)
   (with-access::JsStringLiteral o (left)
      left))

;*---------------------------------------------------------------------*/
;*    js-jsstring->name! ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsstring->name!::JsStringLiteral o::JsStringLiteral %this)
   ;; call js-jsstring->string as the string must be normalized
   ;; before being potentially added in the name hashtable
;*    (with-access::JsGlobalObject %this (js-names)                    */
   (begin
      (let ((str (js-jsstring->string o)))
	 (let ((n (hashtable-get js-names str)))
	    (unless n
	       (set! n o)
	       (hashtable-put! js-names str n))
	    (js-jsstring-name-set! o n)
	    n))))

;*---------------------------------------------------------------------*/
;*    js-ascii-name->jsstring ...                                      */
;*---------------------------------------------------------------------*/
(define (js-ascii-name->jsstring::JsStringLiteralASCII str::bstring %this)
;*    (with-access::JsGlobalObject %this (js-names)                    */
   (begin
      (let ((n (hashtable-get js-names str)))
	 (or n
	     (let ((n (instantiate::JsStringLiteralASCII
			 (weight (fixnum->uint32 (string-length str)))
			 (left str)
			 (right #f))))
		(hashtable-put! js-names str n)
		(js-jsstring-name-set! n n)
		n)))))

;*---------------------------------------------------------------------*/
;*    js-index-name->jsstring ...                                      */
;*---------------------------------------------------------------------*/
(define (js-index-name->jsstring::JsStringLiteralASCII num::uint32 %this)
;*    (with-access::JsGlobalObject %this (js-names)                    */
   (begin
      (let* ((str (fixnum->string (uint32->fixnum num)))
	     (n (hashtable-get js-names str)))
	 (or n
	     (let ((n (instantiate::JsStringLiteralIndex
			 (weight (string-length str))
			 (left str)
			 (right #f)
			 (index num))))
		(hashtable-put! js-names str n)
		(js-jsstring-name-set! n n)
		n)))))

;*---------------------------------------------------------------------*/
;*    js-utf8-name->jsstring ...                                       */
;*---------------------------------------------------------------------*/
(define (js-utf8-name->jsstring str::bstring %this)
;*    (with-access::JsGlobalObject %this (js-names)                    */
   (begin
      (let ((n (hashtable-get js-names str)))
	 (or n
	     (let ((n (instantiate::JsStringLiteralUTF8
			 (weight (fixnum->uint32 (string-length str)))
			 (left str)
			 (right #f))))
		(hashtable-put! js-names str n)
		(js-jsstring-name-set! n n)
		n)))))

;*---------------------------------------------------------------------*/
;*    js-name->jsstring ...                                            */
;*---------------------------------------------------------------------*/
(define (js-name->jsstring::JsStringLiteral str::bstring %this)
   (let ((enc (string-minimal-charset str)))
      (case enc
	 ((ascii) (js-ascii-name->jsstring str %this))
	 ((latin1 utf8) (js-utf8-name->jsstring str %this))
	 (else (error "js-name->jsstring" "unsupported encoding" enc)))))

;*---------------------------------------------------------------------*/
;*    js-integer-name->jsstring ...                                    */
;*---------------------------------------------------------------------*/
(define (js-integer-name->jsstring num::long %this)
   (cond
      ((and (>fx num -10) (<fx num 100))
;*        (with-access::JsGlobalObject %this (js-integer-names)        */
       (begin
	  (vector-ref js-integer-names (+fx num 10))))
      ((and (>fx num 0) (<fx num 65535))
       (js-index-name->jsstring (fixnum->uint32 num) %this))
      (else
       (js-ascii-name->jsstring (fixnum->string num) %this))))
