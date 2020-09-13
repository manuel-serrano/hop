;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/stringliteral.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 21 14:13:28 2014                          */
;*    Last change :  Sun Apr 19 08:03:24 2020 (serrano)                */
;*    Copyright   :  2014-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Internal implementation of literal strings                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_stringliteral

   (library hop)

   (include "types.sch" "names.sch" "stringthread.sch")
   
   (import __hopscript_types
	   __hopscript_names
	   __hopscript_arithmetic
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_private
	   __hopscript_property
	   __hopscript_array
	   __hopscript_regexp)

   (export (js-init-stringliteral! ::JsGlobalObject)
	   (&jsstring-init ::bstring)
	   (js-debug-jsstring ::JsStringLiteral #!optional (msg ""))
	   (js-jsstring-for-in str ::procedure ::JsGlobalObject)
	   (js-jsstring-for-of str ::procedure ::JsGlobalObject)
	   (inline js-ascii->jsstring::JsStringLiteralASCII ::bstring)
	   (js-substring->jsstring::JsStringLiteral ::bstring ::long ::long)
	   (inline js-utf8->jsstring::JsStringLiteralUTF8 ::bstring)
	   (inline js-utf8->jsstring/ulen::JsStringLiteralUTF8 ::bstring ::uint32)
	   (js-string->jsstring::JsStringLiteral ::bstring)
	   (js-symbol->jsstring::JsStringLiteral ::symbol)
	   (js-keyword->jsstring::JsStringLiteral ::keyword)
	   (js-integer->jsstring::JsStringLiteralASCII ::long)
	   (js-stringlist->jsstring ::pair-nil)
	   (inline js-jsstring->string::bstring ::JsStringLiteral)
	   (js-jsstring->number::obj ::JsStringLiteral) 
	   (js-jsstring-character-ref ::obj ::uint32)
	   (js-jsstring-ref ::obj ::uint32 ::JsGlobalObject)
	   (js-ascii-ref ::JsStringLiteralASCII ::uint32 ::JsGlobalObject)
	   (js-jsstring-length::uint32 ::JsStringLiteral)
	   (js-string-ref ::obj ::obj ::JsGlobalObject)
	   (js-string-ref-as-string ::obj ::obj ::JsGlobalObject)
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
	   (js-string-parseint ::bstring ::int32 ::bool)
	   (js-string-parsefloat ::bstring ::bool)
	   (js-string->bool::bool ::bstring)
	   (inline js-jsstring-toboolean::bool ::JsStringLiteral)
	   (js-jsstring-normalize!::JsStringLiteral ::JsStringLiteral)
	   
	   (js-jsstring-normalize-ASCII!::bstring ::JsStringLiteralASCII)
	   (js-jsstring-normalize-SUBSTRING!::bstring ::JsStringLiteralSubstring)
	   (js-jsstring-normalize-UTF8!::bstring ::JsStringLiteralUTF8)
	   (inline js-jsstring-append::JsStringLiteral ::JsStringLiteral ::JsStringLiteral)
	   (js-jsstring-append3::JsStringLiteral ::JsStringLiteral ::JsStringLiteral ::JsStringLiteral)
	   (js-jsstring-append-no-inline::JsStringLiteral ::JsStringLiteral ::JsStringLiteral)
	   (js-jsstring-append-ASCII::JsStringLiteral ::JsStringLiteral ::JsStringLiteral)
	   (js-jsstring-append-UTF8::JsStringLiteral ::JsStringLiteral ::JsStringLiteral)
	   (inline js-jsstring-append-ascii::JsStringLiteral ::JsStringLiteral ::JsStringLiteral)
	   (js-jsstring-append-ascii3::JsStringLiteral ::JsStringLiteral ::JsStringLiteral ::JsStringLiteral)
	   (utf8-codeunit-length::long ::bstring)
	   (js-utf8-ref ::JsStringLiteralUTF8 ::bstring ::long ::JsGlobalObject)
	   (js-get-string ::JsStringLiteral ::obj ::obj)
	   (js-put-string! ::JsStringLiteral ::obj ::obj ::bool ::obj)
	   (js-jsstring-indexof ::JsStringLiteral ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-indexof ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-lastindexof ::JsStringLiteral ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-lastindexof ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-charcodeat ::JsStringLiteral ::obj ::JsGlobalObject)
	   (js-jsstring-charcodeat-as-int32::int32 ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-charcodeatu32 ::JsStringLiteral ::uint32)
	   (js-jsstring-charcodeatu32-as-int32::int32 ::JsStringLiteral ::uint32)
	   (js-jsstring-maybe-charcodeat ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-codepointat ::JsStringLiteral ::obj ::JsGlobalObject)
	   (js-jsstring-codepointat-as-int32::int32 ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-codepointatu32 ::JsStringLiteral ::uint32)
	   (js-jsstring-codepointatu32-as-int32::int32 ::JsStringLiteral ::uint32)
	   (js-jsstring-maybe-codepointat ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-charat ::JsStringLiteral ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-charat ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-substring::JsStringLiteral ::JsStringLiteral ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-substring ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-substr ::JsStringLiteral ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-substr ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-tolowercase ::JsStringLiteral)
	   (js-jsstring-maybe-tolowercase ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-tolocalelowercase ::JsStringLiteral)
	   (js-jsstring-maybe-tolocalelowercase ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-touppercase ::JsStringLiteral)
	   (js-jsstring-maybe-touppercase ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-tolocaleuppercase ::JsStringLiteral)
	   (js-jsstring-maybe-tolocaleuppercase ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-split ::JsStringLiteral ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-split ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-replace-regexp ::obj ::regexp ::long ::bool ::obj ::JsGlobalObject)
	   (js-jsstring-replace-regexp-fun1 ::obj ::regexp ::long ::bool ::procedure ::JsGlobalObject)
	   (js-jsstring-replace-regexp-string ::obj ::regexp ::long ::bool ::obj ::JsGlobalObject)
	   (js-jsstring-replace-string ::obj ::bool ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-replace ::obj ::bool ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-prototype-replace ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-replace ::obj ::bool ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-match ::JsStringLiteral ::obj ::JsGlobalObject)
	   (js-jsstring-match-regexp-from-string ::JsStringLiteral ::JsStringLiteral ::JsRegExp ::JsGlobalObject)
	   (js-jsstring-match-regexp-from-string-as-bool::bool ::JsStringLiteral ::JsStringLiteral ::JsRegExp ::JsGlobalObject)
	   (js-jsstring-maybe-match ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-naturalcompare ::JsStringLiteral ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-naturalcompare ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-localecompare ::JsStringLiteral ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-localecompare ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-trim ::JsStringLiteral)
	   (js-jsstring-maybe-trim ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-fromcharcode ::obj ::JsGlobalObject)
	   (js-jsstring-escape ::JsStringLiteral)
	   (js-jsstring-unescape ::JsStringLiteral ::JsGlobalObject)
	   (js-jsstring-slice ::JsStringLiteral ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-slice1 ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-maybe-slice2 ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring->jsarray ::JsStringLiteral ::JsGlobalObject)
	   (js-jsstring->list ::obj ::JsGlobalObject))

   (cond-expand
      ((not |bigloo4.3a|)
       (pragma (js-string->jsstring default-inline)))))

;*---------------------------------------------------------------------*/
;*    gcroots                                                          */
;*---------------------------------------------------------------------*/
(define gcroots '())

;*---------------------------------------------------------------------*/
;*    string-append-auto-normalize-threshold ...                       */
;*    -------------------------------------------------------------    */
;*    The threshold under which APPEND normalizes strings.             */
;*---------------------------------------------------------------------*/
(define (string-append-auto-normalize-threshold)
   #u32:18)

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-debug-object ::JsStringLiteral ...                            */
;*---------------------------------------------------------------------*/
(define-method (js-debug-object obj::JsStringLiteral #!optional (msg ""))
   (with-access::JsStringLiteral obj (length left right)
      (fprint (current-error-port) "=== " msg (typeof obj)
	 " length=" length
	 " left=" (typeof left)
	 " right=" (typeof right)
	 " pcacher=" (typeof (js-name-pcacher obj))
	 " pachew=" (typeof (js-name-pcachew obj)))
      (when (isa? (js-name-pcacher obj) JsPropertyCache)
	 (fprint (current-error-port) "string pcacher:")
	 (js-debug-pcache (js-name-pcacher obj)))
      (when (isa? (js-name-pcachew obj) JsPropertyCache)
	 (fprint (current-error-port) "string pcachew:")
	 (js-debug-pcache (js-name-pcachew obj)))))

;*---------------------------------------------------------------------*/
;*    js-init-stringliteral! ...                                       */
;*---------------------------------------------------------------------*/
(define (js-init-stringliteral! %this)
   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   (with-access::JsGlobalObject %this (char-table)
      (let ((vec (make-vector 256)))
	 (let loop ((i 0))
	    (when (<=fx i 127)
	       (vector-set! vec i
		  (js-string->name
		     (make-string 1 (integer->char i))))
	       (loop (+fx i 1))))
	 (let loop ((i 128))
	    (when (<=fx i 255)
	       (vector-set! vec i
		  (js-utf8->jsstring/ulen
		     (ucs2-string->utf8-string
			(make-ucs2-string 1 (integer->ucs2 i)))
		     #u32:1))
	       (loop (+fx i 1))))
	 (set! char-table vec))))

;*---------------------------------------------------------------------*/
;*    jsstring-init-lock ...                                           */
;*---------------------------------------------------------------------*/
(define jsstring-init-lock
   (make-mutex "jsjstring-init-lock"))

;*---------------------------------------------------------------------*/
;*    &jsstring-init ...                                               */
;*---------------------------------------------------------------------*/
(define (&jsstring-init str::bstring)
   (js-init-names!)
   (let ((cnsts (string->obj str)))
      ;; start fill at index 1 because of the C declaration
      ;; of the constant vector (see constants_expd.sch)
      (let loop ((i (-fx (vector-length cnsts) 1)))
	 (when (>=fx i 0)
	    (let ((el (vector-ref cnsts i)))
	       (vector-set! cnsts i
		  (case (vector-ref el 0)
		     ((0)
		      ;; an ascii name
		      (let ((str (vector-ref el 1)))
			 (js-ascii-name->jsstring str)))
		     ((1)
		      ;; an utf8 name
		      (let ((str (vector-ref el 1)))
			 (js-utf8-name->jsstring str)))
		     ((2)
		      ;; a fixnum name
		      (let ((num (vector-ref el 1)))
			 (js-integer-name->jsstring num))))))
	    (loop (-fx i 1))))
      (synchronize jsstring-init-lock
	 (set! gcroots (cons cnsts gcroots)))
      cnsts))

;*---------------------------------------------------------------------*/
;*    js-debug-jsstring ...                                            */
;*---------------------------------------------------------------------*/
(define (js-debug-jsstring obj #!optional (msg ""))
   (let loop ((obj obj)
	      (margin ""))
      (with-access::JsStringLiteral obj (left right length)
	 (tprint msg margin (typeof obj)
	    " normalized=" (js-jsstring-normalized? obj)
	    " length=" length
	    " depth=" (js-jsstring-depth obj 1024))
	 (unless (or (js-jsstring-normalized? obj) (js-jsstring-substring? obj))
	    (let ((nm (string-append " " margin)))
	       (loop left nm)
	       (loop right nm))))))
   
;*---------------------------------------------------------------------*/
;*    js-debug-object ::JsStringLiteral ...                            */
;*---------------------------------------------------------------------*/
(define-method (js-debug-object obj::JsStringLiteral #!optional (msg ""))
      
   (define (excerpt str)
      (if (<fx (string-length str) 20)
	  (format "~s" str)
	  (format "~s..." (substring str 0 17))))

   (define (literal-or-string obj)
      (if (string? obj)
	  (excerpt obj)
	  (typeof obj)))
   
   (with-access::JsStringLiteral obj (left right length)
      (tprint msg "[" (typeof obj)
	 " length=" length " depth=" (js-jsstring-depth obj 1024)
	 " left=" (literal-or-string left) " right="
	 (literal-or-string right) "]")))

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
;*    js-jsobject->obj ::JsStringLiteral ...                           */
;*---------------------------------------------------------------------*/
(define-method (js-jsobject->obj obj::JsStringLiteral %this)
   (js-jsstring->string obj))

;*---------------------------------------------------------------------*/
;*    js-toindex ::JsStringLiteral ...                                 */
;*---------------------------------------------------------------------*/
(define-method (js-toindex obj::JsStringLiteral)

   (define false (-u32 #u32:0 #u32:1))

   (define (integer-string? str)
      (let ((len (string-length str)))
	 (let loop ((i 0))
	    (cond
	       ((=fx i len) (>fx i 0))
	       ((char-numeric? (string-ref str i)) (loop (+fx i 1)))
	       (else #f)))))
   
   (define (string->index p::bstring)
      (if (integer-string? p)
	  (let ((num (string->number p)))
	    (if (bignum? num)
		(if (and (>=bx num #z0) (<bx num (exptbx #z2 #z32)))
		    (llong->uint32 (bignum->llong num))
		    false)
		(js-toindex num)))
	  false))

   (string->index (js-jsstring->string obj)))

;*---------------------------------------------------------------------*/
;*    js-toindex ::JsStringLiteralIndex ...                            */
;*---------------------------------------------------------------------*/
(define-method (js-toindex obj::JsStringLiteralIndex)
   (with-access::JsStringLiteralIndex obj (index)
      index))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsStringLiteral ...                            */
;*    -------------------------------------------------------------    */
;*    See runtime/js_comp.scm in the Hop library for the definition    */
;*    of the generic.                                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsStringLiteral op compile isexpr ctx)
   (hop->javascript (js-jsstring->string o) op compile isexpr ctx))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::JsStringLiteral ...                        */
;*---------------------------------------------------------------------*/
(define-method (xml-primitive-value o::JsStringLiteral ctx)
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
   (js-string->jsstring (js-jsstring->string obj)))

;*---------------------------------------------------------------------*/
;*    scheme->response ::JsStringLiteral ...                           */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::JsStringLiteral req ctx)
   (scheme->response (js-jsstring->string obj) req ctx))

;*---------------------------------------------------------------------*/
;*    js-cast-object ...                                               */
;*---------------------------------------------------------------------*/
(define-method (js-cast-object obj::JsStringLiteral %this name)
   (with-access::JsGlobalObject %this (js-string)
      (js-new1 %this js-string obj)))

;*---------------------------------------------------------------------*/
;*    js-substring ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-substring s::bstring start::long end::long %this)
   (if (=fx end (+fx start 1))
       (with-access::JsGlobalObject %this (char-table)
          (vector-ref char-table (char->integer (string-ref s start))))
       (js-substring->jsstring s start (-fx end start))))

;*---------------------------------------------------------------------*/
;*    js-substring/enc ...                                             */
;*---------------------------------------------------------------------*/
(define (js-substring/enc s::bstring start::long end::long utf8 %this)
   (cond
      ((=fx end (+fx start 1))
       (with-access::JsGlobalObject %this (char-table)
          (vector-ref char-table (char->integer (string-ref s start)))))
      (utf8
       (js-utf8->jsstring (substring s start end)))
      (else
       (js-substring->jsstring s start (-fx end start)))))

;*---------------------------------------------------------------------*/
;*    string-dispatch ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (string-dispatch fun this . args)
   `(cond
       ((js-jsstring-ascii? ,this)
	(,(symbol-append 'ascii- fun)
	 (if (js-jsstring-normalized? ,this)
	     (with-access::JsStringLiteral ,this (left) left)
	     (js-jsstring-normalize-ASCII! ,this))
	 ,@args))
       ((js-jsstring-substring? ,this)
	(,(symbol-append 'ascii- fun)
	 (js-jsstring-normalize-SUBSTRING! ,this)
	,@args))
       (else
	(,(symbol-append 'utf8- fun)
	 (js-jsstring-normalize-UTF8! ,this) ,@args))))

;*---------------------------------------------------------------------*/
;*    js-ascii->jsstring ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-ascii->jsstring::JsStringLiteralASCII val::bstring)
   (let ((o (instantiate::JsStringLiteralASCII
	       (length (fixnum->uint32 (string-length val)))
	       (left val))))
      (js-object-mode-set! o (js-jsstring-normalized-ascii-mode))
      (object-widening-set! o #f)
      o))

;*---------------------------------------------------------------------*/
;*    js-substring->jsstring ...                                       */
;*---------------------------------------------------------------------*/
(define (js-substring->jsstring::JsStringLiteral val::bstring start::long len::long)
   (cond
      ((=fx len 0)
       (& ""))
      ((and (=fx start 0) (=fx len (string-length val)))
       (js-ascii->jsstring val))
      ((<u32 (fixnum->uint32 len) (string-append-auto-normalize-threshold))
       (js-ascii->jsstring (substring val start (+fx start len))))
      (else
       (let ((o (instantiate::JsStringLiteralSubstring
		   (length (fixnum->uint32 len))
		   (left val)
		   (right start))))
	  (js-object-mode-set! o (js-jsstring-default-substring-mode))
	  (object-widening-set! o #f)
	  o))))

;*---------------------------------------------------------------------*/
;*    js-utf8->jsstring ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-utf8->jsstring::JsStringLiteralUTF8 val::bstring)
   (let ((o (instantiate::JsStringLiteralUTF8
	       (length (fixnum->uint32 (string-length val)))
	       (left val))))
      (js-object-mode-set! o (js-jsstring-normalized-utf8-mode))
      (object-widening-set! o #f)
      o))

;*---------------------------------------------------------------------*/
;*    js-utf8->jsstring ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-utf8->jsstring/ulen::JsStringLiteralUTF8 val::bstring ulen::uint32)
   (let ((o (instantiate::JsStringLiteralUTF8
	       (length (fixnum->uint32 (string-length val)))
	       (%culen ulen)
	       (left val))))
      (js-object-mode-set! o (js-jsstring-normalized-utf8-mode))
      (object-widening-set! o #f)
      o))

;*---------------------------------------------------------------------*/
;*    js-string->jsstring ...                                          */
;*    -------------------------------------------------------------    */
;*    Create a JsStringLiteral from a Scheme string literal.           */
;*---------------------------------------------------------------------*/
(define (js-string->jsstring::JsStringLiteral val::bstring)
   (let ((enc (string-minimal-charset val)))
      (case enc
	 ((ascii) (js-ascii->jsstring val))
	 ((latin1) (js-utf8->jsstring val))
	 (else (error "string->jsstring" "unsupported encoding" enc)))))

;*---------------------------------------------------------------------*/
;*    js-symbol->jsstring ...                                          */
;*---------------------------------------------------------------------*/
(define (js-symbol->jsstring::JsStringLiteral val::symbol)
   (js-string->jsstring (symbol->string! val)))

;*---------------------------------------------------------------------*/
;*    js-keyword->jsstring ...                                         */
;*---------------------------------------------------------------------*/
(define (js-keyword->jsstring::JsStringLiteral val::keyword)
   (js-string->jsstring (keyword->string! val)))

;*---------------------------------------------------------------------*/
;*    js-integer->jsstring ...                                         */
;*---------------------------------------------------------------------*/
(define (js-integer->jsstring::JsStringLiteralASCII num::long)
   (cond
      ((js-integer-name num)
       =>
       (lambda (str) str))
      ((<fx num 0)
       (js-ascii->jsstring (integer->string num)))
      ((<fx num (bit-lsh 1 29))
       (let ((str (fixnum->string num)))
	  (let ((o (instantiate::JsStringLiteralIndex
		      (length (string-length str))
		      (left str)
		      (index (fixnum->uint32 num)))))
	     (js-object-mode-set! o (js-jsstring-normalized-index-mode))
	     (object-widening-set! o #f)
	     o)))
      (else
       (js-ascii->jsstring (integer->string num)))))
   
;*---------------------------------------------------------------------*/
;*    js-stringlist->jsstring ...                                      */
;*    -------------------------------------------------------------    */
;*    Create a JsStringLiteral from a list of Scheme string literals.  */
;*---------------------------------------------------------------------*/
(define (js-stringlist->jsstring val::pair-nil)
   (cond
      ((null? val)
       (& ""))
      ((null? (cdr val))
       (js-string->jsstring (car val)))
      ((null? (cddr val))
       (js-jsstring-append
	  (js-string->jsstring (car val))
	  (js-string->jsstring (cadr val))))
      (else
       (let ((lav (reverse! val)))
	  (let loop ((lav (cdr lav))
		     (acc (js-string->jsstring (car lav))))
	     (if (null? (cdr lav))
		 (js-jsstring-append (js-string->jsstring (car lav)) acc)
		 (loop (cdr lav)
		    (js-jsstring-append
		       (js-string->jsstring (car lav)) acc))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring->string ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring->string::bstring js::JsStringLiteral)
   (with-access::JsStringLiteral js (left length)
      (unless (js-jsstring-normalized? js)
	 (js-jsstring-normalize! js))
      left))

;*---------------------------------------------------------------------*/
;*    js-jsstring->number ...                                          */
;*---------------------------------------------------------------------*/
(define (js-jsstring->number::obj js::JsStringLiteral)
   (cond
      ((js-jsstring-index? js)
       (uint32->fixnum (js-toindex js)))
      (else
       (string->number (js-jsstring->string js)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-mark-normalized! ...                                 */
;*---------------------------------------------------------------------*/
(define (js-jsstring-mark-normalized! js::JsStringLiteral buffer::bstring)
   (with-access::JsStringLiteral js (left right)
      (js-jsstring-normalized! js)
      (set! left buffer)
      (set! right (js-not-a-string-cache))
      js))

;*---------------------------------------------------------------------*/
;*    js-jsstring-depth ...                                            */
;*---------------------------------------------------------------------*/
(define (js-jsstring-depth s limit)
   (if (or (js-jsstring-normalized? s) (js-jsstring-substring? s))
       0
       (with-access::JsStringLiteral s (left right)
	  (let ((ld (js-jsstring-depth left limit)))
	     (if (>=fx ld limit)
		 ld
		 (let ((rd (js-jsstring-depth right limit)))
		    (if (>=fx rd limit)
			rd
			(+fx 1 (maxfx ld rd)))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize-ASCII! ...                                 */
;*---------------------------------------------------------------------*/
(define (js-jsstring-normalize-ASCII!::bstring js::JsStringLiteralASCII)
   ;; BGl_jszd2jsstringzd2normaliza7ezd2ASCIIz12z67zz__hopscript_stringliteralz00
   
   (define (blit-buffer!::long s::bstring buffer::bstring i::long len::long)
      (case len
	 ((1) (string-set! buffer i (string-ref s 0)))
	 (else (blit-string! s 0 buffer i len))))
   
   (define (blit-subbuffer!::long s::bstring buffer::bstring i::long start::long len::long)
      (blit-string! s start buffer i len))
   
   (define (normalize-small! js::JsStringLiteral)
      ;; small strings, no need for tail recursion
      (let ((buffer (with-access::JsStringLiteral js (length)
		       (make-string (uint32->fixnum length)))))
	 (let loop ((i 0)
		    (s js))
	    (with-access::JsStringLiteral s (left right)
	       (with-access::JsStringLiteral left ((llength length)
						   (lstr left))
		  (with-access::JsStringLiteral right ((rlength length)
						       (rstr left))
		     (let ((len (uint32->fixnum llength)))
			(cond
			   ((js-jsstring-normalized? left)
			    (blit-buffer! lstr buffer i len)
			    (cond
			       ((js-jsstring-normalized? right)
				(blit-buffer! rstr buffer (+fx i len) (uint32->fixnum rlength)))
			       ((js-jsstring-substring? right)
				(with-access::JsStringLiteralSubstring right ((start right))
				   (blit-subbuffer! rstr buffer (+fx i len) start (uint32->fixnum rlength))))
			       (else
				(loop (+fx i len) right))))
			   ((js-jsstring-normalized? right)
			    (blit-buffer! rstr buffer (+fx i len) (uint32->fixnum rlength))
			    (if (js-jsstring-substring? left)
				(with-access::JsStringLiteralSubstring left ((start right))
				   (blit-subbuffer! lstr buffer i start len))
				(loop i left)))
			   ((js-jsstring-substring? left)
			    (with-access::JsStringLiteralSubstring left ((start right))
			       (blit-subbuffer! lstr buffer i start len)
			       (cond
				  ((js-jsstring-normalized? right)
				   (blit-buffer! rstr buffer (+fx i len) (uint32->fixnum rlength)))
				  ((js-jsstring-substring? right)
				   (with-access::JsStringLiteralSubstring right ((start right))
				      (blit-subbuffer! rstr buffer (+fx i len) start (uint32->fixnum rlength))))
				  (else
				   (loop (+fx i len) right)))))
			   ((js-jsstring-substring? right)
			    (with-access::JsStringLiteralSubstring right ((start right))
			       (blit-subbuffer! rstr buffer (+fx i len) start (uint32->fixnum rlength)))
			    (loop i left))
			   ((<u32 len (uint32->fixnum rlength))
			    ;; left is smaller, recurse over left
			    (loop i left)
			    (loop (+fx i len) right))
			   (else
			    (loop (+fx i len) right)
			    (loop i left))))))))
	 (js-jsstring-mark-normalized! js buffer)
	 buffer))
   
   (define (normalize-big! js::JsStringLiteral)
      ;; tail recursive with heap allocated stack
      (let ((buffer (with-access::JsStringLiteral js (length)
		       (make-string (uint32->fixnum length)))))
	 (let loop ((i 0)
		    (s js)
		    (stack '()))
	    (with-access::JsStringLiteral s (left right)
	       (cond
		  ((js-jsstring-normalized? s)
		   (blit-buffer! left buffer i (string-length left))
		   (if (pair? stack)
		       (let* ((top (car stack))
			      (ni (car top))
			      (s (cdr top)))
			  (loop ni s (cdr stack)))
		       (begin
			  (js-jsstring-mark-normalized! js buffer)
			  buffer)))
		  ((js-jsstring-substring? s)
		   (with-access::JsStringLiteralSubstring s ((start right) (llen length))
		      (blit-subbuffer! left buffer i start (uint32->fixnum llen))
		      (if (pair? stack)
			  (let* ((top (car stack))
				 (ni (car top))
				 (s (cdr top)))
			     (loop ni s (cdr stack)))
			  (begin
			     (js-jsstring-mark-normalized! js buffer)
			     buffer))))
		  ((js-jsstring-normalized? left)
		   (with-access::JsStringLiteral left ((str left))
		      (blit-buffer! str buffer i (string-length str))
		      (loop (+fx i (string-length str)) right stack)))
		  ((js-jsstring-substring? left)
		   (with-access::JsStringLiteralSubstring left ((str left) (llength length) (start right))
		      (blit-subbuffer! str buffer i start (uint32->fixnum llength))
		      (loop (+fx i (uint32->fixnum llength)) right stack)))
		  (else
		   (with-access::JsStringLiteral left (length)
		      (let ((len (uint32->fixnum length)))
			 (cond
			    ((js-jsstring-normalized? right)
			     (with-access::JsStringLiteral right ((str left))
				;; write the rhs in advance
				(let ((rlen (string-length str)))
				   (blit-buffer! str buffer (+fx i len) rlen)
				   (loop i left stack))))
			    ((js-jsstring-substring? right)
			     (with-access::JsStringLiteralSubstring right ((str left) (start right) (rlen length))
				(blit-subbuffer! str buffer (+fx i len) start (uint32->fixnum rlen))
				(loop i left stack)))
			    (else
			     (let* ((ni (+fx i len))
				    (nstack (cons (cons ni right) stack)))
				(loop i left nstack))))))))))))

   (with-access::JsStringLiteral js (length left)
      (cond
	 ((js-jsstring-normalized? js)
	  left)
	 ((<u32 length #u32:16384)
	  (normalize-small! js))
	 ((< (js-jsstring-depth js 1024) 1024)
	  (normalize-small! js))
	 (else
	  (normalize-big! js)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize-UTF8! ...                                  */
;*---------------------------------------------------------------------*/
(define (js-jsstring-normalize-UTF8! js::JsStringLiteralUTF8)
   ;; BGl_jszd2jsstringzd2normaliza7ezd2UTF8z12z67zz__hopscript_stringliteralz00
   
   (define (blit-utf8-buffer!::long s::bstring buffer::bstring i::long)
      (let ((len (string-length s)))
	 (case len
	    ((0) i)
	    ((1) (begin (string-set! buffer i (string-ref s 0)) (+fx i 1)))
	    (else (utf8-string-append-fill! buffer i s)))))
   
   (define (blit-buffer!::long s::bstring buffer::bstring i::long len::long)
      (case len
	 ((1) (string-set! buffer i (string-ref s 0)))
	 (else (blit-string! s 0 buffer i len))))

   (define (blit-subbuffer!::long s::bstring buffer::bstring i::long start::long len::long)
      (blit-string! s start buffer i len))
   
   (define (string-weight js::JsStringLiteral)
      (with-access::JsStringLiteral js (left)
	 (with-access::JsStringLiteral left (length)
	    (uint32->fixnum length))))
   
   (define (normalize-small! js)
      (with-access::JsStringLiteralUTF8 js (length %idxutf8 %idxstr)
	 ;; small strings, no need for tail recursion
	 (let* ((len length)
		(buffer (make-string (uint32->fixnum len))))
	    (let ((ni (let loop ((i 0)
				 (s js))
			 (with-access::JsStringLiteral s (left right (llen length))
			    (if (string? left)
				(if (js-jsstring-substring? s)
				    (blit-subbuffer! left buffer i right (uint32->fixnum llen))
				    (blit-utf8-buffer! left buffer i))
				(let ((ni (loop i left)))
				   (unless (js-jsstring-normalized? s)
				      (loop ni right))))))))
	       (string-shrink! buffer ni)
	       (js-jsstring-mark-normalized! js buffer)
	       (set! length (fixnum->uint32 (string-length buffer)))
	       buffer))))

   (define (normalize-big! js::JsStringLiteral)
      ;; tail recursive with heap allocated stack
      (let ((buffer (with-access::JsStringLiteral js (length)
		       (make-string (uint32->fixnum length)))))
	 (let loop ((i 0)
		    (s js)
		    (stack '()))
	    (with-access::JsStringLiteral s (left right)
	       (cond
		  ((js-jsstring-normalized? s)
		   (let ((ni (cond
				((js-jsstring-ascii? s)
				 (let ((len (string-length left)))
				    (blit-buffer! left buffer i len)
				    (+fx i len)))
				(else
				 (blit-utf8-buffer! left buffer i)))))
		      (if (pair? stack)
			  (let* ((top (car stack))
				 (ni (car top))
				 (s (cdr top)))
			     (loop ni s (cdr stack)))
			  (begin
			     (js-jsstring-mark-normalized! js buffer)
			     buffer))))
		  ((js-jsstring-substring? s)
		   (with-access::JsStringLiteralSubstring s (length (start right))
		      (let ((ni (let ((len (uint32->fixnum length)))
				   (blit-subbuffer! left buffer i start len)
				   (+fx i len))))
			 (if (pair? stack)
			     (let* ((top (car stack))
				    (ni (car top))
				    (s (cdr top)))
				(loop ni s (cdr stack)))
			     (begin
				(js-jsstring-mark-normalized! js buffer)
				buffer)))))
		  ((js-jsstring-normalized? left)
		   (with-access::JsStringLiteral left ((str left))
		      (if (js-jsstring-ascii? left)
			  (let ((len (string-length str)))
			     (blit-buffer! str buffer i len)
			     (loop (+fx i len) right stack))
			  (let ((ni (blit-utf8-buffer! str buffer i)))
			     (loop ni right stack)))))
		  ((js-jsstring-substring? left)
		   (with-access::JsStringLiteralSubstring left ((str left) (llen length) (start right))
		      (let ((len (uint32->fixnum llen)))
			 (blit-subbuffer! str buffer i start len)
			 (loop (+fx i len) right stack))))
		  (else
		   (if (and (js-jsstring-normalized? right)
			    (not (js-jsstring-utf8? left)))
		       (with-access::JsStringLiteral right ((str left))
			  ;; write the rhs in advance
			  (with-access::JsStringLiteral left (length)
			     (let ((ni (blit-utf8-buffer! str buffer
					  (+fx i (uint32->fixnum length)))))
				(loop i left stack))))
		       ;; full recursive call with pushed right
		       (let* ((ni (+fx i (string-weight s)))
			      (nstack (cons (cons ni right) stack)))
			  (loop i left nstack)))))))))

   (with-access::JsStringLiteralUTF8 js (length left)
      (cond
	 ((js-jsstring-normalized? js)
	  left)
	 ((<u32 length #u32:16384)
	  (normalize-small! js))
	 ((< (js-jsstring-depth js 1024) 1024)
	  (normalize-small! js))
	 (else
	  (normalize-big! js)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize-SUBSTRING! ...                             */
;*---------------------------------------------------------------------*/
(define (js-jsstring-normalize-SUBSTRING! js::JsStringLiteralSubstring)
   (with-access::JsStringLiteralSubstring js (left right length)
      (js-object-mode-set! js (js-jsstring-normalized-ascii-mode))
      (let ((buffer (if (and (=fx right 0) (=fx (uint32->fixnum length) (string-length left)))
			left
			(substring left right (+fx right (uint32->fixnum length))))))
	 (set! left buffer)
	 (set! right (js-not-a-string-cache))
	 buffer)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize! ...                                       */
;*    -------------------------------------------------------------    */
;*    Tailrec normalization (with explicit stack management).          */
;*---------------------------------------------------------------------*/
(define (js-jsstring-normalize!::JsStringLiteral js::JsStringLiteral)
   (cond
      ((js-jsstring-normalized? js)
       js)
      ((js-jsstring-ascii? js)
       (js-jsstring-normalize-ASCII! js)
       js)
      ((js-jsstring-utf8? js)
       (js-jsstring-normalize-UTF8! js)
       js)
      (else
       (js-jsstring-normalize-SUBSTRING! js)
       js)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-length ...                                           */
;*    -------------------------------------------------------------    */
;*    Returns the number of scheme caracters (i.e., bytes) forming the */
;*    string. For an UTF* string, this number differs from the number  */
;*    of letters of that string.                                       */
;*---------------------------------------------------------------------*/
(define (js-jsstring-length::uint32 js::JsStringLiteral)
   (with-access::JsStringLiteral js (length)
      length))

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
   (if (js-jsstring-ascii? js)
       (with-access::JsStringLiteralASCII js (length)
	  length)
       (fixnum->uint32 (utf8-string-length (js-jsstring->string js)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-codeunit-length ...                                  */
;*    -------------------------------------------------------------    */
;*    This function implements the JavaScript string length, which     */
;*    counts the number of UCS2 characters of the string.              */
;*---------------------------------------------------------------------*/
(define (js-jsstring-codeunit-length js)
   (cond
      ((js-jsstring-ascii? js)
       (with-access::JsStringLiteralASCII js (length)
	  length))
      ((js-jsstring-substring? js)
       (with-access::JsStringLiteralSubstring js (length)
	  length))
      (else
       (with-access::JsStringLiteralUTF8 js (%culen)
	  (when (=u32 %culen #u32:0)
	     (set! %culen
		(fixnum->uint32
		   (utf8-codeunit-length (js-jsstring->string js)))))
	  %culen))))

;*---------------------------------------------------------------------*/
;*    display-js-string ...                                            */
;*---------------------------------------------------------------------*/
(define (display-js-string jstr::JsStringLiteral op)
   (display (js-jsstring->string jstr) op))

;*---------------------------------------------------------------------*/
;*    hop-register-value ::JsStringLiteral ...                         */
;*---------------------------------------------------------------------*/
(define-method (hop-register-value s::JsStringLiteral register)
   #t)

;*---------------------------------------------------------------------*/
;*    js-string->bool ...                                              */
;*---------------------------------------------------------------------*/
(define (js-string->bool::bool str::bstring)
   (>fx (string-length str) 0))

;*---------------------------------------------------------------------*/
;*    js-jsstring-toboolean ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-toboolean::bool s::JsStringLiteral)
   (with-access::JsStringLiteral s (length)
      (>u32 length #u32:0)))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-null? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-null? js)
   (with-access::JsStringLiteral js (length)
      (=u32 length #u32:0)))

;*---------------------------------------------------------------------*/
;*    js-jsstring=? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring=?::bool x y)
   (or (eq? x y)
       (string=? (js-jsstring->string x) (js-jsstring->string y))))

;*---------------------------------------------------------------------*/
;*    js-jsstring>? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring>?::bool x y)
   (string>? (js-jsstring->string x) (js-jsstring->string y)))

;*---------------------------------------------------------------------*/
;*    js-jsstring>=? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring>=?::bool x y)
   (string>=? (js-jsstring->string x) (js-jsstring->string y)))

;*---------------------------------------------------------------------*/
;*    js-jsstring<? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring<?::bool x y)
   (string<? (js-jsstring->string x) (js-jsstring->string y)))

;*---------------------------------------------------------------------*/
;*    js-jsstring<=? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring<=?::bool x y)
   (string<=? (js-jsstring->string x) (js-jsstring->string y)))

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
	  (js-string-parseint str #s32:16 #t))
	 ((string-index str "eE.")
	  (js-string-parsefloat str #t))
	 (else
	  (js-string-parseint str #s32:10 #t)))))

;*---------------------------------------------------------------------*/
;*    js-string-parseint ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.2     */
;*---------------------------------------------------------------------*/
(define (js-string-parseint s::bstring r::int32 strict-syntax::bool)
   
   (define (integer v s r)
      (if (and (fixnum? v) (=fx v 0))
	  (cond
	     ((string-prefix? "0" s) v)
	     ((string-prefix? "+0" s) v)
	     ((string-prefix? "-0" s) -0.0)
	     ((string=? s "+inf.0") +nan.0)
	     ((string=? s "-inf.0") +nan.0)
	     ((string=? s "+nan.0") +nan.0)
	     (else +nan.0))
	  v))
   
   (define (shrink n)
      (let ((r (+ n 0)))
	 (if (fixnum? r)
	     r
	     (bignum->flonum r))))

   (define radix-charset
      '#(unspecified
	 #unspecified
	 "01"
	 "012"
	 "0123"
	 "01234"
	 "012345"
	 "0123456"
	 "01234567"
	 "012345678"
	 "0123456789"
	 "0123456789aA"
	 "0123456789aAbB"
	 "0123456789aAbBcC"
	 "0123456789aAbBcCdD"
	 "0123456789aAbBcCdDeE"
	 "0123456789aAbBcCdDeEfF"
	 "0123456789aAbBcCdDeEfFgG"
	 "0123456789aAbBcCdDeEfFgGhH"
	 "0123456789aAbBcCdDeEfFgGhHiI"
	 "0123456789aAbBcCdDeEfFgGhHiIjJ"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkK"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlL"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmM"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnN"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoO"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpP"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQ"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrR"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsS"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStT"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuU"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvV"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwW"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxX"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyY"
	 "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"))
	 
   (define (string->bignum-safe s r)
      (let ((v (string->bignum s r)))
	 (if (=bx v #z0)
	     (if strict-syntax
		 (let ((i (string-skip s (vector-ref radix-charset r))))
		    (if i
			+nan.0
			0))
		 (let ((i (string-skip s (vector-ref radix-charset r))))
		    (if i
			(shrink (string->bignum (substring s 0 i) r))
			0)))
	     (shrink v))))
   
   (define (str->integer s r)
      (if strict-syntax
	  (or (string->number s r) +nan.0)
	  (string->integer s r)))

   (define (max-int-string)
      (cond-expand
	 ((or bint61 bint64) 19)
	 (else 8)))

   (define (max-hex-string)
      (cond-expand
	 ((or bint61 bint64) 12)
	 (else 7)))

   (let ((l (string-length s)))
      (cond
	 ((and (not (zeros32? r))
	       (or (<s32 r #s32:2)
		   (>s32 r #s32:36)))
	  +nan.0)
	 ((and (or (zeros32? r) (=s32 r #s32:16))
	       (>=fx (string-length s) 2)
	       (char=? (string-ref s 0) #\0)
	       (or (char=? (string-ref s 1) #\x)
		   (char=? (string-ref s 1) #\X)))
	  (let ((s (substring s 2)))
	     (if (<=fx l 9)
		 (integer (str->integer s 16) s 16)
		 (integer (string->bignum-safe s 16) s 16))))
	 ((zeros32? r)
	  (if (<=fx l 8)
	      (integer (str->integer s 10) s 10)
	      (integer (string->bignum-safe s 10) s 10)))
	 (else
	  (let ((r (int32->fixnum r)))
	     (if (<=fx l (cond
			    ((<=fx r 10) (max-int-string))
			    ((<=fx r 16) (max-hex-string))
			    (else 5)))
		 (integer (str->integer s r) s 10)
		 (integer (string->bignum-safe s r) s r)))))))

;*---------------------------------------------------------------------*/
;*    js-parsefloat ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.3     */
;*---------------------------------------------------------------------*/
(define (js-string-parsefloat s::bstring strict::bool)
   (let ((l (string-length s)))
      (cond
	 ((=fx l 0)
	  +nan.0)
	 ((char=? (string-ref s 0) #\E)
	  +nan.0)
	 ((and (>=fx l 2)
	       (char=? (string-ref s 0) #\0)
	       (or (char=? (string-ref s 1) #\x)
		   (char=? (string-ref s 1) #\X)))
	  0.)
	 (else
	  (let ((n (string->real s)))
	     (cond
		((=fl n 0.)
		 (cond
		    ((pregexp-match "^(?:[-+]?)(?:0+.?0*|.?0+)$" s) 0.)
		    (strict +nan.0)
		    ((pregexp-match "(?:[-+]?)(?:0+.?0*|.?0+)" s) 0.)
		    (else +nan.0)))
		((= n +inf.0)
		 (if (string=? s "infinity") +nan.0 n))
		(else
		 n)))))))

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
;*    js-tonumber ::JsStringLiteral ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tonumber this::JsStringLiteralIndex %this)
   (with-access::JsStringLiteralIndex this (index)
      (js-uint32-tointeger (js-toindex this))))

;*---------------------------------------------------------------------*/
;*    js-tointeger ::JsStringLiteral ...                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-method (js-tointeger this::JsStringLiteral %this)
   (js-tointeger (js-jsstring-tonumber this %this) %this))

;*---------------------------------------------------------------------*/
;*    js-tointeger ::JsStringLiteralIndex ...                          */
;*---------------------------------------------------------------------*/
(define-method (js-tointeger this::JsStringLiteralIndex %this)
   (with-access::JsStringLiteralIndex this (index)
      (js-uint32-tointeger (js-toindex this))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-append-ASCII ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-append-ASCII left right)
   (with-access::JsStringLiteral left ((llen length)
				       (lstr left))
      (with-access::JsStringLiteral right ((rlen length)
					   (rstr left))
	 (let ((len (+u32 llen rlen)))
	    (if (and (<u32 len (string-append-auto-normalize-threshold))
		     (js-jsstring-normalized? left)
		     (js-jsstring-normalized? right))
		;; if the sum len if smaller than 18, both string 
		;; lengthes are smaller than 18 too!
		(let ((s (instantiate::JsStringLiteralASCII
			    (length len)
			    (left (string-append lstr rstr))
			    (right (js-not-a-string-cache)))))
		   (js-object-mode-set! s (js-jsstring-normalized-ascii-mode))
		   (object-widening-set! s #f)
		   s)      
		(let ((s (instantiate::JsStringLiteralASCII
			    (length len)
			    (left left)
			    (right right))))
		   (js-object-mode-set! s (js-jsstring-default-ascii-mode))
		   (object-widening-set! s #f)
		   s))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-append-UTF8 ...                                      */
;*---------------------------------------------------------------------*/
(define (js-jsstring-append-UTF8 left right)
   (with-access::JsStringLiteral left ((llen length)
				       (lstr left))
      (with-access::JsStringLiteral right ((rlen length)
					   (rstr left))
	 (let ((len (+u32 llen rlen)))
	    (if (<u32 len (string-append-auto-normalize-threshold))
		(let ((s (instantiate::JsStringLiteralUTF8
			    (length len)
			    (left (utf8-string-append lstr rstr))
			    (right (js-not-a-string-cache)))))
		   (js-object-mode-set! s (js-jsstring-normalized-utf8-mode))
		   (object-widening-set! s #f)
		   s)
		(let ((s (instantiate::JsStringLiteralUTF8
			    (length len)
			    (left left)
			    (right right))))
		   (js-object-mode-set! s (js-jsstring-default-utf8-mode))
		   (object-widening-set! s #f)
		   s))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-append ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-append::JsStringLiteral left::JsStringLiteral right::JsStringLiteral)
   (with-access::JsStringLiteral left ((llen length))
      (with-access::JsStringLiteral right ((rlen length))
	 (cond
	    ((=u32 llen 0)
	     right)
	    ((=u32 rlen 0)
	     left)
	    ((or (js-jsstring-utf8? left) (js-jsstring-utf8? right))
	     (js-jsstring-append-UTF8 left right))
	    (else
	     (js-jsstring-append-ASCII left right))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-append3 ...                                          */
;*---------------------------------------------------------------------*/
(define (js-jsstring-append3::JsStringLiteral left::JsStringLiteral middle::JsStringLiteral right::JsStringLiteral)
   (with-access::JsStringLiteral left ((llen length))
      (with-access::JsStringLiteral right ((rlen length))
	 (if (or (js-jsstring-utf8? left) (js-jsstring-utf8? middle) (js-jsstring-utf8? right))
	     (js-jsstring-append-UTF8 left
		(js-jsstring-append-UTF8 middle right))
	     (js-jsstring-append-ascii3 left middle right)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-append-no-inline ...                                 */
;*---------------------------------------------------------------------*/
(define (js-jsstring-append-no-inline::JsStringLiteral left::JsStringLiteral right::JsStringLiteral)
   (js-jsstring-append left right))

;*---------------------------------------------------------------------*/
;*    js-jsstring-append-ascii ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-append-ascii::JsStringLiteral left::JsStringLiteral right::JsStringLiteral)
   (with-access::JsStringLiteral left ((llen length))
      (with-access::JsStringLiteral right ((rlen length))
	 (cond
	    ((=u32 llen 0) right)
	    ((=u32 rlen 0) left)
	    (else (js-jsstring-append-ASCII left right))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-append-ascii3 ...                                    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-append-ascii3::JsStringLiteral left::JsStringLiteral  middle::JsStringLiteral right::JsStringLiteral)
   (with-access::JsStringLiteral left ((llen length) (lstr left))
      (with-access::JsStringLiteral middle ((mlen length) (mstr left))
	 (with-access::JsStringLiteral right ((rlen length) (rstr left))
	    (let ((len (+u32 llen (+u32 mlen rlen))))
	       (cond
		  ((<u32 len (string-append-auto-normalize-threshold))
		   (let ((s (instantiate::JsStringLiteralASCII
			       (length len)
			       (left (string-append lstr mstr rstr))
			       (right (js-not-a-string-cache)))))
		      (js-object-mode-set! s (js-jsstring-normalized-ascii-mode))
		      (object-widening-set! s #f)
		      s))
		  ((>u32 llen rlen)
		   (js-jsstring-append-ASCII left
		      (js-jsstring-append-ASCII middle right)))
		  (else
		   (js-jsstring-append-ASCII 
		      (js-jsstring-append-ASCII left middle)
		      right))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-concat ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-concat left right)
   (js-jsstring-append left right))

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
;*    Returns the number of UTF16 code units of this code point.       */
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
(define (utf8-codeunit-ref this::JsStringLiteralUTF8 str::bstring i::long)
   
   (define (return-utf8 this str i j c s r u)
      (with-access::JsStringLiteralUTF8 this (%idxutf8 %idxstr)
	 (set! %idxstr r)
	 (set! %idxutf8 u)
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
		(ucs2->integer (ucs2-string-ref ucs2 j)))))))
   
   (define (rollback-utf8 this str i)
      (with-access::JsStringLiteralUTF8 this (%idxutf8 %idxstr)
	 (let loop ((r %idxstr)
		    (j %idxutf8))
	    (let liip ((r r)
		       (s 1))
	       (cond
		  ((<fx r 0)
		   +nan.0)
		  ((=fx (bit-and (char->integer (string-ref-ur str r)) #xc0)
		      #x80)
		   (liip (-fx r 1) (+fx s 1)))
		  (else
		   (let* ((c (string-ref-ur str r))
			  (u (codepoint-length c)))
		      (if (<fx (-fx j u) i)
			  (return-utf8 this str i j c s r j)
			  (loop (-fx r 1) (-fx j u))))))))))
   
   (let ((len (string-length str)))
      (with-access::JsStringLiteralUTF8 this (%idxutf8 %idxstr)
	 ;; adjust with respect to the last position
	 (if (and (<fx i %idxutf8)
		  (>fx i 0)
		  (>=fx i (- %idxutf8 i)))
	     ;; rollback utf8 indexes
	     (rollback-utf8 this str i)
	     ;; look forward
	     (let loop ((r (if (>=fx i %idxutf8) %idxstr 0))
			(j (if (>=fx i %idxutf8) (-fx i %idxutf8) i)))
		(if (>=fx r len)
		    +nan.0
		    (let* ((c (string-ref-ur str r))
			   (s (utf8-char-size c))
			   (u (codepoint-length c)))
		       (if (>=fx j u)
			   (loop (+fx r s) (-fx j u))
			   (return-utf8 this str i j c s r (-fx i j))))))))))

;*---------------------------------------------------------------------*/
;*    utf8-codepoint-ref ...                                           */
;*    -------------------------------------------------------------    */
;*    Returns the ith code point (UTF16 code point) of the UTF8 source */
;*    string.                                                          */
;*---------------------------------------------------------------------*/
(define (utf8-codepoint-ref this::JsStringLiteralUTF8 str::bstring i::long)

   (define (return-utf8-sans-adjust str c s r)
      (case s
	 ((1)
	  c)
	 ((2)
	  (let ((c1 (char->integer (string-ref-ur str (+fx r 1)))))
	     (bit-or (bit-lsh (bit-and c #x1f) 6)
		(bit-and c1 #x3f))))
	 ((3)
	  (let ((c1 (char->integer (string-ref-ur str (+fx r 1))))
		(c2 (char->integer (string-ref-ur str (+fx r 2)))))
	     (bit-or (bit-lsh (bit-and c #xf) 12)
		(bit-or (bit-lsh (bit-and c1 #x3f) 6)
		   (bit-and c2 #x3f)))))
	 (else
	  (let ((c1 (char->integer (string-ref-ur str (+fx r 1))))
		(c2 (char->integer (string-ref-ur str (+fx r 2))))
		(c3 (char->integer (string-ref-ur str (+fx r 3)))))
	     (bit-or (bit-lsh (bit-and c #x7) 18)
		(bit-or (bit-lsh (bit-and c1 #x3f) 12)
		   (bit-or (bit-lsh (bit-and c2 #x3f) 6)
		      (bit-and c3 #x3f))))))))
   
   (define (return-utf8 this str c s r u)
      (with-access::JsStringLiteralUTF8 this (%idxutf8 %idxstr)
	 (set! %idxstr r)
	 (set! %idxutf8 u)
	 (return-utf8-sans-adjust str c s r)))
   
   (define (rollback-utf8 this str i)
      (with-access::JsStringLiteralUTF8 this (%idxutf8 %idxstr)
	 (let loop ((r %idxstr)
		    (j %idxutf8))
	    (let liip ((r r)
		       (s 1))
	       (cond
		  ((<fx r 0)
		   +nan.0)
		  ((=fx (bit-and (char->integer (string-ref-ur str r)) #xc0)
		      #x80)
		   (liip (-fx r 1) (+fx s 1)))
		  (else
		   (let* ((c (string-ref-ur str r))
			  (u (codepoint-length c)))
		      (if (<fx (-fx j u) i)
			  (return-utf8 this str (char->integer c) s r j)
			  (loop (-fx r 1) (-fx j u))))))))))
   
   (let ((len (string-length str)))
      (with-access::JsStringLiteralUTF8 this (%idxutf8 %idxstr)
	 (cond
	    ((=fx i 0)
	     (let* ((c (string-ref-ur str 0))
		    (s (utf8-char-size c)))
		(return-utf8-sans-adjust str (char->integer c) s 0)))
	    ;; adjust with respect to the last position
	    ((and (<fx i %idxutf8)
		  (>fx i 0)
		  (>=fx i (- %idxutf8 i)))
	     ;; rollback utf8 indexes
	     (rollback-utf8 this str i))
	    (else
	     ;; look forward
	     (let loop ((r (if (>=fx i %idxutf8) %idxstr 0))
			(j (if (>=fx i %idxutf8) (-fx i %idxutf8) i)))
		(if (>=fx r len)
		    +nan.0
		    (let* ((c (string-ref-ur str r))
			   (s (utf8-char-size c))
			   (u (codepoint-length c)))
		       (if (>=fx j u)
			   (loop (+fx r s) (-fx j u))
			   (return-utf8 this str (char->integer c) s r (-fx i j)))))))))))

;*---------------------------------------------------------------------*/
;*    js-utf8-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-utf8-ref this::JsStringLiteralUTF8 str::bstring index::long %this)
   (let ((n (utf8-codeunit-ref this str index)))
      (cond
	 ((not (fixnum? n))
	  n)
	 ((<=fx n 255)
	  (with-access::JsGlobalObject %this (char-table)
	     (vector-ref char-table n)))
	 (else
	  (js-utf8->jsstring/ulen
	     (ucs2-string->utf8-string
		(ucs2-string
		   (integer->ucs2 n)))
	     #u32:1)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-ref ...                                              */
;*---------------------------------------------------------------------*/
(define (js-jsstring-ref o index::uint32 %this)
   
   (define (ascii-string-ref val index)
      (if (>=u32 index (fixnum->uint32 (string-length val)))
	  (js-undefined)
	  (with-access::JsGlobalObject %this (char-table)
	     (vector-ref char-table
		(char->integer (string-ref-ur val (uint32->fixnum index)))))))
   
   (define (utf8-string-ref val index)
      (if (>=u32 index (js-jsstring-codeunit-length o))
	  (js-undefined)
	  (js-utf8-ref o val (uint32->fixnum index) %this)))

   (string-dispatch string-ref o index))

;*---------------------------------------------------------------------*/
;*    js-ascii-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-ascii-ref o::JsStringLiteralASCII index::uint32 %this)
   (let ((str (js-jsstring-normalize-ASCII! o)))
      (if (>=u32 index (fixnum->uint32 (string-length str)))
	  (js-undefined)
	  (with-access::JsGlobalObject %this (char-table)
	     (vector-ref char-table
		(char->integer (string-ref-ur str (uint32->fixnum index))))))))

;*---------------------------------------------------------------------*/
;*    js-string-ref ...                                                */
;*---------------------------------------------------------------------*/
(define (js-string-ref o prop %this)
   (cond
      ((and (fixnum? prop) (>=fx prop 0))
       (js-jsstring-ref o (fixnum->uint32 prop) %this))
      (else
       (js-get o prop %this))))

;*---------------------------------------------------------------------*/
;*    js-string-ref-as-string ...                                      */
;*---------------------------------------------------------------------*/
(define (js-string-ref-as-string o prop %this)
   (cond
      ((fixnum? prop)
       (js-jsstring-ref o (fixnum->uint32 prop) %this))
      (else
       (js-tojsstring (js-get o prop %this) %this))))

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
      (if (=u32 (fixnum->uint32 fxpos) (js-jsstring-codeunit-length o))
	  (js-undefined)
	  (js-utf8->jsstring/ulen (utf8-string-ref val fxpos) #u32:1)))
   
   (string-dispatch string-character-ref o (uint32->fixnum index)))

;*---------------------------------------------------------------------*/
;*    js-get ::JsStringLiteral ...                                     */
;*---------------------------------------------------------------------*/
(define-method (js-get o::JsStringLiteral prop %this)
   (js-get-string o prop %this))

;*---------------------------------------------------------------------*/
;*    js-get-string ...                                                */
;*---------------------------------------------------------------------*/
(define (js-get-string o prop %this)
   (if (eq? prop (& "length"))
       (uint32->fixnum (js-jsstring-codeunit-length o))
       (let ((i (js-toindex prop)))
	  (if (js-isindex? i)
	      (js-jsstring-ref o i %this)
	      (let ((p (js-toname prop %this)))
		 (cond
		    ((eq? p (& "length"))
		     (uint32->fixnum (js-jsstring-codeunit-length o)))
		    ((or (eq? p (& "indexOf"))
			 (eq? p (& "lastIndexOf"))
			 (eq? p (& "charCodeAt"))
			 (eq? p (& "charAt"))
			 (eq? p (& "substring"))
			 (eq? p (& "substr"))
			 (eq? p (& "toLowerCase"))
			 (eq? p (& "toLocaleLowerCase"))
			 (eq? p (& "toUpperCase"))
			 (eq? p (& "toLocaleUpperCase"))
			 (eq? p (& "split"))
			 (eq? p (& "slice"))
			 (eq? p (& "replace"))
			 (eq? p (& "match"))
			 (eq? p (& "compare"))
			 (eq? p (& "naturalCompare"))
			 (eq? p (& "localeCompare"))
			 (eq? p (& "trim")))
		     (with-access::JsGlobalObject %this (js-string-prototype)
			(js-get-jsobject/name-cache js-string-prototype prop %this)))
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
(define-method (js-get-length o::JsStringLiteral %this #!optional cache)
   (uint32->fixnum (js-jsstring-codeunit-length o)))

;*---------------------------------------------------------------------*/
;*    js-get-length ::JsStringLiteralASCII ...                         */
;*---------------------------------------------------------------------*/
(define-method (js-get-length o::JsStringLiteralASCII %this #!optional cache)
   (js-jsstring-lengthfx o))

;*---------------------------------------------------------------------*/
;*    js-put-string! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-put-string! _o::JsStringLiteral prop v throw %this)
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
   
   (define (ascii-indexof str::bstring pat patlen)
      (let* ((ulen (string-length str))
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
   
   (define (utf8-indexof str::bstring pat patlen)
      (let* ((ulen (string-length str))
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

      (let* ((pat (js-tostring search %this))
	     (patlen (string-length pat)))
	 (if (=fx patlen 0)
	     0
	     (string-dispatch indexof this pat patlen))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-indexof ...                                    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-indexof this search position %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-indexof this search position %this))
	    ((js-array? this)
	     (js-array-indexof this search position %this
		(or cache (js-pcache-ref js-string-pcache 23))))
	    ((js-object? this)
	     (js-call2 %this
		(js-get-jsobject-name/cache this (& "indexOf") #f %this
		   (or cache (js-pcache-ref js-string-pcache 23)))
		this search position))
	    (else
	     (loop (js-toobject %this this)))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-lastindexof ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.8     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-lastindexof this search position %this)
   
   (define (ascii-lastindexof s)
      (let* ((ulen (string-length s))
	     (numpos (js-tonumber position %this))
	     (pos (if (and (flonum? numpos) (nanfl? numpos))
		      ulen
		      (+ 1 (js-tointeger numpos %this))))
	     (search (js-tostring search %this))
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
			   -1)))))))
   
   (define (utf8-lastindexof s)
      (let* ((search (js-tostring search %this))
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

   (string-dispatch lastindexof this))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-lastindexof ...                                */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-lastindexof this search position %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-lastindexof this search position %this))
	    ((js-object? this)
	     (js-call2 %this
		(js-get-jsobject-name/cache this (& "lastIndexOf") #f %this
		   (or cache (js-pcache-ref js-string-pcache 2)))
		this search position))
	    (else
	     (loop (js-toobject %this this)))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-charcodeat ...                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.5     */
;*    -------------------------------------------------------------    */
;*    See stringliteral_expd.sch                                       */
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

   (string-dispatch charcodeat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-charcodeat-as-int32 ...                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.5     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-charcodeat-as-int32::int32 this position %this)
   
   (define (ascii-charcodeat val::bstring)
      (if (fixnum? position)
	  (cond
	     ((<fx position 0)
	      #s32:0)
	     ((>=fx position (string-length val))
	      #s32:0)
	     (else
	      (fixnum->int32
		 (char->integer (string-ref-ur val position)))))
	  (let ((pos (js-tointeger position %this)))
	     (if (or (< pos 0) (>= pos (string-length val)))
		 #s32:0
		 (fixnum->int32
		    (char->integer (string-ref val (->fixnum pos))))))))

   (define (utf8-charcodeat val::bstring)
      (let ((r (if (fixnum? position)
		   (utf8-codeunit-ref this val position)
		   (let ((pos (js-tointeger position %this)))
		      (utf8-codeunit-ref this val (->fixnum pos))))))
	 (if (fixnum? r)
	     (fixnum->int32 r)
	     #s32:0)))

   (string-dispatch charcodeat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-charcodeatu32 ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.5     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-charcodeatu32 this position::uint32)
   
   (define (ascii-charcodeat val::bstring)
      (if (>=u32 position (fixnum->uint32 (string-length val)))
	  +nan.0
	  (char->integer (string-ref-ur val (uint32->fixnum position)))))

   (define (utf8-charcodeat val::bstring)
      (if (>=u32 position (fixnum->uint32 (string-length val)))
	  +nan.0
	  (utf8-codeunit-ref this val (uint32->fixnum position))))

   (string-dispatch charcodeat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-charcodeatu32-as-int32 ...                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.5     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-charcodeatu32-as-int32::int32 this position::uint32)
   
   (define (ascii-charcodeat val::bstring)
      (if (>=u32 position (fixnum->uint32 (string-length val)))
	  #s32:0
	  (fixnum->int32
	     (char->integer (string-ref-ur val (uint32->fixnum position))))))

   (define (utf8-charcodeat val::bstring)
      (if (>=u32 position (fixnum->uint32 (string-length val)))
	  #s32:0
	  (let ((r (utf8-codeunit-ref this val (uint32->fixnum position))))
	     (if (fixnum? r)
		 (fixnum->int32 r)
		 #s32:0))))

   (string-dispatch charcodeat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-charcodeat ...                                 */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-charcodeat this index %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-charcodeat this index %this))
	    ((js-object? this)
	     (js-call1 %this
		(js-get-jsobject-name/cache this (& "charCodeAt") #f %this
		   (or cache (js-pcache-ref js-string-pcache 3)))
		this index))
	    (else
	     (loop (js-toobject %this this)))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-codepointat ...                                      */
;*    -------------------------------------------------------------    */
;*    https://tc39.es/ecma262/#sec-string.prototype.codepointat        */
;*    -------------------------------------------------------------    */
;*    See stringliteral_expd.sch                                       */
;*---------------------------------------------------------------------*/
(define (js-jsstring-codepointat this position %this)
   
   (define (ascii-codepointat val::bstring)
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

   (define (utf8-codepointat val::bstring)
      (if (fixnum? position)
	  (utf8-codepoint-ref this val position)
	  (let ((pos (js-tointeger position %this)))
	     (utf8-codepoint-ref this val (->fixnum pos)))))

   (string-dispatch codepointat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-codepointat-as-int32 ...                             */
;*---------------------------------------------------------------------*/
(define (js-jsstring-codepointat-as-int32::int32 this position %this)
   
   (define (ascii-codepointat val::bstring)
      (if (fixnum? position)
	  (cond
	     ((<fx position 0)
	      #s32:0)
	     ((>=fx position (string-length val))
	      #s32:0)
	     (else
	      (fixnum->int32
		 (char->integer (string-ref-ur val position)))))
	  (let ((pos (js-tointeger position %this)))
	     (if (or (< pos 0) (>= pos (string-length val)))
		 #s32:0
		 (fixnum->int32
		    (char->integer (string-ref val (->fixnum pos))))))))

   (define (utf8-codepointat val::bstring)
      (let ((r (if (fixnum? position)
		   (utf8-codepoint-ref this val position)
		   (let ((pos (js-tointeger position %this)))
		      (utf8-codepoint-ref this val (->fixnum pos))))))
	 (if (fixnum? r)
	     (fixnum->int32 r)
	     #s32:0)))

   (string-dispatch codepointat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-codepointatu32 ...                                   */
;*---------------------------------------------------------------------*/
(define (js-jsstring-codepointatu32 this position::uint32)
   
   (define (ascii-codepointat val::bstring)
      (if (>=u32 position (fixnum->uint32 (string-length val)))
	  +nan.0
	  (char->integer (string-ref-ur val (uint32->fixnum position)))))

   (define (utf8-codepointat val::bstring)
      (if (>=u32 position (fixnum->uint32 (string-length val)))
	  +nan.0
	  (utf8-codepoint-ref this val (uint32->fixnum position))))

   (string-dispatch codepointat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-codepointatu32-as-int32 ...                          */
;*---------------------------------------------------------------------*/
(define (js-jsstring-codepointatu32-as-int32::int32 this position::uint32)
   
   (define (ascii-codepointat val::bstring)
      (if (>=u32 position (fixnum->uint32 (string-length val)))
	  #s32:0
	  (fixnum->int32
	     (char->integer (string-ref-ur val (uint32->fixnum position))))))

   (define (utf8-codepointat val::bstring)
      (if (>=u32 position (fixnum->uint32 (string-length val)))
	  #s32:0
	  (let ((r (utf8-codepoint-ref this val (uint32->fixnum position))))
	     (if (fixnum? r)
		 (fixnum->int32 r)
		 #s32:0))))

   (string-dispatch codepointat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-codepointat ...                                */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-codepointat this index %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-codepointat this index %this))
	    ((js-object? this)
	     (js-call1 %this
		(js-get-jsobject-name/cache this (& "codePointAt") #f %this
		   (or cache (js-pcache-ref js-string-pcache 3)))
		this index))
	    (else
	     (loop (js-toobject %this this)))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-charat ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.4     */
;*    -------------------------------------------------------------    */
;*    See stringliteral_expd.sch                                       */
;*---------------------------------------------------------------------*/
(define (js-jsstring-charat this position %this)

   (define (ascii-charat/table val position)
      (with-access::JsGlobalObject %this (char-table)
	 (let* ((c (string-ref-ur val position))
		(i (char->integer c)))
	    (vector-ref char-table i))))
   
   (define (ascii-charat val::bstring)
      (if (fixnum? position)
	  (cond
	     ((<fx position 0)
	      (& ""))
	     ((>=fx position (string-length val))
	      (& ""))
	     (else
	      (ascii-charat/table val position)))
	  (let ((pos (js-tointeger position %this)))
	     (if (or (< pos 0) (>= pos (string-length val)))
		 (& "")
		 (ascii-charat/table val (->fixnum pos))))))

   (define (utf8-charat val::bstring)
      (if (fixnum? position)
	  (cond
	     ((<fx position 0)
	      (& ""))
	     ((>=u32 (fixnum->uint32 position) (js-jsstring-codeunit-length this))
	      (& ""))
	     (else
	      (js-utf8-ref this val position %this)))
	  (let ((pos (js-tointeger position %this)))
	     (if (or (< pos 0) (>= pos (js-jsstring-codeunit-length this)))
		 (& "")
		 (js-utf8-ref this val (->fixnum pos) %this)))))

   (string-dispatch charat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-charat ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-charat this index %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-charat this index %this))
	    ((js-object? this)
	     (js-call1 %this
		(js-get-jsobject-name/cache this (& "charAt") #f %this
		   (or cache (js-pcache-ref js-string-pcache 4)))
		this index))
	    (else
	     (loop (js-toobject %this this)))))))

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
	     (frm (minfx finalstart finalend))
	     (to (maxfx finalstart finalend)))
	 ;;(js-ascii->jsstring (substring s frm to))
	 (js-substring->jsstring s frm (-fx to frm))))
   
   (define (utf8-substr s)
      (let* ((len (utf8-string-length s))
	     (intstart (js-tointeger start %this))
	     (intend (if (eq? end (js-undefined)) len (js-tointeger end %this)))
	     (finalstart (->fixnum (min (max intstart 0) len)))
	     (finalend (->fixnum (min (max intend 0) len)))
	     (frm (minfx finalstart finalend))
	     (to (maxfx finalstart finalend)))
	 (js-string->jsstring (utf8-substring s frm to))))

   (string-dispatch substr this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-substring ...                                  */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-substring this start end %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-substring this start end %this))
	    ((js-object? this)
	     (js-call2 %this
		(js-get-jsobject-name/cache this (& "substring") #f %this
		   (or cache (js-pcache-ref js-string-pcache 5)))
		this start end))
	    (else
	     (loop (js-toobject %this this)))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-substr ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-B.2.3        */
;*---------------------------------------------------------------------*/
(define (js-jsstring-substr this start length %this)
   
   (define (ascii-substr r1::bstring)
      (let* ((r2 (js-tointeger start %this))
	     (r3 (if (eq? length (js-undefined))
		     (maxvalfx)
		     (js-tointeger length %this)))
	     (r4 (string-length r1))
	     (r5 (if (>=fx r2 0) r2 (maxfx (+fx r4 r2) 0)))
	     (r6 (minfx (maxfx r3 0) (-fx r4 r5))))
	 (if (<=fx r6 0)
	     (& "")
	     ;;(js-ascii->jsstring (substring r1 r5 (+fx r5 r6)))
	     (js-substring->jsstring r1 r5 r6))))
   
   (define (utf8-substr r1::bstring)
      (let* ((r2 (js-tointeger start %this))
	     (r3 (if (eq? length (js-undefined))
		     (maxvalfx)
		     (js-tointeger length %this)))
	     (r4 (utf8-string-length r1))
	     (r5 (if (>=fx r2 0) r2 (maxfx (+fx r4 r2) 0)))
	     (r6 (minfx (maxfx r3 0) (-fx r4 r5))))
	 (if (<=fx r6 0)
	     (& "")
	     (js-string->jsstring (utf8-substring r1 r5 (+fx r5 r6))))))

   (string-dispatch substr this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-substr ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-substr this start length %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-substr this start length %this))
	    ((js-object? this)
	     (js-call2 %this
		(js-get-jsobject-name/cache this (& "substr") #f %this
		   (or cache (js-pcache-ref js-string-pcache 6)))
		this start length))
	    (else
	     (loop (js-toobject %this this)))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-tolowercase ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.16    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-tolowercase this)

   (define (ascii-tolowercase s)
      (js-ascii->jsstring (string-downcase s)))

   (define (utf8-tolowercase s)
      (with-access::JsStringLiteralUTF8 this (%culen)
	 (js-utf8->jsstring/ulen
	    (ucs2-string->utf8-string
	       (ucs2-string-downcase (utf8-string->ucs2-string s)))
	    %culen)))

   (string-dispatch tolowercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-tolowercase ...                                */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-tolowercase this %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-tolowercase this))
	    ((js-object? this)
	     (js-call0 %this
		(js-get-jsobject-name/cache this (& "toLowerCase") #f %this
		   (or cache (js-pcache-ref js-string-pcache 7)))
		this))
	    (else
	     (loop (js-toobject %this this)))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-tolocalelowercase ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.17    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-tolocalelowercase this)

   (define (ascii-tolowercase s)
      (js-ascii->jsstring (string-downcase s)))

   (define (utf8-tolowercase s)
      (with-access::JsStringLiteralUTF8 this (%culen)
	 (js-utf8->jsstring/ulen (utf8-string-locale-downcase s) %culen)))

   (string-dispatch tolowercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-tolocalelowercase ...                          */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-tolocalelowercase this %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-tolocalelowercase this))
	    ((js-object? this)
	     (js-call0 %this
		(js-get-jsobject-name/cache this (& "toLocaleLowerCase") #f %this
		   (or cache (js-pcache-ref js-string-pcache 8)))
		this))
	    (else
	     (loop (js-toobject %this this)))))))
   
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

   (string-dispatch touppercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-touppercase ...                                */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-touppercase this %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-touppercase this))
	    ((js-object? this)
	     (js-call0 %this
		(js-get-jsobject-name/cache this (& "toUpperCase") #f %this
		   (or cache (js-pcache-ref js-string-pcache 9)))
		this))
	    (else
	     (loop (js-toobject %this this)))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-tolocaleuppercase ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.18    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-tolocaleuppercase this)

   (define (ascii-touppercase s)
      (js-ascii->jsstring (string-upcase s)))

   (define (utf8-touppercase s)
      (with-access::JsStringLiteralUTF8 this (%culen)
	 (js-utf8->jsstring/ulen (utf8-string-locale-upcase s) %culen)))

   (string-dispatch touppercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-tolocaleuppercase ...                          */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-tolocaleuppercase this %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-tolocaleuppercase this))
	    ((js-object? this)
	     (js-call0 %this
		(js-get-jsobject-name/cache this (& "toLocaleUpperCase") #f %this
		   (or cache (js-pcache-ref js-string-pcache 10)))
		this))
	    (else
	     (loop (js-toobject %this this)))))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-split ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.14    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-split this::JsStringLiteral separator limit %this)
   
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
	     (A (js-empty-vector->jsarray %this))
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
	     (js-array-index-set! A #u32:0 jsS #f %this)
	     A)
	    ((=fx s 0)
	     ;; 11
	     (let ((z (split-match S 0 R)))
		(when (eq? z 'failure)
		   (js-array-index-set! A #u32:0 jsS #f %this))
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
					(l (js-get-lengthu32 A %this)))
				     ;; 13.c.iii.2
				     (js-array-index-set! A l
					(js-string->jsstring T) #f %this)
				     (if (=fx (uint32->fixnum (+u32 l #u32:1)) lim)
					 ;; 13.c.iii.4
					 A
					 ;; 13.c.iii.5
					 (let ((p e))
					    (let repeat ((cap cap)
							 (l (+u32 l #u32:1)))
					       (if (pair? cap)
						   (begin
						      ;; 13.c.iii.7.b
						      (js-array-index-set! A l
							 (js-string->jsstring (car cap)) #f %this)
						      (if (=fx (uint32->fixnum (+u32 l #u32:1)) lim)
							  ;; 13.c.iii.7.d
							  A
							  ;; 13.c.iii.8
							  (repeat (cdr cap) (+u32 l #u32:1))))
						   (loop p e))))))))))
		    ;; 14
		    (let ((T (substring S p s))
			  (l (js-get-lengthu32 A %this)))
		       ;; 15
		       (js-array-index-set! A l (js-string->jsstring T) #f %this)
		       ;;16
		       A))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-split ...                                      */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-split this separator limit %this cache)
   (with-access::JsGlobalObject %this (js-string-pcache)
      (let loop ((this this))
	 (cond
	    ((js-jsstring? this)
	     (js-jsstring-split this separator limit %this))
	    ((js-object? this)
	     (js-call2 %this
		(js-get-name/cache (js-tojsstring this %this)
		   (& "split") #f %this
		   (or cache (js-pcache-ref js-string-pcache 11)))
		this separator limit))
	    (else
	     (loop (js-toobject %this this)))))))

;*---------------------------------------------------------------------*/
;*    table22 ...                                                      */
;*---------------------------------------------------------------------*/
(define (table22 fmt::bstring match::pair string::bstring %this)
   
   (define (digit->number c)
      (-fx (char->integer c) (char->integer #\0)))
   
   (define (digit10->number c1 c2)
      (+fx (*fx (digit->number c1) 10) (digit->number c2)))
   
   (let ((stop (-fx (string-length fmt) 1)))
      (let loop ((i 0)
		 (j 0)
		 (res (& "")))
	 (cond
	    ((>=fx i stop)
	     (js-jsstring-concat res (js-substring fmt j (+fx stop 1) %this)))
	    ((not (char=? (string-ref fmt i) #\$))
	     (loop (+fx i 1) j res))
	    (else
	     (case (string-ref fmt (+fx i 1))
		((#\$)
		 (let ((res (js-jsstring-concat res (js-substring fmt j i %this))))
		    (loop (+fx i 2) (+fx i 2)
		       (js-jsstring-concat res (& "$")))))
		((#\&)
		 (let ((res (js-jsstring-concat res (js-substring fmt j i %this)))
		       (portion (js-substring string (caar match) (cdar match) %this)))
		    (loop (+fx i 2) (+fx i 2)
		       (js-jsstring-concat res portion))))
		((#\`)
		 (let ((res (js-jsstring-concat res (js-substring fmt j i %this)))
		       (portion (js-substring string 0 (caar match) %this)))
		    (loop (+fx i 2) (+fx i 2)
		       (js-jsstring-concat res portion))))
		((#\')
		 (let ((res (js-jsstring-concat res
			       (js-substring fmt j i %this)))
		       (portion (js-substring string (cdar match)
				   (string-length string) %this)))
		    (loop (+fx i 2) (+fx i 2)
		       (js-jsstring-concat res portion))))
		((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		 (let ((res (cond
			       ((=fx i 0)
				res)
			       ((=fx j 0)
				(js-substring fmt j i %this))
			       (else
				(js-jsstring-concat res
				   (js-substring fmt j i %this)))))
		       (len (length match)))
		    (if (or (=fx i (-fx stop 1))
			    (not (char-numeric? (string-ref fmt (+fx i 2)))))
			(let ((n (digit->number (string-ref fmt (+fx i 1)))))
			   (if (>fx n len)
			       (loop (+fx i 2) j res)
			       (let* ((m (list-ref match n))
				      (portion (js-substring string
						  (car m) (cdr m) %this)))
				  (loop (+fx i 2) (+fx i 2)
				     (js-jsstring-concat res portion)))))
			(let ((n (digit10->number
				    (string-ref fmt (+fx i 1))
				    (string-ref fmt (+fx i 2)))))
			   (if (>fx n len)
			       (let ((n (digit->number (string-ref fmt (+fx i 1)))))
				  (if (>=fx n len)
				      (loop (+fx i 3) j res)
				      (let* ((m (list-ref match n))
					     (portion (js-substring string
							 (car m) (cdr m) %this)))
					 (loop (+fx i 2) (+fx i 2)
					    (js-jsstring-concat res portion)))))
			       (let* ((m (list-ref match n))
				      (portion (js-substring string
						  (car m) (cdr m) %this)))
				  (loop (+fx i 3) (+fx i 3)
				     (js-jsstring-concat res portion))))))))
		(else
		 ;; MS, 2019-01-09: used to be:
		 ;; (loop (+fx i 2) j res)
		 (loop (+fx i 1) j res))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-replace-regexp ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-replace-regexp this::obj rx::regexp
	   lastindex::long global::bool replacevalue %this)
   (cond
      ((js-procedure? replacevalue)
       (with-access::JsProcedure replacevalue (procedure)
	  (if (=fx ($procedure-arity procedure) 2)
	      (js-jsstring-replace-regexp-fun1 this rx
		 lastindex global procedure %this)
	      (js-jsstring-replace-regexp-funN this rx
		 lastindex global replacevalue %this))))
      ((js-jsstring? replacevalue)
       (js-jsstring-replace-regexp-string this rx
	  lastindex global replacevalue %this))
      (else
       (js-jsstring-replace-regexp-string this rx
	  lastindex global (js-tojsstring replacevalue %this) %this))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-replace-regexp-funN ...                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-replace-regexp-funN this::obj rx::regexp
	   lastindex::long global::bool replacevalue %this)
   
   (define (matches->string-list::pair-nil matches::pair-nil s::bstring enc)
      (map (lambda (m)
	      (if (pair? m)
		  (js-substring/enc s (car m) (cdr m) enc %this)
		  (& "")))
	 matches))

   (with-access::JsGlobalObject %this (js-regexp js-array)
      (let ((s (js-jsstring->string this))
	    (enc (isa? this JsStringLiteralUTF8)))
	 (cond
	    ((not global)
	     (let ((r (pregexp-match-positions rx s lastindex)))
		(cond
		   ((not r)
		    this)
		   (else
		    (js-jsstring-append
		       (js-substring/enc s 0 (caar r) enc %this)
		       (js-jsstring-append
			  (js-tojsstring
			     (js-apply %this replacevalue (js-undefined)
				(append (matches->string-list r s enc)
				   (list (caar r) this)))
			     %this)
			  (js-substring/enc s
			     (cdar r) (string-length s) enc %this)))))))
	    (else
	     (let loop ((len (string-length s)))
		(let loop ((i 0)
			   (res (& "")))
		   (let ((r (pregexp-match-positions rx s i)))
		      (if (not r)
			  (cond
			     ((=fx i 0)
			      this)
			     ((>=fx i len)
			      res)
			     (else
			      (js-jsstring-concat res
				 (js-substring/enc s i len enc %this))))
			  (let ((v (js-tojsstring
				      (js-apply %this replacevalue (js-undefined)
					 (append (matches->string-list r s enc)
					    (list (caar r) this)))
				      %this)))
			     (cond
				((>fx (cdar r) i)
				 (loop (cdar r)
				    (js-jsstring-append
				       (js-jsstring-concat res
					  (js-substring/enc s i (caar r) enc
					     %this))
				       v)))
				((<fx i len)
				 (loop (+fx i 1)
				    (js-jsstring-append
				       (js-jsstring-concat res
					  (js-substring/enc s i (caar r) enc
					     %this))
				       (js-jsstring-append
					  v
					  (js-substring/enc s i (+fx i 1) enc
					     %this)))))
				(else
				 (js-jsstring-append
				    (js-jsstring-concat res
				       (js-substring/enc s i (caar r) enc
					  %this))
				    v)))))))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-replace-regexp-fun1 ...                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-replace-regexp-fun1 this::obj rx::regexp
	   lastindex::long global::bool replacevalue %this)
   
   (define (first-match pos::vector s::bstring)
      (if (>=fx (vector-ref pos 0) 0)
	  (js-substring s (vector-ref pos 0) (vector-ref pos 1) %this)
	  (& "")))

   (with-access::JsGlobalObject %this (js-regexp js-array)
      (let* ((s (js-jsstring->string this))
	     (len (string-length s))
	     (enc (isa? this JsStringLiteralUTF8))
	     (pos (vector -1 -1)))
	 (cond
	    ((not global)
	     (let ((r (pregexp-match-n-positions! rx s pos lastindex len)))
		(cond
		   ((<=fx r 0)
		    this)
		   (else
		    (js-jsstring-append
		       (js-substring/enc s 0 (vector-ref pos 0) enc %this)
		       (js-jsstring-append
			  (js-tojsstring
			     (replacevalue (js-undefined)
				(first-match pos s))
			     %this)
			  (js-substring/enc s
			     (vector-ref pos 1) (string-length s) enc %this)))))))
	    (else
	     (let loop ((i 0)
			(res (& "")))
		(let ((r (pregexp-match-n-positions! rx s pos i len)))
		   (if (<=fx r 0)
		       (cond
			  ((=fx i 0)
			   this)
			  ((>=fx i len)
			   res)
			  (else
			   (js-jsstring-concat res
			      (js-substring/enc s i len enc %this))))
		       (let ((v (js-tojsstring
				   (replacevalue (js-undefined)
				      (first-match pos s))
				   %this)))
			  (cond
			     ((>fx (vector-ref pos 1) i)
			      (loop (vector-ref pos 1)
				 (js-jsstring-append
				    (js-jsstring-concat res
				       (js-substring/enc s i (vector-ref pos 0) enc %this))
				    v)))
			     ((<fx i len)
			      (loop (+fx i 1)
				 (js-jsstring-append
				    (js-jsstring-concat res
				       (js-substring/enc s i
					  (vector-ref pos 0) enc %this))
				    (js-jsstring-append
				       v
				       (js-substring/enc s i (+fx i 1) enc %this)))))
			     (else
			      (js-jsstring-append
				 (js-jsstring-concat res
				    (js-substring/enc s i
				       (vector-ref pos 0) enc %this))
				 v))))))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-replace-regexp-string ...                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-replace-regexp-string this::obj rx::regexp
	   lastindex::long global::bool replacevalue %this)
   
   (define (matches->string-list::pair-nil matches::pair-nil s::bstring enc)
      (map (lambda (m)
	      (if (pair? m)
		  (js-substring/enc s (car m) (cdr m) enc %this)
		  (& "")))
	 matches))

   (let* ((s (js-jsstring->string this))
	  (len (string-length s))
	  (enc (isa? this JsStringLiteralUTF8))
	  (newstring (js-jsstring->string replacevalue)))
      (cond
	 ((not global)
	  (let ((r (pregexp-match-positions rx s lastindex)))
	     (cond
		((not r)
		 this)
		((string-index newstring #\$)
		 (js-jsstring-append
		    (js-substring/enc s 0 (caar r) enc %this)
		    (js-jsstring-append
		       (table22 newstring r s %this)
		       (js-substring/enc s (cdar r)
			  (string-length s) enc %this))))
		(else
		 (js-jsstring-append
		    (js-substring/enc s 0 (caar r) enc %this)
		    (js-jsstring-append
		       replacevalue
		       (js-substring/enc s (cdar r)
			  (string-length s) enc %this)))))))
	 (else
;* 	  (tprint "replace-regexp-string.2 rx=" rx " newstr=" newstring " s=" s) */
	  (if (string-index newstring #\$)
	      (let loop ((i 0)
			 (res (& "")))
		 (let ((r (pregexp-match-positions rx s i)))
		    (if (not r)
			(cond
			   ((=fx i 0)
			    this)
			   ((>=fx i len)
			    res)
			   (else
			    (js-jsstring-concat res
			       (js-substring/enc s i len enc %this))))
			(let ((v (table22 newstring r s %this)))
			   (cond
			      ((>fx (cdar r) i)
			       (loop (cdar r)
				  (js-jsstring-concat
				     (js-jsstring-concat res
					(js-substring/enc s i (caar r) enc %this))
				     v)))
			      ((<fx i len)
			       (loop (+fx i 1)
				  (js-jsstring-append
				     (js-jsstring-concat res
					(js-substring/enc s i (caar r)
					   enc %this))
				     (js-jsstring-append
					v
					(js-substring/enc s i (+fx i 1)
					   enc %this)))))
			      (else
			       (js-jsstring-append
				  (js-jsstring-concat res
				     (js-substring/enc s i (caar r) enc %this))
				  v)))))))
	      (let ((pos (vector -1 -1)))
		 (let loop ((i 0)
			    (res (& "")))
		    (let ((r (pregexp-match-n-positions! rx s pos i len)))
		       (if (<=fx r 0)
			   (begin
			      (cond
				 ((=fx i 0)
				  this)
				 ((>=fx i len)
				  res)
				 (else
				  (js-jsstring-concat res
				     (js-substring/enc s i len enc %this)))))
			   (let ((v replacevalue))
			      (cond
				 ((>fx (vector-ref pos 1) i)
				  (loop (vector-ref pos 1)
				     (js-jsstring-append
					(js-jsstring-concat res
					   (js-substring/enc s i
					      (vector-ref pos 0) enc %this))
					v)))
				 ((<fx i len)
				  (loop (+fx i 1)
				     (js-jsstring-append
					(js-jsstring-concat res
					   (js-substring/enc s i
					      (vector-ref pos 0) enc %this))
					(js-jsstring-append
					   v
					   (js-substring/enc s i
					      (+fx i 1) enc %this)))))
				 (else
				  (js-jsstring-append
				     (js-jsstring-concat res
					(js-substring/enc s i
					   (vector-ref pos 0) enc %this))
				     v)))))))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-replace-string ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-replace-string this::obj need22 searchstr replacevalue %this)
   (let* ((string (js-jsstring->string this))
	  (enc (isa? this JsStringLiteralUTF8))
	  (i (string-contains string (js-jsstring->string searchstr) 0)))
      (cond
	 ((not i)
	  this)
	 ((not need22)
	  (let* ((j (+fx i (js-jsstring-lengthfx searchstr)))
		 (tail (js-jsstring-append replacevalue
			  (js-substring/enc string
			     j (string-length string) enc %this))))
	     (if (>fx i 0)
		 (js-jsstring-append
		    (js-substring/enc string 0 i enc %this) tail)
		 tail)))
	 ((js-procedure? replacevalue)
	  (let ((j (+fx i (js-jsstring-lengthfx searchstr))))
	     (js-jsstring-append
		(js-substring/enc string 0 i enc %this)
		(js-jsstring-append
		   (js-tojsstring
		      (js-call3 %this replacevalue (js-undefined)
			 searchstr i this) %this)
		   (js-substring/enc string
		      j (string-length string) enc %this)))))
	 ((and (js-jsstring? replacevalue)
	       (not (string-index (js-jsstring->string replacevalue) #\$)))
	  (let* ((j (+fx i (js-jsstring-lengthfx searchstr)))
		 (tail (js-jsstring-append replacevalue
			  (js-substring/enc string
			     j (string-length string) enc %this))))
	     (if (>fx i 0)
		 (js-jsstring-append
		    (js-substring/enc string 0 i enc %this) tail)
		 tail)))
	 (else
	  (let* ((newstring (js-tostring replacevalue %this))
		 (j (+fx i (js-jsstring-lengthfx searchstr)))
		 (tail (js-jsstring-append
			  (table22 newstring (list (cons i j)) string %this)
			  (js-substring/enc string
			     j (string-length string) enc %this))))
	     (if (>fx i 0)
		 (js-jsstring-append
		    (js-substring/enc string 0 i enc %this) tail)
		 tail))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-replace ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-replace this::obj need22 searchvalue replacevalue %this)
   
   (define (fun1? v)
      (when (js-procedure? v)
	 (with-access::JsProcedure v (procedure)
	    (correct-arity? procedure 2))))
   
   (define (fun1 v)
      (with-access::JsProcedure v (procedure)
	 procedure))
   
   (cond
      ((isa? searchvalue JsRegExp)
       (with-access::JsRegExp searchvalue (rx flags)
	  (with-access::JsGlobalObject %this (js-string-pcache)
	     (let* ((lastindex (js-get-jsobject-name/cache searchvalue
				  (& "lastIndex")
				  #f %this (js-pcache-ref js-string-pcache 12)))
		    (global (js-regexp-flags-global? flags))
		    (res (cond
			    ((fun1? replacevalue)
			     (js-jsstring-replace-regexp-fun1 this rx
				lastindex global
				(fun1 replacevalue) %this))
			    ((js-jsstring? replacevalue)
			     (js-jsstring-replace-regexp-string this rx
				lastindex global
				replacevalue %this))
			    (else
			     (js-jsstring-replace-regexp this rx
				lastindex global
				replacevalue %this)))))
		(when global
		   (js-put-jsobject-name/cache! searchvalue (& "lastIndex") 0
		      #f %this (js-pcache-ref js-string-pcache 12)))
		res))))
      ((js-jsstring? searchvalue)
       (js-jsstring-replace-string this need22 searchvalue replacevalue %this))
      (else
       (js-jsstring-replace-string this need22 (js-tojsstring searchvalue %this) replacevalue %this)
       )))

;*---------------------------------------------------------------------*/
;*    js-jsstring-prototype-replace ...                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-prototype-replace this searchvalue replacevalue %this)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-replace this #t searchvalue replacevalue %this))
	 ((isa? this JsString)
	  (with-access::JsString this (val)
	     (loop val)))
	 ((js-object? this)
	  (loop (js-tojsstring this %this)))
	 (else
	  (loop (js-tojsstring (js-toobject %this this) %this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-replace ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-replace this need22 searchvalue replacevalue %this cache)
   (cond
      ((js-jsstring? this)
       (js-jsstring-replace this need22 searchvalue replacevalue %this))
      ((isa? this JsString)
       (with-access::JsString this (val)
	  (js-jsstring-replace val need22 searchvalue replacevalue %this)))
      (else
       (with-access::JsGlobalObject %this (js-string-pcache)
	  (js-call2 %this
	     (js-get-name/cache this (& "replace") #f %this
		(or cache (js-pcache-ref js-string-pcache 13)))
	     this searchvalue replacevalue)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-match-regexp ...                                     */
;*    -------------------------------------------------------------    */
;*    This function is used when the second argument is an existing    */
;*    regular expression.                                              */
;*---------------------------------------------------------------------*/
(define (js-jsstring-match-regexp this string rx %this)
   (with-access::JsGlobalObject %this (js-regexp js-array js-regexp-prototype js-string-pcache)
      (let* ((proto (js-get-jsobject-name/cache js-regexp (& "prototype")
		       #f %this (js-pcache-ref js-string-pcache 14)))
	     (exec (js-get-jsobject-name/cache proto (& "exec")
		      #f %this (js-pcache-ref js-string-pcache 15))))
	 (with-access::JsRegExp rx (flags)
	    ;; 7
	    (if (not (js-regexp-flags-global? flags))
		(js-call1 %this exec rx this)
		;; 8
		(let ((lastindex (js-get-jsobject-name/cache rx (& "lastIndex")
				    #f %this (js-pcache-ref js-string-pcache 16)))
		      (previousLastIndex 0)
		      (a (js-null)))
		   (set! lastindex 0)
		   (js-put-jsobject-name/cache! rx (& "lastIndex") lastindex
		      #f %this (js-pcache-ref js-string-pcache 17))
		   (let loop ((n 0))
		      (let ((result (js-call1 %this exec rx this)))
			 (set! lastindex
			    (js-get-jsobject-name/cache rx (& "lastIndex")
			       #f %this (js-pcache-ref js-string-pcache 17)))
			 (if (eq? result (js-null))
			     a
			     (let ((thisIndex lastindex))
				(if (= thisIndex previousLastIndex)
				    (begin
				       (set! lastindex (+ thisIndex 1))
				       (js-put-jsobject-name/cache! rx (& "lastIndex") lastindex
					  #f %this (js-pcache-ref js-string-pcache 17))
				       (set! previousLastIndex (+ 1 thisIndex)))
				    (set! previousLastIndex thisIndex))
				(when (eq? a (js-null))
				   (set! a (js-array-construct-alloc/length %this 1)))
				(let ((matchStr (js-get result 0 %this)))
				   (js-array-index-set! a (fixnum->uint32 n)
				      matchStr #f %this))
				(loop (+fx 1 n))))))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-match-regexp-from-string ...                         */
;*---------------------------------------------------------------------*/
(define (js-jsstring-match-regexp-from-string this string rx %this)
   (with-access::JsGlobalObject %this (js-regexp js-regexp-prototype js-string-pcache)
      (if (js-object-mode-plain? js-regexp-prototype)
	  (js-regexp-prototype-exec-for-match-string %this rx this)
	  (let* ((proto (js-get-jsobject-name/cache js-regexp (& "prototype")
			   #f %this (js-pcache-ref js-string-pcache 35)))
		 (exec (js-get-jsobject-name/cache proto (& "exec")
			  #f %this (js-pcache-ref js-string-pcache 36))))
	     (js-call1 %this exec rx this)))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-match-regexp-from-string-as-bool ...                 */
;*---------------------------------------------------------------------*/
(define (js-jsstring-match-regexp-from-string-as-bool this string rx %this::JsGlobalObject)
   (with-access::JsGlobalObject %this (js-regexp js-regexp-prototype js-string-pcache)
      (if (js-object-mode-plain? js-regexp-prototype)
	  (js-regexp-prototype-exec-as-bool rx this %this)
	  (let* ((proto (js-get-jsobject-name/cache js-regexp (& "prototype")
			   #f %this (js-pcache-ref js-string-pcache 35)))
		 (exec (js-get-jsobject-name/cache proto (& "exec")
			  #f %this (js-pcache-ref js-string-pcache 36))))
	     (js-call1 %this exec rx this)))))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-match ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.10    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-match this regexp %this)
   (with-access::JsGlobalObject %this (js-regexp)
      (cond
	 ((isa? regexp JsRegExp)
	  (js-jsstring-match-regexp this string regexp %this))
	 ((js-jsstring? regexp)
	  (js-jsstring-match-regexp-from-string this regexp
	     (js-new %this js-regexp regexp) %this))
	 (else
	  (js-jsstring-match this (js-new %this js-regexp regexp) %this)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-match ...                                      */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-match this regexp %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-match this regexp %this))
	 ((js-object? this)
	  (with-access::JsGlobalObject %this (js-string-pcache)
	     (js-call1 %this
		(js-get-jsobject-name/cache this (& "match") #f %this
		   (or cache (js-pcache-ref js-string-pcache 18)))
		this regexp)))
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
(define (js-jsstring-maybe-naturalcompare this that %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-naturalcompare this that %this))
	 ((js-object? this)
	  (with-access::JsGlobalObject %this (js-string-pcache)
	     (js-call1 %this
		(js-get-jsobject-name/cache this (& "naturalCompare") #f %this
		   (or cache (js-pcache-ref js-string-pcache 19)))
		this that)))
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
(define (js-jsstring-maybe-localecompare this that %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-localecompare this that %this))
	 ((js-object? this)
	  (with-access::JsGlobalObject %this (js-string-pcache)
	     (js-call1 %this
		(js-get-jsobject-name/cache this (& "localeCompare") #f %this
		   (or cache (js-pcache-ref js-string-pcache 20)))
		this that)))
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
(define (js-jsstring-maybe-trim this %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-trim this))
	 ((js-object? this)
	  (with-access::JsGlobalObject %this (js-string-pcache)
	     (js-call0 %this
		(js-get-jsobject-name/cache this (& "trim") #f %this
		   (or cache (js-pcache-ref js-string-pcache 21)))
		this)))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-fromcharcode ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-fromcharcode code %this)
   (let loop ((code code))
      (cond
	 ((or (not (fixnum? code)) (<fx code 0) (>=fx code 65536))
	  (loop (uint16->fixnum (js-touint16 code %this))))
	 ((and (>=fx code 0) (<=fx code 255))
	  (with-access::JsGlobalObject %this (char-table)
	     (vector-ref char-table code)))
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
   
   (string-dispatch escape this))
   
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

   (string-dispatch unescape this))

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
<				       
				       (blit-string! utf8 0 buf j l)
				       (loop (+fx ni 3) (+fx j l))))))))))))
	    (else
	     (let ((l (-fx ol i)))
		(blit-string! str i buf j l)
		(loop ol (+fx j l))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-slice ...                                            */
;*---------------------------------------------------------------------*/
(define (js-jsstring-slice jss::JsStringLiteral start end %this)
   
   (define (jssubstring this::JsStringLiteral from::long to::long)
      
      (define (ascii-substr s)
	 (if (or (>fx from 0) (<fx to (string-length s)))
	     (js-substring->jsstring s from (-fx to from))
	     this))
      
      (define (utf8-substr s)
	 (js-string->jsstring (utf8-substring s from to)))
      
      (string-dispatch substr this))

   (let* ((len (uint32->fixnum (js-jsstring-length jss)))
	  (intstart (js-tointeger start %this))
	  (intend (if (eq? end (js-undefined)) len (js-tointeger end %this)))
	  (from (->fixnum
		   (if (< intstart 0)
		       (max (+ len intstart) 0)
		       (min intstart len))))
	  (to (->fixnum
		 (if (< intend 0)
		     (max (+ len intend) 0)
		     (min intend len))))
	  (span (maxfx (-fx to from) 0))
	  (end (+ from span)))
      (if (or (>fx from 0) (<fx end len))
	  (jssubstring jss from end)
	  jss)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-slice1 ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-slice1 this start %this cache)
   (cond
      ((js-jsstring? this)
       (js-jsstring-slice this start (js-jsstring-lengthfx this) %this))
      ((js-array? this)
       (js-array-maybe-slice1 this %this cache start))
      (else
       (with-access::JsGlobalObject %this (js-string-pcache)
	  (let ((slice (js-get-name/cache this (& "slice") #f %this
			  (or cache (js-pcache-ref js-string-pcache 37))
			  -1 '(imap+))))
	     (js-call1 %this slice this start))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-slice2 ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-slice2 this start end %this cache)
   (cond
      ((js-jsstring? this)
       (js-jsstring-slice this start end %this))
      ((js-array? this)
       (js-array-maybe-slice2 this %this cache start end))
      (else
       (with-access::JsGlobalObject %this (js-string-pcache)
	  (let ((slice (js-get-name/cache this (& "slice") #f %this
			  (or cache (js-pcache-ref js-string-pcache 22))
			  -1 '(imap+))))
	     (js-call2 %this slice this start end))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring->jsarray ...                                         */
;*---------------------------------------------------------------------*/
(define (js-jsstring->jsarray o::JsStringLiteral %this)
   
   (define (ascii->array val::bstring %this)
      (let* ((len (string-length val))
	     (vec (make-vector len)))
	 (let loop ((i 0))
	    (if (<fx i len)
		(with-access::JsGlobalObject %this (char-table)
		   (let ((val (vector-ref char-table
				 (char->integer (string-ref-ur val i)))))
		      (vector-set! vec i val)
		      (loop (+fx i 1))))
		(js-vector->jsarray vec %this)))))
      
   (define (utf8->array val::bstring %this)
      (let* ((len (string-length val))
	     (vec (make-vector len)))
	 (let loop ((i 0))
	    (if (<fx i len)
		(let* ((z (utf8-char-size (string-ref val i)))
		       (s (substring val i (+fx i z))))
		   (vector-set! vec i (js-string->jsstring s))
		   (loop (+fx i z)))
		(js-vector->jsarray vec %this)))))
   
   (string-dispatch >array o %this))

;*---------------------------------------------------------------------*/
;*    js-jsobject->jsarray ::JsStringLiteral ...                       */
;*---------------------------------------------------------------------*/
(define-method (js-jsobject->jsarray o::JsStringLiteral %this)
   (js-jsstring->jsarray o %this))

;*---------------------------------------------------------------------*/
;*    js-jsstring->list ...                                            */
;*---------------------------------------------------------------------*/
(define (js-jsstring->list o %this)
   
   (define (ascii->list val)
      (with-access::JsGlobalObject %this (char-table)
	 (map! (lambda (c) (vector-ref char-table (char->integer c)))
	    (string->list val))))
   
   (define (utf8->list val)
      (let ((len (string-length val)))
	 (let loop ((i 0)
		    (acc '()))
	    (if (<fx i len)
		(let* ((z (utf8-char-size (string-ref val i)))
		       (s (substring val i (+fx i z))))
		   (loop (+fx i z) (cons (js-string->jsstring s) acc)))
		(reverse! acc)))))
   
   (string-dispatch >list o))

;*---------------------------------------------------------------------*/
;*    js-jsstring-for-in ...                                           */
;*    -------------------------------------------------------------    */
;*    This function is invoked on simple literal ascii strings.        */
;*---------------------------------------------------------------------*/
(define (js-jsstring-for-in o proc %this)
   (let ((len (js-jsstring-length o)))
      (let loop ((i #u32:0))
	 (when (<u32 i len)
	    (let ((val (js-integer->jsstring (uint32->fixnum i))))
	       (proc val %this)
	       (loop (+u32 i 1)))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-for-of ...                                           */
;*    -------------------------------------------------------------    */
;*    This function is invoked on simple literal ascii strings.        */
;*---------------------------------------------------------------------*/
(define (js-jsstring-for-of o proc %this)
   (with-access::JsGlobalObject %this (char-table)
      (let ((len (string-length o)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (let ((val (vector-ref char-table
			     (char->integer (string-ref-ur o i)))))
		  (proc val %this)
		  (loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    js-for-of ::JsStringLiteral ...                                  */
;*---------------------------------------------------------------------*/
(define-method (js-for-of o::JsStringLiteral proc close %this)

   (define (ascii-for-of val proc %this)
      (js-jsstring-for-of val proc %this))

   (define (utf8-for-of val proc %this)
      (let ((len (string-length val)))
	 (let loop ((i 0))
	       (when (<fx i len)
		  (let* ((z (utf8-char-size (string-ref val i)))
			 (s (substring val i (+fx i z))))
		     (proc (js-string->jsstring s) %this)
		     (loop (+fx i z)))))))
   
   (string-dispatch for-of o proc %this))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
