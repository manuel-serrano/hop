;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/stringliteral.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 21 14:13:28 2014                          */
;*    Last change :  Fri Mar 29 07:39:15 2019 (serrano)                */
;*    Copyright   :  2014-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Internal implementation of literal strings                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_stringliteral

   (library hop)

   (include "types.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_private
	   __hopscript_property
	   __hopscript_array)

   (export (js-jsstring-debug msg obj)
	   (js-jsstring-for-in str ::procedure ::JsGlobalObject)
	   (js-jsstring-for-of str ::procedure ::JsGlobalObject)
	   (inline js-jsstring-name::obj ::JsStringLiteral)
	   (inline js-jsstring-name-set!::obj ::JsStringLiteral ::obj)
	   (inline js-ascii->jsstring::bstring ::bstring)
	   (inline js-index->jsstring::JsStringLiteralIndex ::bstring)
	   (inline js-utf8->jsstring::JsStringLiteralUTF8 ::bstring)
	   (js-string->jsstring::obj ::bstring)
	   (js-ascii-name->jsstring::JsStringLiteralASCII ::bstring)
	   (js-utf8-name->jsstring::JsStringLiteralUTF8 ::bstring)
	   (js-name->jsstring::JsStringLiteral ::bstring)
	   (js-stringlist->jsstring ::pair-nil)
	   (inline js-symbol->jsstring::obj ::symbol)
	   (inline js-jsstring->string::bstring ::obj)
	   (inline js-jsstring?::bool ::obj)
	   (js-jsstring-character-ref ::obj ::uint32)
	   (js-jsstring-ref ::obj ::uint32)
	   (js-ascii-ref ::JsStringLiteralASCII ::uint32)
	   (js-jsstring-length::uint32 ::obj)
	   (js-string-literal-length::uint32 ::JsStringLiteral)
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
	   (js-integer->jsstring ::long)
	   (js-string->bool::bool ::bstring)
	   (js-jsstring->bool::bool ::obj)
	   (generic js-jsstring-normalize!::JsStringLiteral ::JsStringLiteral)
	   
	   (js-jsstring-normalize-ASCII!::bstring ::JsStringLiteral)
	   (js-jsstring-normalize-UTF8!::bstring ::JsStringLiteral)
	   (inline js-jsstring-append::JsStringLiteral ::obj ::obj)
	   (utf8-codeunit-length::long ::bstring)
	   (js-utf8-ref ::JsStringLiteralUTF8 ::bstring ::long)
	   (js-get-string ::obj ::obj ::obj)
	   (js-put-string! ::bstring ::obj ::obj ::bool ::obj)
	   (js-jsstring-indexof ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-indexof ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-lastindexof ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-lastindexof ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-charcodeat ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-charcodeat-as-int32::int32 ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-charcodeatu32 ::obj ::uint32 ::JsGlobalObject)
	   (js-jsstring-charcodeatu32-as-int32::int32 ::obj ::uint32 ::JsGlobalObject)
	   (js-jsstring-maybe-charcodeat ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-charat ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-charat ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-substring ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-substring ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-substr ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-substr ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-tolowercase ::obj)
	   (js-jsstring-maybe-tolowercase ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-tolocalelowercase ::obj)
	   (js-jsstring-maybe-tolocalelowercase ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-touppercase ::obj)
	   (js-jsstring-maybe-touppercase ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-tolocaleuppercase ::obj)
	   (js-jsstring-maybe-tolocaleuppercase ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-split ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-split ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-replace-regexp ::obj ::regexp ::long ::bool ::obj ::JsGlobalObject)
	   (js-jsstring-replace-regexp-fun1 ::obj ::regexp ::long ::bool ::procedure ::JsGlobalObject)
	   (js-jsstring-replace-regexp-string ::obj ::regexp ::long ::bool ::obj ::JsGlobalObject)
	   (js-jsstring-replace-string ::obj ::bool ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-replace ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-prototype-replace ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-replace ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-match ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-match ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-naturalcompare ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-naturalcompare ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-localecompare ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-localecompare ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-trim ::obj)
	   (js-jsstring-maybe-trim ::obj ::JsGlobalObject ::obj)
	   (js-jsstring-fromcharcode ::JsObject ::obj ::JsGlobalObject)
	   (js-jsstring-escape ::obj)
	   (js-jsstring-unescape ::obj ::JsGlobalObject)
	   (js-jsstring-slice ::obj ::obj ::obj ::JsGlobalObject)
	   (js-jsstring-maybe-slice ::obj ::obj ::obj ::JsGlobalObject ::obj)
	   (js-jsstring->jsarray ::obj ::JsGlobalObject)
	   (js-jsstring->list ::obj))

   (cond-expand
      ((not bigloo4.3a)
       (pragma (js-string->jsstring default-inline)))))

;*---------------------------------------------------------------------*/
;*    property caches ...                                              */
;*---------------------------------------------------------------------*/
(%define-pcache 34)
(define %pcache (js-make-pcache-table 34 "hopscript/stringliteral.scm"))

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
;*    js-substring ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-substring s::bstring start::long end::long)
   (cond
      ((=fx end (+fx start 1))
       (vector-ref prealloc-strings (char->integer (string-ref s start))))
      ((>=fx start end)
       (js-ascii->jsstring ""))
      (else
       (js-string->jsstring (substring s start end)))))

;*---------------------------------------------------------------------*/
;*    js-substring/enc ...                                             */
;*---------------------------------------------------------------------*/
(define (js-substring/enc s::bstring start::long end::long utf8)
   (cond
      ((=fx end (+fx start 1))
       (vector-ref prealloc-strings (char->integer (string-ref s start))))
      ((>=fx start end)
       (js-ascii->jsstring ""))
      (utf8
       (js-utf8->jsstring (substring s start end)))
      (else
       (js-ascii->jsstring (substring s start end)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-debug ...                                            */
;*---------------------------------------------------------------------*/
(define (js-jsstring-debug msg obj)
   
   (define (excerpt str)
      (if (<fx (string-length str) 20)
	  (format "~s" str)
	  (format "~s..." (substring str 0 17))))

   (define (literal-or-string obj)
      (if (string? obj)
	  (excerpt obj)
	  (typeof obj)))
   
   (cond
      ((string? obj)
       (tprint msg " [string length=" (string-length obj) ":" (excerpt obj) "]"))
      (else
       (with-access::JsStringLiteral obj (left right weight)
	  (tprint msg "[" (typeof obj)
	     " weight=" weight " left=" (literal-or-string left) " right="
	     (literal-or-string right) "]")))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-debug-check! ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-debug-check! js::JsStringLiteral)
   (let loop ((s js))
      (cond
	 ((string? s)
	  #t)
	 ((not s)
	  #t)
	 (else
	  (with-access::JsStringLiteral s (weight left right)
	     (unless (=uint32 (if (string? left)
				  (fixnum->uint32 (string-length left))
				  (js-string-literal-length left))
			weight)
		(error "js-string-debug-check!" "bad string" (cons js s)))
	     (loop left)
	     (loop right))))))

;*---------------------------------------------------------------------*/
;*    string-dispatch ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (string-dispatch fun this . args)
   `(cond
       ((string? ,this)
	(,(symbol-append 'ascii- fun) ,this ,@args))
       ((isa? ,this JsStringLiteralASCII)
	(,(symbol-append 'ascii- fun) (js-jsstring-normalize-ASCII! ,this) ,@args))
       (else
	(,(symbol-append 'utf8- fun) (js-jsstring-normalize-UTF8! ,this) ,@args))))

;*---------------------------------------------------------------------*/
;*    string-dispatch ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (string-dispatch* fun this . args)
   `(cond
       ((string? ,this)
	(,(symbol-append 'ascii- fun) ,this ,@args))
       ((isa? ,this JsStringLiteralASCII)
	(,(symbol-append 'ascii- fun '*) ,this ,@args))
       (else
	(,(symbol-append 'utf8- fun) (js-jsstring-normalize-UTF8! ,this) ,@args))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-name ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-name o::JsStringLiteral)
   (object-widening o))

;*---------------------------------------------------------------------*/
;*    js-jsstring-name-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-name-set! o::JsStringLiteral name)
   (object-widening-set! o name))

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
;*    js-index->jsstring ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-index->jsstring val::bstring)
   (instantiate::JsStringLiteralIndex
      (weight (fixnum->uint32 (string-length val)))
      (left val)
      (index (fixnum->uint32 (string->integer val)))))

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
;*    js-ascii-name->jsstring ...                                      */
;*---------------------------------------------------------------------*/
(define (js-ascii-name->jsstring::JsStringLiteralASCII val::bstring)
   (instantiate::JsStringLiteralASCII
      (weight (fixnum->uint32 (string-length val)))
      (left val)
      (right #f)))

;*---------------------------------------------------------------------*/
;*    js-utf8-name->jsstring ...                                       */
;*---------------------------------------------------------------------*/
(define (js-utf8-name->jsstring val::bstring)
   (js-utf8->jsstring val))

;*---------------------------------------------------------------------*/
;*    js-name->jsstring ...                                            */
;*---------------------------------------------------------------------*/
(define (js-name->jsstring::JsStringLiteral val::bstring)
   (let ((enc (string-minimal-charset val)))
      (case enc
	 ((ascii) (js-ascii-name->jsstring val))
	 ((latin1 utf8) (js-utf8-name->jsstring val))
	 (else (error "js-name->jsstring" "unsupported encoding" enc)))))

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
(define (js-stringlist->jsstring val::pair-nil)
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
       (let ((lav (reverse! val)))
	  (let loop ((lav (cdr lav))
		     (acc (js-string->jsstring (car lav))))
	     (if (null? (cdr lav))
		 (js-jsstring-append (car lav) acc)
		 (loop (cdr lav) (js-jsstring-append (car lav) acc))))))))

;*---------------------------------------------------------------------*/
;*    js-symbol->jsstring ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-symbol->jsstring sym::symbol)
   (js-string->jsstring (symbol->string! sym)))

;*---------------------------------------------------------------------*/
;*    js-jsstring->string ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring->string::bstring js::obj)
   (if (string? js)
       js
       (with-access::JsStringLiteral js (left right)
	  (if (not right)
	      left
	      (begin
		 (js-jsstring-normalize! js)
		 left)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize-ASCII! ...                                 */
;*---------------------------------------------------------------------*/
(define (js-jsstring-normalize-ASCII! js::JsStringLiteral)
   
   (define (blit-buffer!::long s::bstring buffer::bstring i::long)
      (let ((len (string-length s)))
	 (case len
	    ((0) 0)
	    ((1) (begin (string-set! buffer i (string-ref s 0)) 1))
	    (else (begin (blit-string! s 0 buffer i len) len)))))

   (define (len s)
      (if (string? s)
	  (string-length s)
	  (js-string-literal-length s)))

   (with-access::JsStringLiteral js (left right weight)
      (cond
	 ((not right)
	  left)
	 ((=uint32 weight 0)
	  (if (string? right)
	      (let ((r right))
		 (set! weight (fixnum->uint32 (string-length r)))
		 (set! left r)
		 (set! right #f)
		 r)
	      (let ((r (js-jsstring-normalize-ASCII! right)))
		 (set! weight (fixnum->uint32 (string-length r)))
		 (set! left r)
		 (set! right #f)
		 r)))
	 (else
	  (let ((buffer (make-string
			   (uint32->fixnum (js-string-literal-length js)))))
	     (let loop ((i 0)
			(s js)
			(stack '()))
		(if (string? s)
		    (let* ((len (blit-buffer! s buffer i))
			   (ni (+fx i len)))
		       (if (pair? stack)
			   (let* ((top (car stack))
				  (ni (car top))
				  (s (cdr top)))
			      (loop ni s (cdr stack)))
			   (begin
			      (set! weight
				 (fixnum->uint32 (string-length buffer)))
			      (set! left buffer)
			      (set! right #f)
			      buffer)))
		    (with-access::JsStringLiteral s (left right weight)
		       (cond
			  ((not right)
			   ;; plain left descent
			   (loop i left stack))
			  ((string? right)
			   ;; write the rhs in advance
			   (let ((len (string-length right)))
			      (blit-buffer! right buffer
				 (+fx i (uint32->fixnum weight)))
			      (loop i left stack)))
			  (else
			   ;; full recursive call with pushed right
			   (let* ((ni (+fx i (uint32->fixnum weight)))
				  (nstack (cons (cons ni right) stack)))
			      (loop i left nstack))))))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-normalize-UTF8! ...                                  */
;*---------------------------------------------------------------------*/
(define (js-jsstring-normalize-UTF8! js::JsStringLiteral)
   
   (define (blit-utf8-buffer!::long s::bstring buffer::bstring i::long)
      (let ((len (string-length s)))
	 (case len
	    ((0) i)
	    ((1) (begin (string-set! buffer i (string-ref s 0)) (+fx i 1)))
	    (else (utf8-string-append-fill! buffer i s)))))

   (with-access::JsStringLiteralUTF8 js (left right weight %idxutf8 %idxstr)
      (cond
	 ((not right)
	  left)
	 ((=uint32 weight 0)
	  (cond
	     ((string? right)
	      (let ((r right))
		 (set! weight (fixnum->uint32 (string-length r)))
		 (set! left r)
		 (set! right #f)
		 (set! %idxutf8 0)
		 (set! %idxstr 0)
		 r))
	     ((isa? right JsStringLiteralASCII)
	      (let ((r (js-jsstring-normalize-ASCII! right)))
		 (set! weight (fixnum->uint32 (string-length r)))
		 (set! left r)
		 (set! right #f)
		 (set! %idxutf8 0)
		 (set! %idxstr 0)
		 r))
	     (else
	      (with-access::JsStringLiteralUTF8 right ((ridxutf8 %idxutf8)
						       (ridxstr %idxstr))
		 (let ((r (js-jsstring-normalize-UTF8! right)))
		    (set! weight (fixnum->uint32 (string-length r)))
		    (set! left r)
		    (set! right #f)
		    (set! %idxutf8 ridxutf8)
		    (set! %idxstr ridxstr)
		    r)))))
	 (else
	  (let ((buffer (make-string
			   (uint32->fixnum (js-string-literal-length js)))))
	     (let loop ((i 0)
			(s js)
			(stack '()))
		(if (string? s)
		    (let ((ni (blit-utf8-buffer! s buffer i)))
		       (if (pair? stack)
			   (let* ((top (car stack))
				  (ni (car top))
				  (s (cdr top)))
			      (loop ni s (cdr stack)))
			   (begin
			      (string-shrink! buffer ni)
			      (set! weight
				 (fixnum->uint32 (string-length buffer)))
			      (set! left buffer)
			      (set! right #f)
			      buffer)))
		    (with-access::JsStringLiteral s (left right weight)
		       (cond
			  ((not right)
			   ;; plain left descent
			   (loop i left stack))
			  (else
			   ;; full recursive call with pushed right
			   (let* ((ni (+fx i (uint32->fixnum weight)))
				  (nstack (cons (cons ni right) stack)))
			      (loop i left nstack))))))))))))

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
      ((isa? js JsStringLiteralUTF8)
       (with-access::JsStringLiteralUTF8 js (%culen)
	  (when (=u32 %culen #u32:0)
	     (set! %culen (utf8-codeunit-length (js-jsstring->string js))))
	  %culen))
      ((string? js)
       (fixnum->uint32 (string-length js)))
      (else
       (with-access::JsStringLiteralASCII js (right left weight)
	  (if (not right)
	      weight
	      (js-string-literal-length js))))))

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
   (or (eq? left right)
       (string=? (js-jsstring->string left) (js-jsstring->string right))))

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
		 `(js-index->jsstring ,(integer->string i)))
	    (iota num))))

;*---------------------------------------------------------------------*/
;*    integers ...                                                     */
;*---------------------------------------------------------------------*/
(define integers (make-integer-table 256))

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

   (let ((l (string-length s)))
      (cond
	 ((and (not (zeros32? r))
	       (or (<s32 r (fixnum->int32 2))
		   (>s32 r (fixnum->int32 36))))
	  +nan.0)
	 ((and (or (zeros32? r) (=s32 r (fixnum->int32 16)))
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
	     (if (<=fx l (if (<=fx r 10) 8 (if (<=fx r 16) 7 5)))
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
	       (or (char=? (string-ref s 1) #\x) (char=? (string-ref s 1) #\X)))
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
       (vector-ref integers num)))

;*---------------------------------------------------------------------*/
;*    js-string->bool ...                                              */
;*---------------------------------------------------------------------*/
(define (js-string->bool::bool js::bstring)
   (>fx (string-length js) 0))

;*---------------------------------------------------------------------*/
;*    js-jsstring->bool ...                                            */
;*---------------------------------------------------------------------*/
(define (js-jsstring->bool::bool js)
   (let loop ((js js))
      (cond
	 ((string? js)
	  (>fx (string-length js) 0))
	 ((not js)
	  #f)
	 (else
	  (with-access::JsStringLiteral js (weight right)
	     (or (>u32 weight 0) (loop right)))))))

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
(define (utf8-codeunit-ref this::JsStringLiteralUTF8 str i::long)
   
   (define (return-utf8 this str i j c s r u)
      (with-access::JsStringLiteralUTF8 this (%idxutf8 %idxstr)
	 (set! %idxstr r)
	 (set! %idxutf8 u)
;* 	 (tprint "return-utf8 r=" r " u=" u " s=" s " -> " (char->integer c)) */
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
;*    js-utf8-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-utf8-ref this::JsStringLiteralUTF8 str::bstring index::long)
   (let ((n (utf8-codeunit-ref this str index)))
      (cond
	 ((not (fixnum? n))
	  n)
	 ((<=fx n 255)
	  (vector-ref prealloc-strings n))
	 (else
	  (js-utf8->jsstring
	     (ucs2-string->utf8-string
		(ucs2-string
		   (integer->ucs2 n))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-ref ...                                              */
;*---------------------------------------------------------------------*/
(define (js-jsstring-ref o index::uint32)
   
   (define (ascii-string-ref val index)
      (if (>=u32 index (fixnum->uint32 (string-length val)))
	  (js-undefined)
	  (vector-ref prealloc-strings
	     (char->integer (string-ref-ur val (uint32->fixnum index))))))
   
   (define (utf8-string-ref val index)
      (if (>=u32 index (js-jsstring-codeunit-length o))
	  (js-undefined)
	  (js-utf8-ref o val (uint32->fixnum index))))

   (string-dispatch string-ref o index))

;*---------------------------------------------------------------------*/
;*    js-ascii-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-ascii-ref o::JsStringLiteralASCII index::uint32)
   (let ((str (js-jsstring-normalize-ASCII! o)))
      (if (>=u32 index (fixnum->uint32 (string-length str)))
	  (js-undefined)
	  (vector-ref prealloc-strings
	     (char->integer (string-ref-ur str (uint32->fixnum index)))))))

;*---------------------------------------------------------------------*/
;*    js-string-ref ...                                                */
;*---------------------------------------------------------------------*/
(define (js-string-ref o prop %this)
   (cond
      ((and (fixnum? prop) (>=fx prop 0))
       (js-jsstring-ref o (fixnum->uint32 prop)))
      (else
       (js-get o prop %this))))

;*---------------------------------------------------------------------*/
;*    js-string-ref-as-string ...                                      */
;*---------------------------------------------------------------------*/
(define (js-string-ref-as-string o prop %this)
   (cond
      ((fixnum? prop)
       (js-jsstring-ref o (fixnum->uint32 prop)))
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
	  (utf8-string-ref val fxpos)))
   
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
			(let ((proto (js-object-get-name/cache js-string 'prototype
					#f %this
					(js-pcache-ref %pcache 0))))
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
(define-method (js-get-length o::JsStringLiteral %this #!optional cache)
   (uint32->fixnum (js-jsstring-codeunit-length o)))

;*---------------------------------------------------------------------*/
;*    js-get-lengthu32 ::JsStringLiteral ...                           */
;*---------------------------------------------------------------------*/
;* (define-method (js-get-lengthu32 o::JsStringLiteral %this #!optional cache) */
;*    (js-jsstring-codeunit-length o))                                 */

;*---------------------------------------------------------------------*/
;*    js-get-length ::JsStringLiteralASCII ...                         */
;*---------------------------------------------------------------------*/
(define-method (js-get-length o::JsStringLiteralASCII %this #!optional cache)
   (js-jsstring-lengthfx o))

;*---------------------------------------------------------------------*/
;*    js-get-lengthu32 ::JsStringLiteralASCII ...                      */
;*---------------------------------------------------------------------*/
;* (define-method (js-get-lengthu32 o::JsStringLiteralASCII %this #!optional) */
;*    (js-string-literal-length o))                                    */

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

      (let* ((pat (js-jsstring->string search))
	     (patlen (string-length pat)))
	 (if (=fx patlen 0)
	     0
	     (string-dispatch indexof this pat patlen))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-indexof ...                                    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-indexof this search position %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-indexof this (js-tostring search %this) position %this))
	 ((js-array? this)
	  (js-array-indexof this search position %this
	     (or cache (js-pcache-ref %pcache 23))))
	 ((isa? this JsObject)
	  (js-call2 %this
	     (js-get-name/cache this 'indexOf #f %this
		(or cache (js-pcache-ref %pcache 23)))
	     this search position))
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

   (string-dispatch lastindexof this))
   
;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-lastindexof ...                                */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-lastindexof this search position %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-lastindexof this (js-tostring search %this) position %this))
	 ((isa? this JsObject)
	  (js-call2 %this
	     (js-get-name/cache this 'lastIndexOf #f %this
		(or cache (js-pcache-ref %pcache 2)))
	     this search position))
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
(define (js-jsstring-charcodeatu32 this position::uint32 %this)
   
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
(define (js-jsstring-charcodeatu32-as-int32::int32 this position::uint32 %this)
   
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
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-charcodeat this index %this))
	 ((isa? this JsObject)
	  (js-call1 %this
	     (js-get-name/cache this 'charCodeAt #f %this
		(or cache (js-pcache-ref %pcache 3)))
	     this index))
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
	     (o (vector-ref charat-table i)))
	 (or o
	     (let ((ns (js-ascii->jsstring (make-string 1 c))))
		(vector-set! charat-table i ns)
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
	     ((>=u32 (fixnum->uint32 position) (js-jsstring-codeunit-length this))
	      (js-ascii->jsstring ""))
	     (else
	      (js-utf8-ref this val position)))
	  (let ((pos (js-tointeger position %this)))
	     (if (or (< pos 0) (>= pos (js-jsstring-codeunit-length this)))
		 (js-ascii->jsstring "")
		 (js-utf8-ref this val (->fixnum pos))))))

   (string-dispatch charat this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-charat ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-charat this index %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-charat this index %this))
	 ((isa? this JsObject)
	  (js-call1 %this
	     (js-get-name/cache this 'charAt #f %this
		(or cache (js-pcache-ref %pcache 4)))
	     this index))
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
   
   (string-dispatch substr this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-substring ...                                  */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-substring this start end %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-substring this start end %this))
	 ((isa? this JsObject)
	  (js-call2 %this
	     (js-get-name/cache this 'substring #f %this
		(or cache (js-pcache-ref %pcache 5)))
	     this start end))
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
   
   (string-dispatch substr (js-jsstring->string this)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-substr ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-substr this start length %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-substr this start length %this))
	 ((isa? this JsObject)
	  (js-call2 %this
	     (js-get-name/cache this 'substr #f %this
		(or cache (js-pcache-ref %pcache 6)))
	     this start length))
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

   (string-dispatch tolowercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-tolowercase ...                                */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-tolowercase this %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-tolowercase this))
	 ((isa? this JsObject)
	  (js-call0 %this
	     (js-get-name/cache this 'toLowerCase #f %this
		(or cache (js-pcache-ref %pcache 7)))
	     this))
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

   (string-dispatch tolowercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-tolocalelowercase ...                          */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-tolocalelowercase this %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-tolocalelowercase this))
	 ((isa? this JsObject)
	  (js-call0 %this
	     (js-get-name/cache this 'toLocaleLowerCase #f %this
		(or cache (js-pcache-ref %pcache 8)))
	     this))
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

   (string-dispatch touppercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-touppercase ...                                */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-touppercase this %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-touppercase this))
	 ((isa? this JsObject)
	  (js-call0 %this
	     (js-get-name/cache this 'toUpperCase #f %this
		(or cache (js-pcache-ref %pcache 9)))
	     this))
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

   (string-dispatch touppercase this))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-tolocaleuppercase ...                          */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-tolocaleuppercase this %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-tolocaleuppercase this))
	 ((isa? this JsObject)
	  (js-call0 %this
	     (js-get-name/cache this 'toLocaleUpperCase #f %this
		(or cache (js-pcache-ref %pcache 10)))
	     this))
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
					(l (->fixnum (js-get-length A %this))))
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
			  (l (js-get-length A %this)))
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
(define (js-jsstring-maybe-split this separator limit %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-split this separator limit %this))
	 ((isa? this JsObject)
	  (js-call2 %this
	     (js-get-name/cache this 'split #f %this
		(or cache (js-pcache-ref %pcache 11)))
	     this separator limit))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    table22 ...                                                      */
;*---------------------------------------------------------------------*/
(define (table22 fmt::bstring match::pair string::bstring)
   
   (define (digit->number c)
      (-fx (char->integer c) (char->integer #\0)))
   
   (define (digit10->number c1 c2)
      (+fx (*fx (digit->number c1) 10) (digit->number c2)))
   
   (let ((stop (-fx (string-length fmt) 1)))
      (let loop ((i 0)
		 (j 0)
		 (res (js-ascii->jsstring "")))
	 (cond
	    ((>=fx i stop)
	     (js-jsstring-append res (js-substring fmt j (+fx stop 1))))
	    ((not (char=? (string-ref fmt i) #\$))
	     (loop (+fx i 1) j res))
	    (else
	     (case (string-ref fmt (+fx i 1))
		((#\$)
		 (let ((res (js-jsstring-append res (js-substring fmt j i))))
		    (loop (+fx i 2) (+fx i 2)
		       (js-jsstring-append res (js-ascii->jsstring "$")))))
		((#\&)
		 (let ((res (js-jsstring-append res (js-substring fmt j i)))
		       (portion (js-substring string (caar match) (cdar match))))
		    (loop (+fx i 2) (+fx i 2)
		       (js-jsstring-append res portion))))
		((#\`)
		 (let ((res (js-jsstring-append res (js-substring fmt j i)))
		       (portion (js-substring string 0 (caar match))))
		    (loop (+fx i 2) (+fx i 2)
		       (js-jsstring-append res portion))))
		((#\')
		 (let ((res (js-jsstring-append res (js-substring fmt j i)))
		       (portion (js-substring string (cdar match)
				   (string-length string))))
		    (loop (+fx i 2) (+fx i 2)
		       (js-jsstring-append res portion))))
		((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		 (let ((res (js-jsstring-append res (js-substring fmt j i)))
		       (len (length match)))
		    (if (or (=fx i (-fx stop 1))
			    (not (char-numeric? (string-ref fmt (+fx i 2)))))
			(let ((n (digit->number (string-ref fmt (+fx i 1)))))
			   (if (>fx n len)
			       (loop (+fx i 2) j res)
			       (let* ((m (list-ref match n))
				      (portion (js-substring string (car m) (cdr m))))
				  (loop (+fx i 2) (+fx i 2)
				     (js-jsstring-append res portion)))))
			(let ((n (digit10->number
				    (string-ref fmt (+fx i 1))
				    (string-ref fmt (+fx i 2)))))
			   (if (>fx n len)
			       (let ((n (digit->number (string-ref fmt (+fx i 1)))))
				  (if (>=fx n len)
				      (loop (+fx i 3) j res)
				      (let* ((m (list-ref match n))
					     (portion (js-substring string (car m) (cdr m))))
					 (loop (+fx i 2) (+fx i 2)
					    (js-jsstring-append res portion)))))
			       (let* ((m (list-ref match n))
				      (portion (js-substring string (car m) (cdr m))))
				  (loop (+fx i 3) (+fx i 3)
				     (js-jsstring-append res portion))))))))
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
      ((isa? replacevalue JsFunction)
       (with-access::JsFunction replacevalue (procedure)
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
	  lastindex global (js-tostring replacevalue %this) %this))))

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
		  (js-substring/enc s (car m) (cdr m) enc)
		  (js-ascii->jsstring "")))
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
		       (js-substring/enc s 0 (caar r) enc)
		       (js-jsstring-append
			  (js-tojsstring
			     (js-apply %this replacevalue (js-undefined)
				(append (matches->string-list r s enc)
				   (list (caar r) this)))
			     %this)
			  (js-substring/enc s
			     (cdar r) (string-length s) enc)))))))
	    (else
	     (let loop ((len (string-length s)))
		(let loop ((i 0)
			   (res (js-ascii->jsstring "")))
		   (let ((r (pregexp-match-positions rx s i)))
		      (if (not r)
			  (cond
			     ((=fx i 0)
			      this)
			     ((>=fx i len)
			      res)
			     (else
			      (js-jsstring-append res
				 (js-substring/enc s i len enc))))
			  (let ((v (js-tojsstring
				      (js-apply %this replacevalue (js-undefined)
					 (append (matches->string-list r s enc)
					    (list (caar r) this)))
				      %this)))
			     (cond
				((>fx (cdar r) i)
				 (loop (cdar r)
				    (js-jsstring-append
				       (js-jsstring-append res
					  (js-substring/enc s i (caar r) enc))
				       v)))
				((<fx i len)
				 (loop (+fx i 1)
				    (js-jsstring-append
				       (js-jsstring-append res
					  (js-substring/enc s i (caar r) enc))
				       (js-jsstring-append
					  v
					  (js-substring/enc s i (+fx i 1) enc)))))
				(else
				 (js-jsstring-append
				    (js-jsstring-append res
				       (js-substring/enc s i (caar r) enc))
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
	  (js-substring s (vector-ref pos 0) (vector-ref pos 1))
	  (js-ascii->jsstring "")))

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
		       (js-substring/enc s 0 (vector-ref pos 0) enc)
		       (js-jsstring-append
			  (js-tojsstring
			     (replacevalue (js-undefined)
				(first-match pos s))
			     %this)
			  (js-substring/enc s
			     (vector-ref pos 1) (string-length s) enc)))))))
	    (else
	     (let loop ((i 0)
			(res (js-ascii->jsstring "")))
		(let ((r (pregexp-match-n-positions! rx s pos i len)))
		   (if (<=fx r 0)
		       (cond
			  ((=fx i 0)
			   this)
			  ((>=fx i len)
			   res)
			  (else
			   (js-jsstring-append res
			      (js-substring/enc s i len enc))))
		       (let ((v (js-tojsstring
				   (replacevalue (js-undefined)
				      (first-match pos s))
				   %this)))
			  (cond
			     ((>fx (vector-ref pos 1) i)
			      (loop (vector-ref pos 1)
				 (js-jsstring-append
				    (js-jsstring-append res
				       (js-substring/enc s i (vector-ref pos 0) enc))
				    v)))
			     ((<fx i len)
			      (loop (+fx i 1)
				 (js-jsstring-append
				    (js-jsstring-append res
				       (js-substring/enc s i
					  (vector-ref pos 0) enc))
				    (js-jsstring-append
				       v
				       (js-substring/enc s i (+fx i 1) enc)))))
			     (else
			      (js-jsstring-append
				 (js-jsstring-append res
				    (js-substring/enc s i
				       (vector-ref pos 0) enc))
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
		  (js-substring/enc s (car m) (cdr m) enc)
		  (js-ascii->jsstring "")))
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
		    (js-substring/enc s 0 (caar r) enc)
		    (js-jsstring-append
		       (table22 newstring r s)
		       (js-substring/enc s (cdar r)
			  (string-length s) enc))))
		(else
		 (js-jsstring-append
		    (js-substring/enc s 0 (caar r) enc)
		    (js-jsstring-append
		       newstring
		       (js-substring/enc s (cdar r)
			  (string-length s) enc)))))))
	 (else
	  (if (string-index newstring #\$)
	      (let loop ((i 0)
			 (res (js-ascii->jsstring "")))
		 (let ((r (pregexp-match-positions rx s i)))
		    (if (not r)
			(cond
			   ((=fx i 0)
			    this)
			   ((>=fx i len)
			    res)
			   (else
			    (js-jsstring-append res
			       (js-substring/enc s i len enc))))
			(let ((v (table22 newstring r s)))
			   (cond
			      ((>fx (cdar r) i)
			       (loop (cdar r)
				  (js-jsstring-append
				     (js-jsstring-append res
					(js-substring/enc s i (caar r) enc))
				     v)))
			      ((<fx i len)
			       (loop (+fx i 1)
				  (js-jsstring-append
				     (js-jsstring-append res
					(js-substring/enc s i (caar r)
					   enc))
				     (js-jsstring-append
					v
					(js-substring/enc s i (+fx i 1)
					   enc)))))
			      (else
			       (js-jsstring-append
				  (js-jsstring-append res
				     (js-substring/enc s i (caar r) enc))
				  v)))))))
	      (let ((pos (vector -1 -1)))
		 (let loop ((i 0)
			    (res (js-ascii->jsstring "")))
		    (let ((r (pregexp-match-n-positions! rx s pos i len)))
		       (if (<=fx r 0)
			   (begin
			      (cond
				 ((=fx i 0)
				  this)
				 ((>=fx i len)
				  res)
				 (else
				  (js-jsstring-append res
				     (js-substring/enc s i len enc)))))
			   (let ((v newstring))
			      (cond
				 ((>fx (vector-ref pos 1) i)
				  (loop (vector-ref pos 1)
				     (js-jsstring-append
					(js-jsstring-append res
					   (js-substring/enc s i
					      (vector-ref pos 0) enc))
					v)))
				 ((<fx i len)
				  (loop (+fx i 1)
				     (js-jsstring-append
					(js-jsstring-append res
					   (js-substring/enc s i
					      (vector-ref pos 0) enc))
					(js-jsstring-append
					   v
					   (js-substring/enc s i
					      (+fx i 1) enc)))))
				 (else
				  (js-jsstring-append
				     (js-jsstring-append res
					(js-substring/enc s i
					   (vector-ref pos 0) enc))
				     v)))))))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-replace-string ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-replace-string this::obj need22 searchstr replacevalue %this)
   
   (define (matches->string-list::pair-nil a)
      (let ((len (js-get-length a %this)))
	 (let loop ((i 1)
		    (l '()))
	    (if (=fx i len)
		(reverse! l)
		(let ((v (js-get a (js-toname i %this) %this)))
		   (loop (+fx i 1)
		      (cons (if (eq? v (js-undefined)) (js-ascii->jsstring "") v)
			 l)))))))

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
			     j (string-length string) enc))))
	     (if (>fx i 0)
		 (js-jsstring-append (js-substring/enc string 0 i enc) tail)
		 tail)))
	 ((isa? replacevalue JsFunction)
	  (let ((j (+fx i (js-jsstring-lengthfx searchstr))))
	     (js-stringlist->jsstring
		(list
		   (js-substring/enc string 0 i enc)
		   (js-tostring
		      (js-call3 %this replacevalue (js-undefined)
			 searchstr i string) %this)
		   (js-substring/enc string
		      j (string-length string) enc)))))
	 (else
	  (let* ((newstring (js-tostring replacevalue %this))
		 (j (+fx i (js-jsstring-lengthfx searchstr)))
		 (tail (js-jsstring-append
			  (table22 newstring (list (cons i j)) string)
			  (js-substring/enc string
			     j (string-length string) enc))))
	     (if (>fx i 0)
		 (js-jsstring-append (js-substring/enc string 0 i enc) tail)
		 tail))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-replace ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-replace this::obj searchvalue replacevalue %this)
   
   (define (fun1? v)
      (when (isa? v JsFunction)
	 (with-access::JsFunction v (procedure)
	    (correct-arity? procedure 2))))
   
   (define (fun1 v)
      (with-access::JsFunction v (procedure)
	 procedure))
   
   (cond
      ((isa? searchvalue JsRegExp)
       (with-access::JsRegExp searchvalue (rx flags)
	  (let* ((lastindex (js-object-get-name/cache searchvalue 'lastIndex
			       #f %this (js-pcache-ref %pcache 12)))
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
		(js-object-put-name/cache! searchvalue 'lastIndex 0
		   #f %this (js-pcache-ref %pcache 12)))
	     res)))
      ((js-jsstring? searchvalue)
       (js-jsstring-replace-string this #t searchvalue replacevalue %this))
      (else
       (js-jsstring-replace-string this #t (js-tostring searchvalue %this) replacevalue %this)
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
	  (js-jsstring-replace this searchvalue replacevalue %this))
	 ((isa? this JsString)
	  (with-access::JsString this (val)
	     (loop val)))
	 ((isa? this JsObject)
	  (loop (js-tojsstring this %this)))
	 (else
	  (loop (js-tojsstring (js-toobject %this this) %this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-replace ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.5.4.11    */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-replace this searchvalue replacevalue %this cache)
   (cond
      ((js-jsstring? this)
       (js-jsstring-replace this searchvalue replacevalue %this))
      ((isa? this JsString)
       (with-access::JsString this (val)
	  (js-jsstring-replace val searchvalue replacevalue %this)))
      (else
       (js-call2 %this
	  (js-get-name/cache this 'replace #f %this
	     (or cache (js-pcache-ref %pcache 13)))
	  this searchvalue replacevalue))))

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
	     (proto (js-get-name/cache js-regexp 'prototype
		       #f %this (js-pcache-ref %pcache 14)))
	     (exec (js-get-name/cache proto 'exec
		      #f %this (js-pcache-ref %pcache 15))))
	 (with-access::JsRegExp rx (flags)
	    (let ((lastindex (js-object-get-name/cache rx 'lastIndex
				#f %this (js-pcache-ref %pcache 16)))
		  (global (js-regexp-flags-global? flags)))
	       ;; 7
	       (if (not global)
		   (js-call1 %this exec rx s)
		   ;; 8
		   (let ((previousLastIndex 0)
			 (a (js-null)))
		      (set! lastindex 0)
		      (js-object-put-name/cache! rx 'lastIndex lastindex
			 #f %this (js-pcache-ref %pcache 17))
		      (let loop ((n 0))
			 (let ((result (js-call1 %this exec rx s)))
			    (set! lastindex
			       (js-object-get-name/cache rx 'lastIndex
				  #f %this (js-pcache-ref %pcache 17)))
			    (if (eq? result (js-null))
				a
				(let ((thisIndex lastindex))
				   (if (= thisIndex previousLastIndex)
				       (begin
					  (set! lastindex (+ thisIndex 1))
					  (js-object-put-name/cache! rx 'lastIndex lastindex
					     #f %this (js-pcache-ref %pcache 17))
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
				   (loop (+fx 1 n)))))))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-match ...                                      */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-match this regexp %this cache)
   (let loop ((this this))
      (cond
	 ((js-jsstring? this)
	  (js-jsstring-match this regexp %this))
	 ((isa? this JsObject)
	  (js-call1 %this
	     (js-get-name/cache this 'match #f %this
		(or cache (js-pcache-ref %pcache 18)))
	     this regexp))
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
	 ((isa? this JsObject)
	  (js-call1 %this
	     (js-get-name/cache this 'naturalCompare #f %this
		(or cache (js-pcache-ref %pcache 19)))
	     this that))
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
	 ((isa? this JsObject)
	  (js-call1 %this
	     (js-get-name/cache this 'localeCompare #f %this
		(or cache (js-pcache-ref %pcache 20)))
	     this that))
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
	 ((isa? this JsObject)
	  (js-call0 %this
	     (js-get-name/cache this 'trim #f %this
		(or cache (js-pcache-ref %pcache 21)))
	     this))
	 (else
	  (loop (js-toobject %this this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-fromcharcode ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-fromcharcode this code %this)
   (let loop ((code code))
      (cond
	 ((or (not (fixnum? code)) (<fx code 0) (>=fx code 65536))
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
				       
				       (blit-string! utf8 0 buf j l)
				       (loop (+fx ni 3) (+fx j l))))))))))))
	    (else
	     (let ((l (-fx ol i)))
		(blit-string! str i buf j l)
		(loop ol (+fx j l))))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-slice ...                                            */
;*---------------------------------------------------------------------*/
(define (js-jsstring-slice jss::obj start end %this)
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
	  (js-jsstring-substring jss from end %this)
	  jss)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-maybe-slice ...                                      */
;*---------------------------------------------------------------------*/
(define (js-jsstring-maybe-slice this start end %this cache)
   (if (js-jsstring? this)
       (js-jsstring-slice this start end %this)
       (let ((slice (js-get-name/cache this 'slice #f %this
		       (or cache (js-pcache-ref %pcache 22)))))
	  (js-call2 %this slice this start end))))

;*---------------------------------------------------------------------*/
;*    js-jsstring->jsarray ...                                         */
;*---------------------------------------------------------------------*/
(define (js-jsstring->jsarray o %this)
   
   (define (ascii->array val %this)
      (let* ((len (string-length o))
	     (vec (make-vector len)))
	 (let loop ((i 0))
	    (if (<fx i len)
		(let ((val (vector-ref prealloc-strings
			      (char->integer (string-ref-ur o i)))))
		   (vector-set! vec i val)
		   (loop (+fx i 1)))
		(js-vector->jsarray vec %this)))))
      
   (define (utf8->array val %this)
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
(define (js-jsstring->list o)
   
   (define (ascii->list val)
      (map! (lambda (c) (vector-ref prealloc-strings (char->integer c)))
	 (string->list val)))
   
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
   (let ((len (string-length o)))
      (let loop ((i 0))
	 (when (<fx i len)
	    (let ((val (vector-ref prealloc-strings
			  (char->integer (string-ref-ur o i)))))
	       (proc val %this)
	       (loop (+fx i 1)))))))

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

