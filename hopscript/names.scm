;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/names.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar 30 06:29:09 2019                          */
;*    Last change :  Thu Oct 10 10:02:36 2019 (serrano)                */
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
	   __hopscript_public
	   __hopscript_private)
   
   (export (js-init-names!)
	   (js-get-js-string-names)
	   (js-get-js-integer-names)
	   (inline js-name-pcacher::obj ::JsStringLiteral)
	   (inline js-name-pcacher-set! ::JsStringLiteral ::JsPropertyCache)
	   (inline js-name-pcachew::obj ::JsStringLiteral)
	   (inline js-name-pcachew-set! ::JsStringLiteral ::JsPropertyCache)
	   (inline js-jsstring-toname::JsStringLiteral ::JsStringLiteral)
	   (inline js-jsstring-toname-unsafe::JsStringLiteral ::JsStringLiteral)
	   (js-jsstring->name!::JsStringLiteral o::JsStringLiteral)
	   (js-toname::obj ::obj ::JsGlobalObject)
	   (inline js-jsstring-name ::JsStringLiteral)
	   (inline js-name->string::bstring ::JsStringLiteral)
	   (js-ascii-name->jsstring::JsStringLiteralASCII ::bstring)
	   (js-utf8-name->jsstring::JsStringLiteralUTF8 ::bstring)
	   (js-integer-name->jsstring::JsStringLiteralASCII ::long)
	   (js-integer-name::obj ::long)
	   (js-name->jsstring::JsStringLiteral ::bstring))

   (export js-name-lock
	   (macro synchronize-name))
   
   (static js-names js-integer-length)
   (export js-integer-names js-string-names)

   (cond-expand
      (enable-tls
       (export gcroots)))
   
   (cond-expand
      (enable-tls
       (pragma (js-names thread-local)
	  (js-integer-length thread-local)
	  (js-integer-names thread-local)
	  (js-string-names thread-local)))))

;*---------------------------------------------------------------------*/
;*    name tables                                                      */
;*---------------------------------------------------------------------*/
(define js-names #f)
(define js-integer-names #f)
(define js-string-names #f)

(define js-integer-length 100)

;; cannot inline these two functions because of thread-local variables
(define (js-get-js-string-names) js-string-names)
(define (js-get-js-integer-names) js-integer-names)

;*---------------------------------------------------------------------*/
;*    thresholds                                                       */
;*---------------------------------------------------------------------*/
(define-inline (jsindex-threshold::long)
   (uint32->fixnum #u32:1000000))

;*---------------------------------------------------------------------*/
;*    js-name-lock                                                     */
;*---------------------------------------------------------------------*/
(define js-name-lock (make-spinlock "js-names"))

;*---------------------------------------------------------------------*/
;*    synchronize-name ...                                             */
;*---------------------------------------------------------------------*/
(define-macro (synchronize-name . body)
   (cond-expand
      (enable-tls `(begin ,@body))
      (else `(synchronize js-name-lock ,@body))))

;*---------------------------------------------------------------------*/
;*    gcroots                                                          */
;*---------------------------------------------------------------------*/
(define gcroots '())

;*---------------------------------------------------------------------*/
;*    integer-string? ...                                              */
;*---------------------------------------------------------------------*/
(define (integer-string? str)
   (let ((len (string-length str)))
      (case len
	 ((0)
	  #f)
	 ((1)
	  (char-numeric? (string-ref str 0)))
	 (else
	  (case (string-ref str 0)
	     ((#\0)
	      #f)
	     ((#\-)
	      (when (not (char=? (string-ref str 1) #\0))
		 (let loop ((i 1))
		    (cond
		       ((=fx i len) #t)
		       ((char-numeric? (string-ref str i)) (loop (+fx i 1)))
		       (else #f)))))
	     (else
	      (let loop ((i 0))
		 (cond
		    ((=fx i len) #t)
		    ((char-numeric? (string-ref str i)) (loop (+fx i 1)))
		    (else #f)))))))))

;*---------------------------------------------------------------------*/
;*    string-compare? ...                                              */
;*---------------------------------------------------------------------*/
(define (string-compare? x y)
   (cond-expand
      (bigloo-c
       (when (pragma::bool "BSTRING_TO_STRING( $1 )[ 0 ] == BSTRING_TO_STRING( $2 )[ 0 ]" x y)
	  (let ((l1 (string-length x)))
	     (when (=fx l1 (string-length y))
		(if (>fx l1 0)
		    (pragma::bool "!memcmp( (void *)BSTRING_TO_STRING( $1 ), (void *)BSTRING_TO_STRING( $2 ), $3 )" x y l1)
		    #t)))))
      (else
       (string=? x y))))

;*---------------------------------------------------------------------*/
;*    js-init-names! ...                                               */
;*---------------------------------------------------------------------*/
(define (js-init-names!)
   (synchronize js-name-lock
      (unless (hashtable? js-names)
	 (set! js-integer-length
	    100)
	 (set! js-names
	    (let ((table (create-hashtable
			    :eqtest string-compare?
			    :hash string-hash-number
			    :max-length 65536
			    :max-bucket-length 20)))
	       (cond-expand (enable-tls (set! gcroots (cons table gcroots))))
	       table))
         (set! js-integer-names
	    (let ((inames (list->vector
			     (append
				(map (lambda (i)
					(js-integer->name i))
				   (iota 10 -10))
				(map (lambda (i)
					(js-index->name (fixnum->uint32 i)))
				   (iota 100))))))
	       (cond-expand (enable-tls (set! gcroots (cons inames gcroots))))
	       inames))
	 (set! js-string-names
	    (let ((snames (vector-map (lambda (val)
					 (js-ascii-toname-unsafe val))
			     (& strings))))
	       (cond-expand (enable-tls (set! gcroots (cons snames gcroots))))
	       snames)))))

;*---------------------------------------------------------------------*/
;*    js-name-pcacher ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-name-pcacher::obj o::JsStringLiteral)
   (with-access::JsStringLiteral o (pcacher)
      pcacher))

;*---------------------------------------------------------------------*/
;*    js-name-pcacher-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-name-pcacher-set! o::JsStringLiteral c::JsPropertyCache)
   (with-access::JsStringLiteral o (pcacher)
      (set! pcacher c)))

;*---------------------------------------------------------------------*/
;*    js-name-pcachew ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-name-pcachew::obj o::JsStringLiteral)
   (with-access::JsStringLiteral o (pcachew)
      pcachew))

;*---------------------------------------------------------------------*/
;*    js-name-pcachew-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-name-pcachew-set! o::JsStringLiteral c::JsPropertyCache)
   (with-access::JsStringLiteral o (pcachew)
      (set! pcachew c)))

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
;*    js-name->jsstring ...                                            */
;*---------------------------------------------------------------------*/
(define (js-name->jsstring::JsStringLiteral str::bstring)
   (let ((enc (string-minimal-charset str)))
      (case enc
	 ((ascii) (js-ascii-name->jsstring str))
	 ((latin1 utf8) (js-utf8-name->jsstring str))
	 (else (error "js-name->jsstring" "unsupported encoding" enc)))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-toname ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-toname::JsStringLiteral p::JsStringLiteral)
   (or (js-jsstring-name p) (synchronize-name (js-jsstring->name! p))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-toname-unsafe ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-toname-unsafe::JsStringLiteral p::JsStringLiteral)
   (or (js-jsstring-name p) (js-jsstring->name! p)))

;*---------------------------------------------------------------------*/
;*    js-toname ...                                                    */
;*    -------------------------------------------------------------    */
;*    www.ecma-international.org/ecma-262/7.0/#sec-topropertykey       */
;*---------------------------------------------------------------------*/
(define (js-toname p %this)
   (cond
      ((and (object? p)
	    (or (eq? (object-class p) JsStringLiteralASCII)
		(js-jsstring? p)))
       (js-jsstring-toname p))
      ((fixnum? p)
       (js-integer-name->jsstring p))
      ((uint32? p)
       (cond-expand
	  (bint30
	   (if (<u32 p (fixnum->uint32 (bit-lsh 1 29)))
	       (js-integer-name->jsstring (uint32->fixnum p))
	       (js-ascii-toname (llong->string (uint32->llong p)))))
	  (bint32
	   (if (<u32 p (bit-lshu32 (fixnum->uint32 1) 31))
	       (js-integer-name->jsstring (uint32->fixnum p))
	       (js-ascii-toname (llong->string (uint32->llong p)))))
	  (else
	   (js-integer-name->jsstring (uint32->fixnum p)))))
      ((int32? p)
       (cond-expand
	  (bint30
	   (if (and (>s32 p 0) (<s32 p (fixnum->int32 (bit-lsh 1 29))))
	       (js-integer-name->jsstring (int32->fixnum p))
	       (js-ascii-toname (llong->string (int32->llong p)))))
	  (bint32
	   (js-ascii-name->jsstring (fixnum->string (int32->fixnum p))))
	  (else
	   (js-integer-name->jsstring (int32->fixnum p)))))
      ((isa? p JsSymbolLiteral)
       p)
      ((isa? p JsSymbol)
       (with-access::JsSymbol p (val)
	  val))
      ((number? p)
       (js-ascii-name->jsstring (js-number->string p)))
      ((symbol? p)
       (error "js-toname" "Illegal `symbol'" p))
      ((string? p)
       (error "js-toname" "Illegal `string'" p))
      (else
       (js-name->jsstring (js-tostring p %this)))))

;*---------------------------------------------------------------------*/
;*    js-name->string ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-name->string::bstring o::JsStringLiteral)
   (with-access::JsStringLiteral o (left)
      left))

;*---------------------------------------------------------------------*/
;*    js-jsstring->name! ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsstring->name!::JsStringLiteral o::JsStringLiteral)
   ;; call js-jsstring->string as the string must be normalized
   ;; before being potentially added in the name hashtable
   
   (define (string-name str)
      (let ((n (hashtable-get js-names str)))
	 (unless n
	    (set! n o)
	    (hashtable-put! js-names str n))
	 (js-jsstring-name-set! o n)
	 n))
   
   (let ((str (js-jsstring->string o)))
      (if (integer-string? str)
	  (let ((num (string->integer str)))
	     (if (or (<=fx num -10)
		     (>=fx num (jsindex-threshold)))
		 (string-name str)
		 (js-integer-toname-unsafe num)))
	  (string-name str))))

;*---------------------------------------------------------------------*/
;*    js-ascii-toname-unsafe ...                                       */
;*---------------------------------------------------------------------*/
(define (js-ascii-toname-unsafe::JsStringLiteralASCII str::bstring)
   (let ((n (hashtable-get js-names str)))
      (or n
	  (let ((o (instantiate::JsStringLiteralASCII
		      (weight (fixnum->uint32 (string-length str)))
		      (left str)
		      (right #f))))
	     (js-object-mode-set! o (js-jsstring-default-mode))
	     (hashtable-put! js-names str o)
	     (js-jsstring-name-set! o o)
	     o))))

;*---------------------------------------------------------------------*/
;*    js-ascii-toname ...                                              */
;*---------------------------------------------------------------------*/
(define (js-ascii-toname::JsStringLiteralASCII str::bstring)
   (synchronize-name
      (js-ascii-toname-unsafe str)))

;*---------------------------------------------------------------------*/
;*    js-ascii-name->jsstring ...                                      */
;*---------------------------------------------------------------------*/
(define (js-ascii-name->jsstring::JsStringLiteralASCII str::bstring)
   (synchronize-name
      (if (integer-string? str)
	  (let ((num (string->integer str)))
	     (if (or (<=fx num -10)
		     (>=fx num (jsindex-threshold)))
		 (js-ascii-toname-unsafe str)
		 (js-integer-toname-unsafe num)))
	  (js-ascii-toname-unsafe str))))

;*---------------------------------------------------------------------*/
;*    js-index->name ...                                               */
;*---------------------------------------------------------------------*/
(define (js-index->name::JsStringLiteralIndex num::uint32)
   (let ((str (fixnum->string (uint32->fixnum num))))
      (if (<=u32 num (jsindex12-max))
	  (let ((o (instantiate::JsStringLiteralIndex12
		      (weight (string-length str))
		      (left str)
		      (right #f))))
	     (js-object-mode-set! o
		(+u32 (js-jsstring-default-mode) (bit-lshu32 num 3)))
	     (js-jsstring-name-set! o o)
	     o)
	  (let ((o (instantiate::JsStringLiteralIndex32
		      (weight (string-length str))
		      (left str)
		      (right #f)
		      (index num))))
	     (js-object-mode-set! o (js-jsstring-default-mode))
	     (js-jsstring-name-set! o o)
	     o))))

;*---------------------------------------------------------------------*/
;*    js-integer->name ...                                             */
;*---------------------------------------------------------------------*/
(define (js-integer->name::JsStringLiteralASCII num::long)
   (let* ((str (fixnum->string num))
	  (o (instantiate::JsStringLiteralASCII
		(weight (string-length str))
		(left str)
		(right #f))))
      (js-object-mode-set! o (js-jsstring-default-mode))
      (js-jsstring-name-set! o o)
      o))

;*---------------------------------------------------------------------*/
;*    js-utf8-name->jsstring ...                                       */
;*---------------------------------------------------------------------*/
(define (js-utf8-name->jsstring str::bstring)
   (synchronize-name
      (let ((n (hashtable-get js-names str)))
	 (or n
	     (let ((o (instantiate::JsStringLiteralUTF8
			 (weight (fixnum->uint32 (string-length str)))
			 (left str)
			 (right #f))))
		(js-object-mode-set! o (js-jsstring-default-mode))
		(hashtable-put! js-names str o)
		(js-jsstring-name-set! o o)
		o)))))

;*---------------------------------------------------------------------*/
;*    js-integer-toname-unsafe ...                                     */
;*---------------------------------------------------------------------*/
(define (js-integer-toname-unsafe num::long)
   
   (define (number-name num)
      (if (>=fx num 0)
	  (js-index->name (fixnum->uint32 num))
	  (js-integer->name num)))
   
   (define (enlarge-vec! len)
      (let* ((nlen (minfx
		      (if (>fx (*fx 2 len) num) (*fx 2 len) (+fx 16 num))
		      (+fx (jsindex-threshold) 10)))
	     (nvec (copy-vector js-integer-names nlen)))
	 (vector-fill! nvec #f len)
	 ;; replace js-integer-names in the gc roots
	 (cond-expand
	    (enable-tls
	     (let ((l (memq js-integer-names gcroots)))
		(set-car! l nvec))))
	 (set! js-integer-names nvec)))

   (cond
      ((and (>fx num -10) (<fx num js-integer-length))
       (vector-ref js-integer-names (+fx num 10)))
      ((or (<=fx num -10) (>=fx num (jsindex-threshold)))
       (js-ascii-toname-unsafe (fixnum->string num)))
      (else
       (let ((len (vector-length js-integer-names)))
	  (when (<=fx len (+fx 10 num))
	     (enlarge-vec! len))
	  (when (=fx num js-integer-length)
	     (set! js-integer-length (+fx num 1)))
	  (or (vector-ref js-integer-names (+fx num 10))
	      (let ((name (number-name num)))
		 (vector-set! js-integer-names (+fx num 10) name)
		 name))))))

;*---------------------------------------------------------------------*/
;*    js-integer-name->jsstring ...                                    */
;*---------------------------------------------------------------------*/
(define (js-integer-name->jsstring num::long)
   (synchronize-name
      (js-integer-toname-unsafe num)))

;*---------------------------------------------------------------------*/
;*    js-integer-name ...                                              */
;*---------------------------------------------------------------------*/
(define (js-integer-name num::long)
   (synchronize-name
      (when (and (>fx num -10) (<fx num js-integer-length))
	 (vector-ref js-integer-names (+fx num 10)))))
   
