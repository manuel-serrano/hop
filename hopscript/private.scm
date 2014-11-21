;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/private.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:10:39 2013                          */
;*    Last change :  Fri Nov 21 16:45:38 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Private (i.e., not exported by the lib) utilitary functions      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_private

   (library hop js2scheme)
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_error
	   __hopscript_public
	   __hopscript_property
	   __hopscript_worker)

   (export (->fixnum::long ::obj)
	   (->uint32::uint32 ::obj)
	   (->int32::int32 ::obj)
	   (->flonum::double ::obj)
	   
	   (js-cast-object ::JsGlobalObject obj ::bstring)
	   (expander js-toprimitive)

	   (trim-whitespaces+::bstring ::bstring #!key (left #t) (right #f) (plus #f))
	   
	   (=uint32 ::uint32 ::obj)
	   (<uint32 ::uint32 ::obj)
	   (<=uint32 ::uint32 ::obj)
	   (>=uint32 ::uint32 ::obj)
	   (>uint32 ::uint32 ::obj)
	   
	   (=int32 ::int32 ::obj)
	   (<int32 ::int32 ::obj)
	   (>=int32 ::int32 ::obj)
	   
	   (uint32->integer::obj ::uint32)
	   (int32->integer::obj ::int32)
	   
	   (inline u32vref ::vector ::uint32)
	   (inline u32vset! ::vector ::uint32 ::obj)
	   (inline u32vlen::uint32 ::vector)
	   
	   (js-freeze-property! desc::JsPropertyDescriptor)

	   (js-properties-clone ::pair-nil)
	   
	   (generic js-valueof ::obj ::JsGlobalObject)
	   
	   (js-string->number ::bstring)
	   (js-number->string obj)
	   (js-parseint ::bstring ::obj ::bool ::JsGlobalObject)
	   (js-parsefloat ::bstring ::bool ::JsGlobalObject)
	   
	   ))

;*---------------------------------------------------------------------*/
;*    ->fixnum ...                                                     */
;*---------------------------------------------------------------------*/
(define (->fixnum r)
   (cond
      ((fixnum? r) r)
      ((flonum? r) (flonum->fixnum r))
      ((elong? r) (elong->fixnum r))
      ((llong? r) (llong->fixnum r))
      (else (error "->fixnum" (format "Illegal number (~a)" (typeof r)) r))))

;*---------------------------------------------------------------------*/
;*    ->uint32 ...                                                     */
;*    -------------------------------------------------------------    */
;*    Assumes a positive number                                        */
;*---------------------------------------------------------------------*/
(define (->uint32 r)
   (cond
      ((fixnum? r) (fixnum->uint32 r))
      ((flonum? r) (flonum->uint32 r))
      (else (error "->uint32" (format "Illegal number (~a)" (typeof r)) r))))

;*---------------------------------------------------------------------*/
;*    ->int32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (->int32 r)
   (cond
      ((fixnum? r) (fixnum->int32 r))
      ((flonum? r) (flonum->int32 r))
      (else (error "->int32" (format "Illegal number (~a)" (typeof r)) r))))

;*---------------------------------------------------------------------*/
;*    ->flonum ...                                                     */
;*---------------------------------------------------------------------*/
(define (->flonum r)
   (cond
      ((flonum? r) r)
      ((fixnum? r) (fixnum->flonum r))
      ((elong? r) (elong->flonum r))
      ((llong? r) (llong->flonum r))
      (else (error "->flonum" (format "Illegal number (~a)" (typeof r)) r))))

;*---------------------------------------------------------------------*/
;*    js-cast-object/%this ...                                         */
;*---------------------------------------------------------------------*/
(define (js-cast-object %this::JsGlobalObject obj name)
   (cond
      ((isa? obj JsObject)
       obj)
      ((pair? obj)
       obj)
      (else
       (js-raise-type-error %this "cast: not an object \"~a\"" name))))

;*---------------------------------------------------------------------*/
;*    js-toprimitive ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.1          */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.8       */
;*---------------------------------------------------------------------*/
(define-expander js-toprimitive
   (lambda (x e)
      (let ((tmp (gensym)))
	 (match-case x
	    ((?- ?val 'number ?this)
	     `(let ((,tmp ,(e val e)))
		 (if ,(e `(number? ,tmp) e)
		     ,tmp
		     (js-toprimitive ,tmp 'number ,this))))
	    ((?- ?val 'string ?this)
	     `(let ((,tmp ,(e val e)))
		 (if ,(e `(string? ,tmp) e)
		     ,tmp
		     (js-toprimitive ,tmp 'string ,this))))
	    ((?- ?val 'any ?this)
	     `(let ((,tmp ,(e val e)))
		 (if ,(e `(number? ,tmp) e)
		     ,tmp
		     (js-toprimitive ,tmp 'any ,this))))
	    (else
	     (error "js-toprimitive" "illegal call" x))))))

;*---------------------------------------------------------------------*/
;*    js-freeze-property! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-freeze-property! desc::JsPropertyDescriptor)
   (with-access::JsPropertyDescriptor desc (name configurable)
      (when (isa? desc JsValueDescriptor)
	 (with-access::JsValueDescriptor desc (writable)
	    (when (eq? writable #t) (set! writable #f))))
      (when (eq? configurable #t) (set! configurable #f))))

;*---------------------------------------------------------------------*/
;*    js-properties-clone ...                                          */
;*---------------------------------------------------------------------*/
(define (js-properties-clone properties)
   (map (lambda (p)
	   (if (isa? p JsValueDescriptor)
	       (with-access::JsValueDescriptor p (writable)
		  (if writable
		      (duplicate::JsValueDescriptor p)
		      p))
	       p))
      properties))

;*---------------------------------------------------------------------*/
;*    js-valueof ::obj ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (js-valueof obj::obj %this::JsGlobalObject)
   (js-toobject %this obj))

;*---------------------------------------------------------------------*/
;*    js-string->number ...                                            */
;*---------------------------------------------------------------------*/
(define (js-string->number s)
   (let ((n (string->number s)))
      (if (bignum? n)
	  (bignum->flonum n)
	  n)))

;*---------------------------------------------------------------------*/
;*    js-number->string ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.8.1        */
;*---------------------------------------------------------------------*/
(define (js-number->string obj)

   (define (js-bignum->string m)
      (if (=bx m #z0)
	  "0"
	  (let loop ((p 0)
		     (s m))
	     (if (=bx (modulobx s #z10) #z0)
		 (loop (+ p 1) (/bx s #z10))
		 (let liip ((k 1)
			    (t #z10))
		    (if (>bx t s)
			(let ((n (+ p k)))
			   (cond
			      ((and (>= n k) (<= n 21))
			       ;; 6
			       (format "~a~a" (bignum->string s)
				  (make-string (- n k) #\0)))
			      ((and (< 0 n) (<= n 21))
			       ;; 7
			       (let ((s (bignum->string s)))
				  (format "~a.~a"
				     (substring s 0 n)
				     (substring s n))))
			      ((and (< -6 n) (<= n 0))
			       ;; 8
			       (format "0.~a~a"
				  (make-string (- n) #\0)
				  (substring (bignum->string s) n k)))
			      ((= k 1)
			       ;; 9
			       (format "~ae~a~a"
				  s
				  (if (>= n 1) "+" "-")
				  (number->string (abs (- n 1)))))
			      (else
			       ;; 10
			       (let ((s (bignum->string s)))
				  (format "~a.~ae~a~a"
				     (substring s 0 1)
				     (substring s 1)
				     (if (>= n 1) "+" "-")
				     (number->string (abs (- n 1))))))))
			(liip (+ k 1) (*bx t #z10))))))))

   (define (match->bignum::bignum m::pair)
      (let ((exp (string->integer (cadddr m)))) 
	 (+bx
	    (*bx (string->bignum (cadr m))
	       (exptbx #z10 (fixnum->bignum exp)))
	    (*bx (string->bignum (caddr m))
	       (exptbx #z10
		  (fixnum->bignum (-fx exp (string-length (caddr m)))))))))
   
   (define (js-real->string m)
      (if (=fl m 0.0)
	  "0"
	  (let ((s (real->string m)))
	     (cond
		((pregexp-match "^([-]?[0-9]+)[eE]([0-9]+)$" s)
		 =>
		 (lambda (m)
		    (js-bignum->string
		       (*bx (string->bignum (cadr m))
			  (exptbx #z10 (string->bignum (caddr m)))))))
		((pregexp-match "^([0-9]+).([0-9]+)[eE]([0-9]+)$" s)
		 =>
		 (lambda (m) (js-bignum->string (match->bignum m))))
		((pregexp-match "^-([0-9]+).([0-9]+)[eE]([0-9]+)$" s)
		 =>
		 (lambda (m)
		    (js-bignum->string (negbx (match->bignum m)))))
		((pregexp-match "^([-]?[.0-9]+)[.]0+$" s)
		 =>
		 cadr)
		(else
		 s)))))

   (cond
      ((not (= obj obj)) "NaN")
      ((= obj +inf.0) "Infinity")
      ((= obj -inf.0) "-Infinity")
      ((fixnum? obj) (integer->string obj))
      ((real? obj) (js-real->string obj))
      ((bignum? obj) (js-bignum->string obj))
      (else (number->string obj))))

;*---------------------------------------------------------------------*/
;*    js-parseint ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.2     */
;*---------------------------------------------------------------------*/
(define (js-parseint s::bstring radix strict-syntax::bool %this)
   
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

   (let ((r::int32 (js-toint32 radix %this))
	 (l (string-length s)))
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
	  (if (<=fx l 10)
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
(define (js-parsefloat s::bstring strict::bool %this)
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
;*    trim-whitespaces+ ...                                            */
;*---------------------------------------------------------------------*/
(define (trim-whitespaces+::bstring s::bstring #!key (left #t) (right #f) (plus #f))
   
   (define (byte-ref s i)
      (char->integer (string-ref-ur s i)))
   
   (define (trim-left s)
      (let ((len (string-length s)))
	 (let loop ((i 0))
	    (if (=fx i len)
		len
		(let ((c (byte-ref s i)))
		   (case c
		      ((#x09 #x0b #x0c #x20 #x0a #x0d)
		       ;; uni-character space and line break
		       (loop (+fx i 1)))
		      ((#x2b)
		       (if plus (loop (+fx i 1)) i))
		      ((#xc2)
		       ;; 2-characters line break
		       (if (and (<fx i (-fx len 2))
				(=fx (byte-ref s (+fx i 1)) #xa0))
			   (loop (+fx i 2))
			   i))
		      ((#xe1)
		       ;; 3-character whitespace
		       (cond
			  ((>=fx (+fx i 2) len)
			   i)
			  ((=fx (byte-ref s (+fx i 1)) #x9a)
			   (let ((c3 (byte-ref s (+fx i 2))))
			      (if (=fx c3 #x80)
				  (loop (+fx i 3))
				  i)))
			  ((=fx (byte-ref s (+fx i 1)) #xa0)
			   (let ((c3 (byte-ref s (+fx i 2))))
			      (if (=fx c3 #x8e)
				  (loop (+fx i 3))
				  i)))
			  (else
			   i)))
		      ((#xe2)
		       ;; 3-character line break
		       (cond
			  ((>=fx (+fx i 2) len)
			   i)
			  ((=fx (byte-ref s (+fx i 1)) #x80)
			   (let ((c3 (byte-ref s (+fx i 2))))
			      (cond
				 ((or (=fx c3 #xa8) (=fx c3 #xa9) (=fx c3 #xaf))
				  (loop (+fx i 3)))
				 ((and (>=fx c3 #x80) (<=fx c3 #x8a))
				  (loop (+fx i 3)))
				 (else
				  i))))
			  ((=fx (byte-ref s (+fx i 1)) #x81)
			   (if (=fx (byte-ref s (+fx i 2)) #x9f)
			       (loop (+fx i 3))
			       i))
			  (else
			   i)))
		      ((#xe3)
		       (cond
			  ((>=fx (+fx i 2) len)
			   i)
			  ((=fx (byte-ref s (+fx i 1)) #x80)
			   (if (=fx (byte-ref s (+fx i 2)) #x80)
			       (loop (+fx i 3))
			       i))
			  (else
			   i)))
		      ((#xef)
		       ;; bom
		       (if (and (<fx i (-fx len 3))
				(=fx (byte-ref s (+fx i 1)) #xbb)
				(=fx (byte-ref s (+fx i 2)) #xbf))
			   (loop (+fx i 3))
			   i))
		      (else
		       i)))))))
   
   (define (trim-right s stop)
      (let ((len (string-length s)))
	 (let loop ((i (-fx len 1)))
	    (if (<=fx i stop)
		i
		(let ((c (byte-ref s i)))
		   (case c
		      ((#x09 #x0b #x0c #x20 #x0a #x0d)
		       ;; uni-character space and line break
		       (loop (-fx i 1)))
		      ((#x81 #x82 #x83 #x84 #x85 #x86 #x87
			  #x88 #x89 #x8a #xa8 #xa9 #xaf)
		       ;; 3-characters line break
		       (if (<fx i 2)
			   i
			   (if (=fx (byte-ref s (-fx i 1)) #x80)
			       (if (=fx (byte-ref s (-fx i 2)) #xe2)
				   (loop (-fx i 3))
				   i)
			       i)))
		      ((#x80)
		       ;; 3-characters line break
		       (if (<fx i 2)
			   i
			   (cond
			      ((=fx (byte-ref s (-fx i 1)) #x80)
			       (if (or (=fx (byte-ref s (-fx i 2)) #xe2)
				       (=fx (byte-ref s (-fx i 2)) #xe3))
				   (loop (-fx i 3))
				   i))
			      ((=fx (byte-ref s (-fx i 1)) #x9a)
			       (if (=fx (byte-ref s (-fx i 2)) #xe1)
				   (loop (-fx i 3))
				   i))
			      (else
			       i))))
		      ((#x8e)
		       ;; 3-characters line break
		       (if (<fx i 2)
			   i
			   (cond
			      ((=fx (byte-ref s (-fx i 1)) #xa0)
			       (if (=fx (byte-ref s (-fx i 2)) #xe1)
				   (loop (-fx i 3))
				   i))
			      (else
			       i))))
		      ((#x9f)
		       ;; 3-characters line break
		       (if (<fx i 2)
			   i
			   (if (=fx (byte-ref s (-fx i 1)) #x81)
			       (if (=fx (byte-ref s (-fx i 2)) #xe2)
				   (loop (-fx i 3))
				   i)
			       i)))
		      ((#xa0)
		       ;; 2-characters line break
		       (if (<fx i 1)
			   i
			   (if (=fx (byte-ref s (-fx i 1)) #xc2)
			       (loop (-fx i 2))
			       i)))
		      ((#xbf)
		       ;; bom
		       (if (or (<fx i 3)
			       (not (and (=fx (byte-ref s (-fx i 1)) #xbb)
					 (=fx (byte-ref s (-fx i 2)) #xef))))
			   i
			   (loop (-fx i 3))))
		      (else
		       i)))))))
   
   (let* ((i (if left (trim-left s) 0))
	  (j (if right (trim-right s i) (-fx (string-length s) 1))))
      (substring s i (+fx j 1))))

;*---------------------------------------------------------------------*/
;*    =uint32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (=uint32 u::uint32 obj::obj)
   (cond
      ((uint32? obj)
       (=u32 u obj))
      ((negative? obj)
       #f)
      ((flonum? obj)
       (cond
	  ((nanfl? obj) #f)
	  ((or (=fl obj +inf.0) (=fl obj -inf.0)) #f)
	  (else (=fl (uint32->flonum u) obj))))
      (else (=u32 u (->uint32 obj)))))

;*---------------------------------------------------------------------*/
;*    <uint32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (<uint32 u::uint32 obj::obj)
   (cond
      ((uint32? obj) (<u32 u obj))
      ((negative? obj) #f)
      (else (<u32 u (->uint32 obj)))))

;*---------------------------------------------------------------------*/
;*    <=uint32 ...                                                     */
;*---------------------------------------------------------------------*/
(define (<=uint32 u::uint32 obj::obj)
   (cond
      ((uint32? obj)
       (<u32 u obj))
      ((negative? obj)
       #f)
      ((flonum? obj)
       (cond
	  ((=fl obj +inf.0) #t)
	  (else (<fl (uint32->flonum u) obj))))
      (else
       (<=u32 u (->uint32 obj)))))

;*---------------------------------------------------------------------*/
;*    >=uint32 ...                                                     */
;*---------------------------------------------------------------------*/
(define (>=uint32 u::uint32 obj::obj)
   (cond
      ((uint32? obj)
       (>=u32 u obj))
      ((negative? obj)
       #t)
      ((flonum? obj)
       (cond
	  ((=fl obj +inf.0) #f)
	  (else (>=fl (uint32->flonum u) obj))))
      (else
       (>=u32 u (->uint32 obj)))))

;*---------------------------------------------------------------------*/
;*    >uint32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (>uint32 u::uint32 obj::obj)
   (cond
      ((uint32? obj)
       (>=u32 u obj))
      ((negative? obj)
       #t)
      ((flonum? obj)
       (cond
	  ((=fl obj +inf.0) #f)
	  (else (>=fl (uint32->flonum u) obj))))
      (else
       (>=u32 u (->uint32 obj)))))

;*---------------------------------------------------------------------*/
;*    =int32 ...                                                       */
;*---------------------------------------------------------------------*/
(define (=int32 s::int32 obj::obj)
   (cond
      ((int32? obj)
       (=s32 s obj))
      ((negative? obj)
       #f)
      ((flonum? obj)
       (cond
	  ((nanfl? obj) #f)
	  ((or (=fl obj +inf.0) (=fl obj -inf.0)) #f)
	  (else (=s32 s (fixnum->int32 (flonum->fixnum obj))))))
      (else
       (=s32 s (->int32 obj)))))

;*---------------------------------------------------------------------*/
;*    <int32 ...                                                       */
;*---------------------------------------------------------------------*/
(define (<int32 s::int32 obj::obj)
   (cond
      ((int32? obj) (<s32 s obj))
      ((negative? obj) #f)
      (else (<s32 s (->int32 obj)))))

;*---------------------------------------------------------------------*/
;*    >=int32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (>=int32 s::int32 obj::obj)
   (cond
      ((int32? obj) (>=s32 s obj))
      ((negative? obj) #f)
      (else (>=s32 s (->int32 obj)))))

;*---------------------------------------------------------------------*/
;*    uint32->integer ...                                              */
;*---------------------------------------------------------------------*/
(define (uint32->integer u::uint32)
   (cond-expand
      (bint30
       (if (<u32 u (fixnum->uint32 (bit-lsh 1 29)))
	   (uint32->fixnum u)
	   (uint32->flonum u)))
      (else
       (uint32->fixnum u))))

;*---------------------------------------------------------------------*/
;*    int32->integer ...                                               */
;*---------------------------------------------------------------------*/
(define (int32->integer i::int32)
   (cond-expand
      (bint30
       (if (and (<s32 i (fixnum->int32 (bit-lsh 1 28)))
		(>=s32 i (fixnum->int32 (negfx (bit-lsh 1 28)))))
	   (int32->fixnum i)
	   (elong->flonum (uint32->elong i))))
      (bint32
       (if (and (<s32 i (fixnum->int32 (bit-lsh 1 30)))
		(>=s32 i (fixnum->int32 (negfx (bit-lsh 1 30)))))
	   (int32->fixnum i)
	   (elong->flonum (uint32->elong i))))
      (else
       (int32->fixnum i))))

;*---------------------------------------------------------------------*/
;*    u32vref ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (u32vref v::vector i::uint32)
   (vector-ref-ur v (uint32->fixnum i)))

;*---------------------------------------------------------------------*/
;*    u32vset! ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (u32vset! v::vector i::uint32 o)
   (vector-set-ur! v (uint32->fixnum i) o))

;*---------------------------------------------------------------------*/
;*    u32vlen ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (u32vlen v::vector)
   (fixnum->uint32 (vector-length v)))


