;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/private.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:10:39 2013                          */
;*    Last change :  Sun Mar 31 11:16:54 2019 (serrano)                */
;*    Copyright   :  2013-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Private (i.e., not exported by the lib) utilitary functions      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_private

   (library hop)
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_object
	   __hopscript_function
	   __hopscript_error
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_property
	   __hopscript_worker
	   __hopscript_json
	   __hopscript_stringliteral)

   (export (json-parser ::input-port ::JsGlobalObject)
	   (x-javascript-parser ::input-port ::JsGlobalObject)
	   
	   (->fixnum::long ::obj)
	   (->uint32::uint32 ::obj)
	   (->int32::int32 ::obj)
	   (->flonum::double ::obj)
	   
	   (expander js-toprimitive)

	   (trim-whitespaces+::bstring ::bstring #!key (left #t) (right #f) (plus #f))
	   
	   (=uint32 ::uint32 ::obj)
	   (<uint32 ::uint32 ::obj)
	   (<=uint32 ::uint32 ::obj)
	   (>=uint32 ::uint32 ::obj)
	   (>uint32 ::uint32 ::obj)
	   
	   (inline u32vref ::vector ::uint32)
	   (inline u32vset! ::vector ::uint32 ::obj)
	   (inline u32vlen::uint32 ::vector)
	   
	   (js-freeze-property! desc::JsPropertyDescriptor)

	   (js-properties-clone ::pair-nil)
	   
	   (generic js-valueof ::obj ::JsGlobalObject)
	   
	   (js-number->string obj)
	   (js-serializer ::JsObject)
	   (js-unserializer ::obj)

	   (js-get-hashnumber ::obj)
	   ))

;*---------------------------------------------------------------------*/
;*    json-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (json-parser ip ctx)
   (js-json-parser ip #f #f #f ctx))

;*---------------------------------------------------------------------*/
;*    x-javascript-parser ...                                          */
;*---------------------------------------------------------------------*/
(define (x-javascript-parser ip ctx)
   (read-char ip)
   (let ((o (js-json-parser ip #f #t #t ctx)))
      (read-char ip)
      o))
   
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
		 (if ,(e `(js-number? ,tmp) e)
		     ,tmp
		     (js-toprimitive ,tmp 'number ,this))))
	    ((?- ?val 'string ?this)
	     `(let ((,tmp ,(e val e)))
		 (if ,(e `(js-jsstring? ,tmp) e)
		     ,tmp
		     (js-toprimitive ,tmp 'string ,this))))
	    ((?- ?val 'any ?this)
	     `(let ((,tmp ,(e val e)))
		 (if ,(e `(js-number? ,tmp) e)
		     ,tmp
		     (if ,(e `(js-jsstring? ,tmp) e)
			 ,tmp
			 (js-toprimitive ,tmp 'any ,this)))))
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
      (if (and (=fx i 0) (=fx j (-fx (string-length s) 1)))
	  s
	  (substring s i (+fx j 1)))))

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
      (else
       (cond-expand
	  ((or bint30 bint32)
	   (=u32 u (->uint32 obj)))
	  (else
	   (=fx (uint32->fixnum u) obj))))))

;*---------------------------------------------------------------------*/
;*    <uint32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (<uint32 u::uint32 obj::obj)
   (cond
      ((uint32? obj)
       (<u32 u obj))
      ((negative? obj)
       #f)
      (else
       (cond-expand
	  ((or bint30 bint32)
	   (<u32 u (->uint32 obj)))
	  (else
	   (<fx (uint32->fixnum u) obj))))))

;*---------------------------------------------------------------------*/
;*    <=uint32 ...                                                     */
;*---------------------------------------------------------------------*/
(define (<=uint32 u::uint32 obj::obj)
   (cond
      ((uint32? obj)
       (<=u32 u obj))
      ((negative? obj)
       #f)
      ((flonum? obj)
       (cond
	  ((=fl obj +inf.0) #t)
	  (else (<=fl (uint32->flonum u) obj))))
      (else
       (cond-expand
	  ((or bint30 bint32)
	   (<=u32 u (->uint32 obj)))
	  (else
	   (<=fx (uint32->fixnum u) obj))))))

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
       (cond-expand
	  ((or bint30 bint32)
	   (>=u32 u (->uint32 obj)))
	  (else
	   (>=fx (uint32->fixnum u) obj))))))

;*---------------------------------------------------------------------*/
;*    >uint32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (>uint32 u::uint32 obj::obj)
   (cond
      ((uint32? obj)
       (>u32 u obj))
      ((negative? obj)
       #t)
      ((flonum? obj)
       (cond
	  ((=fl obj +inf.0) #f)
	  (else (>fl (uint32->flonum u) obj))))
      (else
       (cond-expand
	  ((or bint30 bint32)
	   (>u32 u (->uint32 obj)))
	  (else
	   (>fx (uint32->fixnum u) obj))))))

;*---------------------------------------------------------------------*/
;*    u32vref ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (u32vref v::vector i::uint32)
   (vector-ref v (uint32->fixnum i)))

;*---------------------------------------------------------------------*/
;*    u32vset! ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (u32vset! v::vector i::uint32 o)
   (vector-set! v (uint32->fixnum i) o))

;*---------------------------------------------------------------------*/
;*    u32vlen ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (u32vlen v::vector)
   (fixnum->uint32 (vector-length v)))


;*---------------------------------------------------------------------*/
;*    js-serializer ...                                                */
;*---------------------------------------------------------------------*/
(define (js-serializer o::JsObject)
   (call-with-output-string (lambda (op) (obj->javascript-expr o op))))

;*---------------------------------------------------------------------*/
;*    js-unserializer ...                                              */
;*---------------------------------------------------------------------*/
(define (js-unserializer s)
   s)

;*---------------------------------------------------------------------*/
;*    js-get-hashnumber ...                                            */
;*---------------------------------------------------------------------*/
(define (js-get-hashnumber key)
   (if (isa? key JsStringLiteral)
       (get-hashnumber (js-jsstring->string key))
       (get-hashnumber key)))

