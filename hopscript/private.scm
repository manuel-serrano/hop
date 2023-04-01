;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/private.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:10:39 2013                          */
;*    Last change :  Thu Feb 24 18:13:02 2022 (serrano)                */
;*    Copyright   :  2013-23 Manuel Serrano                            */
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

	   (=uint32 ::uint32 ::obj)
	   (<uint32 ::uint32 ::obj)
	   (<=uint32 ::uint32 ::obj)
	   (>=uint32 ::uint32 ::obj)
	   (>uint32 ::uint32 ::obj)
	   
	   (inline u32vref ::vector ::uint32)
	   (inline u32vset! ::vector ::uint32 ::obj)
	   (inline u32vlen::uint32 ::vector)
	   
	   (js-seal-property! ::JsPropertyDescriptor)
	   (js-freeze-property! ::JsPropertyDescriptor)

	   (js-number->string ::obj)
	   (js-serializer ::JsObject)
	   (js-unserializer ::obj)))

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
;*    js-seal-property! ...                                            */
;*---------------------------------------------------------------------*/
(define (js-seal-property! desc::JsPropertyDescriptor)
   (with-access::JsPropertyDescriptor desc (name configurable)
      (when (eq? configurable #t) (set! configurable #f))))

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
	  (let* ((s (real->string m))
		 (e (string-char-index-ur s #\e 0 (string-length s))))
	     (cond
		((not e)
		 (let ((d (string-index s #\.))
		       (l (string-length s)))
		    (cond
		       ((<fx d (-fx l 2))
			s)
		       ((=fx d (-fx l 2))
			(if (char=? (string-ref s (-fx l 1)) #\0)
			    (string-shrink! s (-fx l 2))
			    s))
		       (else
			(string-shrink! s (-fx l 1))))))
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
		(else
		 s)))))

   (cond
      ((fixnum? obj) (integer->string obj))
      ((not (=fl obj obj)) "NaN")
      ((=fl obj +inf.0) "Infinity")
      ((=fl obj -inf.0) "-Infinity")
      ((real? obj) (js-real->string obj))
      ((bignum? obj) (js-bignum->string obj))
      (else (number->string obj))))

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

