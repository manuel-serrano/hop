;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/hopscript/private.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:10:39 2013                          */
;*    Last change :  Fri Feb 14 10:54:50 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Private (i.e., not exported by the lib) utilitary functions      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_private

   (include "hopscript.sch")
   
   (library js2scheme)
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_error
	   __hopscript_public
	   __hopscript_property)

   (export (js-raise-type-error ::bstring ::obj)
	   (js-raise-type-error/loc ::obj ::bstring ::obj)

	   (->fixnum::long ::obj)
	   
	   (inline js-cast-object obj ::bstring)
	   (expander js-toprimitive)
	   (generic js-toprimitive ::obj ::symbol)

	   (trim-whitespaces+ ::bstring #!key (left #t) (right #f) (plus #f))
	   
	   

	   
	   
	   (js-freeze-property! desc::JsPropertyDescriptor)

	   (js-construct-get-prototype ::JsFunction)

	   (js-properties-clone ::pair-nil)
	   
	   (js-toindex ::obj)
	   
	   (generic js-valueof ::obj)
	   
	   
	   (generic js-tointeger ::obj)
	   
	   
	   (js-number->string obj)
	   (js-parseint ::bstring ::obj ::bool)
	   (js-parsefloat ::bstring ::bool)

	   (%js-eval ::bstring)
	   
	   
	   
	   ))

;*---------------------------------------------------------------------*/
;*    js-raise-type-error ...                                          */
;*---------------------------------------------------------------------*/
(define (js-raise-type-error fmt::bstring obj)
   (js-raise
      (js-new js-type-error
	 (format fmt
	    (if (isa? obj JsObject)
		(with-handler
		   (lambda (e)
		      (js-typeof obj))
		   (js-call0 (js-get obj 'toString) obj))
		obj)))))

;*---------------------------------------------------------------------*/
;*    js-raise-type-error/loc ...                                      */
;*---------------------------------------------------------------------*/
(define (js-raise-type-error/loc loc fmt::bstring obj)
   (match-case loc
      ((at ?fname ?loc)
       (js-raise
	  (js-new js-type-error
	     (format fmt
		(if (isa? obj JsObject)
		    (with-handler
		       (lambda (e)
			  (js-typeof obj))
		       (js-call0 (js-get obj 'toString) obj))
		    obj))
	     fname
	     loc)))
      (else
       (js-raise-type-error fmt obj))))

;*---------------------------------------------------------------------*/
;*    ->fixnum ...                                                     */
;*---------------------------------------------------------------------*/
(define (->fixnum r)
   (cond
      ((fixnum? r) r)
      ((elong? r) (elong->fixnum r))
      ((llong? r) (llong->fixnum r))
      ((bignum? r) (bignum->fixnum r))
      (else (flonum->fixnum r))))

;*---------------------------------------------------------------------*/
;*    js-cast-object ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-cast-object obj name)
   (if (isa? obj JsObject)
       obj
       (js-raise-type-error "cast: not an object ~a" name)))

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
	    ((?- ?val 'number)
	     `(let ((,tmp ,(e val e)))
		 (if ,(e `(number? ,tmp) e)
		     ,tmp
		     (js-toprimitive ,tmp 'number))))
	    ((?- ?val 'string)
	     `(let ((,tmp ,(e val e)))
		 (if ,(e `(string? ,tmp) e)
		     ,tmp
		     (js-toprimitive ,tmp 'string))))
	    ((?- ?val 'any)
	     `(let ((,tmp ,(e val e)))
		 (if ,(e `(number? ,tmp) e)
		     ,tmp
		     (js-toprimitive ,tmp 'any))))
	    (else
	     (error "js-toprimitive" "illegal call" x))))))

;*---------------------------------------------------------------------*/
;*    js-toprimitive ::obj ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.1          */
;*---------------------------------------------------------------------*/
(define-generic (js-toprimitive obj preferredtype)
   obj)

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
;*    js-construct-get-prototype ...                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-13.2.2       */
;*---------------------------------------------------------------------*/
(define (js-construct-get-prototype constructor::JsFunction)
   (let ((prototype (js-get constructor 'prototype)))
      (if (eq? prototype (js-undefined))
	  js-object-prototype
	  prototype)))

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
;*    js-toindex ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4         */
;*---------------------------------------------------------------------*/
(define (js-toindex p)
   (cond
      ((integer? p)
       (when (and (>= p 0) (< p 4294967295)) p))
      ((isa? p JsNumber)
       (with-access::JsNumber p (val) (js-toindex val)))
      ((string? p)
       (js-toindex (string->number p)))
      ((symbol? p)
       (js-toindex (string->number (symbol->string! p))))
      ((isa? p JsString)
       (with-access::JsString p (val) (js-toindex (string->number val))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    js-valueof ::obj ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (js-valueof obj::obj)
   (js-toobject obj))

;*---------------------------------------------------------------------*/
;*    js-tointeger ::obj ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.4          */
;*---------------------------------------------------------------------*/
(define-generic (js-tointeger obj)
   (cond
      ((fixnum? obj)
       obj)
      ((integer? obj)
       (if (flonum? obj)
	   (if (or (=fl obj +inf.0) (=fl obj -inf.0))
	       obj
	       (inexact->exact obj))
	   obj))
      ((flonum? obj)
       (cond
	  ((=fl obj +inf.0) obj)
	  ((=fl obj -inf.0) obj)
	  ((nanfl? obj) 0)
	  ((<fl obj 0.) (flonum->llong (*fl -1. (floor (abs obj)))))
	  (else (flonum->llong (floor obj)))))
      ((or (string? obj) (symbol? obj))
       (let ((v (js-tonumber obj)))
	  (if (inexact? v)
	      (cond
		 ((nanfl? v) 0)
		 ((or (=fl v +inf.0) (=fl v -inf.0)) v)
		 (else (inexact->exact v)))
	      v)))
      ((eq? obj #t)
       1)
      (else 0)))

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

   (define (js-real->string m)
      (if (=fl m 0.0)
	  "0"
	  (let* ((s (real->string m))
		 (m (pregexp-match "([-]?[0-9]+)[eE]([0-9]+)" s)))
	     (if (pair? m)
		 (js-bignum->string
		    (*bx (string->bignum (cadr m))
		       (exptbx #z10 (string->bignum (caddr m)))))
		 (let ((m (pregexp-match "([0-9]+)[.]0+$" s)))
		    (if (pair? m)
			(cadr m)
			s))))))

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
(define (js-parseint s::bstring radix strict-syntax::bool)
   
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
      (+ n 0))

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
   
   (let ((r (js-toint32 radix))
	 (l (string-length s)))
      (cond
	 ((not (integer? r))
	  +nan.0)
	 ((and (not (= r 0)) (or (< r 2) (> r 36)))
	  +nan.0)
	 ((and (or (= r 0) (= r 16))
	       (>=fx (string-length s) 2)
	       (char=? (string-ref s 0) #\0)
	       (or (char=? (string-ref s 1) #\x)
		   (char=? (string-ref s 1) #\X)))
	  (let ((s (substring s 2)))
	     (if (<=fx l 9)
		 (integer (str->integer s 16) s 16)
		 (integer (string->bignum-safe s 16) s 16))))
	 ((= r 0)
	  (if (<=fx l 10)
	      (integer (str->integer s 10) s 10)
	      (integer (string->bignum-safe s 10) s 10)))
	 (else
	  (let ((r (->fixnum r)))
	     (if (<=fx l (if (<=fx r 10) 8 (if (<=fx r 16) 7 5)))
		 (integer (str->integer s r) s 10)
		 (integer (string->bignum-safe s r) s r)))))))

;*---------------------------------------------------------------------*/
;*    js-parsefloat ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.3     */
;*---------------------------------------------------------------------*/
(define (js-parsefloat s::bstring strict::bool)
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
(define (trim-whitespaces+ s::bstring #!key (left #t) (right #f) (plus #f))
   
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
;*    lib-hopscript-path ...                                           */
;*---------------------------------------------------------------------*/
(define lib-hopscript-path
   (make-file-path (hop-lib-dir) "hop" (hop-version)))

;*---------------------------------------------------------------------*/
;*    %js-eval ...                                                     */
;*---------------------------------------------------------------------*/
(define (%js-eval string)
   (library-load 'hopscript lib-hopscript-path)
   (let ((r (js-undefined)))
      (with-trace 1 '%js-eval
	 (trace-item "string=" string)
	 (call-with-input-string string
	    (lambda (in)
	       (for-each (lambda (e)
			    (with-trace 2 '%js-eval-inner
			       (trace-item "e=" e)
			       (set! r (eval e))
			       (trace-item "r=" r)))
		  (with-handler
		     (lambda (e)
			(exception-notify e)
			(cond
			   ((isa? e &io-parse-error)
			    (with-access::&io-parse-error e (proc msg obj
							       fname location)
			       (js-raise
				  (js-new js-syntax-error
				     (format "~a: ~a -- ~a" proc msg obj)
				     fname location))))
			   ((isa? e &io-error)
			    (with-access::&io-error e (proc msg obj
							 fname location)
			       (js-raise
				  (js-new js-error
				     (format "~a: ~a -- ~a" proc msg obj)
				     fname location))))
			   ((isa? e &error)
			    (with-access::&error e (proc msg obj
						      fname location)
			       (cond
				  ((string=? proc "assignment")
				   (js-raise
				      (js-new js-reference-error
					 (format "~a -- ~a" msg obj)
					 fname location)))
				  (else
				   (js-raise
				      (js-new js-error
					 (format "~a: ~a -- ~a" proc msg obj)
					 fname location))))))
			   (else
			    (raise e))))
		     (j2s-compile in :driver (j2s-eval-driver))))
	       (trace-item "r=" r)
	       r)))))

