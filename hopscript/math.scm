;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/math.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Sun Apr 29 07:42:46 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript Math                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_math
   
   (library hop)
   
   (include "types.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_function
	   __hopscript_error)
   
   (export (js-init-math! ::JsObject)
	   (js-math-ceil ::obj)
	   (js-math-sqrt ::obj ::JsGlobalObject)
	   (inline js-math-sqrtfl ::double)
	   (js-math-floor ::obj ::JsGlobalObject)
	   (js-math-floorfl ::double)
	   (js-math-round ::obj)))

;*---------------------------------------------------------------------*/
;*    js-math ...                                                      */
;*---------------------------------------------------------------------*/
(define js-math #f)

;*---------------------------------------------------------------------*/
;*    js-donate ::JsMath ...                                           */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsMath worker::WorkerHopThread %this)
   (js-undefined))
   
;*---------------------------------------------------------------------*/
;*    js-init-math! ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.1       */
;*---------------------------------------------------------------------*/
(define (js-init-math! %this)
   (with-access::JsGlobalObject %this (__proto__ js-math)
      ;; create the math object
      (set! js-math
	 (instantiateJsMath
	    (cmap (instantiate::JsConstructMap))
	    (__proto__ __proto__)))
      ;; other properties
      (js-bind! %this js-math 'E
	 :value 2.7182818284590452354
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math 'LN10
	 :value (log 10)
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math 'LN2
	 :value (log 2)
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math 'LOG2E
	 :value 1.4426950408889634
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math 'LOG10E
	 :value 0.4342944819032518
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math 'PI
	 :value (* 2 (atan 1 0))
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math 'SQRT1_2
	 :value (sqrt (/ 1 2))
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math 'SQRT2
	 :value (sqrt 2)
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      
      ;; abs
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.1
      (define (js-math-abs this x)
	 (cond
	    ((not (= x x)) x)
	    ((< x 0) (- x))
	    (else x)))
      
      (js-bind! %this js-math 'abs
	 :value (js-make-function %this js-math-abs 1 'abs)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; acos
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.2
      (define (js-math-acos this x)
	 (acos x))
      
      (js-bind! %this js-math 'acos
	 :value (js-make-function %this js-math-acos 1 'acos)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; asin
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.3
      (define (js-math-asin this x)
	 (asin x))
      
      (js-bind! %this js-math 'asin
	 :value (js-make-function %this js-math-asin 1 'asin)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; atan
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.4
      (define (js-math-atan this x)
	 (atan x))
      
      (js-bind! %this js-math 'atan
	 :value (js-make-function %this js-math-atan 1 'atan)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; atan2
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.5
      (define (js-math-atan2 this y x)
	 (if (and (= x 0) (= y 0))
	     (begin
		(cond
		   ((and (flonum? y) (=fx (signbitfl y) 0))
		    (cond
		       ((and (flonum? x) (>fx (signbitfl x) 0))
			(* 2 (atan 1 0)))
		       (else
			0)))
		   ((and (flonum? y) (>fx (signbitfl y) 0))
		    (cond
		       ((and (flonum? x) (>fx (signbitfl x) 0))
			(* -2 (atan 1 0)))
		       (else
			0)))
		   (else
		    0)))
	     (atan y x)))
      
      (js-bind! %this js-math 'atan2
	 :value (js-make-function %this js-math-atan2 2 'atan2)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      (js-bind! %this js-math 'ceil
	 :value (js-make-function %this
		   (lambda (this x) (js-math-ceil x)) 1 'ceil)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; cos
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.7
      (define (js-math-cos this x)
	 (cos x))
      
      (js-bind! %this js-math 'cos
	 :value (js-make-function %this js-math-cos 1 'cos)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; exp
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.8
      (define (js-math-exp this x)
	 (exp x))
      
      (js-bind! %this js-math 'exp
	 :value (js-make-function %this js-math-exp 1 'exp)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      (js-bind! %this js-math 'floor
	 :value (js-make-function %this
		   (lambda (this x) (js-math-floor x %this)) 1 'floor)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; log
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.10
      (define (js-math-log this x)
	 (log (js-tonumber x %this)))
      
      (js-bind! %this js-math 'log
	 :value (js-make-function %this js-math-log 1 'log)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; max
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.11
      (define (js-math-max this . l)
	 (let loop ((l l)
		    (r -inf.0))
	    (if (null? l)
		r
		(let ((n (js-tonumber (car l) %this)))
		   (if (not (= n n))
		       n
		       (loop (cdr l) (if (> n r) n r)))))))
      
      (js-bind! %this js-math 'max
	 :value (js-make-function %this js-math-max 2 'max)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; min
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.12
      (define (js-math-min this . l)
	 (let loop ((l l)
		    (r +inf.0))
	    (if (null? l)
		r
		(let ((n (js-tonumber (car l) %this)))
		   (if (not (= n n))
		       n
		       (loop (cdr l) (if (< n r) n r)))))))
      
      (js-bind! %this js-math 'min
	 :value (js-make-function %this js-math-min 2 'min)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; pow
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.13
      (define (js-math-pow this x y)
	 
	 (define (bignum->js-number o)
	    (if (bignum? o)
		(bignum->flonum o)
		o))
	 
	 (let ((n1 (js-tonumber x %this))
	       (n2 (js-tonumber y %this)))
	    (cond
	       ((flonum? n1)
		(expt n1 n2))
	       ((and (< n2 0) (exact? n2))
		(expt n1 (exact->inexact n2)))
	       ((and (flonum? n2) (or (= n2 +inf.0) (nanfl? n2)))
		+nan.0)
	       ((= n2 +inf.0)
		+nan.0)
	       ((= n2 -inf.0)
		(if (= (abs n1) 1)
		    +nan.0
		    0))
	       ((not (integer? y))
		(if (> y 0) (* x 0) (if (< x 0) -inf.0 +inf.0)))
	       (else
		(let loop ((x n1)
			   (y (inexact->exact n2)))
		   (cond
		      ((= y 0)
		       1)
		      ((even? y)
		       (bignum->js-number (loop (* x x) (quotient y 2))))
		      ((< y 0)
		       (bignum->js-number (* x (loop x (+ y 1)))))
		      (else
		       (bignum->js-number (* x (loop x (- y 1)))))))))))
      
      (js-bind! %this js-math 'pow
	 :value (js-make-function %this js-math-pow 1 'pow)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; random
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.14
      (define (js-math-random this)
	 (randomfl))
      
      (js-bind! %this js-math 'random
	 :value (js-make-function %this js-math-random 1 'random)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; round
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.15
      (js-bind! %this js-math 'round
	 :value (js-make-function %this
		   (lambda (this x) (js-math-round x)) 1 'round)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; sin
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.16
      (define (js-math-sin this x)
	 (sin x))
      
      (js-bind! %this js-math 'sin
	 :value (js-make-function %this js-math-sin 1 'sin)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; sqrt
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.17
      (define (js-math-sqrt this x)
	 (js-math-sqrt x %this))
      
      (js-bind! %this js-math 'sqrt
	 :value (js-make-function %this js-math-sqrt 1 'sqrt)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; ran
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.18
      (define (js-math-tan this x)
	 (tan x))
      
      (js-bind! %this js-math 'tan
	 :value (js-make-function %this js-math-tan 1 'tan)
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; bind Math in the global object
      (js-bind! %this %this 'Math
	 :configurable #f :enumerable #f :value js-math
	 :hidden-class #f)
      js-math))

;*---------------------------------------------------------------------*/
;*    j2s-math-ceil ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.6     */
;*---------------------------------------------------------------------*/
(define (js-math-ceil x)
   (cond
      ((not (flonum? x)) x)
      ((nanfl? x) x)
      ((=fl x +inf.0) x)
      ((=fl x -inf.0) x)
      (else (ceilingfl x))))

;*---------------------------------------------------------------------*/
;*    js-math-sqrt ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.17    */
;*---------------------------------------------------------------------*/
(define (js-math-sqrt x %this)
   (let loop ((x x))
      (cond
	 ((fixnum? x) (js-math-sqrtfl (fixnum->flonum x)))
	 ((not (flonum? x)) (loop (js-tonumber x %this)))
	 (else (js-math-sqrtfl x)))))

;*---------------------------------------------------------------------*/
;*    js-math-sqrtfl ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-math-sqrtfl x)
   (if (<fl x 0.0) +nan.0 (sqrtfl x)))

;*---------------------------------------------------------------------*/
;*    js-math-floor ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.9     */
;*---------------------------------------------------------------------*/
(define (js-math-floor x %this)
   (let loop ((x x))
      (cond
	 ((fixnum? x) x)
	 ((not (flonum? x)) (loop (js-tonumber x %this)))
	 (else (js-math-floorfl x)))))

;*---------------------------------------------------------------------*/
;*    js-math-floorfl ...                                              */
;*---------------------------------------------------------------------*/
(define (js-math-floorfl x::double)
   (cond
      ((nanfl? x) x)
      ((=fl x +inf.0) x)
      ((=fl x -inf.0) x)
      (else
       (cond-expand
	  ((or bint30 bint32)
	   (cond
	      ((>fl x (fixnum->flonum (bit-lsh 1 29)))
	       (floor x))
	      ((<fl x (- (fixnum->flonum (bit-lsh 1 29))))
	       (floor x))
	      (else
	       (flonum->fixnum (floor x)))))
	  (else
	   (flonum->fixnum (floor x)))))))
      
;*---------------------------------------------------------------------*/
;*    js-math-round ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.15    */
;*---------------------------------------------------------------------*/
(define (js-math-round x)
   (cond
      ((not (flonum? x)) x)
      ((nanfl? x) x)
      ((=fl x +inf.0) x)
      ((=fl x -inf.0) x)
      (else (inexact->exact (floor (+ x 0.5))))))
