;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/math.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Fri Jun  5 05:52:17 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
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
   
   (include "types.sch" "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_object
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_function
	   __hopscript_error
	   __hopscript_stringliteral)
   
   (export (js-init-math! ::JsObject)
	   (js-math-ceil ::obj)
	   (js-math-ceil-as-integer ::obj)
	   (js-math-sqrt::double ::obj ::JsGlobalObject)
	   (inline js-math-sqrtfl::double ::double)
	   (js-math-floor ::obj ::JsGlobalObject)
	   (js-math-floorfl ::double)
	   (js-math-abs ::obj ::JsGlobalObject)
	   (inline js-math-absfl::double ::double)
	   (js-math-round ::obj)
	   (js-math-roundfl ::double)
	   (js-math-atan2fl::double ::double ::double)
	   (js-math-atan2::double ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-math ...                                                      */
;*---------------------------------------------------------------------*/
(define js-math #f)

;*---------------------------------------------------------------------*/
;*    js-init-math! ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.1       */
;*---------------------------------------------------------------------*/
(define (js-init-math! %this)
   (with-access::JsGlobalObject %this (js-math)
      ;; local constant strings
      (unless (vector? __js_strings) (set! __js_strings (&init!)))
      ;; create the math object
      (set! js-math
	 (instantiateJsObject
	    (cmap (instantiate::JsConstructMap))
	    (__proto__ (js-object-proto %this))
	    (elements ($create-vector 26))))
      ;; other properties
      (js-bind! %this js-math (& "E")
	 :value 2.7182818284590452354
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #t)
      (js-bind! %this js-math (& "LN10")
	 :value (log 10)
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math (& "LN2")
	 :value (log 2)
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math (& "LOG2E")
	 :value 1.4426950408889634
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math (& "LOG10E")
	 :value 0.4342944819032518
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math (& "PI")
	 :value (* 2 (atan 1 0))
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math (& "SQRT1_2")
	 :value (sqrt (/ 1 2))
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      (js-bind! %this js-math (& "SQRT2")
	 :value (sqrt 2)
	 :writable #f
	 :configurable #f
	 :enumerable #f
	 :hidden-class #f)
      
      ;; abs
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.1
      (js-bind! %this js-math (& "abs")
	 :value (js-make-function %this
		   (lambda (this x) (js-math-abs x %this))
		   (js-function-arity 1 0)
		   (js-function-info :name "abs" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; acos
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.2
      (define (js-math-acos this x)
	 (acos x))
      
      (js-bind! %this js-math (& "acos")
	 :value (js-make-function %this js-math-acos
		   (js-function-arity js-math-acos)
		   (js-function-info :name "acos" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; asin
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.3
      (define (js-math-asin this x)
	 (asin x))
      
      (js-bind! %this js-math (& "asin")
	 :value (js-make-function %this js-math-asin
		   (js-function-arity js-math-asin)
		   (js-function-info :name "asin" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; atan
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.4
      (define (js-math-atan this x)
	 (atan x))
      
      (js-bind! %this js-math (& "atan")
	 :value (js-make-function %this js-math-atan
		   (js-function-arity js-math-atan)
		   (js-function-info :name "atan" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; atan2
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.5
      (define (js-math-atan2 this y x)
	 (js-math-atan2 this y x))
      
      (js-bind! %this js-math (& "atan2")
	 :value (js-make-function %this js-math-atan2
		   (js-function-arity js-math-atan2)
		   (js-function-info :name "atan2" :len 2))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      (js-bind! %this js-math (& "ceil")
	 :value (js-make-function %this
		   (lambda (this x) (js-math-ceil x))
		   (js-function-arity 1 0)
		   (js-function-info :name "ceil" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; cos
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.7
      (define (js-math-cos this x)
	 (cos x))
      
      (js-bind! %this js-math (& "cos")
	 :value (js-make-function %this js-math-cos
		   (js-function-arity js-math-cos)
		   (js-function-info :name "cos" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; exp
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.8
      (define (js-math-exp this x)
	 (exp x))
      
      (js-bind! %this js-math (& "exp")
	 :value (js-make-function %this js-math-exp
		   (js-function-arity js-math-exp)
		   (js-function-info :name "exp" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)

      ;; floor
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.9
      (js-bind! %this js-math (& "floor")
	 :value (js-make-function %this
		   (lambda (this x) (js-math-floor x %this))
		   (js-function-arity 1 0)
		   (js-function-info :name "floor" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; log
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.10
      (define (js-math-log this x)
	 (log (js-tonumber x %this)))
      
      (js-bind! %this js-math (& "log")
	 :value (js-make-function %this js-math-log
		   (js-function-arity js-math-log)
		   (js-function-info :name "log" :len 1))
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
      
      (js-bind! %this js-math (& "max")
	 :value (js-make-function %this js-math-max
		   (js-function-arity js-math-max)
		   (js-function-info :name "max" :len 2))
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
      
      (js-bind! %this js-math (& "min")
	 :value (js-make-function %this js-math-min
		   (js-function-arity js-math-min)
		   (js-function-info :name "min" :len 2))
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
	       ((and (flonum? n2) (or (=fl n2 +inf.0) (nanfl? n2)))
		+nan.0)
	       ((= n2 -inf.0)
		(if (= (abs n1) 1)
		    +nan.0
		    0))
	       ((flonum? n2)
		(exptfl (fixnum->flonum n1) n2))
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
      
      (js-bind! %this js-math (& "pow")
	 :value (js-make-function %this js-math-pow
		   (js-function-arity js-math-pow)
		   (js-function-info :name "pow" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; random
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.14
      (define (js-math-random this)
	 (randomfl))
      
      (js-bind! %this js-math (& "random")
	 :value (js-make-function %this js-math-random
		   (js-function-arity js-math-random)
		   (js-function-info :name "random" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; round
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.15
      (js-bind! %this js-math (& "round")
	 :value (js-make-function %this
		   (lambda (this x) (js-math-round x))
		   (js-function-arity 1 0)
		   (js-function-info :name "round" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; sin
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.16
      (define (js-math-sin this x)
	 (sin x))
      
      (js-bind! %this js-math (& "sin")
	 :value (js-make-function %this js-math-sin
		   (js-function-arity js-math-sin)
		   (js-function-info :name "sin" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; sqrt
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.17
      (define (js-math-prototype-sqrt this x)
	 (js-math-sqrt x %this))
      
      (js-bind! %this js-math (& "sqrt")
	 :value (js-make-function %this
		   (lambda (this x) (js-math-sqrt x %this))
		   (js-function-arity 1 0)
		   (js-function-info :name "sqrt" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; ran
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.18
      (define (js-math-tan this x)
	 (tan x))
      
      (js-bind! %this js-math (& "tan")
	 :value (js-make-function %this js-math-tan
		   (js-function-arity js-math-tan)
		   (js-function-info :name "tan" :len 1))
	 :writable #t
	 :configurable #t
	 :enumerable #f
	 :hidden-class #f)
      
      ;; bind Math in the global object
      (js-bind! %this %this (& "Math")
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
;*    j2s-math-ceil ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.6     */
;*---------------------------------------------------------------------*/
(define (js-math-ceil-as-integer x)
   (cond
      ((not (flonum? x)) x)
      ((nanfl? x) x)
      ((=fl x +inf.0) x)
      ((=fl x -inf.0) x)
      (else (flonum->fixnum (ceilingfl x)))))

;*---------------------------------------------------------------------*/
;*    js-math-sqrt ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.17    */
;*---------------------------------------------------------------------*/
(define (js-math-sqrt x %this)
   (let loop ((x x))
      (cond
	 ((flonum? x) (js-math-sqrtfl x))
	 ((fixnum? x) (js-math-sqrtfl (fixnum->flonum x)))
	 (else (loop (js-tonumber x %this))))))

;*---------------------------------------------------------------------*/
;*    js-math-sqrtfl ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-math-sqrtfl x)
   (sqrtfl-ur x))

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
	       (floorfl x))
	      ((<fl x (- (fixnum->flonum (bit-lsh 1 29))))
	       (floorfl x))
	      (else
	       (flonum->fixnum (floorfl x)))))
	  (else
	   (flonum->fixnum (floorfl x)))))))
      
;*---------------------------------------------------------------------*/
;*    js-math-abs ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.1     */
;*---------------------------------------------------------------------*/
(define (js-math-abs x %this)
   (cond
      ((not (number? x)) (js-math-abs (js-tonumber x %this) %this))
      ((not (= x x)) x)
      ((< x 0) (- x))
      (else x)))

;*---------------------------------------------------------------------*/
;*    js-math-absfl ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.1     */
;*---------------------------------------------------------------------*/
(define-inline (js-math-absfl x)
   (absfl x))

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
      (else
       (cond-expand
	  ((or bint61 bint63)
	   (flonum->fixnum (floorfl (+fl x 0.5))))
	  (else
	   (inexact->exact (floorfl (+fl x 0.5))))))))

;*---------------------------------------------------------------------*/
;*    js-math-roundfl ...                                              */
;*---------------------------------------------------------------------*/
(define (js-math-roundfl x)
   (cond
      ((nanfl? x) x)
      ((=fl x +inf.0) x)
      ((=fl x -inf.0) x)
      (else
       (cond-expand
	  ((or bint61 bint63)
	   (flonum->fixnum (floorfl (+fl x 0.5))))
	  (else
	   (inexact->exact (floorfl (+fl x 0.5))))))))

;*---------------------------------------------------------------------*/
;*    js-math-atan2fl ...                                              */
;*---------------------------------------------------------------------*/
(define (js-math-atan2fl y x)
   (if (and (=fl x 0.0) (=fl y 0.0))
       (cond
	  ((=fx (signbitfl y) 0)
	   (if (>fx (signbitfl x) 0)
	       (*fl 2. (atanfl 1. 0.))
	       0.))
	  ((>fx (signbitfl y) 0)
	   (if (>fx (signbitfl x) 0)
	       (*fl -2. (atan 1. 0.))
	       0.0))
	  (else
	   0.0))
       (atan-2fl-ur y x)))

;*---------------------------------------------------------------------*/
;*    js-math-atan2 ...                                                */
;*---------------------------------------------------------------------*/
(define (js-math-atan2 y x)
   (if (and (= x 0) (= y 0))
       (cond
	  ((and (flonum? y) (=fx (signbitfl y) 0))
	   (cond
	      ((and (flonum? x) (>fx (signbitfl x) 0))
	       (*fl 2. (atanfl 1. 0.)))
	      (else
	       0.)))
	  ((and (flonum? y) (>fx (signbitfl y) 0))
	   (cond
	      ((and (flonum? x) (>fx (signbitfl x) 0))
	       (*fl -2. (atan 1. 0.)))
	      (else
	       0.0)))
	  (else
	   0.0))
       (atan-2fl-ur y x)))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
