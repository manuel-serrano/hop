;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/hopscript/math.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 10:47:16 2013                          */
;*    Last change :  Sun Jan 26 16:06:12 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript Math                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_math

   (import __hopscript_types
	   __hopscript_object
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_function
	   __hopscript_error)

   (export js-math
	   (js-init-math! ::JsObject)))

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
   (let ((obj (instantiate::JsMath
		 (__proto__ js-object-prototype)
		 (properties
		    (map (lambda (nv)
			    (instantiate::JsValueDescriptor
			       (name (car nv))
			       (value (cdr nv))
			       (writable #f)
			       (configurable #f)
			       (enumerable #f)))
		       `((E . 2.7182818284590452354)
			 (LN10 . ,(log 10))
			 (LN2 . ,(log 2))
			 (LOG2E . 1.4426950408889634)
			 (LOG10E . ,0.4342944819032518)
			 (PI . ,(* 2 (atan 1 0)))
			 (SQRT1_2 . ,(sqrt (/ 1 2)))
			 (SQRT2 . ,(sqrt 2))))))))
      (js-bind! obj 'abs
	 :value (js-make-function js-math-abs 1 "abs")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'acos
	 :value (js-make-function js-math-acos 1 "acos")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'asin
	 :value (js-make-function js-math-asin 1 "asin")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'atan
	 :value (js-make-function js-math-atan 1 "atan")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'atan2
	 :value (js-make-function js-math-atan2 2 "atan2")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'ceil
	 :value (js-make-function js-math-ceil 1 "ceil")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'cos
	 :value (js-make-function js-math-cos 1 "cos")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'exp
	 :value (js-make-function js-math-exp 1 "exp")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'floor
	 :value (js-make-function js-math-floor 1 "floor")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'log
	 :value (js-make-function js-math-log 1 "log")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'max
	 :value (js-make-function js-math-max 2 "max")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'min
	 :value (js-make-function js-math-min 2 "min")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'pow
	 :value (js-make-function js-math-pow 1 "pow")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'random
	 :value (js-make-function js-math-random 1 "random")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'round
	 :value (js-make-function js-math-round 1 "round")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'sin
	 :value (js-make-function js-math-sin 1 "sin")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'sqrt
	 :value (js-make-function js-math-sqrt 1 "sqrt")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      (js-bind! obj 'tan
	 :value (js-make-function js-math-tan 1 "tan")
	 :writable #t
	 :configurable #t
	 :enumerable #f)
      ;; bind Math in the global object
      (set! js-math obj)
      (js-bind! %this 'Math :configurable #f :enumerable #f :value js-math)
      js-math))

;*---------------------------------------------------------------------*/
;*    js-math-abs ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.1     */
;*---------------------------------------------------------------------*/
(define (js-math-abs this x)
   (cond
      ((not (= x x)) x)
      ((< x 0) (- x))
      (else x)))

;*---------------------------------------------------------------------*/
;*    js-math-acos ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.2     */
;*---------------------------------------------------------------------*/
(define (js-math-acos this x)
   (acos x))

;*---------------------------------------------------------------------*/
;*    js-math-asin ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.3     */
;*---------------------------------------------------------------------*/
(define (js-math-asin this x)
   (asin x))

;*---------------------------------------------------------------------*/
;*    js-math-atan ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.4     */
;*---------------------------------------------------------------------*/
(define (js-math-atan this x)
   (atan x))

;*---------------------------------------------------------------------*/
;*    js-math-atan2 ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.5     */
;*---------------------------------------------------------------------*/
(define (js-math-atan2 this y x)
   (if (and (= x 0) (= y 0))
       (cond
	  ((and (flonum? y) (=fx (signbitfl y) 0))
	   (cond
	      ((and (flonum? x) (=fx (signbitfl x) 1))
	       (* 2 (atan 1 0)))
	      (else
	       0)))
	  ((and (flonum? y) (=fx (signbitfl y) 1))
	   (cond
	      ((and (flonum? x) (=fx (signbitfl x) 1))
	       (* -2 (atan 1 0)))
	      (else
	       0)))
	  (else
	   0))
       (atan y x)))

;*---------------------------------------------------------------------*/
;*    js-math-ceil ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.6     */
;*---------------------------------------------------------------------*/
(define (js-math-ceil this x)
   (cond
      ((not (flonum? x))
       x)
      ((nanfl? x)
       x)
      ((=fl x +inf.0)
       x)
      ((=fl x -inf.0)
       x)
      (else
       (inexact->exact (ceiling x)))))

;*---------------------------------------------------------------------*/
;*    js-math-cos ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.7     */
;*---------------------------------------------------------------------*/
(define (js-math-cos this x)
   (cos x))

;*---------------------------------------------------------------------*/
;*    js-math-exp ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.8     */
;*---------------------------------------------------------------------*/
(define (js-math-exp this x)
   (exp x))

;*---------------------------------------------------------------------*/
;*    js-math-floor ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.9     */
;*---------------------------------------------------------------------*/
(define (js-math-floor this x)
   (cond
      ((not (flonum? x)) x)
      ((nanfl? x) x)
      ((=fl x +inf.0) x)
      ((=fl x -inf.0) x)
      (else (inexact->exact (floor x)))))

;*---------------------------------------------------------------------*/
;*    ms-math-log ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.10    */
;*---------------------------------------------------------------------*/
(define (js-math-log this x)
   (log (js-tonumber x)))
   
;*---------------------------------------------------------------------*/
;*    js-math-max ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.11    */
;*---------------------------------------------------------------------*/
(define (js-math-max this . l)
   (let loop ((l l)
	      (r -inf.0))
      (if (null? l)
	  r
	  (let ((n (js-tonumber (car l))))
	     (if (not (= n n))
		 n
		 (loop (cdr l) (if (> n r) n r)))))))

;*---------------------------------------------------------------------*/
;*    js-math-min ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.12    */
;*---------------------------------------------------------------------*/
(define (js-math-min this . l)
   (let loop ((l l)
	      (r +inf.0))
      (if (null? l)
	  r
	  (let ((n (js-tonumber (car l))))
	     (if (not (= n n))
		 n
		 (loop (cdr l) (if (< n r) n r)))))))
   
;*---------------------------------------------------------------------*/
;*    js-math-pow ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.13    */
;*---------------------------------------------------------------------*/
(define (js-math-pow this x y)
   (let ((n1 (js-tonumber x))
	 (n2 (js-tonumber y)))
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
	 (else
	  (let loop ((x n1)
		     (y n2))
	     (cond
		((= y 0) 1)
		((even? y) (loop (* x x) (quotient y 2)))
		(else (* x (loop x (- y 1))))))))))
   
;*---------------------------------------------------------------------*/
;*    js-math-random ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.14    */
;*---------------------------------------------------------------------*/
(define (js-math-random this)
   (randomfl))
   
;*---------------------------------------------------------------------*/
;*    js-math-round ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.15    */
;*---------------------------------------------------------------------*/
(define (js-math-round this x)
   (cond
      ((not (flonum? x)) x)
      ((nanfl? x) x)
      ((=fl x +inf.0) x)
      ((=fl x -inf.0) x)
      (else (inexact->exact (floor (+ x 0.5))))))

;*---------------------------------------------------------------------*/
;*    js-math-sin ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.16    */
;*---------------------------------------------------------------------*/
(define (js-math-sin this x)
   (sin x))

;*---------------------------------------------------------------------*/
;*    js-math-sqrt ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.17    */
;*---------------------------------------------------------------------*/
(define (js-math-sqrt this x)
   (if (< x 0)
       +nan.0
       (sqrt x)))

;*---------------------------------------------------------------------*/
;*    js-math-tan ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.8.2.18    */
;*---------------------------------------------------------------------*/
(define (js-math-tan this x)
   (tan x))





   
