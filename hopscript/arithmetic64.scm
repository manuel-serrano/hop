;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arithmetic64.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 19:36:39 2017                          */
;*    Last change :  Tue Dec  5 15:52:06 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Arithmetic operations on 64 bit platforms                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arithmetic64

   (library hop)

   (include "types.sch" "stringliteral.sch")

   (import __hopscript_types
	   __hopscript_object
	   __hopscript_function
	   __hopscript_string
	   __hopscript_error
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public)

   (cond-expand
      ((or bint61 bint64)
       (export
	  (inline overflow53::obj ::long)
	  (inline +fx/overflow::obj ::long ::long)
	  (+/overflow::obj ::obj ::obj)
	  (+js::obj ::obj ::obj ::JsGlobalObject)
	  (inline -fx/overflow::obj ::long ::long)
	  (-/overflow::obj ::obj ::obj)
	  (-js::obj ::obj ::obj ::JsGlobalObject)
	  (*fx/overflow ::long ::long)
	  (*/overflow ::obj ::obj)
	  (*js ::obj ::obj ::JsGlobalObject)))))

;*---------------------------------------------------------------------*/
;*    overflow53 ...                                                   */
;*    -------------------------------------------------------------    */
;*    2^53-1 overflow                                                  */
;*---------------------------------------------------------------------*/
(define-inline (overflow53 v::long)
   (if (=fx (bit-rsh v 53) 0) v (fixnum->flonum v)))

;*---------------------------------------------------------------------*/
;*    tolong ...                                                       */
;*---------------------------------------------------------------------*/
(define (tolong x)
   (cond
      ((fixnum? x) x)
      ((int32? x) (int32->fixnum x))
      ((uint32? x) (uint32->fixnum x))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    todouble ...                                                     */
;*---------------------------------------------------------------------*/
(define (todouble::double x)
   (cond
      ((fixnum? x) (fixnum->flonum x))
      ((int32? x) (int32->flonum x))
      ((uint32? x) (uint32->flonum x))
      (else x)))

;*---------------------------------------------------------------------*/
;*    +fx/overflow ...                                                 */
;*    -------------------------------------------------------------    */
;*    Fixnum addition on 64 bits machines (three tagging bits).        */
;*---------------------------------------------------------------------*/
(define-inline (+fx/overflow::obj x::long y::long)
   (overflow53 (+fx x y)))

;*---------------------------------------------------------------------*/
;*    +/overflow ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function is the compiler fallback used when it finds no     */
;*    specialized addition function. In practice it should hardly      */
;*    be called.                                                       */
;*---------------------------------------------------------------------*/
(define (+/overflow x::obj y::obj)
   (let ((ll (tolong x)))
      (if ll
	  (let ((rl (tolong y)))
	     (if rl
		 (+fx/overflow ll rl)
		 (+fl (todouble x) (todouble y))))
	  (+fl (todouble x) (todouble y)))))

;*---------------------------------------------------------------------*/
;*    +js ...                                                          */
;*---------------------------------------------------------------------*/
(define (+js x::obj y::obj %this)
   (+/overflow
      (if (number? x) x (js-tonumber y %this))
      (if (number? y) y (js-tonumber y %this))))
   
;*---------------------------------------------------------------------*/
;*    -fx/overflow ...                                                 */
;*    -------------------------------------------------------------    */
;*    see +fx/overflow                                                 */
;*---------------------------------------------------------------------*/
(define-inline (-fx/overflow::obj x::long y::long)
   (overflow53 (-fx x y)))

;*---------------------------------------------------------------------*/
;*    -/overflow ...                                                   */
;*    -------------------------------------------------------------    */
;*    see +/overflow                                                   */
;*---------------------------------------------------------------------*/
(define (-/overflow x y)
   (let ((ll (tolong x)))
      (if ll
	  (let ((rl (tolong y)))
	     (if rl
		 (-fx/overflow ll rl)
		 (-fl (todouble x) (todouble y))))
	  (-fl (todouble x) (todouble y)))))

;*---------------------------------------------------------------------*/
;*    -js ...                                                          */
;*---------------------------------------------------------------------*/
(define (-js x::obj y::obj %this)
   (-/overflow
      (if (number? x) x (js-tonumber y %this))
      (if (number? y) y (js-tonumber y %this))))
   
;*---------------------------------------------------------------------*/
;*    *fx/overflow ...                                                 */
;*---------------------------------------------------------------------*/
(define (*fx/overflow x::long y::long)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       (let ((res::long 0))
	  (if (pragma::bool "__builtin_smull_overflow((long)$1, (long)$2, &$3)"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL(((double)($1)) * ((double)($2)))"
		 x y)
	      (overflow53 res))))
      (else
       (define (neg? o)
	  (if (flonum? o)
	      (not (=fx (signbitfl o) 0))
	      (<fx o 0)))
       
       (let ((rnum (if (number? right) right (js-tonumber right %this)))
	     (r (* x y)))
	  (cond
	     ((fixnum? r)
	      (if (=fx r 0)
		  (if (or (and (neg? lnum) (not (neg? rnum)))
			  (and (not (neg? lnum)) (neg? rnum)))
		      -0.0)
		  (oveflow53 r)))
	     ((flonum? r)
	      r)
	     ((bignum? r)
	      (bignum->flonum r))
	     ((elong? r)
	      (elong->flonum r))
	     ((llong? r)
	      (llong->flonum r))
	     (else
	      r))))))

;*---------------------------------------------------------------------*/
;*    */overflow ...                                                   */
;*---------------------------------------------------------------------*/
(define (*/overflow x y)
   (let ((ll (tolong x)))
      (if ll
	  (let ((rl (tolong y)))
	     (if rl
		 (*fx/overflow ll rl)
		 (*fl (todouble x) (todouble y))))
	  (*fl (todouble x) (todouble y)))))

;*---------------------------------------------------------------------*/
;*    *js ...                                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.5.1       */
;*---------------------------------------------------------------------*/
(define (*js x::obj y::obj %this)
   (*/overflow
      (if (number? x) x (js-tonumber y %this))
      (if (number? y) y (js-tonumber y %this))))
