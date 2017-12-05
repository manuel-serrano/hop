;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arithmetic64.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 19:36:39 2017                          */
;*    Last change :  Tue Dec  5 05:54:41 2017 (serrano)                */
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
       (export (+int53/64::bint ::obj ::obj)))))

;*---------------------------------------------------------------------*/
;*    +int53/64 ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function is the compiler fallback used when it finds no     */
;*    specialized addition function. In practice it should hardly      */
;*    be called.                                                       */
;*---------------------------------------------------------------------*/
(define (+int53/64 x y)
   
   (define (tofx x)
      (cond
	 ((fixnum? x) x)
	 ((int32? x) (int32->fixnum x))
	 ((uint32? x) (uint32->fixnum x))
	 ((flonum? x) (flonum->fixnum x))
	 (else (error "+int53/64" "wrong integer type" x))))
       
   (+fx (tofx x) (tofx y)))
