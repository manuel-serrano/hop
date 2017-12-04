;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arithmetic64.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 19:36:39 2017                          */
;*    Last change :  Mon Dec  4 19:54:37 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Arithmetic operations on 64 bit platforms                        */
;*    -------------------------------------------------------------    */
;*    On 32bit platforms the JS types are mapped to SCM types          */
;*    as follows:                                                      */
;*      int29   -> int32                                               */
;*      uint29  -> uint32                                              */
;*      int32   -> int32                                               */
;*      uint32  -> uint32                                              */
;*      index   -> uint32                                              */
;*      length  -> uint32                                              */
;*      int53   -> bint                                                */
;*      integer ->                                                     */
;*      number  -> obj                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_arithmetic32

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
