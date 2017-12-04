;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arithmetic32.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  4 19:36:39 2017                          */
;*    Last change :  Mon Dec  4 19:54:18 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Arithmetic operations on 32 bit platforms                        */
;*    -------------------------------------------------------------    */
;*    On 32bit platforms the JS types are mapped to SCM types          */
;*    as follows:                                                      */
;*      int29   -> int32                                               */
;*      uint29  -> uint32                                              */
;*      int32   -> int32                                               */
;*      uint32  -> uint32                                              */
;*      index   -> uint32                                              */
;*      length  -> uint32                                              */
;*      int53   -> obj                                                 */
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
      ((or bint30 bint32)
       (export (+int53/32::obj ::obj ::obj)))))

;*---------------------------------------------------------------------*/
;*    +s32/overflow ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (+s32/overflow::obj x::int32 y::int32)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       (let ((res::long 0))
	  (cond
	     ((pragma::bool "__builtin_saddl_overflow( (long)$1, (long)$2, &$3 )"
		 x y (pragma res))
	      (pragma::real "DOUBLE_TO_REAL( ((double)($1) + ((double)($2))) )"
		 x y))
	     ((=s32 res (bit-ands32 res (bit-lshs32 #s32:3 30)))
	      (pragma::real "DOUBLE_TO_REAL( ((double)($1) + ((double)($2))) )"
		 x y))
	     (else
	      (pragma::bint "(obj_t)($1 - TAG_INT)" res)))))
      (else
       (let ((z::long (pragma::long "(~((long)$1 ^ (long)$2)) & 0x80000000" x y)))
	  (if (pragma::bool "$1 & (~((((long)$2 ^ (long)$1) + ((long)$3)) ^ ((long) $3)))" z x y)
	      (+fl (int32->flonum x) (int32->flonum y))
	      (+s32 x y))))))

;*---------------------------------------------------------------------*/
;*    +int53/32 ...                                                    */
;*---------------------------------------------------------------------*/
(define (+int53/32 x y)
   
   (define (toint32 x)
      (cond
	 ((fixnum? x) (fixnum->int32 x))
	 ((int32? x) x)
	 ((and (uint32? x) (<u32 x (bit-lshu32 1 30))) (uint32->fixnum x))
	 (else #f)))
   
   (define (tofl::double x)
      (cond
	 ((fixnum? x) (fixnum->flonum x))
	 ((int32? x) (int32->flonum x))
	 ((uint32? x) (uint32->flonum x))
	 (else x)))
   
   (let ((l32 (toint32 left)))
      (if l32
	  (let ((r32 (toint32 right)))
	     (if r32
		 (+s32/overflor l32 r32)
		 (+fl (tofl left) (tofl right))))
	  (+fl (tofl left) (tofl right))))
   
   (+fx (tofx x) (tofx y)))   
