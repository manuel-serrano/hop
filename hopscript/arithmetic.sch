;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/arithmetic.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 13 08:20:08 2016                          */
;*    Last change :  Sun Nov 26 06:35:21 2017 (serrano)                */
;*    Copyright   :  2016-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Arithmetic (overflow) expansion.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-fx32-init! ...                                                */
;*---------------------------------------------------------------------*/
(define-expander js-fx-init!
   (lambda (x e)
      (eval '(define fx-count 0))
      #unspecified))

(js-fx-init!)

;*---------------------------------------------------------------------*/
;*    js+fx32 ...                                                      */
;*    -------------------------------------------------------------    */
;*    Overrides the portable definition of __hopscript_number          */
;*---------------------------------------------------------------------*/
(define-macro (js+fx32 x y)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       `(let ((x::obj ,x)
	      (y::obj ,y)
	      (res::long 0))
	   (if (pragma::bool "__builtin_saddl_overflow( (long)$1, (long)$2, &$3 )"
		  x y (pragma res))
	       (pragma::real "DOUBLE_TO_REAL( ((double)(CINT( $1 )) + CINT( $2) ))"
		  x y)
	       (pragma::bint "(obj_t)($1 - TAG_INT)" res))))
      (else
       `((@ js+fx32 __hopscript_number) ,x ,y))))

;*---------------------------------------------------------------------*/
;*    js-fx32 ...                                                      */
;*    -------------------------------------------------------------    */
;*    Overrides the portable definition of __hopscript_number          */
;*---------------------------------------------------------------------*/
(define-macro (js-fx32 x y)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       `(let ((x::obj ,x)
	      (y::obj ,y)
	      (res::long 0))
	   (if (pragma::bool "__builtin_ssubl_overflow( (long)$1, (long)$2, &$3 )"
		  x y (pragma res))
	       (pragma::real "DOUBLE_TO_REAL( ((double)(CINT( $1 )) - CINT( $2) ))"
		  x y)
	       (pragma::bint "(obj_t)($1 + TAG_INT)" res))))
      (else
       `((@ js-fx32 __hopscript_number) ,x ,y))))

;*---------------------------------------------------------------------*/
;*    js*fx32 ...                                                      */
;*    -------------------------------------------------------------    */
;*    Overrides the portable definition of __hopscript_number          */
;*---------------------------------------------------------------------*/
(define-macro (js*fx32 x y)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       `(let ((x::obj ,x)
	      (y::obj ,y)
	      (res::long 0))
	   (if (pragma::bool "__builtin_smull_overflow( (long)$1 - TAG_INT, CINT( $2 ), &$3 )"
		  x y (pragma res))
	       (pragma::real "DOUBLE_TO_REAL( ((double)(CINT( $1 )) * CINT( $2 ) ))"
		  x y)
	       (pragma::bint "(obj_t)($1 + TAG_INT)" res))))
      (else
       `((@ js*fx32 __hopscript_number) ,x ,y))))

