;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/arithmetic.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 13 08:20:08 2016                          */
;*    Last change :  Wed Dec 21 07:31:06 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
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
;*    Fixnum addition on 32 bits machines (two tagging bits).          */
;*    -------------------------------------------------------------    */
;*    Overrides the portable definition of __hopscript_number          */
;*---------------------------------------------------------------------*/
(define-macro (js+fx32 x y)
   (cond-expand
      (bigloo-c
       (set! fx-count (+fx 1 fx-count))
       (let ((lbl (format "__fx32_~a" fx-count)))
	  `(let* ((x::obj ,x)
		  (y::obj ,y)
		  (s::long (pragma::long "(long)$1 + (long)$2" x y))
		  (r::obj #unspecified))
	      (pragma ,(format "asm goto ( \"jo %l0\" : : : : ~a )" lbl))
	      (pragma::void ,(format "$1 = (obj_t)( $2 - TAG_INT ); goto ~a_ret; ~a: $1 = DOUBLE_TO_REAL( CINT( (obj_t)( $2 - TAG_INT )))" lbl lbl)
		 (pragma r) s)
	      (pragma ,(format "~a_ret:" lbl))
	      r)))
      (else
       `((@ js+fx32 __hopscript_number) ,x ,y))))

;*---------------------------------------------------------------------*/
;*    js-fx32 ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (js-fx32 x y)
   (cond-expand
      (bigloo-c
       (set! fx-count (+fx 1 fx-count))
       (let ((lbl (format "__fx32_~a" fx-count)))
	  `(let* ((x::obj ,x)
		  (y::obj ,y)
		  (s::long (pragma::long "(long)$1 - (long)$2" x y))
		  (r::obj #unspecified))
	      (pragma ,(format "asm goto ( \"jo %l0\" : : : : ~a )" lbl))
	      (pragma::void ,(format "$1 = (obj_t)( $2 + TAG_INT ); goto ~a_ret; ~a: $1 = DOUBLE_TO_REAL( CINT( (obj_t)( $2 + TAG_INT )))" lbl lbl)
		 (pragma r) s)
	      (pragma ,(format "~a_ret:" lbl))
	      r)))
      (else
       `((@ js-fx32 __hopscript_number) ,x ,y))))

;*---------------------------------------------------------------------*/
;*    js*fx32 ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (js*fx32 x y)
   (cond-expand
      (bigloo-c
       (set! fx-count (+fx 1 fx-count))
       (let ((lbl (format "__fx32_~a" fx-count)))
	  `(let* ((xl::long ,x)
		  (y::obj ,y)
		  (yl::long (pragma::long "(long)$1 - TAG_INT" y))
		  (tmp::obj (pragma::obj "(obj_t)((long)$1 * (long)$2)" xl yl)))
	      (pragma::void ,(format "asm goto ( \"jo %l0\" : : : : ~a )" lbl))
	      (pragma::obj "$1 = (obj_t)((long)$1 + TAG_INT)" (pragma tmp))
	      (pragma::void ,(format "goto ~a_ret" lbl))
	      (pragma::void ,(format "~a: $1 = DOUBLE_TO_REAL( (long)($1) >> 2 )" lbl) tmp)
	      (pragma ,(format "~a_ret:" lbl))
	      tmp)))
      (else
       `((@ js*fx32 __hopscript_number) ,x ,y))))

;* (define-macro (js*fx64 x y)                                         */
;*    (cond-expand                                                     */
;*       (bigloo-c                                                     */
;*        (set! fx-count (+fx 1 fx-count))                             */
;*        (let ((lbl (format "__fx64_~a" fx-count)))                   */
;* 	  `(let* ((xl::long ,x)                                        */
;* 		  (y::obj ,y)                                          */
;* 		  (yl::long (pragma::long "(long)$1 - TAG_INT" y))     */
;* 		  (tmp::obj (pragma::obj "(obj_t)((long)$1 * (long)$2)" xl yl))) */
;* 	      (pragma::void ,(format "asm goto ( \"jo %l0\" : : : : ~a )" lbl)) */
;* 	      (pragma::obj "$1 = (obj_t)((long)$1 + TAG_INT)" (pragma tmp)) */
;* 	      (pragma::void ,(format "goto ~a_ret" lbl))               */
;* 	      (pragma::void ,(format "~a: $1 = DOUBLE_TO_REAL( (long)($1) >> 2 )" lbl) tmp) */
;* 	      (pragma ,(format "~a_ret:" lbl))                         */
;* 	      tmp)))                                                   */
;*       (else                                                         */
;*        `((@ js*fx64 __hopscript_number) ,x ,y))))                   */
