;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/arithmetic.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 13 08:20:08 2016                          */
;*    Last change :  Mon Jul  3 17:31:16 2017 (serrano)                */
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
;*    Fixnum addition on 32 bit machines (two tagging bit).            */
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
	       (pragma::real "DOUBLE_TO_REAL( (double)(CINT( $1 ) + CINT( $2) ))"
		  x y)
	       (pragma::bint "(obj_t)($1 - TAG_INT)" res))))
      (else
       `((@ js+fx32 __hopscript_number) ,x ,y))))

;*---------------------------------------------------------------------*/
;*    js-fx32 ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (js-fx32 x y)
   (cond-expand
      ((and bigloo-c (config have-overflow #t))
       `(let ((x::obj ,x)
	      (y::obj ,y)
	      (res::long 0))
	   (if (pragma::bool "__builtin_ssubl_overflow( (long)$1, (long)$2, &$3 )"
		  x y (pragma res))
	       (pragma::real "DOUBLE_TO_REAL( (double)(CINT( $1 ) - CINT( $2) ))"
		  x y)
	       (pragma::bint "(obj_t)($1 + TAG_INT)" res))))
      (else
       `((@ js-fx32 __hopscript_number) ,x ,y))))

;* (define-macro (js+fx32 x y)                                         */
;*    (cond-expand                                                     */
;*       (bigloo-c                                                     */
;*        (set! fx-count (+fx 1 fx-count))                             */
;*        (let ((lbl (format "__fx32_~a" fx-count)))                   */
;* 	  `(let* ((x::obj ,x)                                          */
;* 		  (y::obj ,y)                                          */
;* 		  (r::obj #unspecified)                                */
;* 		  (ov::bool #t)                                        */
;* 		  (s::long (pragma::long "(long)$1 + (long)$2" x y)))  */
;* 	      (pragma ,(format "asm goto ( \"jo %l0\" : : : : ~a )" lbl)) */
;* 	      (set! ov #f)                                             */
;* 	      (pragma::void ,(format "$1 = (obj_t)( $2 - TAG_INT ); goto ~a_ret;\n~a: $1 = DOUBLE_TO_REAL( (double)(CINT( $3 ) + CINT( $4) ))" lbl lbl) */
;* 		 (pragma r) s x y)                                     */
;* 	      (pragma ,(format "~a_ret:" lbl))                         */
;* 	      (when (and (not ov) (not (= r ((@ js+fx32 __hopscript_number) ,x ,y)))) */
;* 		 (tprint "PAS BON " ,lbl " ovf=" ov " r=" r " s=" s " x=" x " y=" y " -> " */
;* 		    ((@ js+fx32 __hopscript_number) ,x ,y)))           */
;* 	      r)))                                                     */
;*       (else                                                         */
;*        `((@ js+fx32 __hopscript_number) ,x ,y))))                   */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    js-fx32 ...                                                      *} */
;* {*---------------------------------------------------------------------*} */
;* (define-macro (js-fx32 x y)                                         */
;*    (cond-expand                                                     */
;*       (bigloo-c                                                     */
;*        (set! fx-count (+fx 1 fx-count))                             */
;*        (let ((lbl (format "__fx32_~a" fx-count)))                   */
;* 	  `(let* ((x::obj ,x)                                          */
;* 		  (y::obj ,y)                                          */
;* 		  (r::obj #unspecified)                                */
;* 		  (s::long (pragma::long "(long)$1 - (long)$2" x y)))  */
;* 	      (pragma ,(format "asm goto ( \"jo %l0\" : : : : ~a )" lbl)) */
;* 	      (pragma::void ,(format "$1 = (obj_t)( $2 + TAG_INT ); goto ~a_ret;\n~a: $1 = DOUBLE_TO_REAL( CINT( (obj_t)( $2 + TAG_INT )))" lbl lbl) */
;* 		 (pragma r) s)                                         */
;* 	      (pragma ,(format "~a_ret:" lbl))                         */
;* 	      r)))                                                     */
;*       (else                                                         */
;*        `((@ js-fx32 __hopscript_number) ,x ,y))))                   */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    js*fx32 ...                                                      *} */
;* {*---------------------------------------------------------------------*} */
;* (define-macro (js*fx32 x y)                                         */
;*    (cond-expand                                                     */
;*       (bigloo-c                                                     */
;*        (set! fx-count (+fx 1 fx-count))                             */
;*        (let ((lbl (format "__fx32_~a" fx-count)))                   */
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
;*        `((@ js*fx32 __hopscript_number) ,x ,y))))                   */

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
