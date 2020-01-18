;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arithmetic.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 13 08:20:08 2016                          */
;*    Last change :  Sat Jan 18 07:28:56 2020 (serrano)                */
;*    Copyright   :  2016-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Arithmetic (overflow) expansion.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    +fx/overflow                                                     */
;*---------------------------------------------------------------------*/
(define-macro (+fx/overflow x y)
   (cond-expand
      ((or bint61 bint64)
       (cond
	  ((eqv? y 1)
	   `(if (=fx ,x 9007199254740992)
		9007199254740992
		(+fx ,x 1)))
	  ((eqv? x 1)
	   `(if (=fx ,y 9007199254740992)
		9007199254740992
		(+fx 1 ,y)))
	  ((eqv? y 2)
	   `(if (>=fx ,x 9007199254740991)
		(fixnum->flonum (+fx ,x 2))
		(+fx ,x 2)))
	  ((eqv? y 3)
	   `(if (>=fx ,x 9007199254740990)
		(fixnum->flonum (+fx ,x 3))
		(+fx ,x 3)))
	  ((eqv? y 4)
	   `(if (>=fx ,x 9007199254740989)
		(fixnum->flonum (+fx ,x 4))
		(+fx ,x 4)))
	  ((eqv? y -1)
	   `(-fx/overflow ,x 1))
	  ((eqv? y -2)
	   `(-fx/overflow ,x 2))
	  ((eqv? y -3)
	   `(-fx/overflow ,x 3))
	  ((eqv? y -4)
	   `(-fx/overflow ,x 4))
	  (else
	   `((@ +fx/overflow __hopscript_arithmetic64) ,x ,y))))
      (else
       `((@ +fx/overflow __hopscript_arithmetic32) ,x ,y))))

;*---------------------------------------------------------------------*/
;*    -fx/overflow                                                     */
;*---------------------------------------------------------------------*/
(define-macro (-fx/overflow x y)
   (cond-expand
      ((or bint61 bint64)
       (cond
	  ((eqv? y 1)
	   `(if (>fx ,x -9007199254740994)
		(-fx ,x 1)
		(fixnum->flonum (-fx ,x 1))))
	  ((eqv? y 2)
	   `(if (>fx ,x -9007199254740993)
		(-fx ,x 2)
		(fixnum->flonum (-fx ,x 2))))
	  ((eqv? y 3)
	   `(if (>=fx ,x -9007199254740992)
		(-fx ,x 3)
		(fixnum->flonum (-fx ,x 3))))
	  ((eqv? y 4)
	   `(if (>=fx ,x -9007199254740991)
		(-fx ,x 4)
		(fixnum->flonum (-fx ,x 4))))
	  (else
	   `((@ -fx/overflow __hopscript_arithmetic64) ,x ,y))))
      (else
       `((@ -fx/overflow __hopscript_arithmetic32) ,x ,y))))

;*---------------------------------------------------------------------*/
;*    js-int53-inc ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (js-int53-inc x)
   (cond-expand
      ((or bint61 bint64)
       (if (symbol? x)
	   `(if (>fx (bit-rsh ,x 53) 0) ,x (+fx ,x 1))
	   (let ((tmp (gensym)))
	      `(let ((,tmp ,x))
		  (js-int53-inc ,tmp)))))
      (else
       `((@ js-int53-inc __hopscript_arithmetic32) ,x))))

;*---------------------------------------------------------------------*/
;*    js-int53-dec ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (js-int53-dec x)
   (cond-expand
      ((or bint61 bint64)
       (if (symbol? x)
	   `(if (<fx (bit-rsh ,x 53) -1) ,x (-fx ,x 1))
	   (let ((tmp (gensym)))
	      `(let ((,tmp ,x))
		  (js-int53-dec ,tmp)))))
      (else
       `((@ js-int53-dec __hopscript_arithmetic32) ,x))))
