;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arity.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 17 07:59:14 2020                          */
;*    Last change :  Fri Apr 17 09:30:31 2020 (serrano)                */
;*    Copyright   :  2020 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    JavaScript function arity.                                       */
;*    -------------------------------------------------------------    */
;*    The function JS-FUNCTION-ARITY is used by both hopscript and     */
;*    js2scheme.                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-function-arity ...                                            */
;*---------------------------------------------------------------------*/
(define (js-function-arity req opt protocol)
   (case protocol
      ((arguments-lazy)
       -2047)
      ((arguments-eager)
       -2048)
      ((arguments)
       0)
      ((rest-lazy)
       (let ((offset (if (=fx opt 0) 2049 4049)))
	  (negfx (+fx offset (-fx req 1)))))
      ((rest)
       (let ((offset (if (=fx opt 0) 3049 5049)))
	  (negfx (+fx offset (-fx req 1)))))
      ((scheme)
       (cond
	  ((=fx opt 0)
	   (+fx req 1))
	  ((=fx opt -1)
	   (negfx (+fx 1 req)))
	  (else
	   (negfx (+fx (+fx 1 req) opt)))))
      ((optional)
       (if (=fx opt 0)
	   (+fx req 1)
	   (negfx (+fx req 1024))))
      (else
       (error "js-function-arity" "Unknown protocol" protocol))))

