;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arity.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 17 07:59:14 2020                          */
;*    Last change :  Sat Apr 29 09:04:56 2023 (serrano)                */
;*    Copyright   :  2020-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript function arity.                                       */
;*    -------------------------------------------------------------    */
;*    The function JS-FUNCTION-ARITY is used by both hopscript and     */
;*    js2scheme.                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-function-arity ...                                            */
;*---------------------------------------------------------------------*/
(define (js-function-arity req #!optional opt (protocol 'fix))
   (if (procedure? req)
       (procedure-arity req)
       (case protocol
	  ((arguments-lazy)
	   -2047)
	  ((arguments-eager)
	   -2048)
	  ((arguments-lonly)
	   (-fx -8192 req))
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
	       (negfx (+fx (+fx 1 req) 1)))
	      (else
	       (negfx (+fx (+fx 1 req) opt)))))
	  ((scheme-optional)
	   (if (and (=fx opt 1) (=fx req 0))
	       -512
	       (error "js-function-arity" "Illegal scheme-optional" (cons opt req))))
	  ((optional)
	   (if (=fx opt 0)
	       (+fx req 1)
	       (negfx (+fx req 1024))))
	  ((fix)
	   (unless (number? opt)
	      (tprint "req=" req " opt=" opt))
	   (if (=fx opt 0)
	       (+fx req 1)
	       (error "js-function-arity" "illegal optional for fix args" opt)))
	  (else
	   (error "js-function-arity" "Unknown protocol" protocol)))))

