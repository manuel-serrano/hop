;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arity.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 17 07:59:14 2020                          */
;*    Last change :  Sat Apr 18 06:36:10 2020 (serrano)                */
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
(define-macro (js-function-arity req opt . protocol)
   (if (and (integer? req) (integer? opt))
       (if (null? protocol)
	   (if (=fx opt 0)
	       (+ req 1)
	       (error "js-function-arity" "illegal optional for fix args" opt))
	   (match-case (car protocol)
	      (((kwote quote) arguments-lazy)
	       -2047)
	      (((kwote quote) arguments-eager)
	       -2047)
	      (((kwote quote) arguments-eager)
	       -2048)
	      (((kwote quote) arguments)
	       0)
	      (((kwote quote) rest-lazy)
	       (let ((offset (if (=fx opt 0) 2049 4049)))
		  (negfx (+fx offset (-fx req 1)))))
	      (((kwote quote) scheme)
	       (cond
		  ((=fx opt 0)
		   (+fx req 1))
		  ((=fx opt -1)
		   (negfx (+fx (+fx 1 req) 1)))
		  (else
		   (negfx (+fx (+fx 1 req) opt)))))
	      (((kwote quote) optional)
	       (if (=fx opt 0)
		   (+fx req 1)
		   (negfx (+fx req 1024))))
	      (((kwote quote) fix)
	       (if (=fx opt 0)
		   (+fx req 1)
		   (error "js-function-arity" "illegal optional for fix args" opt)))
	      (((kwote quote) ?-)
	       (error "js-function-arity" "illegal protocol" (car protocol)))
	      (else
	       `((@ js-function-arity __hopscript_function)
		 ,req ,opt ,@protocol))))))

;*---------------------------------------------------------------------*/
;*    js-function-arity ...                                            */
;*---------------------------------------------------------------------*/
(define (js-function-arity req opt #!optional (protocol 'fix))
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
	   (negfx (+fx (+fx 1 req) 1)))
	  (else
	   (negfx (+fx (+fx 1 req) opt)))))
      ((optional)
       (if (=fx opt 0)
	   (+fx req 1)
	   (negfx (+fx req 1024))))
      ((fix)
       (if (=fx opt 0)
	   (+fx req 1)
	   (error "js-function-arity" "illegal optional for fix args" opt)))
      (else
       (error "js-function-arity" "Unknown protocol" protocol))))

