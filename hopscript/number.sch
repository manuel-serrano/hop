;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/number.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb  5 19:07:35 2017                          */
;*    Last change :  Tue Feb 28 09:34:02 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Number macros for j2sscheme                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-bitand ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (js-bitand left right %this)
   (cond-expand
      ((or bint30 bint32)
       (if (and (or (symbol? left) (fixnum? left))
		(or (symbol? right) (fixnum? right)))
	   (cond
	      ((fixnum? left)
	       (if (fixnum? right)
		   (bit-and left right)
		   `(if (fixnum? ,right)
			(bit-and ,left ,right)
			((@ js-bitand __hopscript_number) ,left ,right ,%this))))
	      ((fixnum? right)
	       `(if (fixnum? ,left)
		    (bit-and ,left ,right)
		    ((@ js-bitand __hopscript_number) ,left ,right ,%this)))
	      (else
	       `(if (and (fixnum? ,left) (fixnum? ,right))
		    (bit-and ,left ,right)
		    ((@ js-bitand __hopscript_number) ,left ,right ,%this))))
	   `((@ js-bitand __hopscript_number) ,left ,right ,%this)))
      (else
       `((@ js-bitand __hopscript_number) ,left ,right ,%this))))

;*---------------------------------------------------------------------*/
;*    js-bitor ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (js-bitor left right %this)
   (cond-expand
      ((or bint30 bint32)
       (if (and (or (symbol? left) (fixnum? left))
		(or (symbol? right) (fixnum? right)))
	   (cond
	      ((fixnum? left)
	       (if (fixnum? right)
		   (bit-or left right)
		   `(if (fixnum? ,right)
			(bit-or ,left ,right)
			((@ js-bitor __hopscript_number) ,left ,right ,%this))))
	      ((fixnum? right)
	       `(if (fixnum? ,left)
		    (bit-or ,left ,right)
		    ((@ js-bitor __hopscript_number) ,left ,right ,%this)))
	      (else
	       `(if (and (fixnum? ,left) (fixnum? ,right))
		    (bit-or ,left ,right)
		    ((@ js-bitor __hopscript_number) ,left ,right ,%this))))
	   `((@ js-bitor __hopscript_number) ,left ,right ,%this)))
      (else
       `((@ js-bitor __hopscript_number) ,left ,right ,%this))))

;*---------------------------------------------------------------------*/
;*    js-bit-xor ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (js-bit-xor left right %this)
   (cond-expand
      ((or bint30 bint32)
       (if (and (or (symbol? left) (fixnum? left))
		(or (symbol? right) (fixnum? right)))
	   (cond
	      ((fixnum? left)
	       (if (fixnum? right)
		   (bit-xor left right)
		   `(if (fixnum? ,right)
			(bit-xor ,left ,right)
			((@ js-bit-xor __hopscript_number) ,left ,right ,%this))))
	      ((fixnum? right)
	       `(if (fixnum? ,left)
		    (bit-xor ,left ,right)
		    ((@ js-bit-xor __hopscript_number) ,left ,right ,%this)))
	      (else
	       `(if (and (fixnum? ,left) (fixnum? ,right))
		    (bit-xor ,left ,right)
		    ((@ js-bit-xor __hopscript_number) ,left ,right ,%this))))
	   `((@ js-bit-xor __hopscript_number) ,left ,right ,%this)))
      (else
       `((@ js-bit-xor __hopscript_number) ,left ,right ,%this))))

;*---------------------------------------------------------------------*/
;*    js-bitnot ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (js-bitnot left %this)
   (cond-expand
      ((or bint30 bint32)
       (cond
	  ((fixnum? left)
	   (bit-not left))
	  ((symbol? left)
	   `(if (fixnum? ,left)
		(bit-not ,left)
		((@ js-bitnot __hopscript_number) ,left ,%this)))
	  (else
	   `((@ js-bitnot __hopscript_number) ,left ,%this))))
      (else
       `((@ js-bitnot __hopscript_number) ,left ,%this))))
	    
