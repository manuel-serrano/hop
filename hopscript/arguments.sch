;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arguments.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec  7 06:56:07 2019                          */
;*    Last change :  Tue Jul 18 09:38:21 2023 (serrano)                */
;*    Copyright   :  2019-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Arguments macros for js2scheme                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-arguments-vector-index-ref ...                                */
;*---------------------------------------------------------------------*/
(define-macro (js-arguments-vector-index-ref vec idx len %arguments %this mode)
   `(cond
       (,%arguments
	(js-get ,%arguments ,idx ,%this))
       ((<fx ,idx ,len)
	(vector-ref ,vec ,idx))
       (else
	(set! ,%arguments 
	   ,(if (eq? mode 'strict)
		`(js-strict-arguments %this (vector-copy ,vec))
		`(js-sloppy-arguments %this (vector-copy ,vec))))
	(js-get ,%arguments ,idx ,%this))))

;*---------------------------------------------------------------------*/
;*    js-arguments-vector-ref ...                                      */
;*---------------------------------------------------------------------*/
(define-macro (js-arguments-vector-ref vec idx len %arguments %this mode)
   `(if (<fx ,idx 0)
	(js-get ,%arguments ,idx ,%this)
	(js-arguments-vector-index-ref ,vec ,idx ,len ,%arguments ,%this ,mode)))

;*---------------------------------------------------------------------*/
;*    js-rest-vector-ref ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (js-rest-vector-ref vec idx)
   `(if (and (>=fx ,idx 0) (<fx ,idx (vector-length ,vec)))
	(vector-ref ,vec ,idx)
	(js-undefined)))

;*---------------------------------------------------------------------*/
;*    js-rest-vector-index-ref ...                                     */
;*---------------------------------------------------------------------*/
(define-macro (js-rest-vector-index-ref vec idx)
   `(if (<u32 ,idx (fixnum->uint32 (vector-length ,vec)))
	(vector-ref ,vec (uint32->fixnum ,idx))
	(js-undefined)))

;*---------------------------------------------------------------------*/
;*    js-rest-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (js-rest-ref vec idx)
   (let ((n (gensym '%n)))
      `(if (fixnum? ,idx)
	   (js-ref-vector-ref ,vec ,idx)
	   (let ((,n (js-tonumber ,idx)))
	      (if (fixnum? ,n)
		  (js-ref-vector-ref ,vec ,n)
		  (js-undefined))))))
