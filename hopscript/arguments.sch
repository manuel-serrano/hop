;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/arguments.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec  7 06:56:07 2019                          */
;*    Last change :  Sat Dec  7 07:13:45 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Arguments macros for js2scheme                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-arguments-vector-ref ...                                      */
;*---------------------------------------------------------------------*/
(define-macro (js-arguments-vector-ref vec arguments idx %this)
   `(if (and (>=fx ,idx 0) (<fx ,idx (vector-length ,vec)))
	(vector-ref-ur ,vec ,idx)
	(begin
	   (set! ,arguments (js-materialize-arguments ,%this ,vec ,arguments))
	   (js-arguments-ref ,arguments ,idx ,%this))))

;*---------------------------------------------------------------------*/
;*    js-arguments-vector-index-ref ...                                */
;*---------------------------------------------------------------------*/
(define-macro (js-arguments-vector-index-ref vec arguments idx %this)
   `(if (<u32 ,idx (fixnum->uint32 (vector-length ,vec)))
	(vector-ref-ur ,vec (uint32->fixnum ,idx))
	(begin
	   (set! ,arguments (js-materialize-arguments ,%this ,vec ,arguments))
	   (js-arguments-index-ref ,arguments ,idx ,%this))))
