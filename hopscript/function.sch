;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/function.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec  7 06:32:41 2019                          */
;*    Last change :  Sat Dec  7 19:41:58 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Function macros for js2scheme                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-function-apply-arguments ...                                  */
;*---------------------------------------------------------------------*/
(define-macro (js-function-apply-arguments %this this thisarg vec arguments cache)
   `(if (and (or (not (js-object? ,this)) (js-object-mode-plain? ,this))
	     (not (object? ,arguments)))
	(js-apply-array ,%this ,this ,thisarg ,vec)
	(begin
	   (set! ,arguments (js-materialize-arguments ,%this ,vec ,arguments))
	   (js-function-apply %this ,this ,thisarg ,arguments ,cache))))
	   
;*---------------------------------------------------------------------*/
;*    js-function-maybe-apply-arguments ...                            */
;*---------------------------------------------------------------------*/
(define-macro (js-function-maybe-apply-arguments %this this thisarg vec arguments cache)
   `(if (js-function? ,this)
	(js-function-apply-arguments ,%this ,this ,thisarg ,vec ,arguments ,cache)
	(begin
	   (set! ,arguments (js-materialize-arguments ,%this ,vec ,arguments))
	   (js-function-maybe-apply %this ,this ,thisarg ,arguments ,cache))))
   
