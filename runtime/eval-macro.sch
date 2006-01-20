;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/eval-macro.sch                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 13:15:43 2006                          */
;*    Last change :  Wed Jan 18 17:54:27 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Library macros                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-pervasive-macro ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (define-pervasive-macro proto . body)
   `(begin
       (eval '(define-macro ,proto ,@body))
       (define-macro ,proto ,@body)))

