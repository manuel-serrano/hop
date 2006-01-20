;*=====================================================================*/
;*    serrano/prgm/project/hop/src/eval-macro.sch                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 14:50:28 2005                          */
;*    Last change :  Wed Jan 18 12:56:09 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Pervasive macros                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-pervasive-macro ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (define-pervasive-macro proto . body)
   `(begin
       (eval '(define-macro ,proto ,@body))
       (define-macro ,proto ,@body)))
 
;*---------------------------------------------------------------------*/
;*    define-eval-macro ...                                            */
;*---------------------------------------------------------------------*/
(define-macro (define-eval-macro proto . body)
   `(eval '(define-macro ,proto ,@body)))

