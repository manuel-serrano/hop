;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/constants.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 22 06:35:05 2014                          */
;*    Last change :  Fri Apr 12 11:44:07 2019 (serrano)                */
;*    Copyright   :  2014-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    constants Helper macros.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (option (loadq "constants_expd.sch")
           (loadq "names_expd.sch")))

;*---------------------------------------------------------------------*/
;*    & ...                                                            */
;*---------------------------------------------------------------------*/
(define-expander &with! &with!-expander)
(define-expander &begin! &begin!-expander)
(define-expander &end! &end!-expander)
(define-expander &init! &init!-expander)
(define-expander & &-expander)
	
