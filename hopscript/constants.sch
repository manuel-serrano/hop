;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/constants.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 22 06:35:05 2014                          */
;*    Last change :  Mon Apr  8 14:29:08 2019 (serrano)                */
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
(define-expander &begin! &begin!-expander)
(define-expander &end! &end!-expander)
(define-expander & &-expander)

(define-expander &define-cnst &define-cnst-expander)	  
(define-expander &cnst-ref &cnst-ref-expander)	  
	
