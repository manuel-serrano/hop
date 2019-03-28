;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/constant.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Thu Mar 28 15:23:08 2019 (serrano)                */
;*    Copyright   :  2016-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript property expanders                                     */
;*    -------------------------------------------------------------    */
;*    See expanders.sch for the actual expander bindings.              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    directives                                                       */
;*---------------------------------------------------------------------*/
(directives (option (loadq "constant_expd.sch")))

;*---------------------------------------------------------------------*/
;*    %define-cnst-table ...                                           */
;*---------------------------------------------------------------------*/
(define-expander %define-cnst-table
   %define-cnst-table-expander)
(define-expander js-make-cnst-table
   js-make-cnst-table-expander)
(define-expander js-cnst-table
   js-cnst-table-expander)
(define-expander js-cnst-table-ref
   js-cnst-table-ref-expander)
