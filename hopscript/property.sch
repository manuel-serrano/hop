;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/property.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Thu Feb 18 09:29:07 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript property expanders                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    directives                                                       */
;*---------------------------------------------------------------------*/
(directives (option (loadq "property_expd.sch")))

;*---------------------------------------------------------------------*/
;*    %define-pcache ...                                               */
;*---------------------------------------------------------------------*/
(define-expander %define-pcache %define-pcache-expander)
(define-expander js-make-pcache js-make-pcache-expander)
(define-expander js-pcache-ref js-pcache-ref-expander)

;*---------------------------------------------------------------------*/
;*    js-get-XXX ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander js-get-name/cache js-get-name/cache-expander)
(define-expander js-object-get-name/cache js-object-get-name/cache-expander)
(define-expander js-global-object-get-name js-global-object-get-name-expander)
(define-expander js-global-object-get-name/cache js-global-object-get-name/cache-expander)

