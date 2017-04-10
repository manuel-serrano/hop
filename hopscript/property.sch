;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/property.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Sun Apr  2 18:42:58 2017 (serrano)                */
;*    Copyright   :  2016-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript property expanders                                     */
;*    -------------------------------------------------------------    */
;*    See expanders.sch for the actual expander bindings.              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    directives                                                       */
;*---------------------------------------------------------------------*/
(directives (option (loadq "property_expd.sch")))

;*---------------------------------------------------------------------*/
;*    %define-pcache ...                                               */
;*---------------------------------------------------------------------*/
(define-expander %define-pcache
   %define-pcache-expander)
(define-expander js-make-pcache
   js-make-pcache-expander)
(define-expander js-pcache-ref
   js-pcache-ref-expander)
(define-expander js-pcache-cmap
   js-pcache-cmap-expander)
(define-expander js-pcache-pmap
   js-pcache-pmap-expander)
(define-expander js-pcache-index
   js-pcache-index-expander)
(define-expander js-pcache-vindex
   js-pcache-vindex-expander)
(define-expander js-pcache-owner
   js-pcache-owner-expander)
(define-expander js-pcache-method
   js-pcache-method-expander)

;*---------------------------------------------------------------------*/
;*    js-get-XXX ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander js-get-name/cache
   js-get-name/cache-expander)
(define-expander js-object-get-name/cache
   js-object-get-name/cache-expander)
(define-expander js-object-get-name/cache-level1
   js-object-get-name/cache-level1-expander)
(define-expander js-object-get-name/cache-level2
   js-object-get-name/cache-level2-expander)
;* (define-expander js-this-get-name/cache                             */
;*    js-this-get-name/cache-expander)                                 */
(define-expander js-global-object-get-name
   js-global-object-get-name-expander)
(define-expander js-global-object-get-name/cache
   js-global-object-get-name/cache-expander)

;*---------------------------------------------------------------------*/
;*    js-put-XXX ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander js-put-name/cache!
   js-put-name/cache-expander)
(define-expander js-object-put-name/cache!
   js-object-put-name/cache-expander)
(define-expander js-object-put-name/cache-level1!
   js-object-put-name/cache-level1-expander)
(define-expander js-object-put-name/cache-level2!
   js-object-put-name/cache-level2-expander)

;* (define-expander js-this-put-name/cache!                            */
;*    js-this-put-name/cache-expander)                                 */

;*---------------------------------------------------------------------*/
;*    js-call-XXX ...                                                  */
;*---------------------------------------------------------------------*/
(define-expander js-call/cache
   js-call/cache-expander)
(define-expander js-method-call-name/cache
   js-method-call-name/cache-expander)
(define-expander js-object-method-call-name/cache
   js-object-method-call-name/cache-expander)
(define-expander js-object-method-call-name/cache-level2
   js-object-method-call-name/cache-level2-expander)
		    
