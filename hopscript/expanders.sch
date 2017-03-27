;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/expanders.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 18:25:11 2006                          */
;*    Last change :  Mon Mar 27 08:15:24 2017 (serrano)                */
;*    Copyright   :  2006-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript expanders installer                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    hopscript-install-expanders! ...                                 */
;*---------------------------------------------------------------------*/
(define (hopscript-install-expanders!)
   
   (eval `(define-expander %define-pcache
	     ,%define-pcache-expander))
   (eval `(define-expander js-make-pcache
	     ,js-make-pcache-expander))
   (eval `(define-expander js-pcache-ref
	     ,js-pcache-ref-expander))
   (eval `(define-expander js-pcache-cmap
	     ,js-pcache-cmap-expander))
   (eval `(define-expander js-pcache-pmap
	     ,js-pcache-pmap-expander))
   (eval `(define-expander js-pcache-index
	     ,js-pcache-index-expander))
   (eval `(define-expander js-pcache-vindex
	     ,js-pcache-vindex-expander))
   (eval `(define-expander js-pcache-owner
	     ,js-pcache-owner-expander))
   (eval `(define-expander js-pcache-method
	     ,js-pcache-method-expander))
   (eval `(define-expander js-get-name/cache
	     ,js-get-name/cache-expander))
   
   (eval `(define-expander js-object-get-name/cache
	     ,js-object-get-name/cache-expander))
   (eval `(define-expander js-object-get-name/cache-level2
	     ,js-object-get-name/cache-level2-expander))
   (eval `(define-expander js-global-object-get-name
	     ,js-global-object-get-name-expander))
   (eval `(define-expander js-global-object-get-name/cache
	     ,js-global-object-get-name/cache-expander))
   
   (eval `(define-expander js-put-name/cache!
	     ,js-put-name/cache-expander))
   (eval `(define-expander js-object-put-name/cache!
	     ,js-object-put-name/cache-expander))
   (eval `(define-expander js-object-put-name/cache-level2!
	     ,js-object-put-name/cache-level2-expander))
   
   (eval `(define-expander js-method-call-name/cache
	     ,js-method-call-name/cache-expander))
   (eval `(define-expander js-object-method-call-name/cache
	     ,js-object-method-call-name/cache-expander))
   (eval `(define-expander js-object-method-call-name/cache-level2
	     ,js-object-method-call-name/cache-level2-expander))
   (eval `(define-expander js-call/cache
	     ,js-call/cache-expander)))
   
