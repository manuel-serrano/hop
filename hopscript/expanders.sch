;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/expanders.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 18:25:11 2006                          */
;*    Last change :  Sun Feb 28 09:51:57 2016 (serrano)                */
;*    Copyright   :  2006-16 Manuel Serrano                            */
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
   (eval `(define-expander js-get-name/cache
	     ,js-get-name/cache-expander))
   (eval `(define-expander js-object-get-name/cache
	     ,js-object-get-name/cache-expander))
   (eval `(define-expander js-global-object-get-name
	     ,js-global-object-get-name-expander))
   (eval `(define-expander js-global-object-get-name/cache
	     ,js-global-object-get-name/cache-expander)))
   
