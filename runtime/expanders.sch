;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/expanders.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 18:25:11 2006                          */
;*    Last change :  Wed Apr  1 16:09:20 2009 (serrano)                */
;*    Copyright   :  2006-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop expanders installer                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    hop-install-expanders! ...                                       */
;*---------------------------------------------------------------------*/
(define (hop-install-expanders!)
   (eval `(define-expander define-service ,hop-define-service-expander))
   (eval `(define-expander service ,hop-service-expander))
   (eval `(define-expander with-hop ,hop-with-hop-expander))
   (eval `(define-expander define-preferences ,hop-define-prefs-expander))
   (eval `(define-expander define-parameter ,hop-define-parameter-expander))
   (eval `(define-expander define-lazy-parameter ,hop-define-lazy-parameter-expander))
   (eval `(define-expander define-xml ,hop-define-xml-expander))
   (eval `(define-expander define-xml-markup ,hop-define-xml-markup-expander))
   (eval `(define-expander define-xml-element ,hop-define-xml-el-expander))
   (eval `(define-expander define-xml-alias ,hop-define-xml-alias-expander))
   (eval `(define-expander define-xml-compound ,hop-define-xml-cpd-expander)))
