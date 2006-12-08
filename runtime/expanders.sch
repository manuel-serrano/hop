;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/expanders.sch                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 18:25:11 2006                          */
;*    Last change :  Thu Dec  7 07:44:53 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop expanders installer                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    hop-install-expanders! ...                                       */
;*---------------------------------------------------------------------*/
(define (hop-install-expanders!)
   (eval `(define-expander define-service ,hop-define-service-expander))
   (eval `(define-expander service ,hop-service-expander))
   (eval `(define-expander $roundtrip ,hop-roundtrip-expander))
   (eval `(define-expander with-hop ,hop-with-hop-expander))
   (eval `(define-expander define-preferences ,hop-define-prefs-expander))
   (eval `(define-expander define-parameter ,hop-define-parameter-expander))
   (eval `(define-expander define-xml ,hop-define-xml-expander))
   (eval `(define-expander define-xml-markup ,hop-define-xml-markup-expander))
   (eval `(define-expander define-xml-element ,hop-define-xml-el-expander))
   (eval `(define-expander define-xml-alias ,hop-define-xml-alias-expander))
   (eval `(define-expander define-xml-compound ,hop-define-xml-cpd-expander)))
