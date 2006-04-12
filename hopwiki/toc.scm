;*=====================================================================*/
;*    serrano/prgm/project/hop/hopwiki/toc.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 12 15:53:32 2006                          */
;*    Last change :  Wed Apr 12 16:25:17 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Wiki toc                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwiki_toc

   (library hop)

   (export  (hop-wiki->toc ::obj #!key (ul <UL>) (li <LI>))))

;*---------------------------------------------------------------------*/
;*    hop-wiki->toc ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-wiki->toc obj #!key (ul <UL>) (li <LI>))
   (tprint "hop-wiki->toc: " (find-runtime-type obj))
   (tprint "els: " (map find-runtime-type (dom-get-elements-by-tag-name obj "h3")))
   (ul (map (lambda (e)
	       (li (<A> :href "#toto" e)))
	    (map xml-element-body (dom-get-elements-by-tag-name obj "h3")))))
      
      
