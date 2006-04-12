;*=====================================================================*/
;*    serrano/prgm/project/hop/hopwiki/toc.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 12 15:53:32 2006                          */
;*    Last change :  Wed Apr 12 17:28:14 2006 (serrano)                */
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
	       (tprint "e: " (find-runtime-type e))
	       (tprint "sibling: " (find-runtime-type (dom-next-sibling e)))
	       (tprint "parent: " (find-runtime-type (xml-element-parent e)))
	       (li
		(<A> :href (string-append "#" (dom-get-attribute (dom-next-sibling e) "name"))
		     (xml-element-body e))))
	    
	    (dom-get-elements-by-tag-name obj "h3"))))
      
      
