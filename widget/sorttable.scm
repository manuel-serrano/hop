;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/widget/sorttable.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  7 08:28:34 2010                          */
;*    Last change :  Mon May 30 14:48:51 2011 (serrano)                */
;*    Copyright   :  2010-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Sorttable                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-sorttable

   (library hop)

   (export (<SORTTABLE> . ::obj)))

;*---------------------------------------------------------------------*/
;*    <SORTTABLE> ...                                                  */
;*---------------------------------------------------------------------*/
(define-tag <SORTTABLE> ((id #unspecified string)
			 (attributes)
			 body)
   (let ((i (xml-make-id)))
      (<SPAN> :id i :hssclass "hop-sorttable"
	 (<TABLE> :id (if (string? id) id (xml-make-id 'SORTTABLE))
	    attributes body)
	 (<SCRIPT> (format "hop_sorttable( '~a' )" i)))))
