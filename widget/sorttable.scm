;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/widget/sorttable.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  7 08:28:34 2010                          */
;*    Last change :  Sun Nov  7 08:30:36 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
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
(define-markup <SORTTABLE> ((id #unspecified string)
			    (attributes)
			    body)
   (let ((i (xml-make-id)))
      (<SPAN> :id i :hssclass "hop-sorttable"
	 (<TABLE> :id (if (string? id) id (xml-make-id 'SORTTABLE))
	    attributes body)
	 (<SCRIPT> (format "hop_sorttable( '~a' )" i)))))
