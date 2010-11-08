;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/widget/foot.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  7 08:26:54 2010                          */
;*    Last change :  Sun Nov  7 08:27:53 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Foot widgets                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-foot

   (library hop)

   (export (<FOOT> . ::obj)
	   (<FOOT-BUTTON> . ::obj)))

;*---------------------------------------------------------------------*/
;*    <FOOT> ...                                                       */
;*---------------------------------------------------------------------*/
(define-markup <FOOT> ((id #unspecified string)
		       (class "foot" string)
		       (inline #f)
		       body)
   (<DIV> :id (xml-make-id id 'FOOT)
      :class class
      (<DIV> :align "center"
	 :class "foot-buttons"
	 (<FOOT-BUTTON>
	    :href "http://hop.inria.fr"
	    :title "HOP home page"
	    :inline inline
	    :src "hop.png")
	 body)))

;*---------------------------------------------------------------------*/
;*    <FOOT-BUTTON> ...                                                */
;*---------------------------------------------------------------------*/
(define-markup <FOOT-BUTTON> ((id #unspecified string)
			      (class "foot-button")
			      (href #f string)
			      (title #f string)
			      (path #f)
			      (inline #f)
			      (src #f))
   (<A> :class class
      :href href
      :title title
      (<IMG> :alt title
	 :inline inline
	 :src (cond
		 ((string? path)
		  path)
		 ((string? src)
		  (if (string=? (dirname src) ".")
		      (format "~a/buttons/~a"
			      (url-path-encode (hop-share-directory))
			      src)
		      src))
		 (else
		  (error "<FOOT-BUTTON>" "Illegal source" src))))))
				     
