;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/password/password.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 14 06:14:00 2005                          */
;*    Last change :  Fri Jan 20 17:55:25 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP password encryption                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module weblet_password)

;*---------------------------------------------------------------------*/
;*    password-directory ...                                           */
;*---------------------------------------------------------------------*/
(define password-directory
   (make-file-name (hop-weblets-directory) "password"))

;*---------------------------------------------------------------------*/
;*    password filter ...                                              */
;*---------------------------------------------------------------------*/
(define-weblet (password)
   (<HTML>
      (<HEAD>
	 (<HOP-HEAD>)
	 (<SCRIPT> :type "text/javascript"
		   :src (format "~a/md5.js" (hop-share-directory)))
	 (<SCRIPT> :type "text/javascript"
		   { function crypt( el ) {
                        var na = document.getElementById( "user-name" )
                        var pd = document.getElementById( "password" )
                        var str = na.value + " " + pd.value
 
                        return hex_md5( str )
                   }})				      
	 (<LINK> :rel "stylesheet"
		 :type "text/css"
		 :href (format "~a/password.hss" password-directory)))
      (<BODY>
	 (<CENTER>
	    (<TABLE>
	       :class "password"
	       (<TR>
		  (<TD>
		     (<IMG> :src (format "~a/password.png"
					 password-directory)))
		  (<TD>
		     (<TABLE>
			(<TR>
			   (<TD> :colspan 2
				 (<DIV> :id "title" "Encrypt user identity")))
			(<TR>
			   (<TD> "User name: ")
			   (<TD> (<INPUT> :type 'text
					  :id "user-name"
					  :name "name"
					  :size 30)))
			(<TR>
			   (<TD> "Password: ")
			   (<TD> (<INPUT> :type 'password
					  :id "password"
					  :name "password"
					  :size 30)))
			(<TR>
			   (<TD>
			      :colspan 2
			      :align 'left
			      (<BUTTON>
				 :onclick {
				    document.getElementById( "result" ).innerHTML = crypt( this )
				    } "crypt")))
			(<TR>
			   (<TD> :colspan 2
				 :align 'center
				 (<DIV> :id "result" "&nbsp;")))))))))))

