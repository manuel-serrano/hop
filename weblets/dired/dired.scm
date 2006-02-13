;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/dired/dired.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  6 15:05:13 2005                          */
;*    Last change :  Sun Feb 12 15:28:54 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A HOP file browser                                               */
;*    -------------------------------------------------------------    */
;*    This script can be loaded with something such as:                */
;*                                                                     */
;*    (autoload "dired/dired.scm"                                      */
;*    	  (lambda (req)                                                */
;*    	     (and (http-request-localhostp req)                        */
;*    		  (with-access::http-request req (path)                */
;*    		     (or (and (file-exists? path) (directory? path))   */
;*    			 (substring-at? path "/hop/dired/" 0))))))     */
;*=====================================================================*/
(library-load 'multimedia)
(library-load 'web)

;*---------------------------------------------------------------------*/
;*    hop-filter-add! ...                                              */
;*---------------------------------------------------------------------*/
(hop-filter-add!
 (lambda (req)
    (with-access::http-request req (path method user localhostp)
       (if (and localhostp (eq? method 'GET)
		(file-exists? path) (directory? path))
	   (if (user-authorized-request? user req)
	       (let* ((l (dired-cookie->exp req "dired"))
		      (mode (or (dired-plist-assq :mode l) 'icon))
		      (hide (or (dired-plist-assq :hide l) 'true))
		      (sort (or (dired-plist-assq :sort l) 'name))
		      (order (or (dired-plist-assq :order l) 'increase)))
		  (dired-show-directory req mode (eq? hide 'true) sort order))
	       (user-access-denied req))))))

;*---------------------------------------------------------------------*/
;*    dired-show-directory ...                                         */
;*---------------------------------------------------------------------*/
(define (dired-show-directory req mode hide sort order)
   (case mode
      ((name)
       (dired-as-name req (http-request-path req) hide sort order))
      ((icon)
       ($roundtrip ((w {window.innerWidth})
		    (h {window.innerHeight}))
	  (dired-as-icon req (http-request-path req) hide sort order w h)))
      (else
       (http-internal-error 'dired-directory-filter
			    "Illegal directory mode"
			    mode))))

;*---------------------------------------------------------------------*/
;*    The default configuration                                        */
;*---------------------------------------------------------------------*/
(hop-load "dired/param.scm")

;*---------------------------------------------------------------------*/
;*    The user configuration                                           */
;*---------------------------------------------------------------------*/
(hop-load-rc "diredrc.hop")

;*---------------------------------------------------------------------*/
;*    The actual sources                                               */
;*---------------------------------------------------------------------*/
(hop-load "dired/lib.scm")
(hop-load "dired/html.scm")
(hop-load "dired/config.scm")
(hop-load "dired/names.scm")
(hop-load "dired/icons.scm")
