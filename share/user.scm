;*=====================================================================*/
;*    serrano/prgm/project/hop/1.11.x/share/user.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 22 11:43:39 2005                          */
;*    Last change :  Sun Feb  8 17:08:56 2009 (serrano)                */
;*    Copyright   :  2005-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    User authentication                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    user authentication ...                                          */
;*---------------------------------------------------------------------*/
(hop-filter-add-always-first!
 (lambda (req)
    (when (http-server-request? req)
       (with-access::http-request req (path method user)
	  (cond
	     ((not (file-exists? path))
	      req)
	     ((user-authorized-request? user req)
	      req)
	     (else
	      (user-access-denied req)))))))
       
