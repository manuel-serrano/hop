;*=====================================================================*/
;*    serrano/prgm/project/hop/share/user.scm                          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 22 11:43:39 2005                          */
;*    Last change :  Thu Nov  2 11:18:05 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    User authentication                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    user authentication ...                                          */
;*---------------------------------------------------------------------*/
(hop-filter-add-always-first!
 (lambda (req)
    (with-access::http-request req (localhostp path method user)
       (when localhostp
	  (cond
	     ((not (file-exists? path))
	      req)
	     ((user-authorized-request? user req)
	      req)
	     (else
	      (user-access-denied req)))))))
       
