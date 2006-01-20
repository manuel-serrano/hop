;*=====================================================================*/
;*    serrano/prgm/project/hop/share/hop.scm                           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 13:55:11 2005                          */
;*    Last change :  Thu Jan 19 08:52:21 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop initialization (default filtering).                          */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    autoloads                                                        */
;*---------------------------------------------------------------------*/
;; password
(autoload "password/password.scm" (autoload-prefix "/hop/password"))

;; info
(autoload "info/info.scm" (autoload-prefix "/hop/info"))

;; dired
(autoload "dired/dired.scm"
	  (lambda (req)
	     (with-access::http-request req (path localhostp)
		(and localhostp
		     (or (and (file-exists? path) (directory? path))
			 ((autoload-prefix "/hop/dired") req))))))

;; bbdb
(autoload "bbdb/bbdb.scm" (autoload-prefix "/hop/bbdb"))

;; jobs
(autoload "jobs/jobs.scm" (autoload-prefix "/hop/jobs"))

;; snd
(autoload "snd/snd.scm" (autoload-prefix "/hop/snd"))

