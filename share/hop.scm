;*=====================================================================*/
;*    serrano/prgm/project/hop/share/hop.scm                           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 13:55:11 2005                          */
;*    Last change :  Tue May 16 09:55:51 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop initialization (default filtering).                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Default libraries                                                */
;*---------------------------------------------------------------------*/
(library-load 'pthread)
(library-load 'web)

;*---------------------------------------------------------------------*/
;*    autoloads                                                        */
;*---------------------------------------------------------------------*/
;* ;; dired                                                            */
;* (autoload "dired/dired.scm"                                         */
;* 	  (lambda (req)                                                */
;* 	     (with-access::http-request req (path localhostp)          */
;* 		(and localhostp                                        */
;* 		     (or (and (file-exists? path) (directory? path))   */
;* 			 ((autoload-prefix "/hop/dired") req))))))     */
