;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/syncg.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 20 08:04:06 2017                          */
;*    Last change :  Tue Jun 18 10:59:14 2024 (serrano)                */
;*    Copyright   :  2017-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Global inter-process synchronization                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    THe module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_syncg

   (library hop hopscript js2scheme web)
   
   (export (synchronize-global ::bstring ::procedure)))

;*---------------------------------------------------------------------*/
;*    synchronize-process ...                                          */
;*---------------------------------------------------------------------*/
(define (synchronize-global lockfile proc)
   (let* ((path (if (char=? (string-ref lockfile 0) (file-separator))
		    lockfile
		    (make-file-name (pwd) lockfile)))
	  (dir (dirname path)))
      (unless (directory? dir)
	 (make-directories dir))
      (call-with-output-file path
	 (lambda (port)
	    (unwind-protect
	       (begin
		  (lockf port 'lock)
		  (proc))
	       (lockf port 'ulock))))))

;*---------------------------------------------------------------------*/
;*    global variables                                                 */
;*---------------------------------------------------------------------*/
(define syncg-state 'init)
(define syncg-mutex (make-mutex))
