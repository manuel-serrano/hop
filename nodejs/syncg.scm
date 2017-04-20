;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/nodejs/syncg.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 20 08:04:06 2017                          */
;*    Last change :  Thu Apr 20 13:40:23 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Global inter-process synchronization                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    THe module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_syncg

   (library pthread)

   (export (synchronize-global ::bstring ::procedure)))

;*---------------------------------------------------------------------*/
;*    synchronize-process ...                                          */
;*---------------------------------------------------------------------*/
(define (synchronize-global lockfile proc)
   
   (define (synchronize-file lockfile proc)
      (let ((dir (dirname lockfile)))
	 (unless (directory? dir)
	    (make-directories dir))
	 (let loop ()
	    (if (file-exists? lockfile)
		(begin
		   (sleep 1000)
		   (loop))
		(unwind-protect
		   (let ((p (open-output-file lockfile)))
		      (display (getpid) p)
		      (close-output-port p)
		      (proc))
		   (delete-file lockfile))))))
   
   (cond-expand
      (bigloo4.3a
       (synchronize-file lockfile proc))
      (else
       (let* ((semname (basename lockfile))
	      (sem (open-semaphore semname)))
	  (if (semaphore? sem)
	      (unwind-protect
		 (begin
		    (semaphore-wait sem)
		    (proc))
		 (begin
		    (semaphore-post sem)
		    (close-semaphore sem)
		    (delete-semaphore semname)))
	      (synchronize-file lockfile proc))))))
	  
