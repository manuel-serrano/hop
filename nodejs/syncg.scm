;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/nodejs/syncg.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 20 08:04:06 2017                          */
;*    Last change :  Thu Apr 27 14:12:38 2017 (serrano)                */
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
	 (synchronize syncg-mutex
	    (when (eq? syncg-state 'init)
	       (set! syncg-state 'running)
	       (register-exit-function!
		  (lambda (status)
		     (synchronize syncg-mutex
			(set! syncg-state 'exit)
			(when (file-exists? lockfile)
			   (delete-file lockfile)))))))
	 (let loop ()
	    (if (file-exists? lockfile)
		(begin
		   (sleep 1000)
		   (loop))
		(synchronize syncg-mutex
		   (when (eq? syncg-state 'running)
		      (unwind-protect
			 (begin
			    (let ((p (open-output-file lockfile)))
			       (display (getpid) p)
			       (close-output-port p)
			       (proc)))
			 (delete-file lockfile))))))))
   
   (cond-expand
      (bigloo4.3a (synchronize-file lockfile proc))
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
	  
;*---------------------------------------------------------------------*/
;*    global variables                                                 */
;*---------------------------------------------------------------------*/
(define syncg-state 'init)
(define syncg-mutex (make-mutex))
    
