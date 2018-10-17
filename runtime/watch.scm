;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/runtime/watch.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 21 18:31:30 2017                          */
;*    Last change :  Tue Jul  3 04:59:12 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Watch for socket close                                           */
;*    -------------------------------------------------------------    */
;*    This module implements the WATCH-SOCKET! facility that waits     */
;*    for a socket termination. More precisely, it waits for a char    */
;*    to be available on the socket, as the only char that can be      */
;*    read is EOF.                                                     */
;*                                                                     */
;*    The *INPIPE* variable is used to "unlock" the SELECT call when   */
;*    a new thread is to be added to the watch set.                    */
;*                                                                     */
;*    An alternative method, without select would consist in spawning  */
;*    a new thread waiting for EOF each time a new socket is to be     */
;*    watched.                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_watch

   (include "thread.sch")

   (import __hop_thread)
   
   (export (watch-socket! ::socket ::procedure)))

;*---------------------------------------------------------------------*/
;*    Global variables ...                                             */
;*---------------------------------------------------------------------*/
(define *inpipe* #f)
(define *outpipe* #f)
(define *sockets* '())

(define *watch-mutex* (make-mutex "watch"))

;*---------------------------------------------------------------------*/
;*    watch-socket! ...                                                */
;*---------------------------------------------------------------------*/
(define (watch-socket! socket onclose)
   (cond-expand
      (bigloo4.3a
       ;; alternative without select
       (thread-start!
	  (instantiate::hopthread
	     (body (lambda ()
		      (input-port-timeout-set! (socket-input socket) 0)
		      (onclose socket))))))
      (else
       ;; efficient method with select and an pipe to unlock it
       ;; when a new socket is to be added
       (synchronize *watch-mutex*
	  (set! *sockets* (cons socket *sockets*))
	  (if *outpipe*
	      (begin
		 (write-char #\Newline *outpipe*)
		 (flush-output-port *outpipe*))
	      (thread-start!
		 (instantiate::hopthread
		    (body (watch-thread onclose)))))))))

;*---------------------------------------------------------------------*/
;*    watch-thread ...                                                 */
;*---------------------------------------------------------------------*/
(define (watch-thread onclose)
   (cond-expand
      (bigloo4.3a
       (lambda () #f))
      (else
       (lambda ()
	  (let loop ()
	     (synchronize *watch-mutex*
		(unless *inpipe*
		   (multiple-value-bind (in out)
		      (open-pipes)
		      (set! *inpipe* in)
		      (set! *outpipe* out))))
	     (multiple-value-bind (readfs _ errfs)
		(with-handler
		   (lambda (e)
		      (values '() #f #f))
		   (select :read (cons *inpipe* *sockets*)))
		(let ((socks (filter socket? readfs)))
		   (with-trace 'watch "watch-thread"
		      (trace-item "readfs=" readfs)
		      (trace-item "errfs=" errfs)
		      (for-each onclose socks)
		      (synchronize *watch-mutex*
			 (for-each (lambda (s)
				      (set! *sockets* (remq! s *sockets*)))
			    socks)
			 (unless (pair? *sockets*)
			    (close-input-port *inpipe*)
			    (close-output-port *outpipe*)
			    (set! *inpipe* #f)
			    (set! *outpipe* #f)))
		      (when *outpipe*
			 (sleep 1000000)
			 (loop))))))))))
