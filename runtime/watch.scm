;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/runtime/watch.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 21 18:31:30 2017                          */
;*    Last change :  Wed Mar  7 07:27:33 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Watch for socket close                                           */
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
       #unspecified)
      (else
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
	     (multiple-value-bind (readfs _ _)
		(with-handler
		   (lambda (e)
		      (values '() #f #f))
		   (select :read (cons *inpipe* *sockets*)))
		(let ((socks (filter socket? readfs)))
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
		      (loop)))))))))
