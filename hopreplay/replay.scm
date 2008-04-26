;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/hopreplay/replay.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr 26 09:44:38 2008                          */
;*    Last change :  Sat Apr 26 15:56:52 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HOPRP Replay machinery                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hoprp_replay

   (library pthread web)
   
   (import  hoprp_log
	    hoprp_param)

   (export  (replay ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    replay-mutex ...                                                 */
;*---------------------------------------------------------------------*/
(define replay-mutex (make-mutex))
(define replay-wait-mutex (make-mutex))
(define replay-condv (make-condition-variable))

;*---------------------------------------------------------------------*/
;*    *reqs* ...                                                       */
;*---------------------------------------------------------------------*/
(define *reqs* '())
(define *req-num* 0)

;*---------------------------------------------------------------------*/
;*    replay ...                                                       */
;*---------------------------------------------------------------------*/
(define (replay reqs)
   (when (hoprp-loop) (set-cdr! (last-pair reqs) reqs))
   (set! *reqs* reqs)
   (set! *req-num* 0)
   (let loop ((num (hoprp-threads-num)))
      (when (>fx num 0)
	 (thread-start! (make-thread replay-thread))
	 (loop (-fx num 1))))
   (mutex-lock! replay-wait-mutex)
   (let loop ((num (hoprp-threads-num)))
      (condition-variable-wait! replay-condv replay-wait-mutex)
      (when (>fx num 0) (loop (-fx num 1)))))

;*---------------------------------------------------------------------*/
;*    replay-thread ...                                                */
;*---------------------------------------------------------------------*/
(define (replay-thread)
   (let loop ()
      (mutex-lock! replay-mutex)
      (when (pair? *reqs*)
	 (let ((req (car *reqs*))
	       (num *req-num*))
	    (set! *req-num* (+fx 1 *req-num*))
	    (set! *reqs* (cdr *reqs*))
	    (mutex-unlock! replay-mutex)
	    (with-access::request req (host port method path header)
	       (print num " " method " " path " (" host ":" port ")")
	       (let* ((s (make-client-socket (hoprp-host) (hoprp-port)))
		      (in (socket-input s))
		      (out (socket-output s)))
		  (unwind-protect
		     (with-handler
			(lambda (e)
			   (exception-notify e)
			   #f)
			(begin
			   (http :in in :out out
			      :method method
			      :header header
			      :host host :port port :path path)
			   (http-parse-response in out read-response)))
		     (socket-close s))))
	    (loop))))
   (mutex-lock! replay-wait-mutex)
   (condition-variable-broadcast! replay-condv)
   (mutex-unlock! replay-wait-mutex))

;*---------------------------------------------------------------------*/
;*    read-response ...                                                */
;*---------------------------------------------------------------------*/
(define (read-response in status-code header clen tenc)
   (when (and clen (> clen 0)) (read-chars clen in)))
			
