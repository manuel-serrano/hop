;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/hopreplay/replay.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr 26 09:44:38 2008                          */
;*    Last change :  Fri Nov 16 14:39:33 2012 (serrano)                */
;*    Copyright   :  2008-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOPRP Replay machinery                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hoprp_replay

   (library web)
   
   (cond-expand
      (enable-threads (library pthread)))

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
   (synchronize replay-wait-mutex
      (let loop ((num (hoprp-threads-num)))
	 (condition-variable-wait! replay-condv replay-wait-mutex)
	 (when (>fx num 0) (loop (-fx num 1))))))

;*---------------------------------------------------------------------*/
;*    replay-thread ...                                                */
;*---------------------------------------------------------------------*/
(define (replay-thread)
   (let loop ()
      (let (req num)
	 (when (synchronize replay-mutex
		  (when (pair? *reqs*)
		     (set! req (car *reqs*))
		     (set! num *req-num*)
		     (set! *req-num* (+fx 1 *req-num*))
		     (set! *reqs* (cdr *reqs*))
		     #t))
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
   (synchronize replay-wait-mutex
      (condition-variable-broadcast! replay-condv)))

;*---------------------------------------------------------------------*/
;*    read-response ...                                                */
;*---------------------------------------------------------------------*/
(define (read-response in status-code header clen tenc)
   (when (and clen (> clen 0)) (read-chars clen in)))
			
