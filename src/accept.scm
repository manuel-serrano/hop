;*=====================================================================*/
;*    serrano/prgm/project/hop/1.10.x/src/accept.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep  1 08:35:47 2008                          */
;*    Last change :  Fri Oct 10 10:26:48 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop accept loop                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_accept

   (library hop)

   (include "stage.sch")

   (cond-expand
      (enable-threads (library pthread)))

   (cond-expand
      (enable-ssl (library ssl)))

   (library hop)

   (import  hop_parseargs
	    hop_param
	    hop_init
	    hop_scheduler
	    hop_scheduler-nothread
	    hop_scheduler-queue
	    hop_scheduler-one-to-one
	    hop_scheduler-pool
	    hop_scheduler-cohort
	    hop_pipeline)

   (export  (generic scheduler-accept-loop ::scheduler ::socket)
	    *verb-mutex*))

;*---------------------------------------------------------------------*/
;*    hop-verb ...                                                     */
;*---------------------------------------------------------------------*/
(define-expander hop-verb
   (lambda (x e)
      (match-case x
	 ((?- (and (? integer?) ?level) . ?rest)
	  (let ((v (gensym)))
	     `(let ((,v ,(e level e)))
		 (if (>=fx (hop-verbose) ,v)
		     (with-lock *verb-mutex*
			(lambda ()
			   (hop-verb ,v ,@(map (lambda (x) (e x e)) rest))))))))
	 (else
	  `(with-lock *verb-mutex*
	      (lambda ()
		 (hop-verb ,@(map (lambda (x) (e x e)) (cdr x)))))))))

;*---------------------------------------------------------------------*/
;*    *verb-mutex* ...                                                 */
;*---------------------------------------------------------------------*/
(define *verb-mutex* (make-mutex 'hop-verb))

;*---------------------------------------------------------------------*/
;*    tune-socket! ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (tune-socket! sock)
   (cond-expand
      ((or bigloo3.1a bigloo3.1b)
       #unspecified)
      (else
       (socket-option-set! sock :TCP_NODELAY #t))))
   
;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (scheduler-accept-loop scd::scheduler serv::socket)
   (let ((dummy-buf (make-string 512)))
      (let loop ((id 1))
	 (let ((sock (socket-accept serv :inbuf dummy-buf :outbuf dummy-buf)))
	    (hop-verb 2 (hop-color id id " ACCEPT")
		      ": " (socket-hostname sock) " [" (current-date) "]\n")
	    ;; tune the socket
	    (tune-socket! sock)
	    ;; process the request
	    (spawn4 scd stage-request id sock 'connect (hop-read-timeout))
	    (loop (+fx id 1))))))

;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ::queue-scheduler ...                      */
;*---------------------------------------------------------------------*/
(define-method (scheduler-accept-loop scd::queue-scheduler serv::socket)
   (let* ((acclen (min 50 (/fx (hop-max-threads) 2)))
	  (dummy-buffers (make-vector acclen (make-string 512)))
	  (socks (make-vector acclen))) 
      (let loop ((id 1))
	 (let ((n (socket-accept-many serv socks
				      :inbufs dummy-buffers
				      :outbufs dummy-buffers)))
	    (when (>fx n 1) (tprint "*** socket-accept-many: n=" n))
	    (let liip ((i 0))
	       (if (=fx i n)
		   (loop (+fx id i))
		   (let ((sock (vector-ref socks i)))
		      (hop-verb 2 (hop-color id id " ACCEPT")
				": " (socket-hostname sock)
				" [" (current-date) "]\n")
		      ;; tune the socket
		      (tune-socket! sock)
		      ;; process the request
		      (spawn4 scd stage-request (+fx id i)
			      sock
			      'connect (hop-read-timeout))
		      (liip (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ::pool-scheduler ...                       */
;*---------------------------------------------------------------------*/
(define-method (scheduler-accept-loop scd::pool-scheduler serv::socket)
   
   (define dummybuf (make-string 512))
   (define idmutex (make-mutex))
   (define idcount (scheduler-size scd))
   
   (define (get-next-id)
      (if (=fx (hop-verbose) 0)
	  0
	  (begin
	     (mutex-lock! idmutex)
	     (let ((v (+fx idcount 1)))
		(set! idcount v)
		(mutex-unlock! idmutex)
		v))))
   
   (define (connect-stage scd thread)
      ;; We need to install our own handler (in addition to the one
      ;; installed by the scheduler when it builds the thread) because
      ;; we need to resume the connect-stage (with new connections) on errors.
      (with-handler
	 (make-scheduler-error-handler thread)
	 (let loop ()
	    (with-access::pool-scheduler scd (naccept)
	       (mutex-lock! (pool-scheduler-mutex scd))
	       (set! naccept (+fx naccept 1))
	       (mutex-unlock! (pool-scheduler-mutex scd)))
	    (let* ((sock (socket-accept serv :inbuf dummybuf :outbuf dummybuf))
		   (id  (get-next-id)))
	       (with-access::pool-scheduler scd (naccept)
		  (mutex-lock! (pool-scheduler-mutex scd))
		  (set! naccept (-fx naccept 1))
		  (mutex-unlock! (pool-scheduler-mutex scd)))
	       (hop-verb 2 (hop-color id id " ACCEPT")
			 (if (>=fx (hop-verbose) 3) (format " ~a" thread) "")
			 ": " (socket-hostname sock) " [" (current-date) "]\n")
	       ;; tune the socket
	       (tune-socket! sock)
	       ;; process the request
	       (stage4 scd thread
		       stage-request id sock
		       'connect (hop-read-timeout))
	       (loop))))
      (connect-stage scd thread))
   
   (let loop ((i (length (pool-scheduler-free scd))))
      (if (<=fx i 1)
	  (thread-join! (spawn0 scd connect-stage))
	  (begin
	     (spawn0 scd connect-stage)
	     (loop (-fx i 1))))))

;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ...                                        */
;*---------------------------------------------------------------------*/
(define-method (scheduler-accept-loop scd::nothread-scheduler serv::socket)
   (let ((thread (nothread-scheduler-get-fake-thread)))
      (with-handler
	 (make-scheduler-error-handler thread)
	 (let ((dummybuf (make-string 512)))
	    (let loop ((id 1))
	       (let ((sock (socket-accept serv :inbuf dummybuf :outbuf dummybuf)))
		  (hop-verb 2 (hop-color id id " ACCEPT")
			    (if (>=fx (hop-verbose) 3) (format " ~a" thread) "")
			    ": " (socket-hostname sock) " [" (current-date)
			    "]\n")
		  ;; tune the socket
		  (tune-socket! sock)
		  ;; process the request
		  (spawn4 scd stage-request id sock 'connect (hop-read-timeout))
		  (loop (+fx id 1))))))))

