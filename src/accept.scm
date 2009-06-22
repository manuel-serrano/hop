;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/src/accept.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep  1 08:35:47 2008                          */
;*    Last change :  Wed Apr 29 14:07:40 2009 (serrano)                */
;*    Copyright   :  2008-09 Manuel Serrano                            */
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
	    hop_scheduler-accept-many
	    hop_pipeline)

   (export  (generic scheduler-accept-loop ::scheduler ::socket ::bool)
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
   (socket-option-set! sock :SO_SNDBUF (hop-sndbuf))
   (socket-option-set! sock :TCP_NODELAY #t))
    
;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (scheduler-accept-loop scd::scheduler serv::socket wait::bool)
   (let loop ((id 1))
      (let ((sock (socket-accept serv)))
	 (hop-verb 2 (hop-color id id " ACCEPT")
		   ": " (socket-hostname sock) " [" (current-date) "]\n")
	 ;; tune the socket
	 (tune-socket! sock)
	 ;; process the request
	 (spawn scd stage-request id sock 'connect (hop-read-timeout))
	 (loop (+fx id 1)))))

;*---------------------------------------------------------------------*/
;*    allocate-vector ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (allocate-vector size fill)
   (let ((v (gensym)))
      `(let ((,v (make-vector ,size)))
	  (let loop ((i 0))
	     (if (=fx i ,size)
		 ,v
		 (begin
		    (vector-set! ,v i ,fill)
		    (loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ::queue-scheduler ...                      */
;*---------------------------------------------------------------------*/
(define-method (scheduler-accept-loop scd::queue-scheduler serv::socket w::bool)
   (let* ((acclen (min 50 (/fx (hop-max-threads) 2)))
	  (socks (make-vector acclen))) 
      (let loop ((id 1))
	 (with-access::queue-scheduler scd (mutex condv qlength max-qlength)
	    (mutex-lock! mutex)
	    (when (>fx qlength max-qlength)
	       (condition-variable-wait! condv mutex))
	    (mutex-unlock! mutex))
	 (let* ((in-buffers (allocate-vector acclen (make-string 512)))
		(out-buffers (allocate-vector acclen (make-string 1024)))
		(n (socket-accept-many serv socks
				       :inbufs in-buffers
				       :outbufs out-buffers)))
	    (when (>fx n 1) (tprint "*** socket-accept-many: n=" n))
	    (let liip ((i 0))
	       (if (=fx i n)
		   (loop (+fx id i))
		   (let ((sock (vector-ref socks i))
			 (nid (+fx id i)))
		      (hop-verb 2 (hop-color nid nid " ACCEPT")
				": " (socket-hostname sock)
				" [" (current-date) "]\n")
		      ;; tune the socket
		      (tune-socket! sock)
		      ;; process the request
		      (spawn scd stage-request nid
			     sock
			     'connect (hop-read-timeout))
		      (liip (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ::pool-scheduler ...                       */
;*---------------------------------------------------------------------*/
(define-method (scheduler-accept-loop scd::pool-scheduler serv::socket w::bool)
   
   (define dummybuf (make-string 512))
   (define idmutex (make-mutex))
   (define idcount (scheduler-size scd))
   (define nbthreads (length (pool-scheduler-free scd)))
   
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
	    (let* ((sock (socket-accept serv
					:inbuf (hopthread-inbuf thread)
					:outbuf (hopthread-outbuf thread)))
		   (id  (get-next-id))
		   (fbuf (hopthread-flushbuf thread)))
	       (output-port-flush-buffer-set! (socket-output sock) fbuf)
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
	       (stage scd thread stage-request id sock 'connect (hop-read-timeout))
	       (loop))))
      (connect-stage scd thread))
   
   (let loop ((i nbthreads))
      (if (<=fx i 1)
	  (let ((th (spawn scd connect-stage)))
	     (when w (thread-join! th)))
	  (begin
	     (spawn scd connect-stage)
	     (loop (-fx i 1))))))

;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ::accept-many-scheduler ...                */
;*---------------------------------------------------------------------*/
(define-method (scheduler-accept-loop scd::accept-many-scheduler serv::socket w)
   
   (define acclen (length (pool-scheduler-free scd)))
   (define dummybufs (make-vector acclen (make-string 10)))
   (define socks (make-vector acclen))

   (define (stage-accept scd thread id sock mode timeout n)
      ;; a little bit of traces
      (hop-verb 2 (hop-color id id " ACCEPT")
		(if (>=fx (hop-verbose) 3)
		    (format " ~a, ~a accept" thread n)
		    "")
		": " (socket-hostname sock) " [" (current-date) "]\n")
      ;; tune the socket
      (tune-socket! sock)
      ;; prepare the socket buffers
      (let ((fbuf (hopthread-flushbuf thread)))
	 (output-port-flush-buffer-set! (socket-output sock) fbuf)
	 (input-port-buffer-set! (socket-input sock) (hopthread-inbuf thread))
	 (output-port-buffer-set! (socket-output sock) (hopthread-outbuf thread)))
      ;; process the request
      (stage scd thread stage-request id sock mode timeout))

   (let loop ((id 1))
      (let ((n (socket-accept-many serv socks
				   :inbufs dummybufs
				   :outbufs dummybufs)))
	 (if (> n 1) (tprint "*** socket-accept-many: " n))
	 (let liip ((i 0))
	    (if (=fx i n)
		(loop (+fx id i))
		(let ((sock (vector-ref socks i)))
		   ;; process the request
		   (spawn scd stage-accept (+fx id i)
			  sock
			  'connect (hop-read-timeout)
			  n)
		   (liip (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ...                                        */
;*---------------------------------------------------------------------*/
(define-method (scheduler-accept-loop scd::nothread-scheduler serv::socket w)
   (letrec ((thread (nothread-scheduler-get-fake-thread
		     (lambda ()
			(with-handler
			   (make-scheduler-error-handler thread)
			   (let loop ((id 1))
			      (let ((s (socket-accept
					serv
					:inbuf (hopthread-inbuf thread)
					:outbuf (hopthread-outbuf thread))))
				 ;; tune the socket
				 (tune-socket! s)
				 ;; process the request
				 (spawn scd stage-request id s
					'connect (hop-read-timeout))
				 (loop (+fx id 1)))))))))
      (if w
	  (thread-join! (thread-start-joinable! thread))
	  (thread-start! thread))))
