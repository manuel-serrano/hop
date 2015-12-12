;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/src/accept.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep  1 08:35:47 2008                          */
;*    Last change :  Sat Dec 12 13:13:39 2015 (serrano)                */
;*    Copyright   :  2008-15 Manuel Serrano                            */
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
;*    *verb-mutex* ...                                                 */
;*---------------------------------------------------------------------*/
(define *verb-mutex* (make-mutex "hop-verb"))

;*---------------------------------------------------------------------*/
;*    tune-socket! ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (tune-socket! sock)
   ;; MS CARE: as of 4 may 2012 setting SO_SNDBUF blocks sendfile
   ;; (config: Linux 3.3.4 glibc 2.15)
   ;;(socket-option-set! sock :SO_SNDBUF (hop-sndbuf))
   (socket-option-set! sock :TCP_NODELAY #t))
    
;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (scheduler-accept-loop scd::scheduler serv::socket wait::bool)
   (let loop ((id 1))
      (let ((sock (socket-accept serv)))
	 (if (socket-reject sock)
	     (begin
		(notify-reject sock)
		(socket-shutdown sock)
		(loop id))
	     (begin
		(hop-verb 2 (hop-color id id " ACCEPT")
			  ": " (socket-hostname sock)
			  " [" (current-date) "]\n")
		;; tune the socket
		(tune-socket! sock)
		;; process the request
		(spawn scd stage-request id sock (hop-read-timeout) 'connect)
		(loop (+fx id 1)))))))

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
	    (synchronize mutex
	       (when (>fx qlength max-qlength)
		  (condition-variable-wait! condv mutex))))
	 (let* ((in-buffers (allocate-vector acclen (make-string 512)))
		(out-buffers (allocate-vector acclen (make-string 1024)))
		(n (socket-accept-many serv socks
		      :inbufs in-buffers
		      :outbufs out-buffers)))
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
		      (spawn scd stage-request nid sock
			 (hop-read-timeout) 'connect)
		      (liip (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ::pool-scheduler ...                       */
;*---------------------------------------------------------------------*/
(define-method (scheduler-accept-loop scd::pool-scheduler serv::socket w::bool)
   
   (define dummybuf (make-string 512))
   (define idmutex (make-mutex "pool-scheduler"))
   (define idcount 0)
   (define nbthreads
      (with-access::pool-scheduler scd (nfree) nfree))
   
   (define (get-next-id)
      (if (=fx (hop-verbose) 0)
	  0
	  (begin
	     (synchronize idmutex
		(let ((v (+fx idcount 1)))
		   (set! idcount v)
		   v)))))

   (define (scheduler-load-add! scd inc)
      (with-access::pool-scheduler scd (mutex naccept)
	 (synchronize mutex
	    (set! naccept (+fx naccept inc)))))

   (define (accept-error-handler e scd)
      (hop-verb 2 (hop-color -1 -1 " CONNECTION FAILED"))
      (hop-verb 2 ": " (with-access::&error e (obj) obj) "\n")
      (scheduler-load-add! scd -1)
      (exception-notify e)
      #unspecified)
   
   (define (connect-stage scd thread)
      ;; We need to install our own handler (in addition to the one
      ;; installed by the scheduler when it builds the thread) because
      ;; we need to resume the connect-stage (with new connections) on errors.
      (with-handler
	 (make-scheduler-error-handler thread)
	 (let loop ()
	    (scheduler-load-add! scd 1)
	    (with-stage-handler accept-error-handler (scd)
	       (with-access::scdthread thread (inbuf outbuf flushbuf)
		  (let ((sock (socket-accept serv
				 :inbuf inbuf
				 :outbuf outbuf)))
		     (if (socket-reject sock)
			 (begin
			    (notify-reject sock)
			    (socket-shutdown sock)
			    (loop))
			 (let ((id (get-next-id)))
			    (scheduler-load-add! scd -1)
			    (output-port-flush-buffer-set!
			       (socket-output sock) flushbuf)
			    (hop-verb 2 (hop-color id id " ACCEPT")
			       (if (>=fx (hop-verbose) 3)
				   (format " ~a" thread) "")
			       ": " (socket-hostname sock)
			       (if (>=fx (hop-verbose) 4)
				   (format ":~a" (socket-port-number sock))
				   "")
			       " [" (current-date) "]\n")
			    ;; tune the socket
			    (tune-socket! sock)
			    ;; process the request
			    (stage scd
			       thread stage-request id sock
			       (hop-read-timeout) 'connect)
			    ;; go back to the accept stage
			    (loop))))))))
      (connect-stage scd thread))

   (when (<fx nbthreads 6)
      (error "hop" "scheduler-accept requires at least 6 threads" nbthreads))
   (let loop ((i (if (>fx nbthreads 8) (- nbthreads 4) (/fx nbthreads 2))))
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
   
   (define acclen (length (with-access::pool-scheduler scd (free) free)))
   (define dummybufs (make-vector acclen (make-string 10)))
   (define socks (make-vector acclen))
   
   (define (stage-accept scd thread id sock timeout n)
      ;; a little bit of traces
      (hop-verb 2 (hop-color id id " ACCEPT")
	 (if (>=fx (hop-verbose) 3)
	     (format " ~a, ~a accept" thread n)
	     "")
	 ": " (socket-hostname sock)
	 " [" (current-date) "]\n")
      ;; tune the socket
      (tune-socket! sock)
      ;; prepare the socket buffers
      (with-access::scdthread thread (flushbuf inbuf outbuf)
	 (let ((fbuf flushbuf))
	    (output-port-flush-buffer-set! (socket-output sock) fbuf)
	    (input-port-buffer-set! (socket-input sock) inbuf)
	    (output-port-buffer-set! (socket-output sock) outbuf)))
      ;; process the request
      (stage scd thread stage-request id sock timeout 'connect))
   
   (let loop ((id 1))
      (let ((n (socket-accept-many serv socks
		  :inbufs dummybufs
		  :outbufs dummybufs)))
	 (let liip ((i 0))
	    (if (=fx i n)
		(loop (+fx id i))
		(let ((sock (vector-ref socks i)))
		   ;; process the request
		   (spawn scd stage-accept (+fx id i)
		      sock (hop-read-timeout) n)
		   (liip (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ...                                        */
;*---------------------------------------------------------------------*/
(define-method (scheduler-accept-loop scd::nothread-scheduler serv::socket w)
   (letrec ((thread (nothread-scheduler-get-fake-thread
		       (lambda ()
			  (let loop ()
			     (with-handler
				(make-scheduler-error-handler thread)
				(with-access::scdthread thread (inbuf outbuf)
				   (let loop ((id 1))
				      (let ((s (socket-accept
						  serv
						  :inbuf inbuf
						  :outbuf outbuf)))
					 (if (socket-reject s)
					     (begin
						;; notify and close
						(notify-reject s)
						(socket-shutdown s)
						(loop id))
					     (begin
						;; tune the socket
						(tune-socket! s)
						;; process the request
						(spawn scd stage-request id s
						   (hop-read-timeout) 'connect)
						(loop (+fx id 1))))))))
			     (loop))))))
      (if w
	  (thread-join! (thread-start-joinable! thread))
	  (thread-start! thread))))
;*                                                                     */
;*---------------------------------------------------------------------*/
;*    socket-reject ...                                                */
;*---------------------------------------------------------------------*/
(define (socket-reject sock)
   (hashtable-get (hop-ip-blacklist-table) (socket-host-address sock)))

;*---------------------------------------------------------------------*/
;*    notify-reject ...                                                */
;*---------------------------------------------------------------------*/
(define (notify-reject sock)
   (hop-verb 1 (hop-color -1 -1 " REJECT")
	     ": " (socket-host-address sock)
	     " [" (current-date) "]\n"))

