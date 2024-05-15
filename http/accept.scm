;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/http/accept.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep  1 08:35:47 2008                          */
;*    Last change :  Wed May 15 08:45:57 2024 (serrano)                */
;*    Copyright   :  2008-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop accept loop                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __http_accept

   (include "stage.sch")
 
   (library pthread)
   
   (cond-expand
      (enable-ssl (library ssl)))

   (import  __http_utils
	    __http_scheduler
	    __http_scheduler-nothread
	    __http_scheduler-queue
	    __http_scheduler-one-to-one
	    __http_scheduler-pool
	    __http_scheduler-accept-many
	    __http_pipeline)

   (export  (generic scheduler-accept-loop ::scheduler ::socket ::bool #!optional blacklist)))

;*---------------------------------------------------------------------*/
;*    tune-socket! ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (tune-socket! sock)
   (socket-option-set! sock :TCP_NODELAY #t))
    
;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (scheduler-accept-loop scd::scheduler serv::socket wait::bool #!optional blacklist)
   (tune-socket! serv)
   (with-access::scheduler scd (onready)
      (when (procedure? onready)
	 (onready)))
   (let ((table (blacklist-table blacklist))
	 (timeout (with-access::scheduler scd (accept-timeout) accept-timeout)))
      (let loop ((id 1))
	 (let ((sock (socket-accept serv)))
	    (if (socket-blacklist? sock table)
		(begin
		   (notify-reject sock)
		   (socket-shutdown sock)
		   (loop id))
		(begin
		   (trace 2 id "ACCEPT"
		      (format ": ~a [~a]\n"
			 (socket-hostname sock)
			 (current-date)))
		   ;; tune the socket
		   (tune-socket! sock)
		   ;; process the request
		   (spawn scd stage-request id sock timeout 'connect)
		   (loop (+fx id 1))))))))

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
(define-method (scheduler-accept-loop scd::queue-scheduler serv::socket w::bool #!optional blacklist)
   (tune-socket! serv)
   (with-access::scheduler scd (onready)
      (when (procedure? onready)
	 (onready)))
   (with-access::queue-scheduler scd (mutex condv qlength max-qlength accept-timeout)   
      (let* ((acclen qlength)
	     (socks (make-vector acclen))
	     (table (blacklist-table blacklist))
	     (timeout accept-timeout))
	 (let loop ((id 1))
	    (synchronize mutex
	       (when (>fx qlength max-qlength)
		  (condition-variable-wait! condv mutex)))
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
			 (trace 2 nid "ACCEPT"
			    (format ": ~a [~a]\n"
			       (socket-hostname sock)
			       (current-date)))
			 ;; tune the socket
			 (tune-socket! sock)
			 ;; process the request
			 (spawn scd stage-request nid sock timeout 'connect)
			 (liip (+fx i 1))))))))))

;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ::pool-scheduler ...                       */
;*---------------------------------------------------------------------*/
(define-method (scheduler-accept-loop scd::pool-scheduler serv::socket w::bool #!optional blacklist)
   
   (define dummybuf (make-string 512))
   (define idmutex (make-spinlock "pool-scheduler"))
   (define idcount 0)
   (define nbthreads (with-access::pool-scheduler scd (nfree) nfree))
   (define table (blacklist-table blacklist))
   (define timeout (with-access::pool-scheduler scd (accept-timeout) accept-timeout))
   
   (define (get-next-id)
      (if (=fx (trace-level) 0)
	  0
	  (synchronize idmutex
	     (let ((v (+fx idcount 1)))
		(set! idcount v)
		v))))

   (define (scheduler-load-add! scd inc)
      (with-access::pool-scheduler scd (mutex naccept)
	 (synchronize mutex
	    (set! naccept (+fx naccept inc)))))

   (define (accept-error-handler e scd)
      (trace 2 -1 "CONNECTION FAILED"
	 ": " (with-access::&error e (obj) obj))
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
		     (if (socket-blacklist? sock table)
			 (begin
			    (notify-reject sock)
			    (socket-shutdown sock)
			    (loop))
			 (let ((id (get-next-id)))
			    (scheduler-load-add! scd -1)
			    (output-port-flush-buffer-set!
			       (socket-output sock) flushbuf)
			    (trace 2 "ACCEPT"
			       (if (>=fx (trace-level) 3)
				   (format " ~a" thread) "")
			       ": " (socket-hostname sock)
			       (if (>=fx (trace-level) 4)
				   (format ":~a" (socket-port-number sock))
				   "")
			       " [" (current-date) "]")
			    ;; tune the socket
			    (tune-socket! sock)
			    ;; process the request
			    (stage scd
			       thread stage-request id sock
			       timeout 'connect)
			    ;; go back to the accept stage
			    (loop))))))))
      (connect-stage scd thread))

   (when (<fx nbthreads 6)
      (error "hop" "scheduler-accept requires at least 6 threads" nbthreads))
   (tune-socket! serv)
   (with-access::scheduler scd (onready)
      (when (procedure? onready)
	 (onready)))
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
(define-method (scheduler-accept-loop scd::accept-many-scheduler serv::socket w #!optional blacklist)
   
   (define acclen (length (with-access::pool-scheduler scd (free) free)))
   (define dummybufs (make-vector acclen (make-string 10)))
   (define socks (make-vector acclen))
   
   (define (stage-accept scd thread id sock timeout n)
      ;; a little bit of traces
      (trace 2 id "ACCEPT"
	 (if (>=fx (trace-level) 3)
	     (format " ~a, ~a accept" thread n)
	     "")
	 (format ": ~a [~a]" (socket-hostname sock) (current-date)))
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

   (tune-socket! serv)
   (with-access::scheduler scd (onready)
      (when (procedure? onready)
	 (onready)))
   (let ((table (blacklist-table blacklist))
	 (timeout (with-access::scheduler scd (accept-timeout) accept-timeout)))
      (let loop ((id 1))
	 (let ((n (socket-accept-many serv socks
		     :inbufs dummybufs
		     :outbufs dummybufs)))
	    (let liip ((i 0))
	       (if (=fx i n)
		   (loop (+fx id i))
		   (let ((s (vector-ref socks i)))
		      ;; tune the socket
		      (tune-socket! s)
		      ;; process the request
		      (spawn scd stage-accept (+fx id i) s timeout n)
		      (liip (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    scheduler-accept-loop ...                                        */
;*---------------------------------------------------------------------*/
(define-method (scheduler-accept-loop scd::nothread-scheduler serv::socket w #!optional blacklist)
   (letrec* ((table (blacklist-table blacklist))
	     (timeout (with-access::scheduler scd (accept-timeout) accept-timeout))
	     (thread (nothread-scheduler-get-fake-thread
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
					  (if (socket-blacklist? s table)
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
						    timeout 'connect)
						 (loop (+fx id 1))))))))
			      (loop))))))
      (tune-socket! serv)
      (with-access::scheduler scd (onready)
	 (when (procedure? onready)
	    (onready)))
      (if w
	 (thread-join! (thread-start-joinable! thread))
	 (thread-start! thread))))

;*---------------------------------------------------------------------*/
;*    blacklist-table ...                                              */
;*---------------------------------------------------------------------*/
(define (blacklist-table blacklist::pair-nil)
   (when (pair? blacklist)
      (let ((table (create-hashtable :size 512  :weak 'open-string)))
	 (for-each (lambda (ip)
		      (hashtable-put! table ip #t))
	    blacklist)
	 table)))

;*---------------------------------------------------------------------*/
;*    socket-blacklist? ...                                            */
;*---------------------------------------------------------------------*/
(define (socket-blacklist? sock::socket table)
   (when table
      (hashtable-get table (socket-host-address sock))))

;*---------------------------------------------------------------------*/
;*    notify-reject ...                                                */
;*---------------------------------------------------------------------*/
(define (notify-reject sock)
   (trace 1 -1 "REJECT"
      ": " (socket-host-address sock)
      " [" (current-date) "]"))

