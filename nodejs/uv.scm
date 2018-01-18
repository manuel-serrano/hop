;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/nodejs/uv.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 14 05:42:05 2014                          */
;*    Last change :  Thu Jan 18 09:17:44 2018 (serrano)                */
;*    Copyright   :  2014-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    NodeJS libuv binding                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_uv

   (library hop hopscript)

   (cond-expand
      (enable-libuv
       (library libuv)))

   (include "nodejs_debug.sch" "nodejs_async.sch" "uv.sch" "nodejs_types.sch")

   (cond-expand
      (enable-libuv
       (static (class JsLoop::UvLoop
		  (async (default #f))
		  (async-count::int (default 0))
		  (async-count-debug::pair-nil (default '()))
		  (actions::pair-nil (default '()))
		  (exiting::bool (default #f)))
	  
	  (class JsChild::UvProcess
	     (ref (default #t))
	     (detached (default #f)))
	  
	  (class JsPipe::UvPipe
	     ;; count are used to decrement the uv-async globa counter
	     ;; because when uv-close is invoked with an input pipe
	     ;; the read-start called is never invoked and then, it
	     ;; has not chance to decrement the counter for itself.
	     (count::int (default 0))
	     (econnreset::bool (default #f)))))
      (else
       (static (class JsLoop))))

   (cond-expand
      (enable-libuv
       (import __nodejs_process
	  __nodejs__process-wrap
	  __nodejs__pipe-wrap
	  __nodejs__buffer)))
   
   (export (!js-callback0 ::obj ::WorkerHopThread ::JsGlobalObject ::JsFunction ::obj)
	   (!js-callback1 ::obj ::WorkerHopThread ::JsGlobalObject ::JsFunction ::obj ::obj)
	   (!js-callback2 ::obj ::WorkerHopThread ::JsGlobalObject ::JsFunction ::obj ::obj ::obj)
	   (!js-callback3 ::obj ::WorkerHopThread ::JsGlobalObject ::JsFunction ::obj ::obj ::obj ::obj)
	   (!js-callback4 ::obj ::WorkerHopThread ::JsGlobalObject ::JsFunction ::obj ::obj ::obj ::obj ::obj)
	   (!js-callback5 ::obj ::WorkerHopThread ::JsGlobalObject ::JsFunction ::obj ::obj ::obj ::obj ::obj ::obj)

	   (nodejs-uv-version::bstring)
	   (nodejs-err-name ::int)
	   (nodejs-process-title-init!)
	   (nodejs-get-process-title::bstring)
	   (nodejs-set-process-title! ::bstring)
	   (nodejs-now ::WorkerHopThread)

	   (nodejs-close ::WorkerHopThread ::JsGlobalObject ::JsObject ::JsObject ::obj)
	   (nodejs-ref ::obj ::WorkerHopThread)
	   (nodejs-unref ::obj ::WorkerHopThread)
	   
	   (nodejs-make-timer ::WorkerHopThread ::JsGlobalObject ::JsObject ::JsObject)
	   (nodejs-timer-start ::WorkerHopThread ::obj ::obj ::obj)
	   (nodejs-timer-close ::WorkerHopThread ::obj)
	   (nodejs-timer-stop ::WorkerHopThread ::obj)
	   (nodejs-timer-unref ::WorkerHopThread ::obj)
	   (nodejs-hrtime::uint64)
	   (nodejs-uptime::uint64 ::WorkerHopThread)

	   (nodejs-make-fs-event ::WorkerHopThread)
	   (nodejs-fs-event-start ::obj ::procedure ::bstring)
	   (nodejs-fs-event-stop ::obj)
	   (nodejs-fs-event-change)
	   (nodejs-fs-event-rename)

	   (nodejs-make-fs-poll ::WorkerHopThread)
	   (nodejs-fs-poll-start ::JsGlobalObject ::JsObject ::obj ::bstring ::procedure ::int)
	   (nodejs-fs-poll-stop ::obj)

	   (nodejs-make-idle ::WorkerHopThread ::JsGlobalObject ::procedure)
	   (nodejs-idle-start ::WorkerHopThread ::JsGlobalObject ::obj)
	   (nodejs-idle-stop ::WorkerHopThread ::JsGlobalObject ::obj)
	   
	   (nodejs-check?::bool ::obj)
	   (nodejs-make-check ::WorkerHopThread ::JsGlobalObject ::JsObject)
	   (nodejs-check-stop ::WorkerHopThread ::JsGlobalObject ::obj)
	   
	   (nodejs-loadavg ::u8vector)
	   (nodejs-getfreemem::double)
	   (nodejs-gettotalmem::double)
	   (nodejs-getresidentmem::long)
	   (nodejs-getcpus::vector)
	   (nodejs-exepath::bstring)
	   (nodejs-getuptime::double)
	   (nodejs-kill ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj)

;* 	   (nodejs-need-tick-callback ::WorkerHopThread ::JsGlobalObject ::JsObject) */
	   
	   (nodejs-rename-file ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj ::obj)
	   (nodejs-ftruncate ::WorkerHopThread ::JsGlobalObject ::JsObject ::int ::obj ::obj)
	   (nodejs-truncate ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::int ::obj)
	   (nodejs-fchown ::WorkerHopThread ::JsGlobalObject ::JsObject ::int ::int ::int ::obj)
	   (nodejs-chown ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::int ::int ::obj)
	   (nodejs-lchown ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::int ::int ::obj)
	   (nodejs-fchmod ::WorkerHopThread ::JsGlobalObject ::JsObject ::int ::int ::obj)
	   (nodejs-chmod ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::int ::obj)
	   (nodejs-lchmod ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::int ::obj)
	   (nodejs-stat ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj ::obj)
	   (nodejs-fstat ::WorkerHopThread ::JsGlobalObject ::JsObject ::int ::obj ::obj)
	   (nodejs-lstat ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj ::obj)
	   (nodejs-link ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj ::obj)
	   (nodejs-symlink ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj ::obj)
	   (nodejs-readlink ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj)
	   (nodejs-unlink ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj)
	   (nodejs-rmdir ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj)
	   (nodejs-fdatasync ::WorkerHopThread ::JsGlobalObject ::JsObject ::int ::obj)
	   (nodejs-mkdir ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::int ::obj)
	   (nodejs-open ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::long ::long ::obj)
	   (nodejs-utimes ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj ::obj ::obj)
	   (nodejs-futimes ::WorkerHopThread ::JsGlobalObject ::JsObject ::int ::obj ::obj ::obj)
	   (nodejs-fsync ::WorkerHopThread ::JsGlobalObject ::JsObject ::int ::obj)
	   (nodejs-write ::WorkerHopThread ::JsGlobalObject ::JsObject ::int ::obj ::long ::long ::obj ::obj)
	   (nodejs-read ::WorkerHopThread ::JsGlobalObject ::JsObject ::int ::obj ::long ::long ::obj ::obj)
	   (nodejs-fs-close ::WorkerHopThread ::JsGlobalObject ::JsObject ::int ::obj)

	   (nodejs-getaddrinfo ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::int)
	   (nodejs-query ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::int ::JsObject)
	   (nodejs-isip ::obj)

	   (nodejs-istty ::WorkerHopThread ::JsGlobalObject ::obj)
	   (nodejs-guess-handle-type ::WorkerHopThread ::JsGlobalObject ::obj)
	   
	   (nodejs-tcp-handle ::WorkerHopThread)
	   (nodejs-stream-write-queue-size::long ::obj)
	   (nodejs-stream-fd::long ::WorkerHopThread ::obj)
	   (nodejs-tcp-connect ::WorkerHopThread ::JsGlobalObject ::obj ::obj ::int ::int ::procedure)
	   (nodejs-tcp-nodelay ::obj ::bool)
	   (nodejs-tcp-keepalive ::obj ::bool ::long)
	   (nodejs-tcp-simultaneous-accepts ::obj ::bool)
	   (nodejs-tcp-getsockname ::JsGlobalObject ::obj)
	   (nodejs-tcp-getpeername ::JsGlobalObject ::obj)
	   (nodejs-tcp-open ::WorkerHopThread ::JsGlobalObject ::obj ::int)
	   (nodejs-tcp-bind ::JsGlobalObject ::JsObject ::obj ::obj ::int ::int)
	   (nodejs-tcp-listen ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj ::int ::procedure)


	   (nodejs-tty-handle ::WorkerHopThread ::int ::bool)
	   (nodejs-tty-set-raw-mode ::obj)
	   (nodejs-tty-get-window-size ::WorkerHopThread ::JsGlobalObject ::obj)
	   
	   (nodejs-udp-handle ::WorkerHopThread)
	   (nodejs-udp-bind ::JsGlobalObject ::JsObject ::obj ::obj ::int ::int ::int)
	   (nodejs-udp-send ::WorkerHopThread ::JsGlobalObject ::obj ::bstring ::long ::long ::long ::bstring ::int ::procedure)
	   (nodejs-udp-recv-start ::WorkerHopThread ::JsGlobalObject ::obj ::procedure ::obj)
	   (nodejs-udp-recv-stop ::obj)
	   (nodejs-udp-getsockname ::JsGlobalObject ::obj)
	   (nodejs-udp-set-ttl ::obj ::int)
	   (nodejs-udp-set-multicast-ttl ::obj ::int)
	   (nodejs-udp-set-multicast-loop ::obj ::obj)
	   (nodejs-udp-set-broadcast ::obj ::obj)
	   (nodejs-udp-set-membership ::obj ::bstring ::obj ::symbol)
	   
	   (nodejs-stream-write ::WorkerHopThread ::JsGlobalObject ::obj ::bstring ::long ::long ::procedure)
	   (nodejs-stream-write2 ::WorkerHopThread ::JsGlobalObject ::obj ::bstring ::long ::long ::obj ::procedure)
	   (nodejs-stream-read-start ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::procedure ::obj)
	   (nodejs-stream-read-stop ::WorkerHopThread ::JsGlobalObject ::obj)
	   (nodejs-stream-shutdown ::WorkerHopThread ::JsGlobalObject ::obj ::procedure)

	   (nodejs-new-process)
	   (nodejs-process-spawn ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj)
	   (nodejs-process-kill ::WorkerHopThread ::JsGlobalObject ::JsObject ::JsObject ::int)
	   (nodejs-new-pipe ::WorkerHopThread ::bool)
	   (nodejs-pipe-ipc?::bool ::obj)
	   (nodejs-pipe-accept::obj ::WorkerHopThread ::JsGlobalObject ::obj ::obj)
	   (nodejs-pipe-open ::WorkerHopThread ::JsGlobalObject ::obj ::int)
	   (nodejs-pipe-bind ::JsGlobalObject ::JsObject ::obj ::obj)
	   (nodejs-pipe-connect ::WorkerHopThread ::JsGlobalObject ::obj ::obj ::procedure)
	   (nodejs-pipe-listen ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj ::int)
	   ))

(define-expander assert-thread
   (lambda (x e)
      (cond-expand
	 (enable-threads (e `(assert ,@(cdr x)) e))
	 (else #unspecified))))

(cond-expand
   (enable-libuv
;;;

;*---------------------------------------------------------------------*/
;*    Constants                                                        */
;*---------------------------------------------------------------------*/
(define ENOENT
   (cond-expand
      ((and bigloo-c enable-libuv) (pragma::long "ENOENT")) (else 2)))

(define EBADF
   (cond-expand
      ((and bigloo-c enable-libuv) (pragma::long "EBADF")) (else 9)))

;*---------------------------------------------------------------------*/
;*    next-tick ...                                                    */
;*---------------------------------------------------------------------*/
(define (next-tick %worker %this)
   (with-access::WorkerHopThread %worker (%process async)
      (with-access::JsProcess %process (tick-callback)
	 (unless tick-callback
	    (set! tick-callback (js-get %process '_tickCallback %this)))
	 (js-call0 %this tick-callback (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    !js-callback0 ...                                                */
;*---------------------------------------------------------------------*/
(define (!js-callback0 name %worker %this proc obj)
   (with-access::WorkerHopThread %worker (call)
      (let ((res (call (lambda () (js-call0 %this proc obj)))))
	 (next-tick %worker %this)
	 res)))

;*---------------------------------------------------------------------*/
;*    !js-callback1 ...                                                */
;*---------------------------------------------------------------------*/
(define (!js-callback1 name %worker %this proc obj arg0)
   (with-access::WorkerHopThread %worker (call)
      (let ((res (call (lambda () (js-call1 %this proc obj arg0)))))
	 (next-tick %worker %this)
	 res)))

;*---------------------------------------------------------------------*/
;*    !js-callback2 ...                                                */
;*---------------------------------------------------------------------*/
(define (!js-callback2 name %worker %this proc obj arg0 arg1)
   (with-access::WorkerHopThread %worker (call)
      (let ((res (call (lambda () (js-call2 %this proc obj arg0 arg1)))))
	 (next-tick %worker %this)
	 res)))

;*---------------------------------------------------------------------*/
;*    !js-callback3 ...                                                */
;*---------------------------------------------------------------------*/
(define (!js-callback3 name %worker %this proc obj arg0 arg1 arg2)
   (with-access::WorkerHopThread %worker (call)
      (let ((res (call (lambda () (js-call3 %this proc obj arg0 arg1 arg2)))))
	 (next-tick %worker %this)
	 res)))

;*---------------------------------------------------------------------*/
;*    !js-callback4 ...                                                */
;*---------------------------------------------------------------------*/
(define (!js-callback4 name %worker %this proc obj arg0 arg1 arg2 arg3)
   (with-access::WorkerHopThread %worker (call)
      (let ((res (call (lambda () (js-call4 %this proc obj arg0 arg1 arg2 arg3)))))
	 (next-tick %worker %this)
	 res)))

;*---------------------------------------------------------------------*/
;*    !js-callback5 ...                                                */
;*---------------------------------------------------------------------*/
(define (!js-callback5 name %worker %this proc obj arg0 arg1 arg2 arg3 arg4)
   (with-access::WorkerHopThread %worker (call)
      (let ((res (call (lambda () (js-call5 %this proc obj arg0 arg1 arg2 arg3 arg4)))))
	 (next-tick %worker %this)
	 res)))

;*---------------------------------------------------------------------*/
;*    to-uint64 ...                                                    */
;*---------------------------------------------------------------------*/
(define (to-uint64 n)
   (cond
      ((fixnum? n) (fixnum->uint64 n))
      ((not (flonum? n)) #u64:0)
      (else (llong->uint64 (flonum->llong n)))))

;*---------------------------------------------------------------------*/
;*    to-int64 ...                                                     */
;*---------------------------------------------------------------------*/
(define (to-int64 %this proc offset default)
   (cond
      ((fixnum? offset)
       (fixnum->int64 offset))
      ((flonum? offset)
       (if (integer? offset)
	   (flonum->int64 offset)
	   (js-raise-type-error %this "Not an integer" offset)))
      ((or (eq? offset (js-undefined)) (null? offset))
       default)
      (else
       (js-raise-range-error %this
	  (format "~a: too large offset (~a)" proc (typeof offset)) offset))))

;*---------------------------------------------------------------------*/
;*    nodejs-uv-version ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-uv-version::bstring)
   (uv-version))

;*---------------------------------------------------------------------*/
;*    nodejs-err-name ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-err-name::obj errno)
   (js-string->jsstring (uv-err-name errno)))

;*---------------------------------------------------------------------*/
;*    nodejs-process-title-init! ...                                   */
;*---------------------------------------------------------------------*/
(define (nodejs-process-title-init!)
   (uv-process-title-init!))
   
;*---------------------------------------------------------------------*/
;*    nodejs-get-process-title ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-get-process-title::bstring)
   (uv-get-process-title))

;*---------------------------------------------------------------------*/
;*    nodejs-set-process-title! ...                                    */
;*---------------------------------------------------------------------*/
(define (nodejs-set-process-title! str)
   (uv-set-process-title! str))

;*---------------------------------------------------------------------*/
;*    worker-loop ...                                                  */
;*---------------------------------------------------------------------*/
(define (worker-loop th::WorkerHopThread)
   (with-access::WorkerHopThread th (%loop)
      (unless %loop
	 (tprint "LACKING LOOP th=" th " " (getpid)))
      %loop))

;*---------------------------------------------------------------------*/
;*    process-fail ...                                                 */
;*---------------------------------------------------------------------*/
(define (process-fail %this process errno)
   (js-put! process 'errno errno #f %this)
   (js-put! process '_errno (js-string->jsstring (uv-err-name errno)) #f %this)
   #f)

;*---------------------------------------------------------------------*/
;*    js-worker-tick ...                                               */
;*---------------------------------------------------------------------*/
(define-method (js-worker-tick th::WorkerHopThread)
   (with-access::WorkerHopThread th (%loop %process %retval
				       call keep-alive mutex)
      (with-access::JsLoop %loop (async actions exiting)
	 (let loop ((acts (synchronize mutex
			     (let ((acts actions))
				(set! actions '())
				(reverse! acts)))))
	    (with-handler
	       (lambda (e)
		  (let ((r (js-worker-exception-handler th e 8)))
		     (if (=fx r 0)
			 (begin
			    (loop acts)
			    (set! exiting #t))
			 (begin
			    (set! %retval r)
			    (set! keep-alive #f)))))
	       (let loop ()
		  (when (pair? acts)
		     (let* ((action (car acts))
			    (actname (car action))
			    (actproc (cdr action)))
			(set! acts (cdr acts))
			(with-trace 'nodejs-async actname
			   (call actproc)))
		     (loop))))))))

;*---------------------------------------------------------------------*/
;*    js-worker-init! ::WorkerHopThread ...                            */
;*---------------------------------------------------------------------*/
(define-method (js-worker-init! th::WorkerHopThread)
   (with-access::WorkerHopThread th (%loop tqueue)
      (set! %loop
	 (instantiate::JsLoop
	    (actions tqueue)))))
   
;*---------------------------------------------------------------------*/
;*    js-worker-loop ::WorkerHopThread ...                             */
;*    -------------------------------------------------------------    */
;*    Overrides the generic functions defined in hopscript/worker      */
;*    to let LIBUV manages the event loop.                             */
;*---------------------------------------------------------------------*/
(define-method (js-worker-loop th::WorkerHopThread)
   (with-access::WorkerHopThread th (mutex condv tqueue
				       %process %this keep-alive services
				       call %retval prerun %loop)
      (letrec* ((loop %loop)
		(async (instantiate::UvAsync
			  (loop loop)
			  (cb (lambda (a)
				 (js-worker-tick th)
				 (with-access::JsLoop loop (actions)
				    (unless (or keep-alive
						(pair? services)
						(pair? actions))
				       (uv-unref async)
				       (when (js-totest (js-get %process '_exiting %this))
					  (uv-stop loop)))))))))
	 (synchronize mutex
	    (nodejs-process th %this)
	    (with-access::JsLoop loop (actions)
	       (set! actions tqueue))
	    (condition-variable-broadcast! condv)
	    (with-access::JsLoop loop ((lasync async))
	       (set! lasync async))
	    [assert-thread (th) (eq? th (current-thread))]
	    (unless (>=fx (bigloo-debug) 2)
	       (with-access::WorkerHopThread th (%this)
		  (signal sigsegv
		     (lambda (x)
			(js-raise-range-error %this
			   "Maximum call stack size exceeded" #f)))))
	    (when (pair? tqueue)
	       (uv-async-send async)))
	 (unwind-protect
	    (with-access::WorkerHopThread th (onexit %process parent state)
	       (let run ()
		  (with-handler
		     (lambda (e)
			(set! state 'error)
			(with-handler
			   (lambda (e)
			      (set! %retval 8))
			   (set! %retval (js-worker-exception-handler th e 8)))
			;; run one more for nexttick
			(with-access::JsLoop loop (exiting)
			   (set! exiting (not (=fx %retval 0))))
			(uv-async-send async)
			(run))
		     (uv-run loop)))
	       ;; call the cleanup function
	       (when (=fx %retval 0)
		  (unless (js-totest (js-get %process '_exiting %this))
		     (with-handler
			(lambda (e)
			   (exception-notify e)
			   (set! %retval 8))
			(when (isa? onexit JsFunction)
			   (js-put! %process '_exiting #t #f %this)
			   (js-call1 %this onexit %process %retval)))))
	       ;; when the parent died, kill the application
	       (unless parent
		  (exit %retval)))
	    (with-access::WorkerHopThread th (services subworkers)
	       ;; unregister all the worker services
	       (for-each unregister-service! services)
	       ;; tell the subworkers that they will never receive
	       ;; any new message from their parent
	       (for-each (lambda (w)
			    (with-access::WorkerHopThread w (mutex keep-alive)
			       (synchronize mutex
				  (set! keep-alive #f)
				  (js-worker-push-thunk! w "ping"
				     (lambda ()
					(uv-async-send async))))))
		  subworkers))))))

;*---------------------------------------------------------------------*/
;*    js-worker-terminate! ::WorkerHopThread ...                       */
;*---------------------------------------------------------------------*/
(define-method (js-worker-terminate! th::WorkerHopThread pred)
   (js-worker-push-thunk! th "stop"
      (lambda ()
	 (with-access::WorkerHopThread th (keep-alive parent exitlisteners)
	    (when (pair? exitlisteners)
	       (js-worker-push-thunk! parent "slave-terminate"
		  (lambda ()
		     (let ((e (instantiate::MessageEvent
				 (name "exit")
				 (target (js-undefined))
				 (data (js-undefined)))))
			(apply-listeners exitlisteners e)))))
	    (set! keep-alive #f))
	 (uv-stop (worker-loop th)))))

;*---------------------------------------------------------------------*/
;*    js-worker-push-thunk! ::WorkerHopThread ...                      */
;*---------------------------------------------------------------------*/
(define-method (js-worker-push-thunk! th::WorkerHopThread name::bstring thunk::procedure)
   
   (define (worker-started? th)
      (with-access::WorkerHopThread th (%loop mutex tqueue)
	 (when %loop
	    (with-access::JsLoop %loop (async)
	       async))))
   
   (with-trace 'nodejs-async "nodejs-async-push"
      (trace-item "name=" name)
      (trace-item "th=" th)
      (with-access::WorkerHopThread th (%loop mutex tqueue)
	 (synchronize mutex
	    (let ((act (cons name thunk)))
	       (if (worker-started? th)
		   (with-access::JsLoop %loop (actions async exiting)
		      (unless (pair? actions)
			 (uv-ref async)
			 (uv-async-send async))
		      ;; push the action to be executed (with a debug name)
		      (set! actions (cons act actions)))
		   ;; the loop is not started yet (this might happend when
		   ;; a master send a message (js-worker-post-master-message)
		   ;; before the slave is fully initialized
		   (with-access::WorkerHopThread th (tqueue)
		      (set! tqueue (append (cons act tqueue))))))))))

;*---------------------------------------------------------------------*/
;*    js-worker-exec ...                                               */
;*---------------------------------------------------------------------*/
(define-method (js-worker-exec th::WorkerHopThread name::bstring thunk::procedure)
   (if (eq? (current-thread) th)
       (thunk)
       (let ((response #f)
	     (mutex (make-mutex))
	     (condv (make-condition-variable))
	     (exn (make-cell #f)))
	  (synchronize mutex
	     (js-worker-push-thunk! th name
		(lambda ()
		   (set! response
		      (with-handler
			 (lambda (e)
			    (cell-set! exn e)
			    exn)
			 (thunk)))
		   (synchronize mutex
		      (condition-variable-signal! condv))))
	     (condition-variable-wait! condv mutex)
	     (if (eq? response exn)
		 (raise (cell-ref exn))
		 response)))))

;*---------------------------------------------------------------------*/
;*    nodejs-now ...                                                   */
;*---------------------------------------------------------------------*/
(define (nodejs-now %worker)
   (uv-update-time (worker-loop %worker))
   (uint64->flonum (uv-now (worker-loop %worker))))

(define close-stack '())

;*---------------------------------------------------------------------*/
;*    nodejs-close ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-close %worker %this process this callback)
   (with-access::JsHandle this (handle flags)
      (cond
	 ((or (not (isa? handle JsPipe))
	      (with-access::JsPipe handle (econnreset)
		 (not econnreset)))
	  (set! close-stack (cons this close-stack))
	  (with-access::UvHandle handle (onclose)
	     (uv-close handle
		(lambda ()
		   (set! close-stack (remq! this close-stack))
		   (when (and (=fx (bit-and flags 1) 1)
			      (isa? callback JsFunction))
		      (!js-callback0 "close" %worker %this
			 callback (js-undefined))))))
	  (set! flags (bit-or flags 1)))
;* 		   (js-worker-push-thunk! %worker "close"              */
;* 		      (lambda ()                                       */
;* 			 (when (isa? callback JsFunction)              */
;* 			    (js-call0 %this callback (js-undefined))))))))) */
	 (else
	  (when (isa? callback JsFunction)
	     (!js-callback0 'close %worker %this
		callback (js-undefined)))))))
   
;*---------------------------------------------------------------------*/
;*    nodejs-ref ...                                                   */
;*---------------------------------------------------------------------*/
(define (nodejs-ref obj %worker)
   (uv-ref obj))
   
;*---------------------------------------------------------------------*/
;*    nodejs-unref ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-unref obj %worker)
   (uv-unref obj))

;*---------------------------------------------------------------------*/
;*    nodejs-make-timer ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-make-timer %worker %this process obj)
   
   (define (timer-body timer status)
      (with-access::UvTimer timer (repeat)
	 (with-trace 'nodejs-async "nodejs-timer-callback"
	    (trace-item "timer-"
	       (integer->string (uv-id timer) 16)
	       " repeat=" repeat)
	    (let ((proc (js-get obj 'ontimeout %this)))
	       (when (isa? proc JsFunction)
		  (js-worker-push-thunk! %worker "tick-spinner"
                     (lambda ()
                        (js-call1 %this proc obj status))))))))

   (instantiate::UvTimer
      (loop (worker-loop %worker))
      (cb (lambda (timer status)
	     (timer-body timer status)))))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-start ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-start %worker timer start rep)
   (with-trace 'nodejs-async "nodejs-timer-start (pre)"
      (uv-timer-start timer (to-uint64 start) (to-uint64 rep))))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-close ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-close %worker timer)
   ;; protect against multi-close (see nodejs/src/handle_wrap.cc:88)
   (with-trace 'nodejs-async "nodejs-timer-close (pre)"
      (trace-item "timer-" (integer->string (uv-id timer) 16))
      (uv-close timer)))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-stop ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-stop %worker timer)
   (with-trace 'nodejs-async "nodejs-timer-stop (pre)"
      (trace-item "timer-" (integer->string (uv-id timer) 16))
      (uv-timer-stop timer)))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-unref ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-unref %worker timer)
   (with-trace 'nodejs-async "nodejs-timer-unref (pre)"
      (trace-item "timer-" (integer->string (uv-id timer) 16)
	 " " (typeof timer))
      (uv-unref timer)))

;*---------------------------------------------------------------------*/
;*    nodejs-hrtime ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-hrtime::uint64)
   (uv-hrtime))

;*---------------------------------------------------------------------*/
;*    nodejs-uptime ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-uptime::uint64 %worker)
   (let ((loop (worker-loop %worker)))
      (uv-update-time loop)
      (uv-now loop)))

;*---------------------------------------------------------------------*/
;*    nodejs-make-fs-event ...                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-make-fs-event %worker)
   (instantiate::UvFsEvent (loop (worker-loop %worker))))

;*---------------------------------------------------------------------*/
;*    nodejs-fs-event-start ...                                        */
;*---------------------------------------------------------------------*/
(define (nodejs-fs-event-start hdl proc path)
   (uv-fs-event-start hdl proc path))

;*---------------------------------------------------------------------*/
;*    nodejs-fs-event-stop ...                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-fs-event-stop hdl)
   (uv-fs-event-stop hdl))

;*---------------------------------------------------------------------*/
;*    nodejs-fs-event-change ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-fs-event-change)
   (uv-fs-event-change))

;*---------------------------------------------------------------------*/
;*    nodejs-fs-event-rename ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-fs-event-rename)
   (uv-fs-event-rename))

;*---------------------------------------------------------------------*/
;*    nodejs-make-fs-poll ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-make-fs-poll %worker)
   (instantiate::UvFsPoll (loop (worker-loop %worker))))

;*---------------------------------------------------------------------*/
;*    nodejs-fs-poll-start ...                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-fs-poll-start %this proto hdl path proc interval)
   (uv-fs-poll-start hdl
      (lambda (this status prev curr)
	 (proc this status
	    (stat->jsobj %this proto prev)
	    (stat->jsobj %this proto curr)))
      path interval))

;*---------------------------------------------------------------------*/
;*    nodejs-fs-poll-stop ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-fs-poll-stop hdl)
   (uv-fs-poll-stop hdl))

;*---------------------------------------------------------------------*/
;*    nodejs-make-idle ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-make-idle %worker %this callback)
   (instantiate::UvIdle
      (loop (worker-loop %worker))
      (cb callback)))

;*---------------------------------------------------------------------*/
;*    nodejs-idle-start ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-idle-start %worker %this obj)
   [assert-thread (%worker) (eq? %worker (current-thread))]
   (uv-idle-start obj))

;*---------------------------------------------------------------------*/
;*    nodejs-idle-stop ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-idle-stop %worker %this obj)
   [assert-thread (%worker) (eq? %worker (current-thread))]
   (uv-idle-stop obj))

;*---------------------------------------------------------------------*/
;*    nodejs-check? ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-check?::bool obj)
   (isa? obj UvCheck))

;*---------------------------------------------------------------------*/
;*    nodejs-make-check ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-make-check %worker %this process)
   [assert-thread (%worker) (eq? %worker (current-thread))]
   (let ((check (instantiate::UvCheck
		   (loop (worker-loop %worker))
		   (cb (lambda (_)
			  (let ((cb (js-get process '_immediateCallback %this)))
			     (when (isa? cb JsFunction)
				(!js-callback0 '_immedateCallback %worker %this
				   cb (js-undefined)))))))))
      (uv-check-start check)
      check))
   
;*---------------------------------------------------------------------*/
;*    nodejs-check-stop ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-check-stop %worker %this obj)
   [assert-thread (%worker) (eq? %worker (current-thread))]
   (uv-check-stop obj))
   
;*---------------------------------------------------------------------*/
;*    nodejs-loadavg ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-loadavg vec::u8vector)
   ($uv-loadavg ($u8vector->double* vec 0)))
   
;*---------------------------------------------------------------------*/
;*    nodejs-getfreemem ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-getfreemem::double)
   (uv-get-free-memory))

;*---------------------------------------------------------------------*/
;*    nodejs-gettotalmem ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-gettotalmem::double)
   (uv-get-total-memory))

;*---------------------------------------------------------------------*/
;*    nodejs-getresidentmem ...                                        */
;*---------------------------------------------------------------------*/
(define (nodejs-getresidentmem::long)
   (uv-get-resident-memory))

;*---------------------------------------------------------------------*/
;*    nodejs-getcpus ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-getcpus::vector)
   (uv-cpus))

;*---------------------------------------------------------------------*/
;*    nodejs-exepath ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-exepath::bstring)
   (or (hop-exepath) (uv-exepath)))

;*---------------------------------------------------------------------*/
;*    nodejs-getuptime ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-getuptime::double)
   (uv-uptime))

;*---------------------------------------------------------------------*/
;*    nodejs-kill ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-kill %worker %this process pid sig)
   (let* ((pid (js-tointeger pid %this))
	  (sig (int32->fixnum (js-toint32 sig %this)))
	  (r (uv-kill (if (flonum? pid) (flonum->fixnum pid) pid) sig)))
      (if (=fx r 0)
	  (js-undefined)
	  (begin
	     (process-fail %this process r)
	     r))))

;*---------------------------------------------------------------------*/
;*    not-implemented-exn ...                                          */
;*---------------------------------------------------------------------*/
(define (not-implemented-exn fun %this)
   (with-access::JsGlobalObject %this (js-error)
      (js-new %this js-error
	 (js-string->jsstring (format "~a not implemented" fun)))))

;*---------------------------------------------------------------------*/
;*    fs-exn ...                                                       */
;*---------------------------------------------------------------------*/
(define (fs-exn fmt obj %this)
   (with-access::JsGlobalObject %this (js-error)
      (js-new %this js-error (js-string->jsstring (format fmt obj)))))

;*---------------------------------------------------------------------*/
;*    fs-errno-exn ...                                                 */
;*---------------------------------------------------------------------*/
(define (fs-errno-exn fmt::bstring errno %this)
   (with-access::JsGlobalObject %this (js-error)
      (let* ((ename (uv-err-name errno))
	     (msg (format fmt (uv-strerror errno)))
	     (obj (js-new %this js-error
		     (if (string? ename)
			 (js-stringlist->jsstring (list ename ", " msg))
			 (js-string->jsstring msg)))))
	 (js-put! obj 'errno errno #f %this)
	 (js-put! obj 'code (js-string->jsstring ename) #f %this)
	 obj)))

;*---------------------------------------------------------------------*/
;*    fs-errno-path-exn ...                                            */
;*---------------------------------------------------------------------*/
(define (fs-errno-path-exn fmt::bstring errno %this path)
   (let ((exn (fs-errno-exn fmt errno %this)))
      (when (js-jsstring? path)
	 (js-put! exn 'path path #t %this))
      exn))

;*---------------------------------------------------------------------*/
;*    fs-callback ...                                                  */
;*---------------------------------------------------------------------*/
(define (fs-callback %worker %this process callback name fmt res #!optional (ok '()))
   (cond
      ((not (integer? res))
       (!js-callback1 (string->symbol name) %worker %this
	  callback (js-undefined) res))
      ((=fx res 0)
       (!js-callback1 (string->symbol name) %worker %this
	  callback (js-undefined) ok))
      (else
       (let ((exn (fs-errno-exn fmt res %this)))
	  (!js-callback1 (string->symbol name) %worker %this
	     callback (js-undefined) exn)))))

;*---------------------------------------------------------------------*/
;*    fs-callback-error ...                                            */
;*---------------------------------------------------------------------*/
(define (fs-callback-error %worker %this name callback . args)
   (with-access::JsGlobalObject %this (js-error)
      (let ((err (js-new %this js-error
		    (js-string->jsstring (format "EBADF, ~a" name)))))
	 (js-put! err 'errno EBADF #f %this)
	 (js-put! err 'code  (js-string->jsstring "EBADF") #f %this)
	 (if (isa? callback JsFunction)
	     (js-worker-push-thunk! %worker name
		(lambda ()
		   (js-apply %this callback (js-undefined) (cons err args))
		   (next-tick %worker %this)))
	     (js-raise err)))))
       
;*---------------------------------------------------------------------*/
;*    nodejs-rename-file ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-rename-file %worker %this process oldp newp callback)
   
   (define (rename-callback res)
      (fs-callback %worker %this process callback "rename-file"
	 (format "rename: cannot rename file ~s into ~s -- ~~s"
	    (js-jsstring->string oldp) (js-jsstring->string newp))
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-rename (js-jsstring->string oldp) (js-jsstring->string newp)
	  :callback rename-callback
	  :loop (worker-loop %worker))
       (let ((r (uv-fs-rename (js-jsstring->string oldp) (js-jsstring->string newp))))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "rename: cannot rename file ~a into ~a"
		       (js-jsstring->string oldp) (js-jsstring->string newp))
		    r %this))
	      r))))

;*---------------------------------------------------------------------*/
;*    nodejs-ftruncate ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-ftruncate %worker %this process fd offset callback)
   
   (define (ftruncate-callback res)
      (fs-callback %worker %this process callback "ftruncate"
	 (format "ftruncate: cannot truncate ~a to ~a -- ~~s" fd offset)
	 res))
   
   (let ((file (int->uvhandle %worker %this fd))
	 (off::int64 (to-int64 %this "ftruncate" offset #s64:0)))
      (if file
	  (if (isa? callback JsFunction)
	      (uv-fs-ftruncate file off
		 :callback ftruncate-callback
		 :loop (worker-loop %worker))
	      (uv-fs-ftruncate file off))
	  (fs-callback-error %worker %this callback "ftruncate"))))

;*---------------------------------------------------------------------*/
;*    nodejs-truncate ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-truncate %worker %this process path offset callback)

   (define (truncate-callback res)
      (fs-callback %worker %this process callback "truncate"
	 (format "truncate: cannot truncate ~a to ~a -- ~~s" (js-jsstring->string path) offset)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-truncate (js-jsstring->string path) offset
	  :callback truncate-callback
	  :loop (worker-loop %worker))
       (uv-fs-truncate (js-jsstring->string path) offset)))

;*---------------------------------------------------------------------*/
;*    nodejs-fchown ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-fchown %worker %this process fd uid guid callback)
   
   (define (fchown-callback res)
      (fs-callback %worker %this process callback "fchown"
	 (format "fchown: cannot chown ~a, ~a, ~a -- ~~s" fd uid guid)
	 res))
   
   (let ((file (int->uvhandle %worker %this fd)))
      (if file
	  (if (isa? callback JsFunction)
	      (uv-fs-fchown file uid guid
		 :callback fchown-callback
		 :loop (worker-loop %worker))
	      (uv-fs-fchown file uid guid))
	  (fs-callback-error %worker %this callback "fchown"))))

;*---------------------------------------------------------------------*/
;*    nodejs-chown ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-chown %worker %this process path uid guid callback)

   (define (fchown-callback res)
      (fs-callback %worker %this process callback "chown"
	 (format "chown: cannot chown ~a, ~a, ~a -- ~~s" (js-jsstring->string path) uid guid)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-chown (js-jsstring->string path) uid guid
	  :callback fchown-callback
	  :loop (worker-loop %worker))
       (uv-fs-chown (js-jsstring->string path) uid guid)))

;*---------------------------------------------------------------------*/
;*    nodejs-lchown ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-lchown %worker %this process path uid guid callback)
   
   (define (lchown-callback res)
      (fs-callback %worker %this process callback "lchown"
	 (format "lchown: cannot chown ~a, ~a, ~a -- ~~s" (js-jsstring->string path) uid guid)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-lchown (js-jsstring->string path) uid guid
	  :callback lchown-callback
	  :loop (worker-loop %worker))
       (uv-fs-lchown (js-jsstring->string path) uid guid)))

;*---------------------------------------------------------------------*/
;*    nodejs-fchmod ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-fchmod %worker %this process fd mod callback)

   (define (fchmod-callback res)
      (fs-callback %worker %this process callback "fchmod"
	 (format "fchmod: cannot chmod ~a, ~a -- ~~s" fd mod)
	 res))
   
   (let ((file (int->uvhandle %worker %this fd)))
      (if file
	  (if (isa? callback JsFunction)
	      (uv-fs-fchmod file mod
		 :callback fchmod-callback
		 :loop (worker-loop %worker))
	      (let ((r (uv-fs-fchmod file mod)))
		 (if (and (integer? r) (<fx r 0))
		     (js-raise
			(fs-errno-exn
			   (format "fchmod: cannot chmod ~a ~a" fd mod)
			   r %this))
		     r)))
	  (fs-callback-error %worker %this callback "fchmod"))))

;*---------------------------------------------------------------------*/
;*    nodejs-chmod ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-chmod %worker %this process path mod callback)

   (define (chmod-callback res)
      (fs-callback %worker %this process callback "chmod"
	 (format "chmod: cannot chmod ~a, ~a -- ~~s" (js-jsstring->string path) mod)
	 res))

   (if (isa? callback JsFunction)
       (uv-fs-chmod (js-jsstring->string path) mod
	  :callback chmod-callback
	  :loop (worker-loop %worker))
       (let ((r (uv-fs-chmod (js-jsstring->string path) mod)))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "chmod: cannot chmod ~a ~a" (js-jsstring->string path) mod)
		    r %this))
	      r))))

;*---------------------------------------------------------------------*/
;*    nodejs-lchmod ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-lchmod %worker %this process fd mod callback)
   
   (define (lchmod-callback res)
      (fs-callback %worker %this process callback "lchmod"
	 (format "lchmod: cannot chmod ~a, ~a -- ~~s" (js-jsstring->string fd) mod)
	 res))
   
   (lchmod-callback (not-implemented-exn "lchmod" %this)))

;*---------------------------------------------------------------------*/
;*    nodejs-open ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-open %worker %this process path flags mode callback)
   
   (define (open-callback res)
      (if (isa? res UvFile)
	  (!js-callback2 'open %worker %this callback (js-undefined) #f
	     (uvfile->int %worker res))
	  (let ((exn (fs-errno-exn
			(format "open '~a'" (js-jsstring->string path))
			res %this)))
	     (js-put! exn 'path path #f %this)
	     (!js-callback2 'open %worker %this
		callback (js-undefined) exn #f))))

   (if (isa? callback JsFunction)
       (uv-fs-open (js-jsstring->string path) flags :mode mode
	  :loop (worker-loop %worker)
	  :callback open-callback)
       (let ((res (uv-fs-open (js-jsstring->string path) flags :mode mode)))
	  (if (isa? res UvFile)
	      (uvfile->int %worker res)
	      (let ((exn (fs-errno-exn
			    (format "open '~a'" (js-jsstring->string path))
			    res %this)))
		 (js-put! exn 'path path #f %this)
		 (js-raise exn))))))

;*---------------------------------------------------------------------*/
;*    nodejs-fs-close ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-fs-close %worker %this process fd callback)
   (let ((file (int->uvhandle %worker %this fd)))
      (if file
	  (begin
	     (close-uvfile %worker fd)
	     (if (isa? callback JsFunction)
		 (uv-fs-close file
		    :loop (worker-loop %worker)
		    :callback
		    (lambda (val)
		       (!js-callback1 'close %worker %this
			  callback (js-undefined) val)))
		 (uv-fs-close file)))
	  (fs-callback-error %worker %this callback "close"))))

;*---------------------------------------------------------------------*/
;*    stat-date ...                                                    */
;*---------------------------------------------------------------------*/
(define (stat-date stat %this)
   (for-each (lambda (k)
		(let ((c (assq k stat)))
		   (when (pair? c)
		      (set-cdr! c (js-date->jsdate
				     (seconds->date (cdr c))
				     %this)))))
      '(mtime atime ctime))
   stat)

;*---------------------------------------------------------------------*/
;*    stat->jsobj ...                                                  */
;*---------------------------------------------------------------------*/
(define (stat->jsobj %this proto res)
   (let ((stat (js-alist->jsobject (stat-date res %this) %this)))
      (with-access::JsObject stat (__proto__)
	 (set! __proto__ proto))
      stat))
   
;*---------------------------------------------------------------------*/
;*    stat-cb ...                                                      */
;*---------------------------------------------------------------------*/
(define (stat-cb %worker %this process callback name obj proto lbl path)
   (lambda (res)
      (if (integer? res)
	  (!js-callback2 'stat %worker %this
	     callback (js-undefined)
	     (fs-errno-path-exn
		(format "~a: cannot stat ~a -- ~~s" name obj)
		res %this path)
	     #f)
	  (let ((jsobj (stat->jsobj %this proto res)))
	     (!js-callback2 'stat %worker %this
		callback (js-undefined) #f jsobj)))))

;*---------------------------------------------------------------------*/
;*    nodejs-fstat ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-fstat %worker %this process fd callback proto)
   (let ((file (int->uvhandle %worker %this fd)))
      (if file
	  (if (isa? callback JsFunction)
	      (let ((lbl (string-append "fstat:" (integer->string fd))))
		 (uv-fs-fstat file
		    :loop (worker-loop %worker)
		    :callback (stat-cb %worker %this process callback
				 "fstat" fd proto lbl #f)))
	      (let ((res (uv-fs-fstat file)))
		 (if (integer? res)
		     (js-raise
			(fs-errno-exn
			   (format "stat: cannot stat ~a -- ~~s" fd)
			   res %this))
		     (stat->jsobj %this proto res))))
	  (fs-callback-error %worker %this callback "fstat" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-stat ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-stat %worker %this process path callback proto)
   (if (isa? callback JsFunction)
       (let ((lbl (string-append "stat:" (js-jsstring->string path))))
	  (uv-fs-stat (js-jsstring->string path)
	     :loop (worker-loop %worker)
	     :callback (stat-cb %worker %this process callback "stat"
			  (js-jsstring->string path) proto lbl path)))
       (let ((res (uv-fs-stat (js-jsstring->string path))))
	  (if (integer? res)
	      (js-raise
		 (fs-errno-path-exn
		    (format "stat: cannot stat ~a -- ~~s"
		       (js-jsstring->string path))
		    res %this path))
	      (stat->jsobj %this proto res)))))

;*---------------------------------------------------------------------*/
;*    nodejs-lstat ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-lstat %worker %this process path callback proto)
   (if (isa? callback JsFunction)
       (let ((lbl (string-append "lstat:" (js-jsstring->string path))))
	  (uv-fs-lstat (js-jsstring->string path)
	     :loop (worker-loop %worker)
	     :callback (stat-cb %worker %this process callback "lstat"
			  path proto lbl path)))
       (let ((res (uv-fs-lstat (js-jsstring->string path))))
	  (if (integer? res)
	      (js-raise
		 (fs-errno-exn (format "lstat: cannot stat ~a -- ~~s"
				  (js-jsstring->string path))
		    res %this))
	      (stat->jsobj %this proto res)))))

;*---------------------------------------------------------------------*/
;*    nodejs-link ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-link %worker %this process src dst callback)
   
   (define (link-callback res)
      (fs-callback %worker %this process callback "link"
	 (format "link: cannot link ~a, ~a -- ~~s"
	    (js-jsstring->string src) (js-jsstring->string dst))
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-link (js-jsstring->string src) (js-jsstring->string dst)
	  :loop (worker-loop %worker)
	  :callback link-callback)
       (let ((r (uv-fs-link (js-jsstring->string src) (js-jsstring->string dst))))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "link: cannot lin ~a ~a"
		       (js-jsstring->string src) (js-jsstring->string dst))
		    r %this))
	      r))))

;*---------------------------------------------------------------------*/
;*    nodejs-symlink ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-symlink %worker %this process src dst callback)
   
   (define (symlink-callback res)
      (fs-callback %worker %this process callback "symlink"
	 (format "symlink: cannot link ~a, ~a -- ~~s"
	    (js-jsstring->string src) (js-jsstring->string dst))
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-symlink (js-jsstring->string src) (js-jsstring->string dst)
	  :loop (worker-loop %worker)
	  :callback symlink-callback)
       (uv-fs-symlink (js-jsstring->string src) (js-jsstring->string dst))))

;*---------------------------------------------------------------------*/
;*    nodejs-readlink ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-readlink %worker %this process src callback)
   
   (define (readlink-callback res)
      (if (integer? res)
	  (!js-callback2 'readlink %worker %this
	     callback (js-undefined)
	     (fs-errno-exn (format "readlink: ~~a ~s"
			      (js-jsstring->string src))
		res %this)
	     (js-undefined))
	  (!js-callback2 'readlink %worker %this
	     callback (js-undefined) '()
	     (js-string->jsstring res))))
   
   (if (isa? callback JsFunction)
       (uv-fs-readlink (js-jsstring->string src)
	  :loop (worker-loop %worker)
	  :callback readlink-callback)
       (let ((r (uv-fs-readlink (js-jsstring->string src))))
	  (if (and (integer? r) (<fx r 0))
	      (with-access::JsGlobalObject %this (js-error)
		 (let ((exn (js-new %this js-error
			       (js-string->jsstring
				  (format "readlink: cannot read link ~a"
				     (js-jsstring->string src))))))
		    (js-put! exn 'errno r #f %this)
		    (js-raise exn)))
	      (js-string->jsstring r)))))

;*---------------------------------------------------------------------*/
;*    nodejs-unlink ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-unlink %worker %this process src callback)
   
   (define (unlink-callback res)
      (fs-callback %worker %this process callback "unlink"
	 (format "unlink: cannot unlink ~a -- ~~s" src)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-unlink (js-jsstring->string src)
	  :loop (worker-loop %worker)
	  :callback unlink-callback)
       (let ((r (uv-fs-unlink (js-jsstring->string src))))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "unlink: cannot unlink ~a" (js-jsstring->string src))
		    r %this))
	      r)
	  r)))

;*---------------------------------------------------------------------*/
;*    nodejs-rmdir ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-rmdir %worker %this process src callback)
   
   (define (rmdir-callback res)
      (fs-callback %worker %this process callback "rmdir"
	 (format "rmdir: cannot rmdir ~a -- ~~s" (js-jsstring->string src))
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-rmdir (js-jsstring->string src)
	  :loop (worker-loop %worker)
	  :callback rmdir-callback)
       (let ((r (uv-fs-rmdir (js-jsstring->string src))))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "rmdir: cannot rmdir ~a" (js-jsstring->string src))
		    r %this))
	      r))))

;*---------------------------------------------------------------------*/
;*    nodejs-fdatasync ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-fdatasync %worker %this process fd callback)
   
   (define (datasync-callback res)
      (fs-callback %worker %this process callback "datasync"
	 (format "datasync: cannot datasync ~a -- ~~s" fd)
	 res))
   
   (let ((file (int->uvhandle %worker %this fd)))
      (if file
	  (if (isa? callback JsFunction)
	      (uv-fs-fdatasync file
		 :loop (worker-loop %worker)
		 :callback datasync-callback)
	      (let ((r (uv-fs-fdatasync file)))
		 (if (and (integer? r) (<fx r 0))
		     (js-raise
			(fs-errno-exn
			   (format "datasync: cannot datasync ~a" fd)
			   r %this))
		     r)))
	  (fs-callback-error %worker %this "fdatasync" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-mkdir ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-mkdir %worker %this process path mode callback)
   
   (define (mkdir-callback res)
      (cond
	 ((not (integer? res))
	  (!js-callback1 'mkdir %worker %this callback (js-undefined) res))
	 ((=fx res 0)
	  (!js-callback1 'mkdir %worker %this callback (js-undefined) '()))
	 (else
	  (let ((exn (fs-errno-path-exn
			(format "mkdir: cannot mkdir ~a -- ~~s"
			   (js-jsstring->string path))
			res %this path)))
	     (!js-callback1 'mkdir %worker %this
		callback (js-undefined) exn)))))
   
   (if (isa? callback JsFunction)
       (uv-fs-mkdir (js-jsstring->string path) mode
	  :loop (worker-loop %worker)
	  :callback mkdir-callback)
       (let ((r (uv-fs-mkdir (js-jsstring->string path) mode)))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "mkdir: cannot mkdir ~a" (js-jsstring->string path))
		    r %this))
	      r))))

;*---------------------------------------------------------------------*/
;*    nodejs-write ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-write %worker %this process fd buffer offset length position callback)
   (let ((file (int->uvhandle %worker %this fd)))
      (if file
	  (if (isa? callback JsFunction)
	      (with-access::JsArrayBufferView buffer (%data byteoffset)
		 (uv-fs-write (int->uvhandle %worker %this fd) %data length
		    :callback (lambda (obj)
				 (if (<fx obj 0)
				     (!js-callback3 'write %worker %this
					callback (js-undefined)
					obj #f buffer)
				     (!js-callback3 'write %worker %this
					callback (js-undefined)
					#f obj buffer)))
		    :offset (+fx offset (uint32->fixnum byteoffset))
		    :position (to-int64 %this "write" position #s64:-1)
		    :loop (worker-loop %worker)))
	      (with-access::JsArrayBufferView buffer (%data byteoffset)
		 (uv-fs-write (int->uvhandle %worker %this fd) %data length
		    :offset (+fx offset (uint32->fixnum byteoffset))
		    :position (to-int64 %this "write" position #s64:-1)
		    :loop (worker-loop %worker))))
	  (fs-callback-error %worker %this "write" callback #f buffer))))

;*---------------------------------------------------------------------*/
;*    nodejs-read ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-read %worker %this process fd buffer offset length position callback)
   (let ((file (int->uvhandle %worker %this fd)))
      (if file
	  (with-access::JsArrayBufferView buffer (%data byteoffset)
	     (if (isa? callback JsFunction)
		 (uv-fs-read file %data length
		    :callback
		    (lambda (obj)
		       (if (<fx obj 0)
			   (!js-callback2 'read %worker %this
			      callback (js-undefined) obj #f)
			   (!js-callback2 'read %worker %this
			      callback (js-undefined) #f obj)))
		    :offset (+fx offset (uint32->fixnum byteoffset))
		    :position (to-int64 %this "read" position #s64:-1)
		    :loop (worker-loop %worker))
		 (uv-fs-read file %data length
			     :offset (+fx offset (uint32->fixnum byteoffset))
			     :position (to-int64 %this "read" position #s64:-1))))
	  (fs-callback-error %worker %this "read" callback #f))))

;*---------------------------------------------------------------------*/
;*    js-todouble ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-todouble num %this)
   (let ((num (js-tonumber num %this)))
      (cond
	 ((flonum? num)
	  num)
	 ((fixnum? num)
	  (fixnum->flonum num))
	 (else
	  0.0))))

;*---------------------------------------------------------------------*/
;*    nodejs-utimes ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-utimes %worker %this process path atime mtime callback)
   
   (define (utimes-callback res)
      (fs-callback %worker %this process callback "utimes"
	 (format "utimes: cannot utimes ~a -- ~~s" (js-jsstring->string path))
	 res (js-undefined)))
   
   (if (isa? callback JsFunction)
       (uv-fs-utime (js-jsstring->string path) (js-todouble atime %this)
	  (js-todouble mtime %this)
	  :loop (worker-loop %worker)
	  :callback utimes-callback)
       (let ((res (uv-fs-utime (js-jsstring->string path)
		     (js-todouble atime %this)
		     (js-todouble mtime %this))))
	  (if (=fx res 0)
	      res
	      (js-raise (fs-errno-exn "~a" res %this))))))

;*---------------------------------------------------------------------*/
;*    nodejs-futimes ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-futimes %worker %this process fd atime mtime callback)
   
   (define (utimes-callback res)
      (fs-callback %worker %this process callback "futimes"
	 (format "futimes: cannot utimes ~a -- ~~s" fd)
	 res (js-undefined)))
   
   (let ((file (int->uvhandle %worker %this fd)))
      (if file
	  (if (isa? callback JsFunction)
	      (uv-fs-futime file (js-todouble atime %this)
		 (js-todouble mtime %this)
		 :loop (worker-loop %worker)
		 :callback utimes-callback)
	      (let ((res (uv-fs-futime file (js-todouble atime %this)
			    (js-todouble mtime %this))))
		 (if (=fx res 0)
		     res
		     (js-raise (fs-errno-exn "~a" res %this)))))
	  (fs-callback-error %worker %this "futimes" callback))))

;*---------------------------------------------------------------------*/
;*    nodejs-fsync ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-fsync %worker %this process fd callback)
   
   (define (fsync-callback res)
      (fs-callback %worker %this process callback "fsync"
	 (format "fsync: cannot fsync ~a -- ~~s" fd)
	 res))
   
   (let ((file (int->uvhandle %worker %this fd)))
      (if file
	  (if (isa? callback JsFunction)
	      (uv-fs-fsync file
		 :loop (worker-loop %worker)
		 :callback fsync-callback)
	      (uv-fs-fsync file))
	  (fs-callback-error %worker %this "fsync" callback))))

;*---------------------------------------------------------------------*/
;*    nodejs-getaddrinfo ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-getaddrinfo %worker %this process node family)
   [assert-thread (%worker) (eq? (current-thread) %worker)]
   (with-access::JsGlobalObject %this (js-object)
      (let ((wrap (js-new %this js-object)))
	 (uv-getaddrinfo (js-jsstring->string node) #f
	    :family family
	    :loop (worker-loop %worker)
	    :callback
	    (lambda (res)
	       (let ((oncomplete (js-get wrap 'oncomplete %this)))
		  (if (isa? oncomplete JsFunction)
		      (if (pair? res)
			  (!js-callback1 'getaddrinfo %worker %this
			     oncomplete (js-undefined)
			     (js-vector->jsarray
				(list->vector (map! js-string->jsstring res))
				%this))
			  (begin
			     (process-ares-fail %this process res)
			     (!js-callback1 'getaddrinfo %worker %this
				oncomplete (js-undefined)
				(js-undefined))))))))
	 wrap)))

;*---------------------------------------------------------------------*/
;*    nodejs-query ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-query %worker %this process node family cb)

   (define (query-callback res)
      (if (pair? res)
	  (let ((v (js-vector->jsarray
		      (list->vector (map! js-string->jsstring res)) %this)))
	     (!js-callback2 'query %worker %this cb (js-undefined) #f v))
	  (!js-callback2 'query %worker %this cb (js-undefined) res '#())))
   
   [assert-thread () (isa? (current-thread) WorkerHopThread)]
   (with-access::JsGlobalObject %this (js-object)
      (let ((res (uv-getaddrinfo (js-jsstring->string node) #f :family family
		    :loop (worker-loop %worker)
		    :callback query-callback)))
	 (if (=fx res 0)
	     (js-new %this js-object)
	     (process-ares-fail %this process res)))))

;*---------------------------------------------------------------------*/
;*    nodejs-isip ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-isip addr)
   (cond
      ((uv-inet-pton (js-jsstring->string addr) :family 4) 4)
      ((uv-inet-pton (js-jsstring->string addr) :family 6) 6)
      (else 0)))

;*---------------------------------------------------------------------*/
;*    nodejs-istty ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-istty %worker %this fd)
   (let ((file (int->uvhandle %worker %this fd)))
      (when file
	 (eq? (uv-guess-handle fd) 'TTY))))

;*---------------------------------------------------------------------*/
;*    nodejs-guess-handle-type ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-guess-handle-type %worker %this fd)
   (let ((file (int->uvhandle %worker %this fd)))
      (when file
	 (js-string->jsstring (symbol->string (uv-guess-handle fd))))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-handle ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-handle %worker)
   (instantiate::UvTcp
      (loop (worker-loop %worker))))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-write-queue-size ...                               */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-write-queue-size::long hdl)
   (uv-stream-write-queue-size hdl))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-fd ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-fd::long %worker hdl)
   (let ((fd (uv-stream-fd hdl)))
      (when (>=fx fd 0) (store-stream-fd! %worker hdl fd))
      fd))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-connect ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-connect %worker %this handle host port family callback)
   [assert-thread (%worker) (eq? (current-thread) %worker)]
   ;; (tprint "tcp-connect host=" host " port=" port " family=" family)
   (uv-tcp-connect handle (js-jsstring->string host) port :family family
      :loop (worker-loop %worker)
      :callback (lambda (status handle)
		   ;; (tprint "tcp-connect, AfterConnect status=" status)
		   (js-worker-push-thunk! %worker "tcp-connect"
		      (lambda ()
			 (callback status handle))))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-nodelay ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-nodelay handle enable)
   (uv-tcp-nodelay handle enable))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-keepalive ...                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-keepalive handle enable timeout)
   (uv-tcp-keepalive handle enable timeout))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-simultaneous-accepts ...                              */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-simultaneous-accepts handle enable)
   (uv-tcp-simultaneous-accepts handle enable))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-getsockname ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-getsockname %this handle)
   (let ((res (uv-tcp-getsockname handle)))
      (if (integer? res)
	  res
	  (js-alist->jsobject res %this))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-getpeername ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-getpeername %this handle)
   (let ((res (uv-tcp-getpeername handle)))
      (if (integer? res)
	  res
	  (js-alist->jsobject res %this))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-open ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-open %worker %this handle fd)
   (let ((r (uv-tcp-open handle fd)))
      (store-stream-fd! %worker handle r)
      r))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-bind ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-bind %this process handle addr port family)
   (let ((r (uv-tcp-bind handle (js-jsstring->string addr) port :family family)))
      (if (=fx r 0)
	  r
	  (begin
	     (process-fail %this process r)
	     -1))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-listen ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-listen %worker %this process this handle backlog tcp-wrap)
   ;; (tprint "tcp-listen")
   [assert-thread (%worker) (eq? (current-thread) %worker)]
   (let ((r (uv-listen handle backlog
	       :loop (worker-loop %worker)
	       :callback
	       (lambda (server status)
		  ;; (tprint "tcp-listen, OnConnection status=" status)
		  (if (< status 0)
		      (process-fail %this process status)
		      (with-access::UvTcp server (loop)
			 (let ((client (instantiate::UvTcp (loop loop))))
			    (let ((r (uv-accept handle client)))
			       (if (< r 0)
				   (process-fail %this process r)
				   (let ((onconn (js-get this 'onconnection %this)))
				      (!js-callback1 'listen %worker %this
					 onconn this
					 (tcp-wrap client))))))))))))
      (when (<fx r 0)
	 (process-fail %this process r))
      r))
   
;*---------------------------------------------------------------------*/
;*    nodejs-tty-handle ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-tty-handle %worker fd readable)
   (instantiate::UvTty
      (loop (worker-loop %worker))
      (fd fd)
      (readable readable)))

;*---------------------------------------------------------------------*/
;*    nodejs-tty-set-raw-mode ...                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-tty-set-raw-mode handle)
   (uv-tty-mode-set! handle 'raw))

;*---------------------------------------------------------------------*/
;*    nodejs-tty-get-window-size ...                                   */
;*---------------------------------------------------------------------*/
(define (nodejs-tty-get-window-size %worker %this handle)
   (js-vector->jsarray (uv-tty-get-window-size handle) %this))
	   
;*---------------------------------------------------------------------*/
;*    nodejs-udp-handle ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-udp-handle %worker)
   (instantiate::UvUdp
      (loop (worker-loop %worker))))

;*---------------------------------------------------------------------*/
;*    nodejs-udp-bind ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-udp-bind %this process handle addr port family flags)
   (let ((r (uv-udp-bind handle (js-jsstring->string addr) port
	       :family family :flags flags)))
      (if (=fx r 0)
	  r
	  (begin
	     (process-fail %this process r)
	     r))))

;*---------------------------------------------------------------------*/
;*    nodejs-udp-send ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-udp-send %worker %this handle buffer offset length port address family callback)
   [assert-thread (%worker) (eq? %worker (current-thread))]
   (uv-udp-send handle buffer offset length port address
      :family family
      :loop (worker-loop %worker)
      :callback callback))

;*---------------------------------------------------------------------*/
;*    nodejs-udp-recv-start ...                                        */
;*---------------------------------------------------------------------*/
(define (nodejs-udp-recv-start %worker %this handle onalloc callback)
   [assert-thread (%worker) (eq? %worker (current-thread))]
   (uv-udp-recv-start handle
      :onalloc onalloc
      :loop (worker-loop %worker)
      :callback callback))

;*---------------------------------------------------------------------*/
;*    nodejs-udp-recv-stop ...                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-udp-recv-stop handle)
   (uv-udp-recv-stop handle))

;*---------------------------------------------------------------------*/
;*    nodejs-udp-getsockname ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-udp-getsockname %this handle)
   (let ((res (uv-udp-getsockname handle)))
      (if (integer? res)
	  res
	  (js-alist->jsobject res %this))))

;*---------------------------------------------------------------------*/
;*    nodejs-udp-set-ttl ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-udp-set-ttl handle ttl)
   (uv-udp-set-ttl handle ttl))

;*---------------------------------------------------------------------*/
;*    nodejs-udp-set-multicast-ttl ...                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-udp-set-multicast-ttl handle ttl)
   (uv-udp-set-multicast-ttl handle ttl))

;*---------------------------------------------------------------------*/
;*    nodejs-udp-set-multicast-loop ...                                */
;*---------------------------------------------------------------------*/
(define (nodejs-udp-set-multicast-loop handle on)
   (uv-udp-set-multicast-loop handle on))

;*---------------------------------------------------------------------*/
;*    nodejs-udp-set-broadcast ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-udp-set-broadcast handle on)
   (uv-udp-set-broadcast handle on))

;*---------------------------------------------------------------------*/
;*    nodejs-udp-set-membership ...                                    */
;*---------------------------------------------------------------------*/
(define (nodejs-udp-set-membership handle addr iface action)
   (uv-udp-set-membership handle addr iface action))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-write ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-write %worker %this handle buffer offset length callback)
   [assert-thread (%worker) (eq? %worker (current-thread))]
;*    (tprint "WriteBuffer offset=" offset " length=" length)          */
   (uv-stream-write handle buffer offset length
      :loop (worker-loop %worker)
      :callback callback))
;*       :callback (lambda (status)                                    */
;* 		   ;; (tprint "AfterWrite status=" status)             */
;* 		   ;; CARE: MS 2 may 2015                              */
;* 		   (js-worker-push-thunk! %worker "stream-write"       */
;* 		     (lambda ()                                        */
;* 			(callback status)))                            */
;* 		   (callback status)                                   */
;* 		   (js-worker-tick %worker))))                         */
   
;*---------------------------------------------------------------------*/
;*    nodejs-stream-write2 ...                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-write2 %worker %this handle buffer offset length sendhandle callback)
;*    (tprint "WriteBuffer2 offset=" offset " length=" length)         */
   (uv-stream-write2 handle buffer offset length sendhandle
      :loop (worker-loop %worker)
      :callback callback))
;*       :callback (lambda (status)                                    */
;* 		   (js-worker-push-thunk! %worker "stream-write"       */
;* 		      (lambda ()                                       */
;* 			 (callback status)))                           */
   
;*---------------------------------------------------------------------*/
;*    nodejs-stream-read-start ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-read-start %worker %this process handle onalloc callback)
   [assert-thread (%worker) (or (not (current-thread)) (eq? %worker (current-thread)))]
   (uv-stream-read-start handle
      :onalloc onalloc
      :loop (worker-loop %worker)
      :callback callback))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-read-stop ...                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-read-stop %worker %this handle)
   (uv-stream-read-stop handle))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-shutdown ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-shutdown %worker %this handle callback)
   [assert-thread (%worker) (eq? %worker (current-thread))]
   (uv-stream-shutdown handle
      :loop (worker-loop %worker)
      :callback callback))
;*       :callback (lambda (status handle)                             */
;* 		   ;; CARE: MS 2 may 2015                              */
;* 		   (js-worker-push-thunk! %worker "stream-shutdown"    */
;* 		      (lambda ()                                       */
;* 			 (callback status handle)))                    */


;*---------------------------------------------------------------------*/
;*    store-stream-fd! ...                                             */
;*---------------------------------------------------------------------*/
(define (store-stream-fd! %worker hdl fd::int)
   (with-access::WorkerHopThread %worker (uvhandles)
      (when (>=fx fd (vector-length uvhandles))
	 (let ((new (make-vector (*fx 2 fd))))
	    (vector-copy! new 0 uvhandles)
	    (set! uvhandles new)))
      (vector-set! uvhandles fd hdl)))
   
;*---------------------------------------------------------------------*/
;*    uvfile->int ...                                                  */
;*---------------------------------------------------------------------*/
(define (uvfile->int %worker file)
   (with-access::UvFile file (fd)
      (store-stream-fd! %worker file fd)
      fd))

;*---------------------------------------------------------------------*/
;*    int->uvhandle ...                                                */
;*---------------------------------------------------------------------*/
(define (int->uvhandle %worker %this fd::int)
   (with-access::WorkerHopThread %worker (uvhandles)
      (cond
	 ((<fx fd 0) #f)
	 ((>fx fd (vector-length uvhandles)) #f)
	 (else
	  (let ((hdl (vector-ref uvhandles fd)))
	     (cond
		((and hdl (not (eq? hdl (js-undefined))))
		 hdl)
		((<=fx fd 2)
		 ;; initialize lazily stdio
		 (let ((file (instantiate::UvFile
				(fd fd)
				(path ""))))
		    (store-stream-fd! %worker file fd)
		    file))
		(else 
		 ;; initialize lazily, other fd as some nodejs examples,
		 ;; cite FD ex-nihilo
		 ;; see simple/test-listen-fd-cluster.js
		 (let ((file (instantiate::UvFile
				(fd fd)
				(path ""))))
		    (store-stream-fd! %worker file fd)
		    file))))))))
;* 		(else                                                  */
;* 		 (tprint "PAS de FD: " fd " pid=" (getpid))            */
;* 		 (js-raise-type-error %this                            */
;* 		    (format "Illegal descriptor (~a) ~~a" fd) fd)))))))) */

;*---------------------------------------------------------------------*/
;*    close-uvfile ...                                                 */
;*---------------------------------------------------------------------*/
(define (close-uvfile %worker fd::int)
   (with-access::WorkerHopThread %worker (uvhandles)
      (vector-set! uvhandles fd #f)))

;*---------------------------------------------------------------------*/
;*    nodejs-new-process ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-new-process)
   (instantiate::JsChild))

;*---------------------------------------------------------------------*/
;*    nodejs-process-spawn ...                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-process-spawn %worker %this %process process options)
   
   (define (signal->string sig)
      (cond
	 ((=fx sig sighup) (js-string->jsstring "SIGHUP"))
	 ((=fx sig sigfpe) (js-string->jsstring "SIGFPE"))
	 ((=fx sig sigill) (js-string->jsstring "SIGILL"))
	 ((=fx sig sigbus) (js-string->jsstring "SIGBUS"))
	 ((=fx sig sigsegv) (js-string->jsstring "SIGSEGV"))
	 ((=fx sig sigpipe) (js-string->jsstring "SIGPIPE"))
	 ((=fx sig sigterm) (js-string->jsstring "SIGTERM"))
	 ((=fx sig sigint) (js-string->jsstring "SIGINT"))
	 ((=fx sig sigkill) (js-string->jsstring "SIGKILL"))
	 ((=fx sig sigabrt) (js-string->jsstring "SIGABRT"))
	 ((=fx sig sigalrm) (js-string->jsstring "SIGALRM"))
	 ((=fx sig sigusr1) (js-string->jsstring "SIGUSR1"))
	 ((=fx sig sigusr2) (js-string->jsstring "SIGUSR2"))
	 (else (js-string->jsstring "SIG???"))))
   
   (define (process-options-stdio-set! opts::UvProcessOptions i hdl)
      (let ((type (js-jsstring->string (js-get hdl 'type %this))))
	 (cond
	    ((string=? type "ignore")
	     (uv-process-options-stdio-container-flags-set! opts i
		(UV-IGNORE)))
	    ((string=? type "pipe")
	     (uv-process-options-stdio-container-flags-set! opts i
		(bit-or
		   (UV-CREATE-PIPE)
		   (bit-or
		      (UV-READABLE-PIPE)
		      (UV-WRITABLE-PIPE))))
	     (with-access::JsHandle (js-get hdl 'handle %this) (handle)
		(uv-process-options-stdio-container-stream-set! opts i
		   handle)))
	    ((string=? type "wrap")
	     (uv-process-options-stdio-container-flags-set! opts i
		(UV-INHERIT-STREAM))
	     (with-access::JsHandle (js-get hdl 'handle %this) (handle)
		(uv-process-options-stdio-container-stream-set! opts i
		   handle)))
	    (else
	     
	     (let ((handle (int->uvhandle %worker %this (js-get hdl 'fd %this))))
		(uv-process-options-stdio-container-flags-set! opts i
		   (UV-INHERIT-FD))
		(uv-process-options-stdio-container-fd-set! opts i handle))))))
   
   (define (onexit this status term)
      (let ((onexit (js-get process 'onexit %this))
	    (status (flonum->fixnum (int64->flonum status))))
	 (with-trace 'nodejs-spawn "process-onexit"
	    (trace-item "status=" status)
	    (when (<fx status 0)
	       (process-fail %this %process status)
	       (set! status -1))
	    (when (isa? onexit JsFunction)
	       (!js-callback2 'onexit %worker %this
		  onexit process
		  status
		  (if (=fx term 0) (js-undefined) (signal->string term)))))))

   (let ((opts (instantiate::UvProcessOptions)))
      (with-access::UvProcessOptions opts ((oflags flags)
					   (ouid uid)
					   (ogid gid)
					   (ofile file)
					   (oargs args)
					   (ocwd cwd)
					   (oenv env))

	 (with-trace 'nodejs-spawn "spawn"
	    ;; options.uid
	    (let ((uid (js-get options 'uid %this)))
	       (when (integer? uid)
		  (trace-item "uid=" uid)
		  (let ((uid (js-toint32 uid %this)))
		     (set! oflags (bit-or oflags (UV-PROCESS-SETUID)))
		     (set! ouid uid))))
	    
	    ;; options.gid
	    (let ((gid (js-get options 'gid %this)))
	       (when (integer? gid)
		  (trace-item "gid=" gid)
		  (let ((gid (js-toint32 gid %this)))
		     (set! oflags (bit-or oflags (UV-PROCESS-SETGID)))
		     (set! ogid gid))))
	    
	    ;; options.file
	    (let ((file (js-get options 'file %this)))
	       (trace-item "file=" file)
	       (unless (js-jsstring? file)
		  (js-raise-type-error %this "Bad argument ~a" file))
	       (set! ofile (js-jsstring->string file)))
	    
	    ;; options.args
	    (let ((args (js-get options 'args %this)))
	       (when (isa? args JsArray)
		  (trace-item "args=" args)
		  (set! oargs
		     (vector-map! (lambda (o) (js-tostring o %this))
			(jsarray->vector args %this)))))
	    
	    ;; options.cwd
	    (let ((cwd (js-get options 'cwd %this)))
	       (when (and (js-jsstring? cwd)
			  (>u32 (js-jsstring-length cwd) #u32:0))
		  (trace-item "cwd=" cwd)
		  (set! ocwd (js-jsstring->string cwd))))
	    
	    ;; options.env
	    (let ((env (js-get options 'envPairs %this)))
	       (when (isa? env JsArray)
		  (trace-item "env=" env)
		  (set! oenv
		     (vector-map! (lambda (o) (js-tostring o %this))
			(jsarray->vector env %this)))))
	    
	    ;; options.stdio
	    (let ((stdios (js-get options 'stdio %this)))
	       (when (isa? stdios JsArray)
		  (trace-item "stdios=" stdios)
		  (let ((len (js-get stdios 'length %this)))
		     (uv-process-options-stdio-container-set! opts len)
		     (let loop ((i 0))
			(when (<fx i len)
			   (process-options-stdio-set! opts i
			      (js-get stdios i %this))
			   (loop (+fx i 1)))))))
	    
	    ;; options.windows_verbatim_arguments
	    (when (js-totest (js-get options 'windowsVerbatimArguments %this))
	       (set! oflags
		  (bit-or oflags (UV-PROCESS-WINDOWS-VERBATIM-ARGUMENTS))))
	    
	    ;; options.detached
	    (when (js-totest (js-get options 'detached %this))
	       (set! oflags (bit-or oflags (UV-PROCESS-DETACHED)))
	       (with-access::JsHandle process (handle)
		  (with-access::JsChild handle (detached)
		     (set! detached #t))))
	    
	    ;; start the process
	    (with-access::JsHandle process (handle)
	       (let ((r (uv-process-spawn handle opts
			   :callback onexit
			   :loop (worker-loop %worker))))
		  (trace-item "r=" r)
		  (case r
		     ((0)
		      (with-access::JsChild handle (pid)
			 (js-put! process 'pid pid #f %this)
			 0))
		     (else
		      (process-fail %this %process r)
		      (js-worker-push-thunk! %worker "spawn-failure"
			 (lambda ()
			    (onexit process (fixnum->int64 r) 0)))
		      0))))))))

;*---------------------------------------------------------------------*/
;*    nodejs-process-kill ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-process-kill %worker %this %process this sig)
   (with-access::JsHandle this (handle)
      (let ((r (uv-process-kill handle sig)))
	 (if (=fx r 0) r (process-fail %this %process r)))))

;*---------------------------------------------------------------------*/
;*    nodejs-new-pipe ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-new-pipe %worker ipc)
   (instantiate::JsPipe
      (count 0)
      (loop (worker-loop %worker))
      (ipc (eq? ipc #t))))

;*---------------------------------------------------------------------*/
;*    nodejs-pipe-ipc? ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-pipe-ipc? pipe)
   (when (isa? pipe UvPipe)
      (and (uv-guess-handle (uv-stream-fd pipe))
	   (uv-pipe-ipc? pipe))))

;*---------------------------------------------------------------------*/
;*    nodejs-pipe-accept ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-pipe-accept %worker %this this pending-type)
   (case pending-type
      ((UDP)
       (with-access::WorkerHopThread %worker (%process)
	  (with-access::JsGlobalObject %this (js-object)
	     (with-access::JsProcess %process (js-udp)
		(let* ((udp (js-new %this js-udp)))
		   (with-access::JsHandle this ((pipe handle))
		      (with-access::JsHandle udp ((client handle))
			 (uv-accept pipe client)))
		   udp)))))
      ((TCP)
       (with-access::WorkerHopThread %worker (%process)
	  (with-access::JsGlobalObject %this (js-object)
	     (with-access::JsProcess %process (js-tcp)
	     (let* ((tcp (js-new %this js-tcp)))
		(with-access::JsHandle this ((pipe handle))
		   (with-access::JsHandle tcp ((client handle))
		      (uv-accept pipe client)))
		tcp)))))
      ((PIPE)
       (with-access::WorkerHopThread %worker (%process)
	  (with-access::JsGlobalObject %this (js-object)
	     (with-access::JsProcess %process (js-pipe)
		(let* ((pipe (js-new %this js-pipe)))
		   (with-access::JsHandle this ((pipe handle))
		      (with-access::JsHandle pipe ((client handle))
			 (uv-accept pipe client)))
		   pipe)))))
      ((UNKNOWN)
       (js-undefined))
      (else
       (error "nodejs-pipe-accept" "Illegal stream type" pending-type))))

;*---------------------------------------------------------------------*/
;*    nodejs-pipe-open ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-pipe-open %worker %this handle fd)
   (let ((r (uv-pipe-open handle fd)))
      (store-stream-fd! %worker handle fd)
      r))

;*---------------------------------------------------------------------*/
;*    nodejs-pipe-bind ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-pipe-bind %this process handle name)
   (let ((r (uv-pipe-bind handle (js-jsstring->string name))))
      (if (=fx r 0)
	  r
	  (begin
	     (process-fail %this process r)
	     r))))

;*---------------------------------------------------------------------*/
;*    nodejs-pipe-connect ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-pipe-connect %worker %this handle name callback)
   ;; (tprint "tcp-listen, pipe-connect")
   [assert-thread (%worker) (eq? (current-thread) %worker)]
   (uv-pipe-connect handle (js-jsstring->string name)
      :loop (worker-loop %worker)
      :callback callback))
;*       :callback (lambda (status handle)                             */
;* 		   ;; (tprint "pipe-connect status=" status)           */
;* 		   (js-worker-push-thunk! %worker "pipe-connect"       */
;* 		      (lambda ()                                       */
;* 			 (callback status handle))))))                 */

;*---------------------------------------------------------------------*/
;*    nodejs-pipe-listen ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-pipe-listen %worker %this process this handle backlog)
   ;; rint "pipe-listen...")
   [assert-thread (%worker) (eq? (current-thread) %worker)]
   (let ((r (uv-listen handle backlog
	       :loop (worker-loop %worker)
	       :callback
	       (lambda (server status)
		  ;; (tprint "pipe-listen listen status=" status)
		  (if (< status 0)
		      (process-fail %this process status)
		      (let ((onconn (js-get this 'onconnection %this)))
			 (!js-callback0 'listen %worker %this onconn this)))))))
      (when (<fx r 0)
	 (process-fail %this process r))	  
      r))

;*---------------------------------------------------------------------*/
;*    cond-expand                                                      */
;*---------------------------------------------------------------------*/
))
