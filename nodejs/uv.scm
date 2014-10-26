;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/uv.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 14 05:42:05 2014                          */
;*    Last change :  Sat Oct 25 18:42:16 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    NodeJS libuv binding                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_uv

   (library hop hopscript libuv)

   (include "nodejs_debug.sch")
   
   (static (class JsLoop::UvLoop
	      (async (default #f))
	      (async-count::int (default 0))
	      (async-count-debug::pair-nil (default '()))
	      (actions::pair-nil (default '())))

	   (class JsChild::UvProcess
	      (ref (default #t))
	      (detached (default #f)))
	   
	   (class JsPipe::UvPipe
	      ;; count are used to decrement the uv-async globa counter
	      ;; because when uv-close is invoked with an input pipe
	      ;; the read-start called is never invoked and then, it
	      ;; has not chance to decrement the counter for itself.
	      (count::int (default 0))
	      (econnreset::bool (default #f))))

   (import __nodejs_process
	   __nodejs__process-wrap
	   __nodejs__pipe-wrap
	   __nodejs__buffer)
   
   (export (nodejs-err-name ::int)
	   (nodejs-now ::WorkerHopThread)

	   (nodejs-close ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj)
	   (nodejs-ref ::obj ::WorkerHopThread)
	   (nodejs-unref ::obj ::WorkerHopThread)
	   
	   (nodejs-make-timer ::WorkerHopThread ::JsGlobalObject ::JsObject)
	   (nodejs-timer-start ::WorkerHopThread ::obj ::obj ::obj)
	   (nodejs-timer-close ::WorkerHopThread ::obj)
	   (nodejs-timer-stop ::WorkerHopThread ::obj)
	   (nodejs-timer-unref ::WorkerHopThread ::obj)

	   (nodejs-make-fs-event ::WorkerHopThread)
	   (nodejs-fs-event-start ::obj ::procedure ::bstring)
	   (nodejs-fs-event-stop ::obj)

	   (nodejs-make-idle ::WorkerHopThread ::JsGlobalObject ::procedure)
	   (nodejs-idle-stop ::WorkerHopThread ::JsGlobalObject ::obj)
	   
	   (nodejs-make-check ::WorkerHopThread ::JsGlobalObject ::JsObject)
	   (nodejs-check-stop ::WorkerHopThread ::JsGlobalObject ::obj)
	   
	   (nodejs-loadavg ::u8vector)
	   (nodejs-getfreemem::double)
	   (nodejs-gettotalmem::double)
	   (nodejs-getcpus::vector)
	   (nodejs-getuptime::double)
	   (nodejs-kill ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj)

	   (nodejs-need-tick-callback ::WorkerHopThread ::JsGlobalObject ::JsObject)
	   
	   (nodejs-rename-file ::WorkerHopThread ::JsGlobalObject ::bstring ::bstring ::obj)
	   (nodejs-ftruncate ::WorkerHopThread ::JsGlobalObject ::int ::int ::obj)
	   (nodejs-truncate ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::obj)
	   (nodejs-fchown ::WorkerHopThread ::JsGlobalObject ::int ::int ::int ::obj)
	   (nodejs-chown ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::int ::obj)
	   (nodejs-lchown ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::int ::obj)
	   (nodejs-fchmod ::WorkerHopThread ::JsGlobalObject ::int ::int ::obj)
	   (nodejs-chmod ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::obj)
	   (nodejs-lchmod ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::obj)
	   (nodejs-stat ::WorkerHopThread ::JsGlobalObject ::bstring ::obj ::obj)
	   (nodejs-fstat ::WorkerHopThread ::JsGlobalObject ::int ::obj ::obj)
	   (nodejs-lstat ::WorkerHopThread ::JsGlobalObject ::bstring ::obj ::obj)
	   (nodejs-link ::WorkerHopThread ::JsGlobalObject ::bstring ::bstring ::obj)
	   (nodejs-symlink ::WorkerHopThread ::JsGlobalObject ::bstring ::bstring ::obj)
	   (nodejs-readlink ::WorkerHopThread ::JsGlobalObject ::bstring ::obj)
	   (nodejs-unlink ::WorkerHopThread ::JsGlobalObject ::bstring ::obj)
	   (nodejs-rmdir ::WorkerHopThread ::JsGlobalObject ::bstring ::obj)
	   (nodejs-fdatasync ::WorkerHopThread ::JsGlobalObject ::int ::obj)
	   (nodejs-mkdir ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::obj)
	   (nodejs-open ::WorkerHopThread ::JsGlobalObject ::bstring ::long ::long ::obj)
	   (nodejs-utimes ::WorkerHopThread ::JsGlobalObject ::bstring ::obj ::obj ::obj)
	   (nodejs-futimes ::WorkerHopThread ::JsGlobalObject ::int ::obj ::obj ::obj)
	   (nodejs-fsync ::WorkerHopThread ::JsGlobalObject ::int ::obj)
	   (nodejs-write ::WorkerHopThread ::JsGlobalObject ::int ::obj ::long ::long ::long ::obj)
	   (nodejs-read ::WorkerHopThread ::JsGlobalObject ::int ::obj ::long ::long ::long ::obj)
	   (nodejs-fs-close ::WorkerHopThread ::JsGlobalObject ::int ::obj)

	   (nodejs-getaddrinfo ::WorkerHopThread ::JsGlobalObject ::JsObject ::bstring ::int)
	   (nodejs-query ::WorkerHopThread ::JsGlobalObject ::JsObject ::bstring ::int ::JsObject)
	   (nodejs-isip ::bstring)

	   (nodejs-istty ::WorkerHopThread ::JsGlobalObject ::obj)
	   (nodejs-guess-handle-type ::WorkerHopThread ::JsGlobalObject ::obj)
	   
	   (nodejs-tcp-handle ::WorkerHopThread)
	   (nodejs-stream-write-queue-size::long ::obj)
	   (nodejs-stream-fd::long ::obj)
	   (nodejs-tcp-connect ::WorkerHopThread ::JsGlobalObject ::obj ::bstring ::int ::int ::procedure)
	   (nodejs-tcp-nodelay ::obj ::bool)
	   (nodejs-tcp-keepalive ::obj ::bool ::long)
	   (nodejs-tcp-simultaneous-accepts ::obj ::bool)
	   (nodejs-tcp-getsockname ::JsGlobalObject ::obj)
	   (nodejs-tcp-getpeername ::JsGlobalObject ::obj)
	   (nodejs-tcp-open ::WorkerHopThread ::JsGlobalObject ::obj ::int)
	   (nodejs-tcp-bind ::JsGlobalObject ::obj ::bstring ::int ::int)
	   (nodejs-tcp-listen ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj ::int ::procedure)

	   (nodejs-stream-write ::WorkerHopThread ::JsGlobalObject ::obj ::bstring ::long ::long ::procedure)
	   (nodejs-stream-read-start ::WorkerHopThread ::JsGlobalObject ::obj ::procedure ::obj)
	   (nodejs-stream-read-stop ::WorkerHopThread ::JsGlobalObject ::obj)
	   (nodejs-stream-shutdown ::WorkerHopThread ::JsGlobalObject ::obj ::procedure)

	   (nodejs-new-process)
	   (nodejs-process-spawn ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj)
	   (nodejs-process-kill ::WorkerHopThread ::JsGlobalObject ::JsObject ::JsObject ::int)
	   (nodejs-new-pipe ::WorkerHopThread ::bool)
	   (nodejs-pipe-open ::WorkerHopThread ::JsGlobalObject ::obj ::int)
	   (nodejs-pipe-bind ::JsGlobalObject ::obj ::bstring)
	   (nodejs-pipe-connect ::WorkerHopThread ::JsGlobalObject ::obj ::bstring ::procedure)
	   (nodejs-pipe-listen ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj ::int)
	   ))

;*---------------------------------------------------------------------*/
;*    Constants                                                        */
;*---------------------------------------------------------------------*/
(define ENOENT
   (cond-expand (bigloo-c (pragma::long "ENOENT")) (else 2)))

(define EBADF
   (cond-expand (bigloo-c (pragma::long "EBADF")) (else 9)))
   
;*---------------------------------------------------------------------*/
;*    nodejs-err-name ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-err-name errno)
   (uv-err-name errno))
	   
;*---------------------------------------------------------------------*/
;*    worker-loop ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (worker-loop::JsLoop th::WorkerHopThread)
   (with-access::WorkerHopThread th (%loop)
      %loop))

;*---------------------------------------------------------------------*/
;*    js-worker-loop ::WorkerHopThread ...                             */
;*    -------------------------------------------------------------    */
;*    Overrides the generic functions defined in hopscript/worker      */
;*    to let LIBUV manages the event loop.                             */
;*---------------------------------------------------------------------*/
(define-method (js-worker-loop th::WorkerHopThread)
   (with-access::WorkerHopThread th (mutex tqueue %this keep-alive services)
      (letrec* ((retval 0)
		(loop (instantiate::JsLoop
			 (actions tqueue)))
		(async (instantiate::UvAsync
			  (loop loop)
			  (cb (lambda (a)
				 (with-handler
				    (lambda (e)
				       (js-worker-exception-handler th e 8))
				    (for-each (lambda (action)
						 (with-trace 'nodejs-async (car action)
						    [assert (th) (eq? th (current-thread))]
						    ((cdr action))))
				       (with-access::WorkerHopThread th (mutex)
					  (synchronize mutex
					     (with-access::JsLoop loop (actions)
						(let ((acts actions))
						   (set! actions '())
						   acts))))))
				 (when (and (not keep-alive) (null? services))
				    (uv-unref async)))))))
	 (synchronize mutex
	    (with-access::JsLoop loop ((lasync async))
	       (set! lasync async))
	    (with-access::WorkerHopThread th (%loop)
	       (set! %loop loop))
	    [assert (th) (eq? th (current-thread))]
	    (with-access::WorkerHopThread th (%this)
	       (signal sigsegv
		  (lambda (x)
		     (js-raise-range-error %this
			"Maximum call stack size exceeded" #f))))
	    (when (pair? tqueue) (uv-async-send async)))
	 (with-handler
	    (lambda (e)
	       (exit (js-worker-exception-handler th e 8)))
	    (unwind-protect
	       (uv-run loop)
	       (with-access::WorkerHopThread th (onexit services subworkers)
		  ;; unregister all the worker services
		  (for-each unregister-service! services)
		  ;; tell the subworkers that they will never receive
		  ;; any message from their parent
		  (for-each (lambda (w)
			       (with-access::WorkerHopThread w (mutex keep-alive)
				  (synchronize mutex
				     (set! keep-alive #f)
				     (js-worker-push-thunk! w "ping"
					(lambda ()
					   (uv-async-send async))))))
		     subworkers)
		  ;; call the cleanup function
		  (when (isa? onexit JsFunction)
		     (js-call1 %this onexit (js-undefined) retval))))))))

;*---------------------------------------------------------------------*/
;*    js-worker-terminate! ::WorkerHopThread ...                       */
;*---------------------------------------------------------------------*/
(define-method (js-worker-terminate! th::WorkerHopThread pred)
   (js-worker-push-thunk! th "stop"
      (lambda ()
	 (with-access::WorkerHopThread th (keep-alive)
	    (set! keep-alive #f))
	 (uv-stop (worker-loop th)))))

;*---------------------------------------------------------------------*/
;*    js-worker-push-thunk! ::WorkerHopThread ...                      */
;*---------------------------------------------------------------------*/
(define-method (js-worker-push-thunk! th::WorkerHopThread name::bstring thunk::procedure)
   (with-trace 'nodejs-async "nodejs-async-push"
      (trace-item "name=" name)
      (trace-item "th=" th)
      (with-access::WorkerHopThread th (%loop mutex tqueue)
	 (if %loop
	     (with-access::JsLoop %loop (actions async)
		(synchronize mutex
		   (uv-ref async)
		   ;; push the action to be executed (with a debug name)
		   (set! actions (append! actions (list (cons name thunk))))
		   ;; tells libuv that there is something to be executed
		   (uv-async-send async)))
	     ;; the loop is not started yet (this might happend when
	     ;; a master send a message (js-worker-post-master-message)
	     ;; before the slave is fully initialized
	     (set! tqueue (append! tqueue (list (cons name thunk))))))))

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

;*---------------------------------------------------------------------*/
;*    nodejs-close ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-close %worker %this this callback)
   (with-access::JsHandle this (handle)
      (cond
	 ((or (not (isa? handle JsPipe))
	      (with-access::JsPipe handle (econnreset)
		 (not econnreset)))
	  (with-access::UvHandle handle (onclose)
	     (uv-close handle
		(lambda ()
		   (when (isa? callback JsFunction)
		      (js-call0 %this callback (js-undefined)))))))
	 (else
	  (when (isa? callback JsFunction)
	     (js-call0 %this callback (js-undefined)))))))
   
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
(define (nodejs-make-timer %worker %this obj)
   
   (define (timer-body timer status)
      (with-access::UvTimer timer (repeat)
	 (with-trace 'nodejs-async "nodejs-timer-callback"
	    (trace-item "timer-"
	       (integer->string (uv-id timer) 16)
	       " repeat=" repeat)
	    (let ((proc (js-get obj 'ontimeout %this)))
;* 	       (when (=u64 repeat #u64:0)                              */
;* 		  (nodejs-timer-unmark! %worker timer))                */
	       (when (isa? proc JsFunction)
		  (js-call1 %this proc obj status))))))
       
   (letrec ((obj (instantiate::UvTimer
		    (loop (worker-loop %worker))
		    (cb (lambda (timer status)
			   (timer-body timer status))))))
      obj))

;* {*---------------------------------------------------------------------*} */
;* {*    nodejs-timer-unmark! ...                                         *} */
;* {*---------------------------------------------------------------------*} */
;* (define (nodejs-timer-unmark! %worker timer)                        */
;*    (with-access::AsTimer timer (asyncmark)                          */
;*       (trace-item "mark=" asyncmark)                                */
;*       (when asyncmark                                               */
;* 	 (set! asyncmark #f)                                           */
;* 	 (uv-async-- %worker (format "timer-~x" (uv-id timer))))))     */

;*---------------------------------------------------------------------*/
;*    nodejs-timer-start ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-start %worker timer start rep)
   
   (define (to-uint64 n)
      (cond
	 ((fixnum? n)
	  (fixnum->uint64 n))
	 ((not (flonum? n))
	  #u64:0)
	 (else
	  (llong->uint64 (flonum->llong n)))))
   
   (define (start-action)
      (uv-timer-start timer (to-uint64 start) (to-uint64 rep)))
   
;*    (with-access::UvTimer timer (asyncmark ref)                      */
      (with-trace 'nodejs-async "nodejs-timer-start (pre)"
;* 	 (trace-item "timer-" (integer->string (uv-id timer) 16)       */
;* 	    " start=" start " rep=" rep " asyncmark=" asyncmark " ref=" ref) */
	 (start-action)))
;* 	 (if (or asyncmark (not ref))                                  */
;* 	     (nodejs-async "timer-start" start-action)                 */
;* 	     (begin                                                    */
;* 		(set! asyncmark #t)                                    */
;* 		(nodejs-async (format "timer-~x" (uv-id timer))        */
;* 		   start-action))))))                                  */

;*---------------------------------------------------------------------*/
;*    nodejs-timer-close ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-close %worker timer)
   ;; protect against multi-close (see nodejs/src/handle_wrap.cc:88)
   (with-trace 'nodejs-async "nodejs-timer-close (pre)"
      (trace-item "timer-" (integer->string (uv-id timer) 16))
      (uv-close timer)))
;*       (nodejs-timer-unmark! %worker timer)                          */
;*       (nodejs-async-push "timer-close"                              */
;* 	 (lambda ()                                                    */
;* 	    (uv-close timer)                                           */
;* 	    (js-worker-push-thunk! %worker "nodejs-timer-close"        */
;* 	       (lambda ()                                              */
;* 		  ;; one for the close                                 */
;* 		  (with-trace 'nodejs-async "nodejs-timer-close"       */
;* 		     (trace-item "timer-" (integer->string (uv-id timer) 16)) */
;* 		     (uv-async-- %worker "timer-close"))))))))         */

;*---------------------------------------------------------------------*/
;*    nodejs-timer-stop ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-stop %worker timer)
   (with-trace 'nodejs-async "nodejs-timer-stop (pre)"
      (trace-item "timer-" (integer->string (uv-id timer) 16))
      (uv-timer-stop timer)))
;*       (nodejs-async-push "timer-stop"                               */
;* 	 (lambda ()                                                    */
;* 	    (uv-timer-stop timer)                                      */
;* 	    (js-worker-push-thunk! %worker "nodejs-timer-stop"         */
;* 	       (lambda ()                                              */
;* 		  (with-trace 'nodejs-async "nodejs-timer-stop"        */
;* 		     (trace-item "timer-" (integer->string (uv-id timer) 16)) */
;* 		     (uv-async-- %worker "timer-stop"))))))))          */

;*---------------------------------------------------------------------*/
;*    nodejs-timer-unref ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-unref %worker timer)
   (with-trace 'nodejs-async "nodejs-timer-unref (pre)"
      (trace-item "timer-" (integer->string (uv-id timer) 16)
	 " " (typeof timer))
      (uv-unref timer)))

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
;*    nodejs-make-idle ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-make-idle %worker %this callback)
   (let ((idle (instantiate::UvIdle
		  (loop (worker-loop %worker))
		  (cb callback))))
      (uv-idle-start idle)
      idle))

;*---------------------------------------------------------------------*/
;*    nodejs-idle-stop ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-idle-stop %worker %this obj)
   [assert (%worker) (eq? %worker (current-thread))]
   (uv-idle-stop obj))

;*---------------------------------------------------------------------*/
;*    nodejs-make-check ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-make-check %worker %this proc)
   [assert (%worker) (eq? %worker (current-thread))]
   (let ((check (instantiate::UvCheck
		   (loop (worker-loop %worker))
		   (cb (lambda (_)
			  (let ((cb (js-get proc '_immediateCallback %this)))
			     (when (isa? cb JsFunction)
				(js-call0 %this cb (js-undefined)))))))))
      (uv-check-start check)
      check))
   
;*---------------------------------------------------------------------*/
;*    nodejs-check-stop ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-check-stop %worker %this obj)
   [assert (%worker) (eq? %worker (current-thread))]
   (uv-check-stop obj))
   
;*---------------------------------------------------------------------*/
;*    nodejs-loadavg ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-loadavg vec::u8vector)
   ($uv-loadavg ($u8vector->double* vec 0)))
   
;*---------------------------------------------------------------------*/
;*    nodejs-getfreemem ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-getfreemem)
   (uv-get-free-memory))

;*---------------------------------------------------------------------*/
;*    nodejs-gettotalmem ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-gettotalmem)
   (uv-get-total-memory))

;*---------------------------------------------------------------------*/
;*    nodejs-getcpus ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-getcpus)
   (uv-cpus))

;*---------------------------------------------------------------------*/
;*    nodejs-getuptime ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-getuptime)
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
;*    nodejs-need-tick-callback ...                                    */
;*---------------------------------------------------------------------*/
(define (nodejs-need-tick-callback %worker %this process)
   (js-worker-push-thunk! %worker "tick-spinner"
      (lambda ()
	 (let ((tick-from-spinner (js-get process '_tickFromSpinner %this)))
	    (js-call0 %this tick-from-spinner (js-undefined))))))

;*    (set! need-tick-cb #t)                                           */
;*    (nodejs-async-push "nextTick"                                    */
;*       (lambda ()                                                    */
;* 	 (uv-idle-start (get-tick-spinner %worker %this process))))    */
   
;*        (uv-idle-start (get-tick-spinner %worker %this process))     */
;*        (uv-async++ "nextTick")                                      */
;*        (uv-async-send uv-async)                                     */
   
   
;*        (js-worker-push-thunk! %worker                               */
;* 	  (lambda ()                                                   */
;* 	     (unwind-protect                                           */
;* 		(unless tick-from-spinner                              */
;* 		   (set! tick-from-spinner                             */
;* 		      (js-get process '_tickFromSpinner %this)))       */
;* 		(js-call0 %this tick-from-spinner (js-undefined)))))   */
;*    )                                                                */

;*---------------------------------------------------------------------*/
;*    tick-spinner ...                                                 */
;*---------------------------------------------------------------------*/
(define tick-spinner #f)
(define need-tick-cb #f)
(define tick-from-spinner #f)

;*---------------------------------------------------------------------*/
;*    get-tick-spinner ...                                             */
;*---------------------------------------------------------------------*/
(define (get-tick-spinner %worker %this process)
   (unless tick-spinner
      (letrec* ((spin (lambda (status)
			 (when need-tick-cb
			    (set! need-tick-cb #f)
			    (uv-idle-stop spinner)
			    
			    (unless tick-from-spinner
			       (set! tick-from-spinner
				  (js-get process '_tickFromSpinner %this)))
			    
			    (js-worker-push-thunk! %worker "tick-spinner"
			       (lambda ()
				  (js-call0 %this tick-from-spinner
				     (js-undefined)))))))
		(spinner (instantiate::UvIdle
			    (cb spin)
			    (loop (worker-loop %worker)))))
	 (set! tick-spinner spinner))))

;*---------------------------------------------------------------------*/
;*    not-implemented-exn ...                                          */
;*---------------------------------------------------------------------*/
(define (not-implemented-exn fun %this)
   (with-access::JsGlobalObject %this (js-error)
      (js-new %this js-error (format "~a not implemented" fun))))

;*---------------------------------------------------------------------*/
;*    fs-exn ...                                                       */
;*---------------------------------------------------------------------*/
(define (fs-exn fmt obj %this)
   (with-access::JsGlobalObject %this (js-error)
      (js-new %this js-error (format fmt obj))))

;*---------------------------------------------------------------------*/
;*    fs-errno-exn ...                                                 */
;*---------------------------------------------------------------------*/
(define (fs-errno-exn fmt errno %this)
   (with-access::JsGlobalObject %this (js-error)
      (let* ((ename (uv-err-name errno))
	     (msg (format fmt (uv-strerror errno)))
	     (obj (js-new %this js-error
		     (if (string? ename)
			 (string-append ename ", " msg)
			 msg))))
	 (js-put! obj 'errno errno #f %this)
	 (js-put! obj 'code ename #f %this)
	 obj)))

;*---------------------------------------------------------------------*/
;*    fs-errno-path-exn ...                                            */
;*---------------------------------------------------------------------*/
(define (fs-errno-path-exn fmt errno %this path)
   (let ((exn (fs-errno-exn fmt errno %this)))
      (when (string? path)
	 (js-put! exn 'path path #t %this))
      exn))

;*---------------------------------------------------------------------*/
;*    fs-callback ...                                                  */
;*---------------------------------------------------------------------*/
(define (fs-callback %worker %this callback name fmt res #!optional (ok '()))
   (cond
      ((not (integer? res))
       (js-call1 %this callback (js-undefined) res))
      ((=fx res 0)
       (js-call1 %this callback (js-undefined) ok))
      (else
       (let ((exn (fs-errno-exn fmt res %this)))
	  (js-call1 %this callback (js-undefined) exn)))))

;*---------------------------------------------------------------------*/
;*    fs-callback-error ...                                            */
;*---------------------------------------------------------------------*/
(define (fs-callback-error %worker %this name callback . args)
   (with-access::JsGlobalObject %this (js-error)
      (let ((err (js-new %this js-error (format "EBADF, ~a" name))))
	   (js-put! err 'errno EBADF #f %this)
	   (js-put! err 'code  "EBADF" #f %this)
	   (if (isa? callback JsFunction)
	       (js-worker-push-thunk! %worker "fs-callback-error"
		  (lambda ()
		     (js-apply %this callback (js-undefined) (cons err args))))
	       (js-raise err)))))
       
;*---------------------------------------------------------------------*/
;*    nodejs-rename-file ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-rename-file %worker %this oldp newp callback)
   
   (define (rename-callback res)
      (fs-callback %worker %this callback "rename-file"
	 (format "rename: cannot rename file ~s into ~s -- ~~s" oldp newp)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-rename oldp newp
	  :callback rename-callback
	  :loop (worker-loop %worker))
       (let ((r (uv-fs-rename oldp newp)))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "rename: cannot rename file ~a into ~a"
		       oldp newp)
		    r %this))
	      r))))

;*---------------------------------------------------------------------*/
;*    nodejs-ftruncate ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-ftruncate %worker %this fd offset callback)
   
   (define (ftruncate-callback res)
      (fs-callback %worker %this callback "ftruncate"
	 (format "ftruncate: cannot truncate ~a to ~a -- ~~s" fd offset)
	 res))
   
   (let ((file (int->uvfile %worker %this fd)))
      (if file
	  (if (isa? callback JsFunction)
	      (uv-fs-ftruncate file offset
		 :callback ftruncate-callback
		 :loop (worker-loop %worker))
	      (uv-fs-ftruncate file offset))
	  (fs-callback-error %worker %this callback "ftruncate"))))

;*---------------------------------------------------------------------*/
;*    nodejs-truncate ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-truncate %worker %this path offset callback)

   (define (truncate-callback res)
      (fs-callback %worker %this callback "truncate"
	 (format "truncate: cannot truncate ~a to ~a -- ~~s" path offset)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-truncate path offset
	  :callback truncate-callback
	  :loop (worker-loop %worker))
       (uv-fs-truncate path offset)))

;*---------------------------------------------------------------------*/
;*    nodejs-fchown ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-fchown %worker %this fd uid guid callback)
   
   (define (fchown-callback res)
      (fs-callback %worker %this callback "fchown"
	 (format "fchown: cannot chown ~a, ~a, ~a -- ~~s" fd uid guid)
	 res))
   
   (let ((file (int->uvfile %worker %this fd)))
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
(define (nodejs-chown %worker %this path uid guid callback)

   (define (fchown-callback res)
      (fs-callback %worker %this callback "chown"
	 (format "chown: cannot chown ~a, ~a, ~a -- ~~s" path uid guid)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-chown path uid guid
	  :callback fchown-callback
	  :loop (worker-loop %worker))
       (uv-fs-chown path uid guid)))

;*---------------------------------------------------------------------*/
;*    nodejs-lchown ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-lchown %worker %this path uid guid callback)
   
   (define (lchown-callback res)
      (fs-callback %worker %this callback "lchown"
	 (format "lchown: cannot chown ~a, ~a, ~a -- ~~s" path uid guid)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-lchown path uid guid
	  :callback lchown-callback
	  :loop (worker-loop %worker))
       (uv-fs-lchown path uid guid)))

;*---------------------------------------------------------------------*/
;*    nodejs-fchmod ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-fchmod %worker %this fd mod callback)

   (define (fchmod-callback res)
      (fs-callback %worker %this callback "fchmod"
	 (format "fchmod: cannot chmod ~a, ~a -- ~~s" fd mod)
	 res))
   
   (let ((file (int->uvfile %worker %this fd)))
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
(define (nodejs-chmod %worker %this path mod callback)

   (define (chmod-callback res)
      (fs-callback %worker %this callback "chmod"
	 (format "chmod: cannot chmod ~a, ~a -- ~~s" path mod)
	 res))

   (if (isa? callback JsFunction)
       (uv-fs-chmod path mod
	  :callback chmod-callback
	  :loop (worker-loop %worker))
       (let ((r (uv-fs-chmod path mod)))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "chmod: cannot chmod ~a ~a" path mod)
		    r %this))
	      r))))

;*---------------------------------------------------------------------*/
;*    nodejs-lchmod ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-lchmod %worker %this fd mod callback)
   
   (define (lchmod-callback res)
      (fs-callback %worker %this callback "lchmod"
	 (format "lchmod: cannot chmod ~a, ~a -- ~~s" fd mod)
	 res))
   
   (lchmod-callback (not-implemented-exn "lchmod" %this)))

;*---------------------------------------------------------------------*/
;*    nodejs-open ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-open %worker %this path flags mode callback)
   
   (define (open-callback res)
      (if (isa? res UvFile)
	  (js-call2 %this callback (js-undefined) #f
	     (uvfile->int %worker res))
	  (let ((exn (fs-errno-exn
			(format "open: cannot open file ~a, ~a, ~a -- ~~s"
			   path flags mode)
			res %this)))
	     (js-call2 %this callback (js-undefined) exn #f))))
   
   (if (isa? callback JsFunction)
       (uv-fs-open path flags :mode mode
	  :loop (worker-loop %worker)
	  :callback open-callback)
       (let ((res (uv-fs-open path flags :mode mode)))
	  (if (isa? res UvFile)
	      (uvfile->int %worker res)
	      (let ((exn (fs-errno-exn
			    (format "open: cannot open file ~a, ~a, ~a -- ~~s"
			       path flags mode)
			    res %this)))
		 (js-raise exn))))))

;*---------------------------------------------------------------------*/
;*    nodejs-fs-close ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-fs-close %worker %this fd callback)
   (let ((file (int->uvfile %worker %this fd)))
      (if file
	  (begin
	     (close-uvfile %worker fd)
	     (if (isa? callback JsFunction)
		 (uv-fs-close file
		    :loop (worker-loop %worker)
		    :callback
		    (lambda (val) (js-call1 %this callback (js-undefined) val)))
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
(define (stat-cb %worker %this callback name obj proto lbl path)
   (lambda (res)
      (if (integer? res)
	  (js-call2 %this callback (js-undefined)
	     (fs-errno-path-exn
		(format "~a: cannot stat ~a -- ~~s" name obj)
		res %this path)
	     #f)
	  (let ((jsobj (stat->jsobj %this proto res)))
	     (js-call2 %this callback (js-undefined) #f jsobj)))))

;*---------------------------------------------------------------------*/
;*    nodejs-fstat ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-fstat %worker %this fd callback proto)
   (let ((file (int->uvfile %worker %this fd)))
      (if file
	  (if (isa? callback JsFunction)
	      (let ((lbl (string-append "fstat:" (integer->string fd))))
		 (uv-fs-fstat file
		    :loop (worker-loop %worker)
		    :callback (stat-cb %worker %this callback
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
(define (nodejs-stat %worker %this path callback proto)
   (if (isa? callback JsFunction)
       (let ((lbl (string-append "stat:" path)))
	  (uv-fs-stat path
	     :loop (worker-loop %worker)
	     :callback (stat-cb %worker %this callback "stat"
			  path proto lbl path)))
       (let ((res (uv-fs-stat path)))
	  (if (integer? res)
	      (js-raise
		 (fs-errno-path-exn
		    (format "stat: cannot stat ~a -- ~~s" path)
		    res %this path))
	      (stat->jsobj %this proto res)))))

;*---------------------------------------------------------------------*/
;*    nodejs-lstat ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-lstat %worker %this path callback proto)
   (if (isa? callback JsFunction)
       (let ((lbl (string-append "lstat:" path)))
	  (uv-fs-lstat path
	     :loop (worker-loop %worker)
	     :callback (stat-cb %worker %this callback "lstat"
			  path proto lbl path)))
       (let ((res (uv-fs-lstat path)))
	  (if (integer? res)
	      (js-raise
		 (fs-errno-exn (format "lstat: cannot stat ~a -- ~~s" path)
		    res %this))
	      (stat->jsobj %this proto res)))))

;*---------------------------------------------------------------------*/
;*    nodejs-link ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-link %worker %this src dst callback)
   
   (define (link-callback res)
      (fs-callback %worker %this callback "link"
	 (format "link: cannot link ~a, ~a -- ~~s" src dst)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-link src dst
	  :loop (worker-loop %worker)
	  :callback link-callback)
       (let ((r (uv-fs-link src dst)))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "link: cannot lin ~a ~a" src dst)
		    r %this))
	      r))))

;*---------------------------------------------------------------------*/
;*    nodejs-symlink ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-symlink %worker %this src dst callback)
   
   (define (symlink-callback res)
      (fs-callback %worker %this callback "symlink"
	 (format "symlink: cannot link ~a, ~a -- ~~s" src dst)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-symlink src dst
	  :loop (worker-loop %worker)
	  :callback symlink-callback)
       (uv-fs-symlink src dst)))

;*---------------------------------------------------------------------*/
;*    nodejs-readlink ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-readlink %worker %this src callback)
   
   (define (readlink-callback res)
      (if (integer? res)
	  (js-call2 %this callback (js-undefined)
	     (fs-errno-exn (format "readlink: ~~a ~s" src) res %this)
	     (js-undefined))
	  (js-call2 %this callback (js-undefined) '() res)))
   
   (if (isa? callback JsFunction)
       (uv-fs-readlink src
	  :loop (worker-loop %worker)
	  :callback readlink-callback)
       (let ((r (uv-fs-readlink src)))
	  (if (and (integer? r) (<fx r 0))
	      (with-access::JsGlobalObject %this (js-error)
		 (let ((exn (js-new %this js-error
			       (format "readlink: cannot read link ~a" src))))
		    (js-put! exn 'errno r #f %this)
		    (js-raise exn)))
	      r))))

;*---------------------------------------------------------------------*/
;*    nodejs-unlink ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-unlink %worker %this src callback)
   
   (define (unlink-callback res)
      (fs-callback %worker %this callback "unlink"
	 (format "unlink: cannot unlink ~a -- ~~s" src)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-unlink src
	  :loop (worker-loop %worker)
	  :callback unlink-callback)
       (let ((r (uv-fs-unlink src)))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "unlink: cannot unlink ~a" src)
		    r %this))
	      r)
	  r)))

;*---------------------------------------------------------------------*/
;*    nodejs-rmdir ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-rmdir %worker %this src callback)
   
   (define (rmdir-callback res)
      (fs-callback %worker %this callback "rmdir"
	 (format "rmdir: cannot rmdir ~a -- ~~s" src)
	 res))
   
   (if (isa? callback JsFunction)
       (uv-fs-rmdir src
	  :loop (worker-loop %worker)
	  :callback rmdir-callback)
       (let ((r (uv-fs-rmdir src)))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "rmdir: cannot rmdir ~a" src)
		    r %this))
	      r))))

;*---------------------------------------------------------------------*/
;*    nodejs-fdatasync ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-fdatasync %worker %this fd callback)
   
   (define (datasync-callback res)
      (fs-callback %worker %this callback "datasync"
	 (format "datasync: cannot datasync ~a -- ~~s" fd)
	 res))
   
   (let ((file (int->uvfile %worker %this fd)))
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
	  (fs-callback-error %worker %this callback "fdatasync" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-mkdir ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-mkdir %worker %this path mode callback)
   
   (define (mkdir-callback res)
      (cond
	 ((not (integer? res))
	  (js-call1 %this callback (js-undefined) res))
	 ((=fx res 0)
	  (js-call1 %this callback (js-undefined) '()))
	 (else
	  (let ((exn (fs-errno-path-exn
			(format "mkdir: cannot mkdir ~a -- ~~s" path)
			res %this path)))
	     (js-call1 %this callback (js-undefined) exn)))))
   
   (if (isa? callback JsFunction)
       (uv-fs-mkdir path mode
	  :loop (worker-loop %worker)
	  :callback mkdir-callback)
       (let ((r (uv-fs-mkdir path mode)))
	  (if (and (integer? r) (<fx r 0))
	      (js-raise
		 (fs-errno-exn
		    (format "mkdir: cannot mkdir ~a" path)
		    r %this))
	      r))))

;*---------------------------------------------------------------------*/
;*    nodejs-write ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-write %worker %this fd buffer offset length position callback)
   (let ((file (int->uvfile %worker %this fd)))
      (if file
	  (if (isa? callback JsFunction)
	      (with-access::JsArrayBufferView buffer (%data byteoffset)
		 (uv-fs-write (int->uvfile %worker %this fd) %data length
		    :callback (lambda (obj)
				 (if (<fx obj 0)
				     (js-call3 %this callback (js-undefined)
					obj #f buffer)
				     (js-call3 %this callback (js-undefined)
					#f obj buffer)))
		    :offset (+fx offset (uint32->fixnum byteoffset))
		    :position position
		    :loop (worker-loop %worker)))
	      (with-access::JsArrayBufferView buffer (%data byteoffset)
		 (uv-fs-write (int->uvfile %worker %this fd) %data length
		    :offset (+fx offset (uint32->fixnum byteoffset))
		    :position position :loop (worker-loop %worker))))
	  (fs-callback-error %worker %this "write" callback #f buffer))))

;*---------------------------------------------------------------------*/
;*    nodejs-read ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-read %worker %this fd buffer offset length position callback)
   (let ((file (int->uvfile %worker %this fd)))
      (if file
	  (with-access::JsArrayBufferView buffer (%data byteoffset)
	     (if (isa? callback JsFunction)
		 (uv-fs-read file %data length
		    :callback
		    (lambda (obj)
		       (if (<fx obj 0)
			   (js-call2 %this callback (js-undefined) obj #f)
			   (js-call2 %this callback (js-undefined) #f obj)))
		    :offset (+fx offset (uint32->fixnum byteoffset))
		    :position position
		    :loop (worker-loop %worker))
		 (uv-fs-read file %data length
		    :offset (+fx offset (uint32->fixnum byteoffset))
		    :position position)))
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
(define (nodejs-utimes %worker %this path atime mtime callback)
   
   (define (utimes-callback res)
      (fs-callback %worker %this callback "utimes"
	 (format "utimes: cannot utimes ~a -- ~~s" path)
	 res (js-undefined)))
   
   (if (isa? callback JsFunction)
       (uv-fs-utime path (js-todouble atime %this)
	  (js-todouble mtime %this)
	  :loop (worker-loop %worker)
	  :callback utimes-callback)
       (let ((res (uv-fs-utime path (js-todouble atime %this)
		     (js-todouble mtime %this))))
	  (if (=fx res 0)
	      res
	      (js-raise (fs-errno-exn "~a" res %this))))))

;*---------------------------------------------------------------------*/
;*    nodejs-futimes ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-futimes %worker %this fd atime mtime callback)
   
   (define (utimes-callback res)
      (fs-callback %worker %this callback "futimes"
	 (format "futimes: cannot utimes ~a -- ~~s" fd)
	 res (js-undefined)))
   
   (let ((file (int->uvfile %worker %this fd)))
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
(define (nodejs-fsync %worker %this fd callback)
   
   (define (fsync-callback res)
      (fs-callback %worker %this callback "fsync"
	 (format "fsync: cannot fsync ~a -- ~~s" fd)
	 res))
   
   (let ((file (int->uvfile %worker %this fd)))
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
   [assert (%worker) (eq? (current-thread) %worker)]
   (with-access::JsGlobalObject %this (js-object)
      (let ((wrap (js-new %this js-object)))
	 (uv-getaddrinfo node #f
	    :family family
	    :loop (worker-loop %worker)
	    :callback
	    (lambda (res)
	       (let ((oncomplete (js-get wrap 'oncomplete %this)))
		  (if (isa? oncomplete JsFunction)
		      (if (pair? res)
			  (js-call1 %this oncomplete (js-undefined)
			     (js-vector->jsarray (list->vector res) %this))
			  (begin
			     (process-fail %this process res)
			     (js-call1 %this oncomplete (js-undefined)
				(js-undefined))))))))
	 wrap)))

;*---------------------------------------------------------------------*/
;*    process-fail ...                                                 */
;*---------------------------------------------------------------------*/
(define (process-fail %this process errno)
   (js-put! process 'errno errno #f %this)
   (js-put! process '_errno (uv-err-name errno) #f %this)
   #f)

;*---------------------------------------------------------------------*/
;*    nodejs-query ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-query %worker %this process node family cb)
   
   (define (query-callback res)
      (if (pair? res)
	  (let ((v (js-vector->jsarray (list->vector res) %this)))
	     (js-call2 %this cb (js-undefined) #f v))
	  (js-call2 %this cb (js-undefined) res '#())))
   
   [assert () (isa? (current-thread) WorkerHopThread)]
   (with-access::JsGlobalObject %this (js-object)
      (let ((res (uv-getaddrinfo node #f :family family
		    :loop (worker-loop %worker)
		    :callback query-callback)))
	 (if (=fx res 0)
	     #t
	     (process-fail %this process res)))))

;*---------------------------------------------------------------------*/
;*    nodejs-isip ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-isip addr)
   (cond
      ((uv-inet-pton addr :family 4) 4)
      ((uv-inet-pton addr :family 6) 6)
      (else 0)))

;*---------------------------------------------------------------------*/
;*    nodejs-istty ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-istty %worker %this fd)
   (let ((file (int->uvfile %worker %this fd)))
      (when file
	 (eq? (uv-guess-handle file) 'TTY))))

;*---------------------------------------------------------------------*/
;*    nodejs-guess-handle-type ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-guess-handle-type %worker %this fd)
   (let ((file (int->uvfile %worker %this fd)))
      (when file
	 (symbol->string (uv-guess-handle file)))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-handle ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-handle %worker)
   (instantiate::UvTcp
      (loop (worker-loop %worker))))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-write-queue-size ...                               */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-write-queue-size hdl)
   (uv-stream-write-queue-size hdl))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-fd ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-fd hdl)
   (uv-stream-fd hdl))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-connect ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-connect %worker %this handle host port family callback)
   [assert (%worker) (eq? (current-thread) %worker)]
   (uv-tcp-connect handle host port :family family
      :loop (worker-loop %worker)
      :callback callback))

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
   (uv-tcp-open handle (int->uvfile %worker %this fd)))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-bind ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-bind %this handle addr port family)
   (uv-tcp-bind handle addr port :family family))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-listen ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-listen %worker %this process this handle backlog tcp-wrap)
   [assert (%worker) (eq? (current-thread) %worker)]
   (let ((r (uv-listen handle backlog
	       :loop (worker-loop %worker)
	       :callback
	       (lambda (server status)
		  (if (< status 0)
		      (process-fail %this process status)
		      (with-access::UvTcp server (loop)
			 (let ((client (instantiate::UvTcp (loop loop))))
			    (let ((r (uv-accept handle client)))
			       (if (< r 0)
				   (process-fail %this process r)
				   (let ((onconn (js-get this 'onconnection %this)))
				      (js-call1 %this onconn this
					 (tcp-wrap client))))))))))))
      (if (<fx r 0)
	  (fs-errno-exn "Listen failed ~s" r %this)
	  r)))
   
;*---------------------------------------------------------------------*/
;*    nodejs-stream-write ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-write %worker %this handle buffer offset length callback)
   [assert (%worker) (eq? %worker (current-thread))]
   (uv-stream-write handle buffer offset length
      :loop (worker-loop %worker)
      :callback callback))
   
;*---------------------------------------------------------------------*/
;*    nodejs-stream-read-start ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-read-start %worker %this handle onalloc callback)
   [assert (%worker) (eq? %worker (current-thread))]
   (uv-stream-read-start handle
      :onalloc onalloc
      :loop (worker-loop %worker)
      :callback callback))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-read-stop ...                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-read-stop %worker %this handle)
   [assert (%worker) (eq? %worker (current-thread))]
   (uv-stream-read-stop handle))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-shutdown ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-shutdown %worker %this handle callback)
   [assert (%worker) (eq? %worker (current-thread))]
   (uv-stream-shutdown handle
      :loop (worker-loop %worker)
      :callback callback))

;*---------------------------------------------------------------------*/
;*    uvfile->int ...                                                  */
;*---------------------------------------------------------------------*/
(define (uvfile->int %worker file::UvFile)
   (with-access::WorkerHopThread %worker (uvfiles)
      (with-access::UvFile file (fd)
	 (when (>=fx fd (vector-length uvfiles))
	    (let ((new (make-vector (*fx 2 fd))))
	       (vector-copy! new 0 uvfiles)
	       (set! uvfiles new)))
	 (vector-set! uvfiles fd file)
	 fd)))

;*---------------------------------------------------------------------*/
;*    int->uvfile ...                                                  */
;*---------------------------------------------------------------------*/
(define (int->uvfile %worker %this fd::int)
   (with-access::WorkerHopThread %worker (uvfiles)
      (cond
	 ((<fx fd 0) #f)
	 ((>fx fd (vector-length uvfiles)) #f)
	 (else
	  (let ((file (vector-ref uvfiles fd)))
	     (cond
		((isa? file UvFile)
		 file)
		((<=fx fd 2)
		 ;; initialize stdio lazily
		 (let ((file (instantiate::UvFile
				(fd fd)
				(path ""))))
		    (vector-set! uvfiles fd file)
		    file))
		(else
		 (js-raise-type-error %this
		    "Illegal file descriptor ~a" fd))))))))

;*---------------------------------------------------------------------*/
;*    close-uvfile ...                                                 */
;*---------------------------------------------------------------------*/
(define (close-uvfile %worker fd::int)
   (with-access::WorkerHopThread %worker (uvfiles)
      (vector-set! uvfiles fd #f)))

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
	 ((=fx sig sigfpe) "SIGFPE")
	 ((=fx sig sigill) "SIGILL")
	 ((=fx sig sigbus) "SIGBUS")
	 ((=fx sig sigsegv) "SIGSEGV")
	 ((=fx sig sigpipe) "SIGPIPE")
	 ((=fx sig sigterm) "SIGTERM")
	 ((=fx sig sigint) "SIGINT")
	 ((=fx sig sigkill) "SIGKILL")
	 (else "SIG???")))
   
   (define (process-options-stdio-set! opts::UvProcessOptions i stdio)
      (let ((type (js-get stdio 'type %this)))
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
	     (uv-process-options-stdio-container-stream-set! opts i
		(with-access::JsHandle (js-get stdio 'handle %this) (handle)
		   handle)))
	    ((string=? type "wrap")
	     (uv-process-options-stdio-container-flags-set! opts i
		(UV-INHERIT-STREAM))
	     (uv-process-options-stdio-container-stream-set! opts i
		(with-access::JsHandle (js-get stdio 'handle %this) (handle)
		   handle)))
	    (else
	     (uv-process-options-stdio-container-flags-set! opts i
		(UV-INHERIT-FD))
	     (uv-process-options-stdio-container-fd-set! opts i
		(int->uvfile %worker %this (js-get stdio 'fd %this)))))))
   
   (define (onexit this status term)
      (let ((onexit (js-get process 'onexit %this))
	    (status (flonum->fixnum (int64->flonum status))))
	 (with-trace 'nodejs-async "process-onexit"
	    (when (isa? onexit JsFunction)
	       (when (<fx status 0)
		  (process-fail %this %process status)
		  (set! status -1))
	       (js-call2 %this onexit process
		  status
		  (if (=fx term 0)
		      (js-undefined)
		      (signal->string term)))))))
   
   (let ((opts (instantiate::UvProcessOptions)))
      (with-access::UvProcessOptions opts ((oflags flags)
					   (ouid uid)
					   (ogid gid)
					   (ofile file)
					   (oargs args)
					   (ocwd cwd)
					   (oenv env))
	 
	 ;; options.uid
	 (let ((uid (js-get options 'uid %this)))
	    (when (integer? uid)
	       (let ((uid (js-toint32 uid %this)))
		  (set! oflags (bit-or oflags (UV-PROCESS-SETUID)))
		  (set! ouid uid))))
	 
	 ;; options.gid
	 (let ((gid (js-get options 'gid %this)))
	    (when (integer? gid)
	       (let ((gid (js-toint32 gid %this)))
		  (set! oflags (bit-or oflags (UV-PROCESS-SETGID)))
		  (set! ogid gid))))
	 
	 ;; options.file
	 (let ((file (js-get options 'file %this)))
	    (unless (string? file)
	       (js-raise-type-error %this "Bad argument ~a" file))
	    (set! ofile file))
	 
	 ;; options.args
	 (let ((args (js-get options 'args %this)))
	    (when (isa? args JsArray)
	       (set! oargs
		  (vector-map! (lambda (o)
				  (js-tostring o %this))
		     (jsarray->vector args %this)))))
	 
	 ;; options.cwd
	 (let ((cwd (js-get options 'cwd %this)))
	    (when (and (string? cwd) (>fx (string-length cwd) 0))
	       (set! ocwd cwd)))
	 
	 ;; options.env
	 (let ((env (js-get options 'envPairs %this)))
	    (when (isa? env JsArray)
	       (set! oenv (jsarray->vector env %this))))
	 
	 ;; options.stdio
	 (let ((stdios (js-get options 'stdio %this)))
	    (when (isa? stdios JsArray)
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
		   0)))))))

;*---------------------------------------------------------------------*/
;*    nodejs-process-kill ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-process-kill %worker %this %process this sig)
   (with-access::JsHandle this (handle)
      (let ((r (uv-process-kill handle sig)))
	 (unless (=fx r 0)
	    (process-fail %this %process r)))))

;*---------------------------------------------------------------------*/
;*    nodejs-new-pipe ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-new-pipe %worker ipc)
   (instantiate::JsPipe
      (count 0)
      (loop (worker-loop %worker))
      (ipc (eq? ipc #t))))

;*---------------------------------------------------------------------*/
;*    nodejs-pipe-open ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-pipe-open %worker %this handle fd)
   (uv-pipe-open handle (int->uvfile %worker %this fd)))

;*---------------------------------------------------------------------*/
;*    nodejs-pipe-bind ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-pipe-bind %this handle name)
   (uv-pipe-bind handle name))

;*---------------------------------------------------------------------*/
;*    nodejs-pipe-connect ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-pipe-connect %worker %this handle name callback)
   [assert (%worker) (eq? (current-thread) %worker)]
   (uv-pipe-connect handle name
      :loop (worker-loop %worker)
      :callback callback))

;*---------------------------------------------------------------------*/
;*    nodejs-pipe-listen ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-pipe-listen %worker %this process this handle backlog)
   [assert (%worker) (eq? (current-thread) %worker)]
   (let ((r (uv-listen handle backlog
	       :loop (worker-loop %worker)
	       :callback
	       (lambda (server status)
		  (if (< status 0)
		      (process-fail %this process status)
		      (let ((onconn (js-get this 'onconnection %this)))
			 (js-call0 %this onconn this)))))))
      (if (<fx r 0)
	  (fs-errno-exn "Listen failed ~s" r %this)
	  r)))
