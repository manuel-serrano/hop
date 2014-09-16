;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/uv.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 14 05:42:05 2014                          */
;*    Last change :  Mon Sep 15 10:02:21 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    NodeJS libuv binding                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_uv

   (library hop hopscript)

   (include "nodejs_debug.sch")
   
   (cond-expand
      (enable-libuv (library libuv))
      (else (import __nodejs__uv)))

   (import __nodejs_process
	   __nodejs__buffer)
   
   (export (nodejs-event-loop)
	   (nodejs-async-push ::procedure)

	   (nodejs-close ::WorkerHopThread ::JsGlobalObject ::obj ::obj)
	   (nodejs-ref ::obj)
	   (nodejs-unref ::obj)
	   
	   (nodejs-make-timer)
	   (nodejs-timer-callback-set! ::obj ::procedure)
	   (nodejs-timer-start ::obj ::uint32 ::uint32)
	   (nodejs-timer-close ::obj)
	   (nodejs-timer-stop ::obj)
	   (nodejs-timer-unref ::obj)
	   
	   (nodejs-loadavg ::u8vector)
	   (nodejs-getfreemem::double)
	   (nodejs-gettotalmem::double)
	   (nodejs-getcpus::vector)

	   (nodejs-need-tick-callback ::WorkerHopThread ::JsGlobalObject ::JsObject)
	   
	   (nodejs-rename-file ::WorkerHopThread ::JsGlobalObject ::bstring ::bstring ::obj)
	   (nodejs-ftruncate ::WorkerHopThread ::JsGlobalObject ::obj ::int ::obj)
	   (nodejs-truncate ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::obj)
	   (nodejs-fchown ::WorkerHopThread ::JsGlobalObject ::obj ::int ::int ::obj)
	   (nodejs-chown ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::int ::obj)
	   (nodejs-lchown ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::int ::obj)
	   (nodejs-fchmod ::WorkerHopThread ::JsGlobalObject ::obj ::int ::obj)
	   (nodejs-chmod ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::obj)
	   (nodejs-lchmod ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::obj)
	   (nodejs-stat ::WorkerHopThread ::JsGlobalObject ::bstring ::obj ::obj)
	   (nodejs-fstat ::WorkerHopThread ::JsGlobalObject ::obj ::obj ::obj)
	   (nodejs-lstat ::WorkerHopThread ::JsGlobalObject ::bstring ::obj ::obj)
	   (nodejs-link ::WorkerHopThread ::JsGlobalObject ::bstring ::bstring ::obj)
	   (nodejs-symlink ::WorkerHopThread ::JsGlobalObject ::bstring ::bstring ::obj)
	   (nodejs-readlink ::WorkerHopThread ::JsGlobalObject ::bstring ::obj)
	   (nodejs-unlink ::WorkerHopThread ::JsGlobalObject ::bstring ::obj)
	   (nodejs-rmdir ::WorkerHopThread ::JsGlobalObject ::bstring ::obj)
	   (nodejs-mkdir ::WorkerHopThread ::JsGlobalObject ::bstring ::int ::obj)
	   (nodejs-open ::WorkerHopThread ::JsGlobalObject ::bstring ::long ::long ::obj)
	   (nodejs-utimes ::WorkerHopThread ::JsGlobalObject ::bstring ::long ::long ::obj)
	   (nodejs-futimes ::WorkerHopThread ::JsGlobalObject ::obj ::long ::long ::obj)
	   (nodejs-fsync ::WorkerHopThread ::JsGlobalObject ::obj ::obj)
;* 	   (nodejs-write ::WorkerHopThread ::JsGlobalObject ::obj ::obj ::long ::long ::long ::obj) */
	   (nodejs-read ::WorkerHopThread ::JsGlobalObject ::obj ::obj ::long ::long ::long ::obj)
	   (nodejs-fs-close ::WorkerHopThread ::JsGlobalObject ::obj ::obj)

	   (nodejs-getaddrinfo ::JsGlobalObject ::JsObject ::bstring ::int)
	   (nodejs-query ::WorkerHopThread ::JsGlobalObject ::JsObject ::bstring ::int ::JsObject)
	   (nodejs-isip ::bstring)

	   (nodejs-istty ::obj)
	   (nodejs-guess-handle-type ::obj)
	   
	   (nodejs-tcp-handle)
	   (nodejs-stream-write-queue-size::long ::obj)
	   (nodejs-stream-fd::long ::obj)
	   (nodejs-tcp-connect ::WorkerHopThread ::JsGlobalObject ::obj ::bstring ::int ::int ::procedure)
	   (nodejs-tcp-nodelay ::obj ::bool)
	   (nodejs-tcp-keepalive ::obj ::bool ::long)
	   (nodejs-tcp-simultaneous-accepts ::obj ::bool)
	   (nodejs-tcp-getsockname ::JsGlobalObject ::obj)
	   (nodejs-tcp-getpeername ::JsGlobalObject ::obj)
	   (nodejs-tcp-open ::JsGlobalObject ::obj ::int)
	   (nodejs-tcp-bind ::JsGlobalObject ::obj ::bstring ::int ::int)
	   (nodejs-tcp-listen ::WorkerHopThread ::JsGlobalObject ::JsObject ::obj ::obj ::int)

	   (nodejs-stream-write ::WorkerHopThread ::JsGlobalObject ::obj ::bstring ::long ::long ::procedure)
	   (nodejs-stream-read-start ::WorkerHopThread ::JsGlobalObject ::obj ::procedure ::obj)
	   (nodejs-stream-read-stop ::JsGlobalObject ::obj)
	   (nodejs-stream-shutdown ::WorkerHopThread ::JsGlobalObject ::obj ::procedure)
	   ))

;*---------------------------------------------------------------------*/
;*    Constants                                                        */
;*---------------------------------------------------------------------*/
(define ENOENT
   (cond-expand (bigloo-c (pragma::long "ENOENT")) (else 2)))

;*---------------------------------------------------------------------*/
;*    uv-mutex ...                                                     */
;*---------------------------------------------------------------------*/
(cond-expand
   (enable-libuv
    
(define uv-mutex (make-mutex "uv-mutex"))
(define uv-actions '())
(define uv-async #f)

))

;*---------------------------------------------------------------------*/
;*    nodejs-event-loop ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-event-loop)
   (cond-expand
      (enable-libuv
       (let ((loop (uv-default-loop)))
	  (set! uv-async
	     (instantiate::UvAsync
		(loop loop)
		(cb (lambda (a)
		       (for-each (lambda (action) (action))
			  (synchronize uv-mutex
			     (let ((actions uv-actions))
				(set! uv-actions '())
				actions)))))))
	  (uv-run loop)))
      (else
       (%nodejs-event-loop))))

;*---------------------------------------------------------------------*/
;*    nodejs-async-push ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-async-push thunk)
   (synchronize uv-mutex
      (set! uv-actions (cons thunk uv-actions))
      (uv-async-send uv-async)))
   
;*---------------------------------------------------------------------*/
;*    nodejs-close ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-close %worker %this obj callback)
   (cond-expand
      (enable-libuv
       (uv-close obj
	  (when (isa? callback JsFunction)
	     (lambda (val)
		(js-worker-push-thunk! %worker
		   (lambda ()
		      (js-call1 %this callback (js-undefined) val)))))))
      (else
       #f)))
   
;*---------------------------------------------------------------------*/
;*    nodejs-ref ...                                                   */
;*---------------------------------------------------------------------*/
(define (nodejs-ref obj)
   (cond-expand
      (enable-libuv
       (uv-ref obj))
      (else
       #f)))
   
;*---------------------------------------------------------------------*/
;*    nodejs-unref ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-unref obj)
   (cond-expand
      (enable-libuv
       (uv-unref obj))
      (else
       #f)))
   
;*---------------------------------------------------------------------*/
;*    nodejs-make-timer ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-make-timer)
   (cond-expand
      (enable-libuv
       (instantiate::UvTimer (loop (uv-default-loop))))
      (else
       (%nodejs-make-timer))))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-callback-set! ...                                   */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-callback-set! timer proc)
   (cond-expand
      (enable-libuv
       (with-access::UvTimer timer (cb)
	  (set! cb proc)))
      (else
       (%nodejs-timer-callback-set! timer proc))))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-start ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-start timer start rep)
   (cond-expand
      (enable-libuv
       (nodejs-async-push
	  (lambda ()
	     (uv-timer-start timer
		(llong->uint64 (uint32->llong start))
		(llong->uint64 (uint32->llong rep))))))
      (else
       (%nodejs-timer-start timer start rep))))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-close ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-close timer)
   (cond-expand
      (enable-libuv
       (nodejs-async-push
	  (lambda () (uv-close timer))))
      (else
       (%nodejs-timer-close timer))))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-stop ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-stop timer)
   (cond-expand
      (enable-libuv
       (nodejs-async-push
	  (lambda () (uv-timer-stop timer))))
      (else
       (%nodejs-timer-stop timer))))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-unref ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-unref timer)
   (cond-expand
      (enable-libuv
       (nodejs-async-push
	  (lambda () (uv-unref timer))))
      (else
       #unspecified)))

;*---------------------------------------------------------------------*/
;*    nodejs-loadavg ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-loadavg vec::u8vector)
   (cond-expand
      (enable-libuv
       ($uv-loadavg ($u8vector->double* vec 0))
       vec)
      (else
       vec)))
   
;*---------------------------------------------------------------------*/
;*    nodejs-getfreemem ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-getfreemem)
   (cond-expand
      (enable-libuv
       (uv-get-free-memory))
      (else
       0.0)))

;*---------------------------------------------------------------------*/
;*    nodejs-gettotalmem ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-gettotalmem)
   (cond-expand
      (enable-libuv
       (uv-get-total-memory))
      (else
       0.0)))

;*---------------------------------------------------------------------*/
;*    nodejs-getcpus ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-getcpus)
   (cond-expand
      (enable-libuv
       (uv-cpus))
      (else
       '#())))

;*---------------------------------------------------------------------*/
;*    nodejs-need-tick-callback ...                                    */
;*---------------------------------------------------------------------*/
(define (nodejs-need-tick-callback %worker %this process)
   (cond-expand
      (enable-libuv
       (set! need-tick-cb #t)
       (uv-idle-start (get-tick-spinner %worker %this process))
       (uv-async-send uv-async)))
   #unspecified)

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
   (cond-expand
      (enable-libuv
       (unless tick-spinner
	  (letrec* ((spin (lambda (status)
			     (when need-tick-cb
				(set! need-tick-cb #f)
				(uv-idle-stop spinner)
				
				(unless tick-from-spinner
				   (set! tick-from-spinner
				      (js-get process '_tickFromSpinner %this)))

				(js-worker-push-thunk! %worker
				   (lambda ()
				      (js-call0 %this tick-from-spinner
					 (js-undefined)))))))
		    (spinner (instantiate::UvIdle
				(cb spin)
				(loop (uv-default-loop)))))
	     (set! tick-spinner spinner)))))
   tick-spinner)

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
   
   (cond-expand
      ((not enable-libuv)
       (define (uv-strerror errno) (integer->string errno))))

   (cond-expand
      ((not enable-libuv)
       (define (uv-err-name errno) (integer->string errno))))

   (with-access::JsGlobalObject %this (js-error)
      (let ((obj (js-new %this js-error
		    (format fmt (uv-strerror errno)))))
	 (js-put! obj 'errno errno #f %this)
	 (js-put! obj 'code (uv-err-name errno) #f %this)
	 obj)))

;*---------------------------------------------------------------------*/
;*    fs-callback ...                                                  */
;*---------------------------------------------------------------------*/
(define (fs-callback %worker %this callback fmt res)
   (cond
      ((not (integer? res))
       (js-worker-push-thunk! %worker
	  (lambda ()
	     (js-call1 %this callback (js-undefined) res))))
      ((=fx res 0)
       (js-worker-push-thunk! %worker
	  (lambda ()
	     (js-call1 %this callback (js-undefined) #f))))
      (else
       (js-worker-push-thunk! %worker
	  (lambda ()
	     (let ((exn (fs-errno-exn fmt res %this)))
		(js-call1 %this callback (js-undefined) exn)))))))

;*---------------------------------------------------------------------*/
;*    nodejs-rename-file ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-rename-file %worker %this oldp newp callback)
   
   (define (rename-callback res)
      (fs-callback %worker %this callback
	 (format "rename: cannot rename file ~s into ~s -- ~~s" oldp newp)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-rename oldp newp
	  :callback
	  (when (isa? callback JsFunction) rename-callback)))
      (else
       (let ((res (rename-file oldp newp)))
	  (if (isa? callback JsFunction)
	      (rename-callback res)
	      res)))))

;*---------------------------------------------------------------------*/
;*    nodejs-ftruncate ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-ftruncate %worker %this fd offset callback)
   
   (define (ftruncate-callback res)
      (fs-callback %worker %this callback
	 (format "ftruncate: cannot truncate ~a to ~a -- ~~s" fd offset)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-ftruncate fd offset
	  :callback
	  (when (isa? callback JsFunction) ftruncate-callback)))
      (else
       (cond
	  ((output-port? port)
	   (let ((res (output-port-truncate port offset)))
	      (if (isa? callback JsFunction)
		  (ftruncate-callback res)
		  res)))
	  (else
	   (ftruncate-callback (not-implemented-exn "ftruncate")))))))

;*---------------------------------------------------------------------*/
;*    nodejs-truncate ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-truncate %worker %this path offset callback)

   (define (truncate-callback res)
      (fs-callback %worker %this callback
	 (format "truncate: cannot truncate ~a to ~a -- ~~s" path offset)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-truncate path offset
	  :callback
	  (when (isa? callback JsFunction) truncate-callback)))
      (else
       (truncate-callback (not-implemented-exn "truncate")))))

;*---------------------------------------------------------------------*/
;*    nodejs-fchown ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-fchown %worker %this fd uid guid callback)
   
   (define (fchown-callback res)
      (fs-callback %worker %this callback
	 (format "fchown: cannot chown ~a, ~a, ~a -- ~~s" fd uid guid)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-fchown fd uid guid
	  :callback
	  (when (isa? callback JsFunction) fchown-callback)))
      (else
       (fchown-callback (not-implemented-exn "fchown")))))

;*---------------------------------------------------------------------*/
;*    nodejs-chown ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-chown %worker %this fd uid guid callback)

   (define (fchown-callback res)
      (fs-callback %worker %this callback
	 (format "chown: cannot chown ~a, ~a, ~a -- ~~s" fd uid guid)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-chown fd uid guid
	  :callback
	  (when (isa? callback JsFunction) fchown-callback)))
      (else
       (chown-callback (not-implemented-exn "chown")))))

;*---------------------------------------------------------------------*/
;*    nodejs-lchown ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-lchown %worker %this fd uid guid callback)
   
   (define (lchown-callback res)
      (fs-callback %worker %this callback
	 (format "lchown: cannot chown ~a, ~a, ~a -- ~~s" fd uid guid)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-lchown fd uid guid
	  :callback
	  (when (isa? callback JsFunction) lchown-callback)))
      (else
       (chown-callback (not-implemented-exn "lchown")))))

;*---------------------------------------------------------------------*/
;*    nodejs-fchmod ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-fchmod %worker %this fd mod callback)

   (define (fchmod-callback res)
      (fs-callback %worker %this callback
	 (format "fchmod: cannot chmod ~a, ~a -- ~~s" fd mod)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-fchmod fd mod
	  :callback
	  (when (isa? callback JsFunction) fchmod-callback)))
      (else
       (fchmod-callback (not-implemented-exn "fchmod")))))

;*---------------------------------------------------------------------*/
;*    nodejs-chmod ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-chmod %worker %this fd mod callback)

   (define (chmod-callback res)
      (fs-callback %worker %this callback
	 (format "chmod: cannot chmod ~a, ~a -- ~~s" fd mod)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-chmod fd mod
	  :callback
	  (when (isa? callback JsFunction) chmod-callback)))
      (else
       (chmod-callback (not-implemented-exn "chmod")))))

;*---------------------------------------------------------------------*/
;*    nodejs-lchmod ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-lchmod %worker %this fd mod callback)
   
   (define (lchmod-callback res)
      (fs-callback %worker %this callback
	 (format "lchmod: cannot chmod ~a, ~a -- ~~s" fd mod)
	 res))
   
   (cond-expand
      (enable-libuv
       (lchmod-callback (not-implemented-exn "lchmod" %this)))
      (else
       (lchmod-callback (not-implemented-exn "lchmod" %this)))))

;*---------------------------------------------------------------------*/
;*    nodejs-open ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-open %worker %this path flags mode callback)

   (define (open-callback res)
      (js-worker-push-thunk! %worker
	 (lambda ()
	    (if (isa? res UvFile)
		(js-call2 %this callback (js-undefined) #f res)
		(let ((exn (fs-errno-exn
			      (format "open: cannot open file ~a, ~a, ~a -- ~~s"
				 path flags mode)
			      res %this)))
		   (js-call2 %this callback (js-undefined) exn #f))))))
   
   (cond-expand
      (enable-libuv
       (uv-fs-open path flags
	  :mode mode
	  :loop (uv-default-loop)
	  :callback (when (isa? callback JsFunction) open-callback)))
      (else
       (let ((ip (cond
		    ((not (integer? flags))
		     #f)
		    ((=fx flags O_RDONLY)
		     (open-input-file path))
		    ((=fx flags O_WRONLY)
		     (open-output-file path))
		    ((=fx flags O_APPEND)
		     (append-output-file path))
		    (else
		     #f))))
	  (if (isa? callback JsFunction)
	      (if (not ip)
		  (open-callback -22)
		  (open-callback ip))
	      ip)))))

;*---------------------------------------------------------------------*/
;*    nodejs-fs-close ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-fs-close %worker %this fd callback)
   (cond-expand
      (enable-libuv
       (uv-fs-close fd :callback
	  (when (isa? callback JsFunction)
	     (lambda (val)
		(js-worker-push-thunk! %worker
		   (lambda ()
		      (js-call1 %this callback (js-undefined) val)))))))
      (else
       (let ((res (cond
		     ((output-port? fd) (close-output-port fd))
		     ((input-port? fd) (close-input-port fd))
		     (else #f))))
	  (if (isa? callback JsFunction)
	      (js-call1 %this callback (js-undefined) res)
	      res)))))

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
(define (stat-cb %worker %this callback name obj proto)
   (lambda (res)
      (if (integer? res)
	  (js-worker-push-thunk! %worker
	     (lambda ()
		(js-call2 %this callback (js-undefined)
		   (fs-errno-exn (format "~a: cannot stat ~a -- ~~s" name obj)
		      res %this)
		   #f)))
	  (let ((jsobj (stat->jsobj %this proto res)))
	     (js-worker-push-thunk! %worker
		(lambda ()
		   (js-call2 %this callback (js-undefined) #f jsobj)))))))

;*---------------------------------------------------------------------*/
;*    nodejs-fstat ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-fstat %worker %this fd callback proto)
   (cond-expand
      (enable-libuv
       (if (isa? callback JsFunction)
	   (uv-fs-fstat fd
	      :callback (stat-cb %worker %this callback "fstat" fd proto))
	   (let ((res (uv-fs-fstat fd)))
	      (if (integer? res)
		  res
		  (stat->jsobj %this proto res)))))
      (else
       ((input-port? fd)
	(let ((obj (js-alist->jsobject
		      `((size . ,(elong->fixnum (input-port-length fd))))
		      %this)))
	   (with-access::JsObject obj (__proto__)
	      (set! __proto__ proto)
	      (if (isa? callback JsFunction)
		  (js-call2 %this callback (js-undefined) #f obj)
		  obj))))
       ((isa? callback JsFunction)
	(js-call2 %this callback (js-undefined)
	   (fs-exn (format "fstat: Not a file descriptor ~a" fd) #f)))
       (else
	(fs-exn "fstat: Not a file descriptor ~a" fd)))))

;*---------------------------------------------------------------------*/
;*    nodejs-stat ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-stat %worker %this path callback proto)
   (cond-expand
      (enable-libuv
       (if (isa? callback JsFunction)
	   (uv-fs-stat path
	      :callback (stat-cb %worker %this callback "stat" path proto))
	   (let ((res (uv-fs-stat path)))
	      (if (integer? res)
		  res
		  (stat->jsobj %this proto res)))))
      (else
       (if (file-exists? path)
	   (let ((obj (js-alist->jsobject
			 `((size . ,(elong->fixnum (file-size path)))
			   (gid . ,(file-gid path))
			   (uid . ,(file-uid path))
			   (mode . ,(file-mode path))
			   (mtime . ,(js-date->jsdate
					(seconds->date
					   (file-modification-time path))
					%this)))
			 %this)))
	      (with-access::JsObject obj (__proto__)
		 (set! __proto__ proto)
		 (if (isa? callback JsFunction)
		     (js-call2 %this callback (js-undefined) #f obj)
		     obj)))
	   (if (isa? callback Js-Undefined)
	       (js-call2 %this callback
		  (fs-exn "stat: does not exist ~a" path) #f)
	       (fs-exn "stat: does not exist ~a" path))))))

;*---------------------------------------------------------------------*/
;*    nodejs-lstat ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-lstat %worker %this path callback proto)
   (cond-expand
      (enable-libuv
       (if (isa? callback JsFunction)
	   (uv-fs-lstat path
	      :callback
	      (stat-cb %worker %this callback "lstat" path proto))
	   (let ((res (uv-fs-lstat path)))
	      (if (integer? res)
		  res
		  (stat->jsobj %this proto res)))))
      (else
       (if (isa? callback Js-Undefined)
	   (js-call2 %this callback (not-implemented-exn "lstat"))
	   (not-implemented-exn "lstat")))))

;*---------------------------------------------------------------------*/
;*    nodejs-link ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-link %worker %this src dst callback)
   
   (define (link-callback res)
      (fs-callback %worker %this callback
	 (format "link: cannot link ~a, ~a -- ~~s" src dst)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-link src dst
	  :callback
	  (when (isa? callback JsFunction) link-callback)))
      (else
       (link-callback (not-implemented-exn "link")))))

;*---------------------------------------------------------------------*/
;*    nodejs-symlink ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-symlink %worker %this src dst callback)
   
   (define (symlink-callback res)
      (fs-callback %worker %this callback
	 (format "symlink: cannot link ~a, ~a -- ~~s" src dst)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-symlink src dst
	  :callback
	  (when (isa? callback JsFunction) symlink-callback)))
      (else
       (symlink-callback (not-implemented-exn "symlink")))))

;*---------------------------------------------------------------------*/
;*    nodejs-readlink ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-readlink %worker %this src callback)
   
   (define (readlink-callback res)
      (fs-callback %worker %this callback
	 (format "readlink: cannot read link ~a -- ~~s" src)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-readlink src
	  :callback
	  (when (isa? callback JsFunction) readlink-callback)))
      (else
       (readlink-callback (not-implemented-exn "readlink")))))

;*---------------------------------------------------------------------*/
;*    nodejs-unlink ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-unlink %worker %this src callback)
   
   (define (unlink-callback res)
      (fs-callback %worker %this callback
	 (format "unlink: cannot unlink ~a -- ~~s" src)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-unlink src
	  :callback
	  (when (isa? callback JsFunction) unlink-callback)))
      (else
       (let ((r (delete-file src)))
	  (unlink-callback (if r 0 (fs-exn "cannot unlink ~a" r)))))))

;*---------------------------------------------------------------------*/
;*    nodejs-rmdir ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-rmdir %worker %this src callback)
   
   (define (rmdir-callback res)
      (fs-callback %worker %this callback
	 (format "rmdir: cannot rmdir ~a -- ~~s" src)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-rmdir src
	  :callback
	  (when (isa? callback JsFunction) rmdir-callback)))
      (else
       (let ((r (delete-directory src)))
	  (rmdir-callback (if r 0 (fs-exn "cannot rmdir ~a" r)))))))

;*---------------------------------------------------------------------*/
;*    nodejs-mkdir ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-mkdir %worker %this src mode callback)
   
   (define (mkdir-callback res)
      (fs-callback %worker %this callback
	 (format "mkdir: cannot mkdir ~a -- ~~s" src)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-mkdir src mode
	  :callback
	  (when (isa? callback JsFunction) mkdir-callback)))
      (else
       (let ((r (make-directory src)))
	  (mkdir-callback (if r 0 (fs-exn "cannot mkdir directory ~a" r)))))))

;* {*---------------------------------------------------------------------*} */
;* {*    nodejs-write ...                                                 *} */
;* {*---------------------------------------------------------------------*} */
;* (define (nodejs-write %worker %this fd buffer offset length position callback) */
;*    (cond-expand                                                     */
;*       (enable-libuv                                                 */
;*        (with-access::JsArrayBufferView buffer (%vec byteoffset)     */
;* 	  (uv-fs-write fd %vec length                                  */
;* 	     :callback                                                 */
;* 	     (when (isa? callback JsFunction)                          */
;* 		(lambda (obj)                                          */
;* 		   (js-worker-push-thunk! %worker                      */
;* 		      (lambda ()                                       */
;* 			 (if (<fx obj 0)                               */
;* 			     (js-call3 %this callback (js-undefined)   */
;* 				obj #f buffer)                         */
;* 			     (js-call3 %this callback (js-undefined)   */
;* 				#f obj buffer))))))                    */
;* 	     :offset (+fx offset (uint32->fixnum byteoffset))          */
;* 	     :position position :loop (uv-default-loop))))             */
;*       (else                                                         */
;*        (cond                                                        */
;* 	  ((output-port? fd)                                           */
;* 	   (when (integer? position)                                   */
;* 	      (set-input-port-position! fd position))                  */
;* 	   (with-access::JsArrayBufferView (%vec byteoffset)           */
;* 	      (let ((res (display-substring buffer                     */
;* 			    (+fx offset byteoffset) (+ byteoffset offset length) */
;* 			    fd)))                                      */
;* 		 (if (<fx res 0)                                       */
;* 		     (js-call3 %this callback (js-undefined) res #f buffer) */
;* 		     (js-call3 %this callback (js-undefined) #f res buffer))))) */
;* 	  (else                                                        */
;* 	   (let ((r (fs-exn "Illegal port ~a" fd)))                    */
;* 	      (if (isa? callback JsFunction)                           */
;* 		  (js-call3 %this callback (js-undefined) r #f buffer) */
;* 		  r)))))))                                             */

;*---------------------------------------------------------------------*/
;*    nodejs-read ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-read %worker %this fd buffer offset length position callback)
   (cond-expand
      (enable-libuv
       (with-access::JsArrayBufferView buffer (%vec byteoffset)
	  (uv-fs-read fd %vec length
	     :callback
	     (when (isa? callback JsFunction)
		(lambda (obj)
		   (js-worker-push-thunk! %worker
		      (lambda ()
			 (if (<fx obj 0)
			     (js-call2 %this callback (js-undefined) obj #f)
			     (js-call2 %this callback (js-undefined) #f obj))))))
	     :offset (+fx offset (uint32->fixnum byteoffset))
	     :position position
	     :loop (uv-default-loop))))
      (else
       (when (integer? position)
	  (set-input-port-position! fd position))
       (let ((buf (make-string length)))
	  (with-access::JsArrayBufferView buffer (%vec byteoffset)
	     (let ((res (read-fill-string! length (+fx offset byteoffset) length fd)))
		(if (<fx res 0)
		    (js-call2 %this callback (js-undefined) res #f)
		    (js-call2 %this callback (js-undefined) #f res))))))))

;*---------------------------------------------------------------------*/
;*    js-todouble ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-todouble num %this)
   (let ((num (js-tonumber num %this)))
      (if (fixnum? num)
	  (fixnum->flonum num)
	  num)))

;*---------------------------------------------------------------------*/
;*    nodejs-utimes ...                                                */
;*---------------------------------------------------------------------*/
(define (nodejs-utimes %worker %this path atime mtime callback)
   
   (define (utimes-callback res)
      (fs-callback %worker %this callback
	 (format "utimes: cannot utimes ~a -- ~~s" path)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-utime path (js-todouble atime %this) (js-todouble mtime %this)
	  :callback
	  (when (isa? callback JsFunction) utimes-callback)))
      (else
       (utimes-callback (not-implemented-exn "utimes")))))

;*---------------------------------------------------------------------*/
;*    nodejs-futimes ...                                               */
;*---------------------------------------------------------------------*/
(define (nodejs-futimes %worker %this fd atime mtime callback)
   
   (define (utimes-callback res)
      (fs-callback %worker %this callback
	 (format "utimes: cannot utimes ~a -- ~~s" fd)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-futime fd (js-todouble atime %this) (js-todouble mtime %this)
	  :callback
	  (when (isa? callback JsFunction) utimes-callback)))
      (else
       (utimes-callback (not-implemented-exn "futimes")))))

;*---------------------------------------------------------------------*/
;*    nodejs-fsync ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-fsync %worker %this fd callback)
   
   (define (fsync-callback res)
      (fs-callback %worker %this callback
	 (format "fsync: cannot fsync ~a -- ~~s" fd)
	 res))
   
   (cond-expand
      (enable-libuv
       (uv-fs-fsync fd 
	  :callback
	  (when (isa? callback JsFunction) fsync-callback)))
      (else
       (if (output-port? fd)
	   (begin
	      (flush-output-port fd)
	      (fsync-callback 0))
	   (fsync-callback (not-implemented-exn "futimes"))))))

;*---------------------------------------------------------------------*/
;*    nodejs-getaddrinfo ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-getaddrinfo %this process node family)
   (cond-expand
      (enable-libuv
       (with-access::JsGlobalObject %this (js-object)
	  (let ((wrap (js-new %this js-object)))
	     (uv-getaddrinfo node #f
		:family family
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
      (else
       (getaddrinfo-callback (not-implemented-exn "getaddrinfo")))))

;*---------------------------------------------------------------------*/
;*    process-fail ...                                                 */
;*---------------------------------------------------------------------*/
(define (process-fail %this process errno)
   
   (cond-expand
      ((not enable-libuv)
       (define (uv-err-name errno) (integer->string errno))))

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
	     (js-worker-push-thunk! %worker
		(lambda ()
		   (js-call2 %this cb (js-undefined)
		      #f v))))
	  (js-worker-push-thunk! %worker
	     (lambda ()
		(js-call2 %this cb (js-undefined)
		   res '#())))))
   
   (cond-expand
      (enable-libuv
       (with-access::JsGlobalObject %this (js-object)
	  (let ((res (uv-getaddrinfo node #f :family family
			:callback query-callback)))
	     (if (=fx res 0)
		 #t
		 (process-fail %this process res)))))
      (else
       (query-callback (not-implemented-exn "query")))))

;*---------------------------------------------------------------------*/
;*    nodejs-isip ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-isip addr)
   (cond-expand
      (enable-libuv
       (cond
	  ((uv-inet-pton addr :family 4) 4)
	  ((uv-inet-pton addr :family 6) 6)
	  (else 0)))
      (else
       (define ipv4-regexp
	  (pregexp "^([0-9]{1,3}[.]){3}[.]([0-9]{1,3})$"))
       
       (define ipv6-regexp
	  (pregexp "^\\s*((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:)))(%.+)?\\s*$"))
       (cond
	  ((pregexp-match ipv4-regexp domain) 4)
	  ((pregexp-match ipv6-regexp domain) 6)
	  (else 0)))))

;*---------------------------------------------------------------------*/
;*    nodejs-istty ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-istty fd)
   (cond-expand
      (enable-libuv
       (eq? (uv-guess-handle fd) 'TTY))
      (else
       (and (output-port? fd) (output-port-isatty? fd)))))

;*---------------------------------------------------------------------*/
;*    nodejs-guess-handle-type ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-guess-handle-type fd)
   (cond-expand
      (enable-libuv
       (symbol->string (uv-guess-handle fd)))
      (else
       (if (and (output-port? fd) (output-port-isatty? fd))
	   "TTY"
	   "UNKNOWN"))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-handle ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-handle)
   (cond-expand
      (enable-libuv
       (instantiate::UvTcp
	  (loop (uv-default-loop))))
      (else
       (error "nodejs-tcp-handle" "not implemented" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-write-queue-size ...                               */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-write-queue-size hdl)
   (cond-expand
      (enable-libuv
       (uv-stream-write-queue-size hdl))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-fd ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-fd hdl)
   (cond-expand
      (enable-libuv
       (uv-stream-fd hdl))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-connect ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-connect %worker %this handle host port family callback)
   (cond-expand
      (enable-libuv
       (uv-tcp-connect handle host port :family family
	  :callback (lambda (status handle)
		       (js-worker-push-thunk! %worker
			  (lambda ()
			     (callback status handle))))))
      (else
       (error "nodejs-tcp-connect" "not implemented" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-nodelay ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-nodelay handle enable)
   (cond-expand
      (enable-libuv
       (uv-tcp-nodelay handle enable))
      (else
       (error "nodejs-tcp-nodelay" "not implemented" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-keepalive ...                                         */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-keepalive handle enable timeout)
   (cond-expand
      (enable-libuv
       (uv-tcp-keepalive handle enable timeout))
      (else
       (error "nodejs-tcp-keepalive" "not implemented" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-simultaneous-accepts ...                              */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-simultaneous-accepts handle enable)
   (cond-expand
      (enable-libuv
       (uv-tcp-simultaneous-accepts handle enable))
      (else
       (error "nodejs-tcp-simultaneous-accepts" "not implemented" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-getsockname ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-getsockname %this handle)
   (cond-expand
      (enable-libuv
       (js-alist->jsobject (uv-tcp-getsockname handle) %this))
      (else
       (error "nodejs-tcp-getsockname" "not implemented" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-getpeername ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-getpeername %this handle)
   (cond-expand
      (enable-libuv
       (js-alist->jsobject (uv-tcp-getpeername handle) %this))
      (else
       (error "nodejs-tcp-getpeername" "not implemented" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-open ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-open %this handle fd)
   (cond-expand
      (enable-libuv
       (uv-tcp-open handle fd))
      (else
       (error "nodejs-tcp-open" "not implemented" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-bind ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-bind %this handle addr port family)
   (cond-expand
      (enable-libuv
       (uv-tcp-bind handle addr port :family family))
      (else
       (error "nodejs-tcp-bind" "not implemented" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-tcp-listen ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-tcp-listen %worker %this process this handle backlog)
   (cond-expand
      (enable-libuv
       (let ((r (uv-listen handle backlog :callback
		   (lambda (server status)
		      (js-worker-push-thunk! %worker
			 (lambda ()
			    (if (< status 0)
				(process-fail %this process status)
				(with-access::UvTcp server (loop)
				   (let ((client (instantiate::UvTcp (loop loop))))
				      (let ((r (uv-accept handle client)))
					 (if (< r 0)
					     (process-fail %this process r)
					     (let ((onconn (js-get this 'onconnection %this)))
						(js-call1 %this onconn this client)
						(js-undefined)))))))))))))
	  (if (<fx r 0)
	      (fs-errno-exn "Listen failed ~s" r %this)
	      r)))
      (else
       (error "nodejs-tcp-listen" "not implemented" #f))))
   
;*---------------------------------------------------------------------*/
;*    nodejs-stream-write ...                                          */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-write %worker %this handle buffer offset length callback)
   (cond-expand
      (enable-libuv
       (uv-stream-write handle buffer offset length
	  :callback (lambda (status)
		       (js-worker-push-thunk! %worker
			  (lambda ()
			     (callback status))))))
      (else
       (error "nodejs-stream-write" "not implemented" #f))))
   
;*---------------------------------------------------------------------*/
;*    nodejs-stream-read-start ...                                     */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-read-start %worker %this handle onalloc callback)
   (cond-expand
      (enable-libuv
       (uv-stream-read-start handle
	  :onalloc onalloc
	  :callback (lambda (buf offset len)
		       (js-worker-push-thunk! %worker
			  (lambda ()
			     (callback buf offset len))))))
      (else
       (error "nodejs-stream-read-start" "not implemented" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-read-stop ...                                      */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-read-stop %this handle)
   (cond-expand
      (enable-libuv
       (uv-stream-read-stop handle))
      (else
       (error "nodejs-stream-read-stop" "not implemented" #f))))

;*---------------------------------------------------------------------*/
;*    nodejs-stream-shutdown ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-stream-shutdown %worker %this handle callback)
   (cond-expand
      (enable-libuv
       (uv-stream-shutdown handle
	  :callback (lambda (status handle)
		       (js-worker-push-thunk! %worker
			  (lambda ()
			     (callback status handle))))))
      (else
       (error "nodejs-stream-shutdown" "not implemented" #f))))
   
;*---------------------------------------------------------------------*/
;*    hopscript binding                                                */
;*---------------------------------------------------------------------*/
(cond-expand
   (enable-libuv

;;;
(define-method (js-toprimitive obj::UvFile preferredtype %this::JsGlobalObject)
   (with-access::UvFile obj (fd)
      fd))

;;;
(define-method (js-inspect obj::UvFile cnt)
   (with-access::UvFile obj (fd)
      fd))

(define-method (js-object-tostring obj::UvFile %this::JsGlobalObject)
   (with-access::UvFile obj (fd)
      (integer->string fd)))
))

