;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/uv.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 14 05:42:05 2014                          */
;*    Last change :  Wed Jul 23 16:24:54 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    NodeJS libuv binding                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_uv

   (library hop hopscript)
   
   (cond-expand
      (enable-libuv (library libuv))
      (else (import __nodejs__uv)))

   (export (nodejs-event-loop)
	   (nodejs-make-timer)
	   (nodejs-timer-callback-set! ::obj ::procedure)
	   (nodejs-timer-start ::obj ::uint32 ::uint32)
	   (nodejs-timer-close ::obj)
	   (nodejs-timer-stop ::obj)
	   (nodejs-timer-unref ::obj)
	   (nodejs-rename-file ::JsGlobalObject ::bstring ::bstring ::obj)
	   (nodejs-ftruncate ::JsGlobalObject ::obj ::int ::obj)
	   (nodejs-truncate ::JsGlobalObject ::bstring ::int ::obj)
	   (nodejs-fchown ::JsGlobalObject ::obj ::int ::int ::obj)
	   (nodejs-chown ::JsGlobalObject ::bstring ::int ::int ::obj)
	   (nodejs-loadavg ::u8vector)
	   (nodejs-getfreemem::double)
	   (nodejs-gettotalmem::double)
	   (nodejs-getcpus::vector)
	   (nodejs-open ::JsGlobalObject ::bstring ::long ::long ::obj)
	   (nodejs-close ::JsGlobalObject ::obj ::obj)
	   (nodejs-fstat ::JsGlobalObject ::obj ::obj)
	   (nodejs-read ::JsGlobalObject ::obj ::bstring ::long ::long ::long ::obj)))

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
		       (synchronize uv-mutex
			  (for-each (lambda (action) (action)) uv-actions)
			  (set! uv-actions '()))))))
	  (uv-run loop)))
      (else
       (%nodejs-event-loop))))

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
       (synchronize uv-mutex
	  (set! uv-actions
	     (cons (lambda ()
		      (uv-timer-start timer
			 (llong->uint64 (uint32->llong start))
			 (llong->uint64 (uint32->llong rep))))
		uv-actions))
	  (uv-async-send uv-async)))
      (else
       (%nodejs-timer-start timer start rep))))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-close ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-close timer)
   (cond-expand
      (enable-libuv
       (synchronize uv-mutex
	  (set! uv-actions
	     (cons (lambda () (uv-close timer))
		uv-actions))
	  (uv-async-send uv-async)))
      (else
       (%nodejs-timer-close timer))))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-stop ...                                            */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-stop timer)
   (cond-expand
      (enable-libuv
       (synchronize uv-mutex
	  (set! uv-actions
	     (cons (lambda () (uv-timer-stop timer))
		uv-actions))
	  (uv-async-send uv-async)))
      (else
       (%nodejs-timer-stop timer))))

;*---------------------------------------------------------------------*/
;*    nodejs-timer-unref ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-timer-unref timer)
   (cond-expand
      (enable-libuv
       (synchronize uv-mutex
	  (set! uv-actions
	     (cons (lambda () (uv-unref timer))
		uv-actions))
	  (uv-async-send uv-async)))
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
;*    not-implemented-exn ...                                          */
;*---------------------------------------------------------------------*/
(define (not-implemented-exn fun %this)
   (with-access::JsGlobalObject %this (js-error)
      (js-new %this js-error (format "~a not implemented" fun))))

;*---------------------------------------------------------------------*/
;*    fs-exn ...                                                       */
;*---------------------------------------------------------------------*/
(define (fs-exn fmt errno %this)
   
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
;*    nodejs-rename-file ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-rename-file %this oldp newp callback)
   
   (define (rename-callback res path)
      (if (=fx res 0)
	  (js-call1 %this callback (js-undefined) #f)
	  (let ((exn (fs-exn
			(format "rename: cannot rename file ~s into ~s -- ~~s"
			   oldp newp)
			res %this)))
	     (js-call1 %this callback  (js-undefined) exn))))
   
   (cond-expand
      (enable-libuv
       (uv-fs-rename oldp newp
	  :callback
	  (when (isa? callback JsFunction) rename-callback)))
      (else
       (let ((res (rename-file oldp newp)))
	  (if (isa? callback JsFunction)
	      (rename-callback res oldp)
	      res)))))

;*---------------------------------------------------------------------*/
;*    nodejs-ftruncate ...                                             */
;*---------------------------------------------------------------------*/
(define (nodejs-ftruncate %this fd offset callback)
   
   (define (ftruncate-callback res)
      (if (=fx res 0)
	  (js-call1 %this callback (js-undefined) #f)
	  (let ((exn (fs-exn
			(format "ftruncate: cannot truncate ~a to ~a -- ~~s"
			   fd offset)
			res %this)))
	     (js-call1 %this callback (js-undefined) exn))))
   
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
(define (nodejs-truncate %this path offset callback)
   
   (define (truncate-callback res)
      (if (=fx res 0)
	  (js-call1 %this callback (js-undefined) #f)
	  (let ((exn (fs-exn
			(format "truncate: cannot truncate ~a to ~a -- ~s"
			   path offset)
			res %this)))
	     (js-call1 %this callback (js-undefined) exn))))
   
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
(define (nodejs-fchown %this fd uid guid callback)
   
   (define (fchown-callback res)
      (if (=fx res 0)
	  (js-call1 %this callback (js-undefined) #f)
	  (let ((exn (fs-exn
			(format "fchown: cannot chown ~a, ~a, ~a -- ~~s"
			   fd uid guid)
			res %this)))
	     (js-call1 %this callback (js-undefined) exn))))
   
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
(define (nodejs-chown %this fd uid guid callback)
   
   (define (chown-callback res)
      (if (=fx res 0)
	  (js-call1 %this callback (js-undefined) #f)
	  (let ((exn (fs-exn
			(format "chown: cannot chown ~a, ~a, ~a -- ~~s"
			   fd uid guid)
			res %this)))
	     (js-call1 %this callback (js-undefined) exn))))
   
   (cond-expand
      (enable-libuv
       (uv-fs-chown fd uid guid
	  :callback
	  (when (isa? callback JsFunction) chown-callback)))
      (else
       (chown-callback (not-implemented-exn "chown")))))

;*---------------------------------------------------------------------*/
;*    nodejs-open ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-open %this path flags mode callback)

   (define (open-callback res)
      (if (isa? res UvFile)
	  (js-call2 %this callback (js-undefined) #f res)
	  (let ((exn (fs-exn
			(format "open: cannot open file ~a, ~a, ~a -- ~~s"
			   path flags mode)
			res %this)))
	     (js-call2 %this callback (js-undefined) exn #f))))
   
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
;*    nodejs-close ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-close %this fd callback)
   (cond-expand
      (enable-libuv
       (uv-fs-close fd :callback
	  (when (isa? callback JsFunction)
	     (lambda (val)
		(js-call1 %this callback (js-undefined) val)))))
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
;*    nodejs-fstat ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-fstat %this fd callback)
   (cond-expand
      (enable-libuv
       (if (isa? callback JsFunction)
	   (uv-fs-fstat fd :callback
	      (lambda (val)
		 (if (integer? val)
		     (js-call2 %this callback (js-undefined)
			(fs-exn (format "fstat, cannot stat ~a -- ~~s" fd)
			   val %this)
			#f)
		     (let ((stat (js-alist->jsobject (stat-date val %this) %this)))
			(js-call2 %this callback (js-undefined) #f stat)))))
	   (let ((val (uv-fs-fstat fd)))
	      (if (integer? val)
		  val
		  (js-alist->jsobject (stat-date val %this) %this)))))
      (else
       ((input-port? fd)
	(let ((obj (js-alist->jsobject
		      `((size . ,(elong->fixnum (input-port-length fd))))
		      %this)))
	   (with-access::JsObject obj (__proto__)
	      (set! __proto__ (get-process-fs-fstats %this))
	      (if (isa? callback JsFunction)
		  (js-call2 %this callback (js-undefined) #f obj)
		  obj))))
       ((isa? callback JsFunction)
	(js-call2 %this callback (js-undefined) "Not a file descriptor" #f))
       (else
	#f))))

;*---------------------------------------------------------------------*/
;*    nodejs-read ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-read %this fd buffer offset length position callback)
   (cond-expand
      (enable-libuv
       (let ((fast-buffer (js-get buffer '%fast-buffer %this)))
	  (uv-fs-read fd fast-buffer length
	     :callback
	     (when (isa? callback JsFunction)
		(lambda (obj)
		   (if (<fx obj 0)
		       (js-call3 %this callback (js-undefined) obj #f buffer)
		       (js-call3 %this callback (js-undefined) #f obj buffer))))
	     :offset offset :position position :loop (uv-default-loop))))
      (else
       (when (integer? position)
	  (set-input-port-position! fd position))
       (let ((fast-buffer (js-get buffer '%fast-buffer %this)))
	  (let ((res (read-fill-string! fast-buffer offset length fd)))
	     (if (<fx res 0)
		 (js-call3 %this callback (js-undefined) res #f buffer)
		 (js-call3 %this callback (js-undefined) #f res buffer)))))))

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
