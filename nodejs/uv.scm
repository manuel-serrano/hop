;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/uv.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 14 05:42:05 2014                          */
;*    Last change :  Wed Jul 23 12:41:23 2014 (serrano)                */
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
	   (nodejs-rename-file ::bstring ::bstring ::procedure)
	   (nodejs-loadavg ::u8vector)
	   (nodejs-getfreemem::double)
	   (nodejs-gettotalmem::double)
	   (nodejs-getcpus::vector)
	   (nodejs-open ::JsGlobalObject ::bstring ::long ::long ::obj)
	   (nodejs-close ::JsGlobalObject ::obj ::obj)
	   (nodejs-fstat ::JsGlobalObject ::obj ::obj)
	   (nodejs-read ::JsGlobalObject ::obj ::bstring ::long ::long ::long ::obj)))

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
;*    nodejs-rename-file ...                                           */
;*---------------------------------------------------------------------*/
(define (nodejs-rename-file oldp newp cb)
   (cond-expand
      (enable-libuv
       (uv-fs-rename oldp newp :callback cb))
      (else
       (if (rename-file oldp newp)
	   (cb (js-undefined))
	   (cb newp #f)))))
	  
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
;*    nodejs-open ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-open %this path flags mode callback)
   (cond-expand
      (enable-libuv
       (uv-fs-open path
	  flags
	  :mode mode
	  :loop (uv-default-loop)
	  :callback (when (isa? callback JsFunction)
		       (lambda (obj)
			  (if (number? obj)
			      (js-call2 %this callback (js-undefined) obj #f)
			      (js-call2 %this callback (js-undefined) #f obj))))))
      (else
       (let ((ip (cond
		    ((not (integer? flags))
		     (error "open" "wrong flag" flags))
		    ((=fx flags O_RDONLY)
		     (open-input-file path))
		    ((=fx flags O_WRONLY)
		     (open-output-file path))
		    ((=fx flags O_APPEND)
		     (append-output-file path))
		    (else
		     (error "open" "flags not implemented" flags)))))
	  (if (isa? callback JsFunction)
	      (if ip
		  (js-call2 %this callback (js-undefined) #f ip)
		  (js-call2 %this callback (js-undefined) "cannot open file" #f))
	      ip)))))

;*---------------------------------------------------------------------*/
;*    nodejs-close ...                                                 */
;*---------------------------------------------------------------------*/
(define (nodejs-close %this fd callback)
   (cond-expand
      (enable-libuv
       (if (isa? callback JsFunction)
	   (uv-fs-close fd :callback
	      (lambda (val)
		 (js-call1 %this callback (js-undefined) val)))
	   (uv-fs-close fd)))
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
		     (js-call2 %this callback (js-undefined) val #f)
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
(define (nodejs-read %this fd buffer offset length position cb)
   (cond-expand
      (enable-libuv
       (uv-fs-read fd buffer length
	  :callback
	  (lambda (obj)
	     (if (<fx obj 0)
		 (js-call3 %this cb (js-undefined) obj #f buffer)
		 (js-call3 %this cb (js-undefined) #f obj buffer)))
	  :offset offset :position position :loop (uv-default-loop)))
      (else
       (when (integer? position)
	  (set-input-port-position! fd position))
       (let ((fast-buffer (js-get buffer '%fast-buffer %this)))
	  (let ((res (read-fill-string! fast-buffer offset length fd)))
	     (if (<fx res 0)
		 (js-call3 %this cb (js-undefined) res #f buffer)
		 (js-call3 %this cb (js-undefined) #f res buffer)))))))

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
