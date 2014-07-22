;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/uv.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 14 05:42:05 2014                          */
;*    Last change :  Tue Jul 22 10:40:32 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    NodeJS libuv binding                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_uv

   (library hop)
   
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
	   (nodejs-read ::input-port ::bstring ::long ::long ::long ::obj)))

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
       (uv-rename-file oldp newp cb (uv-default-loop)))
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
;*    nodejs-read ...                                                  */
;*---------------------------------------------------------------------*/
(define (nodejs-read fd buffer offset length position cb)
   (cond-expand
      (enable-libuv
       (tprint "uf-fs-read..." cb)
       (uv-fs-read fd buffer length cb
	  :offset offset :position position :loop (uv-default-loop)))
      (else
       (when (integer? position)
	  (set-input-port-position! fd position))
       (let ((fast-buffer (js-get buffer '%fast-buffer %this)))
	  (let ((res (read-fill-string! fast-buffer offset length fd)))
	     (if (<fx res 0)
		 (cb res (js-undefined))
		 (cb 0 res fast-buffer)))))))
