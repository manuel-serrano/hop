;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/uv.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 14 05:42:05 2014                          */
;*    Last change :  Thu Jul 10 15:17:16 2014 (serrano)                */
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
	   (nodejs-rename-file ::bstring ::bstring ::procedure)))

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
	  
