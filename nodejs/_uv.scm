;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_uv.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 14 05:50:28 2014                          */
;*    Last change :  Thu May 15 16:42:36 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Compatibility kit when libuv is not available.                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__uv

   (library hop hopscript)
   
   (import __nodejs_uv)
   
   (cond-expand
      ((not enable-libuv)
       (static (class %JsTimer
		  (cb (default #unspecified))))))

   (cond-expand
      ((not enable-libuv)
       (export (%nodejs-event-loop)))))

(cond-expand
   ((not enable-libuv)

;*---------------------------------------------------------------------*/
;*    %nodejs-event-loop ...                                           */
;*---------------------------------------------------------------------*/
(define (%nodejs-event-loop)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    %nodejs-make-timer ...                                           */
;*---------------------------------------------------------------------*/
(define (%nodejs-make-timer)
   (instantiate::%JsTimer))

;*---------------------------------------------------------------------*/
;*    %nodejs-timer-callback-set! ...                                  */
;*---------------------------------------------------------------------*/
(define (%nodejs-timer-callback-set! timer proc)
   (with-access::%JsTimer timer (cb)
      (set! cb proc)))

;*---------------------------------------------------------------------*/
;*    %nodejs-timer-start ...                                          */
;*---------------------------------------------------------------------*/
(define (%nodejs-timer-start timer start rep)
   (thread-start!
      (instantiate::hopthread
	 (body (lambda ()
		  (sleep (* 1000 start))
		  (let loop ()
		     (with-access::%JsTimer timer (cb)
			(when (procedure? cb) (cb timer 0))
			(if rep
			    (sleep (* 1000 rep))
			    (loop)))))))))

;*---------------------------------------------------------------------*/
;*    %nodejs-timer-close ...                                          */
;*---------------------------------------------------------------------*/
(define (%nodejs-timer-close timer)
   (with-access::%JsTimer timer (cb)
      (set! cb #unspecified)))
			    
	    
))
   

