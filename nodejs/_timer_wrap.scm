;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_timer_wrap.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 15:01:14 2014                          */
;*    Last change :  Thu Feb 19 11:30:29 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop Timer                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__timer-wrap

   (library hopscript)

   (import __nodejs_uv)
   
   (static (class JsTimer::JsObject
	      (worker::WorkerHopThread read-only)
	      (timer (default #f))
	      (proc (default #f))
	      (marked (default #f))))

   (export (hopjs-process-timer ::WorkerHopThread ::JsGlobalObject ::JsObject)))

;*---------------------------------------------------------------------*/
;*    hopjs-process-timer ...                                          */
;*---------------------------------------------------------------------*/
(define (hopjs-process-timer %worker %this process)
   
   (define js-timer-prototype
      (instantiate::JsObject))
   
   (init-timer-prototype! %this js-timer-prototype)
   
   (define Timer
      (js-make-function %this 
	 (lambda (this) this)
	 0 "Timer"
	 :prototype js-timer-prototype
	 :construct (js-timer-construct! %worker %this process js-timer-prototype)))

   (js-bind! %this Timer 'now
      :value (js-make-function %this
		(lambda (this)
		   (nodejs-now %worker))
		0 "now")
      :writable #f)
   
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 `((Timer . ,Timer))
	 %this)))

;*---------------------------------------------------------------------*/
;*    js-timer-construct! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-timer-construct! %worker %this::JsGlobalObject process js-timer-prototype)
   (lambda (_)
      (let ((obj (instantiate::JsTimer
		    (__proto__ js-timer-prototype)
		    (worker (js-current-worker)))))
	 (with-access::JsTimer obj (timer)
	    (set! timer (nodejs-make-timer %worker %this process obj)))
	 obj)))

;*---------------------------------------------------------------------*/
;*    init-timer-prototype! ...                                        */
;*---------------------------------------------------------------------*/
(define (init-timer-prototype! %this::JsGlobalObject obj)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "timer_wrap" "binding not implemented" name))
	 0 (symbol->string name)))
   
   (js-bind! %this obj 'start
      :value (js-make-function %this
		(lambda (this start rep)
		   (with-access::JsTimer this (timer worker)
		      (nodejs-timer-start worker timer start rep)))
		2 "start"))
   (js-bind! %this obj 'close
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer worker)
		      (nodejs-timer-close worker timer)))
		0 "close"))
   (js-bind! %this obj 'stop
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer worker)
		      (nodejs-timer-stop worker timer)))
		0 "stop"))
   (js-bind! %this obj 'unref
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer worker)
		      (nodejs-timer-unref worker timer)))
		0 "unref"))

   (for-each (lambda (id)
		(js-bind! %this obj id
		   :value (not-implemented id)))
      '(setRepeat getRepeat again)))
