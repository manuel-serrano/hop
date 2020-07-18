;*=====================================================================*/
;*    /tmp/HOPNEW/hop/nodejs/_timer_wrap.scm                           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 15:01:14 2014                          */
;*    Last change :  Sun Feb 23 15:13:57 2020 (serrano)                */
;*    Copyright   :  2014-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop Timer                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__timer-wrap

   (include "../hopscript/stringthread.sch")
   
   (library hopscript)

   (import __nodejs_uv)
   
   (static (class JsTimer::JsObject
	      (worker::WorkerHopThread read-only)
	      (timer (default #f))
	      (proc (default #f))
	      (marked (default #f))))

   (export (hopjs-process-timer ::WorkerHopThread ::JsGlobalObject ::JsObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    constructors                                                     */
;*---------------------------------------------------------------------*/
(define-instantiate JsTimer)

;*---------------------------------------------------------------------*/
;*    hopjs-process-timer ...                                          */
;*---------------------------------------------------------------------*/
(define (hopjs-process-timer %worker %this process)

   (define __js_strings_execute_first!
      (set! __js_strings (&init!)))
   
   (define js-timer-prototype
      (instantiateJsObject
	 (__proto__ (js-object-proto %this))
	 (elements ($create-vector 1))))

   (init-timer-prototype! %this js-timer-prototype)
   
   (define Timer
      (js-make-function %this 
	 (lambda (this) this)
	 (js-function-arity 0 0)
	 (js-function-info :name "Timer" :len 0)
	 :prototype js-timer-prototype
	 :alloc js-no-alloc
	 :construct (js-timer-construct! %worker %this process js-timer-prototype)))

   (js-bind! %this Timer (& "now")
      :value (js-make-function %this
		(lambda (this)
		   (nodejs-now %worker))
		(js-function-arity 0 0)
		(js-function-info :name "now" :len 0))
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
      (let ((obj (instantiateJsTimer
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
	 (js-function-arity 0 )
	 (js-function-info :name name :len 0)))
   
   (js-bind! %this obj (& "start")
      :value (js-make-function %this
		(lambda (this start rep)
		   (with-access::JsTimer this (timer worker)
		      (nodejs-timer-start worker timer start rep)))
		(js-function-arity 2 0)
		(js-function-info :name "start" :len 2)))
   (js-bind! %this obj (& "close")
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer worker)
		      (nodejs-timer-close worker timer)))
		(js-function-arity 0 0)
		(js-function-info :name "close" :len 0)))
   (js-bind! %this obj (& "stop")
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer worker)
		      (nodejs-timer-stop worker timer)))
		(js-function-arity 0 0)
		(js-function-info :name "stop" :len 0)))
   (js-bind! %this obj (& "unref")
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer worker)
		      (nodejs-timer-unref worker timer)))
		(js-function-arity 0 0)
		(js-function-info :name "unref" :len 0)))

   (for-each (lambda (id)
		(js-bind! %this obj (js-ascii-name->jsstring id)
		   :value (not-implemented id)))
      '("setRepeat" "getRepeat" "again")))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

