;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_timer.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 15:01:14 2014                          */
;*    Last change :  Thu Jun  5 18:19:08 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop Timer                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__timer

   (library hopscript)

   (import __nodejs_uv)
   
   (static (class JsTimer::JsObject
	      (worker::WorkerHopThread read-only)
	      (timer read-only)
	      (proc (default #f))))

   (export (hopjs-process-timer ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    define-js ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (define-js name arity proc . opt)
   `(cons ',name
       (js-make-function %this ,proc ,arity ,(symbol->string name) ,@opt)))
		 
;*---------------------------------------------------------------------*/
;*    hopjs-process-timer ...                                          */
;*---------------------------------------------------------------------*/
(define (hopjs-process-timer %this)
   
   (define js-timer-prototype
      (instantiate::JsObject))
   
   (init-timer-prototype! %this js-timer-prototype)
   
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 (list
	    ;; Timer object
	    (define-js Timer 0
	       (lambda (this) this)
	       :prototype js-timer-prototype
	       :construct (js-timer-construct! %this js-timer-prototype)))
	 %this)))

;*---------------------------------------------------------------------*/
;*    js-timer-construct! ...                                          */
;*---------------------------------------------------------------------*/
(define (js-timer-construct! %this::JsGlobalObject js-timer-prototype)
   (lambda (_)
      (let ((obj (instantiate::JsTimer
		    (__proto__ js-timer-prototype)
		    (worker (js-current-worker))
		    (timer (nodejs-make-timer)))))
	 ;; ontimeout
	 (js-bind! %this obj 'ontimeout
	    :get (js-make-function %this
		    (lambda (this)
		       (with-access::JsTimer this (proc)
			  proc))
		    1 "ontimeout")
	    :set (js-make-function %this
		    (lambda (this p)
		       (with-access::JsTimer this (timer proc worker)
			  (unless (isa? p JsFunction)
			     (js-raise-type-error %this
				"ontimeout: not a function ~s" p))
			  (set! proc p)
			  (nodejs-timer-callback-set! timer
			     (lambda (timer status)
				(js-worker-push-thunk! worker
				   (lambda ()
				      (js-call0 %this p obj)))))))
		    2 "ontimeout"))
	 ;; returns the newly allocated object
	 obj)))

;*---------------------------------------------------------------------*/
;*    init-timer-prototype! ...                                        */
;*---------------------------------------------------------------------*/
(define (init-timer-prototype! %this::JsGlobalObject obj)
   (js-bind! %this obj 'start
      :value (js-make-function %this
		(lambda (this start rep)
		   (with-access::JsTimer this (timer)
		      (nodejs-timer-start timer
			 (js-touint32 start %this) (js-touint32 rep %this))))
		2 "start"))
   (js-bind! %this obj 'close
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer)
		      (nodejs-timer-close timer)))
		0 "close"))
   (js-bind! %this obj 'stop
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer)
		      (nodejs-timer-stop timer)))
		0 "stop"))
   (js-bind! %this obj 'unref
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer)
		      (nodejs-timer-unref timer)))
		0 "unref")))
					
