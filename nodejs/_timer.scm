;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_timer.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 15:01:14 2014                          */
;*    Last change :  Sun Oct 12 09:37:37 2014 (serrano)                */
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
	      (count::int (default 0))
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
			  (set! proc p)
			  (nodejs-timer-callback-set! timer
			     (lambda (timer status)
				(js-worker-push-thunk! worker
				   (lambda ()
				      (when (isa? proc JsFunction)
					 (js-call0 %this proc obj))))))))
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
		   (with-access::JsTimer this (timer worker count)
		      (set! count (+fx 1 count))
		      (nodejs-timer-start worker timer count
			 (js-touint32 start %this) (js-touint32 rep %this))))
		2 "start"))
   (js-bind! %this obj 'close
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer worker count)
		      (nodejs-timer-close worker timer count)
		      (set! count 0)))
		0 "close"))
   (js-bind! %this obj 'stop
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer worker count)
		      (nodejs-timer-stop worker timer count)))
		0 "stop"))
   (js-bind! %this obj 'unref
      :value (js-make-function %this
		(lambda (this)
		   (with-access::JsTimer this (timer worker count)
		      (nodejs-timer-unref worker timer count)))
		0 "unref")))
					
