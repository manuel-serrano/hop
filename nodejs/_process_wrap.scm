;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_process_wrap.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 17 17:07:03 2014                          */
;*    Last change :  Wed Jan 28 07:01:20 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Nodejs child processes bindings                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__process-wrap

   (library hopscript)

   (import  __nodejs_uv
	    __nodejs_process)

   (export (process-process-wrap ::WorkerHopThread ::JsGlobalObject ::JsObject)))

;*---------------------------------------------------------------------*/
;*    process-process-wrap ...                                         */
;*---------------------------------------------------------------------*/
(define (process-process-wrap %worker %this process)
   
   (define process-prototype
      (with-access::JsGlobalObject %this (js-object)
	 (js-new %this js-object)))
   
   ;; bind the methods of the prototype object
   (js-put! process-prototype 'spawn
      (js-make-function %this 
	 (lambda (this options)
	    (nodejs-process-spawn %worker %this process this options))
	 1 'spawn)
      #f %this)
   
   (js-put! process-prototype 'kill
      (js-make-function %this 
	 (lambda (this pid)
	    (nodejs-process-kill %worker %this process this pid))
	 1 'kill)
      #f %this)
   
   (js-put! process-prototype 'close
      (js-make-function %this
	 (lambda (this cb)
	    (nodejs-close %worker %this process this cb))
	 1 "close")
      #f %this)
   
   (js-put! process-prototype 'ref
      (js-make-function %this
	 (lambda (this)
	    (with-access::JsHandle this (handle)
	       (nodejs-ref handle %worker)))
	 0 "ref")
      #f %this)
	    
   (js-put! process-prototype 'unref
      (js-make-function %this
	 (lambda (this)
	    (with-access::JsHandle this (handle)
	       (nodejs-unref handle %worker)))
	 0 "unref")
      #f %this)
   
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 `((Process . ,(js-make-function %this
			  (lambda (this) this)
			  1 'Process
			  :alloc (lambda (o)
				    (instantiate::JsHandle
				       (handle (nodejs-new-process))
				       (__proto__ process-prototype))))))
	 %this)))



