;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/_process_wrap.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 17 17:07:03 2014                          */
;*    Last change :  Fri Feb 23 08:41:22 2024 (serrano)                */
;*    Copyright   :  2014-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Nodejs child processes bindings                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__process-wrap

   (include "../hopscript/stringthread.sch")
   
   (library hopscript)

   (include "nodejs_types.sch")

   (import  __nodejs_uv
	    __nodejs__process)

   (export (process-process-wrap ::WorkerHopThread ::JsGlobalObject ::JsObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    process-process-wrap ...                                         */
;*---------------------------------------------------------------------*/
(define (process-process-wrap %worker %this process)
   
   (define process-prototype
      (with-access::JsGlobalObject %this (js-object)
	 (js-new %this js-object)))

   (set! __js_strings (&init!))
   
   ;; bind the methods of the prototype object
   (js-put! process-prototype (& "spawn")
      (js-make-function %this 
	 (lambda (this options)
	    (nodejs-process-spawn %worker %this process this options))
	 (js-function-arity 1 0)
	 (js-function-info :name "spawn" :len 1))
      #f %this)
   
   (js-put! process-prototype (& "kill")
      (js-make-function %this 
	 (lambda (this pid)
	    (nodejs-process-kill %worker %this process this pid))
	 (js-function-arity 1 0)
	 (js-function-info :name "kill" :len 1))
      #f %this)
   
   (js-put! process-prototype (& "close")
      (js-make-function %this
	 (lambda (this cb)
	    (nodejs-close %worker %this process this cb))
	 (js-function-arity 1 0)
	 (js-function-info :name "close" :len 1))
      #f %this)
   
   (js-put! process-prototype (& "ref")
      (js-make-function %this
	 (lambda (this)
	    (with-access::JsHandle this (handle)
	       (nodejs-ref handle %worker)))
	 (js-function-arity 0 0)
	 (js-function-info :name "ref" :len 0))
      #f %this)
	    
   (js-put! process-prototype (& "unref")
      (js-make-function %this
	 (lambda (this)
	    (with-access::JsHandle this (handle)
	       (nodejs-unref handle %worker)))
	 (js-function-arity 0 0)
	 (js-function-info :name "unref" :len 0))
      #f %this)
   
   (with-access::JsGlobalObject %this (js-object)
      (js-alist->jsobject
	 `((Process . ,(js-make-function %this
			  (lambda (this) this)
			  (js-function-arity 0 0)
			  (js-function-info :name "Process" :len 1)
			  :alloc (lambda (%this o)
				    (instantiateJsHandle
				       (handle (nodejs-new-process))
				       (__proto__ process-prototype))))))
	 %this)))



;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

