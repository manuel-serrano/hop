;*=====================================================================*/
;*    /tmp/HOPNEW/hop/nodejs/_evals.scm                                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct 22 13:35:17 2014                          */
;*    Last change :  Sun Feb 23 15:12:14 2020 (serrano)                */
;*    Copyright   :  2014-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    VM bindings                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__evals

   (include "../hopscript/stringthread.sch")
   
   (library hopscript)

   (import __nodejs_require)
   
   (static (class JsScript::JsObject
	      code::obj
	      ctx::obj
	      filename::obj))
	   
   (export (process-evals ::WorkerHopThread ::JsGlobalObject)))


;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    constructors                                                     */
;*---------------------------------------------------------------------*/
(define-instantiate JsScript)

;*---------------------------------------------------------------------*/
;*    process-evals ...                                                */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/vm.html                                    */
;*---------------------------------------------------------------------*/
(define (process-evals %worker %this)

   (define _ (set! __js_strings (&init!)))

   (define (%createContext this obj)
      (let ((this (js-new-global-object)))
	 (when (js-object? obj)
	    (js-for-in obj
	       (lambda (p %this)
		  (let* ((s (js-jsstring->string p))
			 (y (string->symbol s)))
		     (js-bind! %this this y
			:get (js-make-function %this
				(lambda (this)
				   (js-get obj y %this))
				(js-function-arity 0 0)
				(js-function-info :name s :len 0))
			:set (js-make-function %this
				(lambda (this v)
				   (js-put! obj y v #f %this))
				(js-function-arity 1 0)
				(js-function-info :name s :len 1)))))
	       %this))
	 this))

   (define (call-with-context %this this ctx proc)
      (let* ((%ctx (%createContext this ctx))
	     (prev '()))
	 ;; get all pre-bound variables
	 (js-for-in %ctx (lambda (p %this) (set! prev (cons p prev))) %this)
	 (let ((res (proc %ctx)))
	    (when (js-object? ctx)
	       (js-for-in %ctx
		  (lambda (p %this)
		     (unless (member p prev)
			(js-put! ctx p (js-get %ctx p %this) #f %this)))
		  %this))
	    res)))
   
   (define createContext
      (js-make-function %this %createContext
	 (js-function-arity %createContext)
	 (js-function-info :name "createContext" :len 1)))
   
   (define runInContext
      (js-make-function %this
	 (lambda (this ctx)
	    (if (isa? ctx JsGlobalObject)
		(with-access::JsScript this (code filename)
		   (call-with-input-string (js-jsstring->string code)
		      (lambda (ip)
			 (input-port-name-set! ip filename)
			 (%js-eval ip 'eval ctx this ctx))))
		(js-raise-type-error %this
		   "needs a 'context' argument. ~s" ctx)))
	 (js-function-arity 1 0)
	 (js-function-info :name "runInContext" :len 1)))

   (define runInContextVM
      (js-make-function %this
	 (lambda (this code ctx filename)
	    (if (isa? ctx JsGlobalObject)
		(call-with-input-string (js-jsstring->string code)
		   (lambda (ip)
		      (input-port-name-set! ip (js-tostring filename %this))
		      (%js-eval ip 'eval ctx this ctx)))
		(js-raise-type-error %this
		   "needs a 'context' argument. ~s" ctx)))
	 (js-function-arity 3 0)
	 (js-function-info :name "runInContext" :len 3)))

   (define runInNewContext
      (js-make-function %this
	 (lambda (this ctx)
	    (with-access::JsScript this (code filename)
	       (call-with-input-string (js-jsstring->string code)
		  (lambda (ip)
		     (input-port-name-set! ip filename)
		     (call-with-context %this this ctx
			(lambda (%ctx)
			   (%js-eval ip 'eval %ctx this %ctx)))))))
	 (js-function-arity 1 0)
	 (js-function-info :name "runInNewContext" :len 1)))

   (define runInNewContextVM
      (js-make-function %this
	 (lambda (this code ctx filename)
	    (call-with-input-string (js-jsstring->string code)
	       (lambda (ip)
		  (input-port-name-set! ip (js-tostring filename %this))
		  (call-with-context %this this ctx
		     (lambda (%ctx)
			(%js-eval ip 'eval %ctx this %ctx))))))
	 (js-function-arity 3 0)
	 (js-function-info :name "runInNewContext" :len 3)))

   (define runInThisContext
      (js-make-function %this
	 (lambda (this)
	    (with-access::JsScript this (code filename)
	       (call-with-input-string (js-jsstring->string code)
		  (lambda (ip)
		     (input-port-name-set! ip filename)
		     (%js-eval ip 'eval %this this %this)))))
	 (js-function-arity 0 0)
	 (js-function-info :name "runInThisContext" :len 1)))

   (define runInThisContextVM
      (js-make-function %this
	 (lambda (this code filename)
	    (call-with-input-string (js-jsstring->string code)
	       (lambda (ip)
		  (input-port-name-set! ip (js-tostring filename %this))
		  (%js-eval ip 'eval %this this %this))))
	 (js-function-arity 2 0)
	 (js-function-info :name "runInThisContext" :len 2)))

   (define nodescript-proto
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new0 %this js-object)))
	    (js-put! obj (& "createContext") createContext #f %this)
	    (js-put! obj (& "runInContext") runInContext #f %this)
	    (js-put! obj (& "runInThisContext") runInThisContext #f %this)
	    (js-put! obj (& "runInNewContext") runInNewContext #f %this)
	    obj)))
   
   (define (NodeScript this code ctx filename)
      (instantiateJsScript
	 (code code)
	 (ctx ctx)
	 (filename (js-tostring filename %this))
	 (__proto__ nodescript-proto)))

   (let ((obj (js-make-function %this NodeScript
		 (js-function-arity NodeScript)
		 (js-function-info :name "NodeScript" :len 3)
		 :alloc (lambda (%this o) #unspecified)
		 :prototype nodescript-proto)))
      (js-put! obj (& "createContext") createContext #f %this)
      (js-put! obj (& "runInContext") runInContextVM #f %this)
      (js-put! obj (& "runInNewContext") runInNewContextVM #f %this)
      (js-put! obj (& "runInThisContext") runInThisContextVM #f %this)
      (js-alist->jsobject
	 `((NodeScript . ,obj))
	 %this)))
   
;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

