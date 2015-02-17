;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_evals.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct 22 13:35:17 2014                          */
;*    Last change :  Sat Feb 14 09:03:22 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    VM bindings                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__evals

   (library hopscript)

   (import __nodejs_require)
   
   (static (class JsScript::JsObject
	      code::JsStringLiteral
	      ctx::obj
	      filename::obj))
	   
   (export (process-evals ::WorkerHopThread ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    process-evals ...                                                */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/vm.html                                    */
;*---------------------------------------------------------------------*/
(define (process-evals %worker %this)
   
   (define (%createContext this obj)
      (let ((this (js-new-global-object)))
	 (when (isa? obj JsObject)
	    (js-for-in obj
	       (lambda (p)
		  (let* ((s (js-jsstring->string p))
			 (y (string->symbol s)))
		     (js-bind! %this this y
			:get (js-make-function %this
				(lambda (this)
				   (js-get obj y %this))
				0 s)
			:set (js-make-function %this
				(lambda (this v)
				   (js-put! obj y v #f %this))
				1 s))))
	       %this))
	 this))

   (define (call-with-context %this this ctx proc)
      (let* ((%ctx (%createContext this ctx))
	     (prev '()))
	 ;; get all pre-bound variables
	 (js-for-in %ctx (lambda (p) (set! prev (cons p prev))) %this)
	 (let ((res (proc %ctx)))
	    (when (isa? ctx JsObject)
	       (js-for-in %ctx
		  (lambda (p)
		     (unless (member p prev)
			(js-put! ctx p (js-get %ctx p %this) #f %this)))
		  %this))
	    res)))
   
   (define createContext
      (js-make-function %this %createContext 1 "createContext"))
   
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
	 1 "runInContext"))

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
	 3 "runInContext"))

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
	 1 "runInNewContext"))

   (define runInNewContextVM
      (js-make-function %this
	 (lambda (this code ctx filename)
	    (call-with-input-string (js-jsstring->string code)
	       (lambda (ip)
		  (input-port-name-set! ip (js-tostring filename %this))
		  (call-with-context %this this ctx
		     (lambda (%ctx)
			(%js-eval ip 'eval %ctx this %ctx))))))
	 3 "runInNewContext"))

   (define runInThisContext
      (js-make-function %this
	 (lambda (this)
	    (with-access::JsScript this (code filename)
	       (call-with-input-string (js-jsstring->string code)
		  (lambda (ip)
		     (input-port-name-set! ip filename)
		     (%js-eval ip 'eval %this this %this)))))
	 1 "runInThisContext"))

   (define runInThisContextVM
      (js-make-function %this
	 (lambda (this code filename)
	    (call-with-input-string (js-jsstring->string code)
	       (lambda (ip)
		  (input-port-name-set! ip (js-tostring filename %this))
		  (%js-eval ip 'eval %this this %this))))
	 3 "runInThisContext"))

   (define nodescript-proto
      (with-access::JsGlobalObject %this (js-object)
	 (let ((obj (js-new0 %this js-object)))
	    (js-put! obj 'createContext createContext #f %this)
	    (js-put! obj 'runInContext runInContext #f %this)
	    (js-put! obj 'runInThisContext runInThisContext #f %this)
	    (js-put! obj 'runInNewContext runInNewContext #f %this)
	    obj)))
   
   (define (NodeScript this code ctx filename)
      (instantiate::JsScript
	 (code code)
	 (ctx ctx)
	 (filename (js-tostring filename %this))
	 (__proto__ nodescript-proto)))
   
   (let ((obj (js-make-function %this NodeScript 3 "NodeScript"
		 :alloc (lambda (o) #unspecified)
		 :prototype nodescript-proto
		 :construct NodeScript)))
      (js-put! obj 'createContext createContext #f %this)
      (js-put! obj 'runInContext runInContextVM #f %this)
      (js-put! obj 'runInNewContext runInNewContextVM #f %this)
      (js-put! obj 'runInThisContext runInThisContextVM #f %this)
      (js-alist->jsobject
	 `((NodeScript . ,obj))
	 %this)))




   
