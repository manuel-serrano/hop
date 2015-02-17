;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/nodejs_async.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb  5 05:53:01 2015                          */
;*    Last change :  Thu Feb  5 06:00:39 2015 (serrano)                */
;*    Copyright   :  2015 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Asynchronous user callback invocations.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-domain-protect ...                                            */
;*    -------------------------------------------------------------    */
;*    Callbacks must be protected (i.e., under the async object) when  */
;*    executed from within a domain as errors should not stop the main */
;*    execution.                                                       */
;*---------------------------------------------------------------------*/
(define-macro (js-domain-protect call)
   `(with-access::WorkerHopThread %worker (async)
       (if async
	   (js-worker-push-thunk! %worker "domain" (lambda () ,call))
	   (begin
	      ,call
	      (nodejs-need-tick-callback %worker %this process)))))

;*---------------------------------------------------------------------*/
;*    !js-call0 ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (!js-call0 name %this obj proc)
   (let ((env (gensym 'env))
	 (nm (gensym 'name))
	 (aux (gensym 'aux)))
      `(let ((,env (current-dynamic-env))
	     (,nm ,name))
	  ($env-push-trace ,env ,nm #f)
	  (let ((,aux (js-domain-protect (js-call0 ,%this ,obj ,proc))))
	     ($env-pop-trace ,env)
	     ,aux))))
		 
;*---------------------------------------------------------------------*/
;*    !js-call1 ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (!js-call1 name %this obj proc arg0)
   (let ((env (gensym 'env))
	 (nm (gensym 'name))
	 (aux (gensym 'aux)))
      `(let ((,env (current-dynamic-env))
	     (,nm ,name))
	  ($env-push-trace ,env ,nm #f)
	  (let ((,aux (js-domain-protect (js-call1 ,%this ,obj ,proc ,arg0))))
	     ($env-pop-trace ,env)
	     ,aux))))
		 
;*---------------------------------------------------------------------*/
;*    !js-call2 ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (!js-call2 name %this obj proc arg0 arg1)
   (let ((env (gensym 'env))
	 (nm (gensym 'name))
	 (aux (gensym 'aux)))
      `(let ((,env (current-dynamic-env))
	     (,nm ,name))
	  ($env-push-trace ,env ,nm #f)
	  (let ((,aux (js-domain-protect (js-call2 ,%this ,obj ,proc ,arg0 ,arg1))))
	     ($env-pop-trace ,env)
	     ,aux))))
		 
;*---------------------------------------------------------------------*/
;*    !js-call3 ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (!js-call3 name %this obj proc arg0 arg1 arg2)
   (let ((env (gensym 'env))
	 (nm (gensym 'name))
	 (aux (gensym 'aux)))
      `(let ((,env (current-dynamic-env))
	     (,nm ,name))
	  ($env-push-trace ,env ,nm #f)
	  (let ((,aux (js-domain-protect (js-call3 ,%this ,obj ,proc ,arg0 ,arg1 ,arg2))))
	     ($env-pop-trace ,env)
	     ,aux))))
		 
;*---------------------------------------------------------------------*/
;*    !js-call4 ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (!js-call4 name %this obj proc arg0 arg1 arg2 arg3)
   (let ((env (gensym 'env))
	 (nm (gensym 'name))
	 (aux (gensym 'aux)))
      `(let ((,env (current-dynamic-env))
	     (,nm ,name))
	  ($env-push-trace ,env ,nm #f)
	  (let ((,aux (js-domain-protect (js-call4 ,%this ,obj ,proc ,arg0 ,arg1 ,arg2 ,arg3))))
	     ($env-pop-trace ,env)
	     ,aux))))
		 
;*---------------------------------------------------------------------*/
;*    !js-call5 ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (!js-call5 name %this obj proc arg0 arg1 arg2 arg3 arg4)
   (let ((env (gensym 'env))
	 (nm (gensym 'name))
	 (aux (gensym 'aux)))
      `(let ((,env (current-dynamic-env))
	     (,nm ,name))
	  ($env-push-trace ,env ,nm #f)
	  (let ((,aux (js-domain-protect (js-call5 ,%this ,obj ,proc ,arg0 ,arg1 ,arg2 ,arg3 ,arg4))))
	     ($env-pop-trace ,env)
	     ,aux))))
		 
