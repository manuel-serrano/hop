;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_evals.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct 22 13:35:17 2014                          */
;*    Last change :  Tue Nov 25 14:23:05 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    VM bindings                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__evals

   (library hopscript)

   (export (process-evals ::WorkerHopThread ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    process-evals ...                                                */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/vm.html                                    */
;*---------------------------------------------------------------------*/
(define (process-evals %worker %this)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "evals"
	       (format "binding not implemented ~a" name)
	       l))
	 0 name))

   (define createContext
      (js-make-function %this
	 (lambda (this obj)
	    (let ((%this (js-new-global-object)))
	       (when (isa? obj JsObject)
		  (js-for-in obj
		     (lambda (p)
			(js-put! %this p (js-get obj p %this) #f %this))
		     %this))
	       %this))
	 1 "createContext"))

   (define runInContext
      (js-make-function %this
	 (lambda (this obj ctx)
	    (call-with-input-string (js-string->string obj)
	       (lambda (ip)
		  (%js-eval ip 'eval %this this
		     (if (eq? ctx (js-undefined)) %this ctx)))))
	 2 "runInContext"))
   
   (define NodeScript
      (js-alist->jsobject
	 `((createContext . ,createContext)
	   (runInContext . ,runInContext)
	   (runInThisContext . ,(not-implemented "runInThisContext"))
	   (runInNewContext . ,(not-implemented "runInNewContext")))
	 %this))
   
   (js-alist->jsobject
      `((NodeScript . ,NodeScript))
      %this))




   
