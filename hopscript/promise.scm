;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/promise.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 19 08:19:19 2015                          */
;*    Last change :  Thu Dec 17 05:37:34 2015 (serrano)                */
;*    Copyright   :  2015 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript promises                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/6.0/#25.4             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_promise
   
   (include "../nodejs/nodejs_debug.sch")
   
   (library hop)
   
   (include "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_error
	   __hopscript_worker)

   (export (js-init-promise! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    Jsstringliteral begin                                            */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-begin!)

;*---------------------------------------------------------------------*/
;*    object-serializer ::JsPromise ...                                */
;*---------------------------------------------------------------------*/
(register-class-serialization! JsPromise
   (lambda (o)
      o)
   (lambda (o %this)
      o))

;*---------------------------------------------------------------------*/
;*    js-donate ::JsPromise ...                                        */
;*---------------------------------------------------------------------*/
(define-method (js-donate obj::JsPromise worker::WorkerHopThread %this)
   (js-undefined))
   
;*---------------------------------------------------------------------*/
;*    scheme->response ::JsPromise ...                                 */
;*---------------------------------------------------------------------*/
(define-method (scheme->response obj::JsPromise req)
   
   (define (async-proc k)
      (with-access::JsPromise obj (worker)
	 (with-access::WorkerHopThread worker (%this)
	    (js-promise-then-catch %this obj 
	       (js-make-function %this
		  (lambda (this resp)
		     (k (scheme->response resp req)))
		  1 "reply")
	       (js-make-function %this
		  (lambda (this rej)
		     (let ((errobj (url-path-encode
				      (obj->string rej 'hop-client))))
			(k (instantiate::http-response-hop
			      (start-line "HTTP/1.1 500 Internal Server Error")
			      (backend (hop-xml-backend))
			      (content-type "application/x-hop")
			      (header `((Hop-Error: . ,errobj)))
			      (value rej)))))
		  1 "reject")))))
   
   (instantiate::http-response-async
      (async async-proc)))
		
;*---------------------------------------------------------------------*/
;*    js-init-promise! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-promise! %this::JsGlobalObject)
   
   (define (promise-resolvers promise::JsPromise)
      (with-access::JsPromise promise (watches)
	 (when (pair? watches)
	    (let ((resolved #t))
	       (js-for-in watches
		  (lambda (o)
		     (when (isa? o JsPromise)
			(with-access::JsPromise o (state)
			   (when (eq? state 'fullfilled)
			      (set! resolved #f)))))
		  %this)
	       (when resolved
		  (promise-resolve promise watches))))))

   (define (promise-rejecters promise::JsPromise)
      (with-access::JsPromise promise (watches)
	 (when (pair? watches)
	    (let ((resolved-or-reject #f)
		  (rval #t))
	       (js-for-in watches
		  (lambda (o)
		     (unless resolved-or-reject
			(when (isa? o JsPromise)
			   (with-access::JsPromise o (state val)
			      (unless (eq? state 'fullfilled)
				 (set! resolved-or-reject state)
				 (set! rval val))))))
		  %this)
	       (case resolved-or-reject
		  ((resolved)
		   (promise-resolve promise rval))
		  ((rejected)
		   (promise-reject promise rval)))
	       promise))))
      

   ;; promise-resolve
   (define (promise-resolve o::JsPromise v)
      (with-access::JsPromise o (thens state val resolvers worker)
	 (when (eq? state 'pending)
	    (set! state 'fullfilled)
	    (set! val v)
	    (js-worker-push-thunk! worker "promise"
	       (lambda ()
		  (with-handler
		     exception-notify
		     (for-each (lambda (then)
				  (when (isa? then JsFunction)
				     (js-call1 %this then o v)))
			(reverse thens)))))
	    (for-each (lambda (w) (promise-resolvers w)) resolvers))))
   
   ;; promise-reject
   (define (promise-reject o::JsPromise v)
      (with-access::JsPromise o (catches state val rejecters worker)
	 (when (eq? state 'pending)
	    (set! state 'rejected)
	    (set! val v)
	    (js-worker-push-thunk! worker "promise"
	       (lambda ()
		  (with-handler
		     exception-notify
		     (for-each (lambda (hdl)
				  (js-call1 %this hdl o v))
			(reverse catches)))))
	    (for-each (lambda (w) (promise-rejecters w)) rejecters))))
   
   ;; builtin prototype
   (define js-promise-prototype
      (with-access::JsGlobalObject %this (__proto__)
	 (instantiate::JsPromise
	    (worker (js-undefined))
	    (__proto__ __proto__)
	    (extensible #t))))
   
   ;; http://www.ecma-international.org/ecma-262/6.0/#sec-promise-constructor
   (define (%js-promise::JsPromise o::JsPromise executor)
      (if (not (isa? executor JsFunction))
	  (js-raise-type-error %this "argument not a procedure ~a"
	     (typeof executor))
	  (let ((resolve (js-make-function %this
			    (lambda (_ val)
			       (promise-resolve o val))
			    1 'resolve :src 'builtin))
		(reject (js-make-function %this
			   (lambda (_ err)
			      (promise-reject o err))
			   1 'reject :src 'builtin)))
	     (js-call2 %this executor o resolve reject)
	     o)))
   
   (define (js-promise-construct o::JsPromise executor)
      (%js-promise o executor))
   
   ;; promise allocation
   (define (js-promise-alloc::JsPromise constructor::JsFunction)
      (instantiate::JsPromise
	 (worker (js-current-worker))
	 (__proto__ (js-get constructor 'prototype %this))))
   
   ;; then, create a HopScript object
   (define js-promise
      (with-access::JsGlobalObject %this (js-function-prototype)
	 (js-make-function %this
	    %js-promise 1 'Promise
	    :__proto__ js-function-prototype
	    :prototype js-promise-prototype
	    :construct js-promise-construct
	    :alloc js-promise-alloc)))

   ;; http://www.ecma-international.org/ecma-262/6.0/#sec-promise.all
   (define (js-promise-all this iterable)
      (let ((promise (js-promise-alloc js-promise)))
	 (js-for-in iterable
	    (lambda (o)
	       (when (isa? o JsPromise)
		  (with-access::JsPromise o (resolvers state)
		     (when (eq? state 'fullfilled)
			(set! resolvers (cons this resolvers))))))
	    %this)
	 (with-access::JsPromise promise (watches)
	    (set! watches iterable))
	 (promise-resolvers promise)
	 promise))
   
   (js-bind! %this js-promise 'all
      :configurable #f :enumerable #t
      :value (js-make-function %this js-promise-all 1 'all))
   
   ;; http://www.ecma-international.org/ecma-262/6.0/#sec-promise.race
   (define (js-promise-race this iterable)
      (let ((promise (js-promise-alloc js-promise)))
	 (js-for-in iterable
	    (lambda (o)
	       (when (isa? o JsPromise)
		  (with-access::JsPromise o (rejecters state)
		     (unless (eq? state 'fullfilled)
			(set! rejecters (cons this rejecters))))))
	    %this)
	 (with-access::JsPromise promise (watches)
	    (set! watches iterable))
	 (promise-rejecters promise)
	 promise))

   (js-bind! %this js-promise 'race
      :configurable #f :enumerable #t
      :value (js-make-function %this js-promise-race 1 'race))
   
   ;; http://www.ecma-international.org/ecma-262/6.0/#sec-promise.reject
   (define (js-promise-reject this val)
      (promise-reject (js-promise-alloc js-promise) val))
   
   (js-bind! %this js-promise 'reject
      :configurable #f :enumerable #t
      :value (js-make-function %this js-promise-reject 1 'reject))

   ;; http://www.ecma-international.org/ecma-262/6.0/#sec-promise.resolve
   (define (js-promise-resolve this val)
      (promise-resolve (js-promise-alloc js-promise) val))
   
   (js-bind! %this js-promise 'resolve
      :configurable #f :enumerable #t
      :value (js-make-function %this js-promise-resolve 1 'resolve))
   
   ;; prototype properties
   (init-builtin-promise-prototype! %this js-promise-alloc js-promise-prototype)
   
   ;; bind Promise in the global object
   (js-bind! %this %this 'Promise
      :configurable #f :enumerable #f :value js-promise)

   ;; bind the promise object in the global environment
   (with-access::JsGlobalObject %this ((%js-promise js-promise))
      (set! %js-promise js-promise))
   
   js-promise)
   
;*---------------------------------------------------------------------*/
;*    init-builtin-promise-prototype! ...                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/6.0/#24.4.4.2         */
;*---------------------------------------------------------------------*/
(define (init-builtin-promise-prototype! %this::JsGlobalObject js-promise obj)

   ;; catch
   (js-bind! %this obj 'catch
      :value (js-make-function %this
		(lambda (this fail)
		   (if (not (isa? this JsPromise))
		       (js-raise-type-error %this "argument not a promise ~a"
			  (typeof this))
		       (js-promise-then-catch %this this #f fail))
		   this)
		1 'catch)
      :enumerable #f)
   
   ;; then
   (js-bind! %this obj 'then
      :value (js-make-function %this
		(lambda (this proc fail)
		   (if (not (isa? this JsPromise))
		       (js-raise-type-error %this "argument not a promise ~a"
			  (typeof this))
		       (js-promise-then-catch %this this proc fail))
		   this)
		2 'then)
      :enumerable #f))

;*---------------------------------------------------------------------*/
;*    js-promise-then-catch ...                                        */
;*---------------------------------------------------------------------*/
(define (js-promise-then-catch %this::JsGlobalObject this::JsPromise proc fail)
   (with-access::JsPromise this (state thens rejecters catches val worker)
      (case state
	 ((fullfilled)
	  (if (isa? proc JsFunction)
	      (js-worker-push-thunk! worker "promise"
		 (lambda ()
		    (with-handler
		       exception-notify
		       (js-call1 %this proc this val))))
	      (js-undefined)))
	 ((rejected)
	  (js-worker-push-thunk! worker "promise"
	     (lambda ()
		(with-handler
		   exception-notify
		   (if (isa? fail JsFunction)
		       (js-call1 %this fail this val)
		       (js-raise-error %this "Uncaught (in promise)" val))))))
	 (else
	  (when (isa? proc JsFunction)
	     (set! thens (cons proc thens)))
	  (when (isa? fail JsFunction)
	     (set! catches (cons fail catches)))))
      this))

;*---------------------------------------------------------------------*/
;*    Jsstringliteral end                                              */
;*---------------------------------------------------------------------*/
(%js-jsstringliteral-end!)
