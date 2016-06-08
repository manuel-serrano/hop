;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/promise.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 19 08:19:19 2015                          */
;*    Last change :  Tue May 31 08:34:16 2016 (serrano)                */
;*    Copyright   :  2015-16 Manuel Serrano                            */
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
		  1 "reject")
	       obj))))
   
   (instantiate::http-response-async
      (async async-proc)))
		
;*---------------------------------------------------------------------*/
;*    js-init-promise! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-promise! %this::JsGlobalObject)
   
   (define (iterable-vector this field iterable)
      (vector-map
	 (lambda (o)
	    (let* ((p (cond
			 ((isa? o JsPromise)
			  o)
			 ((eq? (class-field-name field) 'resolvers)
			  (js-promise-resolve this o))
			 (else
			  (js-promise-reject this o))))
		   (old ((class-field-accessor field) p)))
	       ((class-field-mutator field) p (cons this old))
	       p))
	 iterable))
   
   (define (iterable->vector this iterable field)
      (with-access::JsGlobalObject %this (js-symbol-iterator)
	 (let loop ((iterable iterable))
	    (cond
	       ((isa? iterable JsArray)
		(iterable-vector this field
		   (jsarray->vector iterable %this)))
	       ((js-get iterable js-symbol-iterator %this)
		=>
		(lambda (iterator)
		   (if (eq? iterator (js-undefined))
		       '#()
		       (let* ((it (js-call0 %this iterator iterable))
			      (next (js-get it 'next %this)))
			  (if (isa? next JsFunction)
			      (let loop ((acc '()))
				 (let* ((v (js-call0 %this next it))
					(done (js-get v 'done %this))
					(val (js-get v 'value %this)))
				    (if done
					(iterable-vector this field
					   (list->vector (reverse! acc)))
					(loop (cons val acc))))))))))
	       (else
		'#())))))
   
   ;; builtin prototype
   (define js-promise-prototype
      (with-access::JsGlobalObject %this (__proto__)
	 (instantiate::JsPromise
	    (worker (js-undefined))
	    (%this %this)
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
	 (%this %this)
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
	 (with-access::JsPromise promise (watches)
	    (set! watches
	       (iterable->vector promise iterable
		  (find-class-field JsPromise 'resolvers))))
	 (promise-watch-all promise)
	 promise))
   
   (js-bind! %this js-promise 'all
      :configurable #f :enumerable #t
      :value (js-make-function %this js-promise-all 1 'all))
   
   ;; http://www.ecma-international.org/ecma-262/6.0/#sec-promise.race
   (define (js-promise-race this iterable)
      (let ((promise (js-promise-alloc js-promise)))
	 (with-access::JsPromise promise (watches)
	    (set! watches
	       (iterable->vector promise iterable
		  (find-class-field JsPromise 'rejecters))))
	 (promise-watch-race promise)
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
      (cond
	 ((isa? val JsPromise)
	  val)
	 ((isa? val JsObject)
	  (let ((then (js-get val 'then %this)))
	     (if (isa? then JsFunction)
		 (let ((promise (js-promise-alloc js-promise))
		       (reject (js-get js-promise 'reject %this)))
		    (with-handler
		       (lambda (e)
			  (js-call1 %this reject promise e))
		       (js-call2 %this then promise
			  (js-get js-promise 'resolve %this)
			  reject))
		    promise)
		 (promise-resolve (js-promise-alloc js-promise) val))))
	 (else
	  (promise-resolve (js-promise-alloc js-promise) val))))
   
   (js-bind! %this js-promise 'resolve
      :configurable #f :enumerable #t
      :value (js-make-function %this js-promise-resolve 1 'resolve))
   
   ;; prototype properties
   (init-builtin-promise-prototype! %this js-promise-alloc js-promise-prototype)

   (with-access::JsGlobalObject %this (js-symbol-species)
      (js-bind! %this js-promise js-symbol-species
	 :configurable #f :enumerable #f :writable #f
	 :get (js-make-function %this (lambda (o) js-promise) 0 '@@species)))
   
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
		       (let ((promise (duplicate::JsPromise this
					 (state 'pending))))
			  (js-promise-then-catch %this this #f fail promise)
			  promise)))
		1 'catch)
      :enumerable #f)
   
   ;; then
   (js-bind! %this obj 'then
      :value (js-make-function %this
		(lambda (this proc fail)
		   (if (not (isa? this JsPromise))
		       (js-raise-type-error %this "argument not a promise ~a"
			  (typeof this))
		       (let ((promise (duplicate::JsPromise this
					 (state 'pending))))
			  (js-promise-then-catch %this this proc fail promise)
			  promise)))
		2 'then)
      :enumerable #f))

;*---------------------------------------------------------------------*/
;*    promise-resolvers ...                                            */
;*---------------------------------------------------------------------*/
(define (promise-resolvers promise::JsPromise)
   (with-access::JsPromise promise (state)
      (when (eq? state 'pending)
	 (promise-watch-all promise))))

;*---------------------------------------------------------------------*/
;*    promise-rejecters ...                                            */
;*---------------------------------------------------------------------*/
(define (promise-rejecters promise::JsPromise)
   (with-access::JsPromise promise (state)
      (when (eq? state 'pending)
	 (promise-watch-race promise))))

;*---------------------------------------------------------------------*/
;*    promise-watch-all ...                                            */
;*---------------------------------------------------------------------*/
(define (promise-watch-all promise::JsPromise)
   (with-access::JsPromise promise (watches %this)
      (let loop ((i (-fx (vector-length watches) 1))
		 (fullfilledp #t))
	 (if (=fx i -1)
	     (when fullfilledp
		(promise-resolve promise
		   (js-vector->jsarray
		      (vector-map! (lambda (o::JsPromise)
				      (with-access::JsPromise o (val)
					 val))
			 watches)
		      %this)))
	     (let ((o (vector-ref watches i)))
		(with-access::JsPromise o ((ostate state) (oval val))
		   (case ostate
		      ((rejected)
		       (promise-reject promise oval))
		      ((pending)
		       (loop (-fx i 1) #f))
		      (else
		       (loop (-fx i 1) fullfilledp)))))))))

;*---------------------------------------------------------------------*/
;*    promise-watch-race ...                                           */
;*---------------------------------------------------------------------*/
(define (promise-watch-race promise::JsPromise)
   (with-access::JsPromise promise (watches)
      (let loop ((i (-fx (vector-length watches) 1)))
	 (when (>fx i -1)
	    (let ((o (vector-ref watches i)))
	       (with-access::JsPromise o ((ostate state) (oval val))
		  (case ostate
		     ((rejected)
		      (promise-reject promise oval))
		     ((fullfilled)
		      (promise-resolve promise oval))
		     (else
		      (loop (-fx i 1))))))))))

;*---------------------------------------------------------------------*/
;*    promise-resolve ...                                              */
;*---------------------------------------------------------------------*/
(define (promise-resolve o::JsPromise v)
   (with-access::JsPromise o (thens state val resolvers rejecters worker %this)
      (when (eq? state 'pending)
	 (set! state 'fullfilled)
	 (set! val v)
	 (js-worker-push-thunk! worker "promise"
	    (lambda ()
	       (with-handler
		  exception-notify
		  (for-each (lambda (then)
			       (when (isa? then JsFunction)
				  (js-call1 %this then o val)))
		     (reverse thens)))))
	 (for-each (lambda (w) (promise-rejecters w)) rejecters)
	 (for-each (lambda (w) (promise-resolvers w)) resolvers)
	 o)))

;*---------------------------------------------------------------------*/
;*    promise-reject ...                                               */
;*---------------------------------------------------------------------*/
(define (promise-reject o::JsPromise v)
   (with-access::JsPromise o (catches state val resolvers rejecters worker %this)
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
	 (for-each (lambda (w) (promise-rejecters w)) rejecters)
	 (for-each (lambda (w) (promise-resolvers w)) resolvers)
	 o)))

;*---------------------------------------------------------------------*/
;*    js-promise-then-catch ...                                        */
;*---------------------------------------------------------------------*/
(define (js-promise-then-catch %this::JsGlobalObject this::JsPromise proc fail np)
   (with-access::JsPromise this (state thens rejecters catches val worker)
      (case state
	 ((fullfilled)
	  (if (isa? proc JsFunction)
	      (js-worker-push-thunk! worker "promise"
		 (lambda ()
		    (with-handler
		       exception-notify
		       (promise-resolve np (js-call1 %this proc this val)))))
	      (js-undefined)))
	 ((rejected)
	  (js-worker-push-thunk! worker "promise"
	     (lambda ()
		(with-handler
		   exception-notify
		   (if (isa? fail JsFunction)
		       (promise-reject np (js-call1 %this fail this val))
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
