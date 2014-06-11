;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/service.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 17 08:19:20 2013                          */
;*    Last change :  Wed Jun 11 11:54:38 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript service implementation                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_service

   (library hop)

   (import __hopscript_types
	   __hopscript_rts
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_property
	   __hopscript_function
	   __hopscript_worker
	   __hopscript_json)

   (static (class JsHopFrame::JsObject
	      (url read-only)))

   (export (js-init-service! ::JsGlobalObject)
	   (js-make-hopframe ::JsGlobalObject url)
	   (js-make-service::JsService ::JsGlobalObject ::procedure ::obj ::hop-service)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::JsHopFrame ...                                 */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript o::JsHopFrame op compile isexpr)
   (with-access::JsHopFrame o (url)
      (display "new HopFrame(\"" op)
      (display url op)
      (display "\")" op)))

;*---------------------------------------------------------------------*/
;*    js-init-service! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-service! %this::JsGlobalObject)
   ;; first, create the builtin prototype
   (with-access::JsGlobalObject %this (__proto__ js-function
					 js-service-prototype
					 js-hopframe-prototype)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))

	 ;; service prototype
	 (set! js-service-prototype
	    (instantiate::JsService
	       (__proto__ js-function-prototype)
	       (name "service")
	       (alloc (lambda (_) #unspecified))
	       (construct (lambda (constructor args)
			     (js-raise-type-error %this "not a constructor ~s"
				js-function-prototype)))
	       (arity -1)
	       (procedure list)
	       (svc #f)
	       (extensible #t)))

	 (js-bind! %this js-function-prototype 'resource
	    :value (js-make-function %this
		      (lambda (this file)
			 (service-resource this file))
		      1 'resource)
	    :writable #t
	    :configurable #t
	    :enumerable #f)
	 
	 ;; HopFrame prototype
	 (set! js-hopframe-prototype
	    (instantiate::JsObject
	       (__proto__ __proto__)
	       (extensible #t)))

	 (js-bind! %this js-hopframe-prototype 'post
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame success opt)
			 (with-access::JsHopFrame this (url)
			    (post url success opt %this)))
		      2 'post))
	 
	 (js-bind! %this js-hopframe-prototype 'toString
	    :value (js-make-function %this
		      (lambda (this::JsHopFrame)
			 (with-access::JsHopFrame this (url)
			    url))
		      0 'toString))
	 
	 ;; HopFrame constructor 
	 (letrec ((js-hopframe (js-make-function %this
				  (lambda (this url)
				     (js-new %this js-hopframe url))
				  1 'JsHopFrame
				  :__proto__ js-function-prototype
				  :prototype js-hopframe-prototype
				  :construct (lambda (this url)
						(js-make-hopframe %this url)))))
	    (js-bind! %this %this 'HopFrame
	       :configurable #f :enumerable #f :value js-hopframe))
	 
	 (js-undefined))))

;*---------------------------------------------------------------------*/
;*    js-make-hopframe ...                                             */
;*---------------------------------------------------------------------*/
(define (js-make-hopframe %this::JsGlobalObject url)
   (with-access::JsGlobalObject %this (js-hopframe-prototype)
      (let ((obj (instantiate::JsHopFrame
		    (url url)
		    (__proto__ js-hopframe-prototype))))
	 obj)))

;*---------------------------------------------------------------------*/
;*    post ...                                                         */
;*---------------------------------------------------------------------*/
(define (post svc::bstring success opt %this)
   
   (define (parse-json in)
      (js-json-parser in (js-undefined) %this))

   (let ((host "localhost")
	 (port (hop-port))
	 (user #f)
	 (password #f)
	 (authorization #f)
	 (fail #f))
      (unless (eq? opt (js-undefined))
	 (let ((h (js-get opt 'host %this))
	       (p (js-get opt 'port %this))
	       (u (js-get opt 'user %this))
	       (w (js-get opt 'password %this))
	       (a (js-get opt 'authorization %this))
	       (f (js-get opt 'fail %this)))
	    (unless (eq? h (js-undefined))
	       (set! host (js-tostring h %this)))
	    (unless (eq? p (js-undefined))
	       (set! port (js-tointeger p %this)))
	    (unless (eq? u (js-undefined))
	       (set! user u))
	    (unless (eq? w (js-undefined))
	       (set! password (js-tostring w %this)))
	    (unless (eq? a (js-undefined))
	       (set! authorization (js-tostring a %this)))
	    (when (isa? f JsFunction)
	       (set! fail (lambda (x) (js-call1 %this f %this x))))))
      (with-hop-remote svc
	 (if (isa? success JsFunction)
	     (lambda (x) (js-call1 %this success %this x))
	     (lambda (x) x))
	 fail
	 :host host :port port 
	 :user user :password password :authorization authorization)))

;*---------------------------------------------------------------------*/
;*    js-make-service ...                                              */
;*---------------------------------------------------------------------*/
(define (js-make-service %this proc name svc)
   (with-access::JsGlobalObject %this (js-service-prototype)
      (instantiate::JsService
	 (procedure proc)
	 (arity (procedure-arity proc))
	 (__proto__ js-service-prototype)
	 (name (or name proc))
	 (alloc (lambda (_)
		   (js-raise-type-error %this
		      "service not a constructor" #f)))
	 (construct (lambda (_ arg)
		       (js-raise-type-error %this
			  "service not a constructor" arg)))
	 (properties (list
			(instantiate::JsValueDescriptor
			   (name 'length)
			   (value 0))
			(instantiate::JsAccessorDescriptor
			   (name 'path)
			   (get (js-make-function %this
				   (lambda (o)
				      (with-access::JsService o (svc)
					 (with-access::hop-service svc (path)
					    path)))
				   1 'path))
			   (set (js-make-function %this
				   (lambda (o v)
				      (with-access::JsService o (svc)
					 (unregister-service! svc)
					 (with-access::hop-service svc (path)
					    (set! path (js-tostring v %this))
					    (register-service! svc))))
				   2 'path)))))
	 
	 (svc svc))))

;*---------------------------------------------------------------------*/
;*    service? ::JsService ...                                         */
;*---------------------------------------------------------------------*/
(define-method (service? obj::JsService)
   #t)

;*---------------------------------------------------------------------*/
;*    service-resource ::JsService ...                                 */
;*---------------------------------------------------------------------*/
(define-method (service-resource obj::JsService #!optional file)
   (with-access::JsService obj (svc)
      (with-access::hop-service svc (resource)
	 (if (string? file)
	     (string-append resource "/" file)
	     resource))))

;*---------------------------------------------------------------------*/
;*    service-path ::JsService ...                                     */
;*---------------------------------------------------------------------*/
(define-method (service-path obj::JsService)
   (with-access::JsService obj (svc)
      (with-access::hop-service svc (path)
	 path)))

;*---------------------------------------------------------------------*/
;*    service-proc ::JsService ...                                     */
;*---------------------------------------------------------------------*/
(define-method (service-proc obj::JsService)
   (with-access::JsService obj (svc)
      (with-access::hop-service svc (proc)
	 proc)))
