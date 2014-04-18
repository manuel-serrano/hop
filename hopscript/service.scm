;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/service.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 17 08:19:20 2013                          */
;*    Last change :  Fri Apr 18 07:15:14 2014 (serrano)                */
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
	   __hopscript_worker)

   (export (js-init-service! ::JsGlobalObject)
	   (js-make-service::JsService ::JsGlobalObject ::procedure ::obj ::hop-service)))

;*---------------------------------------------------------------------*/
;*    js-init-service! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-service! %this::JsGlobalObject)
   ;; first, create the builtin prototype
   (with-access::JsGlobalObject %this (js-function js-service-prototype)
      (with-access::JsFunction js-function ((js-function-prototype __proto__))

	 ;; create a HopScript regexp object constructor
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
	 ;; prototype properties
	 (js-bind! %this js-function-prototype 'resource
	    :value (js-make-function %this
		      (lambda (this file)
			 (service-resource this file))
		      1 "resource")
	    :writable #t
	    :configurable #t
	    :enumerable #f)
	 (js-undefined))))

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
				   1 "path"))
			   (set (js-make-function %this
				   (lambda (o v)
				      (with-access::JsService o (svc)
					 (unregister-service! svc)
					 (with-access::hop-service svc (path)
					    (set! path (js-tostring v %this))
					    (register-service! svc))))
				   2 "path")))))
	 
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
