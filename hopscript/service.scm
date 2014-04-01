;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/service.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 17 08:19:20 2013                          */
;*    Last change :  Sat Mar 22 16:19:33 2014 (serrano)                */
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
	   __hopscript_function)

   (export (js-make-service::JsService ::procedure ::obj ::hop-service)))

;*---------------------------------------------------------------------*/
;*    js-make-service ...                                              */
;*---------------------------------------------------------------------*/
(define (js-make-service proc name svc)
   (instantiate::JsService
      (procedure proc)
      (arity (procedure-arity proc))
      (__proto__ js-function)
      (name (or name proc))
      (alloc (lambda (_)
		(js-raise-type-error "service not a constructor" #f)))
      (construct (lambda (_ arg)
		    (js-raise-type-error "service not a constructor" arg)))
      (properties (list
		     (instantiate::JsValueDescriptor
			(name 'length)
			(value 0))
		     (instantiate::JsAccessorDescriptor
			(name 'path)
			(get (js-make-function
				(lambda (o)
				   (with-access::JsService o (svc)
				      (with-access::hop-service svc (path)
					 path)))
				1 "path"))
			(set (js-make-function
				(lambda (o v)
				   (with-access::JsService o (svc)
				      (unregister-service! svc)
				      (with-access::hop-service svc (path)
					 (set! path (js-tostring v))
					 (register-service! svc))))
				2 "path")))))
      
      (svc svc)))

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
