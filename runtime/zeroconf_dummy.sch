;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/runtime/zeroconf_dummy.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:13:46 2011                          */
;*    Last change :  Wed Jul  6 12:23:38 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dummy (fake) Zeroconf implementation                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(directives

   (export (class zeroconf-dummy::zeroconf
	      (publishers::pair-nil (default '()))
	      (subscribers::pair-nil (default '())))))

;*---------------------------------------------------------------------*/
;*    *zeroconf-backend* ...                                           */
;*---------------------------------------------------------------------*/
(define *zeroconf-backend* (instantiate::zeroconf-dummy))

;*---------------------------------------------------------------------*/
;*    Register the dummy-backend                                       */
;*---------------------------------------------------------------------*/
(zeroconf-register-backend!
   (instantiate::zeroconf-dummy
      (onready list)))
      
;*---------------------------------------------------------------------*/
;*    zeroconf-backend-publish-service! ::zeroconf-dummy ...           */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-backend-publish-service! o::zeroconf-dummy name port type opts)
   (with-access::zeroconf-dummy o (publishers)
      (set! publishers (cons (list name port type opts) publishers))))

;*---------------------------------------------------------------------*/
;*    zeroconf-backend-add-service-event-listener! ::zeroconf-dummy ...*/
;*---------------------------------------------------------------------*/
(define-method (zeroconf-backend-add-service-event-listener! o::zeroconf-dummy event proc)
   (with-access::zeroconf-dummy o (subscribers)
      (set! subscribers (cons (list event proc) subscribers))))

;*---------------------------------------------------------------------*/
;*    zeroconf-backend-stop ::zeroconf-dummy ...                       */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-backend-stop o::zeroconf-dummy)
   (with-access::zeroconf-dummy o (publishers subscribers)
      (set! publishers '())
      (set! subscribers '())))



