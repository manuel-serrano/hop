;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/zeroconf.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:00:54 2011                          */
;*    Last change :  Thu Jun 28 08:41:44 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop Zeroconf support                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_zeroconf
   
   (cond-expand
      ((and enable-avahi (library pthread) (library avahi))
       (include "zeroconf_avahi.sch"))
      (else
       (include "zeroconf_dummy.sch")))

   (include "service.sch")
   
   (import __hop_configure
	   __hop_service
	   __hop_types
	   __hop_hop
	   __hop_param
	   __hop_misc
	   __hop_read
	   __hop_event
	   __hop_user)
   
   (export (class zeroconf
	      (zeroconf-init!)
	      (name::bstring read-only)
	      (onready::procedure (default list)))

	   (abstract-class zeroconf-discoverer)
	   
	   (class zeroconf-service-discoverer::zeroconf-discoverer)

	   (class zeroconf-service-event::server-event
	      (interface::int read-only)
	      (protocol::bstring read-only)
	      (type::bstring read-only)
	      (domain::bstring read-only)
	      (hostname::bstring read-only)
	      (port::int read-only)
	      (address::bstring read-only)
	      (options::pair-nil read-only (default '())))

	   (generic zeroconf-init! ::zeroconf)

	   (zeroconf-backend)
	   (zeroconf-register-backend! ::zeroconf)
	   
	   (generic zeroconf-start ::zeroconf)
	   (generic zeroconf-stop ::zeroconf)
	   (zeroconf-publish! ::zeroconf #!key name port type #!rest opts)
	   (generic zeroconf-publish-service! ::zeroconf
	      ::bstring ::int ::bstring ::pair-nil)
	   (generic zeroconf-add-service-event-listener! ::zeroconf
	      ::zeroconf-discoverer ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    *zeroconf-backend* ...                                           */
;*---------------------------------------------------------------------*/
(define *zeroconf-backend* *zeroconf-backend*)

;*---------------------------------------------------------------------*/
;*    zeroconf-register-backend! ...                                   */
;*---------------------------------------------------------------------*/
(define (zeroconf-register-backend! o::zeroconf)
   (set! *zeroconf-backend* o))

;*---------------------------------------------------------------------*/
;*    zeroconf-backend ...                                             */
;*---------------------------------------------------------------------*/
(define (zeroconf-backend)
   *zeroconf-backend*)

;*---------------------------------------------------------------------*/
;*    zeroconf-init! ::zeroconf ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (zeroconf-init! o::zeroconf)
   o)

;*---------------------------------------------------------------------*/
;*    zeroconf-start ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (zeroconf-start o::zeroconf)
   #f)

;*---------------------------------------------------------------------*/
;*    zeroconf-stop ::zeroconf ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (zeroconf-stop o::zeroconf)
   #f)

;*---------------------------------------------------------------------*/
;*    zeroconf-publish-service! ::zeroconf ...                         */
;*---------------------------------------------------------------------*/
(define-generic (zeroconf-publish-service! o::zeroconf name port type opts)
   #f)

;*---------------------------------------------------------------------*/
;*    zeroconf-add-service-event-listener! ::zeroconf ...              */
;*---------------------------------------------------------------------*/
(define-generic (zeroconf-add-service-event-listener! o::zeroconf zd evt proc)
   #f)

;*---------------------------------------------------------------------*/
;*    add-event-listener! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! zd::zeroconf-service-discoverer evt proc . capture)
   (when (isa? *zeroconf-backend* zeroconf)
      (zeroconf-add-service-event-listener! *zeroconf-backend* zd evt proc)))

;*---------------------------------------------------------------------*/
;*    zeroconf-publish! ...                                            */
;*---------------------------------------------------------------------*/
(define (zeroconf-publish! zc::zeroconf #!key name port type #!rest opts)
   (zeroconf-publish-service! zc name port type opts))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::zeroconf ...                               */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! zc::zeroconf evt proc . captuer)
   (with-access::zeroconf zc (onready)
      (set! onready proc)))
