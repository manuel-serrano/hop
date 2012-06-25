;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/zeroconf.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:00:54 2011                          */
;*    Last change :  Mon Jun 25 09:29:31 2012 (serrano)                */
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
	      (name::bstring read-only))

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

	   (zeroconf-register-backend! ::zeroconf)
	   (zeroconf-start! ::procedure)
	   
	   (generic zeroconf-start-backend! ::zeroconf ::procedure)
	   (generic zeroconf-init! ::zeroconf)
	   (generic zeroconf-close! ::zeroconf)
	   (generic zeroconf-publish-service! ::zeroconf
	      ::bstring ::int ::bstring ::pair-nil)
	   (generic zeroconf-add-service-event-listener! ::zeroconf
	      ::zeroconf-discoverer ::obj ::procedure)

	   (zeroconf-publish! #!key name port type #!rest opts)))

;*---------------------------------------------------------------------*/
;*    *zeroconf-backend* ...                                           */
;*---------------------------------------------------------------------*/
(define *zeroconf-backend* *zeroconf-backend*)
(define *zeroconf-start-proc* *zeroconf-start-proc*)

;*---------------------------------------------------------------------*/
;*    zeroconf-init! ::zeroconf ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (zeroconf-init! o::zeroconf)
   o)

;*---------------------------------------------------------------------*/
;*    zeroconf-start! ...                                              */
;*---------------------------------------------------------------------*/
(define (zeroconf-start! thunk)
   (when (isa? *zeroconf-backend* zeroconf)
      (zeroconf-start-backend! *zeroconf-backend* thunk)))

;*---------------------------------------------------------------------*/
;*    zeroconf-start-backend! ::zeroconf ...                           */
;*---------------------------------------------------------------------*/
(define-generic (zeroconf-start-backend! o::zeroconf thunk))

;*---------------------------------------------------------------------*/
;*    zeroconf-register-backend! ...                                   */
;*---------------------------------------------------------------------*/
(define (zeroconf-register-backend! o::zeroconf)
   (set! *zeroconf-backend* o))

;*---------------------------------------------------------------------*/
;*    zeroconf-close! ::zeroconf ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (zeroconf-close! o::zeroconf)
   #f)

;*---------------------------------------------------------------------*/
;*    zeroconf-publish-service! ::zeroconf ...                         */
;*---------------------------------------------------------------------*/
(define-generic (zeroconf-publish-service! o::zeroconf name port type opts)
   #f)

;*---------------------------------------------------------------------*/
;*    zeroconf-publish! ...                                            */
;*---------------------------------------------------------------------*/
(define (zeroconf-publish! #!key name port type #!rest opts)
   (when (isa? *zeroconf-backend* zeroconf)
      (zeroconf-publish-service! *zeroconf-backend* name port type opts)))

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
