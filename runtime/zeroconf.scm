;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/zeroconf.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:00:54 2011                          */
;*    Last change :  Wed Dec 21 17:50:42 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
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
	      (hop-zeroconf-init!)
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

	   (hop-zeroconf-start!)
	   
	   (generic hop-zeroconf-init! ::zeroconf)
	   (generic hop-zeroconf-close! ::zeroconf)
	   (generic hop-zeroconf-publish-service! ::zeroconf
	      ::bstring ::int ::bstring ::pair-nil)
	   (generic hop-zeroconf-add-service-event-listener! ::zeroconf
	      ::zeroconf-discoverer ::obj ::procedure)

	   (hop-zeroconf-publish! #!key name port type #!rest opts)))

;*---------------------------------------------------------------------*/
;*    *hop-zeroconf-backend* ...                                       */
;*---------------------------------------------------------------------*/
(define *hop-zeroconf-backend* *hop-zeroconf-backend*)
(define *hop-zeroconf-start-thunk* *hop-zeroconf-start-thunk*)

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-start! ...                                          */
;*---------------------------------------------------------------------*/
(define (hop-zeroconf-start!)
   (set! *hop-zeroconf-backend* (*hop-zeroconf-start-thunk*)))
   
;*---------------------------------------------------------------------*/
;*    hop-zeroconf-init! ::zeroconf ...                                */
;*---------------------------------------------------------------------*/
(define-generic (hop-zeroconf-init! o::zeroconf)
   o)

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-register-backend! ...                               */
;*---------------------------------------------------------------------*/
(define (hop-zeroconf-register-backend! thunk::procedure)
   (set! *hop-zeroconf-start-thunk* thunk))

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-close! ::zeroconf ...                               */
;*---------------------------------------------------------------------*/
(define-generic (hop-zeroconf-close! o::zeroconf)
   #f)

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-publish-service! ::zeroconf ...                     */
;*---------------------------------------------------------------------*/
(define-generic (hop-zeroconf-publish-service! o::zeroconf name port type opts)
   #f)

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-publish! ...                                        */
;*---------------------------------------------------------------------*/
(define (hop-zeroconf-publish! #!key name port type #!rest opts)
   (when (isa? *hop-zeroconf-backend* zeroconf)
      (hop-zeroconf-publish-service! *hop-zeroconf-backend* name port type opts)))

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-add-service-event-listener! ::zeroconf ...          */
;*---------------------------------------------------------------------*/
(define-generic (hop-zeroconf-add-service-event-listener! o::zeroconf zd evt proc)
   #f)

;*---------------------------------------------------------------------*/
;*    add-event-listener! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! zd::zeroconf-service-discoverer evt proc . capture)
   (when (isa? *hop-zeroconf-backend* zeroconf)
      (hop-zeroconf-add-service-event-listener! *hop-zeroconf-backend* zd evt proc)))
