;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/upnp.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 10 16:49:31 2013                          */
;*    Last change :  Fri Apr 12 19:17:36 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    UPNP Hop support                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_upnp

   (include "thread.sch")
   
   (library upnp)

   (import __hop_configure
	   __hop_service
	   __hop_types
	   __hop_hop
	   __hop_param
	   __hop_misc
	   __hop_read
	   __hop_event
	   __hop_user
	   __hop_thread)

   (static (class upnp
	      (thread::hopthread read-only)
	      (mutex::mutex read-only (default (make-mutex "upnp")))
	      (cache::pair-nil (default '()))
	      (socket::datagram-socket (default (make-datagram-unbound-socket 'inet)))
	      (listeners::pair-nil (default '()))))

   (export (class upnp-event::server-event
	      (val read-only)))
   
   (export (upnp-backend)))

;*---------------------------------------------------------------------*/
;*    upnp-mutex ...                                                   */
;*---------------------------------------------------------------------*/
(define upnp-mutex (make-mutex))

;*---------------------------------------------------------------------*/
;*    *upnp-backend* ...                                               */
;*---------------------------------------------------------------------*/
(define *upnp-backend* #f)

;*---------------------------------------------------------------------*/
;*    filter-cache! ...                                                */
;*---------------------------------------------------------------------*/
(define (filter-cache! o::upnp)
   (let ((sec (current-seconds)))
      (with-access::upnp o (cache)
	 (set! cache
	    (filter (lambda (e)
		       (with-access::upnp-event e (val)
			     (with-access::ssdp-search-response val (expiration-date)
				(<elong expiration-date (current-seconds)))))
	       cache))
	 cache)))
      
;*---------------------------------------------------------------------*/
;*    hop-upnp-discover ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-upnp-discover e u::upnp)
   (with-access::upnp u (mutex listeners cache)
      (let ((e (instantiate::upnp-event
		  (name "upnp")
		  (target u)
		  (val e))))
	 (synchronize mutex
	    (set! cache (cons e (filter-cache! u)))
	    (for-each (lambda (l) ((cdr l) e)) listeners)))))

;*---------------------------------------------------------------------*/
;*    upnp-backend ...                                                 */
;*---------------------------------------------------------------------*/
(define (upnp-backend)
   (synchronize upnp-mutex
      (unless *upnp-backend*
	 (set! *upnp-backend*
	    (co-instantiate ((upnp (instantiate::upnp
				      (thread th)))
			     (th (instantiate::hopthread
				    (name "upnp")
				    (body (lambda ()
					     (ssdp-discover :ondiscover
						(lambda (e)
						   (hop-upnp-discover e upnp))))))))
	       (thread-start! th)
	       upnp))))
   *upnp-backend*)

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::upnp ...                                   */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! o::upnp evt proc . capture)
   (with-access::upnp o (mutex cache listeners)
      ;; add the listener for future events
      (synchronize mutex
	 (set! listeners (cons (cons evt proc) listeners)))
      ;; notify on past events
      (synchronize mutex
	 (for-each proc (filter-cache! o)))
      o))

