;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/upnp.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 10 16:49:31 2013                          */
;*    Last change :  Mon Apr 15 08:44:26 2013 (serrano)                */
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
	      (listeners::obj (default (make-hashtable)))))

   (export (class upnp-event::event))
   
   (export (upnp-backend)
	   (upnp-discover ::obj ::bstring)))

;*---------------------------------------------------------------------*/
;*    upnp-mutex ...                                                   */
;*---------------------------------------------------------------------*/
(define upnp-mutex (make-mutex))

;*---------------------------------------------------------------------*/
;*    *upnp-backend* ...                                               */
;*---------------------------------------------------------------------*/
(define *upnp-backend* #f)

;*---------------------------------------------------------------------*/
;*    update-cache! ...                                                */
;*---------------------------------------------------------------------*/
(define (update-cache! cache)
   (let ((sec (current-seconds)))
      (filter (lambda (e)
		 (cond
		    ((isa? e ssdp-notify)
		     (with-access::ssdp-notify e (edate)
			(<elong edate sec)))
		    ((isa? e ssdp-response)
		     (with-access::ssdp-response e (edate)
			(<elong edate sec)))))
	 cache)))

;*---------------------------------------------------------------------*/
;*    apply-listeners ...                                              */
;*---------------------------------------------------------------------*/
(define (apply-listeners listeners::obj e::event u::upnp)
   (let loop ((l listeners))
      (when (pair? l)
	 ((car l) e)
	 (with-access::event e (stopped)
	    (unless stopped
	       (loop (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    upnp-onmessage ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (upnp-onmessage e::ssdp-message upnp))

;*---------------------------------------------------------------------*/
;*    upnp-onmessage ::ssdp-notify ...                                 */
;*---------------------------------------------------------------------*/
(define-method (upnp-onmessage e::ssdp-notify upnp)
   (tprint "UPNP-NOTIFY: " e)
   (with-access::ssdp-notify e (usn nt)
      (with-access::upnp upnp (mutex listeners cache)
	 (synchronize mutex
	    (set! cache (update-cache! cache))
	    (unless (find (lambda (e::ssdp-notify)
			     (with-access::ssdp-notify e ((usne usn))
				(string=? usn usne)))
		       cache)
	       (set! cache (cons e cache))
	       (let ((listeners (hashtable-get listeners nt)))
		  (when (pair? listeners)
		     (let ((he (instantiate::upnp-event
				  (name "ssdp-notify")
				  (target upnp)
				  (value event))))
			(apply-listeners listeners he upnp)))))))))

;*---------------------------------------------------------------------*/
;*    upnp-onmessage ::ssdp-response ...                               */
;*---------------------------------------------------------------------*/
(define-method (upnp-onmessage e::ssdp-response upnp)
   (tprint "UPNP-RESPONSE: " e)
   (with-access::ssdp-response e (usn st)
      (with-access::upnp upnp (mutex listeners cache)
	 (synchronize mutex
	    (set! cache (update-cache! cache))
	    (unless (find (lambda (e::ssdp-response)
			     (with-access::ssdp-response e ((usne usn))
				(string=? usn usne)))
		       cache)
	       (set! cache (cons e cache))
	       (let ((listeners (hashtable-get listeners st)))
		  (when (pair? listeners)
		     (let ((he (instantiate::upnp-event
				  (name "ssdp-response")
				  (target upnp)
				  (value event))))
			(apply-listeners listeners he upnp)))))))))

;*---------------------------------------------------------------------*/
;*    upnp-onmessage ::ssdp-m-search ...                               */
;*---------------------------------------------------------------------*/
(define-method (upnp-onmessage e::ssdp-m-search upnp)
   (tprint "M-SEARCH...todo"))

;*---------------------------------------------------------------------*/
;*    upnp-backend ...                                                 */
;*---------------------------------------------------------------------*/
(define (upnp-backend)
   (synchronize upnp-mutex
      (unless *upnp-backend*
	 (let ((socket (make-datagram-server-socket 1900)))
	    (set! *upnp-backend*
	       (co-instantiate ((upnp (instantiate::upnp
					 (thread th)
					 (socket socket)))
				(th (instantiate::hopthread
				       (name "upnp")
				       (body (lambda ()
						(ssdp-discover-loop
						   :onmessage
						   (lambda (e)
						      (upnp-onmessage e upnp))
						   :socket socket))))))
		  (thread-start! th)
		  upnp)))))
   *upnp-backend*)

;*---------------------------------------------------------------------*/
;*    upnp-discover ...                                                */
;*---------------------------------------------------------------------*/
(define (upnp-discover o target)
   (with-access::upnp o (socket)
      (ssdp-discover-m-search :socket socket :target target)))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::upnp ...                                   */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! u::upnp evt proc . capture)
   (with-access::upnp u (mutex cache listeners)
      ;; add the listener for future events
      (synchronize mutex
	 (hashtable-add! listeners proc cons proc '()))
      ;; notify on past events
      (synchronize mutex
	 (set! cache (update-cache! cache))
	 (for-each (lambda (e) (apply-listeners (list proc) e u))
	    (filter (lambda (e::ssdp-message)
		       (cond
			  ((isa? e ssdp-notify)
			   (with-access::ssdp-notify e (nt)
			      (string=? nt evt)))
			  ((isa? e ssdp-response)
			   (with-access::ssdp-response e (st)
			      (string=? st evt)))))
	       cache)))
      ;; ping
      (upnp-discover u "ssdp:all")
      proc))


