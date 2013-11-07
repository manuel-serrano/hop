;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/runtime/upnp.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 10 16:49:31 2013                          */
;*    Last change :  Thu Nov  7 10:00:49 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    UPNP Hop support                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_upnp

   (include "thread.sch")

   (cond-expand
      ((and enable-upnp (library upnp))
       (library upnp)))

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
	      (mutex::mutex read-only (default (make-mutex "upnp")))
	      (cache::pair-nil (default '()))
	      (socket::datagram-socket (default (make-datagram-unbound-socket 'inet)))
	      (listeners::obj (default (make-hashtable)))
	      (ssdp::obj (default #f))))

   (export (class upnp-event::event))
   
   (export (upnp-backend)
	   (upnp-discover ::obj ::bstring)))

(cond-expand
   ((and enable-upnp (library upnp))
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
		 (with-access::ssdp-discovery e (edate)
		    (>elong edate sec)))
	 cache)))

;*---------------------------------------------------------------------*/
;*    upnp-onmessage ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (upnp-onmessage e::ssdp-message upnp))

;*---------------------------------------------------------------------*/
;*    upnp-onmessage ::ssdp-notify ...                                 */
;*---------------------------------------------------------------------*/
(define-method (upnp-onmessage e::ssdp-notify upnp)
;*    (tprint "UPNP-NOTIFY: " e)                                       */
   (with-access::ssdp-notify e (usn nt)
      (with-access::upnp upnp (mutex listeners cache)
	 (synchronize mutex
	    (set! cache (update-cache! cache))
;* 	    (tprint "USN=" usn)                                        */
;* 	    (tprint "CACHE=" cache)                                    */
	    (unless (find (lambda (e::ssdp-discovery)
			     (with-access::ssdp-discovery e ((usne usn))
				(string=? usn usne)))
		       cache)
	       (set! cache (cons e cache))
	       (let ((listeners (hashtable-get listeners nt)))
		  (when (pair? listeners)
		     (let ((he (instantiate::upnp-event
				  (name "ssdp-notify")
				  (target upnp)
				  (value e))))
			(apply-listeners listeners he)))))))))

;*---------------------------------------------------------------------*/
;*    upnp-onmessage ::ssdp-response ...                               */
;*---------------------------------------------------------------------*/
(define-method (upnp-onmessage e::ssdp-response upnp)
;*    (tprint "UPNP-RESPONSE: " e)                                     */
   (with-access::ssdp-response e (usn st)
      (with-access::upnp upnp (mutex listeners cache)
	 (synchronize mutex
	    (set! cache (update-cache! cache))
;* 	    (tprint "USN=" usn)                                        */
;* 	    (tprint "CACHE=" cache)                                    */
	    (unless (find (lambda (e::ssdp-discovery)
			     (with-access::ssdp-discovery e ((usne usn))
				(string=? usn usne)))
		       cache)
	       (set! cache (cons e cache))
	       (let ((listeners (hashtable-get listeners st)))
		  (when (pair? listeners)
		     (let ((he (instantiate::upnp-event
				  (name "ssdp-response")
				  (target upnp)
				  (value e))))
			(apply-listeners listeners he)))))))))

;*---------------------------------------------------------------------*/
;*    upnp-onmessage ::ssdp-m-search ...                               */
;*---------------------------------------------------------------------*/
(define-method (upnp-onmessage e::ssdp-m-search upnp)
;*    (tprint "M-SEARCH...todo")                                       */
   #f)

;*---------------------------------------------------------------------*/
;*    upnp-backend ...                                                 */
;*---------------------------------------------------------------------*/
(define (upnp-backend)
   
   (define (ssdp-loop upnp)
      (with-access::upnp upnp (ssdp socket)
	 (set! ssdp 
	    (ssdp-discover-loop
	       :ondiscovery (lambda (e) (upnp-onmessage e upnp))
	       :socket socket))))
   
   (synchronize upnp-mutex
      (unless *upnp-backend*
	 (let ((socket (make-datagram-server-socket 1900)))
	    (set! *upnp-backend*
	       (co-instantiate ((upnp (instantiate::upnp
					 (socket socket)))
				(th (instantiate::hopthread
				       (name "upnp")
				       (body (lambda () (ssdp-loop upnp))))))
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
	 (hashtable-add! listeners evt cons proc '()))
      ;; notify on past events
      (synchronize mutex
	 (set! cache (update-cache! cache))
	 (for-each (lambda (e) (apply-listeners (list proc) e))
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


;*---------------------------------------------------------------------*/
;*    conditional compilation                                          */
;*---------------------------------------------------------------------*/
))
