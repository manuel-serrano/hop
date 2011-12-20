;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/zeroconf_avahi.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:04:07 2011                          */
;*    Last change :  Tue Dec 20 17:54:15 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Avahi support for Hop                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(directives
   (import __hop_configure
	   __hop_param
	   __hop_misc
	   __hop_weblets
	   __hop_types)
   (library avahi pthread)
   (static (class avahi::zeroconf
	      (poll::avahi-simple-poll (default (class-nil avahi-simple-poll)))
	      (client::avahi-client (default (class-nil avahi-client)))
	      (thread (default #f)))))

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-init! ::avahi ...                                   */
;*---------------------------------------------------------------------*/
(define-method (hop-zeroconf-init! o::avahi)
   (with-access::avahi o (poll client thread)
      (set! poll (instantiate::avahi-simple-poll))
      (set! client (instantiate::avahi-client
		      (proc (lambda (c s) (client-callback c s)))
		      (poll poll)))

      (with-access::avahi-client client (version)
	 (hop-verb 1 (format "Zeroconf (avahi ~a) setup...\n" version)))))

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-start! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (hop-zeroconf-start! o::avahi)
   (with-access::avahi o (poll client thread)
      (unless thread
	 (set! thread (instantiate::pthread
			 (body (lambda ()
				  (unwind-protect
				     (avahi-simple-poll-loop poll)
				     (begin
					(avahi-client-close client)
					(avahi-simple-poll-close poll)))))))
	 (thread-start! thread))))

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-close! ::avahi ...                                  */
;*---------------------------------------------------------------------*/
(define-method (hop-zeroconf-close! o::avahi)
   (with-access::avahi o (poll)
      (avahi-simple-poll-timeout poll
	 0
	 (lambda ()
	    (avahi-simple-poll-quit poll)))))

;*---------------------------------------------------------------------*/
;*    client-callback ...                                              */
;*---------------------------------------------------------------------*/
(define (client-callback client::avahi-client state::symbol)
   (case state
      ((avahi-client-failure)
       (warning "hop-zeroconf" (avahi-client-error-message client) state)
       (avahi-simple-poll-quit (-> client poll)))
      ((avahi-client-collision avahi-client-registering avahi-client-connecting)
       #unspecified)))

;*---------------------------------------------------------------------*/
;*    entry-group-callback ...                                         */
;*---------------------------------------------------------------------*/
(define (entry-group-callback egroup::avahi-entry-group state::symbol)
   (case state
      ((avahi-entry-group-collision)
       (raise
	  (instantiate::&avahi-collision-error
	     (proc "zeroconf-avahi")
	     (msg "Service name collision")
	     (obj egroup))))
      ((avahi-entry-group-failure)
       (error "zeroconfg-avahi"
	  "Cannot register service"
	  (avahi-client-error-message (-> egroup client))))))

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-publish-service! ::avahi ...                        */
;*---------------------------------------------------------------------*/
(define-method (hop-zeroconf-publish-service! o::avahi name port type opts)
   (hop-zeroconf-start! o)
   (with-access::avahi o (client poll)
      (avahi-simple-poll-timeout poll
	 1
	 (lambda ()
	    (let ((group (instantiate::avahi-entry-group
			    (proc entry-group-callback)
			    (client client))))
	       (let loop ((name name))
		  (with-handler
		     (lambda (e)
			(if (isa? e &avahi-collision-error)
			    (begin
			       (avahi-entry-group-reset! group)
			       (loop (avahi-alternative-service-name name)))
			    (raise e)))
		     (begin
			;; add the service for hop
			(apply avahi-entry-group-add-service! group
			   :name name
			   :type type
			   :port (hop-port)
			   opts)
			;; tell the server to register the service
			(avahi-entry-group-commit group)))))))))

;*---------------------------------------------------------------------*/
;*    service-browser ...                                              */
;*---------------------------------------------------------------------*/
(define (service-browser o::avahi zd type proc)
   (with-access::avahi o (client poll)
      (instantiate::avahi-service-browser
	 (client client)
	 (type type)
	 (proc (lambda (b intf proto event name type domain flags)
		  (when (or (eq? event 'avahi-browser-new)
			    (eq? event 'avahi-browser-remove))
		     (instantiate::avahi-service-resolver
			(client client)
			(interface intf)
			(protocol proto)
			(name name)
			(type type)
			(domain domain)
			(proc (lambda (r intf proto event svc type domain
					 host addr port txtlst flags)
				 (let* ((name (if (eq? event 'avahi-resolver-found)
						  "add" "remove"))
					(proto (case proto
						  ((avahi-proto-inet)
						   "ipv4")
						  ((avahi-proto-inet6)
						   "ipv6")
						  (else
						   "unknown")))
					(evt (instantiate::zeroconf-service-event
						(name name)
						(target zd)
						(interface intf)
						(protocol proto)
						(value svc)
						(type type)
						(domain domain)
						(hostname host)
						(address addr)
						(options txtlst))))
				    (proc evt)))))))))))

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-publish-service! ::avahi ...                        */
;*---------------------------------------------------------------------*/
(define-method (hop-zeroconf-add-service-event-listener! o::avahi zd event proc)
      (cond
	 ((string? event)
	  (service-browser o zd event proc))
;* 	 ((not event)                                                  */
;* 	  (instantiate::avahi-service-type-browser                     */
;* 		     (client client)                                   */
;* 		     (proc (lambda (b intf proto even type domain flags) */
;* 			      (start-service-browser client type proc))))) */
;* 	  (instantiate::avahi-service-browser                          */
;* 	     (client client)                                           */
;* 	     (type event)                                              */
;* 	     (proc (lambda (b intf proto event name type domain flags) */
;* 		      (when (or (eq? event 'avahi-browser-new)         */
;* 				(eq? event 'avahi-browser-remove))     */
;* 			 (instantiate::avahi-service-resolver          */
;* 			    (client client)                            */
;* 			    (interface intf)                           */
;* 			    (protocol proto)                           */
;* 			    (name name)                                */
;* 			    (type type)                                */
;* 			    (domain domain)                            */
;* 			    (proc (lambda (r intf proto event svc type domain */
;* 					     host addr port txtlst flags) */
;* 				     (let* ((name (if (eq? event 'avahi-resolver-found) */
;* 						      "add" "remove")) */
;* 					    (proto (case proto         */
;* 						      ((avahi-proto-inet) */
;* 						       "ipv4")         */
;* 						      ((avahi-proto-inet6) */
;* 						       "ipv6")         */
;* 						      (else            */
;* 						       "unknown")))    */
;* 					    (evt (instantiate::zeroconf-service-event */
;* 						    (name name)        */
;* 						    (target zd)        */
;* 						    (interface intf)   */
;* 						    (protocol proto)   */
;* 						    (value svc)        */
;* 						    (type type)        */
;* 						    (domain domain)    */
;* 						    (hostname host)    */
;* 						    (address addr)     */
;* 						    (options txtlst)))) */
;* 					(proc evt))))))))))            */
	 (else
	  (error "add-event-listener! ::zeroconf-service-discoverer"
	     "Illegal event (should be string or #f)"
	     event))))

;*---------------------------------------------------------------------*/
;*    Register the avahi-backend                                       */
;*---------------------------------------------------------------------*/
(hop-zeroconf-register-backend!
   (instantiate::avahi
      (name "avahi")))
