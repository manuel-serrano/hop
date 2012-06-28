;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/zeroconf_avahi.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:04:07 2011                          */
;*    Last change :  Thu Jun 28 08:34:02 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Avahi support for Hop                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(directives

   (include "verbose.sch")
   
   (import __hop_configure
	   __hop_param
	   __hop_misc
	   __hop_weblets
	   __hop_types)
   
   (library avahi pthread)
   
   (static (class avahi::zeroconf
	      (lock::mutex read-only (default (make-mutex)))
	      (condv::condvar read-only (default (make-condition-variable)))
	      (state::symbol (default 'init))
	      (poll::avahi-simple-poll (default (class-nil avahi-simple-poll)))
	      (client::avahi-client (default (class-nil avahi-client))))))

;*---------------------------------------------------------------------*/
;*    avahi-wait-ready! ...                                            */
;*---------------------------------------------------------------------*/
(define (avahi-wait-ready! o::avahi proc)
   ;; wait for the initialization to be completed
   (with-access::avahi o (lock condv state)
      (if (eq? state 'ready)
	  (proc o)
	  (with-lock lock
	     (lambda ()
		(let loop ()
		   (case state
		      ((init)
		       (condition-variable-wait! condv lock)
		       (loop))
		      ((ready)
		       (proc o)))))))))

;*---------------------------------------------------------------------*/
;*    zeroconf-start ::avahi ...                                       */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-start o::avahi)
   
   ;; start the avahi thread
   (thread-start!
      (instantiate::pthread
	 (body (lambda ()
		  (with-access::avahi o (poll client lock condv state)
		     (set! poll (instantiate::avahi-simple-poll))
		     (set! client (instantiate::avahi-client
				     (proc (lambda (c s)
					      (client-callback c s o)))
				     (poll poll)))
		     (with-access::avahi-client client (version)
			(hop-verb 1
			   (format "Zeroconf (avahi ~a) setup...\n" version)))
		     (avahi-simple-poll-loop poll))))))

   (with-access::zeroconf o (onready)
      (avahi-wait-ready! o onready)))
   
;*---------------------------------------------------------------------*/
;*    zeroconf-stop ::avahi ...                                        */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-stop o::avahi)
   (with-access::avahi o (poll state lock)
      (avahi-simple-poll-timeout poll
	 0
	 (lambda ()
	    (avahi-simple-poll-quit poll)))
      (mutex-lock! lock)
      (set! state 'close)
      (mutex-unlock! lock)))

;*---------------------------------------------------------------------*/
;*    client-callback ...                                              */
;*---------------------------------------------------------------------*/
(define (client-callback client::avahi-client cstate::symbol o::avahi)
   (with-access::avahi o (lock condv state)
      (case cstate
	 ((avahi-client-failure)
	  (set! state 'failure)
	  (warning "zeroconf" (avahi-client-error-message client) " -- " state))
	 ((avahi-client-collision avahi-client-registering avahi-client-connecting)
	  #unspecified)
	 ((avahi-client-running)
	  (set! state 'ready)
	  (mutex-lock! lock)
	  (condition-variable-broadcast! condv)
	  (mutex-unlock! lock))
	 (else
	  (set! state 'failure)
	  (warning "zeroconf" "cannot connect to daemon" " -- " state)))))

;*---------------------------------------------------------------------*/
;*    entry-group-callback ...                                         */
;*---------------------------------------------------------------------*/
(define (entry-group-callback egroup::avahi-entry-group state::symbol)
   (with-access::avahi-entry-group egroup (client)
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
	     (avahi-client-error-message (-> egroup client)))))))

;*---------------------------------------------------------------------*/
;*    zeroconf-publish-service! ::avahi ...                            */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-publish-service! o::avahi name port type opts)
   (avahi-wait-ready! o
      (lambda (o)
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
			      (avahi-entry-group-commit group)))))))))))

;*---------------------------------------------------------------------*/
;*    service-browser ...                                              */
;*---------------------------------------------------------------------*/
(define (service-browser o::avahi zd type proc)
   (avahi-wait-ready! o
      (lambda (o)
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
						      (port port)
						      (address addr)
						      (options txtlst))))
					  (proc evt)))))))))))))

;*---------------------------------------------------------------------*/
;*    zeroconf-publish-service! ::avahi ...                            */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-add-service-event-listener! o::avahi zd event proc)
   (cond
      ((not (string? event))
       (error "add-event-listener! ::zeroconf-service-discoverer"
	  "Illegal event (should be string or #f)"
	  event))
      ((string=? event "")
       (avahi-wait-ready! o
	  (lambda (o)
	     (with-access::avahi o (client)
		(with-access::avahi-client client (poll)
		   (instantiate::avahi-service-type-browser
		      (client client)
		      (proc (lambda (b intf proto event type domain flags)
			       (when (eq? event 'avahi-browser-new)
				  (service-browser o zd type proc))))))))))
      (else
       (avahi-wait-ready! o
	  (lambda (o)
	     (service-browser o zd event proc))))))

;*---------------------------------------------------------------------*/
;*    Register the avahi backend                                       */
;*---------------------------------------------------------------------*/
(zeroconf-register-backend!
   (instantiate::avahi
      (name "avahi")))
