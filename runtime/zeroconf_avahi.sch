;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/zeroconf_avahi.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:04:07 2011                          */
;*    Last change :  Mon Feb 18 09:40:50 2013 (serrano)                */
;*    Copyright   :  2011-13 Manuel Serrano                            */
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
   
   (export (class avahi::zeroconf
	      (lock::mutex read-only (default (make-mutex)))
	      (state::symbol (default 'init))
	      (poll::avahi-simple-poll (default (class-nil avahi-simple-poll)))
	      (client::avahi-client (default (class-nil avahi-client))))))

;*---------------------------------------------------------------------*/
;*    avahi-wait-ready! ...                                            */
;*---------------------------------------------------------------------*/
(define (avahi-wait-ready! o::avahi proc)
   ;; apply proc is already initialized, otherwise, register the callback
   (with-access::avahi o (lock state client poll onready)
      (if (eq? state 'ready)
	  (avahi-apply proc o)
	  (let loop ()
	     (case state
		((init)
		 (avahi-add-onready-listener! o proc))
		((failure)
		 (warning "avahi failure, action ignored")
		 #f)
		((ready)
		 (avahi-apply proc o)))))))

;*---------------------------------------------------------------------*/
;*    avahi-apply ...                                                  */
;*---------------------------------------------------------------------*/
(define (avahi-apply proc o)
   (with-access::avahi o (lock poll)
      (synchronize lock
	 (avahi-simple-poll-timeout poll 1
	    (lambda ()
	       (with-handler
		  (lambda (e)
		     (exception-notify e)
		     #f))
	       (proc o))))))

;*---------------------------------------------------------------------*/
;*    avahi-add-onready-listener! ...                                  */
;*---------------------------------------------------------------------*/
(define (avahi-add-onready-listener! o proc)
   (with-access::avahi o (lock onready)
      (synchronize lock
	 (let ((old onready))
	    (set! onready
	       (lambda (o)
		  (old o)
		  (avahi-apply proc o)))))))

;*---------------------------------------------------------------------*/
;*    avahi-failure ...                                                */
;*---------------------------------------------------------------------*/
(define (avahi-failure o #!optional err)
   (with-access::avahi o (poll client lock state)
      (synchronize lock
	 (set! state 'failure)
	 (hop-verb 1 "  zeroconf: "
	    (hop-color 4 "error" "")
	    (cond
	       ((string? err)
		(format " (~a)\n" err))
	       ((isa? err &error)
		(with-access::&error err (proc obj msg)
		   (format " (~a: ~a -- ~a)\n" proc msg (typeof obj))))
	       (else
		"\n"))))))
   
;*---------------------------------------------------------------------*/
;*    zeroconf-backend-start ::avahi ...                               */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-backend-start o::avahi)
   (call-next-method)
   (thread-start!
      (instantiate::pthread
	 (body (lambda ()
		  (with-access::avahi o (poll client lock state exception)
		     (with-handler
			(lambda (e)
			   (avahi-failure o e))
			(begin
			   (set! poll (instantiate::avahi-simple-poll))
			   (set! client (instantiate::avahi-client
					   (proc (lambda (c s)
						    (client-callback c s o)))
					   (poll poll)))
			   (avahi-simple-poll-loop poll)))))))))
   
;*---------------------------------------------------------------------*/
;*    zeroconf-backend-stop ::avahi ...                                */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-backend-stop o::avahi)
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
   (with-access::avahi o (lock state onready)
      (case cstate
	 ((avahi-client-failure)
	  (avahi-failure o (avahi-client-error-message client)))
	 ((avahi-client-collision avahi-client-registering avahi-client-connecting)
	  #unspecified)
	 ((avahi-client-running)
	  (set! state 'ready)
	  (with-access::avahi-client client (version)
	     (hop-verb 1 (format "  zeroconf: ~a\n" (hop-color 2 "" version))))
	  (onready o))
	 (else
	  (set! state 'failure)
	  (warning "zeroconf" "cannot connect to daemon" " -- " state)))))

;*---------------------------------------------------------------------*/
;*    zeroconf-backend-publish-service! ::avahi ...                    */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-backend-publish-service! o::avahi name port type opts)
   (avahi-wait-ready! o
      (lambda (o)
	 (let loop ((name name))
	    
	    (define (entry-group-callback egroup state)
	       (case state
		  ((avahi-entry-group-collision)
		   (collision egroup))
		  ((avahi-entry-group-failure)
		   (with-access::avahi-entry-group egroup (client)
		      (error "zeroconfg-avahi"
			 "Cannot register service"
			 (avahi-client-error-message client))))))

	    (define (collision group)
	       (avahi-entry-group-close group)
	       (loop (avahi-alternative-service-name name)))
	    
	    (with-access::avahi o (client poll)
	       (let ((group (instantiate::avahi-entry-group
			       (proc entry-group-callback)
			       (client client))))
		  ;; add the service for hop
		  (with-handler
		     (lambda (e)
			(if (isa? e &avahi-collision-error)
			    (collision group)
			    (raise e)))
		     (begin
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
(define (service-browser o::avahi type proc)
   
   (define (apply-safe proc arg)
      (with-handler
	 (lambda (e)
	    (exception-notify e)
	    #f))
      (proc arg))

   (define (avahi-protocol proto)
      (case proto
	 ((avahi-proto-inet) "ipv4")
	 ((avahi-proto-inet6) "ipv6")
	 (else "unknown")))

   (define (client-find-resolver client::avahi-client intf proto nme typ dmain)
      (with-access::avahi-client client (resolvers)
	 (find (lambda (r::avahi-service-resolver)
		  (with-access::avahi-service-resolver r (interface protocol type domain name)
		     (and (=fx interface intf)
			  (eq? protocol proto)
			  (string=? type typ)
			  (string=? domain dmain)
			  (string=? name nme))))
	    resolvers)))
			   
   (avahi-wait-ready! o
      (lambda (o)
	 (with-access::avahi o (client poll)
	    (instantiate::avahi-service-browser
	       (client client)
	       (type type)
	       (proc (lambda (b intf proto event name type domain flags)
			(case event
			   ((avahi-browser-new)
			    (instantiate::avahi-service-resolver
			       (client client)
			       (interface intf)
			       (protocol proto)
			       (name name)
			       (type type)
			       (domain domain)
			       (proc (lambda (r intf proto event value type domain
						host addr port txtlst flags)
					(when (eq? event 'avahi-resolver-found)
					   (let ((evt (instantiate::zeroconf-service-event
							 (name "found")
							 (target o)
							 (interface intf)
							 (protocol (avahi-protocol proto))
							 (value name)
							 (type type)
							 (domain domain)
							 (hostname host)
							 (port port)
							 (address addr)
							 (options txtlst))))
					      (apply-safe proc evt)))))))
			   ((avahi-browser-remove)
			    (let ((rv (client-find-resolver client intf proto name type domain)))
			       (when rv
				  (avahi-service-resolver-close rv)))
			    (let ((evt (instantiate::zeroconf-service-event
					  (name "removed")
					  (target o)
					  (interface intf)
					  (protocol (avahi-protocol proto))
					  (value name)
					  (type type)
					  (domain domain)
					  (hostname name)
					  (options '()))))
			       (apply-safe proc evt)))))))))))


;*---------------------------------------------------------------------*/
;*    zeroconf-backend-add-service-event-listener! ::avahi ...         */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-backend-add-service-event-listener! o::avahi event proc)
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
				  (service-browser o type proc))))))))))
      ((string=? event "onready")
       (avahi-add-onready-listener! o proc))
      (else
       (avahi-wait-ready! o
	  (lambda (o)
	     (service-browser o event proc))))))

;*---------------------------------------------------------------------*/
;*    Register the avahi backend                                       */
;*---------------------------------------------------------------------*/
(zeroconf-register-backend!
   (instantiate::avahi))
