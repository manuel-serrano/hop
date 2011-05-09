;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/discovery.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May  1 17:02:55 2011                          */
;*    Last change :  Fri May  6 16:06:39 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop discovery mechanism (for automatically discovery other       */
;*    running Hops).                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_discovery
   
   (cond-expand
      (enable-threads
       (library pthread)))

   (include "service.sch")

   (import __hop_configure
	   __hop_service
	   __hop_types
	   __hop_hop
	   __hop_param
	   __hop_misc
	   __hop_read
	   __hop_event)
      
   (export (class discoverer
	      (filter::procedure read-only (default (lambda (h p k s) #t)))
	      (%mutex read-only (default (make-mutex)))
	      (%listeners::pair-nil (default '())))

	   (class discover-event::event
	      (key::bstring read-only)
	      (port::int read-only))

	   (hop-discovery-init!)
	   (hop-discovery-server ::int)
	   (hop-discover #!key
	      (address::bstring "255.255.255.255")
	      (port::int (hop-discovery-port))
	      service)))

;*---------------------------------------------------------------------*/
;*    discovery-key ...                                                */
;*---------------------------------------------------------------------*/
(define discovery-key 0)

;*---------------------------------------------------------------------*/
;*    discovery-service ...                                            */
;*---------------------------------------------------------------------*/
(define discovery-service #f)

;*---------------------------------------------------------------------*/
;*    discovery-host ...                                               */
;*---------------------------------------------------------------------*/
(define discovery-host #f)

;*---------------------------------------------------------------------*/
;*    discovers ...                                                    */
;*---------------------------------------------------------------------*/
(define discovers '())

;*---------------------------------------------------------------------*/
;*    mutex                                                            */
;*---------------------------------------------------------------------*/
(define discovery-mutex (make-mutex))

;*---------------------------------------------------------------------*/
;*    hop-discovery-server ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-discovery-server port)
   (cond-expand
      (enable-threads
	 (let ((serv (make-datagram-server-socket port)))
	    (thread-start-joinable!
	       (instantiate::pthread
		  (body (lambda () (discovery-loop serv)))))))
      (else
       (error "hop-discovery" "discovery requires thread support" #f))))

;*---------------------------------------------------------------------*/
;*    discovery-loop ...                                               */
;*---------------------------------------------------------------------*/
(define (discovery-loop serv::datagram-socket)
   
   (define (service-exists? svc)
      (let ((creq (current-request)))
	 (unwind-protect
	    (let ((req (instantiate::http-server-request
			  (abspath (string-append (hop-service-base) "/" svc))
			  (method 'get))))
	       (current-request-set! #f req)
	       (let ((rep (service-filter req)))
		  (when (%http-response-local? rep)
		     (with-access::%http-response-local rep (start-line)
			(or (substring-at? start-line "200" 9 3)
			    (substring-at? start-line "401" 9 3))))))
	    (current-request-set! #f creq))))
   
   (let loop ((id -1))
      (multiple-value-bind (msg host)
	 (datagram-socket-receive serv 1024)
	 (let ((l (string-split msg)))
	    (when (=fx (length l) 2)
	       (let ((svc (cadr l))
		     (port (string->number (car l))))
		  (when (and (or (not (string=? host discovery-host))
				 (not (=fx port (hop-port))))
			     (or (string=? svc "*")
				 (service-exists? svc)))
		     (hop-verb 2 (hop-color id id " DISCOVERY ")
			host ":" port "\n")
		     (hop-discovery-reply host port svc))))))
      (loop (-fx id 1))))

;*---------------------------------------------------------------------*/
;*    hop-discovery-reply ...                                          */
;*---------------------------------------------------------------------*/
(define (hop-discovery-reply host port service)
   (when (=fx discovery-key 0)
      (set! discovery-key
	 (bit-rsh (absfx (elong->fixnum (date->seconds (current-date)))) 2)))
   (let ((url (format "http://~a:~a/hop/discovery?host=~a&port=~a&key=~a&service=~a" host port
		 (hostname) (hop-port) discovery-key service)))
      (with-url url (lambda (e) #unspecified))))
      
;*---------------------------------------------------------------------*/
;*    hop-discovery-init! ...                                          */
;*---------------------------------------------------------------------*/
(define (hop-discovery-init!)
   (set! discovery-host (host (hostname)))
   (set! discovery-service
      (service :name "discovery" (#!key host port key service)
	 (mutex-lock! discovery-mutex)
	 ;; find all the discovers that matches that event
	 (let ((d (filter (lambda (d)
			     (with-access::discoverer d (filter %listeners)
				(when (pair? %listeners)
				   (filter host port key service))))
		     discovers)))
	    (mutex-unlock! discovery-mutex)
	    ;; invoke all the discovers
	    (for-each (lambda (d)
			 (with-access::discoverer d (%listeners %mutex)
			    (with-lock %mutex
			       (lambda ()
				  (let ((e (instantiate::discover-event
					      (name "discover")
					      (target service)
					      (value host)
					      (port (string->number port))
					      (key key))))
				     (let loop ((l %listeners))
					(when (pair? l)
					   ((caar l) e)
					   (unless (event-stopped? e)
					      (loop (cdr l))))))))))
	       d)))))
				  
;*---------------------------------------------------------------------*/
;*    hop-discover ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop-discover
	   #!key
	   (address::bstring "255.255.255.255")
	   (port::int (hop-discovery-port))
	   service)
   (let* ((sock (make-datagram-client-socket address port #t))
	  (msg (format "~a ~a" (hop-port)
		  (if (string? service) service "*"))))
      (display msg (datagram-socket-output-port sock))
      (datagram-socket-close sock)
      #t))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::discoverer ...                             */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! d::discoverer event proc . capture)
   (when (equal? event "discover")
      (with-access::discoverer d (%listeners %mutex)
	 (mutex-lock! %mutex)
	 (when (null? %listeners)
	    (mutex-lock! discovery-mutex)
	    (set! discovers (cons d discovers))
	    (mutex-unlock! discovery-mutex))
	 (set! %listeners (cons (cons proc capture) %listeners))
	 (mutex-unlock! %mutex))))

;*---------------------------------------------------------------------*/
;*    remove-event-listener! ::discoverer ...                          */
;*---------------------------------------------------------------------*/
(define-method (remove-event-listener! d::discoverer event proc . capture)
   (when (equal? event "discover")
      (with-access::discoverer d (%listeners %mutex)
	 (mutex-lock! %mutex)
	 (let ((c (assq proc %listeners)))
	    (when (and (pair? c) (equal? (cdr c) capture))
	       (set! %listeners (remq! c %listeners))
	       (when (null? %listeners)
		  (mutex-lock! discovery-mutex)
		  (set! discovers (remq! d discovers))
		  (mutex-unlock! discovery-mutex))))
	 (mutex-unlock! %mutex))))
