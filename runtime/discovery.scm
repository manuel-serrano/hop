;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/discovery.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May  1 17:02:55 2011                          */
;*    Last change :  Sat Dec  3 19:20:11 2011 (serrano)                */
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
	      (port::int read-only)
	      (hostname::bstring read-only)
	      (session::int read-only))
	   
	   (hop-discovery-init!)
	   (hop-discovery-server ::int)
	   (hop-discover #!key
	      (address::bstring "255.255.255.255")
	      (port::int (hop-discovery-port))
	      service
	      (broadcast::bool #t))))

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
	 (when (<fx port 1024)
	    (error "hop-discovery-server"
	       "Discovery ports must be greater than 1023"
	       port))
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
		  (when (isa? rep %http-response-local)
		     (with-access::%http-response-local rep (start-line)
			(or (substring-at? start-line "200" 9 3)
			    (substring-at? start-line "401" 9 3))))))
	    (current-request-set! #f creq))))
   
   (let loop ((id -1))
      (tprint ">>> DISCOVERY-LOOP id=" id)
      (multiple-value-bind (msg clienthost)
	 (datagram-socket-receive serv 1024)
	 (tprint "--- DISCOVERY-LOOP id=" id " msg=" msg " clienthost=" clienthost)
	 (let ((l (string-split msg)))
	    (when (=fx (length l) 2)
	       (let ((svc (cadr l))
		     (clientport (string->number (car l))))
		  (when (and (or (string=? svc "*")
				 (service-exists? svc))
			     (or (not (=fx clientport (hop-port)))
				 (not (string=? (host clienthost) discovery-host))))
		     (hop-discovery-reply
			(datagram-socket-host-address serv)
			clienthost clientport svc id))))))
      (tprint "<<< DISCOVERY-LOOP id=" id)
      (loop (-fx id 1))))

;*---------------------------------------------------------------------*/
;*    hop-discovery-reply ...                                          */
;*---------------------------------------------------------------------*/
(define (hop-discovery-reply hostip clienthost clientport service id)
   (hop-verb 2 (hop-color id id " DISCOVERY ") clienthost ":" clientport "\n")
   (when (=fx discovery-key 0)
      (set! discovery-key
	 (bit-rsh (absfx (elong->fixnum (current-seconds))) 2)))
   (let* ((name (hostname))
	  (url (format "http://~a:~a/hop/discovery?host=~a&port=~a&hostname=~a&key=~a&service=~a&session=~a" clienthost clientport
		  hostip (hop-port) hostip discovery-key service (hop-session))))
      (tprint "DISCOVER-REPLY id=" id " url=" url)
      (with-handler
	 (lambda (e) #f)
	 (with-url url (lambda (e) #unspecified)))))
      
;*---------------------------------------------------------------------*/
;*    hop-discovery-init! ...                                          */
;*---------------------------------------------------------------------*/
(define (hop-discovery-init!)
   
   (set! discovery-host (host (hostname)))
   
   (set! discovery-service
      (service :name "discovery" :id discovery (#!key host port hostname key service session)
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
					      (hostname hostname)
					      (port (string->number port))
					      (session (string->integer session))
					      (key key))))
				     (let loop ((l %listeners))
					(when (pair? l)
					   ((caar l) e)
					   (with-access::event e (stopped)
					      (unless stopped
						 (loop (cdr l)))))))))))
	       d)))))
				  
;*---------------------------------------------------------------------*/
;*    hop-discover ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop-discover #!key
	   (address::bstring "255.255.255.255")
	   (port::int (hop-discovery-port))
	   service
	   (broadcast::bool #t))
   (let* ((sock (make-datagram-client-socket address port broadcast))
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
