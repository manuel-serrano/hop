;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/event.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 27 05:45:08 2005                          */
;*    Last change :  Thu Feb 23 02:45:44 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of the event loop                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_event

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_hop
	    __hop_http-response
	    __hop_js-lib
	    __hop_service)

   (export  (class hop-event
	       (%hop-event-init!)
	       (name::bstring read-only)
	       (queue-size::int read-only (default 1))
	       (guard::procedure (default (lambda () #t)))
	       (%service (default #unspecified))
	       (%request (default #f))
	       (%closep::bool (default #f))
	       (%fifo::pair-nil (default '()))
	       (%fifol::pair-nil (default '())))

	    (%hop-event-init! ::hop-event)
	    (signal-hop-event! ::hop-event ::obj)
	    (hop-event-close ::hop-event)
	    
	    (<HOP-EVENT> . args)
	    (<TIMEOUT-EVENT> . args)))

;*---------------------------------------------------------------------*/
;*    check-queued-events ...                                          */
;*---------------------------------------------------------------------*/
(define (check-queued-events evt::hop-event)
   (with-access::hop-event evt (%request %fifo %fifol)
      (define (pop!)
	 (let ((r (car %fifo)))
	    (set-car! %fifo #unspecified)
	    (set! %fifo (cdr %fifo))
	    r))
      (define (send! val socket)
	 (http-response (scheme->response val %request) socket)
	 (set! %request #f)
	 (socket-close socket))
      (if (and (not (null? %fifo))
	       (not (eq? (car %fifo) #unspecified))
	       (http-request? %request))
	  (let* ((socket (http-request-socket %request))
		 (port (socket-output socket)))
	     (if (output-port? port)
		 (begin
		    (send! (pop!) socket)
		    'pop))))))
   
;*---------------------------------------------------------------------*/
;*    %hop-event-init! ...                                             */
;*---------------------------------------------------------------------*/
(define (%hop-event-init! evt::hop-event)
   (with-access::hop-event evt (%service %request %closep queue-size %fifo %fifol)
      (when (>fx queue-size 0)
	 (set! %fifo (make-list queue-size #unspecified))
	 (set-cdr! (last-pair %fifo) %fifo)
	 (set! %fifol %fifo))
      (set! %service
	    (procedure->service
	     (lambda (name)
		(if %closep
		    (instantiate::http-response-string
		       (start-line "HTTP/1.0 400 Bad Request")
		       (body (format "Closed server event `~a'" name)))
		    (begin
		       (set! %request (the-current-request))
		       (check-queued-events evt)
		       (instantiate::http-response-persistent))))))))
   
;*---------------------------------------------------------------------*/
;*    signal-hop-event! ...                                            */
;*---------------------------------------------------------------------*/
(define (signal-hop-event! evt val)
   (with-access::hop-event evt (%request %fifo %fifol)
      (define (push! val)
	 (if (pair? %fifol)
	     (begin
		(set-car! %fifol val)
		(set! %fifol (cdr %fifol))
		'push)
	     'ignore))
      (push! val)
      (check-queued-events evt)))

;*---------------------------------------------------------------------*/
;*    hop-event-close ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-event-close evt)
   (with-access::hop-event evt (%request %closep)
      (when (http-request? %request)
	 (let ((socket (http-request-socket %request)))
	    (socket-close socket)
	    (set! %request #f)
	    (set! %closep #f)))))

;*---------------------------------------------------------------------*/
;*    HOP-EVENT ...                                                    */
;*---------------------------------------------------------------------*/
(define-xml-compound <HOP-EVENT> ((id #unspecified string)
				  (event #f hop-event)
				  (handler #f)
				  (failure #f)
				  body)
   (cond
      ((not event)
       (error '<HOP-EVENT> "Event missing" handler))
      ((not handler)
       (error '<HOP-EVENT> "Event handler missing" event))
      (else
       (cons (<SCRIPT>
		:id (xml-make-id id 'HOP-EVENT)
		(format "hop_event_handler_set( ~a, \"~a\", function( event ) { ~a; return true; }, function( event ) { ~a; return true; } )"
			(scheme->javascript (hop-event-%service event))
			(string-for-read (hop-event-name event))
			handler
			(or failure "false")))
	     body))))

;*---------------------------------------------------------------------*/
;*    TIMEOUT-EVENT ...                                                */
;*---------------------------------------------------------------------*/
(define-xml-compound <TIMEOUT-EVENT> ((id #unspecified string)
				      (eager #f boolean)
				      (timeout 1000 integer)
				      (handler #f)
				      body)
   (cond
      ((not handler)
       (error '<TIMEOUT-EVENT> "Event handler missing" id))
      (else
       (let ((id (xml-make-id id 'TIMEOUT-EVENT)))
	  (cons (<SCRIPT>
		   :id id
		   (format "hop_timeout( \"~a\", function() { ~a; return true; }, ~a, ~a )"
			   id handler timeout (if eager "true" "false")))
		body)))))
		      
	  
					   
   
   

