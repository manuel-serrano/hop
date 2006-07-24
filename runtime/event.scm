;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/event.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 27 05:45:08 2005                          */
;*    Last change :  Sat Jul 22 09:20:04 2006 (serrano)                */
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
	       (queue-size::int read-only (default 20))
	       (%service (default #unspecified))
	       (%requests (default '()))
	       (%closep::bool (default #f))
	       (%fifo::pair-nil (default '()))
	       (%fifol::pair-nil (default '()))
	       (%mutex::mutex read-only (default (make-mutex "hop-event"))))

	    (%hop-event-init! ::hop-event)
	    (signal-hop-event! ::hop-event ::obj)
	    (broadcast-hop-event! ::hop-event ::obj)
	    (hop-event-close ::hop-event)
	    
	    (<HOP-EVENT> . args)
	    (<TIMEOUT-EVENT> . args)))

;*---------------------------------------------------------------------*/
;*    *void* ...                                                       */
;*---------------------------------------------------------------------*/
(define *void* (cons 0 0))

;*---------------------------------------------------------------------*/
;*    %hop-event-init! ...                                             */
;*---------------------------------------------------------------------*/
(define (%hop-event-init! evt::hop-event)
   (with-access::hop-event evt (%service %requests %closep queue-size %fifo %fifol %mutex)
      (when (>fx queue-size 0)
	 (set! %fifo (make-list queue-size *void*))
	 (set-cdr! (last-pair %fifo) %fifo)
	 (set! %fifol %fifo))
      (set! %service
	    (procedure->service
	     (lambda (name)
		(let ((req (the-current-request)))
		   (if %closep
		       (instantiate::http-response-string
			  (start-line "HTTP/1.0 400 Bad Request")
			  (request req)
			  (body (format "Closed server event `~a'" name)))
		       (begin
			  (mutex-lock! %mutex)
			  (let ((v (pop-queued-events! evt)))
			     (if (eq? v *void*)
				 (begin
				    (if (pair? %requests)
					(append! %requests (list req))
					(set! %requests (list req)))
				    (mutex-unlock! %mutex)
				    (instantiate::http-response-persistent
				       (request req)))
				 (begin
				    (mutex-unlock! %mutex)
				    v)))))))))))

;*---------------------------------------------------------------------*/
;*    pop-queued-events! ...                                           */
;*---------------------------------------------------------------------*/
(define (pop-queued-events! evt::hop-event)
   (with-access::hop-event evt (queue-size %fifo %fifol)
      (if (>fx queue-size 0)
	  (let ((v (car %fifo)))
	     (unless (eq? v *void*)
		(set-car! %fifo *void*)
		(set! %fifo (cdr %fifo)))
	     v)
	  *void*)))
	 
;*---------------------------------------------------------------------*/
;*    signal-hop-event! ...                                            */
;*---------------------------------------------------------------------*/
(define (signal-hop-event! evt val)
   (with-access::hop-event evt (%fifo %fifol queue-size %mutex %requests)
      (mutex-lock! %mutex)
      (let loop ()
	 (if (pair? %requests)
	     (let* ((req (car %requests))
		    (socket (http-request-socket req))
		    (port (socket-output socket)))
		(set! %requests (cdr %requests))
		(if (output-port? port)
		    (begin
		       (mutex-unlock! %mutex)
		       (http-response (scheme->response val req) socket)
		       (socket-close socket))
		    (loop)))
	     (begin
		(when (>fx queue-size 0)
		   (set-car! %fifol val)
		   (set! %fifol (cdr %fifol)))
		(mutex-unlock! %mutex))))))
      
;*---------------------------------------------------------------------*/
;*    broadcast-hop-event! ...                                         */
;*---------------------------------------------------------------------*/
(define (broadcast-hop-event! evt val)
   (with-access::hop-event evt (%fifo %fifol queue-size %mutex %requests)
      (mutex-lock! %mutex)
      (let loop ((f #t))
	 (if (pair? %requests)
	     (let* ((req (car %requests))
		    (socket (http-request-socket req))
		    (port (socket-output socket)))
		(set! %requests (cdr %requests))
		(if (output-port? port)
		    (begin
		       (http-response (scheme->response val req) socket)
		       (socket-close socket)
		       (loop #f))
		    (loop f)))
	     (when (and f (>fx queue-size 0))
		(set-car! %fifol val)
		(set! %fifol (cdr %fifol)))))
      (mutex-unlock! %mutex)))

;*---------------------------------------------------------------------*/
;*    hop-event-close ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-event-close evt)
   (with-access::hop-event evt (%requests %closep %mutex)
      (mutex-lock! %mutex)
      (unless %closep
	 (when (pair? %requests)
	    (for-each (lambda (r)
			 (let ((socket (http-request-socket r)))
			    (socket-close socket)))
		      %requests)
	    (set! %requests '())))
      (set! %closep #t)
      (mutex-unlock! %mutex)))

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
		:type "text/javascript"
		:id (xml-make-id id 'HOP-EVENT)
		(format "hop_event_handler_set( ~a, \"~a\", function( event ) { ~a; return true; }, function( event ) { ~a; return true; } )"
			(hop->json (hop-event-%service event))
			(string-for-read (hop-event-name event))
			(if (xml-tilde? handler)
			    (xml-tilde-body handler)
			    handler)
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
		   (format "hop_timeout( ~a, ~a, function() { ~a; return true; }, ~a )"
			   id timeout handler (if eager "true" "false")))
		body)))))
