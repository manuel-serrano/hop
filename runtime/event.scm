;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/event.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 27 05:45:08 2005                          */
;*    Last change :  Wed Oct  3 05:50:13 2007 (serrano)                */
;*    Copyright   :  2005-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of server events                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_event

   (include "xml.sch"
	    "service.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_hop-extra
	    __hop_misc
	    __hop_hop
	    __hop_http-response
	    __hop_js-lib
	    __hop_cgi
	    __hop_read
	    __hop_service
	    __hop_http-response)

   (static  (class http-response-event::%http-response
	       (name::bstring read-only)))
	    
   (export  (hop-event-init! ::obj)
	    (hop-event-signal! ::bstring ::obj)
	    (hop-event-broadcast! ::bstring ::obj)
	    (hop-event-client-ready? ::bstring)))

;*---------------------------------------------------------------------*/
;*    *event-mutex* ...                                                */
;*---------------------------------------------------------------------*/
(define *event-mutex* (make-mutex "hop-event"))

;*---------------------------------------------------------------------*/
;*    event services ...                                               */
;*---------------------------------------------------------------------*/
(define *port-service* #f)
(define *register-service* #f)
(define *init-service* #f)
(define *client-key* 0)
(define *default-request* (instantiate::http-request))

;*---------------------------------------------------------------------*/
;*    hop-event-init! ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-event-init! port)
   
   (define (ajax-register-event! req name key)
      
      (define (ajax-register-active!)
	 ;; set an output timeout on the socket
	 (output-timeout-set! (socket-output (http-request-socket req))
			      (hop-connection-timeout))
	 (hashtable-update! *ajax-active-table*
			    name
			    (lambda (l)
			       (cons (cons key req) l))
			    (list (cons key req)))
	 (instantiate::http-response-persistent
	    (request req)))
      
      (let ((waiting (hashtable-get *ajax-wait-table* name)))
	 (if (pair? waiting)
	     (let ((bucket (assq key waiting)))
		(if (and (pair? bucket) (pair? (cdr bucket)))
		    (let ((val (cadr bucket)))
		       ;; remove the entry from the waiting table and send it
		       (set-cdr! (cdr bucket) (cddr bucket))
		       (ajax-signal-value req val))
		    (ajax-register-active!)))
	     (ajax-register-active!))))
   
   (define (flash-register-event! req name key)
      ;; set an output timeout on the socket
      (output-timeout-set! (socket-output (http-request-socket req))
			   (hop-connection-timeout))
      ;; update the event table
      (hashtable-update! *flash-socket-table*
			 name
			 (lambda (x) (cons req (filter-requests x)))
			 (list req))
      (instantiate::http-response-string))

   (with-lock *event-mutex*
      (lambda ()
	 (when (=fx *client-key* 0)
	    (set! *client-key* (elong->fixnum (current-seconds)))
	    
	    (set! *port-service*
		  (service :url "server-event-info" ()
		     (vector (hostname) port (get-ajax-key port))))
	    
	    (set! *init-service*
		  (service :url "server-event-init" (key)
		     (let ((req (current-request)))
			;; read the Flash's ending zero byte
			(read-byte (socket-input (http-request-socket req)))
			(set! *flash-request-list*
			      (cons (cons (string->symbol key) req)
				    *flash-request-list*))
			(instantiate::http-response-event
			   (request req)
			   (name key)))))
	    
	    (set! *register-service*
		  (service :url "server-event-register" (event key flash)
		     (let ((req (current-request))
			   (key (string->symbol key)))
			(if flash
			    (let ((req (cdr (assq key *flash-request-list*))))
			       (flash-register-event! req event key))
			    (ajax-register-event! req event key)))))))))

;*---------------------------------------------------------------------*/
;*    *flash-socket-table* ...                                         */
;*---------------------------------------------------------------------*/
(define *flash-socket-table* (make-hashtable))
(define *flash-request-list* '())

;*---------------------------------------------------------------------*/
;*    *ajax-wait-table* ...                                            */
;*---------------------------------------------------------------------*/
(define *ajax-wait-table* (make-hashtable))
(define *ajax-active-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    http-response ::http-response-event ...                          */
;*---------------------------------------------------------------------*/
(define-method (http-response r::http-response-event socket)
   (let ((p (socket-output socket)))
      (fprintf p "<acknowledge name='~a'/>" (http-response-event-name r))
      (display #a000 p)
      (flush-output-port p)
      'persistent))

;*---------------------------------------------------------------------*/
;*    filter-requests ...                                              */
;*    -------------------------------------------------------------    */
;*    Filters out the requests whose socket is closed.                 */
;*---------------------------------------------------------------------*/
(define (filter-requests l)
   (filter (lambda (r)
	      (let ((s (http-request-socket r)))
		 (if (socket-down? s)
		     (begin
			;; close the socket
			(socket-close s)
			;; remove the connection from the *flash* table
			(mutex-lock! *event-mutex*)
			(set! *flash-request-list*
			      (filter! (lambda (x) (not (eq? (cdr x) r)))
				       *flash-request-list*))
			(mutex-unlock! *event-mutex*)
			#f)
		     s)))
	   l))

;*---------------------------------------------------------------------*/
;*    get-ajax-key ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-ajax-key port)
   (mutex-lock! *event-mutex*)
   (set! *client-key* (+fx 1 *client-key*))
   (let ((r (format "~a:~a://~a" (hostname) port *client-key*)))
      (mutex-unlock! *event-mutex*)
      r))

;*---------------------------------------------------------------------*/
;*    ajax-signal-value ...                                            */
;*---------------------------------------------------------------------*/
(define (ajax-signal-value req resp)
   (let ((s (http-request-socket req)))
      (with-handler
	 (lambda (e)
	    (unless (&io-timeout-error e)
	       (raise e)))
	 (begin
	    (http-response resp s)
	    (socket-close s)))))

;*---------------------------------------------------------------------*/
;*    ajax-make-signal-value ...                                       */
;*---------------------------------------------------------------------*/
(define (ajax-make-signal-value val req)
   (scheme->response val req))

;*---------------------------------------------------------------------*/
;*    flash-signal-value ...                                           */
;*---------------------------------------------------------------------*/
(define (flash-signal-value req name value)
   (let* ((s (http-request-socket req))
	  (p (socket-output s)))
      (with-handler
	 (lambda (e)
	    (if (&io-timeout-error? e)
		(socket-close s)
		(raise e)))
	 (begin
	    (fprintf p "<event name='~a'>" name)
	    (display value p)
	    (display "</event>\n" p)
	    (display #a000 p)
	    (flush-output-port p)))))

;*---------------------------------------------------------------------*/
;*    flash-make-signal-value ...                                      */
;*---------------------------------------------------------------------*/
(define (flash-make-signal-value value)
   (cond
      ((xml? value)
       (xml->string value (hop-char-encoding) (hop-xml-backend)))
      ((or (string? value) (number? value))
       value)
      (else
       (string-append "<json>" (hop->json value) "</json>"))))

;*---------------------------------------------------------------------*/
;*    for-each-socket ...                                              */
;*---------------------------------------------------------------------*/
(define (for-each-socket table name proc)
   (let ((r #f))
      (hashtable-update! table
			 name
			 (lambda (l)
			    (let ((l2 (filter-requests l)))
			       (set! r (proc l2))
			       l2))
			 '())
      r))
   
;*---------------------------------------------------------------------*/
;*    hop-event-signal! ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-event-signal! name value)

   (define (ajax-event-signal! name value)
      (let* ((al (hashtable-get *ajax-active-table* name))
	     (wl (hashtable-get *ajax-wait-table* name))
	     (req (if (pair? al) (cdar al) *default-request*))
	     (val (ajax-make-signal-value value req)))
	 (if (pair? al)
	     (let* ((a (car al))
		    (key (car a))
		    (req (cdr a)))
		;; signal the value
		(ajax-signal-value req val)
		;; update the wait list
		(hashtable-update! *ajax-wait-table*
				   name
				   (lambda (l)
				      (cons (cons key val) l))
				   (list (cons key val)))
		;; we the active connection have been thrown out
		(hashtable-put! *ajax-active-table*
				name
				(delete! a al)))
	     ;; update the wait list
	     (when (pair? wl)
		(let ((w (car wl)))
		   (if (pair? (cdr w))
		       (set-cdr! w (append! (cdr w) (list val)))
		       (set-cdr! w (list val))))))))
   
   (define (flash-event-signal! name value)
      (for-each-socket
       *flash-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (flash-make-signal-value value)))
		(flash-signal-value (car l) name val)
		#t)))))

   (mutex-lock! *event-mutex*)
   (unless (flash-event-signal! name value)
      (ajax-event-signal! name value))
   (mutex-unlock! *event-mutex*))

;*---------------------------------------------------------------------*/
;*    hop-event-broadcast! ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-event-broadcast! name value)
   
   (define (ajax-event-broadcast! name value)
      (let* ((al (hashtable-get *ajax-active-table* name))
	     (wl (hashtable-get *ajax-wait-table* name))
	     (req (if (pair? al) (cdar al) *default-request*))
	     (val (ajax-make-signal-value value req)))
	 ;; update the wait list
	 (when (pair? wl)
	    (for-each (lambda (w)
			 (when (pair? (cdr w))
			    (set-cdr! w (append! (cdr w) (list val)))))
		      wl))
	 ;; signal the active connections
	 (when (pair? al)
	    (for-each (lambda (a)
			 (let ((key (car a))
			       (req (cdr a)))
			    ;; signal the value
			    (ajax-signal-value req val)
			    ;; update the wait list
			    (hashtable-update!
			     *ajax-wait-table*
			     name
			     (lambda (l)
				(cons (cons key val) l))
			     (list (cons key val)))))
		      al)
	    ;; the active connection have been thrown out
	    (hashtable-put! *ajax-active-table* name '()))))
   
   (define (flash-event-broadcast! name value)
      (for-each-socket
       *flash-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (flash-make-signal-value value)))
		(for-each (lambda (req)
			     (flash-signal-value req name val))
			  l))))))

   (mutex-lock! *event-mutex*)
   (ajax-event-broadcast! name value)
   (flash-event-broadcast! name value)
   (mutex-unlock! *event-mutex*)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hop-event-client-ready? ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-event-client-ready? name)
   
   (define (ajax-event-client-ready? name)
      (let ((wl (hashtable-get *ajax-wait-table* name)))
	 (and (pair? wl)
	      (any? (lambda (a)
		       (let* ((req (cdr a))
			      (s (http-request-socket req)))
			  (not (socket-down? s))))
		    wl))))

   (define (flash-event-client-ready? name)
      (let ((l (hashtable-get *flash-socket-table* name)))
	 (and (pair? l)
	      (any? (lambda (req)
		       (let ((s (http-request-socket req)))
			  (not (socket-down? s))))
		    l))))

   (or (ajax-event-client-ready? name)
       (flash-event-client-ready? name)))
