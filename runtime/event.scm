;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/event.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 27 05:45:08 2005                          */
;*    Last change :  Tue Oct 30 10:22:59 2007 (serrano)                */
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
	       (name::bstring read-only))

	    (class ajax-connection
	       (req (default #f))
	       key::symbol
	       (buffer::pair-nil (default '()))
	       (lastpair::pair-nil (default '()))
	       (date::elong (default (current-seconds)))
	       (count::int (default 0))))
	    
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
(define *unregister-service* #f)
(define *init-service* #f)
(define *client-key* 0)
(define *default-request* (instantiate::http-request))

;*---------------------------------------------------------------------*/
;*    *ajax-connection-table* ...                                      */
;*---------------------------------------------------------------------*/
(define *ajax-connection-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    ajax-find-connection ...                                         */
;*---------------------------------------------------------------------*/
(define (ajax-find-connection name key)
   (let ((bucket (hashtable-get *ajax-connection-table* name)))
      (let loop ((bucket bucket))
	 (when (pair? bucket)
	    (if (eq? key (ajax-connection-key (car bucket)))
		(car bucket)
		(loop (cdr bucket)))))))

;*---------------------------------------------------------------------*/
;*    ajax-store-connection! ...                                       */
;*---------------------------------------------------------------------*/
(define (ajax-store-connection! name conn)
   (hashtable-update! *ajax-connection-table*
		      name
		      (lambda (l) (cons conn l))
		      (list conn)))

;*---------------------------------------------------------------------*/
;*    ajax-connection-get! ...                                         */
;*    -------------------------------------------------------------    */
;*    Get values from a non empty buffer.                              */
;*---------------------------------------------------------------------*/
(define (ajax-connection-get! conn)
   (with-access::ajax-connection conn (buffer lastpair count date)
      (let ((res buffer))
	 (set! date (current-seconds))
	 (set! count 0)
	 (set! buffer '())
	 (set! lastpair '())
	 res)))

;*---------------------------------------------------------------------*/
;*    ajax-connection-put! ...                                         */
;*    -------------------------------------------------------------    */
;*    Put into a non-full buffer.                                      */
;*---------------------------------------------------------------------*/
(define (ajax-connection-put! conn val)
   (with-access::ajax-connection conn (buffer lastpair count)
      (set! count (+fx count 1))
      (if (pair? lastpair)
	  (set-cdr! lastpair (cons val '()))
	  (begin
	     (set! buffer (cons val '()))
	     (set! lastpair buffer)))
      val))

;*---------------------------------------------------------------------*/
;*    ajax-connection-empty? ...                                       */
;*---------------------------------------------------------------------*/
(define (ajax-connection-empty? conn)
   (with-access::ajax-connection conn (count)
      (=fx count 0)))

;*---------------------------------------------------------------------*/
;*    ajax-connection-available? ...                                   */
;*---------------------------------------------------------------------*/
(define (ajax-connection-available? conn)
   (with-access::ajax-connection conn (count date req)
      (or (http-request? req)
	  (and (<fx count (hop-event-buffer-size))
	       (<elong (-elong (current-seconds) date) (hop-event-timeout))))))
   
;*---------------------------------------------------------------------*/
;*    hop-event-init! ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-event-init! port)
   (with-lock *event-mutex*
      (lambda ()
	 (when (=fx *client-key* 0)
	    (set! *client-key* (elong->fixnum (current-seconds)))
	    
	    (set! *port-service*
		  (service :url "server-event-info" ()
		     (vector (hostname) port (get-ajax-key port))))
	    
	    (set! *init-service*
		  (service :url "server-event-init" (key)
		     (with-lock *event-mutex*
			(lambda ()
			   (let ((req (current-request)))
			      ;; read the Flash's ending zero byte
			      (read-byte
			       (socket-input (http-request-socket req)))
			      (set! *flash-request-list*
				    (cons (cons (string->symbol key) req)
					  *flash-request-list*))
			      (instantiate::http-response-event
				 (request req)
				 (name key)))))))

	    (set! *unregister-service*
		  (service :url "server-event-unregister" (event key)
		     (server-event-unregister event key)))
	    
	    (set! *register-service*
		  (service :url "server-event-register" (event key flash)
		     (server-event-register event key flash)))))))

;*---------------------------------------------------------------------*/
;*    server-event-register ...                                        */
;*---------------------------------------------------------------------*/
(define (server-event-register event key flash)
   
   (define (ajax-register-event! req name key)
      (let ((conn (ajax-find-connection name key)))
	 (if conn
	     ;; we already have a connection...
	     (if (ajax-connection-empty? conn)
		 ;; with an empty buffer...
		 (with-access::ajax-connection conn ((r req) date)
		    (set! date (current-seconds))
		    (when (http-request? r)
		       ;; we already have a connection but this connection
		       ;; has probably timeouted, so we close it before
		       ;; using the new one
		       (socket-close (http-request-socket r)))
		    (set! r req)
		    (instantiate::http-response-persistent
		       (request req)))
		 ;; we already have a value
		 (scheme->response (ajax-connection-get! conn) req))
	     ;; we don't have connection yet
	     (let ((conn (instantiate::ajax-connection
			    (req req)
			    (key key))))
		(ajax-store-connection! name conn)
		;; adapt dynamically the buffer size
		(let ((sz (hashtable-size *ajax-connection-table*)))
		   (when (>fx sz (hop-event-buffer-size))
		      (hop-event-buffer-size-set! (*fx 2 sz))))
		(instantiate::http-response-persistent
		   (request req))))))
   
   (define (flash-register-event! req name)
      (hashtable-update! *flash-socket-table*
			 name
			 (lambda (l)
			    (cons req (filter-requests l)))
			 (list req))
      (instantiate::http-response-string))

   (tprint "server-event-register: " event " key=" key " flash=" flash)
   (with-lock *event-mutex*
      (lambda ()
	 (let ((req (current-request))
	       (key (string->symbol key)))
	    ;; set an output timeout on the socket
	    (output-timeout-set! (socket-output (http-request-socket req))
				 (hop-connection-timeout))
	    ;; register the client
	    (if flash
		(let ((req (cdr (assq key *flash-request-list*))))
		   (flash-register-event! req event))
		(ajax-register-event! req event key))))))

;*---------------------------------------------------------------------*/
;*    server-event-unregister ...                                      */
;*---------------------------------------------------------------------*/
(define (server-event-unregister event key)
   
   (define (unregister-ajax-event! event key)
      (let ((conn (ajax-find-connection event key)))
	 (when (ajax-connection? conn)
	    (with-access::ajax-connection conn (req)
	       (when (http-request? req)
		  (socket-close (http-request-socket req)))
	       (hashtable-update! *ajax-connection-table*
				  event
				  (lambda (l) (delete! conn l))
				  '())))))
   
   (define (unregister-flash-event! event key)
      (let ((c (assq (string->symbol key) *flash-request-list*)))
	 (when (pair? c)
	    (let ((req (cdr c)))
	       (hashtable-update! *flash-socket-table*
				  event
				  (lambda (l)
				     (let ((nl (filter-requests l)))
					(delete! req nl)))
				  '())))))
   
   (tprint "server-event-unregister: " event " key=" key)
   (with-lock *event-mutex*
      (lambda ()
	 (unregister-ajax-event! event key)
	 (unregister-flash-event! event key)
	 #f)))

;*---------------------------------------------------------------------*/
;*    hop-event-client-ready? ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-event-client-ready? name)
   
   (define (ajax-event-client-ready? name)
      (pair? (hashtable-get *ajax-connection-table* name)))

   (define (flash-event-client-ready? name)
      (let ((l (hashtable-get *flash-socket-table* name)))
	 (and (pair? l)
	      (any? (lambda (req)
		       (let ((s (http-request-socket req)))
			  (not (socket-down? s))))
		    l))))

   (or (ajax-event-client-ready? name)
       (flash-event-client-ready? name)))

;*---------------------------------------------------------------------*/
;*    *flash-socket-table* ...                                         */
;*---------------------------------------------------------------------*/
(define *flash-socket-table* (make-hashtable))
(define *flash-request-list* '())

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
	    (unless (&io-timeout-error? e)
	       (raise e)))
	 (begin
	    (http-response resp s)
	    (socket-close s)))))

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
       (xml->string value (hop-xml-backend)))
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
      (let ((conns (hashtable-get *ajax-connection-table* name)))
	 (when (pair? conns)
	    (let loop ((l conns))
	       (if (pair? l)
		   (with-access::ajax-connection (car l) (req)
		      (if (http-request? req)
			  (let ((val (scheme->response (list value) req)))
			     (ajax-signal-value req val)
			     (set! req #f))
			  (loop (cdr l))))
		   ;; there is no active ajax connection, fill the first
		   ;; non full one
		   (let ((l (filter! ajax-connection-available? conns)))
		      (when (pair? l)
			 (ajax-connection-put! (car l) value))
		      (hashtable-put! *ajax-connection-table* name l)))))))
   
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
      (let ((conns (hashtable-get *ajax-connection-table* name)))
	 (when (pair? conns)
	    (let ((val (scheme->response (list value) *default-request*)))
	       (for-each (lambda (conn)
			    (with-access::ajax-connection conn (req)
			       (when (http-request? req)
				  (tprint "ajax broadcast: " name)
				  (ajax-signal-value req val))))
			 conns)
	       (let ((l (filter! ajax-connection-available? conns)))
		  (when (pair? l)
		     (for-each (lambda (conn)
				  (with-access::ajax-connection conn (req)
				     (if (http-request? req)
					 (set! req #f)
					 (begin
					    (tprint "ajax store: " name)
					    (ajax-connection-put! conn value)))))
			       l))
		  (hashtable-put! *ajax-connection-table* name l))))))
   
   (define (flash-event-broadcast! name value)
      (for-each-socket
       *flash-socket-table*
       name
       (lambda (l)
	  (when (pair? l)
	     (let ((val (flash-make-signal-value value)))
		(for-each (lambda (req)
			     (tprint "flash broadcast: " name " "
				     (http-request-socket req))
			     (flash-signal-value req name val))
			  l))))))

   (mutex-lock! *event-mutex*)
   (ajax-event-broadcast! name value)
   (flash-event-broadcast! name value)
   (mutex-unlock! *event-mutex*)
   #unspecified)
