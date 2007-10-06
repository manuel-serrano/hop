;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/event.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 27 05:45:08 2005                          */
;*    Last change :  Sat Oct  6 11:27:39 2007 (serrano)                */
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
(define *unregister-service* #f)
(define *init-service* #f)
(define *client-key* 0)
(define *default-request* (instantiate::http-request))

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
;*    server-event-unregister ...                                      */
;*---------------------------------------------------------------------*/
(define (server-event-unregister event key)
   
   (define (unregister-ajax-event! event key)
      
      (define (unregister-ajax-active-event!)
	 (hashtable-update! *ajax-active-table*
			    event
			    (lambda (l)
			       (let ((p (assq key l)))
				  (when (http-request? (cdr p))
				     (socket-close
				      (http-request-socket (cdr p))))
				  (delete! p l)))
			    '()))
      
      (define (unregister-ajax-waiting-event!)
	 (hashtable-update! *ajax-wait-table*
			    event
			    (lambda (l)
			       (let ((p (assq key l)))
				  (delete! p l)))
			    '()))
      
      (unregister-ajax-active-event!)
      (unregister-ajax-waiting-event!))
   
   (define (unregister-flash-event! event key)
      (let ((c (assq (string->symbol key) *flash-request-list*)))
	 (when (pair? c)
	    (socket-close (http-request-socket (cdr c))))))
   
   (with-lock *event-mutex*
      (lambda ()
	 (unregister-ajax-event! event key)
	 (unregister-flash-event! event key)
	 #f)))

;*---------------------------------------------------------------------*/
;*    server-event-register ...                                        */
;*---------------------------------------------------------------------*/
(define (server-event-register event key flash)
   
   (define (ajax-register-event! req name key)

      (define (ajax-register-active!)
	 ;; set an output timeout on the socket
	 (output-timeout-set! (socket-output (http-request-socket req))
			      (hop-connection-timeout))
	 (hashtable-update! *ajax-active-table*
			    name
			    (lambda (l)
			       ;; create a new entry only if key is unbound
			       ;; in the active table
			       (let ((p (assq key l)))
				  (if (pair? p)
				      (begin
					 (set-cdr! p req)
					 l)
				      (cons (cons key req) l))))
			    (list (cons key req)))
	 (instantiate::http-response-persistent
	    (request req)))

      (let ((waiting (hashtable-get *ajax-wait-table* name)))
	 (if (pair? waiting)
	     (let ((sigs (assq key waiting)))
		(if (and (pair? sigs) (pair? (cdr sigs)))
		    (let ((val (cadr sigs)))
		       ;; remove the entry from the waiting table and send it
		       (set-cdr! sigs (cddr sigs))
		       val)
		    (ajax-register-active!)))
	     (ajax-register-active!))))
   
   (define (flash-register-event! req name key)
      ;; set an output timeout on the socket
      (output-timeout-set! (socket-output (http-request-socket req))
			   (hop-connection-timeout))
      ;; update the event table
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
	    (if flash
		(let ((req (cdr (assq key *flash-request-list*))))
		   (flash-register-event! req event key))
		(ajax-register-event! req event key))))))

;*---------------------------------------------------------------------*/
;*    hop-event-client-ready? ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-event-client-ready? name)
   
   (define (ajax-event-client-ready? name)
      
      (define (ajax-event-active-ready? name)
	 (let ((wl (hashtable-get *ajax-active-table* name)))
	    (and (pair? wl)
		 (any? (lambda (a)
			  (let ((req (cdr a)))
			     (when (http-request? req)
				(let ((sock (http-request-socket req)))
				   (not (socket-down? sock))))))
		       wl))))
      
      (define (ajax-event-waiting-ready? name)
	 ;; waiting events are ready if it exists a key for which a value
	 ;; is waiting
	 (let ((wl (hashtable-get *ajax-wait-table* name)))
	    (and (pair? wl)
		 (any? (lambda (a)
			  (pair? (cdr a)))
		       wl))))
      
      (or (ajax-event-active-ready? name)
	  (ajax-event-waiting-ready? name)))
	     

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
;*    *ajax-wait-table* ...                                            */
;*    -------------------------------------------------------------    */
;*    The *ajax-wait-table* and *ajax-active-table* hashtables are     */
;*    indexed by event names. The values in the tables are pairs       */
;*    <key x (val0, val1, ...)>. Unless is event is unregistered, once */
;*    the pair is the table it is never removed (in order to avoid     */
;*    consing).                                                        */
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

   (define (find-active al)
      (when (pair? al)
	 (let* ((a (car al))
		(key (car a))
		(req (cdr a)))
	    (if (http-request? req)
		a
		(find-active (cdr al))))))

   (define (ajax-event-signal! name value)
      (let* ((a (find-active (hashtable-get *ajax-active-table* name)))
	     (req (if (pair? a) (cdr a) *default-request*))
	     (val (ajax-make-signal-value value req)))
	 (if (pair? a)
	     ;; we have active connections
	     (let ((key (car a))
		   (req (cdr a)))
		;; we close the active connection
		(set-cdr! a #f)
		;; we have to create an entry in the waitlist for that
		;; client in order to not loose the event that might be
		;; emitted before the client has re-registered.
		(hashtable-update! *ajax-wait-table*
				   name
				   (lambda (l)
				      (let ((p (assq key l)))
					 (if (pair? p)
					     l
					     (cons (cons key '()) l))))
				   (list (cons key '())))
		;; we signal the value
		(ajax-signal-value req val))
	     ;; we don't have active connections
	     (let ((wl (hashtable-get *ajax-wait-table* name)))
		;; update the wait list
		(when (pair? wl)
		   (let ((p (car wl)))
		      (if (pair? (cdr p))
			  (set-cdr! p (append! (cdr p) (list val)))
			  (set-cdr! p (list val)))))))))
   
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
	     (req *default-request*)
	     (val (ajax-make-signal-value value req)))
	 ;; we update the wait list in two stages and we signal
	 ;;  1- we add entry entries for the currently active connections
	 ;;  2- we register values for waiting connections that are not
	 ;;     currently active.
         ;;  3- we signal the active connections
	 (when (pair? al)
	    (for-each (lambda (a)
			 (let ((key (car a))
			       (req (cdr a)))
			    (hashtable-update! *ajax-wait-table*
					       name
					       (lambda (l)
						  (let ((p (assq key l)))
						     (if (pair? p)
							 l
							 (cons (cons key '())
							       l))))
					       (list (cons key '())))))
		      al))
	 ;; update the wait list
	 (when (pair? wl)
	    (for-each (lambda (w)
			 (let ((k (car w)))
			    (let ((c (assq k al)))
			       (unless (and (pair? c) (http-request? (cdr c)))
				  (if (pair? (cdr w))
				      (set-cdr! w (append! (cdr w) (list val)))
				      (set-cdr! w (list val)))))))
		      wl))
	 ;; signal and close the active connections the active connections
	 (when (pair? al)
	    (for-each (lambda (a)
			 (let ((key (car a))
			       (req (cdr a)))
			    (when (http-request? req)
			       (set-cdr! a #f)
			       ;; signal the value
			       (tprint "ajax signal (for broadd): " name)
			       (ajax-signal-value req val))))
		      al))))
   
   (define (flash-event-broadcast! name value)
      (for-each-socket
       *flash-socket-table*
       name
       (lambda (l)
	  (tprint "flash signal (for broadc): " name " l=" l)
	  (when (pair? l)
	     (let ((val (flash-make-signal-value value)))
		(for-each (lambda (req)
			     (flash-signal-value req name val))
			  l))))))

   (tprint "hop-event-broadcast!: " name " " value)
   (mutex-lock! *event-mutex*)
   (ajax-event-broadcast! name value)
   (flash-event-broadcast! name value)
   (mutex-unlock! *event-mutex*)
   #unspecified)
