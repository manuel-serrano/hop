;*=====================================================================*/
;*    .../prgm/project/hop/2.2.x/arch/android/hopdroid/phone.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 12 12:30:23 2010                          */
;*    Last change :  Mon Oct 25 19:17:18 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Android Phone implementation                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-phone

   (library phone pthread hop)

   (export (class androidphone::phone
	      (host::bstring read-only (default "localhost"))
	      (port1::int read-only (default 8081))
	      (port2::int read-only (default 8082))
	      (protocol::byte read-only (default 1))
	      (%socket1 (default #unspecified))
	      (%socket2 (default #unspecified))
	      (%evthread (default #unspecified))
	      (%evtable (default #unspecified))
	      (%mutex::mutex read-only (default (make-mutex))))

	   (class androidevent::event)

	   (android-load-plugin::int ::androidphone ::bstring)
	   (android-send-command ::androidphone ::int . args)
	   (android-send-command/result ::androidphone ::int . args)))

;*---------------------------------------------------------------------*/
;*    android-plugin-mutex ...                                         */
;*---------------------------------------------------------------------*/
(define android-plugin-mutex (make-mutex "android-plugin"))

;*---------------------------------------------------------------------*/
;*    Standard plugins                                                 */
;*---------------------------------------------------------------------*/
(define vibrate-plugin #f)
(define sensor-plugin #f)
(define sms-plugin #f)

;*---------------------------------------------------------------------*/
;*    phone-init ::androidphone ...                                    */
;*---------------------------------------------------------------------*/
(define-method (phone-init p::androidphone)
   (with-access::androidphone p (host port1 %socket1)
      (set! %socket1 (make-client-socket host port1)))
   (unless vibrate-plugin
      (set! vibrate-plugin (android-load-plugin p "vibrate")))
   (unless sensor-plugin
      (set! sensor-plugin (android-load-plugin p "sensor"))))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::androidphone ...                           */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! p::androidphone event proc . capture)
   (with-access::androidphone p (host %mutex port2 %socket2 %evthread %evtable)
      (with-lock %mutex
	 (lambda ()
	    (unless (hashtable? %evtable)
	       (set! %evtable (make-hashtable 8)))
	    (unless (socket? %socket2)
	       (set! %socket2 (make-client-socket host port2)))
	    (unless (thread? %evthread)
	       (set! %evthread
		     (thread-start!
		      (instantiate::pthread
			 (body (lambda () (android-event-listener p)))))))
	    (hashtable-update! %evtable event cons (list proc))
	    (let ((op (socket-output %socket2)))
	       (send-string event op)
	       (send-byte 1 op)
	       (flush-output-port op))))))

;*---------------------------------------------------------------------*/
;*    remove-event-listener! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (remove-event-listener! p::androidphone event proc . capture)
   (with-access::androidphone p (host %mutex port2 %socket2 %evthread %evtable)
      (with-lock %mutex
	 (lambda ()
	    (when (hashtable? %evtable)
	       (hashtable-update! %evtable event
				  (lambda (l) (remq! proc l)) '()))
	    (when (socket? %socket2)
	       (let ((op (socket-output %socket2)))
		  (send-string event op)
		  (send-byte 0 op)
		  (flush-output-port op)))))))

;*---------------------------------------------------------------------*/
;*    android-event-listener ...                                       */
;*---------------------------------------------------------------------*/
(define (android-event-listener p::androidphone)
   (with-access::androidphone p (%mutex %socket2 %evtable %evthread)
      (let ((ip (socket-input %socket2)))
	 (let loop ()
	    (let ((name (read ip)))
	       (unless (eof-object? name)
		  (let ((args (read ip)))
		     (unless (eof-object? args)
			(let ((procs (with-lock %mutex
					(lambda ()
					   (hashtable-get %evtable name))))
			      (event (instantiate::androidevent
					(name name)
					(target p)
					(value args))))
			   (let liip ((procs procs))
			      (when (pair? procs)
				 ((car procs) event)
				 (unless (event-stopped? event)
				    (liip (cdr procs)))))))))
	       (loop)))
	 (with-lock %mutex
	    (lambda ()
	       (socket-close %socket2)
	       (set! %evtable #unspecified)
	       (set! %evthread #unspecified))))))

;*---------------------------------------------------------------------*/
;*    android-load-plugin ...                                          */
;*---------------------------------------------------------------------*/
(define (android-load-plugin p::androidphone name)
   (with-lock android-plugin-mutex
      (lambda ()
	 (hop-verb 2 "Loading android plugin \"" name "\"...\n")
	 (let ((n (android-send-command/result p 0 name)))
	    (if (>=fx n 0)
		n
		(error "android-load-plugin"
		       (case n
			  ((-1) "Plugin not found")
			  ((-2) "Class not found")
			  ((-3) "Constructor not found")
			  ((-4) "Security exception")
			  ((-5) "Cannot create instance")
			  ((-6) "Illegal access")
			  ((-7) "Illegal argument")
			  ((-8) "Invocation target")
			  (else "Cannot load plugin"))
		       name))))))

;*---------------------------------------------------------------------*/
;*    phone-vibrate ::androidphone ...                                 */
;*---------------------------------------------------------------------*/
(define-method (phone-vibrate p::androidphone vibration::obj repeat)
   (cond
      ((vector? vibration)
       (android-send-command p vibrate-plugin #\p vibration repeat))
      ((integer? vibration)
       (android-send-command p vibrate-plugin #\b vibration))
      (else
       (android-send-command p vibrate-plugin #\b 2))))

;*---------------------------------------------------------------------*/
;*    phone-vibrate-stop ::androidphone ...                            */
;*---------------------------------------------------------------------*/
(define-method (phone-vibrate-stop p::androidphone)
   (android-send-command p vibrate-plugin #\e))

;*---------------------------------------------------------------------*/
;*    phone-sensor-list ::androidphone ...                             */
;*---------------------------------------------------------------------*/
(define-method (phone-sensor-list p::androidphone)
   (android-send-command/result p sensor-plugin #\i))

;*---------------------------------------------------------------------*/
;*    phone-sensor ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (phone-sensor p::androidphone type . delay)
   (let ((t (case type
	       ((orientation) 0)
	       ((light) 1)
	       ((magnetic-field) 2)
	       ((proximity) 3)
	       ((temperature) 4)
	       ((accelerometer) 5)
	       ((pressure) 6)
	       (else (error "sensor" "unknown sensor type" type)))))
      (android-send-command/result p sensor-plugin #\b t
				   (phone-sensor-ttl p)
				   (if (pair? delay) (car delay) 0))))

;*---------------------------------------------------------------------*/
;*    phone-sms-send ::androidphone ...                                */
;*---------------------------------------------------------------------*/
(define-method (phone-sms-send p::androidphone no::bstring msg::bstring)
   (unless sms-plugin
      (set! sms-plugin (android-load-plugin p "sms")))
   (android-send-command p sms-plugin #\s no msg))

;*---------------------------------------------------------------------*/
;*    send-string ...                                                  */
;*---------------------------------------------------------------------*/
(define (send-string s::bstring op::output-port)
   (send-int32 (string-length s) op)
   (display-string s op))

;*---------------------------------------------------------------------*/
;*    send-int32 ...                                                   */
;*---------------------------------------------------------------------*/
(define (send-int32 i::int op::output-port)
   (write-byte (bit-and (bit-rsh i 24) #xff) op)
   (write-byte (bit-and (bit-rsh i 16) #xff) op)
   (write-byte (bit-and (bit-rsh i 8) #xff) op)
   (write-byte (bit-and i #xff) op))

;*---------------------------------------------------------------------*/
;*    send-int64 ...                                                   */
;*---------------------------------------------------------------------*/
(define (send-int64 i::llong op::output-port)
   (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 54) #lxff)) op)
   (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 48) #lxff)) op)
   (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 40) #lxff)) op)
   (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 32) #lxff)) op)
   (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 24) #lxff)) op)
   (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 16) #lxff)) op)
   (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 8) #lxff)) op)
   (write-byte (llong->fixnum (bit-andllong i #lxff)) op))

;*---------------------------------------------------------------------*/
;*    send-boolean ...                                                 */
;*---------------------------------------------------------------------*/
(define (send-boolean b::bool op::output-port)
   (write-byte (if b 1 0) op))

;*---------------------------------------------------------------------*/
;*    send-char ...                                                    */
;*---------------------------------------------------------------------*/
(define (send-char b::char op::output-port)
   (send-byte (char->integer b) op))

;*---------------------------------------------------------------------*/
;*    send-byte ...                                                    */
;*---------------------------------------------------------------------*/
(define (send-byte b::byte op::output-port)
   (write-byte b op))

;*---------------------------------------------------------------------*/
;*    send-vector ...                                                  */
;*---------------------------------------------------------------------*/
(define (send-vector v::vector op::output-port)
   (let ((l (vector-length v)))
      (send-int32 l op)
      (let loop ((i 0))
	 (when (<fx i l)
	    (send-obj (vector-ref v i) op)
	    (loop (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    send-obj ...                                                     */
;*---------------------------------------------------------------------*/
(define (send-obj o::obj op::output-port)
      (cond
	 ((string? o) (send-string o op))
	 ((llong? o) (send-int64 o op))
	 ((integer? o) (send-int32 o op))
	 ((boolean? o) (send-boolean o op))
	 ((char? o) (send-char o op))
	 ((vector? o) (send-vector o op))))

;*---------------------------------------------------------------------*/
;*    android-send ...                                                 */
;*---------------------------------------------------------------------*/
(define (android-send p::androidphone plugin::int args)
   (with-access::androidphone p (protocol %socket1 %mutex)
      (let ((op (socket-output %socket1)))
	 (write-byte protocol op)
	 (send-int32 plugin op)
	 (for-each (lambda (o) (send-obj o op)) args)
	 (flush-output-port op))))

;*---------------------------------------------------------------------*/
;*    android-send-command ...                                         */
;*---------------------------------------------------------------------*/
(define (android-send-command p::androidphone plugin::int . args)
   (with-access::androidphone p (%mutex)
      (with-lock %mutex
	 (lambda ()
	    (android-send p plugin args)))))

;*---------------------------------------------------------------------*/
;*    android-send-command/result ...                                  */
;*---------------------------------------------------------------------*/
(define (android-send-command/result p::androidphone plugin::int . args)
   (with-access::androidphone p (%mutex %socket1)
      (with-lock %mutex
	 (lambda ()
	    (android-send p plugin args)
	    (let ((ip (socket-input %socket1)))
	       (read ip))))))


