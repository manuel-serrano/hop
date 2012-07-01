;*=====================================================================*/
;*    .../prgm/project/hop/2.4.x/arch/android/hopdroid/phone.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 12 12:30:23 2010                          */
;*    Last change :  Sun Jul  1 18:52:04 2012 (serrano)                */
;*    Copyright   :  2010-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Android Phone implementation                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-phone

   (library phone mail pthread hop)

   (import __hopdroid-tts)
   
   (export (class androidphone::phone
	      (host::bstring read-only (default "localhost"))
	      (sdk::int (default -1))
	      (port1::int read-only (default 8081))
	      (port2::int read-only (default 8082))
	      (protocol::byte read-only (default 1))
	      (%socket1 (default #unspecified))
	      (%socket2 (default #unspecified))
	      (%evthread (default #unspecified))
	      (%evtable (default #unspecified))
	      (%mutex::mutex read-only (default (make-mutex)))
	      (%mutex-listener::mutex read-only (default (make-mutex))))

	   (class androidevent::event)

	   (android)
	   
	   (android-load-plugin::int ::androidphone ::bstring)
	   (android-send-command ::androidphone ::int . args)
	   (android-send-command/result ::androidphone ::int . args))

   (cond-expand
      (bigloo3.5a
       (export (generic (phone-locales ::phone))
	       (generic (phone-current-locale ::phone))
	       (generic (phone-current-locale-set! ::phone ::obj))))))

;*---------------------------------------------------------------------*/
;*    backward compatibility                                           */
;*---------------------------------------------------------------------*/
(cond-expand
   (bigloo3.5a
    (define-generic (phone-current-locale ::phone))
    (define-generic (phone-current-locale-set! ::phone ::obj))
    (define-generic (phone-locales p::phone))))

;*---------------------------------------------------------------------*/
;*    Standard plugins                                                 */
;*---------------------------------------------------------------------*/
(define vibrate-plugin #f)
(define sensor-plugin #f)
(define sms-plugin #f)
(define contact-plugin #f)
(define call-plugin #f)
(define battery-plugin #f)
(define locale-plugin #f)
(define build-plugin #f)

;*---------------------------------------------------------------------*/
;*    *android* ...                                                    */
;*---------------------------------------------------------------------*/
(define *android* #f)
(define *android-mutex* (make-mutex))

;*---------------------------------------------------------------------*/
;*    android ...                                                      */
;*---------------------------------------------------------------------*/
(define (android)
   (mutex-lock! *android-mutex*)
   (unless (isa? *android* androidphone)
      (set! *android* (instantiate::androidphone)))
   (mutex-unlock! *android-mutex*)
   *android*)

;*---------------------------------------------------------------------*/
;*    phone-init ::androidphone ...                                    */
;*---------------------------------------------------------------------*/
(define-method (phone-init p::androidphone)
   (with-access::androidphone p (host port1 %socket1 sdk)
      (set! %socket1 (make-client-socket host port1))
      (set! build-plugin (android-load-plugin p "build"))
      (set! sdk (android-send-command/result p build-plugin #\v))))

;*---------------------------------------------------------------------*/
;*    phone-locales ::androidphone ...                                 */
;*---------------------------------------------------------------------*/
(define-method (phone-locales p::androidphone)
   (unless locale-plugin
      (set! locale-plugin (android-load-plugin p "locale")))
   (android-send-command/result p locale-plugin #\l))

;*---------------------------------------------------------------------*/
;*    phone-current-locale ::androidphone ...                          */
;*---------------------------------------------------------------------*/
(define-method (phone-current-locale p::androidphone)
   (unless locale-plugin
      (set! locale-plugin (android-load-plugin p "locale")))
   (android-send-command/result p locale-plugin #\c))

;*---------------------------------------------------------------------*/
;*    phone-current-locale ::androidphone ...                          */
;*---------------------------------------------------------------------*/
(define-method (phone-current-locale-set! p::androidphone l)
   (unless locale-plugin
      (set! locale-plugin (android-load-plugin p "locale")))
   (if (and (list? l) (every? string? l))
       (android-send-command p locale-plugin #\s l)
       (error "phone-current-locale-set!" "Illegal locale" l)))

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::androidphone ...                           */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! p::androidphone event proc . capture)
   (with-access::androidphone p (host %mutex-listener port2 %socket2
				      %evthread %evtable)
      (with-lock %mutex-listener
	 (lambda ()
	    (unless (hashtable? %evtable)
	       (set! %evtable (make-hashtable 8)))
	    (unless (socket? %socket2)
	       (set! %socket2 (make-client-socket host port2)))
	    (unless (isa? %evthread thread)
	       (set! %evthread
		     (thread-start!
		      (instantiate::pthread
			 (body (lambda () (android-event-listener p)))))))
	    (hashtable-update! %evtable event
			       (lambda (v) (cons proc v))
			       (list proc))
	    (let ((op (socket-output %socket2)))
	       (send-string event op)
	       (send-byte 1 op)
	       (flush-output-port op))
	    (cond
	       ((string=? event "battery")
		(register-battery-listener! p))
	       ((string=? event "tts")
		(register-tts-listener! p))
	       ((string=? event "call")
		(register-call-listener! p))
	       ((string=? event "orientation")
		(register-orientation-listener! p)))))))

;*---------------------------------------------------------------------*/
;*    remove-event-listener! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (remove-event-listener! p::androidphone event proc . capture)
   (with-access::androidphone p (%mutex-listener %socket2 %evthread %evtable)
      (with-lock %mutex-listener
	 (lambda ()
	    (when (hashtable? %evtable)
	       (hashtable-update! %evtable event
				  (lambda (l)
				     (cond
					((string=? event "battery")
					 (remove-battery-listener! p))
					((string=? event "tts")
					 (remove-tts-listener! p))
					((string=? event "call")
					 (remove-call-listener! p))
					((string=? event "orientation")
					 (remove-call-listener! p)))
				     (remq! proc l))
				  '()))
	    (when (socket? %socket2)
	       (let ((op (socket-output %socket2)))
		  (send-string event op)
		  (send-byte 0 op)
		  (flush-output-port op)))))))

;*---------------------------------------------------------------------*/
;*    register-battery-listener! ...                                   */
;*---------------------------------------------------------------------*/
(define (register-battery-listener! p::androidphone)
   (unless battery-plugin
      (set! battery-plugin (android-load-plugin p "battery")))
   (android-send-command p battery-plugin #\b))

;*---------------------------------------------------------------------*/
;*    remove-battery-listener! ...                                     */
;*---------------------------------------------------------------------*/
(define (remove-battery-listener! p::androidphone)
   (android-send-command p battery-plugin #\e))

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
				 (with-access::androidevent event (stopped)
				    (unless stopped
				       (liip (cdr procs))))))))))
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
   (hop-verb 2 "Loading android plugin \"" name "\"...\n")
   (let ((n (android-send-command/result p 0 name)))
      (if (and (fixnum? n) (>=fx n 0))
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
		 name))))

;*---------------------------------------------------------------------*/
;*    phone-vibrate ::androidphone ...                                 */
;*---------------------------------------------------------------------*/
(define-method (phone-vibrate p::androidphone vibration::obj repeat)
   (unless vibrate-plugin
      (set! vibrate-plugin (android-load-plugin p "vibrate")))
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
   (unless vibrate-plugin
      (set! vibrate-plugin (android-load-plugin p "vibrate")))
   (android-send-command p vibrate-plugin #\e))

;*---------------------------------------------------------------------*/
;*    phone-sensor-list ::androidphone ...                             */
;*---------------------------------------------------------------------*/
(define-method (phone-sensor-list p::androidphone)
   (unless sensor-plugin
      (set! sensor-plugin (android-load-plugin p "sensor")))
   (android-send-command/result p sensor-plugin #\i))

;*---------------------------------------------------------------------*/
;*    sensor-type-number ...                                           */
;*---------------------------------------------------------------------*/
(define (sensor-type-number type)
   (case type
      ((orientation) 0)
      ((light) 1)
      ((magnetic-field) 2)
      ((proximity) 3)
      ((temperature) 4)
      ((accelerometer) 5)
      ((pressure) 6)
      (else (error "sensor" "unknown sensor type" type))))

;*---------------------------------------------------------------------*/
;*    phone-sensor ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (phone-sensor p::androidphone type . delay)
   (unless sensor-plugin
      (set! sensor-plugin (android-load-plugin p "sensor")))
   (android-send-command/result p sensor-plugin #\b
      (sensor-type-number type)
      (with-access::androidphone p (sensor-ttl)
	 sensor-ttl)
      (if (pair? delay) (car delay) 0)))

;*---------------------------------------------------------------------*/
;*    register-orientation-listener! ...                               */
;*---------------------------------------------------------------------*/
(define (register-orientation-listener! p::androidphone)
   (android-send-command p sensor-plugin #\a (sensor-type-number 'orientation)))

;*---------------------------------------------------------------------*/
;*    remove-orientation-listener! ...                                 */
;*---------------------------------------------------------------------*/
(define (remove-orientation-listener! p::androidphone)
   (android-send-command p sensor-plugin #\r (sensor-type-number 'orientation)))

;*---------------------------------------------------------------------*/
;*    phone-sms-send ::androidphone ...                                */
;*---------------------------------------------------------------------*/
(define-method (phone-sms-send p::androidphone no::bstring msg::bstring)
   (unless sms-plugin
      (set! sms-plugin (android-load-plugin p "sms")))
   (android-send-command p sms-plugin #\s no msg))

;*---------------------------------------------------------------------*/
;*    phone-contact ::androidphone ...                                 */
;*---------------------------------------------------------------------*/
(define-method (phone-contact p::androidphone)
   (unless contact-plugin
      (set! contact-plugin (android-load-plugin p "contact")))
   (if contact-plugin
       (map! (lambda (e)
		(match-case e
		   ((?first ?family ?akas ?comp ?phones ?addrs ?emails ?notes ?-)
		    (let* ((face (let ((c (assq 'face notes)))
				    (when (pair? c)
				       (set! notes (remq! c notes))
				       (cdr c))))
			   (url (let ((c (assq 'url notes)))
				   (when (pair? c)
				      (set! notes (remq! c notes))
				      (cdr c))))
			   (notes (if (pair? akas)
				      (cons (cons 'akas akas) notes)
				      notes))
			   (fn (if (string? first)
				   (if (string? family)
				       (string-append first " " family)
				       first)
				   (if (string? family)
				       family
				       ""))))
		       (instantiate::vcard
			  (fn fn)
			  (familyname family)
			  (firstname first)
			  (org comp)
			  (face face)
			  (url url)
			  (phones phones)
			  (addresses addrs)
			  (emails emails)
			  (notes notes))))))
	  (android-send-command/result p contact-plugin #\l))
       '()))

;*---------------------------------------------------------------------*/
;*    phone-contact-add! ::androidphone ...                            */
;*---------------------------------------------------------------------*/
(define-method (phone-contact-add! p::androidphone vcard::obj)
   (unless contact-plugin
      (set! contact-plugin (android-load-plugin p "contact")))
   (when contact-plugin
      (android-send-command/result p contact-plugin #\a vcard)))

;*---------------------------------------------------------------------*/
;*    phone-contact-remove! ::androidphone ...                         */
;*---------------------------------------------------------------------*/
(define-method (phone-contact-remove! p::androidphone id::obj)
   (unless contact-plugin
      (set! contact-plugin (android-load-plugin p "contact")))
   (when contact-plugin
      (android-send-command/result p contact-plugin #\r id)))

;*---------------------------------------------------------------------*/
;*    phone-call-log ::androidphone ...                                */
;*---------------------------------------------------------------------*/
(define-method (phone-call-log p::androidphone . optional)
   (unless call-plugin
      (set! call-plugin (android-load-plugin p "call")))
   (let ((n (if (pair? optional) (car optional) -1)))
      (android-send-command/result p call-plugin #\l n)))

;*---------------------------------------------------------------------*/
;*    phone-call-info ::androidphone ...                               */
;*---------------------------------------------------------------------*/
(define-method (phone-call-info p::androidphone)
   (unless call-plugin
      (set! call-plugin (android-load-plugin p "call")))
      (android-send-command/result p call-plugin #\i))

;*---------------------------------------------------------------------*/
;*    register-call-listener! ...                                      */
;*---------------------------------------------------------------------*/
(define (register-call-listener! p::androidphone)
   (unless call-plugin
      (set! call-plugin (android-load-plugin p "call")))
   (android-send-command p call-plugin #\b))

;*---------------------------------------------------------------------*/
;*    remove-call-listener! ...                                        */
;*---------------------------------------------------------------------*/
(define (remove-call-listener! p::androidphone)
   (unless call-plugin
      (set! call-plugin (android-load-plugin p "call")))
   (android-send-command p call-plugin #\e))

;*---------------------------------------------------------------------*/
;*    phone-call-start ::androidphone ...                              */
;*---------------------------------------------------------------------*/
(define-method (phone-call-start p::androidphone n::bstring . optional)
   (unless call-plugin
      (set! call-plugin (android-load-plugin p "call")))
   (let ((window (if (pair? optional) (car optional) #f)))
      (android-send-command p call-plugin #\c n window)))

;*---------------------------------------------------------------------*/
;*    phone-call-stop ::androidphone ...                               */
;*---------------------------------------------------------------------*/
(define-method (phone-call-stop p::androidphone)
   (when call-plugin
      (android-send-command p call-plugin #\k)))

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
(define-generic (send-obj o::obj op::output-port)
   (cond
      ((string? o)
       (send-string o op))
      ((llong? o)
       (send-int64 o op))
      ((fixnum? o)
       (send-int32 o op))
      ((flonum? o)
       (send-string (real->string o) op))
      ((boolean? o)
       (send-boolean o op))
      ((char? o)
       (send-char o op))
      ((vector? o)
       (send-vector o op))
      ((and (pair? o) (or (null? (cdr o)) (pair? (cdr o))))
       (send-obj (list->vector o) op))
      ((pair? o)
       (send-obj (car o) op) (send-obj (cdr o) op))
      ((symbol? o)
       (send-string (symbol->string! o) op))
      (else
       (error "send-obj" "cannot serialize value" o))))

;*---------------------------------------------------------------------*/
;*    send-obj ::vcard ...                                             */
;*---------------------------------------------------------------------*/
(define-method (send-obj o::vcard op)
   (with-access::vcard o (firstname
			  familyname org url phones
			  addresses emails notes)
      (tprint "ADDING first=" firstname " fam=" familyname " org=" org
	      " url=" url " emails=" emails " phones=" phones)
      (send-string (or firstname "") op)
      (send-string (or familyname "") op)
      (send-string (or org "") op)
      (send-string (or url "") op)
      (send-obj emails op)))
;*       (send-string phones op)                                       */
;*       (send-string addresses op)                                    */
;*       (send-string notes op)))                                      */

;*---------------------------------------------------------------------*/
;*    android-send ...                                                 */
;*---------------------------------------------------------------------*/
(define (android-send p::androidphone plugin::int args)
   (with-access::androidphone p (protocol %socket1 %mutex)
      (let ((op (socket-output %socket1)))
	 (write-byte protocol op)
	 (send-int32 plugin op)
	 (for-each (lambda (o) (send-obj o op)) args)
	 (send-char #a127 op)
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
