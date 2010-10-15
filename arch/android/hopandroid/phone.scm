;*=====================================================================*/
;*    .../prgm/project/hop/2.2.x/arch/android/hopandroid/phone.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 12 12:30:23 2010                          */
;*    Last change :  Fri Oct 15 08:32:25 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Android Phone implementation                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopandroid-phone

   (library phone pthread)

   (export (class androidphone::phone
	      (host::bstring read-only (default "localhost"))
	      (port::int read-only (default 8081))
	      (protocol::byte read-only (default 1))
	      (%socket (default #unspecified))
	      (%mutex::mutex read-only (default (make-mutex))))
	   
	   (android-send-command ::androidphone ::char ::char . args)
	   (android-send-command/result ::androidphone ::char ::char . args)))

;*---------------------------------------------------------------------*/
;*    phone-init ::androidphone ...                                    */
;*---------------------------------------------------------------------*/
(define-method (phone-init p::androidphone)
   (with-access::androidphone p (port host %socket)
      (set! %socket (make-client-socket host port))))

;*---------------------------------------------------------------------*/
;*    phone-vibrate ::androidphone ...                                 */
;*---------------------------------------------------------------------*/
(define-method (phone-vibrate p::androidphone vibration::obj repeat)
   (cond
      ((vector? vibration)
       (android-send-command p #\V #\p vibration repeat))
      ((integer? vibration)
       (android-send-command p #\V #\b vibration))
      (else
       (android-send-command p #\V #\b 2))))

;*---------------------------------------------------------------------*/
;*    phone-vibrate-stop ::androidphone ...                            */
;*---------------------------------------------------------------------*/
(define-method (phone-vibrate-stop p::androidphone)
      (android-send-command p #\V #\e))

;*---------------------------------------------------------------------*/
;*    phone-sensor-list ::androidphone ...                             */
;*---------------------------------------------------------------------*/
(define-method (phone-sensor-list p::androidphone)
   (android-send-command/result p #\S #\b i))

;*---------------------------------------------------------------------*/
;*    phone-sensor ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (phone-sensor p::androidphone type . delay)
   (let ((t (case type
	       ((accelerometer) 0)
	       ((orientation) 0)
	       ((light) 1)
	       ((magnetic-field) 2)
	       ((proximity) 3)
	       ((temperature) 4)
	       ((tricorder) 5)
	       (else (error "sensor" "unknown sensor type" type)))))
      (android-send-command/result p #\S #\b t
				   (phone-sensor-ttl p)
				   (if (pair? delay) (car delay) 0))))

;*---------------------------------------------------------------------*/
;*    send ...                                                         */
;*---------------------------------------------------------------------*/
(define (send p::androidphone service::char cmd::char . args)
   
   (define (send-string s::bstring op::output-port)
      (send-int32 (string-length s) op)
      (display-string s op))
   
   (define (send-int32 i::int op::output-port)
      (write-byte (bit-and (bit-rsh i 24) #xff) op)
      (write-byte (bit-and (bit-rsh i 16) #xff) op)
      (write-byte (bit-and (bit-rsh i 8) #xff) op)
      (write-byte (bit-and i #xff) op))
   
   (define (send-int64 i::llong op::output-port)
      (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 54) #lxff)) op)
      (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 48) #lxff)) op)
      (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 40) #lxff)) op)
      (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 32) #lxff)) op)
      (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 24) #lxff)) op)
      (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 16) #lxff)) op)
      (write-byte (llong->fixnum (bit-andllong (bit-rshllong i 8) #lxff)) op)
      (write-byte (llong->fixnum (bit-andllong i #lxff)) op))
   
   (define (send-boolean b::bool op::output-port)
      (write-byte (if b 1 0) op))
   
   (define (send-char b::char op::output-port)
      (write-byte (char->integer b) op))
   
   (define (send-vector v::vector op::output-port)
      (let ((l (vector-length v)))
	 (send-int32 l op)
	 (let loop ((i 0))
	    (when (<fx i l)
	       (send (vector-ref v i) op)
	       (loop (+fx i 1))))))

   (define (send o::obj op::output-port)
      (cond
	 ((string? o) (send-string o op))
	 ((llong? o) (send-int64 o op))
	 ((integer? o) (send-int32 o op))
	 ((boolean? o) (send-boolean o op))
	 ((char? o) (send-char o op))
	 ((vector? o) (send-vector o op))))

   
   (with-access::androidphone p (protocol %socket %mutex)
      (let ((op (socket-output %socket)))
	 (write-byte protocol op)
	 (write-char service op)
	 (write-char cmd op)
	 (for-each (lambda (o) (send o op)) args)
	 (flush-output-port op))))

;*---------------------------------------------------------------------*/
;*    android-send-command ...                                         */
;*---------------------------------------------------------------------*/
(define (android-send-command p::androidphone service::char cmd::char . args)
   (with-access::androidphone p (%mutex)
      (with-lock %mutex
	 (lambda ()
	    (apply send p service cmd args)))))

;*---------------------------------------------------------------------*/
;*    android-send-command/result ...                                  */
;*---------------------------------------------------------------------*/
(define (android-send-command/result p::androidphone service::char cmd::char . args)
   (with-access::androidphone p (%mutex %socket)
      (with-lock %mutex
	 (lambda ()
	    (apply send p service cmd args)
	    (let ((ip (socket-input %socket)))
	       (read ip))))))

