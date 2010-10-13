;*=====================================================================*/
;*    .../prgm/project/hop/2.2.x/arch/android/hopandroid/phone.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 12 12:30:23 2010                          */
;*    Last change :  Wed Oct 13 16:57:07 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Android Phone implementation                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopandroid-phone

   (library phone)

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
   
   (define (send-boolean b::bool op::output-port)
      (write-byte (if b 1 0) op))
   
   (define (send-char b::char op::output-port)
      (write-byte (char->integer b) op))
   
   (with-access::androidphone p (protocol %socket %mutex)
      (let ((op (socket-output %socket)))
	 (write-byte protocol op)
	 (write-char service op)
	 (write-char cmd op)
	 (when (pair? args)
	    (for-each (lambda (a)
			 (cond
			    ((string? a) (send-string a op))
			    ((integer? a) (send-int32 a op))
			    ((boolean? a) (send-boolean a op))
			    ((char? a) (send-char a op))))
		      args))
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

;*---------------------------------------------------------------------*/
;*    phone-vibrate ::androidphone ...                                 */
;*---------------------------------------------------------------------*/
(define-method (phone-vibrate p::androidphone duration::obj)
   (android-send-command p #\V #\b))
