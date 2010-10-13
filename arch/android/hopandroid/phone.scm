;*=====================================================================*/
;*    .../prgm/project/hop/2.2.x/arch/android/hopandroid/phone.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 12 12:30:23 2010                          */
;*    Last change :  Wed Oct 13 08:41:30 2010 (serrano)                */
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

	   (android-send-command ::androidphone ::char ::char . args)))

;*---------------------------------------------------------------------*/
;*    phone-init ::androidphone ...                                    */
;*---------------------------------------------------------------------*/
(define-method (phone-init p::androidphone)
   (with-access::androidphone p (port host %socket)
      (set! %socket (make-client-socket host port))))

;*---------------------------------------------------------------------*/
;*    android-send-command ...                                         */
;*---------------------------------------------------------------------*/
(define (android-send-command p::androidphone service::char cmd::char . args)
   
   (define (send-string op s)
      (send-int32 op (string-length s))
      (write-string s op))

   (define (send-int32 op i)
      (write-byte (bit-and (bit-rsh i 24) #xff))
      (write-byte (bit-and (bit-rsh i 16) #xff))
      (write-byte (bit-and (bit-rsh i 8) #xff))
      (write-byte (bit-and i #xff)))

   (define (send-boolean op b)
      (write-byte (if b 1 0) op))

   (define (send-char op b)
      (write-byte (char->integer b) op))

   (with-access::androidphone p (protocol %socket %mutex)
      (with-lock %mutex
	 (lambda ()
	    (let ((op (socket-output %socket)))
	       (write-byte protocol op)
	       (write-char service op)
	       (write-char cmd op)
	       (when (pair? args)
		  (for-each (lambda (a)
			       (cond
				  ((string? a) (send-string op a))
				  ((integer? a) (send-int32 op a))
				  ((boolean? a) (send-boolean op a))
				  ((char? a) (send-char op a))))
			    args))
	       (flush-output-port op))))))

;*---------------------------------------------------------------------*/
;*    phone-vibrate ::androidphone ...                                 */
;*---------------------------------------------------------------------*/
(define-method (phone-vibrate p::androidphone duration::obj)
   (android-send-command p #\V #\b))
   
