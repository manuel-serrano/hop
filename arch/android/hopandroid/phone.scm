;*=====================================================================*/
;*    .../prgm/project/hop/2.2.x/arch/android/hopandroid/phone.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 12 12:30:23 2010                          */
;*    Last change :  Tue Oct 12 14:40:45 2010 (serrano)                */
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
	      (%socket (default #unspecified)))))

;*---------------------------------------------------------------------*/
;*    phone-init ::androidphone ...                                    */
;*---------------------------------------------------------------------*/
(define-method (phone-init p::androidphone)
   (with-access::androidphone p (port host %socket)
      (set! %socket (make-client-socket host port))))

;*---------------------------------------------------------------------*/
;*    phone-vibrate ::androidphone ...                                 */
;*---------------------------------------------------------------------*/
(define-method (phone-vibrate p::androidphone duration::obj)
   (with-access::androidphone p (protocol %socket)
      (let ((op (socket-output %socket)))
	 (write-byte protocol op)
	 (write-char #\V op)
	 (flush-output-port op))))
   
