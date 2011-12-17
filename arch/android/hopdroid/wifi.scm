;*=====================================================================*/
;*    .../prgm/project/hop/2.3.x/arch/android/hopdroid/wifi.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 17 07:08:41 2011                          */
;*    Last change :  Sat Dec 17 07:14:02 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Wifi control                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-wifi

   (library phone hop)

   (import __hopdroid-phone)

   (export (class androidwifi
	      (androidtts-init)
	      (%mutex::mutex (default (make-mutex)))
	      (phone::androidphone read-only))

	   (androidwifi-init ::androidwifi)))

;*---------------------------------------------------------------------*/
;*    Standard plugins                                                 */
;*---------------------------------------------------------------------*/
(define wifi-plugin #f)

;*---------------------------------------------------------------------*/
;*    androidwifi-init ::androidwifi ...                               */
;*---------------------------------------------------------------------*/
(define (androidwifi-init t)
   (with-access::androidwifi t (phone)
      (unless wifi-plugin
	 (set! wifi-plugin (android-load-plugin phone "wifi")))))

;*---------------------------------------------------------------------*/
;*    androidwifi-multicast-lock-acquire ...                           */
;*---------------------------------------------------------------------*/
(define (androidwifi-multicast-lock-acquire w::androidwifi)
   (with-access::androidtts t (%mutex phone)
      (with-lock %mutex
	 (lambda ()
	    (android-send-command phone tts-plugin #\m)))))

;*---------------------------------------------------------------------*/
;*    androidwifi-multicast-lock-release ...                           */
;*---------------------------------------------------------------------*/
(define (androidwifi-multicast-lock-release w::androidwifi)
   (with-access::androidtts t (%mutex phone)
      (with-lock %mutex
	 (lambda ()
	    (android-send-command phone tts-plugin #\r)))))

;*---------------------------------------------------------------------*/
;*    androidwifi-multicast-lock-held? ...                             */
;*---------------------------------------------------------------------*/
(define (androidwifi-multicast-lock-held? w::androidwifi)
   (with-access::androidtts t (%mutex phone)
      (with-lock %mutex
	 (lambda ()
	    (android-send-command/result phone tts-plugin #\s)))))
