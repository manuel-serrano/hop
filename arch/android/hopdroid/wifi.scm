;*=====================================================================*/
;*    .../prgm/project/hop/2.4.x/arch/android/hopdroid/wifi.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 17 07:08:41 2011                          */
;*    Last change :  Fri Jul  6 11:58:44 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
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
	      (androidwifi-init)
	      (%mutex::mutex (default (make-mutex)))
	      (phone::androidphone read-only)
	      (info read-only
		 (get (lambda (o::androidwifi)
			 (android-send-command/result o.phone wifi-plugin #\i)))))

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
   (with-access::androidwifi w (%mutex phone)
      (with-lock %mutex
	 (lambda ()
	    (android-send-command phone wifi-plugin #\m)))))

;*---------------------------------------------------------------------*/
;*    androidwifi-multicast-lock-release ...                           */
;*---------------------------------------------------------------------*/
(define (androidwifi-multicast-lock-release w::androidwifi)
   (with-access::androidwifi w (%mutex phone)
      (with-lock %mutex
	 (lambda ()
	    (android-send-command phone wifi-plugin #\r)))))

;*---------------------------------------------------------------------*/
;*    androidwifi-multicast-lock-held? ...                             */
;*---------------------------------------------------------------------*/
(define (androidwifi-multicast-lock-held? w::androidwifi)
   (with-access::androidwifi w (%mutex phone)
      (with-lock %mutex
	 (lambda ()
	    (android-send-command/result phone wifi-plugin #\s)))))
