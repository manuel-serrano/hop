;*=====================================================================*/
;*    .../prgm/project/hop/2.4.x/arch/android/hopdroid/system.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 22 16:55:45 2012                          */
;*    Last change :  Mon Nov 26 15:07:38 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    System configuration                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-system
   
   (library multimedia phone hop)

   (import __hopdroid-phone)

   (export (system-wifi-sleep-policy::symbol ::androidphone)
	   (system-wifi-sleep-policy-set! ::androidphone ::symbol)

	   (system-accelerometer-rotation::bool ::androidphone)
	   (system-accelerometer-rotation-set! ::androidphone ::bool)))

;*---------------------------------------------------------------------*/
;*    system plugin                                                    */
;*---------------------------------------------------------------------*/
(define system-plugin #f)

;*---------------------------------------------------------------------*/
;*    system-wifi-sleep-policy ...                                     */
;*---------------------------------------------------------------------*/
(define (system-wifi-sleep-policy p::androidphone)
   (unless system-plugin
      (set! system-plugin (android-load-plugin p "system")))
   (android-send-command/result p system-plugin #\w))

;*---------------------------------------------------------------------*/
;*    system-wifi-sleep-policy-set! ...                                */
;*---------------------------------------------------------------------*/
(define (system-wifi-sleep-policy-set! p::androidphone policy)
   (unless system-plugin
      (set! system-plugin (android-load-plugin p "system")))
   (unless (android-send-command/result p system-plugin #\W policy)
      (error "system-wifi-sleep-policy" "Cannot set policy" policy)))

;*---------------------------------------------------------------------*/
;*    system-accelerometer-rotation ...                                */
;*---------------------------------------------------------------------*/
(define (system-accelerometer-rotation p::androidphone)
   (unless system-plugin
      (set! system-plugin (android-load-plugin p "system")))
   (android-send-command/result p system-plugin #\r))

;*---------------------------------------------------------------------*/
;*    system-accelerometer-rotation-set! ...                           */
;*---------------------------------------------------------------------*/
(define (system-accelerometer-rotation-set! p::androidphone v)
   (unless system-plugin
      (set! system-plugin (android-load-plugin p "system")))
   (android-send-command p system-plugin #\R v)
   v)

   
