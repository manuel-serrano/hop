;*=====================================================================*/
;*    .../prgm/project/hop/2.4.x/arch/android/hopdroid/system.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 22 16:55:45 2012                          */
;*    Last change :  Thu Nov 22 17:05:27 2012 (serrano)                */
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
	   (system-wifi-sleep-policy-set! ::androidphone ::symbol)))

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
;*    system-wifi-sleep-policy ...                                     */
;*---------------------------------------------------------------------*/
(define (system-wifi-sleep-policy p::androidphone policy)
   (unless system-plugin
      (set! system-plugin (android-load-plugin p "system")))
   (unless (android-send-command/result p system-plugin #\W policy)
      (error "system-wifi-sleep-policy" "Cannot set policy" policiy)))
