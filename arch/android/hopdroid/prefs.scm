;*=====================================================================*/
;*    .../prgm/project/hop/3.1.x/arch/android/hopdroid/prefs.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 17 09:31:15 2016                          */
;*    Last change :  Sun Jul 17 14:25:11 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Android preferences plugin                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-prefs

   (import __hopdroid-phone)

   (export (preferences-set! ::androidphone ::symbol ::obj)
	   (preferences-get ::androidphone ::symbol)))

;*---------------------------------------------------------------------*/
;*    Standard plugins                                                 */
;*---------------------------------------------------------------------*/
(define prefs-plugin #f)

;*---------------------------------------------------------------------*/
;*    preferences-init ...                                             */
;*---------------------------------------------------------------------*/
(define (preferences-init phone::androidphone)
   (unless prefs-plugin
      (set! prefs-plugin (android-load-plugin phone "prefs"))))

;*---------------------------------------------------------------------*/
;*    preferences-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (preferences-set! phone::androidphone key val)
   (preferences-init phone)
   (android-send-command p prefs-plugin #\s (symbol->string! key) val))

;*---------------------------------------------------------------------*/
;*    preferences-get ...                                              */
;*---------------------------------------------------------------------*/
(define (preferences-get phone::androidphone key)
   (preferences-init phone)
   (android-send-command/result p prefs-plugin #\g (symbol->string! key)))



