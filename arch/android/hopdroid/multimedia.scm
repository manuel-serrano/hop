;*=====================================================================*/
;*    .../project/hop/2.2.x/arch/android/hopdroid/multimedia.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 14 09:54:12 2010                          */
;*    Last change :  Mon Oct 25 09:17:29 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Access and control multimedia resources of an Android phone      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-multimedia
   
   (library multimedia phone hop)
   
   (import __hopdroid-phone)

   (export (audio-record-close phone)
	   (audio-record-start phone path #!optional (quality 0))
	   (audio-record-stop phone)))

;*---------------------------------------------------------------------*/
;*    audio-record-close ...                                           */
;*---------------------------------------------------------------------*/
(define (audio-record-close phone)
   '(android-send-command phone #\R #\x))
   
;*---------------------------------------------------------------------*/
;*    audio-record-start ...                                           */
;*---------------------------------------------------------------------*/
(define (audio-record-start phone path #!optional (quality 0))
   '(android-send-command phone #\R #\b quality))
   
;*---------------------------------------------------------------------*/
;*    audio-record-stop ...                                            */
;*---------------------------------------------------------------------*/
(define (audio-record-stop phone)
   '(android-send-command phone #\R #\e))
   
