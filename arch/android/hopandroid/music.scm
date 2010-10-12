;*=====================================================================*/
;*    .../prgm/project/hop/2.2.x/arch/android/hopandroid/music.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 12 12:31:01 2010                          */
;*    Last change :  Tue Oct 12 18:57:13 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Android music implementation                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopandroid-music

   (library multimedia phone)

   (import __hopandroid-phone)

   (export (class androidmusic::music
	      (phone::androidphone read-only))))

;*---------------------------------------------------------------------*/
;*    music-play ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-play m::androidmusic . song)
   (with-access::androidphone (androidmusic-phone m) (protocol %socket)
      (let ((op (socket-output %socket)))
	 (write-byte protocol op)
	 (write-char #\M op)
	 (flush-output-port op))))
