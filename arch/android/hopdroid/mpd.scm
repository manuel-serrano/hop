;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/arch/android/hopdroid/mpd.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 11 08:16:32 2011                          */
;*    Last change :  Wed May 11 08:45:22 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Android MPD implementation                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-mpd

   (library multimedia phone hop)

   (import  __hopdroid-phone)

   (export  (class androidmpd-database::mpd-database
	       (phone::androidphone read-only))))

;*---------------------------------------------------------------------*/
;*    android mpd plugin                                               */
;*---------------------------------------------------------------------*/
(define mpd-plugin #f)

;*---------------------------------------------------------------------*/
;*    mpd-database-init! ::androidmpd-database ...                     */
;*---------------------------------------------------------------------*/
(define-method (mpd-database-init! o::androidmpd-database)
   (with-access::androidmpd-database o (phone)
      (unless mpd-plugin
	 (set! mpd-plugin (android-load-plugin phone "mpd-database")))))

;*---------------------------------------------------------------------*/
;*    mpd-database-listgenre ::androidmpd-database ...                 */
;*---------------------------------------------------------------------*/
(define-method (mpd-database-listgenre o::androidmpd-database op)
   (with-access::androidmpd-database o (phone %genre)
      (set! %genres (android-send-command/result phone mpd-plugin #\G))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    mpd-database-listartist ::androidmpd-database ...                */
;*---------------------------------------------------------------------*/
(define-method (mpd-database-listartist o::androidmpd-database op)
   (with-access::androidmpd-database o (phone %artist)
      (set! %artists (android-send-command/result phone mpd-plugin #\A))
      (call-next-method)))

      
   
