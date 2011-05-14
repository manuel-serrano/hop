;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/arch/android/hopdroid/mpd.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 11 08:16:32 2011                          */
;*    Last change :  Sat May 14 14:43:30 2011 (serrano)                */
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
	 (set! mpd-plugin (android-load-plugin phone "mediaaudio")))))

;*---------------------------------------------------------------------*/
;*    mpd-database-getgenre ...                                        */
;*---------------------------------------------------------------------*/
(define-method (mpd-database-getgenre::pair-nil o::androidmpd-database)
   (with-access::androidmpd-database o (phone %genres)
      (set! %genres
	 (map list (android-send-command/result phone mpd-plugin #\G)))
      %genres))
   
;*---------------------------------------------------------------------*/
;*    mpd-database-getartist ...                                       */
;*---------------------------------------------------------------------*/
(define-method (mpd-database-getartist::pair-nil o::androidmpd-database)
   (with-access::androidmpd-database o (phone %artists)
      (set! %artists
	 (map list (android-send-command/result phone mpd-plugin #\A)))
      %artists))
   
;*---------------------------------------------------------------------*/
;*    mpd-database-getgenreartist ...                                  */
;*---------------------------------------------------------------------*/
(define-method (mpd-database-getgenreartist o::androidmpd-database genre)
   (with-access::androidmpd-database o (phone)
      (android-send-command/result phone mpd-plugin #\g genre)))

;*---------------------------------------------------------------------*/
;*    mpd-database-getartistalbum ...                                  */
;*---------------------------------------------------------------------*/
(define-method (mpd-database-getartistalbum o::androidmpd-database artist)
   (with-access::androidmpd-database o (phone)
      (android-send-command/result phone mpd-plugin #\d artist)))

;*---------------------------------------------------------------------*/
;*    mpd-database-get-album ::androidmpd-database ...                 */
;*---------------------------------------------------------------------*/
(define-method (mpd-database-get-album o::androidmpd-database album)
   (with-access::androidmpd-database o (phone %albums)
      (android-send-command/result phone mpd-plugin #\a album)))
   
