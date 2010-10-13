;*=====================================================================*/
;*    .../prgm/project/hop/2.2.x/arch/android/hopandroid/music.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 12 12:31:01 2010                          */
;*    Last change :  Wed Oct 13 08:54:16 2010 (serrano)                */
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
	      (phone::androidphone read-only)

	      (%open::bool (default #t))
	      (%playlist::pair-nil (default '()))
	      (%meta::pair-nil (default '()))
	      (%tag::obj (default '())))))

;*---------------------------------------------------------------------*/
;*    music-init ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-init o::androidmusic)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    music-close ::androidmusic ...                                   */
;*---------------------------------------------------------------------*/
(define-method (music-close o::androidmusic)
   (with-access::androidmusic o (phone %mutex %open %playlist %meta %tag)
      (with-lock %mutex
	 (lambda ()
	    (when %open
	       (set! %open #t)
	       (set! %meta '())
	       (set! %tag #unspecified)
	       (set! %playlist '())
	       (android-send-command phone #\M #\x)
	       (call-next-method))))))

;*---------------------------------------------------------------------*/
;*    music-closed? ::androidmusic ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-closed? o::androidmusic)
   (with-access::androidmusic o (%open)
      (not %open)))

;*---------------------------------------------------------------------*/
;*    music-reset! ::androidmusic ...                                  */
;*---------------------------------------------------------------------*/
(define-method (music-reset! o::androidmusic)
   (music-close o))

;*---------------------------------------------------------------------*/
;*    music-playlist-get ::androidmusic ...                            */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-get o::androidmusic)
   (with-access::androidmusic o (%playlist)
      %playlist))

;*---------------------------------------------------------------------*/
;*    music-playlist-add! ::androidmusic ...                           */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-add! androidmusic::androidmusic n)
   (call-next-method)
   (with-access::androidmusic androidmusic (%mutex %playlist %status)
      (with-lock %mutex
	 (lambda ()
	    (set! %playlist (append %playlist (list n)))
	    (with-access::musicstatus %status (playlistid playlistlength)
	       (set! playlistid (+fx 1 playlistid))
	       (set! playlistlength (+fx 1 playlistlength)))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::androidmusic ...                        */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! androidmusic::androidmusic n)
   (with-access::androidmusic androidmusic (%mutex %playlist %status)
      (with-lock %mutex
	 (lambda ()
	    (set! %playlist (delete! n %playlist string=?))
	    (with-access::musicstatus %status (playlistid playlistlength)
	       (when (and (>=fx n 0) (<fx n playlistlength))
		  (set! %playlist (remq! (list-ref %playlist n) %playlist))
		  (set! playlistid (+fx 1 playlistid))
		  (set! playlistlength (length %playlist))))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-clear! ::androidmusic ...                         */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-clear! androidmusic::androidmusic)
   (with-access::androidmusic androidmusic (%mutex %playlist %status)
      (with-lock %mutex
	 (lambda ()
	    (set! %playlist '())
	    (with-access::musicstatus %status (playlistlength song songid)
	       (set! song 0)
	       (set! songid 0)
	       (set! playlistlength 0))))))

;*---------------------------------------------------------------------*/
;*    set-song! ...                                                    */
;*---------------------------------------------------------------------*/
(define (set-song! o i)
   (with-access::androidmusic o (%status %playlist)
      (cond
	 ((<fx i 0)
	  (raise
	   (instantiate::&io-error
	      (proc "set-song!")
	      (msg (format "No such song: ~a" i))
	      (obj %playlist))))
	 ((>=fx i (length %playlist))
	  #f)
	 (else
	  (let ((m (list-ref %playlist i)))
	     (musicstatus-song-set! %status i)
	     m)))))

;*---------------------------------------------------------------------*/
;*    charset-convert ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (charset-convert str)
   (case (os-charset)
      ((UTF-8)
       str)
      ((UCS-2)
       `(utf8-string->ucs2-string ,str))
      ((CP-1252)
       `(utf8->cp1252 ,str))
      (else
       `(utf8->iso-latin ,str))))

;*---------------------------------------------------------------------*/
;*    music-play ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-play o::androidmusic . song)
   (with-access::androidmusic o (%mutex %open %status phone)
      (with-lock %mutex
	 (lambda ()
	    (unless %open
	       (error "music-play ::androidmusic"
		      "Player closed (or badly initialized)"
		      o))
	    (let ((url (if (pair? song)
			   (if (not (integer? (car song)))
			       (bigloo-type-error
				"music-play ::androidmusic"
				'int
				(car song))
			       (set-song! o (car song)))
			   (set-song! o (musicstatus-song %status)))))
	       (when (string? url)
		  (let ((uri (charset-convert url)))
		     (android-send-command phone #\M #\u uri)
		     (with-access::musicstatus %status (state)
			(set! state 'play)))))))))

;*---------------------------------------------------------------------*/
;*    music-stop ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-stop o::androidmusic)
   (with-access::androidmusic o (phone %status)
      (android-send-command phone #\M #\e)
      (with-access::musicstatus %status (state)
	 (set! state 'stop))))

;*---------------------------------------------------------------------*/
;*    music-pause ::androidmusic ...                                   */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::androidmusic)
   (with-access::androidmusic o (%mutex %status phone)
      (with-lock %mutex
	 (lambda ()
	    (with-access::musicstatus %status (state)
	       (if (eq? state 'pause)
		   (begin
		      (set! state 'play)
		      (android-send-command phone #\M #\p))
		   (begin
		      (set! state 'pause)
		      (android-send-command phone #\M #\b))))))))

;*---------------------------------------------------------------------*/
;*    music-update-status! ::androidmusic ...                              */
;*---------------------------------------------------------------------*/
;* (define-method (music-update-status! o::androidmusic status::musicstatus) */
;*    (with-access::androidmusic o (%mutex %pipeline)                  */
;*       (with-access::musicstatus status (state songpos songlength volume) */
;* 	 (mutex-lock! %mutex)                                          */
;* 	 (if (gst-element? %pipeline)                                  */
;* 	     (begin                                                    */
;* 		(set! songpos (music-position o))                      */
;* 		(set! songlength (music-duration o))                   */
;* 		(set! volume (music-volume-get o)))                    */
;* 	     (musicstatus-state-set! status 'stop))                    */
;* 	 (mutex-unlock! %mutex)                                        */
;* 	 status)))                                                     */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    music-status ...                                                 *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (music-status o::androidmusic)                       */
;*    (let ((status (music-%status o)))                                */
;*       (music-update-status! o status)                               */
;*       status))                                                      */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    music-song ::androidmusic ...                                        *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (music-song o::androidmusic)                         */
;*    (with-access::androidmusic o (%mutex %status)                    */
;*       (with-lock %mutex                                             */
;* 	 (lambda ()                                                    */
;* 	    (musicstatus-song %status)))))                             */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    music-songpos ::androidmusic ...                                     *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (music-songpos o::androidmusic)                      */
;*    ;; this function assumes that %pipeline is still valid (i.e., the */
;*    ;; gstmm music player has not been closed yet)                   */
;*    (llong->fixnum                                                   */
;*     (/llong (gst-element-query-position (androidmusic-%pipeline o)) */
;* 	    #l1000000000)))                                            */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    music-meta ...                                                   *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (music-meta o::androidmusic)                         */
;*    (with-access::androidmusic o (%meta)                             */
;*       %meta))                                                       */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    music-volume-get ::androidmusic ...                                  *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (music-volume-get o::androidmusic)                   */
;*    (with-access::androidmusic o (%status %audiomixer)               */
;*       (if (gst-element? %audiomixer)                                */
;* 	  (let ((vol (inexact->exact                                   */
;* 		      (* 100 (gst-object-property %audiomixer :volume))))) */
;* 	     (musicstatus-volume-set! %status vol)                     */
;* 	     vol)                                                      */
;* 	  0)))                                                         */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    music-volume-set! ::androidmusic ...                                 *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (music-volume-set! o::androidmusic vol)              */
;*    (with-access::androidmusic o (%status %audiomixer)               */
;*       (when (gst-element? %audiomixer)                              */
;* 	 (gst-object-property-set! %audiomixer :volume (/ vol 100))    */
;* 	 (musicstatus-volume-set! %status vol))))                      */
