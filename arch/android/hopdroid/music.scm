;*=====================================================================*/
;*    .../prgm/project/hop/2.2.x/arch/android/hopdroid/music.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 12 12:31:01 2010                          */
;*    Last change :  Mon Dec 20 18:00:23 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Android music implementation                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-music
   
   (library multimedia phone hop)
   
   (import __hopdroid-phone)
   
   (export (class androidmusic::music
	      (phone::androidphone read-only)

	      (%open::bool (default #t))
	      (%playlist::pair-nil (default '()))
	      (%meta::pair-nil (default '()))
	      (%tag::obj (default '())))))

;*---------------------------------------------------------------------*/
;*    Standard plugins                                                 */
;*---------------------------------------------------------------------*/
(define music-plugin #f)

;*---------------------------------------------------------------------*/
;*    music-init ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-init o::androidmusic)
   (unless music-plugin
      (set! music-plugin
	    (android-load-plugin (androidmusic-phone o) "musicplayer")))
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
	       (android-send-command phone music-plugin #\x)
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
		     (android-send-command phone music-plugin #\u uri)
		     (with-access::musicstatus %status (state)
			(set! state 'play)))))))))

;*---------------------------------------------------------------------*/
;*    music-seek ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-seek o::androidmusic pos . song)
   (with-access::android o (%mutex phone)
      (with-lock %mutex
	 (lambda ()
	    (when (pair? song)
	       (if (not (integer? (car song)))
		   (bigloo-type-error "|music-seek ::androidmusic"
				      'int (car song))
		   (set-song! o (car song))))
	    (android-send-command phone music-plugin #\k pos)))))

;*---------------------------------------------------------------------*/
;*    music-stop ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-stop o::androidmusic)
   (with-access::androidmusic o (phone %status)
      (android-send-command phone music-plugin #\e)
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
		      (android-send-command phone music-plugin #\b))
		   (begin
		      (set! state 'pause)
		      (android-send-command phone music-plugin #\p))))))))

;*---------------------------------------------------------------------*/
;*    music-update-status! ::androidmusic ...                          */
;*---------------------------------------------------------------------*/
(define-method (music-update-status! o::androidmusic status::musicstatus)
   (with-access::androidmusic o (%mutex phone %status)
      (let ((s (android-send-command/result phone music-plugin #\S)))
	 (when (pair? s)
	    (musicstatus-state-set! status (car s))
	    (musicstatus-songlength-set! status (cadr s))
	    (musicstatus-songpos-set! status (caddr s))))
      status))

;*---------------------------------------------------------------------*/
;*    music-status ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (music-status o::androidmusic)
   (let ((status (music-%status o)))
      (music-update-status! o status)
      status))

;*---------------------------------------------------------------------*/
;*    music-song ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-song o::androidmusic)
   (with-access::androidmusic o (%mutex %status)
      (with-lock %mutex
	 (lambda ()
	    (musicstatus-song %status)))))

;*---------------------------------------------------------------------*/
;*    music-songpos ::androidmusic ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-songpos o::androidmusic)
   ;; this function assumes that %pipeline is still valid (i.e., the
   ;; gstmm music player has not been closed yet)
   (with-access::androidmusic o (%status)
      (music-update-status! o %status)
      (musicstatus-songpos %status)))

;*---------------------------------------------------------------------*/
;*    music-meta ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (music-meta o::androidmusic)
   (with-access::androidmusic o (%meta)
      %meta))

;*---------------------------------------------------------------------*/
;*    music-volume-get ::androidmusic ...                              */
;*---------------------------------------------------------------------*/
(define-method (music-volume-get o::androidmusic)
   (with-access::androidmusic o (%status)
      (musicstatus-volume %status)))

;*---------------------------------------------------------------------*/
;*    music-volume-set! ::androidmusic ...                             */
;*---------------------------------------------------------------------*/
(define-method (music-volume-set! o::androidmusic vol)
   (with-access::androidmusic o (%status phone)
      (musicstatus-volume-set! %status vol)
      (android-send-command phone music-plugin #\v vol vol)))
