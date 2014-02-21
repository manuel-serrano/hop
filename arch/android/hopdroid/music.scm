;*=====================================================================*/
;*    .../prgm/project/hop/2.6.x/arch/android/hopdroid/music.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 12 12:31:01 2010                          */
;*    Last change :  Fri Feb 21 13:25:02 2014 (serrano)                */
;*    Copyright   :  2010-14 Manuel Serrano                            */
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
	      (%playlistlength::int (default 0))
	      (%playlistid::int (default 0))
	      (%meta::pair-nil (default '()))
	      (%tag::obj (default '())))))

;*---------------------------------------------------------------------*/
;*    Standard plugins                                                 */
;*---------------------------------------------------------------------*/
(define music-plugin #f)

;*---------------------------------------------------------------------*/
;*    musics ...                                                       */
;*---------------------------------------------------------------------*/
(define musics '())

;*---------------------------------------------------------------------*/
;*    music-init ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-init o::androidmusic)
   
   (define (onstate e)
      (with-access::androidevent e (value)
	 (for-each (lambda (o)
		      (with-access::androidmusic o (onstate %status %mutex
						      %playlistid)
			 (with-access::musicstatus %status (state playlistid
							      playlistlength
							      song songpos)
			    (set! state value)
			    (onstate o %status)
			    (if (synchronize %mutex
				   (and (eq? state 'ended)
					(=fx playlistid %playlistid)
					(<fx song (-fx playlistlength 1))))
				(playlist-load! o (+fx song 1) #f)
				(set! songpos 0)))))
	    musics)))

   (define (onerror e)
      (with-access::androidevent e (value)
	 (for-each (lambda (o)
		      (with-access::androidmusic o (onerror %status)
			 (with-access::musicstatus %status (state)
			    (set! state 'error)
			    (onerror o value))))
	    musics)))

   (define (onevent e)
      (with-access::androidevent e (value)
	 (for-each (lambda (o)
		      (with-access::androidmusic o (onstate %status)
			 (with-access::musicstatus %status (songlength songpos)
			    (case (car value)
			       ((position)
				(set! songpos (/fx (cadr value) 1000))
				(set! songlength (/fx (caddr value) 1000))
				(onstate o %status))))))
	    musics)))

   (define (onvolume e)
      (with-access::androidevent e (value)
	 (for-each (lambda (o)
		      (with-access::androidmusic o (onvolume %status)
			 (with-access::musicstatus %status (volume)
			    (set! volume value)
			    (onvolume o value))))
	    musics)))
   
   (with-access::androidmusic o (phone)
      (unless music-plugin
	 (set! music-plugin (android-load-plugin phone "musicplayer"))
	 (add-event-listener! phone "androidmusic-state" onstate)
	 (add-event-listener! phone "androidmusic-error" onerror)
	 (add-event-listener! phone "androidmusic-event" onevent)
	 (add-event-listener! phone "androidmusic-volume" onvolume)))

   (set! musics (cons o musics))
   (call-next-method)
   (with-access::androidmusic o (%status)
      (with-access::musicstatus %status (volume)
	 (set! volume 100))))

;*---------------------------------------------------------------------*/
;*    music-close ::androidmusic ...                                   */
;*---------------------------------------------------------------------*/
(define-method (music-close o::androidmusic)
   (set! musics (delete! o musics))
   (with-access::androidmusic o (phone %mutex %open %playlist %playlistlength %meta %tag)
      (synchronize %mutex
	 (when %open
	    (set! %open #t)
	    (set! %meta '())
	    (set! %tag #unspecified)
	    (set! %playlist '())
	    (set! %playlistlength 0)
	    (android-send-command phone music-plugin #\x)
	    (call-next-method)))))

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
   (with-access::androidmusic androidmusic (%mutex %playlist %playlistlength %status)
      (synchronize %mutex
	 (set! %playlist (append %playlist (list n)))
	 (set! %playlistlength (+fx %playlistlength 1))
	 (with-access::musicstatus %status (playlistid playlistlength)
	    (set! playlistid (+fx 1 playlistid))
	    (set! playlistlength %playlistlength)))))

;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::androidmusic ...                        */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! androidmusic::androidmusic n)
   (with-access::androidmusic androidmusic (%mutex %playlist %playlistlength %status)
      (synchronize %mutex
	 (set! %playlist (delete! n %playlist string=?))
	 (set! %playlistlength (-fx %playlistlength 1))
	 (with-access::musicstatus %status (playlistid playlistlength)
	    (when (and (>=fx n 0) (<fx n playlistlength))
	       (set! %playlist (remq! (list-ref %playlist n) %playlist))
	       (set! playlistid (+fx 1 playlistid))
	       (set! playlistlength %playlistlength))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-clear! ::androidmusic ...                         */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-clear! androidmusic::androidmusic)
   (with-access::androidmusic androidmusic (%mutex %playlist %playlistlength %status)
      (synchronize %mutex
	 (set! %playlist '())
	 (set! %playlistlength 0)
	 (with-access::musicstatus %status (playlistlength song songid)
	    (set! song 0)
	    (set! songid 0)
	    (set! playlistlength 0)))))

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
	     (with-access::musicstatus %status (song)
		(set! song i))
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
;*    playlist-load! ...                                               */
;*---------------------------------------------------------------------*/
(define (playlist-load! o i pid)
   
   (define (playlist-next-url)
      (with-access::androidmusic o (%open %status %playlist %playlistid)
	 (with-access::musicstatus %status (song songid songpos songlength playlistlength state playlistid)
	    (set! state 'init)
	    (set! %playlistid playlistid)
	    (if (or (<fx i 0) (>=fx i playlistlength))
		(raise
		   (instantiate::&io-error
		      (proc 'playlist-load!)
		      (msg (format "No such song: ~a" i))
		      (obj %playlist)))
		(let ((playlist %playlist))
		   (set! song i)
		   (set! songid i)
		   (set! songpos 0)
		   (set! songlength 0)
		   (charset-convert (list-ref playlist i)))))))
   
   (with-access::androidmusic o (onevent %mutex phone)
      (let ((url (synchronize %mutex (playlist-next-url))))
	 (when pid (onevent o 'playlist pid))
	 (android-send-command phone music-plugin #\u url))))

;*---------------------------------------------------------------------*/
;*    music-play ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-play o::androidmusic . s)
   (with-access::androidmusic o (%mutex %open %status phone)
      (with-access::musicstatus %status (song state playlistlength playlistid)
	 (cond
	    ((not %open)
	     (error "music-play ::androidmusic"
		"Player closed (or badly initialized)"
		o))
	    ((pair? s)
	     (unless (integer? (car s))
		(bigloo-type-error '|music-play ::androidmusic| 'int (car s)))
	     (playlist-load! o (car s) playlistid))
	    ((eq? state 'pause)
	     (android-send-command phone music-plugin #\b))
	    ((and (>=fx song 0) (<fx song playlistlength))
	     (playlist-load! o song playlistid))))))

;*---------------------------------------------------------------------*/
;*    music-seek ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-seek o::androidmusic pos . song)
   (with-access::androidmusic o (%mutex phone)
      (synchronize %mutex
	 (when (pair? song)
	    (if (not (integer? (car song)))
		(bigloo-type-error "|music-seek ::androidmusic"
		   'int (car song))
		(set-song! o (car song))))
	 (android-send-command phone music-plugin #\k pos))))

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
      (synchronize %mutex
	 (with-access::musicstatus %status (state)
	    (if (eq? state 'pause)
		(begin
		   (set! state 'play)
		   (android-send-command phone music-plugin #\b))
		(begin
		   (set! state 'pause)
		   (android-send-command phone music-plugin #\p)))))))

;*---------------------------------------------------------------------*/
;*    music-song ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-song o::androidmusic)
   (with-access::androidmusic o (%mutex %status)
      (synchronize %mutex
	 (with-access::musicstatus %status (song)
	    song))))

;*---------------------------------------------------------------------*/
;*    music-songpos ::androidmusic ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-songpos o::androidmusic)
   ;; this function assumes that %pipeline is still valid (i.e., the
   ;; gstmm music player has not been closed yet)
   (with-access::androidmusic o (%status)
      (with-access::musicstatus %status (songpos)
	 songpos)))

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
      (with-access::musicstatus %status (volume)
	 volume)))

;*---------------------------------------------------------------------*/
;*    music-volume-set! ::androidmusic ...                             */
;*---------------------------------------------------------------------*/
(define-method (music-volume-set! o::androidmusic vol)
   (with-access::androidmusic o (%status phone)
      (with-access::musicstatus %status (volume)
	 (set! volume vol))
      (android-send-command phone music-plugin #\v vol vol)))

;*---------------------------------------------------------------------*/
;*    music-can-play-type? ::music ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-can-play-type? m::androidmusic mimetype::bstring)
   ;; If some knows a method to do this programmatically, let him speaks!
   ;; For know, I don't know any better method than using the static
   ;; official Google table available at:
   ;;   http://developer.android.com/guide/appendix/media-formats.html
   (cond
      ((member mimetype '("audio/mpeg" "audio/ogg" "audio/aac"))
       ;; all android
       #t)
      ((member mimetype '("audio/flac" "application/x-flac" "audio/x-flac"))
       ;; android >= 3.1
       (with-access::androidmusic m (phone)
	  (with-access::androidphone phone (sdk)
	     (>= (string-natural-compare3 sdk "3.1") 0))))
      ((member mimetype '("audio/wav" "audio/x-wav" "audio/x-pn-windows-acm"))
       ;; android >= 4.1
       (with-access::androidmusic m (phone)
	  (with-access::androidphone phone (sdk)
	     (>= (string-natural-compare3 sdk "4.1") 0))))
      ((member mimetype '("audio/mp4"))
       ;; android >= 4.1
       (with-access::androidmusic m (phone)
	  (with-access::androidphone phone (sdk)
	     (>= (string-natural-compare3 sdk "4.1") 0))))
      (else
       #f)))
