;*=====================================================================*/
;*    .../prgm/project/hop/2.3.x/arch/android/hopdroid/music.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 12 12:31:01 2010                          */
;*    Last change :  Thu Jan 26 15:56:43 2012 (serrano)                */
;*    Copyright   :  2010-12 Manuel Serrano                            */
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
			    (mutex-lock! %mutex)
			    (if (and (eq? state 'ended)
				     (=fx playlistid %playlistid)
				     (<fx song (-fx playlistlength 1)))
				(playlist-load! o (+fx song 1) #f)
				(begin
				   (set! songpos 0)
				   (mutex-unlock! %mutex))))))
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
   
   (with-access::androidmusic o (phone %status)
      (unless music-plugin
	 (set! music-plugin (android-load-plugin phone "musicplayer"))
	 (add-event-listener! phone "androidmusic-state" onstate)
	 (add-event-listener! phone "androidmusic-error" onerror)
	 (add-event-listener! phone "androidmusic-event" onevent)))

   (set! musics (cons o musics))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    music-close ::androidmusic ...                                   */
;*---------------------------------------------------------------------*/
(define-method (music-close o::androidmusic)
   (set! musics (delete! o musics))
   (with-access::androidmusic o (phone %mutex %open %playlist %playlistlength %meta %tag)
      (with-lock %mutex
	 (lambda ()
	    (when %open
	       (set! %open #t)
	       (set! %meta '())
	       (set! %tag #unspecified)
	       (set! %playlist '())
	       (set! %playlistlength 0)
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
   (with-access::androidmusic androidmusic (%mutex %playlist %playlistlength %status)
      (with-lock %mutex
	 (lambda ()
	    (set! %playlist (append %playlist (list n)))
	    (set! %playlistlength (+fx %playlistlength 1))
	    (with-access::musicstatus %status (playlistid playlistlength)
	       (set! playlistid (+fx 1 playlistid))
	       (set! playlistlength %playlistlength))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::androidmusic ...                        */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! androidmusic::androidmusic n)
   (with-access::androidmusic androidmusic (%mutex %playlist %playlistlength %status)
      (with-lock %mutex
	 (lambda ()
	    (set! %playlist (delete! n %playlist string=?))
	    (set! %playlistlength (-fx %playlistlength 1))
	    (with-access::musicstatus %status (playlistid playlistlength)
	       (when (and (>=fx n 0) (<fx n playlistlength))
		  (set! %playlist (remq! (list-ref %playlist n) %playlist))
		  (set! playlistid (+fx 1 playlistid))
		  (set! playlistlength %playlistlength)))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-clear! ::androidmusic ...                         */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-clear! androidmusic::androidmusic)
   (with-access::androidmusic androidmusic (%mutex %playlist %playlistlength %status)
      (with-lock %mutex
	 (lambda ()
	    (set! %playlist '())
	    (set! %playlistlength 0)
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
   (with-access::androidmusic o (%mutex %open %status phone %playlist
				   %playlistid onevent)
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
		(let ((uri (charset-convert (list-ref playlist i))))
		   (set! song i)
		   (set! songid i)
		   (set! songpos 0)
		   (set! songlength 0)
		   (mutex-unlock! %mutex)
		   (when pid (onevent o 'playlist pid))
		   (with-handler
		      (lambda (e) #f)
		      (android-send-command phone music-plugin #\u uri))))))))

;*---------------------------------------------------------------------*/
;*    music-play ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-play o::androidmusic . s)
   (with-access::androidmusic o (%mutex %open %status phone)
      (with-access::musicstatus %status (song state playlistlength playlistid)
	 (with-lock %mutex
	    (lambda ()
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
		   (playlist-load! o song playlistid))))))))

;*---------------------------------------------------------------------*/
;*    music-seek ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-seek o::androidmusic pos . song)
   (with-access::androidmusic o (%mutex phone)
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
;*    music-song ::androidmusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-song o::androidmusic)
   (with-access::androidmusic o (%mutex %status)
      (with-lock %mutex
	 (lambda ()
	    (with-access::musicstatus %status (song)
	       song)))))

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
