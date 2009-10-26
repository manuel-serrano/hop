;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/widget/audio.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 29 08:37:12 2007                          */
;*    Last change :  Mon Oct 26 07:35:55 2009 (serrano)                */
;*    Copyright   :  2007-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop Audio support.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-audio
   
   (library multimedia web hop)

   (import  __hopwidget-slider)
   
   (cond-expand
      (enable-threads (library pthread)))
   
   
   (export  (init-hop-audio-services!)

	    (<AUDIO> . args)
	    
	    (class hop-audio-player::music
	       (engine read-only)
	       (name read-only (default #f))
	       (ident read-only (default (gensym 'hop-audio)))
	       (%hmutex (default (make-mutex)))
	       (%thread (default #f))
	       (%service (default #unspecified))
	       (%event (default #unspecified))
	       (%errcount::int (default 0))
	       (%state::symbol (default 'init))
	       (%log::pair-nil (default '())))
	    
	    (hop-audio-player-json ::hop-audio-player)))

;*---------------------------------------------------------------------*/
;*    debug-players-list ...                                           */
;*---------------------------------------------------------------------*/
(define debug-players-list '())
(define debug-mutex (make-mutex "hop-audio-debug"))
(define debug-svc #f)
(define debug-log-length 10)

(define-struct logentry command args complete)

;*---------------------------------------------------------------------*/
;*    debug-player ...                                                 */
;*---------------------------------------------------------------------*/
(define (debug-player p)
   
   (define (<log> log)
      (<TABLE> :style "border: 1px dashed #777; width: 100%"
	 (let loop ((log log)
		    (i debug-log-length)
		    (res '()))
	    (if (>fx i 0)
		(loop (cdr log)
		      (-fx i 1)
		      (cons (<TR> (<TD> :style "vertical-align: top"
				     (<TT> :style (unless (logentry-complete (car log))
						     "color: red; font-weight: bold")
					(logentry-command (car log))))
				  (<TD> :style "vertical-align: top"
				     (<TT> (logentry-args (car log)))))
			    res))
		res))))
   
   (with-access::hop-audio-player p (name %state %hmutex %log ident)
      (with-lock %hmutex
	 (lambda ()
	    (<TABLE> :style "border: 2px solid red; width: 100%"
	       (<COLGROUP> (<COL> :width "0*"))
	       (<TR> (<TH> :colspan 2 :style "text-align: left" "Player " name))
	       (<TR> (<TD> "ident: " (<TD> (<TT> ident))))
	       (<TR> (<TD> "status:") (<TD> (<TT> %state)))
	       (<TR> (<TD> "log:" :style "vertical-align: top")
		     (<TD> :style "vertical-align: top"
			(<log> %log))))))))

;*---------------------------------------------------------------------*/
;*    init-hop-audio-services! ...                                     */
;*---------------------------------------------------------------------*/
(define (init-hop-audio-services!)
   (set! debug-svc
	 (service :name "hop-audio/debug" (name)
	    (when (authorized-service? (current-request) 'admin)
	       (with-lock debug-mutex
		  (lambda ()
		     (let ((p (if name
				  (filter (lambda (p)
					     (string=? (hop-audio-player-name p)
						       name))
					  debug-players-list)
				  debug-players-list)))
			(<HTML>
			   (<BODY>
			      (map debug-player p))))))))))
						      
;*---------------------------------------------------------------------*/
;*    <AUDIO> ...                                                      */
;*    -------------------------------------------------------------    */
;*    See __hop_css for HSS types.                                     */
;*---------------------------------------------------------------------*/
(define-xml-compound <AUDIO> ((id #unspecified string)
			      (src #f)
			      (autoplay #f boolean)
			      (start 0)
			      (loopstart 0)
			      (loopend 0)
			      (end -1)
			      (loopcount 0)
			      (controls #f)
			      (onload #f)
			      (onprogress #f)
			      (onerror #f)
			      (onended #f)
			      (onloadedmetadata #f)
			      (onplay #f)
			      (onstop #f)
			      (onpause #f)
			      (onnext #f)
			      (onprev #f)
			      (onclose #f)
			      (onplayer #f)
			      ;; the player
			      (player #f)
			      ;; controls click events
			      (onprevclick #unspecified)
			      (onplayclick #unspecified)
			      (onpauseclick #unspecified)
			      (onnextclick #unspecified)
			      (onstopclick #unspecified)
			      (onloadclick "alert('load: not implemented')")
			      (onpodcastclick "alert('podcast: not implemented')")
			      (onprefsclick "alert('prefs: not implemented')")
			      (onmuteclick #unspecified)
			      (onvolumechange #unspecified)
			      (onpanchange #unspecified)
			      (attr)
			      body)
   
   (let* ((id (xml-make-id id 'audio))
	  (pid (xml-make-id 'hopaudio))
	  (controller (when controls
			 (<AUDIO-CONTROLS>
			    :id id
			    :onprevclick onprevclick
			    :onpauseclick onpauseclick
			    :onplayclick onplayclick
			    :onstopclick onstopclick
			    :onnextclick onnextclick
			    :onloadclick onloadclick
			    :onprefsclick onprefsclick
			    :onpodcastclick onpodcastclick
			    :onmuteclick onmuteclick)))
	  (init (<AUDIO-INIT> :id id :pid pid :player player
		   :src src :autoplay autoplay
		   :start start
		   :onplay (hop->js-callback onplay)
		   :onstop (hop->js-callback onstop)
		   :onpause (hop->js-callback onpause)
		   :onload (hop->js-callback onload)
		   :onerror (hop->js-callback onerror)
		   :onended (hop->js-callback onended)
		   :onprogress (hop->js-callback onprogress)
		   :onloadedmetadata (hop->js-callback onloadedmetadata)
		   :onclose (hop->js-callback onclose)
		   :onplayer (hop->js-callback onplayer))))
      (<AUDIO-OBJECT> id pid init controller)))

;*---------------------------------------------------------------------*/
;*    <AUDIO-OBJECT> ...                                               */
;*---------------------------------------------------------------------*/
(define (<AUDIO-OBJECT> id pid init controller)
   (let ((swf (make-file-path (hop-share-directory) "flash" "HopAudio.swf"))
	 (fvar (string-append "arg=hop_audio_flash_init_" pid)))
      (<DIV> :id id :class "hop-audio"
	 controller
	 (<OBJECT> :id (string-append id "-object") :class "hop-audio"
	    :width "1px" :height "1px"
	    :title "hop-audio" :classId "HopAudio.swf"
	    (<PARAM> :name "movie" :value swf)
	    (<PARAM> :name "src" :value swf)
	    (<PARAM> :name "wmode" :value "transparent")
	    (<PARAM> :name "play" :value "1")
	    (<PARAM> :name "allowScriptAccess" :value "sameDomain")
	    (<PARAM> :name "FlashVars" :value fvar)
	    (<EMBED> :id (string-append id "-embed") :class "hop-audio"
	       :width "1px" :height "1px"
	       :src swf
	       :type "application/x-shockwave-flash"
	       :name id
	       :swliveconnect #t
	       :allowScriptAccess "sameDomain"
	       :FlashVars fvar))
	 init)))

;*---------------------------------------------------------------------*/
;*    <AUDIO-INIT> ...                                                 */
;*---------------------------------------------------------------------*/
(define (<AUDIO-INIT> #!key id pid player src autoplay start
		      onplay onstop onpause onload onerror onended onprogress
		      onloadedmetadata onclose onplayer)
   (<SCRIPT>
      (format "function hop_audio_flash_init_~a() {hop_audio_flash_init( ~s, ~a, ~a, ~a );};"
	      pid id
	      (if (string? src) (string-append "'" src "'") "false")
	      (if autoplay "true" "false")
	      (if player (hop->json player #f #f) "false"))
      (format "hop_window_onload_cons(
                function() {hop_audio_init( ~s, ~s, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a , ~a, ~a, ~a, ~a );} );"
	      id
	      start
	      (if (string? src) (string-append "'" src "'") "false")
	      (if autoplay "true" "false")
	      onplay
	      onstop
	      onpause
	      onload
	      onerror
	      onended
	      onprogress
	      onloadedmetadata
	      onclose
	      onplayer)))

;*---------------------------------------------------------------------*/
;*    <AUDIO-CONTROLS> ...                                             */
;*---------------------------------------------------------------------*/
(define (<AUDIO-CONTROLS> #!key
			  id
			  onprevclick onplayclick onpauseclick
			  onstopclick onnextclick
			  onloadclick onpodcastclick onmuteclick onprefsclick
			  onvolumechange onpanchange)
   
   (define (on action builtin)
      (if builtin
	  (format "var el = document.getElementById( '~a' );
                   if( el.on~aclick ) el.on~aclick( event );
	           if( !event.stopped ) hop_audio_~a( el );"
		  id action action action)
	  (format "var el = document.getElementById( '~a' );
                  if( el.on~a ) el.on~a( event );"
		  id action action)))
   
   (define (<BUT> #!key (class "") title id src onclick)
      (<DIV> :class (string-append "hop-audio-button " class)
	 :id (or id (xml-make-id "hopaudio-but"))
	 :title title :alt title :inline #t
	 :onclick onclick))

   (<DIV> :id (string-append id "-controls") :class "hop-audio-controls"
      ;; the controls callbacks
      (<SCRIPT>
	 (format "hop_window_onload_cons(
                   function() {var el=document.getElementById(~s);"
		 (string-append id "-controls"))
	 "el.onload=hop_audio_controls_onload;"
	 "el.onerror=hop_audio_controls_onerror;"
	 "el.onplay=hop_audio_controls_onplay;"
	 "el.onmetadata=hop_audio_controls_onmetadata;"
	 "el.onpause=hop_audio_controls_onpause;"
	 "el.onstop=hop_audio_controls_onstop;"
	 "el.onclose=hop_audio_controls_onclose;"
	 "el.onended=hop_audio_controls_onended;"
         "el.onprogress=hop_audio_controls_onprogress;"
         "el.onvolume=hop_audio_controls_onvolume;"
         "el.onplayer=hop_audio_controls_onplayer;})")
      ;; the info line
      (<TABLE> :class "hop-audio-panel"
	 (<TR>
	    (<TD> :class "hop-audio-panel"
	       (<AUDIO-CONTROLS-STATUS> id))
	    (<TD> :class "hop-audio-panel"
	       (<DIV> :class "hop-audio-panel2"
		  (<AUDIO-CONTROLS-METADATA> id)
		  (<AUDIO-CONTROLS-SOUND> id)))))
      ;; separator
      (<DIV> :class "hop-audio-separator")
      ;; the button line
      (<DIV> :class "hop-audio-buttons"
	 (<BUT> :title "Previous"
	    :class "hop-audio-button-prev"
	    :onclick (if (eq? onprevclick #unspecified)
			 (format "hop_audio_playlist_prev(document.getElementById(~s))"
				 id)
			 onprevclick))
	 (<BUT> :title "Play"
	    :id (string-append id "-hop-audio-button-play")
	    :class "hop-audio-button-play"
	    :onclick (if (eq? onplayclick #unspecified)
			 (format "hop_audio_playlist_play(document.getElementById(~s), 0)"
				 id)
			 onplayclick))
	 (<BUT> :title "Pause"
	    :id (string-append id "-hop-audio-button-pause")
	    :class "hop-audio-button-pause"
	    :onclick (if (eq? onpauseclick #unspecified)
			 (format "hop_audio_pause(document.getElementById(~s))"
				 id)
			 onpauseclick))
	 (<BUT> :title "Stop"
	    :id (string-append id "-hop-audio-button-stop")
	    :class "hop-audio-button-stop"
	    :onclick (if (eq? onstopclick #unspecified)
			 (format "hop_audio_stop(document.getElementById(~s))"
				 id)
			 onstopclick))
	 (<BUT> :title "Next"
	    :class "hop-audio-button-next"
	    :onclick (if (eq? onnextclick #unspecified)
			 (format "hop_audio_playlist_next(document.getElementById(~s))"
				 id)
			 onnextclick))
	 (<BUT> :title "Playlist"
	    :class "hop-audio-button-playlist"
	    :onclick onloadclick)
	 (<BUT> :title "Podcast" :src "podcast.png"
	    :class "hop-audio-button-podcast"
	    :onclick onpodcastclick)
	 (<BUT> :title "Mute"
	    :id (string-append id "-hop-audio-button-mute")
	    :class "hop-audio-button-mute"
	    :onclick (if (eq? onmuteclick #unspecified)
			 (format "hop_audio_mute(document.getElementById(~s))"
				 id)
			 onmuteclick))
	 (<BUT> :title "Preferences"
	    :id (string-append id "-hop-audio-button-prefs")
	    :class "hop-audio-button-prefs"
	    :onclick onprefsclick))))

;*---------------------------------------------------------------------*/
;*    <AUDIO-CONTROLS-STATUS> ...                                      */
;*---------------------------------------------------------------------*/
(define (<AUDIO-CONTROLS-STATUS> id)
   (<DIV> :class "hop-audio-info-status"
      :id (string-append id "-controls-status")
      (<DIV>
	 (<DIV> :class "hop-audio-info-status-img hop-audio-info-status-stop"
	    :id (string-append id "-controls-status-img")
	    " ")
	 (<SPAN> :class "hop-audio-info-status-position"
	    :id (string-append id "-controls-status-position")
	    "88:88"))
      (<DIV> :class "hop-audio-info-status-length"
	 (<SPAN> :class "hop-audio-info-status-length"
	    :id (string-append id "-controls-status-length-min")
	    "  ")
	 (<SPAN> :class "hop-audio-info-status-length-label" "min")
	 (<SPAN> :class "hop-audio-info-status-length"
	    :id (string-append id "-controls-status-length-sec")
	    "  ")
	 (<SPAN> :class "hop-audio-info-status-length-label" "sec"))
      (<DIV> :class "hop-audio-info-status-track"
	 (<SPAN> :class "hop-audio-info-status-track-label" "track")
	 (<SPAN> :class "hop-audio-info-status-track"
	    :id (string-append id "-controls-status-track")
	    "88888"))))

;*---------------------------------------------------------------------*/
;*    <AUDIO-CONTROLS-METADATA> ...                                    */
;*---------------------------------------------------------------------*/
(define (<AUDIO-CONTROLS-METADATA> id)
   (<DIV> :class "hop-audio-panel-metadata"
      :id (string-append id "-controls-metadata")
      (<DIV> :class "hop-audio-panel-metadata-song"
	 :id (string-append id "-controls-metadata-song")
	 "")
      (<TABLE> :class "hop-audio-panel-metadata"
	 (<TR>
	    (<TD> :class "hop-audio-panel-metadata-artist"
	       (<DIV>
		  :id (string-append id "-controls-metadata-artist")
		  ""))
	    (<TD> :class "hop-audio-panel-metadata-album"
	       (<DIV>
		  :id (string-append id "-controls-metadata-album")
		  ""))
	    (<TD> :class "hop-audio-panel-metadata-year"
	       (<DIV>
		  :id (string-append id "-controls-metadata-year")
		  ""))))))

;*---------------------------------------------------------------------*/
;*    <AUDIO-CONTROLS-SOUND> ...                                       */
;*---------------------------------------------------------------------*/
(define (<AUDIO-CONTROLS-SOUND> id)
   
   (define (on action)
      (format "var el = document.getElementById( '~a' );
               var evt = new HopAudioEvent();

               evt.value = this.value;
               if( el.on~aclick ) el.on~aclick( evt );
	       if( !evt.stopped ) hop_audio_~a( el, this.value );"
	       id action action action))

   (<DIV> :class "hop-audio-panel-sound"
      (<TABLE>
	 (<TR>
	    (<TH> "VOL")
	    (<TD>
	       (<SLIDER> :id (string-append id "-controls-volume")
		  :class "hop-audio-panel-volume"
		  :min 0 :max 100 :step 1 :value 100 :caption #f
		  :onchange (on "volume_set")))
	    (<TH> :class "hop-audio-panel-left" "L")
	    (<TD> (<SLIDER> :id (string-append id "-controls-pan")
		     :class "hop-audio-panel-pan"
		     :min -100 :max 100 :step 1 :value 0 :caption #f
		     :onchange (on "pan_set")))
	    (<TH> :class "hop-audio-panel-right" "R")))))

;*---------------------------------------------------------------------*/
;*    audio-onstate ...                                                */
;*---------------------------------------------------------------------*/
(define (audio-onstate %event engine player)
   (lambda (status)
      (with-trace 3 "audio-onstate"
	 (trace-item "engine=" (find-runtime-type engine))
	 (trace-item "player=" (find-runtime-type player))
	 (with-access::musicstatus status (state song songpos songlength volume)
	    (let ((ev (list state songlength songpos volume song)))
	       (trace-item "state=" state)
	       (trace-item "songlength=" songlength)
	       (hop-audio-player-%errcount-set! player 0)
	       (hop-event-broadcast! %event ev))))))

;*---------------------------------------------------------------------*/
;*    audio-onmeta ...                                                 */
;*---------------------------------------------------------------------*/
(define (audio-onmeta %event engine player)

   (define conv (charset-converter 'UTF-8 (hop-charset)))
   
   (define (convert-file file)
      (if (or (substring-at? file "http://" 0)
	      (substring-at? file "https://" 0))
	  ;; MS, note 2 Jan 09: This assumes that URL are encoded using
	  ;; the hop-locale value and not hop-charset, nor UTF-8. Of
	  ;; course this works iff all Hop uses the same locale encoding.
	  ;; the only fully correct solution would be to encode the encoding
	  ;; in the URL and Hop should detect that encoding.
	  ((hop-locale->charset) (url-decode file))
	  (conv file)))
   
   (define (signal-meta s plist)
      (let ((s (cond
		  ((id3? s)
		   (duplicate::id3 s
		      (title ((hop-locale->charset) (id3-title s)))
		      (artist ((hop-locale->charset) (id3-artist s)))
		      (album ((hop-locale->charset) (id3-album s)))
		      (orchestra #f)
		      (conductor #f)
		      (interpret #f)
		      (comment ((hop-locale->charset) (id3-comment s)))))
		  ((string? s)
		   (convert-file s))
		  (else
		   s)))
	    (plist (map convert-file plist)))
	 (trace-item "s=" (if (string? s) s (find-runtime-type s)))
	 (trace-item "pl length=" (length plist))
	 (hop-event-broadcast! %event (list 'meta s plist))))
   
   (define (audio-onfile-name meta plist)
      (let ((url (url-decode meta)))
	 (signal-meta url plist)))
   
   (lambda (meta playlist)
      (with-trace 3 "audio-onmeta"
	 (trace-item "engine=" (find-runtime-type engine))
	 (trace-item "player=" (find-runtime-type player))
	 (hop-audio-player-%errcount-set! player 0)
	 (if (string? meta)
	     ;; this is a file name (a url)
	     (let ((file (charset-convert meta 'UTF-8 (hop-locale))))
		(if (not (file-exists? file))
		    (audio-onfile-name file playlist)
		    (let ((id3 (mp3-id3 file)))
		       (if (not id3)
			   (audio-onfile-name file playlist)
			   (signal-meta id3 playlist)))))
	     (signal-meta #f playlist)))))

;*---------------------------------------------------------------------*/
;*    audio-onerror ...                                                */
;*---------------------------------------------------------------------*/
(define (audio-onerror %event engine)
   (lambda (error)
      (with-trace 3 "audio-onerror"
	 (trace-item "engine=" (find-runtime-type engine))
	 (hop-event-broadcast! %event (list 'error error)))))

;*---------------------------------------------------------------------*/
;*    audio-onvolume ...                                               */
;*---------------------------------------------------------------------*/
(define (audio-onvolume %event engine)
   (lambda (vol)
      (with-trace 3 "audio-onvolume"
	 (trace-item "engine=" (find-runtime-type engine))
	 (hop-event-broadcast! %event (list 'volume vol)))))

;*---------------------------------------------------------------------*/
;*    make-audio-thread ...                                            */
;*---------------------------------------------------------------------*/
(define (make-audio-thread player)
   
   (define (audio-thread-trace player)
      (with-access::hop-audio-player player (%event %state %hmutex engine %thread)
	 (let ((th (current-thread)))
	    ;; debug
	    (with-trace 2 'make-audio-thread
	       (trace-item "player=" (find-runtime-type player))
	       (trace-item "engine=" (find-runtime-type engine))
	       (trace-item "thread=" thread)
	       (thread-cleanup-set!
		th
		(lambda (_)
		   (with-trace 2 'audio-thread-cleanup
		      (trace-item "thread=" thread)
		      (with-access::musicstatus (music-%status engine) (err)
			 (if err
			     (hop-event-broadcast! %event (list 'abort err))
			     (begin
				(set! %state 'closed)
				(hop-event-broadcast! %event (list 'close))))))
		   (set! %thread #f)))))))
   
   (thread-start!
    (make-thread
     (lambda ()
	(audio-thread-trace player)
	(with-access::hop-audio-player player (engine %event %errcount %state)
	   (let ((onstate (audio-onstate %event engine player))
		 (onerror (audio-onerror %event engine))
		 (onvolume (audio-onvolume %event engine))
		 (onmeta (audio-onmeta %event engine player)))
	      (with-handler
		 (lambda (e)
		    (exception-notify e)
		    (let ((msg (with-error-to-string
				  (lambda ()
				     (exception-notify e)))))
		       (set! %state 'error)
		       (onerror msg)))
		 (set! %state 'running)
		 (music-event-loop engine
				   :onstate onstate
				   :onerror onerror
				   :onvolume onvolume
				   :onmeta onmeta))))))))

;*---------------------------------------------------------------------*/
;*    hop-music-playlist-add! ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-music-playlist-add! engine s)
   (music-playlist-add! engine (charset-convert s (hop-charset) 'UTF-8)))
   
;*---------------------------------------------------------------------*/
;*    music-playlist-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (music-playlist-set! engine a1)
   (music-playlist-clear! engine)
   (for-each (lambda (s) (hop-music-playlist-add! engine s)) a1))

;*---------------------------------------------------------------------*/
;*    debug-log ...                                                    */
;*---------------------------------------------------------------------*/
(define (debug-log player a0 a1)
   (with-access::hop-audio-player player (%log %hmutex)
      (mutex-lock! %hmutex)
      (let ((loge (car %log)))
	 (logentry-command-set! loge a0)
	 (logentry-args-set! loge a1)
	 (logentry-complete-set! loge #f)
      (mutex-unlock! %hmutex))))

;*---------------------------------------------------------------------*/
;*    debug-log-complete ...                                           */
;*---------------------------------------------------------------------*/
(define (debug-log-complete player a0 a1)
   (with-access::hop-audio-player player (%log %hmutex)
      (mutex-lock! %hmutex)
      (let ((loge (car %log)))
	 (logentry-complete-set! loge #t)
	 (set! %log (cdr %log)))
      (mutex-unlock! %hmutex)))

;*---------------------------------------------------------------------*/
;*    music-init ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (music-init player::hop-audio-player)

   (define (hop-audio-player-event-loop-start player)
      (with-access::hop-audio-player player (%thread %hmutex %state %log engine)
	 (mutex-lock! %hmutex)
	 (if (thread? %thread)
	     (begin
		(set! %state 'reset-event-loop)
		(mutex-unlock! %hmutex)
		(music-event-loop-reset! engine))
	     (unwind-protect
		(begin
		   (set! %state 'start-event-loop)
		   (set! %thread (make-audio-thread player)))
		(mutex-unlock! %hmutex)))))

   (mutex-lock! debug-mutex)
   (set! debug-players-list (cons player debug-players-list))
   (let ((log (map! (lambda (_) (logentry #f #f #t)) (iota debug-log-length))))
      (set-cdr! (last-pair log) log)
      (hop-audio-player-%log-set! player log))
   (mutex-unlock! debug-mutex)
   
   (cond-expand
      (enable-threads
       (with-access::hop-audio-player player (%service %event engine)
	  (set! %service (service :name (get-service-url "hop-audio") (a0 a1)
			    (debug-log player a0 a1)
			    (with-trace 2 'hop-audio
			       (trace-item "a0=" a0)
			       (trace-item "a1=" a1)
			       (trace-item "engine=" (find-runtime-type engine))
			       (trace-item "player=" (find-runtime-type player))
			       (with-handler
				  (lambda (e)
				     (error-notify e)
				     #f)
				  (case a0
				     ((ready)
				      (hop-audio-player-event-loop-start player)
				      #unspecified)
				     ((load)
				      (music-playlist-clear! engine)
				      (hop-music-playlist-add! engine a1))
				     ((pause)
				      (music-pause engine))
				     ((play)
				      (music-play engine a1)
				      #unspecified)
				     ((stop)
				      (music-stop engine))
				     ((position)
				      (music-seek engine a1))
				     ((playlist)
				      (music-playlist-set! engine a1))
				     ((volume)
				      (music-volume-set! engine a1))
				     ((close)
				      (music-close player)))
				  (debug-log-complete player a0 a1)
				  #t))))
	  (set! %event (service-path %service))
	  player))
      (else
       (error 'hop-audio-player
	      "Player cannot be started in single-thread setting"
	      "Re-configure HOP with multi-threading enabled"))))

;*---------------------------------------------------------------------*/
;*    music-close ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (music-close audio::hop-audio-player)
   (with-access::hop-audio-player audio (%hmutex %thread %state engine)
      (with-lock %hmutex
	 (lambda ()
	    (set! %state 'close)
	    (when (thread? %thread)
	       (music-close engine)
	       (set! %thread #f)
	       (mutex-lock! debug-mutex)
	       (set! debug-players-list (remq! audio debug-players-list))
	       (mutex-unlock! debug-mutex))))))

;*---------------------------------------------------------------------*/
;*    music-closed? ::hop-audio-player ...                             */
;*---------------------------------------------------------------------*/
(define-method (music-closed? audio::hop-audio-player)
   (with-access::hop-audio-player audio (%state %hmutex)
      (with-lock %hmutex
	 (lambda ()
	    (eq? %state 'close)))))

;*---------------------------------------------------------------------*/
;*    music-reset! ::hop-audio-player ...                              */
;*---------------------------------------------------------------------*/
(define-method (music-reset! audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-reset! engine)))

;*---------------------------------------------------------------------*/
;*    music-playlist-get ::hop-audio-player ...                        */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-get audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-playlist-get engine)))
   
;*---------------------------------------------------------------------*/
;*    music-playlist-add! ::hop-audio-player ...                       */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-add! audio::hop-audio-player song)
   (with-access::hop-audio-player audio (engine)
      (music-playlist-add! engine song)))
   
;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::hop-audio-player ...                    */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! audio::hop-audio-player num)
   (with-access::hop-audio-player audio (engine)
      (music-playlist-delete! engine num)))
   
;*---------------------------------------------------------------------*/
;*    music-playlist-clear! ::hop-audio-player ...                     */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-clear! audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-playlist-clear! engine)))
   
;*---------------------------------------------------------------------*/
;*    music-play ::hop-audio-player ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-play audio::hop-audio-player . song)
   (with-access::hop-audio-player audio (engine)
      (apply music-play engine song)))
   
;*---------------------------------------------------------------------*/
;*    music-seek ::hop-audio-player ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-seek audio::hop-audio-player obj . song)
   (with-access::hop-audio-player audio (engine)
      (apply music-seek engine obj song)))
   
;*---------------------------------------------------------------------*/
;*    music-stop ::hop-audio-player ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-stop audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-stop engine)))
   
;*---------------------------------------------------------------------*/
;*    music-pause ::hop-audio-player ...                               */
;*---------------------------------------------------------------------*/
(define-method (music-pause audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-pause engine)))
   
;*---------------------------------------------------------------------*/
;*    music-next ::hop-audio-player ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-next audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-next engine)))
   
;*---------------------------------------------------------------------*/
;*    music-prev ::hop-audio-player ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-prev audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-next engine)))
   
;*---------------------------------------------------------------------*/
;*    music-crossfade ::hop-audio-player ...                           */
;*---------------------------------------------------------------------*/
(define-method (music-crossfade audio::hop-audio-player i)
   (with-access::hop-audio-player audio (engine)
      (music-crossfade engine i)))
   
;*---------------------------------------------------------------------*/
;*    music-random-set! ::hop-audio-player ...                         */
;*---------------------------------------------------------------------*/
(define-method (music-random-set! audio::hop-audio-player b)
   (with-access::hop-audio-player audio (engine)
      (music-random-set! engine b)))
   
;*---------------------------------------------------------------------*/
;*    music-repeat-set! ::hop-audio-player ...                         */
;*---------------------------------------------------------------------*/
(define-method (music-repeat-set! audio::hop-audio-player b)
   (with-access::hop-audio-player audio (engine)
      (music-repeat-set! engine b)))

;*---------------------------------------------------------------------*/
;*    music-reset-error! ::hop-audio-player ...                        */
;*---------------------------------------------------------------------*/
(define-method (music-reset-error! audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-reset-error! engine)))
   
;*---------------------------------------------------------------------*/
;*    music-status ::hop-audio-player ...                              */
;*---------------------------------------------------------------------*/
(define-method (music-status audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-status engine)))
   
;*---------------------------------------------------------------------*/
;*    music-update-status! ::hop-audio-player ...                      */
;*---------------------------------------------------------------------*/
(define-method (music-update-status! audio::hop-audio-player status)
   (with-access::hop-audio-player audio (engine)
      (music-update-status! engine status)))
   
;*---------------------------------------------------------------------*/
;*    music-song ::hop-audio-player ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-song audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-song engine)))
   
;*---------------------------------------------------------------------*/
;*    music-songpos ::hop-audio-player ...                             */
;*---------------------------------------------------------------------*/
(define-method (music-songpos audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-songpos engine)))
   
;*---------------------------------------------------------------------*/
;*    music-meta ::hop-audio-player ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-meta audio::hop-audio-player)
   (with-access::hop-audio-player audio (engine)
      (music-meta engine)))
   
;*---------------------------------------------------------------------*/
;*    hop-audio-player-json ...                                        */
;*---------------------------------------------------------------------*/
(define (hop-audio-player-json player)
   (with-access::hop-audio-player player (%event)
      %event))

;*---------------------------------------------------------------------*/
;*    hop->json ::hop-audio-player ...                                 */
;*---------------------------------------------------------------------*/
(define-method (hop->json player::hop-audio-player isrep isflash)
   (format "\"~a\"" (hop-audio-player-json player)))
