;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-audio.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 29 08:37:12 2007                          */
;*    Last change :  Mon Oct 29 17:49:05 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop Audio support.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-audio
   
   (library multimedia
	    web)

   (library pthread)
   
   (include "xml.sch"
	    "service.sch")
   
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_js-lib
	    __hop_service
	    __hop_hop-slider
	    __hop_hop-extra
	    __hop_thread
	    __hop_cgi
	    __hop_read
	    __hop_event
	    __hop_http-error
	    __hop_charset)
   
   (export  (<AUDIO> . args)
	    
	    (class hop-audio-player
	       (hop-audio-player-init)
	       (%thread (default #f))
	       (%service (default #unspecified))
	       (%event (default #unspecified))
	       (%mutex (default (make-mutex 'hop-audio-player)))
	       (%refresh (default #t))
	       (engine read-only))
	    
	    (generic hop-audio-player-init ::hop-audio-player)
	    (generic hop-audio-player-close ::hop-audio-player)))

;*---------------------------------------------------------------------*/
;*    <AUDIO> ...                                                      */
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
			      (onvolumechange #f)
			      (onloadedmetadata #f)
			      (onplay #f)
			      (onstop #f)
			      (onpause #f)
			      (onnext #f)
			      (onprev #f)
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

   (define (expr->function expr)
      (cond
	 ((xml-tilde? expr)
	  (format "function( event ) { ~a }" (tilde->string expr)))
	 ((string? expr)
	  (format "function( event ) { ~a }" expr))
	 (else
	  "false")))
   
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
		   :onplay (expr->function onplay)
		   :onstop (expr->function onstop)
		   :onpause (expr->function onpause)
		   :onload (expr->function onload)
		   :onerror (expr->function onerror)
		   :onended (expr->function onended)
		   :onprogress (expr->function onprogress))))
      (<AUDIO-OBJECT> id pid init controller)))

;*---------------------------------------------------------------------*/
;*    <AUDIO-OBJECT> ...                                               */
;*---------------------------------------------------------------------*/
(define (<AUDIO-OBJECT> id pid init controller)
   (let ((swf (make-file-path (hop-share-directory) "flash" "HopAudio.swf"))
	 (fvar (string-append "arg=hop_audio_flash_init_" pid)))
      (<DIV> :id id :class "hop-audio"
	 controller
	 (<OBJECT> :id (string-append "object-" id) :class "hop-audio"
	    :width "1px" :height "1px"
	    :title "hop-audio" :classId "HopAudio.swf"
	    (<PARAM> :name "movie" :value swf)
	    (<PARAM> :name "src" :value swf)
	    (<PARAM> :name "wmode" :value "transparent")
	    (<PARAM> :name "play" :value "1")
	    (<PARAM> :name "allowScriptAccess" :value "sameDomain")
	    (<PARAM> :name "FlashVars" :value fvar)
	    (<EMBED> :id (string-append "embed-" id) :class "hop-audio"
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
		      onplay onstop onpause onload onerror onended onprogress)
   (<SCRIPT>
      (format "function hop_audio_flash_init_~a() {hop_audio_flash_init( ~s, ~a, ~a, ~a );};"
	      pid id
	      (if (string? src) (string-append "'" src "'") "false")
	      (if autoplay "true" "false")
	      (if player (hop->json player) "false"))
      (format "hop_window_onload_add(
                function() {hop_audio_init( ~s, ~s, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a );} );"
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
	      onprogress)))

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
      (<IMG> :class (string-append "hop-audio-button " class)
	 :id (or id (xml-make-id "hopaudio-but"))
	 :title title :alt title :inline #t
	 :src (make-file-path (hop-icons-directory) "hop-audio" src)
	 :onclick onclick))
   
   (<DIV> :id (string-append "controls-" id) :class "hop-audio-controls"
      ;; the controls callbacks
      (<SCRIPT>
	 (format "hop_window_onload_add(
                   function() {var el=document.getElementById(~s);"
		 (string-append "controls-" id))
	 "el.onload=hop_audio_controls_onload;"
	 "el.onerror=hop_audio_controls_onerror;"
	 "el.onplay=hop_audio_controls_onplay;"
	 "el.onpause=hop_audio_controls_onpause;"
	 "el.onstop=hop_audio_controls_onstop;"
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
	 (<BUT> :title "Previous" :src "prev.png"
	    :onclick (if (eq? onprevclick #unspecified)
			 (format "hop_audio_playlist_prev(document.getElementById(~s))"
				 id)
			 onprevclick))
	 (<BUT> :title "Play" :src "play.png"
	    :id (string-append "hop-audio-button-play-" id)
	    :onclick (if (eq? onplayclick #unspecified)
			 (format "hop_audio_play(document.getElementById(~s))"
				 id)
			 onplayclick))
	 (<BUT> :title "Pause" :src "pause.png"
	    :id (string-append "hop-audio-button-pause-" id)
	    :onclick (if (eq? onpauseclick #unspecified)
			 (format "hop_audio_pause(document.getElementById(~s))"
				 id)
			 onpauseclick))
	 (<BUT> :title "Stop" :src "stop.png"
	    :id (string-append "hop-audio-button-stop-" id)
	    :onclick (if (eq? onstopclick #unspecified)
			 (format "hop_audio_stop(document.getElementById(~s))"
				 id)
			 onstopclick))
	 (<BUT> :title "Next" :src "next.png"
	    :onclick (if (eq? onnextclick #unspecified)
			 (format "hop_audio_playlist_next(document.getElementById(~s))"
				 id)
			 onnextclick))
	 (<BUT> :title "Playlist" :src "playlist.png"
	    :class "hop-audio-button-playlist"
	    :onclick onloadclick)
	 (<BUT> :title "Podcast" :src "podcast.png"
	    :class "hop-audio-button-podcast"
	    :onclick onpodcastclick)
	 (<BUT> :title "Mute" :src "mute.png"
	    :id (string-append "hop-audio-button-mute-" id)
	    :class "hop-audio-button-mute"
	    :onclick (if (eq? onmuteclick #unspecified)
			 (format "hop_audio_mute(document.getElementById(~s))"
				 id)
			 onmuteclick))
	 (<BUT> :title "Preferences" :src "prefs.png"
	    :id (string-append "hop-audio-button-prefs-" id)
	    :class "hop-audio-button-prefs"
	    :onclick onprefsclick))))

;*---------------------------------------------------------------------*/
;*    <AUDIO-CONTROLS-STATUS> ...                                      */
;*---------------------------------------------------------------------*/
(define (<AUDIO-CONTROLS-STATUS> id)
   (<DIV> :class "hop-audio-info-status"
      :id (string-append "controls-status-" id)
      (<DIV>
	 (<IMG> :class "hop-audio-info-status-img"
	    :id (string-append "controls-status-img-" id)
	    :src (make-file-path (hop-icons-directory) "hop-audio" "stop.png"))
	 (<SPAN> :class "hop-audio-info-status-position"
	    :id (string-append "controls-status-position-" id)
	    "88:88"))
      (<DIV> :class "hop-audio-info-status-length"
	 (<SPAN> :class "hop-audio-info-status-length"
	    :id (string-append "controls-status-length-min-" id)
	    "&nbsp;&nbsp;")
	 (<SPAN> :class "hop-audio-info-status-length-label" "min")
	 (<SPAN> :class "hop-audio-info-status-length"
	    :id (string-append "controls-status-length-sec-" id)
	    "&nbsp;&nbsp;")
	 (<SPAN> :class "hop-audio-info-status-length-label" "sec"))
      (<DIV> :class "hop-audio-info-status-track"
	 (<SPAN> :class "hop-audio-info-status-track-label" "track")
	 (<SPAN> :class "hop-audio-info-status-track"
	    :id (string-append "controls-status-track-" id)
	    "88888"))))

;*---------------------------------------------------------------------*/
;*    <AUDIO-CONTROLS-METADATA> ...                                    */
;*---------------------------------------------------------------------*/
(define (<AUDIO-CONTROLS-METADATA> id)
   (<DIV> :class "hop-audio-panel-metadata"
      :id (string-append "controls-metadata-" id)
      (<DIV> :class "hop-audio-panel-metadata-song"
	 :id (string-append "controls-metadata-song-" id)
	 "")
      (<TABLE> :class "hop-audio-panel-metadata"
	 (<TR>
	    (<TD> :class "hop-audio-panel-metadata-artist"
	       (<DIV>
		  :id (string-append "controls-metadata-artist-" id)
		  ""))
	    (<TD> :class "hop-audio-panel-metadata-album"
	       (<DIV>
		  :id (string-append "controls-metadata-album-" id)
		  ""))
	    (<TD> :class "hop-audio-panel-metadata-year"
	       (<DIV>
		  :id (string-append "controls-metadata-year-" id)
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
	    (<TD> (<SLIDER> :id (string-append "controls-volume-" id)
		     :class "hop-audio-panel-volume"
		     :min 0 :max 100 :step 1 :value 100 :caption #f
		     :onchange (on "volume_set")))
	    (<TH> "&nbsp;L")
	    (<TD> (<SLIDER> :id (string-append "controls-pan-" id)
		     :class "hop-audio-panel-pan"
		     :min -100 :max 100 :step 1 :value 0 :caption #f
		     :onchange (on "pan_set")))
	    (<TH> "R")))))

;*---------------------------------------------------------------------*/
;*    signal-meta! ...                                                 */
;*---------------------------------------------------------------------*/
(define (signal-meta! %event engine state pos)
   (let* ((s (music-song engine))
	  (c (when (pair? s) (assq :file s)))
	  (file (if (pair? c) (cdr c) #f))
	  (_ (tprint "music-playlist-get.1.."))
	  (pl (music-playlist-get engine)))
      (tprint "music-playlist-get.2..:" pl)
      (multiple-value-bind (_ _ song _ len vol _ _ _)
	 (music-info engine)
	 (tprint "signal-meta: " state " song=" song " len=" len " pos=" pos)
	 (if (string? file)
	     (with-handler
		(lambda (e)
		   (when (string? file)
		      (let ((s (charset-convert (url-decode file)
						(hop-locale)
						(hop-charset))))
			 (hop-event-broadcast!
			  %event (list 'meta state len pos s pl song vol)))))
		(let ((s (charset-convert (mp3-id3 file)
					  (hop-locale)
					  (hop-charset))))
		   (hop-event-broadcast!
		    %event (list 'meta state len pos s pl song vol))))
	     (hop-event-broadcast!
	      %event (list 'meta state len pos #f pl song vol))))))

;*---------------------------------------------------------------------*/
;*    signal-volume! ...                                               */
;*---------------------------------------------------------------------*/
(define (signal-volume! %event vol)
   (hop-event-broadcast! %event (list 'volume vol)))

;*---------------------------------------------------------------------*/
;*    signal-state! ...                                                */
;*---------------------------------------------------------------------*/
(define (signal-state! %event state len pos)
   (tprint "signal-state! state=" state " pos=" pos " len=" len)
   (hop-event-broadcast! %event (list state len pos)))

;*---------------------------------------------------------------------*/
;*    refresh-forced? ...                                              */
;*---------------------------------------------------------------------*/
(define (refresh-forced? player)
   (with-access::hop-audio-player player (%mutex %refresh)
      (mutex-lock! %mutex)
      (if %refresh
	  (begin
	     (set! %refresh #f)
	     (mutex-unlock! %mutex)
	     #t)
	  (begin
	     (mutex-unlock! %mutex)
	     #f))))

;*---------------------------------------------------------------------*/
;*    audio-loop ...                                                   */
;*---------------------------------------------------------------------*/
(define (audio-loop player)
   (with-access::hop-audio-player player (%event engine)
      (let luup ((oldstate 'stop)
		 (oldvol -1)
		 (oldsong -1)
		 (oldplaylist -1)
		 (ttl 60))
	 (multiple-value-bind (state playlist song pos len vol err _ _)
	    (music-info engine)
	    (cond
	       ((string? err)
		(unless (eq? oldstate 'error)
		   (signal-state! %event 'error err #f))
		;; an new error has been spotted
		(sleep 1000347)
		(luup 'error oldvol oldsong oldplaylist 60))
	       ((not (=fx vol oldvol))
		;; volume notification
		(signal-volume! %event vol)
		(luup oldstate vol oldsong oldplaylist 60))
	       ((refresh-forced? player)
		;; a refresh has been forced
		(signal-meta! %event engine state pos)
		(luup state vol song playlist 60))
	       ((or (not (=fx oldsong song))
		    (not (=fx oldplaylist playlist)))
		;; playlist (meta) notification
		(signal-meta! %event engine state pos)
		(luup state vol song playlist 60))
	       ((=fx len 0)
		;; the engine has not gathered yet the music length
		(sleep 1000347)
		(luup 'length-unknown vol song playlist ttl))
	       ((or (not (eq? oldstate state)) (=fx ttl 0))
		;; notity a state change (or a ttl reaches)
		(signal-state! %event state len pos)
		(luup state vol song playlist 60))
	       ((hop-event-client-ready? %event)
		;; nothing has changed but clients are still connected
		(sleep 1000347)
		(luup state vol song playlist 60)))))
      #f))

;*---------------------------------------------------------------------*/
;*    make-audio-thread ...                                            */
;*---------------------------------------------------------------------*/
(define (make-audio-thread player)
   
   (define (wait-first-client player)
      (with-access::hop-audio-player player (%event)
	 (let loop ((count 120))
	    (cond
	       ((=fx count 0)
		#f)
	       ((hop-event-client-ready? %event)
		#t)
	       (else
		(sleep 1000347)
		(loop (-fx count 1)))))))
   
   (make-hop-thread
    (lambda ()
       (with-access::hop-audio-player player (%event engine)
	  (when (wait-first-client player)
	     (let liip ()
		(with-handler
		   (lambda (e)
		      (tprint "***AUDIO-THREAD ERROR: " e)
		      (if (&io-error? e)
			  (begin
			     (error-notify e)
			     (signal-state! %event 'error (&io-error-msg e) #f))
			  (begin
			     (tprint "========= CLOSING..." %event)
			     (music-close engine)
			     (raise e))))
		   (tprint "======== AUDIO-LOOP STARTED...")
		   (audio-loop player))
		(sleep 3000562)
		(when (hop-event-client-ready? %event)
		   (liip))))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (music-playlist-set! engine a1)
   (music-playlist-clear! engine)
   (for-each (lambda (s)
		(music-playlist-add! engine s))
	     a1))
   
;*---------------------------------------------------------------------*/
;*    hop-audio-player-init ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (hop-audio-player-init player::hop-audio-player)

   (define (hop-audio-player-event-loop-start player)
      (with-access::hop-audio-player player (%thread %mutex %event %refresh)
	 (with-lock (hop-audio-player-%mutex player)
	    (lambda ()
	       (set! %refresh #t)
	       (unless (hop-thread? %thread)
		  (set! %thread (make-audio-thread player)))))))
   
   (cond-expand
      (enable-threads
       (with-access::hop-audio-player player (%service %event engine)
	  (set! %service (service (a0 a1)
			    (tprint "player received: " a0)
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
				   (music-playlist-add! engine a1))
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
				   (music-volume-set! engine a1)))
			       #t)))
	  (set! %event (hop-service-path %service))
	  player))
      (else
       (error 'hop-audio-player
	      "Player cannot be started in single-thread setting"
	      "Re-configure HOP with multi-threading enabled"))))

;*---------------------------------------------------------------------*/
;*    hop-audio-player-close ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (hop-audio-player-close audio::hop-audio-player)
   (with-access::hop-audio-player audio (%mutex %thread engine)
      (with-lock %mutex
	 (lambda ()
	    (music-close engine)
	    (when (hop-thread? %thread)
	       (hop-thread-terminate! %thread)
	       (set! %thread #f))))))

;*---------------------------------------------------------------------*/
;*    hop->json ::hop-audio-player ...                                 */
;*---------------------------------------------------------------------*/
(define-method (hop->json player::hop-audio-player)
   (with-access::hop-audio-player player (%event)
      (format "\"~a\"" %event)))
