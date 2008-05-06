;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/hop-audio.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 29 08:37:12 2007                          */
;*    Last change :  Tue May  6 19:56:18 2008 (serrano)                */
;*    Copyright   :  2007-08 Manuel Serrano                            */
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
	    __hop_img
	    __hop_misc
	    __hop_js-lib
	    __hop_service
	    __hop_hop-slider
	    __hop_hop-extra
	    __hop_cgi
	    __hop_read
	    __hop_hop-sym
	    __hop_event
	    __hop_http-error
	    __hop_charset)
   
   (export  (<AUDIO> . args)
	    
	    (class hop-audio-player
	       (hop-audio-player-init)
	       (engine read-only)
	       (name read-only (default #f))
	       (%thread (default #f))
	       (%service (default #unspecified))
	       (%event (default #unspecified))
	       (%mutex (default (make-mutex 'hop-audio-player)))
	       (%errcount::int (default 0)))
	    
	    (generic hop-audio-player-init ::hop-audio-player)
	    (generic hop-audio-player-close ::hop-audio-player)

	    (hop-audio-player-json ::hop-audio-player)))

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
		   :onclose (hop->js-callback onclose))))
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
		      onloadedmetadata onclose)
   (<SCRIPT>
      (format "function hop_audio_flash_init_~a() {hop_audio_flash_init( ~s, ~a, ~a, ~a );};"
	      pid id
	      (if (string? src) (string-append "'" src "'") "false")
	      (if autoplay "true" "false")
	      (if player (hop->json player #f #f) "false"))
      (format "hop_window_onload_add(
                function() {hop_audio_init( ~s, ~s, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a , ~a, ~a, ~a );} );"
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
	      onclose)))

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

   (<DIV> :id (string-append id "-controls") :class "hop-audio-controls"
      ;; the controls callbacks
      (<SCRIPT>
	 (format "hop_window_onload_add(
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
	 (<BUT> :title "Previous" :src "prev.png"
	    :class "hop-audio-button-prev"
	    :onclick (if (eq? onprevclick #unspecified)
			 (format "hop_audio_playlist_prev(document.getElementById(~s))"
				 id)
			 onprevclick))
	 (<BUT> :title "Play" :src "play.png"
	    :id (string-append id "-hop-audio-button-play")
	    :class "hop-audio-button-play"
	    :onclick (if (eq? onplayclick #unspecified)
			 (format "hop_audio_play(document.getElementById(~s))"
				 id)
			 onplayclick))
	 (<BUT> :title "Pause" :src "pause.png"
	    :id (string-append id "-hop-audio-button-pause")
	    :class "hop-audio-button-pause"
	    :onclick (if (eq? onpauseclick #unspecified)
			 (format "hop_audio_pause(document.getElementById(~s))"
				 id)
			 onpauseclick))
	 (<BUT> :title "Stop" :src "stop.png"
	    :id (string-append id "-hop-audio-button-stop")
	    :class "hop-audio-button-stop"
	    :onclick (if (eq? onstopclick #unspecified)
			 (format "hop_audio_stop(document.getElementById(~s))"
				 id)
			 onstopclick))
	 (<BUT> :title "Next" :src "next.png"
	    :class "hop-audio-button-next"
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
	    :id (string-append id "-hop-audio-button-mute")
	    :class "hop-audio-button-mute"
	    :onclick (if (eq? onmuteclick #unspecified)
			 (format "hop_audio_mute(document.getElementById(~s))"
				 id)
			 onmuteclick))
	 (<BUT> :title "Preferences" :src "prefs.png"
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
	 (<IMG> :class "hop-audio-info-status-img"
	    :id (string-append id "-controls-status-img")
	    :src (make-file-path (hop-icons-directory) "hop-audio" "stop.png"))
	 (<SPAN> :class "hop-audio-info-status-position"
	    :id (string-append id "-controls-status-position")
	    "88:88"))
      (<DIV> :class "hop-audio-info-status-length"
	 (<SPAN> :class "hop-audio-info-status-length"
	    :id (string-append id "-controls-status-length-min")
	    "&nbsp;&nbsp;")
	 (<SPAN> :class "hop-audio-info-status-length-label" "min")
	 (<SPAN> :class "hop-audio-info-status-length"
	    :id (string-append id "-controls-status-length-sec")
	    "&nbsp;&nbsp;")
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
	    (<TH> "&nbsp;L")
	    (<TD> (<SLIDER> :id (string-append id "-controls-pan")
		     :class "hop-audio-panel-pan"
		     :min -100 :max 100 :step 1 :value 0 :caption #f
		     :onchange (on "pan_set")))
	    (<TH> "R")))))

;*---------------------------------------------------------------------*/
;*    audio-onstate ...                                                */
;*---------------------------------------------------------------------*/
(define (audio-onstate %event engine player)
   (lambda (status)
      (with-access::musicstatus status (state song songpos songlength volume)
	 (let ((ev (list state songlength songpos volume song)))
	    (tprint "audio signal state: event=" ev
		    " engine=" (find-runtime-type engine))
	    (hop-audio-player-%errcount-set! player 0)
	    (hop-event-broadcast! %event ev)))))

;*---------------------------------------------------------------------*/
;*    audio-onmeta ...                                                 */
;*---------------------------------------------------------------------*/
(define (audio-onmeta %event engine player)
   
   (define (signal-meta s plist)
      (let* ((conv (hop-locale->charset))
	     (s (cond
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
		    ((hop-locale->charset) s))
		   (else
		    s)))
	     (plist (map (lambda (f) (conv (url-decode f))) plist)))
	 (tprint "signal meta s=" (if (string? s) s (find-runtime-type s))
		 " plist.length=" (length plist)
		 " engine=" (find-runtime-type engine))
	 (hop-event-broadcast! %event (list 'meta s plist))))
   
   (define (audio-onfile-name meta plist)
      (let ((url (url-decode meta)))
	 (signal-meta url plist)))
   
   (lambda (meta playlist)
      (hop-audio-player-%errcount-set! player 0)
      (if (string? meta)
	  ;; this is a file name (a url)
	  (let ((file ((hop-charset->locale) meta)))
	     (if (not (file-exists? file))
		 (audio-onfile-name file playlist)
		 (let ((id3 (mp3-id3 file)))
		    (if (not id3)
			(audio-onfile-name file playlist)
			(signal-meta id3 playlist)))))
	  (signal-meta #f playlist))))

;*---------------------------------------------------------------------*/
;*    audio-onerror ...                                                */
;*---------------------------------------------------------------------*/
(define (audio-onerror %event engine)
   (lambda (error)
      (tprint "audio signal error=" error " engine=" (find-runtime-type engine))
      (hop-event-broadcast! %event (list 'error error))))

;*---------------------------------------------------------------------*/
;*    audio-onvolume ...                                               */
;*---------------------------------------------------------------------*/
(define (audio-onvolume %event engine)
   (lambda (vol)
      (tprint "audio signal volume=" vol " engine=" (find-runtime-type engine))
      (hop-event-broadcast! %event (list 'volume vol))))

;*---------------------------------------------------------------------*/
;*    make-audio-thread ...                                            */
;*---------------------------------------------------------------------*/
(define (make-audio-thread player)
   
   (define (audio-thread-trace player)
      (with-access::hop-audio-player player (%event engine)
	 (let ((th (current-thread)))
	    ;; debug
	    (tprint ">>> AUDIO-LOOP STARTED, e=" (find-runtime-type engine))
	    (thread-cleanup-set!
	     th
	     (lambda (_)
		(hop-event-broadcast! %event (list 'close))
		(tprint "<<< AUDIO-LOOP THREAD ENDED, e=" (find-runtime-type engine)))))))
   
   (thread-start!
    (make-thread
     (lambda ()
	(audio-thread-trace player)
	(with-access::hop-audio-player player (engine %event %errcount)
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
		       (onerror msg)))
		 (music-event-loop engine
				   :onstate onstate
				   :onerror onerror
				   :onvolume onvolume
				   :onmeta onmeta))))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (music-playlist-set! engine a1)
   (music-playlist-clear! engine)
   (for-each (lambda (s)
		(music-playlist-add! engine ((hop-charset->locale) s)))
	     a1))
   
;*---------------------------------------------------------------------*/
;*    hop-audio-player-init ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (hop-audio-player-init player::hop-audio-player)

   (define (hop-audio-player-event-loop-start player)
      (with-access::hop-audio-player player (%thread %mutex engine)
	 (mutex-lock! %mutex)
	 (if (thread? %thread)
	     (begin
		(mutex-unlock! %mutex)
		(music-event-loop-reset! engine))
	     (unwind-protect
		(set! %thread (make-audio-thread player))
		(mutex-unlock! %mutex)))))
   
   (cond-expand
      (enable-threads
       (with-access::hop-audio-player player (%service %event engine)
	  (set! %service (service :name (get-service-url "hopaudio") (a0 a1)
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
				   (music-volume-set! engine a1))
				  ((close)
				   (hop-audio-player-close player)))
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
	    (when (thread? %thread)
	       (music-event-loop-abort! engine)
	       (music-close engine)
	       (thread-terminate! %thread)
	       (set! %thread #f))))))

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

   
