;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/hop-audio.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 29 08:37:12 2007                          */
;*    Last change :  Sat Mar 21 17:56:49 2009 (serrano)                */
;*    Copyright   :  2007-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop Audio support.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-audio
   
   (library multimedia
	    web)

   (cond-expand
      (enable-threads (library pthread)))
   
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
	    __hop_charset
	    __hop_user
	    __hop_http-request
	    __hop_hop)
   
   (export  (init-hop-audio-services!)

	    (<AUDIO> . args)
	    
	    (class hop-audio-player
	       (hop-audio-player-init)
	       (engine read-only)
	       (name read-only (default #f))
	       (ident read-only (default (gensym 'hop-audio)))
	       (%thread (default #f))
	       (%service (default #unspecified))
	       (%event (default #unspecified))
	       (%mutex (default (make-mutex 'hop-audio-player)))
	       (%errcount::int (default 0))
	       (%status::symbol (default 'init))
	       (%log::pair-nil (default '())))
	    
	    (generic hop-audio-player-init ::hop-audio-player)
	    (generic hop-audio-player-close ::hop-audio-player)

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
   
   (with-access::hop-audio-player p (name %status %mutex %log ident)
      (with-lock %mutex
	 (lambda ()
	    (<TABLE> :style "border: 2px solid red; width: 100%"
	       (<COLGROUP> (<COL> :width "0*"))
	       (<TR> (<TH> :colspan 2 :style "text-align: left" "Player " name))
	       (<TR> (<TD> "ident: " (<TD> (<TT> ident))))
	       (<TR> (<TD> "status:") (<TD> (<TT> %status)))
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
   (let ((swf (make-file-path (hop-flash-directory) "HopAudio.swf"))
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
      (<IMG> :class (string-append "hop-audio-button " class)
	 :id (or id (xml-make-id "hopaudio-but"))
	 :title title :alt title :inline #t
	 :src (make-file-path (hop-icons-directory) "hop-audio" src)
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
			 (format "hop_audio_playlist_play(document.getElementById(~s), 0)"
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
		    " engine=" (find-runtime-type engine)
		    " state=" state " songlength=" songlength)
	    (hop-audio-player-%errcount-set! player 0)
	    (hop-event-broadcast! %event ev)))))

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
	  (let ((file (charset-convert meta 'UTF-8 (hop-locale))))
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
      (hop-event-broadcast! %event (list 'error error))))

;*---------------------------------------------------------------------*/
;*    audio-onvolume ...                                               */
;*---------------------------------------------------------------------*/
(define (audio-onvolume %event engine)
   (lambda (vol)
      (hop-event-broadcast! %event (list 'volume vol))))

;*---------------------------------------------------------------------*/
;*    make-audio-thread ...                                            */
;*---------------------------------------------------------------------*/
(define (make-audio-thread player)
   
   (define (audio-thread-trace player)
      (with-access::hop-audio-player player (%event %status %mutex engine %thread)
	 (let ((th (current-thread)))
	    ;; debug
	    (tprint ">>> AUDIO-LOOP STARTED, e=" (find-runtime-type engine))
	    (thread-cleanup-set!
	     th
	     (lambda (_)
		(with-access::musicstatus (music-%status engine) (err)
		   (if err
		       (hop-event-broadcast! %event (list 'abort err))
		       (begin
			  (set! %status 'closed)
			  (hop-event-broadcast! %event (list 'close)))))
		(tprint "<<< AUDIO-LOOP THREAD ENDED, e=" (find-runtime-type engine))
		(set! %thread #f))))))
   
   (thread-start!
    (make-thread
     (lambda ()
	(audio-thread-trace player)
	(with-access::hop-audio-player player (engine %event %errcount %status)
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
		       (set! %status 'error)
		       (onerror msg)))
		 (set! %status 'running)
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
   (with-access::hop-audio-player player (%log %mutex)
      (mutex-lock! %mutex)
      (let ((loge (car %log)))
	 (logentry-command-set! loge a0)
	 (logentry-args-set! loge a1)
	 (logentry-complete-set! loge #f)
      (mutex-unlock! %mutex))))

;*---------------------------------------------------------------------*/
;*    debug-log-complete ...                                           */
;*---------------------------------------------------------------------*/
(define (debug-log-complete player a0 a1)
   (with-access::hop-audio-player player (%log %mutex)
      (mutex-lock! %mutex)
      (let ((loge (car %log)))
	 (logentry-complete-set! loge #t)
	 (set! %log (cdr %log)))
      (mutex-unlock! %mutex)))

;*---------------------------------------------------------------------*/
;*    hop-audio-player-init ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (hop-audio-player-init player::hop-audio-player)

   (define (hop-audio-player-event-loop-start player)
      (with-access::hop-audio-player player (%thread %mutex %status %log engine)
	 (mutex-lock! %mutex)
	 (if (thread? %thread)
	     (begin
		(set! %status 'reset-event-loop)
		(mutex-unlock! %mutex)
		(music-event-loop-reset! engine))
	     (unwind-protect
		(begin
		   (set! %status 'start-event-loop)
		   (set! %thread (make-audio-thread player)))
		(mutex-unlock! %mutex)))))

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
				   (hop-audio-player-close player)))
			       (debug-log-complete player a0 a1)
			       #t)))
	  (set! %event (service-path %service))
	  player))
      (else
       (error 'hop-audio-player
	      "Player cannot be started in single-thread setting"
	      "Re-configure HOP with multi-threading enabled"))))

;*---------------------------------------------------------------------*/
;*    hop-audio-player-close ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (hop-audio-player-close audio::hop-audio-player)
   (with-access::hop-audio-player audio (%mutex %thread %status engine)
      (with-lock %mutex
	 (lambda ()
	    (set! %status 'close)
	    (when (thread? %thread)
	       (music-close engine)
	       (set! %thread #f)
	       (mutex-lock! debug-mutex)
	       (set! debug-players-list (remq! audio debug-players-list))
	       (mutex-unlock! debug-mutex))))))

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
