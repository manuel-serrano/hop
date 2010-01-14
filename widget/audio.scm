;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/widget/audio.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 29 08:37:12 2007                          */
;*    Last change :  Thu Jan 14 11:04:34 2010 (serrano)                */
;*    Copyright   :  2007-10 Manuel Serrano                            */
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

   (cond-expand
      (bigloo3.3a
       (export
	(class audio-server
	   (audio-server-init)
	   (%music (default #f))
	   (%hmutex read-only (default (make-mutex)))
	   (%thread (default #f))
	   (%path (default #unspecified))
	   (%service (default #unspecified))
	   (%event (default #unspecified))
	   (%errcount::int (default 0))
	   (%state::symbol (default 'init))
	   (%log::pair-nil (default '())))
	
	(audio-server-music ::audio-server)
	(audio-server-music-set! ::audio-server ::obj)))
      (else
       (export
	(class audio-server
	   (audio-server-init)
	   (music (get (lambda (o)
			  (audio-server-%music o)))
		  (set (lambda (o v)
			  (with-lock (audio-server-%hmutex o)
			     (lambda ()
				(when (music? (audio-server-%music o))
				   (music-close (audio-server-%music o)))
				(when (thread? (audio-server-%thread o))
				   (thread-terminate! (audio-server-%thread o)))
				(audio-server-%music-set! o v)
				(when (music? v)
				   (audio-server-%thread-set! o
                                      (make-audio-server-thread o v)))))))
		  (default #f))
	   (%music (default #f))
	   (%hmutex read-only (default (make-mutex)))
	   (%thread (default #f))
	   (%path (default #unspecified))
	   (%service (default #unspecified))
	   (%event (default #unspecified))
	   (%errcount::int (default 0))
	   (%state::symbol (default 'init))
	   (%log::pair-nil (default '()))))))
   
   (export  (generic audio-server-init ::audio-server)
	    
	    (<AUDIO> . args)
	    (audio-server-close ::obj)))

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
			      (onbackend #f)
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
			      (browser 'auto)
			      (server #f)
			      (attr)
			      body)

   (set! id (xml-make-id id 'hopaudio))
   (define hid (xml-make-id 'html5))
   (define fid (xml-make-id 'flash))
   (define cid (xml-make-id 'hopaudio))
   
   (define (<controls>)
      (<AUDIO:CONTROLS>
	 :id cid :audioid id
	 :onprevclick onprevclick
	 :onpauseclick onpauseclick
	 :onplayclick onplayclick
	 :onstopclick onstopclick
	 :onnextclick onnextclick
	 :onloadclick onloadclick
	 :onprefsclick onprefsclick
	 :onpodcastclick onpodcastclick
	 :onmuteclick onmuteclick))
   
   (define (<init> backendid init)
      (<AUDIO:INIT> :id id :backendid backendid
	 :init init
	 :server server
	 :src src :playlist (playlist body)
	 :autoplay autoplay :start start
	 :onplay (hop->js-callback onplay)
	 :onstop (hop->js-callback onstop)
	 :onpause (hop->js-callback onpause)
	 :onload (hop->js-callback onload)
	 :onerror (hop->js-callback onerror)
	 :onended (hop->js-callback onended)
	 :onprogress (hop->js-callback onprogress)
	 :onloadedmetadata (hop->js-callback onloadedmetadata)
	 :onclose (hop->js-callback onclose)
	 :onbackend (hop->js-callback onbackend)))

   (define (playlist suffixes)
      (filter-map (lambda (x)
		     (when (xml-markup-is? x 'source)
			(let ((src (dom-get-attribute x "src")))
			   (when (string? src)
			      (let ((type (dom-get-attribute x "type")))
				 (if type
				     (list type src)
				     src))))))
		  body))

   (<DIV> :id id :class "hop-audio"
      (when controls (<controls>))
      (case browser
	 ((flash)
	  (list (<init> fid "audio.browserbackend = browserbackend;")
		(<AUDIO:FLASH> :id fid)))
	 ((html5)
	  (list (<init> hid
			"audio.browserbackend = hop_audio_html5_init(browserbackend);")
		(<AUDIO:HTML5> :id hid)))
	 ((none)
	  (<init> fid "audio.browserbackend = false;"))
	 ((auto)
	  (list (<init> hid
			(format "if(hop_config.html5_audio ) {
                                   hop_audio_html5_init(browserbackend);
                                   audio.browserbackend = browserbackend;
                                 } else 
                                  audio.browserbackend = document.getElementById( '~a' );" fid))
		(<AUDIO:HTML5> :id hid (<AUDIO:FLASH> :id fid))))
	 (else
	  (error '<AUDIO> "Illegal backend" browser)))))

;*---------------------------------------------------------------------*/
;*    <AUDIO:HTML5> ...                                                */
;*---------------------------------------------------------------------*/
(define (<AUDIO:HTML5> #!key id  #!rest body)
   (instantiate::xml-element
      (id id)
      (markup 'AUDIO)
      (attributes '(:controls #f :autoplay #f))
      (body body)))
   
;*---------------------------------------------------------------------*/
;*    <AUDIO:FLASH> ...                                                */
;*---------------------------------------------------------------------*/
(define (<AUDIO:FLASH> #!key id)
   (let* ((swf (make-file-path (hop-share-directory) "flash" "HopAudio.swf"))
	  (init (string-append "hop_audio_flash_init_" id))
	  (fvar (string-append "arg=" init)))
      (<DIV> :id id
	 (<SCRIPT>
	    (format "function ~a() {" init)
	    (format "var backend = document.getElementById( ~s );" id)
	    "if( backend ) hop_audio_flash_init( backend );}")
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
	       :FlashVars fvar)))))

;*---------------------------------------------------------------------*/
;*    <AUDIO:INIT> ...                                                 */
;*---------------------------------------------------------------------*/
(define (<AUDIO:INIT> #!key id backendid
		      server
		      init
		      src playlist
		      autoplay start
		      onplay onstop onpause onload onerror onended onprogress
		      onloaded onloadedmetadata onclose onbackend)
   (<SCRIPT>
      (format "hop_audio_init[ '~a' ] = function (_) {" id)
      (format "var audio = document.getElementById( '~a' );" id)
      "if( audio.browserbackend ) return;"
      (format "var browserbackend = document.getElementById( '~a' );" backendid)
      init
      (format "if( audio.browserbackend ) audio.browserbackend.audio = audio;")
      (if server
	  (format "audio.serverbackend = new HopAudioServerBackend( audio, ~a )
                   hop_audio_server_init( audio.serverbackend );
                   audio.backend = audio.serverbackend;"
		  (hop->json server #f #f))
	  "audio.serverbackend = false; audio.backend = audio.browserbackend;")
      "audio.paused = false;"
      "audio.state = false;"
      (format "audio.src = ~a;" (hop->json src #f #f))
      "audio.initialized = true;"
      (format "audio.start = ~a;" start)
      (format "audio.onplay = ~a;" onplay)
      (format "audio.onstop = ~a;" onstop)
      (format "audio.onpause = ~a;" onpause)
      (format "audio.onload = ~a;" onload)
      (format "audio.onerror = ~a;" onerror)
      (format "audio.onended = ~a;" onended)
      (format "audio.onprogress = ~a;" onprogress)
      (format "audio.onloadedmetadata = ~a;" onloadedmetadata)
      (format "audio.onclose = ~a;" onclose)
      (format "audio.onbackend = ~a;" onbackend)
      "audio.hop_add_event_listener = hop_audio_add_event_listener;"
      "audio.toString = function() { return '[object HopAudio]' };"
      (when (pair? playlist)
	 (format "hop_audio_playlist_set( audio, ~a );" (hop->json (list playlist) #f #f)))
      (when autoplay
	 "hop_audio_playlist_play( audio, 0 );")
      "};\n"
      (format "hop_add_event_listener( window, 'load', hop_audio_init[ '~a' ] );" id)))

;*---------------------------------------------------------------------*/
;*    <AUDIO:CONTROLS> ...                                             */
;*---------------------------------------------------------------------*/
(define (<AUDIO:CONTROLS> #!key id audioid
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

   (<DIV> :id id :class "hop-audio-controls"
      ;; the controls callbacks
      (<SCRIPT>
	 "hop_add_event_listener( window, 'load', function(_) {"
	 (format "var audio = document.getElementById(~s);" audioid)
	 (format "var controls = document.getElementById(~s);" id)
	 "audio.controls = controls; controls.audio = audio;"
	 (format "hop_audio_controls_listeners_init( ~s );" id)
	 "})")
      ;; the info line
      (<TABLE> :class "hop-audio-panel"
	 (<TR>
	    (<TD> :class "hop-audio-panel"
	       (<AUDIO:CONTROLS-STATUS> id))
	    (<TD> :class "hop-audio-panel"
	       (<DIV> :class "hop-audio-panel2"
		  (<AUDIO:CONTROLS-METADATA> id)
		  (<AUDIO:CONTROLS-SOUND> id)))))
      ;; separator
      (<DIV> :class "hop-audio-separator")
      ;; the button line
      (<DIV> :class "hop-audio-buttons"
	 (<BUT> :title "Previous"
	    :class "hop-audio-button-prev"
	    :onclick (if (eq? onprevclick #unspecified)
			 (format "hop_audio_playlist_prev(document.getElementById('~a'))" audioid)
			 onprevclick))
	 (<BUT> :title "Play"
	    :id (string-append id "-hop-audio-button-play")
	    :class "hop-audio-button-play"
	    :onclick (if (eq? onplayclick #unspecified)
			 (format "hop_audio_playlist_play(document.getElementById('~a'), 0)" audioid)
			 onplayclick))
	 (<BUT> :title "Pause"
	    :id (string-append id "-hop-audio-button-pause")
	    :class "hop-audio-button-pause"
	    :onclick (if (eq? onpauseclick #unspecified)
			 (format "hop_audio_pause(document.getElementById('~a'))" audioid)
			 onpauseclick))
	 (<BUT> :title "Stop"
	    :id (string-append id "-hop-audio-button-stop")
	    :class "hop-audio-button-stop"
	    :onclick (if (eq? onstopclick #unspecified)
			 (format "hop_audio_stop(document.getElementById('~a'))" audioid)
			 onstopclick))
	 (<BUT> :title "Next"
	    :class "hop-audio-button-next"
	    :onclick (if (eq? onnextclick #unspecified)
			 (format "hop_audio_playlist_next(document.getElementById('~a'))" audioid)
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
			 (format "hop_audio_mute(document.getElementById('~a'))" audioid)
			 onmuteclick))
	 (<BUT> :title "Preferences"
	    :id (string-append id "-hop-audio-button-prefs")
	    :class "hop-audio-button-prefs"
	    :onclick onprefsclick))))

;*---------------------------------------------------------------------*/
;*    <AUDIO:CONTROLS-STATUS> ...                                      */
;*---------------------------------------------------------------------*/
(define (<AUDIO:CONTROLS-STATUS> id)
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
;*    <AUDIO:CONTROLS-METADATA> ...                                    */
;*---------------------------------------------------------------------*/
(define (<AUDIO:CONTROLS-METADATA> id)
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
;*    <AUDIO:CONTROLS-SOUND> ...                                       */
;*---------------------------------------------------------------------*/
(define (<AUDIO:CONTROLS-SOUND> id)
   
   (define (on action)
      (format "var el = document.getElementById( '~a' );
               var evt = new HopAudioEvent();

               evt.value = this.value;
               if( el.on~aclick ) el.on~aclick( evt );
	       if( !evt.stopped ) hop_audio_~a( el.audio, this.value );"
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
;*    audio-server-init ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (audio-server-init as::audio-server)
   (let* ((p (gen-service-url "audio"))
	  (s (service :name p (a0 a1)
		(with-handler
		   (lambda (e)
		      (error-notify e)
		      #f)
		   (with-access::audio-server as (%music %hmutex %state)
		      (with-lock %hmutex
			 (lambda ()
			    (when (eq? %state 'ready)
			       (case a0
				  ((volume)
				   (music-volume-set! %music a1)
				   #t)
				  ((load)
				   (music-playlist-clear! %music)
				   (hop-music-playlist-add! %music a1)
				   #t)
				  ((pause)
				   (music-pause %music)
				   #t)
				  ((play)
				   (music-play %music a1)
				   #t)
				  ((stop)
				   (music-stop %music)
				   #t)
				  ((position)
				   (music-seek %music a1)
				   #t)
				  ((playlist)
				   (music-playlist-set! %music a1)
				   #t)
				  ((status)
				   (audio-status-event-value
				    (music-status %music)))
				  (else
				   (tprint "unknown msg..." a0)
				   #f))))))))))
      (cond-expand
	 (enable-threads
	  (with-access::audio-server as (%service %music %thread %path %event)
	     (set! %path p)
	     (set! %event (string-append (hop-service-base) "/" (audio-server-%path as)))
	     (set! %service s)))
	 (else
	  (error 'audio-server
	     "Backend cannot be started in single-thread setting"
	     "Re-configure HOP with multi-threading enabled")))))

;*---------------------------------------------------------------------*/
;*    audio-server-close ...                                           */
;*---------------------------------------------------------------------*/
(define (audio-server-close as)
   (with-access::audio-server as (%hmutex %state %thread %music)
      (with-lock %hmutex
	 (lambda ()
	    (set! %state 'close)
	    (when (music? %music) (music-close %music))
	    (when (thread? %thread) (thread-terminate! %thread))))))

;*---------------------------------------------------------------------*/
;*    audio-server-status ...                                          */
;*---------------------------------------------------------------------*/
(define (audio-server-status as)
   (with-access::audio-server as (%music)
      (audio-status-event-value (music-status %music))))

;*---------------------------------------------------------------------*/
;*    bigloo3.3a workaround                                            */
;*---------------------------------------------------------------------*/
(cond-expand
   (bigloo3.3a
    
    (define (audio-server-music as)
       (with-access::audio-server as (%music)
	  %music))
    
    (define (audio-server-music-set! as m)
       (with-access::audio-server as (%hmutex %state %thread %music)
	  (with-lock %hmutex
	     (lambda ()
		(when (music? %music) (music-close %music))
		(when (thread? %thread) (thread-terminate! %thread))
		(audio-server-%music-set! as m)
		(when (music? m)
		   (set! %thread (make-audio-server-thread as m)))))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (music-playlist-set! music a1)
   (music-playlist-clear! music)
   (for-each (lambda (s)
		(let ((f (charset-convert s (hop-charset) 'UTF-8)))
		   (music-playlist-add! music f)))
	     a1))

;*---------------------------------------------------------------------*/
;*    make-audio-server-thread ...                                     */
;*---------------------------------------------------------------------*/
(define (make-audio-server-thread as mu)
   
   (define (thread-body)
      (with-access::audio-server as (%music %event %errcount %state)
	 (let ((onstate (audio-onstate %event %music as))
	       (onerror (audio-onerror %event %music))
	       (onvolume (audio-onvolume %event %music))
	       (onmeta (audio-onmeta %event %music as)))
	    (with-handler
	       (lambda (e)
		  (exception-notify e)
		  (let ((msg (with-error-to-string
				(lambda ()
				   (exception-notify e)))))
		     (set! %state 'error)
		     (onerror msg)))
	       (set! %state 'ready)
	       (music-event-loop %music
		  :onstate onstate
		  :onerror onerror
		  :onvolume onvolume
		  :onmeta onmeta)))))
   
   (define (thread-cleanup th)
      (with-access::audio-server as (%event %state %thread %music)
	 (with-access::musicstatus (music-status %music) (err)
	    (if (and err (eq? %music mu) (not (music-closed? mu)))
	       (hop-event-broadcast! %event (list 'abort err))
	       (begin
		  (set! %state 'close)
		  (hop-event-broadcast! %event (list 'close)))))
	 (set! %thread #f)))
   
   (cond-expand
      (enable-threads
       (let ((th (instantiate::pthread
		    (body thread-body)
		    (cleanup thread-cleanup))))
	  (thread-start! th)
	  th))
      (else
       (error 'make-audio-server-thread
	  "Backend cannot be started in single-thread setting"
	  "Re-configure HOP with multi-threading enabled"))))

;*---------------------------------------------------------------------*/
;*    audio-status-event-value ...                                     */
;*---------------------------------------------------------------------*/
(define (audio-status-event-value status)
   (with-access::musicstatus status (state song songpos songlength volume)
      (list state songlength songpos volume song)))

;*---------------------------------------------------------------------*/
;*    audio-onstate ...                                                */
;*---------------------------------------------------------------------*/
(define (audio-onstate event music as)
   (lambda (status)
      (with-trace 3 "audio-onstate"
	 (trace-item "music=" (find-runtime-type music))
	 (trace-item "as=" (find-runtime-type as))
	 (let ((ev (audio-status-event-value status)))
	    (audio-server-%errcount-set! as 0)
	    (hop-event-broadcast! event ev)))))

;*---------------------------------------------------------------------*/
;*    audio-onmeta ...                                                 */
;*---------------------------------------------------------------------*/
(define (audio-onmeta event music as)

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
	 (hop-event-broadcast! event (list 'meta s plist))))
   
   (define (audio-onfile-name meta plist)
      (let ((url (url-decode meta)))
	 (signal-meta url plist)))
   
   (lambda (meta playlist)
      (with-trace 3 "audio-onmeta"
	 (trace-item "music=" (find-runtime-type music))
	 (trace-item "as=" (find-runtime-type as))
	 (audio-server-%errcount-set! as 0)
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
(define (audio-onerror event music)
   (lambda (error)
      (with-trace 3 "audio-onerror"
	 (trace-item "music=" (find-runtime-type music))
	 (hop-event-broadcast! event (list 'error error)))))

;*---------------------------------------------------------------------*/
;*    audio-onvolume ...                                               */
;*---------------------------------------------------------------------*/
(define (audio-onvolume event music)
   (lambda (vol)
      (with-trace 3 "audio-onvolume"
	 (trace-item "music=" (find-runtime-type music))
	 (hop-event-broadcast! event (list 'volume vol)))))

;*---------------------------------------------------------------------*/
;*    hop-music-playlist-add! ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-music-playlist-add! music s)
   (music-playlist-add! music (charset-convert s (hop-charset) 'UTF-8)))
   
;*---------------------------------------------------------------------*/
;*    hop->json ::%audio-server ...                                    */
;*---------------------------------------------------------------------*/
(define-method (hop->json as::audio-server isrep isflash)
   (string-append "\"" (hop-service-base) "/" (audio-server-%path as) "\""))
