/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-audio.js                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Aug 21 13:48:47 2007                          */
/*    Last change :  Thu Nov 29 07:18:14 2007 (serrano)                */
/*    Copyright   :  2007 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    HOP client-side audio support.                                   */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    HopAudioEvent ...                                                */
/*---------------------------------------------------------------------*/
function HopAudioEvent( name, audio ) {
   var o = new Object();
   o.isStopped = false;
   o.name = name;
   o.audio = audio;
   
   return o;
}

/*---------------------------------------------------------------------*/
/*    HopAudioProxy ...                                                */
/*---------------------------------------------------------------------*/
function HopAudioProxy() {
   var o = new Object();

   o.load = function( src, stream ) { return true; };
   o.play = function( start ) { return true; };
   o.playlist_play = function( index ) { return true; };
   o.playlist_set = function( pl, autorun ) { return true; };
   o.playlist_get = function() { return NULL; };
   o.stop = function() { return true; };
   o.pause = function() { return true; };
   o.position_get = function() { return true; };
   o.position_set = function() { return true; };
   o.pan_get = function() { return true; };
   o.pan_set = function() { return true; };
   o.volume_get = function() { return true; };
   o.volume_set = function() { return true; };
   o.duration_get = function() { return true; };
   o.metadata_get = function() { return true; };
}

/*---------------------------------------------------------------------*/
/*    pre-allocated symbols                                            */
/*---------------------------------------------------------------------*/
var Splay = sc_string2symbol_immutable( "play" );
var Spause = sc_string2symbol_immutable( "pause" );
var Sstop = sc_string2symbol_immutable( "stop" );
var Svolume = sc_string2symbol_immutable( "volume" );
var Sload = sc_string2symbol_immutable( "load" );
var Sinfo = sc_string2symbol_immutable( "info" );
var Sready = sc_string2symbol_immutable( "ready" );
var Sposition = sc_string2symbol_immutable( "position" );
var Span = sc_string2symbol_immutable( "pan" );
var Smeta = sc_string2symbol_immutable( "meta" );
var Splaylist = sc_string2symbol_immutable( "playlist" );
var Serror = sc_string2symbol_immutable( "error" );

/*---------------------------------------------------------------------*/
/*    HopAudioServerProxy ...                                          */
/*---------------------------------------------------------------------*/
function HopAudioServerProxy( audio, url ) {
   var o = new HopAudioProxy();
   var current_duration = false;
   var current_position = 0;
   var current_volume = 0;
   var current_pan = 0;
   var current_metadata = false;
   var playlist = null;
   var playlist_index = -1;

   var err = function( h ) { if( !h ) hop_audio_run_hooks( audio, "error" ) };
   
   o.playlist_play = function( index ) {
      with_hop( hop_service_url_varargs( url, Splay, index ), err );
   }
   
   o.play = function( start ) {
      with_hop( hop_service_url_varargs( url, Splay, 0 ), err );
   };
   o.stop = function() {
      with_hop( hop_service_url_varargs( url, Sstop ), err );
   };
   o.pause = function() {
      with_hop( hop_service_url_varargs( url, Spause ), err );
   };
   o.load = function( l, s ) {
      with_hop( hop_service_url_varargs( url, Sload, l ),
		function( h ) {
		   if( !h ) {
		      err( h );
		   } else {
		      if( s ) {
			 with_hop( hop_service_url_varargs( url, Splay, 0 ), err );
		      }
		      hop_audio_run_hooks( audio, "load" );
		   }
		} );
   };
   o.position_get = function() {
      return current_position;
   };
   o.position_set = function( p ) {
      with_hop( hop_service_url_varargs( url, Sposition, p ), err );
   };
   o.pan_get = function() {
      return current_pan;
   };
   o.pan_set = function() {
      with_hop( hop_service_url_varargs( url, Span, s ), err );
   };
   o.volume_get = function() {
      return current_volume;
   };
   o.volume_set = function( val ) {
      with_hop( hop_service_url_varargs( url, Svolume, val ), err );
   };
   o.duration_get = function() {
      return current_duration;
   };
   o.metadata_get = function() {
      return current_metadata;
   };
   o.playlist_get = function() {
      return playlist;
   }
   o.playlist_set = function( playlist, autorun ) {
      with_hop( hop_service_url_varargs( url, Splaylist, playlist ),
		autorun ? function( h ) { o.playlist_play( 0 ) } : false );
   }
   o.playlist_index_get = function() {
      return playlist_index;
   }

   o.event = url;
   o.event_listener = function( e ) {
      if( sc_isPair( e.value ) ) {
	 var k = e.value.car;
	 var rest = e.value.cdr;
	    
	 if( k === Splay ) {
	    // play
	    audio.paused = false;
	    current_duration = rest.car;
	    current_position = rest.cdr.car;
	    current_volume = rest.cdr.cdr.car;
	    hop_audio_run_hooks( audio, "play" );
	    hop_audio_run_hooks( audio, "volume" );
	 } else if( k == Spause ) {
	    // pause
	    audio.paused = true;
	    current_duration = rest.car;
	    current_volume = rest.cdr.cdr.car;
	    hop_audio_run_hooks( audio, "pause" );
	    hop_audio_run_hooks( audio, "volume" );
	 } if( k == Sstop ) {
	    // stop
	    audio.paused = false;
	    current_duration = rest.car;
	    current_volume = rest.cdr.cdr.car;
	    hop_audio_run_hooks( audio, "stop" );
	    hop_audio_run_hooks( audio, "volume" );
	 } else if( k == Serror ) {
	    // error
	    hop_audio_run_hooks( audio, "error", rest.car );
	 } else if( k == Smeta ) {
	    // meta (and playlist)
	    var state  = rest.car;
	    current_duration = rest.cdr.car;
	    current_position = rest.cdr.cdr.car;

	    rest = rest.cdr.cdr.cdr;
	    
	    var val = rest.car;
	    var plist = rest.cdr.car;
	    var plindex = rest.cdr.cdr.car;

	    current_volume = rest.cdr.cdr.cdr.car;
	    hop_audio_run_hooks( audio, "volume" );
	    
	    audio.paused = ( state == Spause );
	    
	    playlist = plist;
	    playlist_index = plindex;
	    
	    if( typeof val == "string" ) {
	       current_metadata = {
	       title: sc_basename( val ),
	       artist: sc_basename( sc_dirname( sc_dirname( val ) ) ),
	       album: sc_basename( sc_dirname( val ) )
	       };
	    } else if( val ) {
	       current_metadata = val;
	    }

	    hop_audio_run_hooks( audio, sc_symbol2string_immutable( state ) );
	 }
      } else {
	 if( !e.value ) {
	    // the server has closed the connection, act as a plain stop action
	    hop_audio_run_hooks( audio, "stop" );
	 }
      }
      
      return;
   }

   // install the server listener...
   hop_add_event_listener( o.event, "server", o.event_listener );

   // ... and starts the server event notification
   hop_add_event_listener( document, "serverready", function() {
	 with_hop( hop_service_url_varargs( url, Sready ) );
      } );
   
   return o;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_add_event_listener ...                                 */
/*---------------------------------------------------------------------*/
function hop_audio_add_event_listener( event, proc, capture ) {
   audio[ "on" + event ] = proc;
   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_init_obj ...                                           */
/*---------------------------------------------------------------------*/
function hop_audio_init_obj( audio ) {
   if( !audio.obj ) {
      audio.obj = document.getElementById( (hop_msiep() ? "object-":"embed-")
					   + audio.id );
   }

   return audio.obj;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_init ...                                               */
/*---------------------------------------------------------------------*/
function hop_audio_init( id, start, src, stream,
			 onplay, onstop, onpause, onload, onerror,
			 onended, onprogress ) {
   var audio = document.getElementById( id );

   hop_audio_init_obj( audio );
   
   audio.controls = document.getElementById( "controls-" + id );

   audio.playlist = null;
   audio.start = start;
   
   audio.onplay = onplay;
   audio.onstop = onstop;
   audio.onpause = onpause;
   audio.onload = onload;
   audio.onerror = onerror;
   audio.onended = onended;
   audio.onprogress = onprogress;
   audio.initialized = true;
   
   audio.hop_add_event_listener = hop_audio_add_event_listener;
   audio.toString = function() { return "[object Audio]"; };
   if( !audio.proxy ) {
      // create a proxy only if flash initialization has not occured yet
      audio.proxy = new HopAudioProxy();
   } else {
      if( src ) hop_audio_load( audio, src, stream );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_flash_init ...                                         */
/*    -------------------------------------------------------------    */
/*    This function is automatically invoked by the Flash script when  */
/*    the builtin initialization completes.                            */
/*---------------------------------------------------------------------*/
function hop_audio_flash_init( id, src, stream, player ) {
   /* we are now sure that at least version 8 of flash is running */
   hop_flash_minversion_set( 8 );

   var audio = document.getElementById( id );
   var proxy = hop_audio_init_obj( audio );

   audio.proxy = proxy;
   audio.client_proxy = proxy;

   proxy.playlist = null;
   proxy.playlist_index = 0;
   
   proxy.play = function( start ) {
      proxy.flash_play( start );
      hop_audio_run_hooks( audio, "play" );
   }
   proxy.stop = function() {
      proxy.flash_stop();
      hop_audio_run_hooks( audio, "stop" );
   }
   proxy.pause = function() {
      proxy.flash_pause();
      hop_audio_run_hooks( audio, "pause" );
   }
   
   proxy.playlist_play = function( index ) {
      proxy.playlist_index = index;
      hop_audio_load( audio, sc_listRef( proxy.playlist, index ), true );
   }
   proxy.playlist_set = function( playlist, autorun ) {
      proxy.playlist = playlist;
      if( autorun ) proxy.playlist_play( 0 );
   }
   proxy.playlist_get = function() {
      return proxy.playlist;
   }
   proxy.playlist_index_get = function() {
      return proxy.playlist_index;
   }
   
   proxy.onload_set( "hop_audio_onload_callback", id );
   proxy.onerror_set( "hop_audio_onerror_callback", id );
   proxy.onended_set( "hop_audio_onended_callback", id );
   proxy.metadata_get = function() {
      try {
	 return eval( audio.proxy.id3_get() );
      } catch( _ ) {
	 return false;
      }
   }
   if( src && audio.initialized ) {
      hop_audio_load( audio, src, stream );
   }

   if( player ) {
      hop_audio_player_set( audio, player, "server" );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_run_hooks ...                                          */
/*---------------------------------------------------------------------*/
function hop_audio_run_hooks( audio, evname, value ) {
   var evt = new HopAudioEvent( evname, audio );
   var handler = "on" + evname;
   evt.value = value;

   if( audio[ handler ] )
      audio[ handler ]( evt );
   if( !evt.isStopped && audio.controls && audio.controls[ handler ] )
      audio.controls[ handler ]( evt );
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_player_set ...                                         */
/*---------------------------------------------------------------------*/
function hop_audio_player_set( audio, player, name ) {
   /* cleanup the current proxy */
   if( audio.server_proxy ) {
      hop_remove_event_listener( audio.server_proxy.event,
				 "server",
				 audio.server_proxy.event_listener );
   }
   /* install the new proxy */
   if( !player ) {
      audio.proxy = audio.client_proxy;
   } else {
      var proxy = new HopAudioServerProxy( audio, player );
      audio.proxy = proxy;
      audio.server_proxy = proxy;

      hop_audio_run_hooks( audio, "player", name );
   }
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_onload_callback ...                                    */
/*---------------------------------------------------------------------*/
function hop_audio_onload_callback( id, play ) {
   var audio = document.getElementById( id );
   var evt = new HopAudioEvent( "load", audio );

   if( audio.onload )
      audio.onload( evt );
   if( !evt.isStopped && play && audio.onplay )
      audio.onplay( evt );
   if( !evt.isStopped && audio.controls && audio.controls.onload )
      audio.controls.onload( evt );
   if( !evt.isStopped && play && audio.controls && audio.controls.onplay )
      audio.controls.onplay( evt );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_onerror_callback ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_onerror_callback( id, play ) {
   var audio = document.getElementById( id );
   var evt = new HopAudioEvent( "error", audio );

   if( audio.onerror )
      audio.onerror( evt );
   if( !evt.isStopped && audio.controls && audio.controls.onerror )
      audio.controls.onerror( evt );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_onended_callback ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_onended_callback( id ) {
   var audio = document.getElementById( id );
   var evt = new HopAudioEvent( "ended", audio );

   if( audio.onended )
      audio.onended( evt );
   if( !evt.isStopped && audio.controls && audio.controls.onended)
      audio.controls.onended( evt );
   if( !evt.isStopped &&
       (hop_audio_playlist_index( audio ) ||
	(hop_audio_playlist_index( audio ) == 0)) )
      hop_audio_playlist_play( audio, hop_audio_playlist_index( audio ) + 1 );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_load ...                                               */
/*---------------------------------------------------------------------*/
function hop_audio_load( audio, src, stream ) {
   audio.src = src;
   audio.playlist_index = false;
   audio.paused = false;
   hop_audio_run_hooks( audio, "progress" );

   return audio.proxy.load( src, stream );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_set ...                                       */
/*---------------------------------------------------------------------*/
function hop_audio_playlist_set( audio, playlist, autorun ) {
   audio.proxy.playlist_set( playlist, autorun );
   return;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_get ...                                       */
/*---------------------------------------------------------------------*/
function hop_audio_playlist_get( audio ) {
   return audio.proxy.playlist_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_index ...                                     */
/*---------------------------------------------------------------------*/
function hop_audio_playlist_index( audio ) {
   return audio.proxy.playlist_index_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_play ...                                      */
/*---------------------------------------------------------------------*/
function hop_audio_playlist_play( audio, i ) {
   var pl = hop_audio_playlist_get( audio );

   if( (i >= 0) && (i < sc_length( pl )) ) {
      audio.proxy.playlist_play( i );
      return i;
   }

   return -1;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_prev ...                                      */
/*---------------------------------------------------------------------*/
function hop_audio_playlist_prev( audio ) {
   var pl = hop_audio_playlist_get( audio );
   var i = hop_audio_playlist_index( audio );

   if( i > 0 ) {
      if( hop_audio_controls_time( audio ) <= 3 ) {
	 return hop_audio_playlist_play( audio, i - 1 );
      } else {
	 return hop_audio_seek( audio, 0 );
      }
   }
   
   if( i == 0 ) {
      return hop_audio_seek( audio, 0 );
   }
   
   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_next ...                                      */
/*---------------------------------------------------------------------*/
function hop_audio_playlist_next( audio ) {
   var pl = hop_audio_playlist_get( audio );
   var i = hop_audio_playlist_index( audio );

   if( i < (sc_length( pl ) - 1)) {
      return hop_audio_playlist_play( audio, i + 1 );
   }

   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_play ...                                               */
/*---------------------------------------------------------------------*/
function hop_audio_play( audio, start ) {
   audio.playlist_index = false;
   audio.paused = false;
   audio.proxy.play( start ? start : audio.start );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_stop ...                                               */
/*---------------------------------------------------------------------*/
function hop_audio_stop( audio ) {
   audio.proxy.stop();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_pause ...                                              */
/*---------------------------------------------------------------------*/
function hop_audio_pause( audio ) {
   audio.paused = !audio.paused;
   audio.proxy.pause();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_volume ...                                             */
/*---------------------------------------------------------------------*/
function hop_audio_volume( audio ) {
   return audio.proxy.volume_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_volume_set ...                                         */
/*---------------------------------------------------------------------*/
function hop_audio_volume_set( audio, vol ) {
   return audio.proxy.volume_set( vol );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_mute ...                                               */
/*---------------------------------------------------------------------*/
function hop_audio_mute( audio ) {
   var img = document.getElementById( "hop-audio-button-mute-" + audio.id );
   
   if( audio.mute_volume ) {
      var vol = audio.mute_volume;
      img.src = hop_share_directory() + "/icons/hop-audio/mute.png";
      audio.mute_volume = false;
      return hop_audio_volume_set( audio, vol );
   } else {
      img.src = hop_share_directory() + "/icons/hop-audio/unmute.png";
      audio.mute_volume = hop_audio_volume( audio );
      return hop_audio_volume_set( audio, 0 );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_pan ...                                                */
/*---------------------------------------------------------------------*/
function hop_audio_pan( audio ) {
   return audio.proxy.pan_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_pan_set ...                                            */
/*---------------------------------------------------------------------*/
function hop_audio_pan_set( audio, pan ) {
   return audio.proxy.pan_set( pan );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_duration ...                                           */
/*---------------------------------------------------------------------*/
function hop_audio_duration( audio ) {
   return audio.proxy.duration_get();
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_current_time ...                                       */
/*---------------------------------------------------------------------*/
function hop_audio_current_time( audio ) {
   return audio.proxy.position_get();
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_seek ...                                               */
/*---------------------------------------------------------------------*/
function hop_audio_seek( audio, pos ) {
   audio.min = Math.floor( pos / 60 );
   audio.sec = pos % 60;
   audio.ctime = pos;
   
   return audio.proxy.position_set( pos );
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_metadata ...                                           */
/*---------------------------------------------------------------------*/
function hop_audio_metadata( audio ) {
   return audio.proxy.metadata_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_metadata_set ...                                       */
/*---------------------------------------------------------------------*/
function hop_audio_metadata_set( audio, metadata ) {
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onload ...                                    */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onload( evt ) {
   var audio = evt.audio;
   var tl = document.getElementById( "controls-metadata-song-" + audio.id );

   hop_audio_controls_metadata( audio, true );
   tl.innerHTML = "Ready...";
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onerror ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onerror( evt ) {
   var audio = evt.audio;
   var tl = document.getElementById( "controls-metadata-song-" + audio.id );
   var min = document.getElementById( "controls-status-length-min-" + audio.id );
   var sec = document.getElementById( "controls-status-length-sec-" + audio.id );

   tl.innerHTML = evt.value ? evt.value : "Error";
   min.innerHTML = "  ";
   sec.innerHTML = "  ";

   hop_audio_time_interval_clear( audio );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onprogress ...                                */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onprogress( evt ) {
   var audio = evt.audio;
   var tl = document.getElementById( "controls-metadata-song-" + audio.id );

   hop_audio_controls_metadata( audio, true );
   tl.innerHTML = "Buffering...";
   audio.min = 0;
   audio.sec = 0;
   audio.ctime = 0;
}

/*---------------------------------------------------------------------*/
/*    int2 ...                                                         */
/*---------------------------------------------------------------------*/
function int2( i ) {
   if( i < 10 )
      return "0" + i;
   else
      return i;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_time ...                                      */
/*---------------------------------------------------------------------*/
function hop_audio_controls_time( audio ) {
   return audio.ctime;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_time_interval_set ...                                  */
/*---------------------------------------------------------------------*/
function hop_audio_time_interval_set( audio ) {
   var id = audio.id;
   var pos = document.getElementById( "controls-status-position-" + id );
   var ctime = hop_audio_current_time( audio );
   
   audio.ctime = ctime;
   audio.min = Math.floor( ctime / 60 );
   audio.sec = ctime % 60;

   hop_audio_time_interval_clear( audio );
   pos.innerHTML = int2( audio.min ) + ":" + int2( audio.sec );
   
   audio.interval = setInterval( function() {
	 if( !audio.paused ) {
	    audio.ctime++;
	    audio.sec++;
	    if( audio.sec == 60 ) { audio.min++; audio.sec = 0 };
	    pos.innerHTML = int2( audio.min ) + ":" + int2( audio.sec );
	 }
      }, 1000 );
   
   pos.className = "hop-audio-info-status-position-on-play";
}

/*---------------------------------------------------------------------*/
/*    hop_audio_time_interval_clear ...                                */
/*---------------------------------------------------------------------*/
function hop_audio_time_interval_clear( audio ) {
   var id = audio.id;
   var pos = document.getElementById( "controls-status-position-" + id );
   pos.className = "hop-audio-info-status-position";
   pos.innerHTML = "88:88";
   
   if( audio.interval ) {
      clearInterval( audio.interval );
      audio.interval = false;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_metadata ...                                  */
/*---------------------------------------------------------------------*/
function hop_audio_controls_metadata( audio, reset ) {
   var tl = document.getElementById( "controls-metadata-song-" + audio.id );
   var ab = document.getElementById( "controls-metadata-album-" + audio.id );
   var at = document.getElementById( "controls-metadata-artist-" + audio.id );
   var ye = document.getElementById( "controls-metadata-year-" + audio.id );

   if( reset ) {
      tl.innerHTML = "";
      ab.innerHTML = "";
      at.innerHTML = "";
      ye.innerHTML = "";
   } else {
      var md = audio.proxy.metadata_get();

      if( md ) {
	 tl.innerHTML = md.title;
	 at.innerHTML = md.artist;
	 ab.innerHTML = md.album;
	 if( md.year && md.year > 0 )
	    ye.innerHTML = md.year;
	 else
	    ye.innerHTML = "";
      } else {
	 if( audio.src ) {
	    tl.innerHTML = sc_basename( audio.src );
	    at.innerHTML = sc_basename( sc_dirname( sc_dirname( audio.src ) ) );
	    ab.innerHTML = sc_basename( sc_dirname( audio.src ) );
	    if( md.year && md.year > 0 )
	       ye.innerHTML = md.year;
	    else
	       ye.innerHTML = "";
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onplay ...                                    */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onplay( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var status = document.getElementById( "controls-status-img-" + id );
   var playbut = document.getElementById( "hop-audio-button-play-" + id );
   var track = document.getElementById( "controls-status-track-" + id );
   var min = document.getElementById( "controls-status-length-min-" + id );
   var sec = document.getElementById( "controls-status-length-sec-" + id );
   var alen = hop_audio_duration( audio );
   var plen = sc_length( hop_audio_playlist_get( audio ) );

   if( alen > 0 ) {
      status.src = playbut.src;

      track.className = "hop-audio-info-status-track-on-play";
      if( plen > 0 ) {
	 track.innerHTML = int2( 1 + hop_audio_playlist_index( audio ) )
	    + "/" + int2( plen );
      } else {
	 track.innerHTML = "01/01";
      }

      min.innerHTML = int2( Math.floor( alen / 60 ) );
      sec.innerHTML = int2( alen % 60 );

      hop_audio_controls_metadata( audio, false );
      hop_audio_time_interval_set( audio );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onpause ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onpause( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var status = document.getElementById( "controls-status-img-" + id );
   var pausebut = document.getElementById( "hop-audio-button-pause-" + id );

   if( audio.paused ) {
      audio.status_old_src = status.src;
      status.src = pausebut.src;
   } else {
      status.src = audio.status_old_src;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onstop ...                                    */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onstop( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var status = document.getElementById( "controls-status-img-" + id );
   var stopbut = document.getElementById( "hop-audio-button-stop-" + id );
   var track = document.getElementById( "controls-status-track-" + id );
   var min = document.getElementById( "controls-status-length-min-" + id );
   var sec = document.getElementById( "controls-status-length-sec-" + id );

   track.className = "hop-audio-info-status-track";
   track.innerHTML = "88888";

   min.innerHTML = "  ";
   sec.innerHTML = "  ";

   hop_audio_time_interval_clear( audio );
   hop_audio_controls_metadata( audio, true );
   status.src = stopbut.src;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onplayer ...                                  */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onplayer( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var status = document.getElementById( "controls-status-img-" + id );
   var stopbut = document.getElementById( "hop-audio-button-stop-" + id );
   var track = document.getElementById( "controls-status-track-" + id );
   var min = document.getElementById( "controls-status-length-min-" + id );
   var sec = document.getElementById( "controls-status-length-sec-" + id );
   var tl = document.getElementById( "controls-metadata-song-" + audio.id );

   track.className = "hop-audio-info-status-track";
   track.innerHTML = "88888";

   min.innerHTML = "  ";
   sec.innerHTML = "  ";

   hop_audio_time_interval_clear( audio );
   hop_audio_controls_metadata( audio, true );

   tl.innerHTML = evt.value ?
      evt.value + " initialized..." :
      "Player initialized...";
   
   status.src = stopbut.src;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onended ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onended( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var status = document.getElementById( "controls-status-" + id );
   var track = document.getElementById( "controls-status-track-" + id );
   var min = document.getElementById( "controls-status-length-min-" + id );
   var sec = document.getElementById( "controls-status-length-sec-" + id );

   min.innerHTML = "  ";
   sec.innerHTML = "  ";

   hop_audio_time_interval_clear( audio );
   hop_audio_controls_metadata( audio, true );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onvolume ...                                  */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onvolume( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var slider = document.getElementById( "controls-volume-" + id );

   hop_slider_value_set( slider, hop_audio_volume( audio ) );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_gui_playlist ...                                       */
/*---------------------------------------------------------------------*/
function hop_audio_gui_playlist( id ) {
}
