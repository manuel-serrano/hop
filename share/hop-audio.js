/*=====================================================================*/
/*    serrano/prgm/project/hop/1.9.x/share/hop-audio.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Aug 21 13:48:47 2007                          */
/*    Last change :  Sun Apr 27 08:36:18 2008 (serrano)                */
/*    Copyright   :  2007-08 Manuel Serrano                            */
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
   o.close = function() { return true; };
   o.pause = function() { return true; };
   o.position_get = function() { return 0; };
   o.position_set = function() { return true; };
   o.pan_get = function() { return true; };
   o.pan_set = function() { return true; };
   o.volume_get = function() { return 0; };
   o.volume_set = function() { return true; };
   o.duration_get = function() { return 0; };
   o.metadata_get = function() { return true; };
}

/*---------------------------------------------------------------------*/
/*    pre-allocated symbols                                            */
/*---------------------------------------------------------------------*/
var Splay = sc_jsstring2symbol( "play" );
var Sstart = sc_jsstring2symbol( "start" );
var Spause = sc_jsstring2symbol( "pause" );
var Sstop = sc_jsstring2symbol( "stop" );
var Sclose = sc_jsstring2symbol( "close" );
var Svolume = sc_jsstring2symbol( "volume" );
var Sload = sc_jsstring2symbol( "load" );
var Sinfo = sc_jsstring2symbol( "info" );
var Sready = sc_jsstring2symbol( "ready" );
var Sposition = sc_jsstring2symbol( "position" );
var Span = sc_jsstring2symbol( "pan" );
var Smeta = sc_jsstring2symbol( "meta" );
var Splaylist = sc_jsstring2symbol( "playlist" );
var Serror = sc_jsstring2symbol( "error" );

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
   o.close = function() {
      with_hop( hop_service_url_varargs( url, Sclose ), err );
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
   o.playlist_set = function( plist, autorun ) {
      playlist = plist;
      with_hop( hop_service_url_varargs( url, Splaylist, plist ),
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

	 if( (k === Splay) || (k === Sstart) ) {
	    // play
	    audio.paused = false;
	    audio.state = Splay;
	    current_duration = rest.car;
	    current_position = rest.cdr.car;
	    current_volume = rest.cdr.cdr.car;
	    playlist_index = rest.cdr.cdr.cdr.car;
	    
	    hop_audio_run_hooks( audio, "play" );
	    hop_audio_run_hooks( audio, "volume" );
	 } else if( k == Spause ) {
	    // pause
	    audio.paused = true;
	    audio.state = Spause;
	    current_duration = rest.car;
	    current_position = rest.cdr.car;
	    current_volume = rest.cdr.cdr.car;
	    playlist_index = rest.cdr.cdr.cdr.car;
	    
	    hop_audio_run_hooks( audio, "pause" );
	    hop_audio_run_hooks( audio, "volume" );
	 } if( k == Sstop ) {
	    // stop
	    audio.paused = false;
	    audio.state = Sstop;
	    current_duration = rest.car;
	    current_position = rest.cdr.car;
	    current_volume = rest.cdr.cdr.car;
	    playlist_index = rest.cdr.cdr.cdr.car;
	    
	    hop_audio_run_hooks( audio, "stop" );
	    hop_audio_run_hooks( audio, "volume" );
	 } else if( k == Svolume ) {
	    current_volume = rest.car;
	    
	    hop_audio_run_hooks( audio, "volume" );
	 } else if( k == Serror ) {
	    // error
	    hop_audio_run_hooks( audio, "error", rest.car );
	 } else if( k == Sclose ) {
	    // close
	    hop_audio_run_hooks( audio, "close", false );
	 } else if( k == Smeta ) {
	    // meta (and playlist)
	    var val = rest.car;
	    playlist = rest.cdr.car;

	    if( typeof val == "string" ) {
	       current_metadata = {
	         title: sc_basename( val ),
		 artist: sc_basename( sc_dirname( sc_dirname( val ) ) ),
		 album: sc_basename( sc_dirname( val ) )
	       };
	    } else if( val ) {
	       current_metadata = val;
	    }
	    
	    hop_audio_run_hooks( audio, "metadata", current_metadata );
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
   hop_add_event_listener( document, "serverclose", function( evt ) {
	 hop_audio_onerror_callback( audio.id, false );
      } );

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
      var id = audio.id + (hop_msiep() ? "-object" : "-embed");
      audio.obj = document.getElementById( id );
   }

   return audio.obj;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_init ...                                               */
/*---------------------------------------------------------------------*/
function hop_audio_init( id, start, src, stream,
			 onplay, onstop, onpause, onload, onerror,
			 onended, onprogress,
			 onloadedmetadata, onclose ) {
   var audio = document.getElementById( id );

   hop_audio_init_obj( audio );

   audio.controls = document.getElementById( id + "-controls" );

   audio.playlist = null;
   audio.start = start;
   
   audio.onplay = onplay;
   audio.onstop = onstop;
   audio.onpause = onpause;
   audio.onload = onload;
   audio.onerror = onerror;
   audio.onended = onended;
   audio.onprogress = onprogress;
   audio.onmetadata = onloadedmetadata;
   audio.initialized = true;
   audio.onclose = onclose;
   
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
   audio.player = false;

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
   proxy.close = function() {
      proxy.flash_stop();
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
/*    hop_audio_player_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export audio-player-set!)) */
function hop_audio_player_set( audio, player, name ) {
   audio.player = player;
   
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
/*    hop_audio_player ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export audio-player)) */
function hop_audio_player( audio ) {
   return audio.player;
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
   evt.value = "Server closed connection...";

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
/*** META ((export audio-load)) */
function hop_audio_load( audio, src, stream ) {
   audio.src = src;
   audio.playlist_index = false;
   audio.paused = false;
   audio.state = stream ? Splay : Sstop;
   hop_audio_run_hooks( audio, "progress" );

   return audio.proxy.load( src, stream );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_state ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export audio-state)) */
function hop_audio_state( audio ) {
   return audio.state;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_set ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist-set!)) */
function hop_audio_playlist_set( audio, playlist, autorun ) {
   audio.proxy.playlist_set( playlist, autorun );
   return;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_get ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist)
           (peephole (postfix ".proxy.playlist_get()"))) */
function hop_audio_playlist_get( audio ) {
   return audio.proxy.playlist_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_index ...                                     */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist-index)
           (peephole (postfix ".proxy.playlist_index_get()"))) */
function hop_audio_playlist_index( audio ) {
   return audio.proxy.playlist_index_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_play ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist-play)) */
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
/*** META ((export audio-playlist-prev)) */
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
/*** META ((export audio-playlist-next)) */
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
/*** META ((export audio-play audio-start)) */
function hop_audio_play( audio, start ) {
   audio.playlist_index = false;
   audio.paused = false;
   audio.state = start ? Splay : Sstop;
   audio.proxy.play( start ? start : audio.start );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_close ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export audio-close)
           (peephole (postfix ".proxy.close()"))) */
function hop_audio_close( audio ) {
   audio.proxy.close();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_stop ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export audio-stop)
           (peephole (postfix ".proxy.stop()"))) */
function hop_audio_stop( audio ) {
   audio.proxy.stop();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_pause ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export audio-pause)) */
function hop_audio_pause( audio ) {
   audio.paused = !audio.paused;
   audio.state = audio.paused ? Spause : Splay;
   audio.proxy.pause();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_volume ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export audio-volume)
           (peephole (postfix ".proxy.volume_get()"))) */
function hop_audio_volume( audio ) {
   return audio.proxy.volume_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_volume_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export audio-volume-set!)
           (peephole (hole 2 audio ".proxy.volume_set(" vol ")"))) */
function hop_audio_volume_set( audio, vol ) {
   return audio.proxy.volume_set( vol );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_mute ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export audio-mute)) */
function hop_audio_mute( audio ) {
   var img = document.getElementById( audio.id + "-hop-audio-button-mute" );
   
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
/*** META ((export audio-pan)
           (peephole (postfix ".proxy.pan_get()"))) */
function hop_audio_pan( audio ) {
   return audio.proxy.pan_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_pan_set ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export audio-pan-set!)
           (peephole (hole 2 audio ".proxy.pan_set(" pan ")"))) */
function hop_audio_pan_set( audio, pan ) {
   return audio.proxy.pan_set( pan );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_duration ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export audio-duration)) */
function hop_audio_duration( audio ) {
   return audio.proxy.duration_get();
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_current_time ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export audio-current-time)) */
function hop_audio_current_time( audio ) {
   return audio.proxy.position_get();
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_seek ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export audio-seek)) */
function hop_audio_seek( audio, pos ) {
   audio.min = Math.floor( pos / 60 );
   audio.sec = pos % 60;
   audio.ctime = pos;
   
   return audio.proxy.position_set( pos );
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_metadata ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export audio-metadata)
           (peephole (postfix ".proxy.metadata_get()"))) */
function hop_audio_metadata( audio ) {
   return audio.proxy.metadata_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_metadata_set ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export audio-metadata-set!)) */
function hop_audio_metadata_set( audio, metadata ) {
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onload ...                                    */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onload( evt ) {
   var audio = evt.audio;
   var tl = document.getElementById( audio.id + "-controls-metadata-song" );

   hop_audio_controls_metadata( audio, true );
   tl.className = "hop-audio-panel-metadata-song";
   tl.innerHTML = "Ready...";
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onerror ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onerror( evt ) {
   var audio = evt.audio;
   var tl = document.getElementById( audio.id + "-controls-metadata-song" );
   var min = document.getElementById( audio.id + "-controls-status-length-min" );
   var sec = document.getElementById( audio.id + "-controls-status-length-sec" );

   tl.className = "hop-audio-panel-metadata-error";
   tl.innerHTML = evt.value ? evt.value : "Error";

   hop_audio_time_interval_clear( audio );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onprogress ...                                */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onprogress( evt ) {
   var audio = evt.audio;
   var tl = document.getElementById( audio.id + "-controls-metadata-song" );

   hop_audio_controls_metadata( audio, true );
   tl.className = "hop-audio-panel-metadata-song";
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
   var pos = document.getElementById( id + "-controls-status-position" );
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
   var pos = document.getElementById( id + "-controls-status-position" );
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
   var tl = document.getElementById( audio.id + "-controls-metadata-song" );
   var ab = document.getElementById( audio.id + "-controls-metadata-album" );
   var at = document.getElementById( audio.id + "-controls-metadata-artist" );
   var ye = document.getElementById( audio.id + "-controls-metadata-year" );

   if( reset ) {
      tl.innerHTML = "";
      ab.innerHTML = "";
      at.innerHTML = "";
      ye.innerHTML = "";
   } else {
      var md = audio.proxy.metadata_get();

      if( md ) {
	 tl.className = "hop-audio-panel-metadata-song";
	 tl.innerHTML = md.title;
	 at.innerHTML = md.artist;
	 ab.innerHTML = md.album;
	 if( md.year && md.year > 0 )
	    ye.innerHTML = md.year;
	 else
	    ye.innerHTML = "";
      } else {
	 if( audio.src ) {
	    tl.className = "hop-audio-panel-metadata-song";
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
/*    hop_audio_controls_onmetadata ...                                */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onmetadata( evt ) {
   var id = audio.id;
   var status = document.getElementById( id + "-controls-status-img" );
   var playbut = document.getElementById( id + "-hop-audio-button-play" );
   var track = document.getElementById( id + "-controls-status-track" );
   var min = document.getElementById( id + "-controls-status-length-min" );
   var sec = document.getElementById( id + "-controls-status-length-sec" );
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
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onclose ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onclose( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var tl = document.getElementById( id + "-controls-metadata-song" );
   var min = document.getElementById( id + "-controls-status-length-min" );
   var sec = document.getElementById( id + "-controls-status-length-sec" );
   var status = document.getElementById( id + "-controls-status-img" );
   var stopbut = document.getElementById( id + "-hop-audio-button-stop" );
   var track = document.getElementById( id + "-controls-status-track" );

   min.innerHTML = "  ";
   sec.innerHTML = "  ";
   track.innerHTML = "     ";
   
   hop_audio_time_interval_clear( audio );
   hop_audio_controls_metadata( audio, true );
   
   status.src = stopbut.src;
   tl.className = "hop-audio-panel-metadata-close";
   tl.innerHTML = "Player closed...";

}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onplay ...                                    */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onplay( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var status = document.getElementById( id + "-controls-status-img" );
   var playbut = document.getElementById( id + "-hop-audio-button-play" );
   var track = document.getElementById( id + "-controls-status-track" );
   var min = document.getElementById( id + "-controls-status-length-min" );
   var sec = document.getElementById( id + "-controls-status-length-sec" );
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
   var status = document.getElementById( id + "-controls-status-img" );
   var pausebut = document.getElementById( id + "-hop-audio-button-pause" );

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
   var status = document.getElementById( id + "-controls-status-img" );
   var stopbut = document.getElementById( id + "-hop-audio-button-stop" );
   var track = document.getElementById( id + "-controls-status-track" );
   var min = document.getElementById( id + "-controls-status-length-min" );
   var sec = document.getElementById( id + "-controls-status-length-sec" );

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
   var status = document.getElementById( id + "-controls-status-img" );
   var stopbut = document.getElementById( id + "-hop-audio-button-stop" );
   var track = document.getElementById( id + "-controls-status-track" );
   var min = document.getElementById( id + "-controls-status-length-min" );
   var sec = document.getElementById( id + "-controls-status-length-sec" );
   var tl = document.getElementById( id + "-controls-metadata-song" );

   track.className = "hop-audio-info-status-track";
   track.innerHTML = "88888";

   min.innerHTML = "  ";
   sec.innerHTML = "  ";

   hop_audio_time_interval_clear( audio );
   hop_audio_controls_metadata( audio, true );

   tl.className = "hop-audio-panel-metadata-song";
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
   var min = document.getElementById( id + "-controls-status-length-min" );
   var sec = document.getElementById( id + "-controls-status-length-sec" );

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
   var slider = document.getElementById( id + "-controls-volume" );

   hop_slider_value_set( slider, hop_audio_volume( audio ) );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_gui_playlist ...                                       */
/*---------------------------------------------------------------------*/
function hop_audio_gui_playlist( id ) {
}
