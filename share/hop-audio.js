/*=====================================================================*/
/*    serrano/prgm/project/hop/2.0.x/share/hop-audio.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Aug 21 13:48:47 2007                          */
/*    Last change :  Sun Aug 30 12:04:38 2009 (serrano)                */
/*    Copyright   :  2007-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP client-side audio support.                                   */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    HopAudioEvent ...                                                */
/*---------------------------------------------------------------------*/
function HopAudioEvent( n, a, v ) {
   this.isStopped = false;
   this.preventDefault = function() { };
   this.stopPropagation = this.preventDefault;
   this.name = n;
   this.audio = a;
   this.value = v;
}

/*---------------------------------------------------------------------*/
/*    HopAudioProxy ...                                                */
/*---------------------------------------------------------------------*/
function HopAudioProxy() {
}

HopAudioProxy.prototype.toString = function() { return "[object HopAudioProxy]"; };
HopAudioProxy.prototype.load = function( src, stream ) { return true; };
HopAudioProxy.prototype.play = function( start ) { return true; };
HopAudioProxy.prototype.playlist_play = function( index ) { return true; };
HopAudioProxy.prototype.playlist_set = function( pl, autorun ) { return true; };
HopAudioProxy.prototype.playlist_get = function() { return null; };
HopAudioProxy.prototype.playlist_index_get = function() { return 0; }
HopAudioProxy.prototype.stop = function() { return true; };
HopAudioProxy.prototype.close = function() { return true; };
HopAudioProxy.prototype.pause = function() { return true; };
HopAudioProxy.prototype.position_get = function() { return 0; };
HopAudioProxy.prototype.position_set = function() { return 0; };
HopAudioProxy.prototype.pan_get = function() { return 0; };
HopAudioProxy.prototype.pan_set = function() { return true; };
HopAudioProxy.prototype.volume_get = function() { return 0; };
HopAudioProxy.prototype.volume_set = function() { return 0; };
HopAudioProxy.prototype.duration_get = function() { return 0; };
HopAudioProxy.prototype.metadata_get = function() { return false; };

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
var Sabort = sc_jsstring2symbol( "abort" );

/*---------------------------------------------------------------------*/
/*    hop_audio_run_hooks ...                                          */
/*---------------------------------------------------------------------*/
function hop_audio_run_hooks( audio, evname, value ) {
   var evt = new HopAudioEvent( evname, audio, value );
   var handler = "on" + evname;

   if( (handler in audio) && (typeof( audio[ handler ] ) == "function") ) {
      audio[ handler ]( evt );
   }

   if( !evt.isStopped
       && audio.controls
       && (handler in audio.controls)
       && ((typeof audio.controls[ handler ]) == "function" ) ) {
      audio.controls[ handler ]( evt );
   }
}
   
/*---------------------------------------------------------------------*/
/*    HopAudioServerProxy ...                                          */
/*---------------------------------------------------------------------*/
function HopAudioServerProxy( a, u ) {
   var proxy = this;
   this.url = u;
   this.audio = a;
   
   this.current_duration = false;
   this.current_position = 0;
   this.current_volume = 0;
   this.current_pan = 0;
   this.current_metadata = false;
   this.playlist = null;
   this.playlist_index = -1;

   this.err = function( h ) {
      if( !h ) hop_audio_run_hooks( this.audio, "error" );
   };
   
   // install the server listener...
   hop_add_event_listener( this.url, "server", function( evt ) {
	 proxy.event_listener( evt );
      } );
   hop_add_event_listener( document, "serverclose", function( evt ) {
	 hop_audio_onerror_callback( a.id, false, "Server closed connection!" );
      } );

   // ... and starts the server event notification
   hop_add_event_listener( document, "serverready", function() {
	 with_hop( hop_apply_url( u, [ Sready, false ] ) );
      } );
}

HopAudioServerProxy.prototype = new HopAudioProxy();

HopAudioProxy.prototype.toString = function() {
   return "[object HopAudioServerProxy]";
};

HopAudioServerProxy.prototype.playlist_play = function( index ) {
   with_hop( hop_apply_url( this.url, [ Splay, index ] ), this.err );
};
   
HopAudioServerProxy.prototype.play = function( start ) {
   with_hop( hop_apply_url( this.url, [ Splay, 0 ] ), this.err );
};

HopAudioServerProxy.prototype.stop = function() {
   with_hop( hop_apply_url( this.url, [ Sstop, false ] ), this.err );
};

HopAudioServerProxy.prototype.close = function() {
   with_hop( hop_apply_url( this.url, [ Sclose, false ] ), this.err );
};

HopAudioServerProxy.prototype.pause = function() {
   with_hop( hop_apply_url( this.url, [ Spause, false ] ), this.err );
};

HopAudioServerProxy.prototype.load = function( l, s ) {
   var err = this.err;
   var url = this.url;

   var success = function( h ) {
      if( !h ) {
	 err( h );
      } else {
	 if( s ) {
	    with_hop( hop_apply_url( url, [ Splay, 0 ] ), err );
	 }
	 hop_audio_run_hooks( audio, "load" );
      }
   };
      
   with_hop( hop_apply_url( url, [ Sload, l ] ), success );
};

HopAudioServerProxy.prototype.position_get = function() {
   return this.current_position;
};

HopAudioServerProxy.prototype.position_set = function( p ) {
   with_hop( hop_apply_url( this.url, [ Sposition, p ] ), this.err );
};

HopAudioServerProxy.prototype.pan_get = function() {
   return this.current_pan;
};

HopAudioServerProxy.prototype.pan_set = function( p ) {
   with_hop( hop_apply_url( this.url, [ Span, p ] ), this.err );
};

HopAudioServerProxy.prototype.volume_get = function() {
   return this.current_volume;
};

HopAudioServerProxy.prototype.volume_set = function( val ) {
   with_hop( hop_apply_url( this.url, [ Svolume, val ] ), this.err );
};

HopAudioServerProxy.prototype.duration_get = function() {
   return this.current_duration;
};

HopAudioServerProxy.prototype.metadata_get = function() {
   return this.current_metadata;
};

HopAudioServerProxy.prototype.playlist_get = function() {
   return this.playlist;
};
   
HopAudioServerProxy.prototype.playlist_set = function( plist, autorun ) {
   var o = this;
   this.playlist = plist;
   
   with_hop( hop_apply_url( this.url, [ Splaylist, plist ] ),
	     function( h ) {
		hop_audio_run_hooks( o.audio, "load" );
		if( autorun ) o.playlist_play( 0 );
	     }, false );
};

HopAudioServerProxy.prototype.playlist_index_get = function() {
   return this.playlist_index;
};

HopAudioServerProxy.prototype.event_listener = function( e ) {
   if( sc_isPair( e.value ) ) {
      var k = e.value.car;
      var rest = e.value.cdr;

      if( (k === Splay) || (k === Sstart) ) {
	 // play
	 this.audio.paused = false;
	 this.audio.state = Splay;
	 this.current_duration = rest.car;
	 this.current_position = rest.cdr.car;
	 this.current_volume = rest.cdr.cdr.car;
	 this.playlist_index = rest.cdr.cdr.cdr.car;
	    
	 hop_audio_run_hooks( this.audio, "play" );
	 hop_audio_run_hooks( this.audio, "volume" );
      } else if( k == Spause ) {
	 // pause
	 this.audio.paused = true;
	 this.audio.state = Spause;
	 this.current_duration = rest.car;
	 this.current_position = rest.cdr.car;
	 this.current_volume = rest.cdr.cdr.car;
	 this.playlist_index = rest.cdr.cdr.cdr.car;
	    
	 hop_audio_run_hooks( this.audio, "pause" );
	 hop_audio_run_hooks( this.audio, "volume" );
      } if( k == Sstop ) {
	 // stop
	 this.audio.paused = false;
	 this.audio.state = Sstop;
	 this.current_duration = rest.car;
	 this.current_position = rest.cdr.car;
	 this.current_volume = rest.cdr.cdr.car;
	 this.playlist_index = rest.cdr.cdr.cdr.car;
	    
	 hop_audio_run_hooks( this.audio, "stop" );
	 hop_audio_run_hooks( this.audio, "volume" );
      } else if( k == Svolume ) {
	 this.current_volume = rest.car;
	    
	 hop_audio_run_hooks( this.audio, "volume" );
      } else if( k == Serror ) {
	 // error
	 hop_audio_run_hooks( this.audio, "error", rest.car );
      } else if( k == Sabort ) {
	 // abort
	 hop_audio_run_hooks( this.audio, "error", rest.car );
	 hop_audio_shutdown( this.audio );
      } else if( k == Sclose ) {
	 // close
	 hop_audio_close( this.audio );
	 hop_audio_run_hooks( this.audio, "close", false );
      } else if( k == Smeta ) {
	 // meta (and playlist)
	 var val = rest.car;
	 this.playlist = rest.cdr.car;

	 if( typeof val === "string" ) {
	    this.current_metadata = {
	    title: sc_basename( val ),
	    artist: sc_basename( sc_dirname( sc_dirname( val ) ) ),
	    album: sc_basename( sc_dirname( val ) )
	    };
	 } else if( val ) {
	    this.current_metadata = val;
	 }
	    
	 hop_audio_run_hooks( this.audio, "metadata", this.current_metadata );
      }
   } else {
      if( !e.value ) {
	 // the server has closed the connection, act as a plain stop action
	 hop_audio_run_hooks( this.audio, "stop" );
      }
   }
      
   return;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_add_event_listener ...                                 */
/*---------------------------------------------------------------------*/
function hop_audio_add_event_listener( event, proc, capture ) {
   this[ "on" + event ] = proc;
   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_init_obj ...                                           */
/*---------------------------------------------------------------------*/
function hop_audio_init_obj( audio ) {
   if( !audio.obj ) {
      var id = audio.id + "-" + hop_config.flash_markup;
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
			 onloadedmetadata, onclose, onplayer ) {
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
   audio.onplayer = onplayer;

   audio.hop_add_event_listener = hop_audio_add_event_listener;
   audio.toString = function() { return "[object HopAudio]"; };

   if( !("proxy" in audio) ) {
      // create a proxy only if flash initialization has not occured yet
      audio.proxy = new HopAudioProxy();
      if( src )
	 hop_audio_load( audio, src, stream );
   } else {
      // there is a proxy, add to the playlist
      hop_audio_playlist_set( audio, sc_list( src ) );
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
   hop_flash_audio_set( true );

   var audio = document.getElementById( id );
   var proxy = hop_audio_init_obj( audio );

   audio.proxy = proxy;
   audio.client_proxy = proxy;
   audio.player = false;

   proxy.playlist = null;
   proxy.playlist_index = 0;

   proxy.play = function( start ) {
      if( audio.src ) {
	 proxy.flash_play( start );
	 hop_audio_run_hooks( audio, "play" );
      }
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

   if( player ) {
      hop_audio_player_set( audio, player, "server" );
   } else {
      if( src && audio.initialized ) {
	 hop_audio_load( audio, src, stream );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_player_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export audio-player-set!) (arity #t)) */
function hop_audio_player_set( audio, player, name ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   
   audio.player = player;

   /* cleanup the current proxy */
   if( audio.server_proxy ) {
      hop_remove_event_listener( audio.server_proxy.url,
				 "server",
				 audio.server_proxy.event_listener );
   }

   /* install the new proxy */
   if( !player ) {
      if( "client_proxy" in audio ) {
	 audio.proxy = audio.client_proxy;
      } else {
	 audio.proxy = new HopAudioProxy();
      }
      
      hop_audio_run_hooks( audio, "player", name );
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
/*** META ((export audio-player) (arity #t)) */
function hop_audio_player( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
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
/*    hop_audio_shutdown ...                                           */
/*    -------------------------------------------------------------    */
/*    This internal function is closed to shutdown a proxy player      */
/*    either after an error or a audio-close call.                     */
/*---------------------------------------------------------------------*/
function hop_audio_shutdown( audio ) {
   audio.state = Sclose;
   audio.proxy = new HopAudioProxy();
   audio.player = false;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_onerror_callback ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_onerror_callback( id, play, msg ) {
   var audio = document.getElementById( id );
   var evt = new HopAudioEvent( "error", audio, msg );

   if( audio.onerror )
      audio.onerror( evt );
   if( !evt.isStopped && audio.controls && audio.controls.onerror )
      audio.controls.onerror( evt );

   hop_audio_shutdown( audio );
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
/*** META ((export audio-load) (arity #t)) */
function hop_audio_load( audio, src, stream ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );

   audio.src = src;
   audio.playlist_index = false;
   audio.paused = false;
   
   if( src ) {
      audio.state = stream ? Splay : Sstop;
      hop_audio_run_hooks( audio, "progress" );

      return audio.proxy.load( src, stream );
   } else {
      audio.state = Sstop;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_state ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export audio-state) (arity #t)) */
function hop_audio_state( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.state;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_set ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist-set!) (arity #t)) */
function hop_audio_playlist_set( audio, playlist, autorun ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );

   audio.proxy.playlist_set( playlist, autorun );
   return;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_get ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist) (arity #t)) */
function hop_audio_playlist_get( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.proxy.playlist_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_index ...                                     */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist-index) (arity #t)) */
function hop_audio_playlist_index( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.proxy.playlist_index_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_play ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist-play) (arity #t)) */
function hop_audio_playlist_play( audio, i ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   var pl = hop_audio_playlist_get( audio );

   if( (i >= 0) && (i < sc_length( pl )) ) {
      audio.proxy.playlist_play( i );
      return i;
   } else {
      if( (sc_length( pl ) == 0) && (i == 0) ) {
	 /* play the src file */
	 audio.proxy.play( true );
      }
   }

   return -1;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_prev ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist-prev) (arity #t)) */
function hop_audio_playlist_prev( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
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
/*** META ((export audio-playlist-next) (arity #t)) */
function hop_audio_playlist_next( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
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
/*** META ((export audio-play audio-start) (arity #t)) */
function hop_audio_play( audio, start ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   
   audio.playlist_index = false;
   audio.paused = false;
   audio.state = start ? Splay : Sstop;
   audio.proxy.play( start ? start : audio.start );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_close ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export audio-close) (arity #t)) */
function hop_audio_close( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   
   /* cleanup the current proxy */
   if( audio.server_proxy ) {
      hop_remove_event_listener( audio.server_proxy.url,
				 "server",
				 audio.server_proxy.event_listener );
   }

   audio.proxy.close();

   /* shutdown the proxy */
   hop_audio_shutdown( audio );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_stop ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export audio-stop) (arity #t)) */
function hop_audio_stop( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   audio.proxy.stop();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_pause ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export audio-pause) (arity #t)) */
function hop_audio_pause( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   audio.paused = !audio.paused;
   audio.state = audio.paused ? Spause : Splay;
   audio.proxy.pause();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_volume ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export audio-volume) (arity #t)) */
function hop_audio_volume( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.proxy.volume_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_volume_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export audio-volume-set!) (arity #t)) */
function hop_audio_volume_set( audio, vol ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   if( hop_audio_volume( audio ) != vol )
      return audio.proxy.volume_set( vol );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_mute ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export audio-mute) (arity #t)) */
function hop_audio_mute( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
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
/*** META ((export audio-pan) (arity #t)) */
function hop_audio_pan( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.proxy.pan_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_pan_set ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export audio-pan-set!) (arity #t)) */
function hop_audio_pan_set( audio, pan ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.proxy.pan_set( pan );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_duration ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export audio-duration) (arity #t)) */
function hop_audio_duration( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.proxy.duration_get();
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_current_time ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export audio-current-time) (arity #t)) */
function hop_audio_current_time( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.proxy.position_get();
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_seek ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export audio-seek) (arity #t)) */
function hop_audio_seek( audio, pos ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   
   audio.min = Math.floor( pos / 60 );
   audio.sec = pos % 60;
   audio.ctime = pos;
   
   return audio.proxy.position_set( pos );
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_metadata ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export audio-metadata) (arity #t)) */
function hop_audio_metadata( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.proxy.metadata_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_metadata_set ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export audio-metadata-set!) (arity #t)) */
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
//      tl.innerHTML = "";
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
   var audio = evt.audio;
   var id = audio.id;
   var status = document.getElementById( id + "-controls-status-img" );
   var playbut = document.getElementById( id + "-hop-audio-button-play" );
   var track = document.getElementById( id + "-controls-status-track" );
   var min = document.getElementById( id + "-controls-status-length-min" );
   var sec = document.getElementById( id + "-controls-status-length-sec" );
   var alen = hop_audio_duration( audio );
   var plen = sc_length( hop_audio_playlist_get( audio ) );

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
   var track = document.getElementById( id + "-controls-status-track" );

   min.innerHTML = "  ";
   sec.innerHTML = "  ";
   track.innerHTML = "     ";
   
   hop_audio_time_interval_clear( audio );
   hop_audio_controls_metadata( audio, true );
   
   status.className = "hop-audio-info-status-img hop-audio-info-status-stop";
   if( tl.className === "hop-audio-panel-metadata-error" ) {
      var ab = document.getElementById( audio.id + "-controls-metadata-album" );
      ab.innerHTML = (evt.value ? evt.value : "player closed...");
   } else {
      tl.className = "hop-audio-panel-metadata-close";
      tl.innerHTML = evt.value ? evt.value + " closed..." : "Player closed...";
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onplay ...                                    */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onplay( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var status = document.getElementById( id + "-controls-status-img" );
   var track = document.getElementById( id + "-controls-status-track" );
   var min = document.getElementById( id + "-controls-status-length-min" );
   var sec = document.getElementById( id + "-controls-status-length-sec" );
   var alen = hop_audio_duration( audio );
   var plen = sc_length( hop_audio_playlist_get( audio ) );

   status.className = "hop-audio-info-status-img hop-audio-info-status-play";

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

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onpause ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onpause( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var status = document.getElementById( id + "-controls-status-img" );

   if( audio.paused ) {
      audio.old_className = status.className;
      status.className = "hop-audio-info-status-img hop-audio-info-status-pause";
   } else {
      status.className = audio.old_className;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onstop ...                                    */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onstop( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var status = document.getElementById( id + "-controls-status-img" );
   var track = document.getElementById( id + "-controls-status-track" );
   var min = document.getElementById( id + "-controls-status-length-min" );
   var sec = document.getElementById( id + "-controls-status-length-sec" );
   var tl = document.getElementById( id + "-controls-metadata-song" );

   track.className = "hop-audio-info-status-track";
   track.innerHTML = "88888";

   min.innerHTML = "  ";
   sec.innerHTML = "  ";
   tl.innerHTML = "";

   hop_audio_time_interval_clear( audio );
   hop_audio_controls_metadata( audio, true );
   status.className = "hop-audio-info-status-img hop-audio-info-status-stop";
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onplayer ...                                  */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onplayer( evt ) {
   var audio = evt.audio;
   var id = audio.id;
   var status = document.getElementById( id + "-controls-status-img" );
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

   tl.className = "hop-audio-panel-metadata-player";
   tl.innerHTML = evt.value ?
      evt.value + " player ready..." :
      "Player ready...";

   status.className = "hop-audio-info-status-img hop-audio-info-status-stop";
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
