/*=====================================================================*/
/*    serrano/prgm/project/hop/2.0.x/share/hop-audio.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Aug 21 13:48:47 2007                          */
/*    Last change :  Fri Jan  8 15:17:14 2010 (serrano)                */
/*    Copyright   :  2007-10 Manuel Serrano                            */
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
/*    HopAudioServerBackend ...                                        */
/*---------------------------------------------------------------------*/
function HopAudioServerBackend( a, u ) {
   var be = this;
   this.url = u;
   this.audio = a;
}

// toString
HopAudioServerBackend.prototype.toString = function() {
   return "[object HopAudioServerBackend]";
};

// playurl
HopAudioServerBackend.prototype.playurl = function( url, start ) {
   if( !sc_isPair( this.playlist ) ) {
      var success = function( h ) {
	 with_hop( hop_apply_url( this.url, [ Splay, 0 ] ), this.err );
      }
      var url = this.audio.src;
      with_hop( hop_apply_url( this.url, [ Sload, url ] ), success );
   } else {
      with_hop( hop_apply_url( this.url, [ Splay, 0 ] ), this.err );
   }
};

// playlist_play
HopAudioServerBackend.prototype.playlist_play = function( index ) {
   with_hop( hop_apply_url( this.url, [ Splay, index ] ), this.err );
};

// stop
HopAudioServerBackend.prototype.stop = function() {
   with_hop( hop_apply_url( this.url, [ Sstop, false ] ), this.err );
};

// close
HopAudioServerBackend.prototype.close = function() {
   this.isStopped = true;
   hop_audio_invoke_listeners( this.audio, "close", false );
   hop_remove_event_listener( this.audio.serverbackend.url,
			      "server",
			      this.audio.serverbackend.event_listener );
   this.audio.backend = this.audio.clientbackend;

   with_hop( hop_apply_url( this.url, [ Sclose, false ] ) );
};

// pause
HopAudioServerBackend.prototype.pause = function() {
   with_hop( hop_apply_url( this.url, [ Spause, false ] ), this.err );
};

// load
HopAudioServerBackend.prototype.load = function( l, s ) {
   var err = this.err;
   var url = this.url;

   var success = function( h ) {
      if( !h ) {
	 err( h );
      } else {
	 if( s ) {
	    with_hop( hop_apply_url( url, [ Splay, 0 ] ), err );
	 }
	 hop_audio_invoke_listeners( audio, "load" );
      }
   };

   with_hop( hop_apply_url( url, [ Sload, l ] ), success );
};

// position_get
HopAudioServerBackend.prototype.position_get = function() {
   return this.current_position;
};

// position_set
HopAudioServerBackend.prototype.position_set = function( p ) {
   with_hop( hop_apply_url( this.url, [ Sposition, p ] ), this.err );
};

// pan_get
HopAudioServerBackend.prototype.pan_get = function() {
   return this.current_pan;
};

// pan_set
HopAudioServerBackend.prototype.pan_set = function( p ) {
   with_hop( hop_apply_url( this.url, [ Span, p ] ), this.err );
};

// volume_get
HopAudioServerBackend.prototype.volume_get = function() {
   return this.current_volume;
};

// volume_set
HopAudioServerBackend.prototype.volume_set = function( val ) {
   with_hop( hop_apply_url( this.url, [ Svolume, val ] ), this.err );
};

// duration_get
HopAudioServerBackend.prototype.duration_get = function() {
   return this.current_duration;
};

// metadata_get
HopAudioServerBackend.prototype.metadata_get = function() {
   return this.current_metadata;
};

// playlist_get
HopAudioServerBackend.prototype.playlist_get = function() {
   return this.playlist;
};

// playlist_set
HopAudioServerBackend.prototype.playlist_set = function( plist, autorun ) {
   var o = this;
   
   var server_canplay = function( o ) {
      if( typeof o === "string" ) {
	 return o;
      }
      if( !sc_isPair( o ) ) {
	 return false;
      }

      /* find a string element, or return the first element of the list */
      var no = o;
      while( sc_isPair( no ) ) {
	 var e = no.car;

	 if( typeof e === "string" ) {
	    return e;
	 }

	 no = no.cdr;
      }
      
      return o.car.cdr.car;
   }
   
   this.playlist = sc_filterMap1( server_canplay, plist );

   with_hop( hop_apply_url( this.url, [ Splaylist, this.playlist ] ),
	     function( h ) {
		hop_audio_invoke_listeners( o.audio, "load" );
		if( autorun ) o.playlist_play( 0 );
	     }, false );
};

// playlist_index_get
HopAudioServerBackend.prototype.playlist_index_get = function() {
   return this.playlistindex;
};

// update
HopAudioServerBackend.prototype.update = function() {
   var backend = this;
   with_hop( hop_apply_url( backend.url, [ Sstatus, 0 ] ),
	     function( val ) {
		hop_audio_server_event_listener( {value: val}, backend );
	     },
	     backend.err,
	     false,
	     true,
	     5000 );
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_init ...                                               */
/*    -------------------------------------------------------------    */
/*    The list of audio init functions.                                */
/*---------------------------------------------------------------------*/
var hop_audio_init = {};

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
var Sposition = sc_jsstring2symbol( "position" );
var Span = sc_jsstring2symbol( "pan" );
var Smeta = sc_jsstring2symbol( "meta" );
var Splaylist = sc_jsstring2symbol( "playlist" );
var Serror = sc_jsstring2symbol( "error" );
var Sabort = sc_jsstring2symbol( "abort" );
var Sstatus = sc_jsstring2symbol( "status" );

var Sclient = sc_jsstring2symbol( "client" );
var Sserver = sc_jsstring2symbol( "server" );

/*---------------------------------------------------------------------*/
/*    hop_audio_add_event_listener ...                                 */
/*---------------------------------------------------------------------*/
function hop_audio_add_event_listener( event, proc, capture ) {
   this[ "on" + event ] = proc;
   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_invoke_listeners ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_invoke_listeners( audio, evname, value ) {
   var evt = new HopAudioEvent( evname, audio, value );
   var handler = "on" + evname;

   /* the audio listener */
   if( (handler in audio) && (typeof( audio[ handler ] ) == "function") ) {
      audio[ handler ]( evt );
   }

   /* the controller listener */
   if( !evt.isStopped
       && audio.controls
       && (handler in audio.controls)
       && ((typeof audio.controls[ handler ]) == "function" ) ) {
      audio.controls[ handler ]( evt );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_listeners_init ...                            */
/*    -------------------------------------------------------------    */
/*    Install the default listener of the controllers                  */
/*---------------------------------------------------------------------*/
function hop_audio_controls_listeners_init( id ) {
   var el = document.getElementById( id );

   el.onload = hop_audio_controls_onload;
   el.onerror = hop_audio_controls_onerror;
   el.onplay = hop_audio_controls_onplay;
   el.onloadedmetadata = hop_audio_controls_onloadedmetadata;
   el.onpause = hop_audio_controls_onpause;
   el.onstop = hop_audio_controls_onstop;
   el.onclose = hop_audio_controls_onclose;
   el.onended = hop_audio_controls_onended;
   el.onprogress = hop_audio_controls_onprogress;
   el.onvolume = hop_audio_controls_onvolume;
   el.onbackend = hop_audio_controls_onbackend;

   return el;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_html5_init ...                                         */
/*---------------------------------------------------------------------*/
function hop_audio_html5_init( backend ) {

   backend.playlistindex = -1;
   backend.playlist = null;
   backend.loadedurl = false;
   backend.isstopped = false;

   var html5_canplay = function( o ) {
      if( typeof o === "string" ) {
	 return o;
      }
      if( !sc_isPair( o ) ) {
	 return false;
      }
      while( sc_isPair( o ) ) {
	 var e = o.car;

	 if( typeof e === "string" ) {
	    return e;
	 }
	 if( sc_isPair( e ) && sc_isPair( e.cdr ) ) {
	    if( backend.canPlayType( e.car ) !== "" ) {
	       return e.cdr.car;
	    }
	 }
	 o = o.cdr;
      }
      return false;
   }
   
   backend.playurl = function( url, start ) {
      this.isStopped = false;
      if( this.loadedurl !== url ) {
	 this.loadedurl = false;
	 this.src = url;
	 this.autoplay = true;
	 this.load( url );
      } else {
	 this.play();
      }
   }
   backend.playlist_play = function( index ) {
      this.isStopped = false;
      this.playlistindex = index;

      this.playurl( sc_listRef( this.playlist, index ), 0 );
   }
   backend.stop = function() {
      this.isStopped = true;
      return this.pause();
   }
   backend.close = function() {
      this.isStopped = true;
      this.pause();
      hop_audio_invoke_listeners( this.audio, "close" );
   }
   backend.volume_get = function() {
      return Math.round( this.volume * 100 );
   }
   backend.volume_set = function( v ) {
      this.volume = v / 100;
      return v;
   }
   backend.duration_get = function() {
      return this.duration ? Math.round( this.duration ) : 0;
   }
   backend.position_get = function() {
      return this.currentTime ? Math.round( this.currentTime ) : 0;
   }
   backend.playlist_index_get = function() {
      return this.playlistindex;
   }
   backend.playlist_get = function() {
      return this.playlist;
   }
   backend.playlist_set = function( playlist, autorun ) {
      this.playlist = sc_filterMap1( html5_canplay, playlist );
      if( autorun ) this.playlist_play( 0 );
   }
   backend.metadata_get = function() {
      if( typeof this.loadedurl === "string" ) {
	 var a = url_decode( this.loadedurl ).split( "/" );

	 if( a.length >= 3 ) {
	    return { title: a[ a.length - 1 ],
		     album: a[ a.length - 2 ],
		     artist: a[ a.length - 3 ] };
	 if( a.length >= 2 ) {
	    return { title: a[ a.length - 1 ],
		     album: a[ a.length - 2 ],
		     artist: false };
	 }
	 if( a.length >= 1 )
	    return { title: a[ a.length - 1 ],
		     album: false,
		     artist: false };
	 }
	 return { title: s,
		  album: false,
		  artist: false };
      } else {
	 return false;
      }
   }
   backend.update = function() {
      ;
   }
   
   backend.addEventListener( "play", function( e ) {
	 this.state = Splay;
	 hop_audio_invoke_listeners( this.audio, "play" );
      }, true, true );
   backend.addEventListener( "pause", function( e ) {
	 if( this.isStopped ) {
	    this.state =  Sstop;
	    hop_audio_invoke_listeners( this.audio, "stop" );
	 } else {
	    this.state = Spause;
	    hop_audio_invoke_listeners( this.audio, "pause" );
	 }
      }, true, true );
   backend.addEventListener( "load", function( e ) {
	 hop_audio_invoke_listeners( this.audio, "load" );
      }, true, true );
   backend.addEventListener( "loadstart", function( e ) {
	 this.loadedurl = this.src;
	 hop_audio_invoke_listeners( this.audio, "progress" );
      }, true, true );
   backend.addEventListener( "ended", function( e ) {
	 var i = this.playlistindex;
	 this.state = Sstop;
	 hop_audio_invoke_listeners( this.audio, "ended" );
	 if( i >= 0 && (i < (sc_length( this.playlist ) - 1)) )
	    this.playlist_play( i + 1 );
      }, true, true );
   backend.addEventListener( "loadedmetadata", function( e ) {
	 hop_audio_invoke_listeners( this.audio, "loadedmetadata" );
      }, true, true );
   backend.addEventListener( "error", function( e ) {
	 hop_audio_invoke_listeners( this.audio, "error" );
      }, true, true );
   
   return backend;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_server_init ...                                        */
/*---------------------------------------------------------------------*/
function hop_audio_server_init( backend ) {
   backend.current_duration = false;
   backend.current_position = 0;
   backend.current_volume = 0;
   backend.current_pan = 0;
   backend.current_metadata = false;
   backend.playlist = null;
   backend.playlistindex = -1;

   // install the server listener...
   hop_add_event_listener( backend.url, "server", function( evt ) {
	 if( backend.audio.backend === backend ) 
	    return hop_audio_server_event_listener( evt, backend );
      } );
   hop_add_event_listener( document, "serverclose", function( evt ) {
	 if( backend.audio.backend === backend && !backend.state === Sclose ) {
	    hop_audio_close( backend.audio );
	    hop_audio_invoke_listeners( backend.audio, "close", false );
	 }
      } );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_flash_init ...                                         */
/*    -------------------------------------------------------------    */
/*    This function is automatically invoked by the Flash script when  */
/*    the builtin initialization completes.                            */
/*---------------------------------------------------------------------*/
function hop_audio_flash_init( backend ) {
   
   /* we are now sure that at least version 8 of flash is     */
   /* running because this function has been called by flash  */
   hop_flash_minversion_set( 8 );
   hop_flash_audio_set( true );

   backend.playlistindex = -1;
   backend.playlist = null;
   backend.loadedurl = false;
   backend.isStopped = false;

   var flash = document.getElementById( backend.id + "-" + hop_config.flash_markup );

   var flash_canplay = function( o ) {
      if( typeof o === "string" ) {
	 return o;
      }
      if( !sc_isPair( o ) ) {
	 return false;
      }
      while( sc_isPair( o ) ) {
	 var e = o.car;
	 if( typeof e === "string" ) {
	    return e;
	 }
	 if( sc_isPair( e ) && sc_isPair( e.cdr ) ) {
	    if( e.car === "audio/mpeg" ) {
	       return e.cdr.car;
	    }
	 }
	 o = o.cdr;
      }
      return false;
   }
   backend.playurl = function( url, start ) {
      this.isStopped = false;
      if( this.loadedurl !== url ) {
	 this.loadedurl = false;
	 this.src = url;
	 flash.load( url, true );
      } else {
	 flash.flash_play( start );
      }
   }
   backend.playlist_play = function( index ) {
      this.isStopped = false;
      this.playlistindex = index;

      this.playurl( sc_listRef( this.playlist, index ), 0 );
   }
   backend.stop = function() {
      this.isStopped = true;
      flash.flash_stop();
      hop_audio_invoke_listeners( this.audio, "stop" );
   }
   backend.pause = function() {
      flash.flash_pause();
   }
   backend.close = function() {
      this.isStopped = true;
      flash.flash_stop();
      hop_audio_invoke_listeners( this.audio, "close" );
   }
   backend.duration_get = function() {
      return flash.duration_get();
   }
   backend.position_get = function() {
      return flash.position_get();
   }
   backend.position_set = function( v ) {
      return flash.position_set( v );
   }
   backend.volume_get = function() {
      return flash.volume_get();
   }
   backend.volume_set = function( v ) {
      return flash.volume_get( v );
   }
   backend.playlist_index_get = function() {
      return this.playlistindex;
   }
   backend.playlist_get = function() {
      return this.playlist;
   }
   backend.playlist_set = function( playlist, autorun ) {
      this.playlist = sc_filterMap1( flash_canplay, playlist );
      if( autorun ) this.playlist_play( 0 );
   }
   backend.playlist_index_get = function() {
      return this.playlistindex;
   }
   
   backend.metadata_get = function() {
      try {
	 return eval( flash.id3_get() );
      } catch( _ ) {
	 return false;
      }
   }
   backend.update = function() {
      ;
   }

   flash.onload_set( "hop_audio_flash_onload_callback", backend.id );
   flash.onerror_set( "hop_audio_flash_onerror_callback", backend.id );
   flash.onended_set( "hop_audio_flash_onended_callback", backend.id );

   return backend;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_flash_onload_callback ...                              */
/*---------------------------------------------------------------------*/
function hop_audio_flash_onload_callback( id, play ) {
   var backend = document.getElementById( id );
   backend.state = Splay;
   
   hop_audio_invoke_listeners( backend.audio, "load" );
   hop_audio_invoke_listeners( backend.audio, "play" );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_flash_onended_callback ...                             */
/*---------------------------------------------------------------------*/
function hop_audio_flash_onended_callback( id ) {
   var backend = document.getElementById( id );
   var i = backend.playlistindex;

   backend.state = Sstop;
   hop_audio_invoke_listeners( backend.audio, "ended" );
   if( i >= 0 && (i < (sc_length( backend.playlist ) - 1)) )
      backend.playlist_play( i + 1 );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_flash_onerror_callback ...                             */
/*---------------------------------------------------------------------*/
function hop_audio_flash_onerror_callback( id, play, msg ) {
   var backend = document.getElementById( id );

   alert( "ERROR.flash");
   hop_audio_invoke_listeners( backend.audio, "error" );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_server_event_listener ...                              */
/*---------------------------------------------------------------------*/
function hop_audio_server_event_listener( e, backend ) {
   if( sc_isPair( e.value ) ) {
      var k = e.value.car;
      var rest = e.value.cdr;

      if( (k === Splay) || (k === Sstart) ) {
	 // play
	 backend.state = Splay;
	 backend.current_duration = rest.car;
	 backend.current_position = rest.cdr.car;
	 backend.current_volume = rest.cdr.cdr.car;
	 backend.playlistindex = rest.cdr.cdr.cdr.car;
	    
	 hop_audio_invoke_listeners( backend.audio, "play" );
	 hop_audio_invoke_listeners( backend.audio, "volume" );
      } else if( k == Spause ) {
	 // pause
	 backend.state = Spause;
	 backend.current_duration = rest.car;
	 backend.current_position = rest.cdr.car;
	 backend.current_volume = rest.cdr.cdr.car;
	 backend.playlistindex = rest.cdr.cdr.cdr.car;
	    
	 hop_audio_invoke_listeners( backend.audio, "pause" );
	 hop_audio_invoke_listeners( backend.audio, "volume" );
      } if( k == Sstop ) {
	 // stop
	 backend.state = Sstop;
	 backend.current_duration = rest.car;
	 backend.current_position = rest.cdr.car;
	 backend.current_volume = rest.cdr.cdr.car;
	 backend.playlistindex = rest.cdr.cdr.cdr.car;
	    
	 hop_audio_invoke_listeners( backend.audio, "stop" );
	 hop_audio_invoke_listeners( backend.audio, "volume" );
      } else if( k == Svolume ) {
	 backend.current_volume = rest.car;
	    
	 hop_audio_invoke_listeners( backend.audio, "volume" );
      } else if( k == Serror ) {
	 // error
	 hop_audio_invoke_listeners( backend.audio, "error", rest.car );
      } else if( k == Sabort ) {
	 // abort
	 hop_audio_close( backend.audio );
	 alert( "ERROR.abort: audio=" + backend.audio + " be=" + backend);
	 hop_audio_invoke_listeners( backend.audio, "error", rest.car );
      } else if( k == Sclose ) {
	 // close
	 hop_audio_close( backend.audio );
	 hop_audio_invoke_listeners( backend.audio, "close", false );
      } else if( k == Smeta ) {
	 // meta (and playlist)
	 var val = rest.car;
	 backend.playlist = rest.cdr.car;

	 if( typeof val === "string" ) {
	    backend.current_metadata = {
	       title: sc_basename( val ),
	       artist: sc_basename( sc_dirname( sc_dirname( val ) ) ),
  	       album: sc_basename( sc_dirname( val ) )
	    };
	 } else if( val ) {
	    backend.current_metadata = val;
	 }
	    
	 hop_audio_invoke_listeners( backend.audio, "loadedmetadata",
				     backend.current_metadata );
      }
   } else {
      if( !e.value ) {
	 // the server has closed the connection, act as a plain stop action
	 hop_audio_invoke_listeners( backend.audio, "stop" );
      }
   }
      
   return;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_backend ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export audio-backend) (arity #t)) */
function hop_audio_backend( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );

   return ( audio.backend === audio.clientbackend ) ? Sclient : Sserver;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_backend_set ...                                        */
/*---------------------------------------------------------------------*/
/*** META ((export audio-backend-set!) (arity #t)) */
function hop_audio_backend_set( audio, backend ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );

   if( backend === Sclient ) {
      if( audio.clientbackend && audio.clientbackend !== audio.backend ) {
	 if( audio.backend ) hop_audio_stop( audio );
	 audio.backend = audio.clientbackend;
      }
      hop_audio_invoke_listeners( audio, "backend" );
   } else {
      if( audio.serverbackend ) {
	 if( audio.backend !== audio.serverbackend ) {
	    if( audio.backend ) hop_audio_stop( audio );
	    audio.backend = audio.serverbackend;
	 }
	 audio.serverbackend.url = backend;
	 hop_audio_invoke_listeners( audio, "backend" );
      } else {
	 backend = new HopAudioServerBackend( audio, backend );
	 audio.serverbackend = backend;
	 audio.backend = backend;
	 hop_audio_server_init( backend );
      }

      hop_audio_update( audio );
   }

   /* force the init of the audio */
   if( audio.id in hop_audio_init ) hop_audio_init[ audio.id ];
}

/*---------------------------------------------------------------------*/
/*    hop_audio_update ...                                             */
/*---------------------------------------------------------------------*/
function hop_audio_update( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );

   audio.backend.update();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_load ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export audio-load) (arity #t)) */
function hop_audio_load( audio, src, stream ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );

   audio.src = src;
   
   if( src ) {
      audio.backend.state = stream ? Splay : Sstop;
      hop_audio_invoke_listeners( audio, "progress" );

      return audio.proxy.load( src, stream );
   } else {
      audio.backend.state = Sstop;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_state ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export audio-state) (arity #t)) */
function hop_audio_state( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.backend.state;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_set ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist-set!) (arity #t)) */
function hop_audio_playlist_set( audio, playlist, autorun ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );

   audio.backend.playlist_set( playlist, autorun );
   return;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_get ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist) (arity #t)) */
function hop_audio_playlist_get( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.backend.playlist_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_index ...                                     */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist-index) (arity #t)) */
function hop_audio_playlist_index( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.backend.playlist_index_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_play ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export audio-play audio-start) (arity #t)) */
function hop_audio_play( audio, start ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );

   audio.backend.playurl( audio.src, start );

   return true;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_playlist_play ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export audio-playlist-play) (arity #t)) */
function hop_audio_playlist_play( audio, i ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   
   var pl = hop_audio_playlist_get( audio );

   if( (i >= 0) && (i < sc_length( pl )) ) {
      audio.backend.playlist_play( i );
   } else {
      if( (sc_length( pl ) == 0) && (i == 0) && audio.src ) {
	 audio.backend.playurl( audio.src, 0 );
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
	 audio.backend.playlist_play( i - 1 );
      } else {
	 audio.backend.playlist_play( i );
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

   if( i < sc_length( pl ) - 1 ) {
      audio.backend.playlist_play( i + 1 );
   } else {
      audio.backend.stop();
   }

   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_audio_close ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export audio-close) (arity #t)) */
function hop_audio_close( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   
   audio.src = false;
   audio.backend.close();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_stop ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export audio-stop) (arity #t)) */
function hop_audio_stop( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );

   audio.src = false;
   audio.backend.stop();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_pause ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export audio-pause) (arity #t)) */
function hop_audio_pause( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );

   audio.backend.pause();
   
   hop_audio_invoke_listeners( audio, "pause" );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_volume ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export audio-volume) (arity #t)) */
function hop_audio_volume( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.backend.volume_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_volume_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export audio-volume-set!) (arity #t)) */
function hop_audio_volume_set( audio, vol ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.backend.volume_set( vol );
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
   return audio.backend.pan_get();
}

/*---------------------------------------------------------------------*/
/*    hop_audio_pan_set ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export audio-pan-set!) (arity #t)) */
function hop_audio_pan_set( audio, pan ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.backend.pan_set( pan );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_duration ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export audio-duration) (arity #t)) */
function hop_audio_duration( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.backend.duration_get();
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_current_time ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export audio-current-time) (arity #t)) */
function hop_audio_current_time( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.backend.position_get();
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
   
   return audio.backend.position_set( pos );
}
   
/*---------------------------------------------------------------------*/
/*    hop_audio_metadata ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export audio-metadata) (arity #t)) */
function hop_audio_metadata( audio ) {
   if( typeof audio === "string" ) audio = document.getElementById( audio );
   return audio.backend.metadata_get();
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
   var id = audio.controls.id;

   hop_audio_controls_metadata( audio, true );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onerror ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onerror( evt ) {
   var audio = evt.audio;
   var id = audio.controls.id;
   var tl = document.getElementById( id + "-controls-metadata-song" );
   var min = document.getElementById( id + "-controls-status-length-min" );
   var sec = document.getElementById( id + "-controls-status-length-sec" );

   tl.className = "hop-audio-panel-metadata-error";
   tl.innerHTML = evt.value ? evt.value : "Error";

   hop_audio_time_interval_clear( audio );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onprogress ...                                */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onprogress( evt ) {
   var audio = evt.audio;
   var id = audio.controls.id;
   var tl = document.getElementById( id + "-controls-metadata-song" );

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
   var id = audio.controls.id;
   var pos = document.getElementById( id + "-controls-status-position" );
   var ctime = hop_audio_current_time( audio );
   
   audio.ctime = ctime;
   audio.min = Math.floor( ctime / 60 );
   audio.sec = ctime % 60;

   hop_audio_time_interval_clear( audio );
   pos.innerHTML = int2( audio.min ) + ":" + int2( audio.sec );

   audio.interval = setInterval( function() {
	 if( audio.backend.state === Splay ) {
	    audio.ctime++;
	    audio.sec++;
	    if( audio.sec == 60 ) {
	       audio.min++; audio.sec = 0;
	       hop_audio_update( audio );
	    };
	    pos.innerHTML = int2( audio.min ) + ":" + int2( audio.sec );
	 }
      }, 1000 );
   
   pos.className = "hop-audio-info-status-position-on-play";
}

/*---------------------------------------------------------------------*/
/*    hop_audio_time_interval_clear ...                                */
/*---------------------------------------------------------------------*/
function hop_audio_time_interval_clear( audio ) {
   var id = audio.controls.id;
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
   var id = audio.controls.id;
   var tl = document.getElementById( id + "-controls-metadata-song" );
   var ab = document.getElementById( id + "-controls-metadata-album" );
   var at = document.getElementById( id + "-controls-metadata-artist" );
   var ye = document.getElementById( id + "-controls-metadata-year" );

   if( reset ) {
      ab.innerHTML = "";
      at.innerHTML = "";
      ye.innerHTML = "";
   } else {
      var md = audio.backend.metadata_get();

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
/*    hop_audio_controls_onloadedmetadata ...                          */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onloadedmetadata( evt ) {
   var audio = evt.audio;
   var id = audio.controls.id;
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
   var id = audio.controls.id;
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
      var ab = document.getElementById( id + "-controls-metadata-album" );
      ab.innerHTML = (evt.value ? evt.value : "backend closed...");
   } else {
      tl.className = "hop-audio-panel-metadata-close";
      tl.innerHTML = evt.value ? evt.value + " closed..." : "Backend closed...";
   }
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onplay ...                                    */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onplay( evt ) {
   var audio = evt.audio;
   var id = audio.controls.id;
   var status = document.getElementById( id + "-controls-status-img" );
   var track = document.getElementById( id + "-controls-status-track" );
   var min = document.getElementById( id + "-controls-status-length-min" );
   var sec = document.getElementById( id + "-controls-status-length-sec" );
   var alen = hop_audio_duration( audio );
   var plen = sc_length( hop_audio_playlist_get( audio ) );
   var tl = document.getElementById( id + "-controls-metadata-song" );
   var i = hop_audio_playlist_index( audio );

   status.className = "hop-audio-info-status-img hop-audio-info-status-play";

   track.className = "hop-audio-info-status-track-on-play";
   if( plen > 0 ) {
      track.innerHTML = int2( 1 + i ) + "/" + int2( plen );
   } else {
      track.innerHTML = "01/01";
   }

   min.innerHTML = int2( Math.floor( alen / 60 ) );
   sec.innerHTML = int2( alen % 60 );

   if( i > 0 ) tl.innerHTML = "Track " + i;
   
   hop_audio_controls_metadata( audio, false );
   hop_audio_time_interval_set( audio );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onpause ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onpause( evt ) {
   var audio = evt.audio;
   var id = audio.controls.id;
   var status = document.getElementById( id + "-controls-status-img" );

   audio.old_className = status.className;
   status.className = "hop-audio-info-status-img hop-audio-info-status-pause";
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onstop ...                                    */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onstop( evt ) {
   var audio = evt.audio;
   var id = audio.controls.id;
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
/*    hop_audio_controls_onbackend ...                                 */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onbackend( evt ) {
   var audio = evt.audio;
   var id = audio.controls.id;
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

   tl.className = "hop-audio-panel-metadata-backend";
   tl.innerHTML = evt.value ?
      evt.value + " ready..." :
      "Backend ready...";

   status.className = "hop-audio-info-status-img hop-audio-info-status-stop";
}

/*---------------------------------------------------------------------*/
/*    hop_audio_controls_onended ...                                   */
/*---------------------------------------------------------------------*/
function hop_audio_controls_onended( evt ) {
   var audio = evt.audio;
   var id = audio.controls.id;
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
   var id = audio.controls.id;
   var slider = document.getElementById( id + "-controls-volume" );

   hop_slider_value_set( slider, hop_audio_volume( audio ) );
}

/*---------------------------------------------------------------------*/
/*    hop_audio_gui_playlist ...                                       */
/*---------------------------------------------------------------------*/
function hop_audio_gui_playlist( id ) {
}
