/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/share/hop-event.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 20 07:19:56 2007                          */
/*    Last change :  Sun Apr 11 07:16:28 2021 (serrano)                */
/*    Copyright   :  2007-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop event machinery.                                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    window event listener ...                                        */
/*---------------------------------------------------------------------*/
var hop_is_ready = false;
var hop_ready_timeout = 10;
var hop_elements_ready_counter = 0;
var hop_window_ready_list = null;

var hop_reconnect_max_wait = 60 * 1000 * 10;

/*---------------------------------------------------------------------*/
/*    HopEvent ...                                                     */
/*---------------------------------------------------------------------*/
function HopEvent( n, v ) {
   this.isStopped = false;
   this.preventDefault = function() { ; };
   this.stopPropagation = this.preventDefault;
   this.name = n;
   this.type = n;
   this.value = v;
   return this;
}

/*---------------------------------------------------------------------*/
/*    hop_event_stoppedp ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export event-stopped?) (arity 1))) */
function hop_event_stoppedp( e ) {
   return e.isStopped == true;
}

/*---------------------------------------------------------------------*/
/*    hop_add_event_listener ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export add-event-listener!) (arity -3))) */
function hop_add_event_listener( obj, event, proc, capture ) {
   var p = proc;

   if( hop_debug() > 0 ) {
      var msg = obj + "<-" + event;
      p = hop_callback( proc, hop_callback_listener_context( msg ) );
   }
   
   if( event === "timeout" )
      return hop_add_timeout_listener( obj, p );

   if( event === "ready" ) {
      proc.handler = p;
      proc.handler.enable = true;

      if( obj === window ) {
	 if( hop_is_ready ) {
	    var evt = new HopEvent( "ready", window )
	    evt.target = window;
	    
	    window.ready = p;
	    return window.ready( evt );
	 } else {
	    hop_window_ready_list = sc_cons( p, hop_window_ready_list );
	    return proc;
	 }
      } else if( obj === hop_server ) {
	 return hop_add_serverready_listener( obj, p );
      } else if( typeof obj === "string" ) {
	 return hop_add_ready_listener( obj, p, 20 );
      } else {
	 return sc_error( "add-event-listener!",
			  "Illegal \"ready\" event for object",
			  obj );
      }
   }

   /* store the actual listener for listener removal */
   if( p != proc ) {
      if( proc[ obj ] == undefined ) proc[ obj ] = new Object();
      proc[ obj ][ event ] = p;
   }
   
   if( ("hop_add_event_listener" in obj) &&
       (obj != window) &&
       (obj.hop_add_event_listener != hop_add_event_listener) ) {
      return obj.hop_add_event_listener( event, p, capture );
   }

   if( event === "hashchange" && !hop_config.hashchange_event ) {
      return hop_add_hashchange_listener( obj, p );
   }

   return hop_add_native_event_listener( obj, event, p, capture );
}

/*---------------------------------------------------------------------*/
/*    hop_remove_event_listener ...                                    */
/*---------------------------------------------------------------------*/
/*** META ((export remove-event-listener!) (arity -4)) */
function hop_remove_event_listener( obj, event, proc, capture ) {
   var p = ( (obj in proc) && proc[ obj ] && proc[ obj ][ event ] ) ?
      proc[ obj ][ event ] : proc;
   
   if( event === "timeout" )
      return hop_remove_timeout_listener( p );

   if( event === "ready" ) {
      if( obj === hop_server ) {
	 return hop_remove_serverready_listener( obj, p );
      } else if( p.handler ) {
	 p.handler.enable = false;
	 return;
      }
   }
   
   if( (obj.hop_remove_event_listener != undefined) &&
      (obj.hop_remove_event_listener != hop_remove_event_listener) ) {
      return obj.hop_remove_event_listener( event, p, capture );
   }

   if( event === "hashchange" && !hop_config.hashchange_event ) {
      return hop_remove_hashchange_listener( obj, p );
   }

   /* remove the debug instrumented listener */
   return hop_remove_native_event_listener( obj, event, p, capture );
} 

/*---------------------------------------------------------------------*/
/*    hop_add_ready_listener ...                                       */
/*---------------------------------------------------------------------*/
function hop_add_ready_listener( obj, proc, ttl ) {
   var el = document.getElementById( obj );

   hop_elements_ready_counter++;
   
   if( !el ) {
      if( ttl > 0 ) {
	 setTimeout( function() {
	    return hop_add_ready_listener( obj, proc, ttl - 1 );
	 }, hop_ready_timeout );
      } else {
	 hop_elements_ready_counter--;
	 return sc_error( "add-event-listener!",
			  "Timeout when getting \"ready\" object",
			  obj );
      }
   } else {
      hop_elements_ready_counter--;

      if( proc.enable ) {
	 var evt = new HopEvent( "ready", el );
	 evt.target = el;
	 el.ready = proc;
	 el.ready( evt );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_get_hashchange_interval ...                                  */
/*---------------------------------------------------------------------*/
function hop_get_hashchange_interval() {
   if( hop_hashchange_interval ) {
      return hop_hashchange_interval;
   } else {
      window.hop_hashchange_href = window.location.href;
      var e = new HopEvent( "hashchange", window );
      
      var interval = function() {
	 if( window.hop_hashchange_href !== window.location.href ) {
	    if( window.hop_hashchange_href != window.location.href ) {
	       var l = obj.hop_hashchange_listener;
	       window.hop_hashchange_href = window.location.href;
	       
	       while( sc_isPair( l ) ) {
		  l.__hop_car( window.location )( e );
		  l = l.__hop_cdr;
	       }
	    }
	 }
	 return true;
      }
	 
      hop_hashchange_interval =
	 setInterval( interval, hop_hashchange_timeout );
      return hop_hashchange_interval;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_add_hashchange_listener ...                                  */
/*---------------------------------------------------------------------*/
function hop_add_hashchange_listener( obj, proc ) {
   obj.hop_hashchange_proc = proc;

   var check = function() {
      if( obj.hop_hashchange_href !== window.location.href ) {
	 if( obj.hop_hashchange_href != window.location.href ) {
	    obj.hop_hashchange_href = window.location.href;
	    obj.hop_hashchange_proc( window.location );
	 }
      }
      return true;
   }

   obj.hop_hashchange_listeners =
      sc_cons( sc_cons( proc, setInterval( check, hop_hashchange_timeout ) ),
	       ( "hop_hashchange_listeners" in obj ) ?
	       hop_hashchange_listeners : null );

   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_remove_active_hashchange_listener ...                        */
/*---------------------------------------------------------------------*/
function hop_remove_active_hashchange_listener( obj, proc ) {
   if( sc_isPair( obj.hop_hashchange_listener ) ) {
      var c = sc_assq( proc, obj.hop_hashchange_listener );

      if( c ) {
	 clearInterval( c.__hop_cdr );
	 obj.hop_hashchange_listener =
	    sc_deleteBang( c, obj.hop_hashchange_listener );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_hashchange_set ...                                           */
/*---------------------------------------------------------------------*/
function hop_hashchange_set( obj, href ) {
   window.location.href = href;
}

/*---------------------------------------------------------------------*/
/*    hop_servevt_id ...                                               */
/*---------------------------------------------------------------------*/
var hop_servevt_id = "__hop_serevt_proxy";
var hop_server_event_count = 0;
var re = new RegExp( "<[\/]?event[^>]*>", "g" );

/*---------------------------------------------------------------------*/
/*    HopServerEvent ...                                               */
/*---------------------------------------------------------------------*/
function HopServerEvent( n, text, val ) {
   this.isStopped = false;
   this.name = n;
   this.type = n;
   this.value = val;
   this.id = hop_server_event_count++;
   this.preventDefault = function() { ; };
   this.stopPropagation = this.preventDefault;
   
   if( typeof text == "string" ) {
      this.responseText = text.replace( re, "" );
   } else {
      this.responseText = "";
   }
}

/*---------------------------------------------------------------------*/
/*    hop_servevt_proxy ...                                            */
/*---------------------------------------------------------------------*/
var hop_servevt_proxy = false;

/*---------------------------------------------------------------------*/
/*    hop_servevt_table ...                                            */
/*---------------------------------------------------------------------*/
var hop_servevt_table = {};
var hop_servevt_ctable = {};
var hop_servevt_dlist = null;

var hop_servevt_envelope_re =
   new RegExp( "^<([rsxifj]) name='([^']+)'>((?:.|[\n])*)</[rsxifj]>$" );
var hop_servevt_envelope_cdata_re =
   new RegExp( "^<!\\[CDATA\\[((?:.|[\n])*)\\]\\]>$" );

/*---------------------------------------------------------------------*/
/*    hop_servevt_envelope_parse_error ...                             */
/*---------------------------------------------------------------------*/
function hop_servevt_envelope_parse_error( xhr ) {
   var exc = new Error( "bad server event envelope" );
   
   exc.name = "HopServerError";
   exc.scObject = xhr;
   exc.message = xhr.responseText === "" ? "Empty envelope" : xhr.responseText;
   exc.scOffset = 2;

   hop_callback_handler( exc, xhr.precontext );
}

/*---------------------------------------------------------------------*/
/*    hop_servevt_envelope_parse ...                                   */
/*---------------------------------------------------------------------*/
function hop_servevt_envelope_parse( val, xhr ) {
   var m = val.match( hop_servevt_envelope_re );

   if( m != null ) {
      var k = m [ 1 ];
      var id = decodeURIComponent( m[ 2 ] );
      var text = m[ 3 ];

      try {
	 if( k === "i" ) {
	    hop_trigger_servevt( id, text, parseInt( text ), false );
	 } else if( k == "f" ) {
	    hop_trigger_servevt( id, text, parseFloat( text ), false );
	 } else if( k == "s" ) {
	    hop_trigger_servevt( id, text, unescape( text ), false );
	 } else if( k == "x" ) {
	    hop_trigger_servevt( id, text, hop_create_element( text ), false );
	 } else if( k == "j" ) {
	    var t = text.match( hop_servevt_envelope_cdata_re );
	    if( t ) {
	       hop_trigger_servevt( id, t[ 1 ], hop_url_encoded_to_obj( t[ 1 ] ), false );
	    }
	 } else if( k == "r" ) {
	    // register, first event listener added to the server
	    hop_trigger_serverready_event();
	 } else {
	    hop_servevt_envelope_parse_error( xhr );
	 }
      } catch( e ) {
	 var ctx = hop_callback_listener_context( "server<-" + id );
	    
	 hop_callback_handler( e, ctx );
      }
   } else {
      hop_servevt_envelope_parse_error( xhr );
   }
}

/*---------------------------------------------------------------------*/
/*    start_servevt_websocket_proxy ...                                */
/*---------------------------------------------------------------------*/
function start_servevt_websocket_proxy( key, host, port, ssl ) {
   var reconnect_debug = 0;
   
   function open_websocket( url ) {
      var ws = new WebSocket( url );

      ws.onopen = function() {
	 // after a reconnection, the onerror listener must be removed
	 ws.onerror = undefined;
	 ws.registry = {};

	 if( reconnect_debug > 0 ) {
	    console.log( "reconnect onopen" );
	 }
	 
	 // we are ready to register now
	 hop_server.state = "open";
	 hop_servevt_proxy.register = register;
	 hop_servevt_proxy.unregister = unregister;
	 hop_servevt_proxy.reconnect = reconnect;
	 hop_servevt_proxy.reconnect_url = url;
	 hop_servevt_proxy.websocket = ws;
	 hop_servevt_proxy.opentime = Math.round((new Date()).getTime() / 1000);
	 
	 if( reconnect_debug > 0 ) {
	    var s = "";
	    for( var p in hop_servevt_table ) {
	       if( sc_isPair( hop_servevt_table[ p ] ) ) {
		  s += p + " ";
	       }
	    }
	    
	    if( reconnect_debug > 0 ) {
	       console.log( "reconnect register: " + s );
	    }
	 }
	 
	 // register the unitialized events
	 for( var p in hop_servevt_table ) {
	    if( sc_isPair( hop_servevt_table[ p ] ) ) {
	       register( p );
	    }
	 }

	 if( reconnect_debug > 0 ) {
	    console.log( "reconnect trigger ready" );
      	    reconnect_debug = 0;
	 }
	 hop_trigger_serverready_event();
	 
	 reconnect_debug = 0;
      }
      
      ws.onerror = function( e ) {
	 if( reconnect_debug > 0 ) {
	    console.log( "reconnect error" );
      	    reconnect_debug = 0;
	 }
	 hop_server.state = "error";
	 hop_serverready_triggered = false
	 hop_servevt_onclose();
	 throw e;
      }
	 
      ws.onclose = function( e ) {
      	 reconnect_debug = 0;
	 hop_server.state = "close";
	 hop_serverready_triggered = false
	 hop_servevt_onclose();
      }
      
      ws.onmessage = function ( e ) {
      	 reconnect_debug = 0;
	 e.responseText = e.data;
	 hop_servevt_envelope_parse( e.data, e );
      }

      return ws;
   }
      
   var register = function( id ) {
      if( !(id in hop_servevt_proxy.websocket.registry) ) {
	 hop_servevt_proxy.websocket.registry[ id ] = true;
	 var encid = encodeURIComponent( id );
	 var svc = window.hop.serviceBase +
	     "/public/server-event/register?event=" + encid +
	    "&key=" + key  + "&mode=websocket";

	 if( reconnect_debug > 0 ) {
	    console.log( "re-registering: " + id );
	 }
	 hop_send_request( svc, false, function() { ; }, false, false, [] );
      }
   };

   var unregister = function( id ) {
      if( id in hop_servevt_proxy.websocket.registry ) {
	 delete hop_servevt_proxy.websocket.registry[ id ];
	 
	 var encid = encodeURIComponent( id );
	 var svc = window.hop.serviceBase +
	    "/public/server-event/unregister?event=" + encid +
	    "&key=" + key;
	 
	 hop_send_request( svc, false, function() { ; }, false, false, [] );
      }
   };

   var reconnect = function( wait, max ) {
      if( hop_server.state !== "reconnect" ) {
	 hop_server.state = "reconnect";

	 // reconnect_debug = 10;
	 open_websocket( hop_servevt_proxy.reconnect_url );
      
	 hop_servevt_proxy.websocket.onerror = function( e ) {
	    console.log( "reconnection error..." );
	    if( !wait ) wait = 1000;

	    if( max == -1 ) {
	       hop_trigger_servererror_event( "Cannot reconnect" );
	    } else {
	       sc_after( wait, function() {
		  var nwait = wait < hop_reconnect_max_wait ? wait * 2 : wait;
		  var nmax = max === undefined ? max : max - 1;
		  reconnect( nwait, nmax );
	       } );
	    }
	 }
      }
   }

   if( !hop_servevt_proxy.websocket ) {
      var url = (ssl ? "wss://" : "ws://") + host + ":" + port +
	 window.hop.serviceBase + "/public/server-event/websocket?key=" + key;

      hop_servevt_proxy.key = key;
      hop_servevt_proxy.host = host;
      hop_servevt_proxy.port = port;

      open_websocket( url );
   }
}

/*---------------------------------------------------------------------*/
/*    start_servevt_xhr_multipart_proxy ...                            */
/*---------------------------------------------------------------------*/
function start_servevt_xhr_multipart_proxy( key ) {
   if( !hop_servevt_proxy.httpreq ) {
      var server_ready = false;
      var registry = {};

      var register = function( id ) {
	 if( !(id in registry) ) {
	    registry[ id ] = true;
	    
	    var encid = encodeURIComponent( id );
	    var svc = window.hop.serviceBase +
	       "/public/server-event/register?event=" + encid +
	       "&key=" + key  + "&mode=xhr-multipart";
	    
	    var success = function( val, xhr ) {
	       hop_servevt_envelope_parse( val, xhr );
	    }

	    var failure = function( xhr ) {
	       if( xhr.exception ) {
		  if( typeof hop_callback_handler === "function" ) {
		     hop_callback_handler( xhr.exception, xhr.precontext );
		  }
	       }
	    
	       if( "hop_servevt_onclose" in window )
		  hop_servevt_onclose();
	    }

	    var req = hop_make_xml_http_request();
	    req.multipart = true;

	    hop_servevt_proxy.httpreq =
	       hop_send_request( svc,
				 // asynchronous call
				 false,
				 // success callback
				 success,
				 // failure callback
				 failure,
				 // no anim
				 false,
				 // no environment
				 [],
				 // no authentication
				 false,
				 // no timeout
				 false,
				 // xhr request
				 req );
	    }
      };

      var unregister = function( id ) {
	 if( id in registry ) {
	    delete registry[ id ];
	    
	    hop_servevt_proxy.httpreq.abort();

	    var encid = encodeURIComponent( id );
	    var svc = window.hop.serviceBase +
	       "/public/server-event/unregister?event=" + encid +
   	       "&key=" + hop_servevt_proxy.key;
	 
	    hop_servevt_proxy.httpreq = hop_send_request( svc, false,
							  function() { ; }, false,
							  false, [] );
	 }
      };

      // complete the proxy definition
      hop_servevt_proxy.register = register;
      hop_servevt_proxy.unregister = unregister;

      // register the unitialized events
      for( var p in hop_servevt_table ) {
	 if( sc_isPair( hop_servevt_table[ p ] ) ) {
	    register( p );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_servevt_signal ...                                           */
/*    -------------------------------------------------------------    */
/*    This has to be a global function because it is called from       */
/*    the toplevel.                                                    */
/*---------------------------------------------------------------------*/
function hop_servevt_signal( val ) {
   // null is used as a marker for an abandonned connection
   if( val != null ) {
      // invoke all the user handlers (we have received a list of
      // values corresponding to server buffer).
      while( sc_isPair( val ) ) {
	 var v = val.__hop_car;
	 var id = v.__hop_car;
	 var vals = v.__hop_cdr;

	 while( vals != null ) {
	    hop_trigger_servevt( id, vals.__hop_car, vals.__hop_car, false );
	    vals = vals.__hop_cdr;
	 }

	 val = val.__hop_cdr;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_servevt_onerror ...                                          */
/*---------------------------------------------------------------------*/
function hop_servevt_onerror( msg ) {
   sc_error( "servevt", msg, "internal server event error" );
}

/*---------------------------------------------------------------------*/
/*    servevt_websocketp ...                                           */
/*---------------------------------------------------------------------*/
function servevt_websocketp() {
   return hop_config.websocket;
}

/*---------------------------------------------------------------------*/
/*    servevt_xhr_multipartp ...                                       */
/*---------------------------------------------------------------------*/
function servevt_xhr_multipartp() {
   return hop_config.xhr_multipart;
}

/*---------------------------------------------------------------------*/
/*    servevt_scriptp ...                                              */
/*---------------------------------------------------------------------*/
function servevt_scriptp() {
   return true;
}

/*---------------------------------------------------------------------*/
/*    hop_start_servevt_proxy ...                                      */
/*    -------------------------------------------------------------    */
/*    http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol      */
/*    for details on the Web protocol.                                 */
/*---------------------------------------------------------------------*/
function hop_start_servevt_proxy() {
   hop_server.state = "start";

   hop_servevt_proxy = new Object();
   hop_servevt_proxy.websocket = false;
   hop_servevt_proxy.script = false;
   hop_servevt_proxy.register = function( x ) {};

   hop_send_request(
      window.hop.serviceBase + "/public/server-event/info",
      // asynchronous call
      false,
      // success callback
      function( v ) {
	 var host = v[ 0 ];
	 var port = v[ 1 ];
	 var key = v[ 2 ];
	 var ssl = v[ 3 ];
	 if( servevt_websocketp() ) {
	    // websocket backend
	    start_servevt_websocket_proxy( key, host, port, ssl );
	 } else if( servevt_xhr_multipartp() ) {
	    // xhr_multipart backend
	    start_servevt_xhr_multipart_proxy( key );
	 }

	 hop_add_native_event_listener(
	    window,
	    "online",
	    function() {
	       hop_server.reconnect( 1000, 5 );
	    },
	    false );
      },
      // failure callback
      function( xhr ) {
	 if( xhr.exception ) {
	    throw xhr.exception;
	 } else {
	    throw new Error( "No event server acknowledge" );
	 }
      },
      // run the anim during the call
      true,
      // no environment
      [] );
}

/*---------------------------------------------------------------------*/
/*    hop_trigger_servevt ...                                          */
/*    -------------------------------------------------------------    */
/*    This function is invoked upon event reception                    */
/*---------------------------------------------------------------------*/
function hop_trigger_servevt( id, text, value, js ) {
   var proc;

   try {
      var v = (js ? eval( value ) : value);
      var evt = new HopServerEvent( id, text, v );
      var p2 = hop_servevt_table[ id ];

      if( sc_isPair( hop_servevt_dlist ) &&
	  sc_isPair( hop_servevt_ctable[ id ] ) ) {
	 // the event is captured and we have document listener bound
	 var p1 = hop_servevt_dlist;

	 while( sc_isPair( p1 ) ) {
	    proc = p1.__hop_car;
	    proc( evt );
	    p1 = p1.__hop_cdr;
	 }
      }

      evt.isStopped = false;

      while( sc_isPair( p2 ) ) {
	 try {
	    proc = p2.__hop_car;
	    proc( evt );
	 } catch( exc ) {
	    throw exc;
	 }
	 if( evt.isStopped ) break;
	 p2 = p2.__hop_cdr;
      }
   } catch( exc ) {
      if( proc && ("displayName" in proc) ) {
	 var c = sc_assoc( "hop_trigger_servevt", hop_name_aliases );

	 if( sc_isPair( c ) ) {
	    sc_setCdrBang( c, proc.displayName );
	 }
      }
      throw exc;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_add_server_event_listener ...                                */
/*---------------------------------------------------------------------*/
function hop_add_server_event_listener( event, proc, capture ) {
   if( event === "down" ) {
      return hop_add_serverdown_listener( this, proc );
   } else if( event === "ready" ) {
      hop_add_serverready_listener( this, proc );
   } else if( event === "error" ) {
      hop_add_servererror_listener( this, proc );
   } else {
      return hop_add_server_listener( event, proc, capture );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_remove_server_event_listener ...                             */
/*---------------------------------------------------------------------*/
function hop_remove_server_event_listener( event, proc, capture ) {
   if( event === "down" ) {
      return hop_remove_serverdown_listener( this, proc );
   } else if( event === "ready" ) {
      hop_remove_serverready_listener( this, proc );
   } else if( event === "error" ) {
      hop_remove_servererror_listener( this, proc );
   } else {
      return hop_remove_server_listener( event, proc, capture );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_serverdown_list ...                                          */
/*---------------------------------------------------------------------*/
var hop_serverdown_list = null;
var hop_serverdown_triggered = false;

/*---------------------------------------------------------------------*/
/*    hop_add_serverdown_listener ...                                  */
/*---------------------------------------------------------------------*/
function hop_add_serverdown_listener( obj, proc ) {
   if( obj === hop_server || obj === document ) {
      if( hop_serverdown_triggered ) {
	 // the server is close
	 var evt = new HopServerEvent( "serverdown", false, false );
	 proc( evt );
      } else {
	 // the server is not closed yet, we register the callback
	 hop_serverdown_list = sc_cons( proc, hop_serverdown_list );
      }
   } else {
      throw new Error( "add-event-listener!: Illegal `serverdown' recipient"
		       + obj );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_remove_serverdown_listener ...                               */
/*---------------------------------------------------------------------*/
function hop_remove_serverdown_listener( obj, proc ) {
   if( obj === hop_server || obj === document ) {
      hop_serverdown_list = sc_remqBang( proc, hop_serverdown_list );
      return true;
   } else {
      throw new Error( "remove-event-listener!: Illegal `serverdown' recipient"
		       + obj );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_servevt_onclose ...                                          */
/*    -------------------------------------------------------------    */
/*    This function is invoked by WebSocket, on connection close.      */
/*---------------------------------------------------------------------*/
function hop_servevt_onclose() {
   // allocate a new event in order to hide handler side effects
   var evt = new HopServerEvent( "serverdown", false, false );
   var p = hop_serverdown_list;

   while( sc_isPair( p ) ) {
      p.__hop_car( evt );
      if( evt.isStopped ) break;
      p = p.__hop_cdr;
   }

   hop_serverdown_triggered = true;
}

/*---------------------------------------------------------------------*/
/*    hop_add_server_listener ...                                      */
/*---------------------------------------------------------------------*/
function hop_add_server_listener( event, proc, capture ) {
   if( typeof proc != "function" ) {
      throw new Error( "Illegal procedure: " + proc );
   }

   if( !document.body ) {
      // delay until the document is fully built
      hop_add_event_listener(
	 window, "ready", 
	 function( e ) {
	    hop_add_server_listener( event, proc, capture );
	 } );
   } else {
      var o = hop_servevt_table[ event ];
      
      hop_servevt_table[ event ] = sc_cons( proc, sc_isPair( o ) ? o : null );

      if( capture ) {
	 var o = hop_servevt_ctable[ event ];
	 hop_servevt_ctable[ event ] = sc_cons( proc, sc_isPair( o ) ? o : null );
      }
      
      if( !hop_servevt_proxy ) {
	 hop_start_servevt_proxy();
      } else {
	 hop_servevt_proxy.register( event );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_remove_server_listener ...                                   */
/*---------------------------------------------------------------------*/
function hop_remove_server_listener( event, proc, capture ) {
   // unregister the event listener
   if( sc_isPair( hop_servevt_table[ event ] ) ) {
      hop_servevt_table[ event ] =
	 sc_remqBang( proc, hop_servevt_table[ event ] );
   }
   if( sc_isPair( hop_servevt_ctable[ event ] ) ) {
      hop_servevt_ctable[ event ] =
	 sc_remqBang( proc, hop_servevt_ctable[ event ] );
   }

   // try to close the socket
   for( var id in hop_servevt_table ) {
      if( sc_isPair( hop_servevt_table[ event ] ) ) {
	 return;
      }
   }

   // no new events expected, close the connection
   hop_servevt_proxy.unregister( event );
}

/*---------------------------------------------------------------------*/
/*    hop_serverready_list ...                                         */
/*---------------------------------------------------------------------*/
var hop_serverready_list = null;
var hop_serverready_triggered = false;

/*---------------------------------------------------------------------*/
/*    HopServerReadyEvent ...                                          */
/*---------------------------------------------------------------------*/
function HopServerReadyEvent() {
   var o = new Object();
   o.isStopped = false;
   
   return o;
}

/*---------------------------------------------------------------------*/
/*    hop_trigger_serverready_event ...                                */
/*---------------------------------------------------------------------*/
function hop_trigger_serverready_event() {
   if( !hop_serverready_triggered ) {
      var evt = new HopServerReadyEvent();
      var l = hop_serverready_list;
      
      hop_serverready_triggered = true;

      while( sc_isPair( l ) ) {
	 l.__hop_car( evt );
	 if( evt.isStopped ) break;
	 l = l.__hop_cdr;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_add_serverready_listener ...                                 */
/*---------------------------------------------------------------------*/
function hop_add_serverready_listener( obj, proc ) {
   if( obj === hop_server || obj === document ) {
      if( hop_serverready_triggered ) {
	 // the server is ready
	 proc();
      }
      hop_serverready_list = sc_cons( proc, hop_serverready_list );
   } else {
      throw new Error( "add-event-listener!: Illegal `serverready' recipient"
		       + obj );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_remove_serverready_listener ...                              */
/*---------------------------------------------------------------------*/
function hop_remove_serverready_listener( obj, proc ) {
   if( obj === hop_server || obj === document ) {
      hop_serverready_list = sc_remqBang( proc, hop_serverready_list );
      return true;
   } else {
      throw new Error( "remove-event-listener!: Illegal `serverready' recipient"
		       + obj );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_servererror_list ...                                         */
/*---------------------------------------------------------------------*/
var hop_servererror_list = null;

/*---------------------------------------------------------------------*/
/*    HopServerErrorEvent ...                                          */
/*---------------------------------------------------------------------*/
function HopServerErrorEvent( v ) {
   var o = new Object();
   o.isStopped = false;
   o.value = v;
   
   return o;
}

/*---------------------------------------------------------------------*/
/*    hop_trigger_servererror_event ...                                */
/*---------------------------------------------------------------------*/
function hop_trigger_servererror_event( v ) {
   var evt = new HopServerErrorEvent( v);
   var l = hop_servererror_list;
      
   while( sc_isPair( l ) ) {
      l.__hop_car( evt );
      if( evt.isStopped ) break;
      l = l.__hop_cdr;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_add_servererror_listener ...                                 */
/*---------------------------------------------------------------------*/
function hop_add_servererror_listener( obj, proc ) {
   if( obj === hop_server || obj === document ) {
      hop_servererror_list = sc_cons( proc, hop_servererror_list );
   } else {
      throw new Error( "add-event-listener!: Illegal `servererror' recipient"
		       + obj );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_remove_servererror_listener ...                              */
/*---------------------------------------------------------------------*/
function hop_remove_servererror_listener( obj, proc ) {
   if( obj === hop_server || obj === document ) {
      hop_servererror_list = sc_remqBang( proc, hop_servererror_list );
      return true;
   } else {
      throw new Error( "remove-event-listener!: Illegal `servererror' recipient"
		       + obj );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_timeout_listeners ...                                        */
/*---------------------------------------------------------------------*/
var hop_timeout_listeners = null;

/*---------------------------------------------------------------------*/
/*    hop_add_timeout_listener ...                                     */
/*---------------------------------------------------------------------*/
function hop_add_timeout_listener( obj, proc ) {
   hop_timeout_listeners =
      sc_cons( sc_cons( proc, setInterval( proc, obj ) ),
	       hop_timeout_listeners );
}

/*---------------------------------------------------------------------*/
/*    hop_remove_timeout_listener ...                                  */
/*---------------------------------------------------------------------*/
function hop_remove_timeout_listener( proc ) {
   var p = hop_timeout_listeners;
   
   if( sc_isPair( p ) ) {
      if( p.__hop_car.__hop_car === proc ) {
	 clearInterval( p.__hop_car.__hop_cdr );
	 hop_timeout = p.__hop_cdr;
      } else {
	 while( sc_isPair( p.__hop_cdr ) ) {
	    if( p.__hop_cdr.__hop_car === proc ) {
	       clearInterval( p.__hop_cdr.__hop_cdr );
	       p.__hop_cdr = p.__hop_cdr.__hop_cdr;
	       break;
	    } else {
	       p = p.__hop_cdr;
	    }
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop window ready listener ...                                    */
/*---------------------------------------------------------------------*/
hop_add_native_event_listener(
   window, "load", function( evt ) {
      var i = setInterval( function() {
	 if( hop_elements_ready_counter == 0 ) {
	    clearInterval( i );
	    hop_is_ready = true;

	    while( sc_isPair( hop_window_ready_list ) ) {
	       if( hop_window_ready_list.__hop_car.enable ) {
		  window.ready = hop_window_ready_list.__hop_car;
		  window.ready( evt );
	       
		  if( evt.isStopped ) break;
	       }
	       hop_window_ready_list = hop_window_ready_list.__hop_cdr;
	    }
	 }
      }, hop_ready_timeout + 1 );
   } );
      
