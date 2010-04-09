/*=====================================================================*/
/*    serrano/prgm/project/hop/2.1.x/share/hop-event.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 20 07:19:56 2007                          */
/*    Last change :  Fri Apr  9 14:44:24 2010 (serrano)                */
/*    Copyright   :  2007-10 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop event machinery.                                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    window event listener ...                                        */
/*---------------------------------------------------------------------*/
var hop_is_ready = false;

hop_add_native_event_listener( window, "load", function() { hop_is_ready = true; } );

/*---------------------------------------------------------------------*/
/*    hop_add_event_listener ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export add-event-listener!) (arity -4))) */
function hop_add_event_listener( obj, event, proc, capture ) {
   if( event === "location" )
      return hop_add_active_location_listener( obj, proc );

   if( event === "server" )
      return hop_add_server_listener( obj, proc, capture );

   if( event === "serverready" )
      return hop_add_serverready_listener( obj, proc );

   if( event === "serverclose" )
      return hop_add_serverclose_listener( obj, proc );

   if( event === "timeout" )
      return hop_add_timeout_listener( obj, proc );

   if( ("hop_add_event_listener" in obj) &&
       (obj.hop_add_event_listener != hop_add_event_listener) )
      return obj.hop_add_event_listener( event, proc, capture );

   if( event === "ready" ) {
      if( hop_is_ready ) {
	 return proc( new function() { this.name = 'ready' } );
      } else {
	 return hop_add_native_event_listener( window, "load", proc, capture );
      }
   }
   
   return hop_add_native_event_listener( obj, event, proc, capture );
}

/*---------------------------------------------------------------------*/
/*    hop_remove_event_listener ...                                    */
/*---------------------------------------------------------------------*/
/*** META ((export remove-event-listener!) (arity -4)) */
function hop_remove_event_listener( obj, event, proc, capture ) {
   if( event === "location" )
      return hop_remove_active_location_listener( obj, proc );

   if( event === "server" )
      return hop_remove_server_listener( obj, proc );

   if( event === "serverready" )
      return hop_remove_serverready_listener( obj, proc );

   if( event === "serverclose" )
      return hop_remove_serverclose_listener( obj, proc );

   if( event === "timeout" )
      return hop_remove_timeout_listener( proc );

   if( (obj.hop_remove_event_listener != undefined) &&
      (obj.hop_remove_event_listener != hop_remove_event_listener) )
      return obj.hop_remove_event_listener( event, proc, capture );

   return hop_remove_native_event_listener( obj, event, proc, capture );
} 

/*---------------------------------------------------------------------*/
/*    hop_add_active_location_listener ...                             */
/*---------------------------------------------------------------------*/
function hop_add_active_location_listener( obj, proc ) {
   obj.hop_active_location_proc = proc;
   var i = window.location.href.indexOf( "#" );
   
   if( i === -1 ) {
      obj.hop_active_location_href = window.location.href;
   } else {
      obj.hop_active_location_href = window.location.href.substring( 0, i-1 );
   }

   if( obj.hop_active_location_interval === undefined ) {
      var check = function() {
	 if( obj.hop_active_location_href !== window.location.href ) {
	    if( obj.hop_active_location_href != window.location.href ) {
	       obj.hop_active_location_href = window.location.href;
	       obj.hop_active_location_proc( window.location );
	    }
	 }
	 return true;
      }

      obj.hop_active_location_interval =
	 setInterval( check, hop_active_location_timeout );
   }
   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_remove_active_location_listener ...                          */
/*---------------------------------------------------------------------*/
function hop_remove_active_location_listener( obj, proc ) {
   if( obj.hop_active_location_interval != undefined ) {
      clearInterval( obj.hop_active_location_interval );
      obj.hop_active_location_interval = undefined;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_active_location_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_active_location_set( obj, href ) {
   window.location.href = href;
   obj.hop_active_location_href = window.location.href;
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

var hop_servevt_enveloppe_re =
   new RegExp( "^<([rsxifj]) name='([^']+)'>((?:.|[\n])*)</[rsxifj]>$" );
var hop_servevt_enveloppe_cdata_re =
   new RegExp( "^<!\\[CDATA\\[((?:.|[\n])*)\\]\\]>$" );

/*---------------------------------------------------------------------*/
/*    hop_servevt_enveloppe_parse ...                                  */
/*---------------------------------------------------------------------*/
function hop_servevt_enveloppe_parse( val, xhr, server_ready ) {
   var m = val.match( hop_servevt_enveloppe_re );

   if( m != null ) {
      var k = m [ 1 ];
      var id = m[ 2 ];
      var text = m[ 3 ];

      if( k === "i" ) {
	 hop_trigger_servevt( id, text, parseInt( text ), false );
      } else if( k == "f" ) {
	 hop_trigger_servevt( id, text, parseFloat( text ), false );
      } else if( k == "s" ) {
	 hop_trigger_servevt( id, text, text, false );
      } else if( k == "x" ) {
	 hop_trigger_servevt( id, text, hop_create_element( text ), false );
      } else if( k == "j" ) {
	 var t = text.match( hop_servevt_enveloppe_cdata_re );
	 if( t ) {
	    hop_trigger_servevt( id, t[ 1 ], t[ 1 ], true );
	 }
      } else if( k == "r" ) {
	 if( !server_ready ) {
	    hop_trigger_serverready_event( new HopServerReadyEvent() );
	 }
	 return true;
      } else {
	 alert( "unknow event message: [" + xhr.responseText + "]" );
      }
   } else {
      alert( "unknow event message: [" + xhr.responseText + "]" );
   }
   return server_ready;
}

/*---------------------------------------------------------------------*/
/*    start_servevt_websocket_proxy ...                                */
/*---------------------------------------------------------------------*/
function start_servevt_websocket_proxy( key, host, port ) {
   if( !hop_servevt_proxy.websocket ) {
      var url = "ws://" + host + ":" + port +
	 hop_service_base() + "/server-event/websocket?key=" + key;
/*       var url = "ws://" + host + ":" + 8788 + "/echo";              */

      var ws = new WebSocket( url );

      var register = function( id ) {
	 var svc = hop_service_base() +
	 "/server-event/register?event=" + id +
	 "&key=" + key  + "&mode=websocket";

	 hop_send_request( svc, false,
			   function() { ; }, false,
			   false, [] );
      }

      var unregister = function( id ) {
	 hop_servevt_proxy.httpreq.abort();

	 var svc = hop_service_base() +
	 "/server-event/unregister?event=" + id +
	 "&key=" + key;
	 
	 hop_send_request( svc, false,
			   function() { ; }, false,
			   false, [] );
      };

      // complete the proxy definition
      hop_servevt_proxy.websocket = ws;

      ws.onopen = function() {
	 // we are ready to register now
	 hop_servevt_proxy.register = register;
	 hop_servevt_proxy.unregister = unregister;

	 // register the unitialized events
	 for( var p in hop_servevt_table ) {
	    if( hop_servevt_table[ p ].hop_servevt ) {
	       register( p );
	    }
	 }
	 hop_trigger_serverready_event( new HopServerReadyEvent() );
      }
      ws.onclose = function() {
	 hop_servevt_onclose();
      }
      ws.onmessage = function ( e ) {
	 e.responseText = e.data;
	 hop_servevt_enveloppe_parse( e.data, e, true );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    start_servevt_xhr_multipart_proxy ...                            */
/*---------------------------------------------------------------------*/
function start_servevt_xhr_multipart_proxy( key ) {
   if( !hop_servevt_proxy.httpreq ) {
      var server_ready = false;
      
      var register = function( id ) {
	 var svc = hop_service_base() +
	    "/server-event/register?event=" + id +
	    "&key=" + key  + "&mode=xhr-multipart";

	 var success = function( val, xhr ) {
	    server_ready = hop_servevt_enveloppe_parse( val, xhr, server_ready );
	 }

	 var failure = function( xhr ) {
	    if( xhr.exception ) {
	       if( typeof hop_report_exception === "function" ) {
		  hop_report_exception( xhr.exception );
	       }
	    }
	    
	    if( "hop_servevt_onclose" in window ) hop_servevt_onclose();
	 }

	 var req = hop_make_xml_http_request();
	 req.multipart = true;

	 hop_servevt_proxy.httpreq = hop_send_request( svc,
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

      var unregister = function( id ) {
	 hop_servevt_proxy.httpreq.abort();

	 var svc = hop_service_base() +
	    "/server-event/unregister?event=" + id +
   	    "&key=" + hop_servevt_proxy.key;
	 
	 hop_servevt_proxy.httpreq = hop_send_request( svc, false,
						       function() { ; }, false,
						       false, [] );
      };

      // complete the proxy definition
      hop_servevt_proxy.register = register;
      hop_servevt_proxy.unregister = unregister;

      // register the unitialized events
      for( var p in hop_servevt_table ) {
	 if( hop_servevt_table[ p ].hop_servevt ) {
	    register( p );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    start_servevt_ajax_proxy ...                                     */
/*---------------------------------------------------------------------*/
function start_servevt_ajax_proxy( key ) {
   if( !hop_servevt_proxy.httpreq ) {
      var xhr_error_ttl = 6 * 3;
      var server_ready = false;
      
      var register = function( id ) {
	 var svc = hop_service_base() +
	    "/server-event/register?event=" + id +
	    "&key=" + key  + "&mode=ajax";

	 var success = function( val, xhr ) {
	    if( !server_ready ) {
	       server_ready = true;
	       hop_trigger_serverready_event( new HopServerReadyEvent() );
	    }
	    
	    // null is used as a marker for an abandonned connection
	    if( val != null ) {
	       // erase previous errors
	       xhr_error_ttl = 6 * 3;
	       // re-register the event as soon as possible
	       register( "" );
	       // invoke all the user handlers (we have received a list of
	       // values corresponding to server buffer).
	       while( sc_isPair( val ) ) {
		  var v = val.car;
		  var id = v.car;
		  var vals = v.cdr;

		  while( vals != null ) {
		     hop_trigger_servevt( id, vals.car, vals.car, false );
		     vals = vals.cdr;
		  }

		  val = val.cdr;
	       }
	    }
	 }

	 var failure = function( xhr ) {
	    if( !xhr.status &&
		(xhr_error_ttl > 0) &&
		!xhr.getAllResponseHeaders() ) {
	       // mark the connection timeout error in order to avoid
	       // falling into an infinit loop when the server has crashed.
	       xhr_error_ttl--;
	       // we have reached a timeout, we just re-register
	       register( id );
	    } else {
	       hop_servevt_onclose();
	    }
	 }

	 hop_servevt_proxy.httpreq = hop_send_request( svc,
						       // asynchronous call
						       false,
						       // success callback
						       success,
						       // failure callback
						       failure,
						       // no anim
						       false,
						       // no environment
						       [] );

      }

      var unregister = function( id ) {
	 hop_servevt_proxy.httpreq.abort();

	 var svc = hop_service_base() +
	    "/server-event/unregister?event=" + id +
   	    "&key=" + hop_servevt_proxy.key;
	 
	 hop_servevt_proxy.httpreq = hop_send_request( svc, false,
						       function() { ; }, false,
						       false, [] );
      };

      // complete the proxy definition
      hop_servevt_proxy.register = register;
      hop_servevt_proxy.unregister = unregister;

      // register the unitialized events
      for( var p in hop_servevt_table ) {
	 if( hop_servevt_table[ p ].hop_servevt ) {
	    register( p );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    start_servevt_flash_proxy ...                                    */
/*---------------------------------------------------------------------*/
function start_servevt_flash_proxy( key, host, port ) {
   var object_proxy = function() {
      return "<object id='" + hop_servevt_id + "' class='hop-servevt-proxy'" +
      " style='visibility: hidden; position: fixed; top: 0; right: 0'" +
      " width='1px' height='1px' title='hop-servevt' classId='HopServevt.swf'>" +
      "<param name='movie' value='" + hop_share_directory() + "/flash/HopServevt.swf'/>" +
      "<param name='allowScriptAccess' value='always'/>" +
      "<param name='FlashVars' value='init=hop_servevt_proxy_flash_init" +
      "&host=" + host + "&port=" + port + "&key=" + key +
      "&onevent=hop_trigger_servevt&onclose=hop_servevt_onclose" +
      "&onerror=hop_servevt_onerror'/>" +
      "</object>";
   }
   
   var embed_proxy = function() {
      /* EMBED is not a legal XHTML markup, hence we cannot add the */
      /* using a "innerHTML = xxx" expression. As a workaround we   */
      /* use DOM constructor and methods.                           */
      var embed = document.createElement( "embed" );
      
      embed.id = hop_servevt_id;
      embed.className = "hop-servevt-proxy";
      embed.setAttribute( "width", "1px" );
      embed.setAttribute( "height", "1px" );
      embed.setAttribute( "src", hop_share_directory() + "/flash/HopServevt.swf" );
      embed.setAttribute( "type", "application/x-shockwave-flash" );
      embed.setAttribute( "name", "__hop_servevt_proxy" );
      embed.setAttribute( "swliveconnect", "true" );
      embed.setAttribute( "allowScriptAccess", "always" );
      embed.setAttribute( "FlashVars", "init=hop_servevt_proxy_flash_init" +
			  "&host=" + host + "&port=" + port + "&key=" + key +
			  "&onevent=hop_trigger_servevt" +
			  "&onclose=hop_servevt_onclose" +
			  "&onerror=hop_servevt_onerror" );
      
      return embed;
   }
   
   var proxy = document.createElement( "div" );
/*    node_style_set( proxy, "visibility", "hidden" );                 */
   node_style_set( proxy, "position", "fixed" );
   node_style_set( proxy, "top", "0" );
   node_style_set( proxy, "right", "0" );
   node_style_set( proxy, "background", "transparent" );

   if( hop_config.flash_markup === "embed" ) {
      proxy.appendChild( embed_proxy() );
   } else {
      proxy.innerHTML = object_proxy();
   }

   document.body.appendChild( proxy );
   document.getElementById( hop_servevt_id ).key = key;

   return proxy;
}

/*---------------------------------------------------------------------*/
/*    hop_servevt_onerror ...                                          */
/*---------------------------------------------------------------------*/
function hop_servevt_onerror( msg ) {
   sc_error( "servevt", msg, "internal server event error" );
}

/*---------------------------------------------------------------------*/
/*    hop_servevt_proxy_flash_init ...                                 */
/*    -------------------------------------------------------------    */
/*    This function is called by Flash when the ActionScript           */
/*    installation completes.                                          */
/*    -------------------------------------------------------------    */
/*    When the flash init completes, we ask the Hop server its         */
/*    current server-event port number. When we get this number,       */
/*    we open the flash socket. Then, events are ready to be           */
/*    received.                                                        */
/*---------------------------------------------------------------------*/
function hop_servevt_proxy_flash_init() {
   /* if we are here, we are sure that Flash v8 or better is running */
   hop_flash_minversion_set( 8 );

   var pending_events = 0;

   hop_servevt_proxy = document.getElementById( hop_servevt_id );

   var abort = function( id ) {
      var svc = hop_service_base() +
         "/server-event/unregister?event=" + id
         + "&key=" + hop_servevt_proxy.key;
      hop_servevt_proxy.httpreq = hop_send_request( svc, false,
						    function() {;}, false,
						    false, [] );
   }
      
   var failure = function( e ) {
      hop_servevt_onclose();
      
      for( var p in hop_servevt_table ) {
	 if( hop_servevt_table[ p ].hop_servevt ) {
	    abort( p );
	 }
      }

      hop_send_request( hop_service_base() +
			"/server-event/close?key=" + hop_servevt_proxy.key,
			false,
			function() {;}, false,
			false, [] );
   }

   var register = function( id ) {
      var svc = hop_service_base() + "/server-event/register?event=" + id
         + "&key=" + hop_servevt_proxy.key + "&mode=flash";
      
      var success = function( e ) {
	 if( pending_events > 0 ) {
	    if( pending_events == 1 ) {
	       hop_trigger_serverready_event( new HopServerReadyEvent() );
	    }
	    pending_events--;
	 }
      }

      hop_servevt_proxy.httpreq = hop_send_request( svc,
						    // asynchronous call
						    false,
						    // success callback
						    success,
						    // failure callback
						    failure,
						    // no anim
						    false,
						    // no environment
						    [] );
   }

   hop_servevt_proxy.register = register;
   hop_servevt_proxy.unregister = function( id ) {
      // This function does not close the socket, otherwise, no event
      // could take place because they all share the same socket.
      abort( id );
   }

   // register the event event deregistration
   hop_add_event_listener( window, "unload", failure );

   // count the number of pre-registered events
   for( var p in hop_servevt_table ) {
      if( hop_servevt_table[ p ].hop_servevt ) {
	 pending_events++;
      }
   }

   if( pending_events > 0 ) {
      // regsiter them
      for( var p in hop_servevt_table ) {
	 if( hop_servevt_table[ p ].hop_servevt ) {
	    register( p );
	 }
      }
   } else {
      hop_trigger_serverready_event( new HopServerReadyEvent() );
   }
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
/*    servevt_flashp ...                                               */
/*---------------------------------------------------------------------*/
function servevt_flashp( port ) {
   return port &&
      (hop_config.flash_version >= 8) &&
      (hop_config.flash_external_interface);
}
      
/*---------------------------------------------------------------------*/
/*    hop_start_servevt_proxy ...                                      */
/*    -------------------------------------------------------------    */
/*    http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol      */
/*    for details on the Web protocol.                                 */
/*---------------------------------------------------------------------*/
function hop_start_servevt_proxy() {
   hop_servevt_proxy = new Object();
   hop_servevt_proxy.websocket = false;
   hop_servevt_proxy.register = function( x ) {};

   hop_send_request( hop_service_base() + "/server-event/info",
		     // asynchronous call
		     false,
		     // success callback
		     function( v ) {
			var host = v[ 0 ];
			var port = v[ 1 ];
			var key = v[ 2 ];

			if( servevt_websocketp() ) {
			   // websocket backend
			   start_servevt_websocket_proxy( key, host, port );
			} else if( servevt_xhr_multipartp() ) {
			   // xhr_multipart backend
			   start_servevt_xhr_multipart_proxy( key );
			} else if( servevt_flashp( port ) ) {
			   // flash backend
			   try {
			      start_servevt_flash_proxy( key, host, port );
			   } catch( e ) {
			      e.scObject = ("port=" + port);
			      throw( e );
			   }
			} else {
			   // fallback xhr backend
			   start_servevt_ajax_proxy( key );
			}
		     },

		     // failure callback
		     function( v ) {
			throw new Error( "No event server acknowledge" );
		     },
		     // run the anim during the call
		     true,
		     // no environment
		     [] );
}

/*---------------------------------------------------------------------*/
/*    hop_trigger_servevt ...                                          */
/*    -------------------------------------------------------------    */
/*    This function is invoked by Flash and Ajax on event reception.   */
/*---------------------------------------------------------------------*/
function hop_trigger_servevt( id, text, value, json ) {
   try {
      var v = (json ? eval( value ) : value);
      var evt = new HopServerEvent( id, text, v );
      var p2 = hop_servevt_table[ id ];

      if( sc_isPair( hop_servevt_dlist ) &&
	  sc_isPair( hop_servevt_ctable[ id ] ) ) {
	 // the event is captured and we have document listener bound
	 var p1 = hop_servevt_dlist;

	 while( sc_isPair( p1 ) ) {
	    p1.car( evt );
	    p1 = p1.cdr;
	 }
      }

      evt.isStopped = false;

      while( sc_isPair( p2 ) ) {
	 try {
	    p2.car( evt );
	 } catch( exc ) {
	    exc.scObject = ("event=" + id + ", val=" + p2.car );
	    throw exc;
	 }
	 
	 if( evt.isStopped ) break;
	 p2 = p2.cdr;
      }
   } catch( exc ) {
      exc.scObject = ("event=" + id + ", val=" + value );
      throw exc;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_serverclose_list ...                                         */
/*---------------------------------------------------------------------*/
var hop_serverclose_list = null;
var hop_serverclose_triggered = false;

/*---------------------------------------------------------------------*/
/*    hop_add_serverclose_listener ...                                 */
/*---------------------------------------------------------------------*/
function hop_add_serverclose_listener( obj, proc ) {
   if( obj === document ) {
      if( hop_serverclose_triggered ) {
	 // the server is close
	 var evt = new HopServerEvent( "serverclose", false, false );
	 proc( evt );
      } else {
	 // the server is not close yet, we register the callback
	 hop_serverclose_list = sc_cons( proc, hop_serverclose_list );
      }
   } else {
      throw new Error( "add-event-listener!: Illegal `serverclose' recipient"
		       + obj );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_remove_serverclose_listener ...                              */
/*---------------------------------------------------------------------*/
function hop_remove_serverclose_listener( obj, proc ) {
   if( obj === document ) {
      hop_serverclose_list = sc_remqBang( proc, hop_serverclose_list );
      return true;
   } else {
      throw new Error( "remove-event-listener!: Illegal `serverclose' recipient"
		       + obj );
      return false;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_servevt_onclose ...                                          */
/*    -------------------------------------------------------------    */
/*    This function is invoked by Flash and Ajax on connection close.  */
/*---------------------------------------------------------------------*/
function hop_servevt_onclose() {
   // allocate a new event in order to hide handler side effects
   var evt = new HopServerEvent( "serverclose", false, false );
   var p = hop_serverclose_list;

   while( sc_isPair( p ) ) {
      p.car( evt );
      if( evt.isStopped ) break;
      p = p.cdr;
   }

   hop_serverclose_triggered = true;
}

/*---------------------------------------------------------------------*/
/*    hop_add_server_listener ...                                      */
/*---------------------------------------------------------------------*/
function hop_add_server_listener( obj, proc, capture ) {
   if( typeof proc != "function" ) {
      throw new Error( "Illegal procedure: " + proc );
   }

   if( obj === document ) {
      hop_servevt_dlist = sc_cons( proc, hop_servevt_dlist );
   } else {
      if( !document.body ) {
	 // delay until the document is fully built
	 hop_add_event_listener(
	    window, "ready", 
	    function( e ) {
	       hop_add_server_listener( obj, proc, capture );
	    } );
      } else {
	 var o = hop_servevt_table[ obj ];
      
	 hop_servevt_table[ obj ] = sc_cons( proc, sc_isPair( o ) ? o : null );
	 hop_servevt_table[ obj ].hop_servevt = true;

	 if( capture ) {
	    var o = hop_servevt_ctable[ obj ];
	    hop_servevt_ctable[ obj ] = sc_cons( proc, sc_isPair( o ) ? o : null );
	 }

	 if( !hop_servevt_proxy ) {
	    hop_start_servevt_proxy();
	 } else {
	    hop_servevt_proxy.register( obj );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_remove_server_listener ...                                   */
/*---------------------------------------------------------------------*/
function hop_remove_server_listener( obj, proc ) {
   if( obj === document ) {
      hop_servevt_dlist = sc_remqBang( proc, hop_servevt_dlist );
   } else {
      // unregister the event listener
      if( sc_isPair( hop_servevt_table[ obj ] ) )
	 hop_servevt_table[ obj ] =
	    sc_remqBang( proc, hop_servevt_table[ obj ] );
      if( sc_isPair( hop_servevt_ctable[ obj ] ) )
	 hop_servevt_ctable[ obj ] =
	    sc_remqBang( proc,hop_servevt_ctable[ obj ] );

      // try to close the socket
      for( id in hop_servevt_table ) {
	 if( sc_isPair( hop_servevt_table[ obj ] ) )
	    return;
      }

      // no event is still expected, close the connection
      hop_servevt_proxy.unregister( obj );
   }
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
function hop_trigger_serverready_event( evt ) {
   while( sc_isPair( hop_serverready_list ) ) {
      hop_serverready_list.car( evt );
      if( evt.isStopped ) break;
      hop_serverready_list = hop_serverready_list.cdr;
   }

   hop_serverready_triggered = true;
}

/*---------------------------------------------------------------------*/
/*    hop_add_serverready_listener ...                                 */
/*---------------------------------------------------------------------*/
function hop_add_serverready_listener( obj, proc ) {
   if( obj === document ) {
      if( hop_serverready_triggered ) {
	 // the server is ready
	 proc();
      } else {
	 // the server is not ready yet, we register the callback
	 hop_serverready_list = sc_cons( proc, hop_serverready_list );
      }
   } else {
      throw new Error( "add-event-listener!: Illegal `serverready' recipient"
		       + obj );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_remove_serverready_listener ...                              */
/*---------------------------------------------------------------------*/
function hop_remove_serverready_listener( obj, proc ) {
   if( obj === document ) {
      hop_serverready_list = sc_remqBang( proc, hop_serverready_list );
      return true;
   } else {
      throw new Error( "remove-event-listener!: Illegal `serverready' recipient"
		       + obj );
      return false;
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
   hop_timeout_listeners = sc_cons( sc_cons( proc, setInterval( proc, obj ) ),
				    hop_timeout_listeners );
}

/*---------------------------------------------------------------------*/
/*    hop_remove_timeout_listener ...                                  */
/*---------------------------------------------------------------------*/
function hop_remove_timeout_listener( proc ) {
   var p = hop_timeout_listeners;
   
   if( sc_isPair( p ) ) {
      if( p.car.car === proc ) {
	 clearInterval( p.car.cdr );
	 hop_timeout = p.cdr;
      } else {
	 while( sc_isPair( p.cdr ) ) {
	    if( p.cdr.car === proc ) {
	       clearInterval( p.cdr.cdr );
	       p.cdr = p.cdr.cdr;
	       break;
	    } else {
	       p = p.cdr;
	    }
	 }
      }
   }
}
