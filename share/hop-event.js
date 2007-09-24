/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-event.js                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 20 07:19:56 2007                          */
/*    Last change :  Mon Sep 24 16:55:54 2007 (serrano)                */
/*    Copyright   :  2007 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hop event machinery.                                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_add_event_listener ...                                       */
/*---------------------------------------------------------------------*/
function hop_add_event_listener( obj, event, proc, capture ) {
   if( event == "location" )
      return hop_add_active_location_listener( obj, proc );

   if( event == "server" )
      return hop_add_server_listener( obj, proc, capture );

   if( event == "serverready" )
      return hop_add_serverready_listener( obj, proc );

   if( event == "timeout" )
      return hop_add_timeout_listener( obj, proc );

   if( (obj.hop_add_event_listener != undefined) &&
      (obj.hop_add_event_listener != hop_add_event_listener) )
      return obj.hop_add_event_listener( event, proc, capture );

   return hop_add_native_event_listener( obj, event, proc, capture );
}

/*---------------------------------------------------------------------*/
/*    hop_remove_event_listener ...                                    */
/*---------------------------------------------------------------------*/
function hop_remove_event_listener( obj, event, proc, capture ) {
   if( event == "location" )
      return hop_remove_active_location_listener( obj, proc );

   if( event == "server" )
      return hop_remove_server_listener( obj, proc );

   if( event == "serverready" )
      return hop_remove_serverready_listener( obj, proc );

   if( event == "timeout" )
      return hop_remove_timeout_listener( proc );

   if( (obj.hop_remove_event_listener != undefined) &&
      (obj.hop_remove_event_listener != hop_remove_event_listener) )
      obj.hop_remove_event_listener( event, proc, capture );

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

/*---------------------------------------------------------------------*/
/*    HopServerEvent ...                                               */
/*---------------------------------------------------------------------*/
function HopServerEvent( name, text, value ) {
   var o = new Object();
   o.isStopped = false;
   o.name = name;
   o.value = value;
   o.responseText = text;
   
   return o;
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

/*---------------------------------------------------------------------*/
/*    start_servevt_ajax_proxy ...                                     */
/*---------------------------------------------------------------------*/
function start_servevt_ajax_proxy( key, obj ) {
   if( !hop_servevt_proxy.httpreq ) {
      var readystate = 0;
      
      var register = function( id ) {
	 var svc = "/hop/server-event-register?event=" + id + "&key=" + key;

	 var success = function( val, http ) {
	    // re-register the event as soon as possible
	    register( id );
	    // invoke the user handler
	    hop_trigger_servevt( id,
				 http.responseText,
				 val,
				 hop_is_http_json( http ) );
	 }

	 var failure = function( http ) {
	    hop_servevt_onclose();
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
      hop_servevt_proxy.close = function() { hop_servevt_proxy.http.abort() };

      // scan all the previously registered events an register on the server
      for( var p in hop_servevt_table ) {
	 if( hop_servevt_table[ p ].hop_servevt ) {
	    hop_servevt_proxy.register( p );
	 }
      }
      
      hop_trigger_serverready_event( new HopServerReadyEvent() );
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
   node_style_set( proxy, "visibility", "hidden" );
   node_style_set( proxy, "position", "fixed" );
   node_style_set( proxy, "top", "0" );
   node_style_set( proxy, "right", "0" );

   if( hop_msiep() ) {
      proxy.innerHTML = object_proxy();
   } else {
      proxy.appendChild( embed_proxy() );
   }

   document.body.appendChild( proxy );
   document.getElementById( hop_servevt_id ).key = key;

   return proxy;
}

/*---------------------------------------------------------------------*/
/*    hop_servevt_onerror ...                                          */
/*---------------------------------------------------------------------*/
function hop_servevt_onerror( msg ) {
   alert( msg );
   throw new Error( msg );
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
   var readystate = 0;
   hop_servevt_proxy = document.getElementById( hop_servevt_id );

   var register = function( id ) {
      var svc = "/hop/server-event-register?event=" + id
         + "&key=" + hop_servevt_proxy.key + "&flash=true";
      
      var failure = function( e ) {
	 hop_servevt_onclose();
      }

      var success = function( e ) {
	 if( readystate === 1 ) {
	    hop_trigger_serverready_event( new HopServerReadyEvent() );
	    readystate = 2;
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

   // scan all the previously registered events an register on the server
   for( var p in hop_servevt_table ) {
      if( hop_servevt_table[ p ].hop_servevt ) {
	 if( readystate === 0 ) readystate = 1;
	 register( p );
      }
   }

   if( readystate === 0 ) {
      readystate = 2;
      hop_trigger_serverready_event( new HopServerReadyEvent() );
   }
}

/*---------------------------------------------------------------------*/
/*    start_servevt_proxy ...                                          */
/*---------------------------------------------------------------------*/
function start_servevt_proxy( obj ) {
   hop_servevt_proxy = new Object();
   hop_servevt_proxy.register = function( x ) {};

   hop_send_request( "/hop/server-event-info",
		     // asynchronous call
		     false,
		     // success callback
		     function( v ) {
			var host = v[ 0 ];
			var port = v[ 1 ];
			var key = v[ 2 ];

			if( port &&
			    (hop_flash_version() >= 8) &&
			    !(hop_operap()) ) {
			   try {
			      start_servevt_flash_proxy( key, host, port );
			   } catch( e ) {
			      hop_servevt_onerror( "Cannot start flash proxy: " +
						   e );
			   }
			} else {
			   start_servevt_ajax_proxy( key, obj );
			}
		     },
		     // failure callback
		     function( v ) {
			throw new Error( "Cannot get server event port number" );
		     },
		     // run the anim during the call
		     true,
		     // no environment
		     [] );
}

/*---------------------------------------------------------------------*/
/*    hop_trigger_servevt ...                                          */
/*    -------------------------------------------------------------    */
/*    This function is invoked by Flash on reception of an event.      */
/*---------------------------------------------------------------------*/
function hop_trigger_servevt( id, text, value, json ) {
   var evt = new HopServerEvent( id, text, json ? eval( value ) : value );
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
      p2.car( evt );
      if( evt.isStopped ) break;
      p2 = p2.cdr;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_servevt_onclose ...                                          */
/*    -------------------------------------------------------------    */
/*    This function is invoked by Flash on connection close.           */
/*---------------------------------------------------------------------*/
function hop_servevt_onclose() {
   for( id in hop_servevt_table ) {
      // allocate a new event in order to hide handler side effects
      var evt = new HopServerEvent( id, false, false );
      var p = hop_servevt_table[ id ];

      while( sc_isPair( p ) ) {
	 p.car( evt );
	 if( evt.isStopped ) break;
	 p = p.cdr;
      }
   }
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
      var o = hop_servevt_table[ obj ]; 
      hop_servevt_table[ obj ] = sc_cons( proc, sc_isPair( o ) ? o : null );
	 
      hop_servevt_table[ obj ].hop_servevt = true;

      if( capture ) {
	 var o = hop_servevt_ctable[ obj ];
	 hop_servevt_ctable[ obj ] = sc_cons( proc, sc_isPair( o ) ? o : null );
      }

      if( !hop_servevt_proxy ) {
	 start_servevt_proxy( obj );
      } else {
	 hop_servevt_proxy.register( obj );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_remove_server_listener ...                                   */
/*---------------------------------------------------------------------*/
function hop_remove_server_listener( obj, proc ) {
   if( obj === document ) {
      hop_servevt_dlist = sc_remqbang( proc, hop_servevt_dlist );
   } else {
      // unregister the event listener
      hop_servevt_table[ id ] = sc_remqbang( proc, hop_servevt_table[ id ] );
      hop_servevt_ctable[ id ] = sc_remqbang( proc, hop_servevt_ctable[ id ] );

      // try to close the socket
      for( id in hop_servevt_table ) {
	 if( sc_isPair( hop_servevt_table[ id ] ) )
	    return;
      }

      // no event is still expected, close the connection
      hop_servevt_proxy.close();
   }
}

/*---------------------------------------------------------------------*/
/*    hop_serverready_list ...                                         */
/*---------------------------------------------------------------------*/
var hop_serverready_list = null;

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
   var p = hop_serverready_list;
   
   while( sc_isPair( p ) ) {
      p.car( evt );
      if( evt.isStopped ) break;
      p = p.cdr;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_add_serverready_listener ...                                 */
/*---------------------------------------------------------------------*/
function hop_add_serverready_listener( obj, proc ) {
   if( obj === document ) {
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
   if( obj === document ) {
      hop_serverready_list = sc_remqbang( proc, hop_serverready_list );
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
