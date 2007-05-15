/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop.js                            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec 25 06:57:53 2004                          */
/*    Last change :  Tue May 15 17:19:23 2007 (serrano)                */
/*    Copyright   :  2004-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Standard HOP JavaScript library                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_busy_anim ...                                                */
/*---------------------------------------------------------------------*/
var hop_busy_anim = "data:image/gif;base64,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";

/*---------------------------------------------------------------------*/
/*    hop_service_url ...                                              */
/*---------------------------------------------------------------------*/
function hop_service_url( service, formals, args ) {
   var len = formals.length;

   if( len == 0 ) {
      return service + "?hop-encoding=hop";
   } else {
      var url = service + "?hop-encoding=hop";
      var i;

      if( (args.length == 1) &&
	  (HTMLFormElement != undefined) &&
	  (args[ 0 ] instanceof HTMLFormElement) ) {
	 var els = args[ 0 ].elements;

	 for( i = 0; i < els.length; i++ ) {
	    if( els[ i ].type == "checkbox" ) {
	       var val = els[ i ].checked ? els[ i ].value : false;

	       url += "&" + els[ i ].name + "=" + hop_serialize( val );
	    } else {
	       if( els[ i ].type == "radio" ) {
		  if( els[ i ].checked ) {
		     url += "&" + els[ i ].name + "=" + hop_serialize( els[ i ].value );
		  }
	       } else {
		  url += "&" + els[ i ].name + "=" + hop_serialize( els[ i ].value );
	       }
	    }
	 }

	 return url;
      } else {
	 for( i = 0; i < len; i++ ) {
	    url += "&" + formals[ i ] + "=" + hop_serialize( args[ i ] );
	 }

	 return url;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_service_url_varargs ...                                      */
/*---------------------------------------------------------------------*/
function hop_service_url_varargs( service, args ) {
   var len = args.length;
   
   if( len == 0 ) {
      return service;
   } else {
      var url = service + "?hop-encoding=hop";
      var i;

      for( i = 0; 0 < len; i++ ) {
	 url += "&a" + i + "=" + hop_serialize( args[ i ] );
      }

      return url;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_replace_document ...                                         */
/*---------------------------------------------------------------------*/
function hop_replace_document( http ) {
   if( http.responseText != null ) {
      hop_set_cookie( http );
      document.open();
      document.write( http.responseText );
      document.close();
   }
}

/*---------------------------------------------------------------------*/
/*    hop_replace_inner ...                                            */
/*---------------------------------------------------------------------*/
function hop_replace_inner( el ) {
   if( el != undefined ) {
      return function( http ) {
	 if( http.responseText != null ) {
	    el.innerHTML = http.responseText;
	    hop_js_eval( http );
	 }
      }
   } else {
      alert( "*** Hop Error, Can't find element" );
      return function( http ) { };
   }
}

/*---------------------------------------------------------------------*/
/*    hop_replace_inner_id ...                                         */
/*---------------------------------------------------------------------*/
function hop_replace_inner_id( id ) {
   return hop_replace_inner( document.getElementById( id ) );
}

/*---------------------------------------------------------------------*/
/*    hop_append ...                                                   */
/*---------------------------------------------------------------------*/
function hop_append( el ) {
   if( el != undefined ) {
      return function( http ) {
	 if( http.responseText != null ) {
	    el.innerHTML += http.responseText;
	    hop_js_eval( http );
	 }
      }
   } else {
      alert( "*** Hop Error, Can't find element" );
      return function( http ) { };
   }
}

/*---------------------------------------------------------------------*/
/*    hop_remove ...                                                   */
/*---------------------------------------------------------------------*/
function hop_remove( el ) {
   if( el != undefined ) {
      var p = el.parentNode;
      
      return function( http ) {
	 p.removeChild( el );
	 if( http.responseText != null ) {
	    hop_js_eval( http );
	 }
      }
   } else {
      alert( "*** Hop Error, Can't find element" );
      return function( http ) { };
   }
}

/*---------------------------------------------------------------------*/
/*    hop_remove_id ...                                                */
/*---------------------------------------------------------------------*/
function hop_remove_id( id ) {
   return hop_remove( document.getElementById( id ) );
}

/*---------------------------------------------------------------------*/
/*    hop_eval ...                                                     */
/*---------------------------------------------------------------------*/
function hop_eval( proc ) {
   return function( http ) {
      return proc( hop_js_eval( http ) );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_node_eval ...                                                */
/*---------------------------------------------------------------------*/
function hop_node_eval( node, text ) {
   var res;
   var scripts = node.getElementsByTagName( "script" );

   if( scripts.length > 0 ) {
      for ( var j = 0; j < scripts.length; j++ ) {
	 if( scripts[ j ].childNodes.length > 0 ) {
	    res = eval( scripts[ j ].childNodes[ 0 ].nodeValue );
	 }
      }
   } else {
      var script = text.match( /<script[^>]*>/i );
      if( script != null ) {
	 /* I don't why yet, IE 7 does not include SCRIPT nodes */
	 /* in the resulting node!                              */
	 var start = script.index + script[ 0 ].length;
	 var end = text.search( /<\/script>/i );
	 if( (end != null) && (end > start) ) {
	    res = eval( text.substr( start, end - start ) );
	 }
      }
   }

   return res;
}
   
/*---------------------------------------------------------------------*/
/*    hop_js_eval ...                                                  */
/*---------------------------------------------------------------------*/
function hop_js_eval( http ) {
   if( http.responseText != null ) {
      var node = document.createElement( "div" );

      node.innerHTML = http.responseText;

      return hop_node_eval( node, http.responseText );
   }

   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_default_failure ...                                          */
/*---------------------------------------------------------------------*/
function hop_default_failure( http ) {
   var t = http.responseText;
   var div = document.getElementById( "hop_default_failure" );
   var div2 = document.getElementById( "hop_default_failure_background" );

   t = t.replace( /<!DOCTYPE[^>]*>/g, "" );
   t = t.replace( /<head[^>]*>/g, "<div style='display: none;'>" );
   t = t.replace( /<\/head>/g, "</div>" );
   t = t.replace( /<(meta|link)[^>]*>/g, "<span style='display: none'></span>" );
   t = t.replace( /<html[^>]*>/g, "<div align='center' style='background: transparent; cursor: pointer' onclick='document.body.removeChild( document.getElementById( \"hop_default_failure_background\" ) ); document.body.removeChild( document.getElementById( \"hop_default_failure\" ) );' title='Click to hide this message'>" );
   t = t.replace( /<\/html>/g, "</div>" );
   t = t.replace( /<body[^>]*>/g, "<div align='center' style='border: 3px dashed red; overflow: auto; width: 50em; background: white; padding: 4px; font-family: sans serif; text-align: center;'>" );
   t = t.replace( /<\/body>/g, "</div>" );
/*    t = t.replace( /&lt;/g, "<" );                                   */
/*    t = t.replace( /&gt;/g, ">" );                                   */
   t = t.replace( /&quot;/g, "\"" );

   if( !div2 ) {
      div2 = document.createElement( "div" );
      div2.id = "hop_default_failure_background";
      node_style_set( div2, "position", "fixed" );
      node_style_set( div2, "top", "0" );
      node_style_set( div2, "bottom", "0" );
      node_style_set( div2, "left", "0" );
      node_style_set( div2, "right", "0" );
      node_style_set( div2, "background", "#000" );
      node_style_set( div2, "opacity", "0.5" );
      node_style_set( div2, "overflow", "hidden" );
      node_style_set( div2, "text-align", "center" );
      node_style_set( div2, "z-index", "9999" );

      document.body.appendChild( div2 );
   }
   
   if( !div ) {
      div = document.createElement( "div" );
      div.id = "hop_default_failure";
      node_style_set( div, "position", "fixed" );
      node_style_set( div, "top", "100px" );
      node_style_set( div, "left", "0" );
      node_style_set( div, "right", "0" );
      node_style_set( div, "text-align", "center" );
      node_style_set( div, "border", "0" );
      node_style_set( div, "z-index", "10000" );
      node_style_set( div, "opacity", "1" );
      div.align = "center";

      div.innerHTML = t;

      document.body.appendChild( div );
   } else {
      div.innerHTML = t;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_failure_alert ...                                            */
/*---------------------------------------------------------------------*/
function hop_failure_alert( http ) {
   var t = http.responseText;
   
   t = t.replace( /<\tr>/g, "\n" );
   t = t.replace( /<[^>]+>/g, " " );
   t = t.replace( /&lt;/g, "<" );
   t = t.replace( /&gt;/g, ">" );
   t = t.replace( /&quot;/g, "\"" );
   
   alert( "*** Hop Error " + http.status + ": " + t );
}

/*---------------------------------------------------------------------*/
/*    hop_anim ...                                                     */
/*---------------------------------------------------------------------*/
function hop_anim( title ) {
   var vis = document.createElement( "div" );
      
   node_style_set( vis, "position", "fixed" );
   node_style_set( vis, "top", "5px" );
   node_style_set( vis, "right", "5px" );
   node_style_set( vis, "z-index", "100" );
   node_style_set( vis, "background", "#eeeeee" );
   node_style_set( vis, "border-color", "black" );
   node_style_set( vis, "border-style", "outset" );
   node_style_set( vis, "border-width", "1px" );
   node_style_set( vis, "padding", "2px" );
   node_style_set( vis, "-moz-opacity", "0.7" );
   node_style_set( vis, "width", "16px" );
   node_style_set( vis, "height", "16px" );
      
   vis.title = title;

   var img = document.createElement( "img" );
   img.classname = "hop-busy-anim";
   img.src = hop_busy_anim;

   vis.appendChild( img );
 
   return vis;
}

/*---------------------------------------------------------------------*/
/*    hop_inner ...                                                    */
/*---------------------------------------------------------------------*/
function hop_inner( http, success, failure, vis ) {
   http.onreadystatechange = function() {
      if( http.readyState == 4 ) {
	 var status;

	 if( vis != false ) {
	    document.body.removeChild( vis );
	 }

	 try {
	    status = http.status;
	 } catch( e ) {
	    if( failure ) {
	       failure( http );
	    } else {
	       throw( e );
	    }
	 }

	 switch( status ) {
	    case 200:
	    if( success ) {
	       success( http );
	    } else {
	       hop_js_eval( http );
	    }
	    break;

	    case 204:
	    break;

	    case 257:
	    hop_js_eval( http );
	    break;

	    case 258:
	    if( http.responseText != null ) eval( http.responseText );
	    break;

	    case 259:
	    hop_set_cookie( http );
	    break;

	    case 407:
	    alert( "*** Hop Authentication Error " + http.status + ": `"
		   + http.responseText + "'" );
	    break;

	    default:
	    if( (status > 200) && (status < 300) ) {
	       if( success ) {
		  success( http );
	       }
	    } else {
	       if( failure ) {
		  failure( http );
	       } else {
		  hop_default_failure( http );
	       }
	    }
	 }
      }
   }

   try {
      http.send( null );
   } catch( e ) {
      alert( "*** HOP send error: " + e );
   }

   return http;
}

/*---------------------------------------------------------------------*/
/*    resume_XXX ...                                                   */
/*---------------------------------------------------------------------*/
var resume_success = hop_replace_document;
var resume_failure = false;

/*---------------------------------------------------------------------*/
/*    hop ...                                                          */
/*---------------------------------------------------------------------*/
function hop( service, success, failure, sync ) {
   if( success == true ) {
      location.href = service;
      return true;
   } else {
      resume_success = success;
      resume_failure = failure;
   }

   var http = hop_make_xml_http_request();
   var vis = hop_anim( service );

   document.body.appendChild( vis );

   http.open( "GET", service, (sync != true) );

   http.setRequestHeader( 'Content-Type', 'application/x-www-form-urlencoded; charset=ISO-8859-1' );
   http.setRequestHeader( 'Connection', 'close' );
   http.setRequestHeader( 'Hop-Env', hop_serialize_request_env() );

   return hop_inner( http, success, failure, vis );
}

/*---------------------------------------------------------------------*/
/*    WithHopError ...                                                 */
/*---------------------------------------------------------------------*/
function WithHopError( service ) {
   var e = new Error( "with-hop error" );
   e.service = service;

   return e;
}

/*---------------------------------------------------------------------*/
/*    with_hop ...                                                     */
/*---------------------------------------------------------------------*/
function with_hop( service, success, failure ) {
   if( !success ) success = function( h ) { return h };
/*    if( !failure ) failure = function( h ) { throw new WithHopError(service); }; */
   
   return hop( service,
	       function( http ) {
                 var json;

                 switch( http.status ) {
		    case 200:
		       if( hop_is_http_json( http ) ) {
			  success( eval( http.responseText ) );
		       } else {
			  success( http.responseText );
		       }
		       return;
		    case 202:
		       success( hop_unserialize( http.responseText ) );
		       return;
		    default:
		       success( http );
		       return;
		    }
		 }, 
	       failure );
}

/*---------------------------------------------------------------------*/
/*    with_hop_callcc ...                                              */
/*---------------------------------------------------------------------*/
function with_hop_callcc( service ) {
   var sc_storage = sc_CALLCC_STORAGE;
   if (sc_storage.doRestore) {
      var res = sc_callcc();
      if (res.failure)
	 throw res.value; // TODO
      else
	 return res.value;
   } else {
      sc_callcc(function(k) {
	 function success(val) {
	    k({value: val});
	 };
	 function failure(val) {
	    k({failure: true, value: val});
	 };
	 hop( service,
	      function( http ) {
		 var json;

		 switch( http.status ) {
		 case 200:
		    if( hop_is_http_json( http ) ) {
		       success( eval( http.responseText ) );
		    } else {
		       success( http.responseText );
		    }
		    return;
		 case 202:
		    success( hop_unserialize( http.responseText ) );
		    return;
		 default:
		    success( http );
		    return;
		 }
	      }, 
	      failure );
	 sc_EMPTY_CALLCC(); // abort execution here.
      });
   }
   return undefined; // for FF2.0
}

/*---------------------------------------------------------------------*/
/*    hop_event_hander_set ...                                         */
/*---------------------------------------------------------------------*/
function hop_event_handler_set( svc, evt, success, failure ) {
   var req = hop_make_xml_http_request();
   
   var handler = function ( http ) {
      http.eventName = evt;
      var res = success( http );

      if( res ) {
	 hop_event_handler_set( svc, evt, success, failure );
      }
			
      return res;
   }

   req.open( "GET", svc( evt ) );

   req.setRequestHeader( 'Content-Type', 'application/x-www-form-urlencoded; charset=ISO-8859-1' );
   req.setRequestHeader( 'Connection', 'close' );

   return hop_inner( req, handler, failure, false );
}

/*---------------------------------------------------------------------*/
/*    hop_current_tooltip ...                                          */
/*---------------------------------------------------------------------*/
var hop_current_tooltip;

/*---------------------------------------------------------------------*/
/*    hop_tooltip_show ...                                             */
/*---------------------------------------------------------------------*/
function hop_tooltip_show( event, id, ux, uy ) {
   var el;

   if( hop_is_html_element( id ) ) {
      el = id;
   } else {
      if( (id instanceof String) || (typeof id == "string") ) {
	 el = document.getElementById( id );
      } else {
	 alert( "*** ERROR:hop_tooltip_show:Illegal id -- " + id );
      }
   }

   if( hop_is_html_element( el ) && (hop_current_tooltip != el) ) {

      var p = el.parentNode;
      
      if( p != document.body ) {
	 p.removeChild( el );
	 document.body.appendChild( el );
      }

      if( hop_is_html_element( hop_current_tooltip ) ) {
	 node_style_set( hop_current_tooltip, "visibility", "hidden" );
      }

      node_style_set( el, "visibility", "visible" );
      
      var x = ux != undefined ? ux : event.pageX - 200;
      var y = uy != undefined ? uy : event.pageY - 200;
      var b = document.getElementsByTagName( "body" )[ 0 ];

      node_style_set( el, "left", (x < 0) ? 0 : (x + "px") );
      node_style_set( el, "top", (y < 0) ? 0: (y + "px") );

      // re-parent the popup
      if( el.parentNode != b ) {
	 el.parentNode.removeChild( el );
	 b.appendChild( el );
      }

      hop_current_tooltip = el;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tooltip_hide ...                                             */
/*---------------------------------------------------------------------*/
function hop_tooltip_hide() {
   if( hop_is_html_element( hop_current_tooltip ) ) {
      node_style_set( hop_current_tooltip, "visibility", "hidden" );
      hop_current_tooltip = null;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_set_cookie ...                                               */
/*---------------------------------------------------------------------*/
function hop_set_cookie( http ) {
   try {
      var cookie = http.getResponseHeader( "set-cookie" );
      if( cookie )
	 document.cookie = cookie;
   } catch( e ) {
      ;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_cookie_remove ...                                            */
/*---------------------------------------------------------------------*/
function hop_cookie_remove( name, path, domain ) {
   if( hop_cookie_get_value( name ) ) {
      hop_cookie_set_value( name, "", path, domain );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_cookie_get_value ...                                         */
/*---------------------------------------------------------------------*/
function hop_cookie_get_value( name ) {
   var cookies = document.cookie;
   var i = cookies.indexOf( name + "=" );
   
   if( i !== -1 ) {
      var start = i + name.length + 1;
      var end = cookies.indexOf( ";", start );
      if( end == -1 ) end = cookies.length;
      return unescape( cookies.substring( start, end ) );
   } else {
      return null;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_cookie_set_value ...                                         */
/*---------------------------------------------------------------------*/
function hop_cookie_set_value( name, val, path, domain, expires ) {
   var cookie = name + "=" + val;

   if( (path instanceof String) || (typeof path == "string") ) {
      cookie += "; path=" + path;
   } else {
      cookie += "; path=/";
   }

   if( (expires instanceof String) || (typeof expires == "string") ) {
      cookie += "; expires=" + expires;
   } else {
      if( expires instanceof Date ) {
	 cookie += "; expires=" + expires.toGMTString();
      }
   }

   if( (domain instanceof String) || (typeof domain == "string") ) {
      cookie += "; domain=" + domain;
   }
   
   document.cookie = cookie;
}

/*---------------------------------------------------------------------*/
/*    hop_request_env ...                                              */
/*---------------------------------------------------------------------*/
var hop_request_env = [];
var hop_request_env_string = "";

/*---------------------------------------------------------------------*/
/*    hop_serialize_request_env ...                                    */
/*---------------------------------------------------------------------*/
function hop_serialize_request_env() {
   if( (hop_request_env_string == null) ||
       (hop_request_env_string.length == 0) ) {
      var tmp = null;

      for( var p in hop_request_env ) {
	 if( typeof hop_request_env[ p ] != "function" ) {
	    tmp = sc_cons( sc_cons( p, hop_request_env[ p ] ) );
	 }
      }
      
      hop_request_env_string = hop_serialize( tmp );
   }
   return hop_request_env_string;
}

/*---------------------------------------------------------------------*/
/*    hop_request_reset ...                                            */
/*    -------------------------------------------------------------    */
/*    Is this really needed?                                           */
/*    I think that if it is, a function that returns the whole list    */
/*    of currently binding cells will also be required. For now,       */
/*       this function is not bound in the Hop syntax (hop-alias.scm). */
/*---------------------------------------------------------------------*/
function hop_request_reset() {
   hop_request_env = [];
   hop_request_env_string = "";
   return null;
}

/*---------------------------------------------------------------------*/
/*    hop_request_set ...                                              */
/*---------------------------------------------------------------------*/
function hop_request_set( key, val ) {
   hop_request_env_string = null;
   hop_request_env[ key ] = val;
   return val;
}

/*---------------------------------------------------------------------*/
/*    hop_request_get ...                                              */
/*---------------------------------------------------------------------*/
function hop_request_get( key ) {
   return hop_request[ key ];
}

/*---------------------------------------------------------------------*/
/*    hop_element_x ...                                                */
/*---------------------------------------------------------------------*/
function hop_element_x( obj ) {
   var res = 0;

   while( obj != null ) {
      if( typeof obj.offsetLeft == "number" ) 
	 res += obj.offsetLeft;
      else {
	 break;
      }
      obj = obj.offsetParent;
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    hop_element_y ...                                                */
/*---------------------------------------------------------------------*/
function hop_element_y( obj ) {
   var res = 0;

   while( obj != null ) {
      if( typeof obj.offsetTop == "number" ) 
	 res += obj.offsetTop;
      else {
	 break;
      }
      obj = obj.offsetParent;
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    hop_timeout ...                                                  */
/*---------------------------------------------------------------------*/
function hop_timeout( id, timeout, proc, eager ) {
   window[ id ] = setInterval( proc, timeout );
   window[ id ].proc = proc;
   window[ id ].timeout = timeout;
   
   if( eager == true ) proc();
}

/*---------------------------------------------------------------------*/
/*    hop_timeout_reset ...                                            */
/*---------------------------------------------------------------------*/
function hop_timeout_reset( id, timeout, proc ) {
   var p = proc ? proc : window[ id ].proc;
   var t = timeout ? timeout : window[ id ].timeout;
   clearInterval( window[ id ] );

   window[ id ] = setInterval( p, t );
}

/*---------------------------------------------------------------------*/
/*    hop_clear_timeout ...                                            */
/*---------------------------------------------------------------------*/
function hop_clear_timeout( id ) {
   clearInterval( window[ id ] );
   window[ id ] = false;
}

/*---------------------------------------------------------------------*/
/*    hop_load_frequency ...                                           */
/*---------------------------------------------------------------------*/
var hop_load_frequency = 100;

/*---------------------------------------------------------------------*/
/*    HopLoadError ...                                                 */
/*---------------------------------------------------------------------*/
function HopLoadError( file ) {
   var e = new Error( "hop-load error" );
   e.file = file;

   return e;
}

/*---------------------------------------------------------------------*/
/*    hop_load ...                                                     */
/*---------------------------------------------------------------------*/
function hop_load( src, timeout ) {
   var script = document.createElement( "script" );
   script.src = src;
   var loaded = false;
   var holder = document.getElementsByTagName( "head" );

   if( !timeout || (timeout == undefined) ) timeout = -1;

   if( holder != null ) {
      if( timeout != 0 ) script.onload = function( e ) { loaded = true; };
      holder[ 0 ].appendChild( script );
      if( timeout != 0 ) {
	 var it;
	 var p = function() {
	    if( loaded == true ) {
	       alert( "timeout=" + loaded );
	       clearInterval( it );
	    } else {
	       if( timeout > 0 ) {
		  timeout -= hop_load_frequency;
		  if( timeout <= 0 ) {
		     alert( "timeout <=0 " + loaded );
		     clearInterval( it );
		     throw( new HopLoadError( src ) );
		  }
	       }
	    }
	 };
	 it = setInterval( p, hop_load_frequency );
      }
   } else {
      alert( "*** Hop Error, Can't find HEAD element" );
   }
}

/*---------------------------------------------------------------------*/
/*    function                                                         */
/*    hop_style_attribute_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_style_attribute_set( obj, val ) {
   var expr;
   if( (val instanceof String) || (typeof val == "string") )
      expr = eval( val );
   
   for( var p in expr ) {
      node_style_set( obj, p, expr[ p ] );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_serialize ...                                                */
/*---------------------------------------------------------------------*/
function hop_serialize( item ) {
   return encodeURIComponent( hop_bigloo_serialize( item ) );
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize ...                                         */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize( item ) {
   var tname = typeof item;
   
   if( (item instanceof String) || (tname == "string") ) {
      if( sc_isSymbol_immutable( item ) ) {
	 return "'"
	    + hop_serialize_string( '"', sc_symbol2string_immutable( item ) );
      } else {
	 return hop_serialize_string( '"', item );
      }
   }

   if( (typeof item) == "number" )
      return hop_serialize_number( item );
      
   if( (item instanceof Boolean) || (tname == "boolean") )
      return hop_serialize_boolean( item );
      
   if( item instanceof Array )
      return hop_serialize_array( item );
   
   if( item == undefined )
      return ";";
   
   if( item == null )
      return ".";

   if( item instanceof Date )
      return hop_serialize_date( item );

   if( (item instanceof Object) &&
       (typeof item.hop_bigloo_serialize == "function") )
      return item.hop_bigloo_serialize();

   if( (HTMLCollection != undefined) && (item instanceof HTMLCollection) )
      return hop_serialize_array( item );
      
   if( (HTMLInputElement != undefined) && (item instanceof HTMLInputElement) )
      return hop_bigloo_serialize( item.value );

   if( (HTMLTextAreaElement != undefined) && (item instanceof HTMLTextAreaElement) )
      return hop_bigloo_serialize( item.value );

   if( (HTMLSelectElement != undefined) && (item instanceof HTMLSelectElement) )
      return hop_bigloo_serialize( item.value );

   alert( "*** Hop Error, Can't serialize element: `" + item +
	  "' (" + tname + "). Ignoring value." );
   
   return hop_bigloo_serialize( false );
}

/*---------------------------------------------------------------------*/
/*    hop_size_of_word ...                                             */
/*---------------------------------------------------------------------*/
function hop_size_of_word( word ) {
   var s = 0;

   while( word > 0 ) {
      s++;
      word >>= 8;
   }

   return s;
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_word ...                                           */
/*---------------------------------------------------------------------*/
function hop_serialize_word( word ) {
   var s = hop_size_of_word( word );

   if( s == 0 ) {
      return String.fromCharCode( s ).valueOf();
   } else {
      var rw = String.fromCharCode( s ) + '';

      s--;
      while( s >= 0 ) {
	 var c = ((word >> (s * 8)) & 0xff);

	 rw += String.fromCharCode( c );
	 s--;
      }

      return rw;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_string ...                                         */
/*---------------------------------------------------------------------*/
function hop_serialize_string( mark, item ) {
   return mark + hop_serialize_word( item.length ) + item;
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_number ...                                         */
/*---------------------------------------------------------------------*/
function hop_serialize_number( item ) {
   var sitem = item + "";

   if( sitem.indexOf( "." ) == -1 ) {
      if( item < 0 )
	 return '-' + (-item);
      else 
	 return hop_serialize_word( item );
   } else {
      return 'f' + hop_serialize_word( sitem.length ) + sitem;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_boolean ...                                        */
/*---------------------------------------------------------------------*/
function hop_serialize_boolean( item ) {
   return item ? 'T' : 'F';
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_array ...                                          */
/*---------------------------------------------------------------------*/
function hop_serialize_array( item ) {
   var l = item.length;
   var ra = '[' + hop_serialize_word( l );
   var i = 0;

   for( i = 0; i < l; i++ ) {
      ra += hop_bigloo_serialize( item[ i ] );
   }

   return ra;
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_date ...                                           */
/*---------------------------------------------------------------------*/
function hop_serialize_date( item ) {
   var utc = Date.UTC( item.getUTCFullYear(),
		       item.getUTCMonth(),
		       item.getUTCDate(),
		       item.getUTCHours(),
		       item.getUTCMinutes(),
		       item.getUTCSeconds() ) + "";
   var ms = utc.substring( 0, utc.length - 3 );

   return 'd' + hop_serialize_word( ms.length ) + ms;
}

/*---------------------------------------------------------------------*/
/*    hop_set_state ...                                                */
/*---------------------------------------------------------------------*/
function hop_set_state( id, op, val ) {
   var hash = window.location.hash;
   var i = hash.indexOf( id + "=" );

   if( i == -1 ) {
      if( hash.length == 0 ) {
	 hash = "#" + id + "=" + op + ":" + val;
      } else {
	 hash += hash + "," + id + "=" + op + ":" + val;
      }
   } else {
      var end = hash.indexOf( ",", i + 1 );
      if( end == -1 ) {
	 if( i == 1 ) {
	    hash =  "#" + id + "=" + op + ":" + val;
	 } else {
	    var pref = hash.substring( 0, i );
	    hash = pref + "," + id + "=" + op + ":" + val;
	 }
      } else {
	 var suf = hash.substring( end );
	 if( i == 1 ) {
	    hash = "#" + id + "=" + op + ":" + val + suf;
	 } else {
	    var pref = hash.substring( 0, i );
	    hash = pref + "," + id + "=" + op + ":" + val + suf;
	 }
      }
   }
      
   hop_location_set( document, hash );
}

/*---------------------------------------------------------------------*/
/*    hop_eval_state ...                                               */
/*---------------------------------------------------------------------*/
function hop_eval_state( location ) {
   var hash = location.hash;

   if( hash.length > 0 ) {
      var split = hash.split( "," );
      for( var i = 0; i < split.length; i++ ) {
	 var el = hash.match( /#?([^=]+)=([^:]+):([^:]+)+/ );
	 if( el ) {
	    var id = el[ 1 ];
	    var op = el[ 2 ];
	    var arg = el[ 3 ];

	    if( op == "np" ) {
	       var np = document.getElementById( id );
	       hop_notepad_inner_select( np, parseInt( arg ) );
	    }
	 }
      }
   }
}

window.onload = function() {
   hop_add_event_listener( document, "location", hop_eval_state );
};
      
/* {*---------------------------------------------------------------------*} */
/* {*    hopBehaviour class ...                                           *} */
/* {*---------------------------------------------------------------------*} */
/* var hopBehaviour = {                                                */
/*     behaviours: {},                                                 */
/*                                                                     */
/*     register: function( className, func ) {                         */
/* 	hopBehaviour.behaviours[ className ] = func;                   */
/*     },                                                              */
/*                                                                     */
/*     plug: function() {                                              */
/* 	var all = hopBehaviour.behaviours;                             */
/*                                                                     */
/* 	for( var name in all ) {                                       */
/* 	    var list = document.getElementsByClass( name );            */
/* 	                                                               */
/* 	    for( var i in list ) {                                     */
/* 		all[ name ]( list[ i ] );                              */
/* 	    }                                                          */
/* 	}                                                              */
/*     },                                                              */
/*                                                                     */
/*     start: function() {                                             */
/* 	var oldonload = window.onload;                                 */
/*                                                                     */
/* 	if( typeof window.onload != 'function' ) {                     */
/* 	    window.onload = hopBehaviour.plug;                         */
/* 	} else {                                                       */
/* 	    window.onload = function() {                               */
/* 		oldonload();                                           */
/* 		hopBehaviour.plug();                                   */
/* 	    }                                                          */
/* 	}                                                              */
/*     }                                                               */
/* };                                                                  */
/*                                                                     */
/* hopBehaviour.start();                                               */
/*                                                                     */
