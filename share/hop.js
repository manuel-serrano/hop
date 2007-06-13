/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop.js                            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec 25 06:57:53 2004                          */
/*    Last change :  Wed Jun 13 17:47:02 2007 (serrano)                */
/*    Copyright   :  2004-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Standard HOP JavaScript library                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_busy_anim ...                                                */
/*---------------------------------------------------------------------*/
var hop_busy_anim_16_16 = "data:image/gif;base64,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";

var hop_busy_anim_128_128 = "data:image/gif;base64,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";

var hop_busy_anim = hop_busy_anim_128_128;

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

   try {
      if( scripts.length > 0 ) {
	 for ( var j = 0; j < scripts.length; j++ ) {
	    if( scripts[ j ].childNodes.length > 0 ) {
	       res = eval( scripts[ j ].childNodes[ 0 ].nodeValue );
	    }
	 }
      } else {
	 var script = text.match( /<script[^>]*>/i );
	 if( script != null ) {
	    /* I don't understand why yet, IE 7 does not include */
	    /* SCRIPT nodes in the resulting node!               */
	    var start = script.index + script[ 0 ].length;
	    var end = text.search( /<\/script>/i );
	    if( (end != null) && (end > start) ) {
	       res = eval( text.substr( start, end - start ) );
	    }
	 }
      }
   } catch( e ) {
      alert( e );
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
/*    hop_anim_16_16 ...                                               */
/*---------------------------------------------------------------------*/
function hop_anim_16_16( title ) {
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
/*    hop_anim_128_128 ...                                             */
/*---------------------------------------------------------------------*/
function hop_anim_128_128( title ) {
   var vis = document.createElement( "div" );
   var y = (window.innerHeight / 2) - 64;
   var x = (window.innerWidth / 2) - 64;
   
   node_style_set( vis, "position", "fixed" );
   node_style_set( vis, "top", "150px" );
   node_style_set( vis, "left", x + "px" );
   node_style_set( vis, "z-index", "100" );
   node_style_set( vis, "background", "#eeeeee" );
   node_style_set( vis, "border-color", "black" );
   node_style_set( vis, "border-style", "outset" );
   node_style_set( vis, "-moz-border-radius", "0.3em" );
   node_style_set( vis, "border-width", "1px" );
   node_style_set( vis, "padding", "2px" );
   node_style_set( vis, "-moz-opacity", "0.85" );
   node_style_set( vis, "width", "130px" );
   node_style_set( vis, "height", "130px" );
      
   vis.title = title;

   var img = document.createElement( "img" );
   img.classname = "hop-busy-anim";
   img.src = hop_busy_anim;

   vis.appendChild( img );
   
   return vis;
}

/*---------------------------------------------------------------------*/
/*    hop_anim_vis ...                                                 */
/*---------------------------------------------------------------------*/
var hop_anim_vis = false;

/*---------------------------------------------------------------------*/
/*    hop_anim ...                                                     */
/*---------------------------------------------------------------------*/
function hop_anim( title ) {
   if( hop_anim_vis ) {
      hop_anim_vis.title = title;
      node_style_set( hop_anim_vis, "visibility", "visible" );
      return hop_anim_vis;
   } else {
      hop_anim_vis = hop_anim_128_128( title );
      document.body.appendChild( hop_anim_vis );
      return hop_anim_vis;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_inner ...                                                    */
/*---------------------------------------------------------------------*/
function hop_inner( http, success, failure, vis ) {
   http.onreadystatechange = function() {
      if( http.readyState == 4 ) {
	 var status;

	 if( vis != false ) {
	    node_style_set( vis, "visibility", "hidden" );
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

   http.open( "GET", service, (sync != true) );

   http.setRequestHeader( 'Content-Type', 'application/x-www-form-urlencoded; charset=ISO-8859-1' );
   http.setRequestHeader( 'Connection', 'close' );
   http.setRequestHeader( 'Hop-Env', hop_serialize_request_env() );

   return hop_inner( http, success, failure, hop_anim( service ) );
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
   
   return hop( service,
	       function( http ) {
                 var json;

                 switch( http.status ) {
		    case 200:
		       if( hop_is_http_json( http ) ) {
			  success( eval( http.responseText ) );
		       } else {
			  success( http.responseText );
			  // MS: 6 Jun 2007, the previous version were
			  // not correctly evaluating JS scripts!
			  // The scripts have to be evaluated after the
			  // success callback has been called because this
			  // might create new nodes referenced in the scripts.
			  hop_js_eval( http );
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
/*    hop_innerHTML_set ...                                            */
/*---------------------------------------------------------------------*/
function hop_innerHTML_set( nid, html ) {
   if( (nid instanceof String) || (typeof nid == "string") ) {
      var el = document.getElementById( nid );

      if( el == undefined ) {
	 alert("*** ERROR:innerHTML-set! -- cannot find element \""
	       + nid + "\"");
      }

      el.innerHTML = html;
   } else {
      nid.innerHTML = html;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_outerHTML_set ...                                            */
/*---------------------------------------------------------------------*/
function hop_outerHTML_set( nid, html ) {
   if( (nid instanceof String) || (typeof nid == "string") ) {
      var el = document.getElementById( nid );
      var p;
      
      if( el == undefined ) {
	 alert("*** ERROR:innerHTML-set! -- cannot find element \""
	       + nid + "\"");
      }
      
      el.parentNode.innerHTML = html;
   } else {
      nid.parentNode.innerHTML = html;
   }
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
var hop_request_env_invalid = false;

/*---------------------------------------------------------------------*/
/*    hop_serialize_request_env ...                                    */
/*---------------------------------------------------------------------*/
function hop_serialize_request_env() {
   if( hop_request_env_invalid ) {
      var tmp = null;

      for( var p in hop_request_env ) {
	 if( typeof hop_request_env[ p ] != "function" ) {
	    tmp = sc_cons( sc_cons( p, hop_request_env[ p ] ), tmp );
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
   hop_request_env_string = "";
   hop_request_env_set = false;
   return null;
}

/*---------------------------------------------------------------------*/
/*    hop_request_set ...                                              */
/*---------------------------------------------------------------------*/
function hop_request_set( key, val ) {
   hop_request_env_invalid = true;
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
/*    hop_window_onload_add ...                                        */
/*---------------------------------------------------------------------*/
function hop_window_onload_add( proc ) {
   var oldonload = window.onload;

   if( typeof oldonload != 'function' ) {
      window.onload = proc;
   } else {
      window.onload = function( e ) {
	 oldonload( e );
	 proc( e );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_onunload_add ...                                      */
/*---------------------------------------------------------------------*/
function hop_window_onunload_add( proc ) {
   var oldonload = window.onunload;

   if( typeof oldonunload != 'function' ) {
      window.onunload = proc;
   } else {
      window.onunload = function( e ) {
	 oldonunload( e );
	 proc( e );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_update ...                                                   */
/*    -------------------------------------------------------------    */
/*    This function is called when a widget select a new child         */
/*    (e.g., a notepad or a tabslider). It gives a child the           */
/*    opportunity to update (i.e., to re-compute dimensions).          */
/*    Widgets interested have to register by setting their             */
/*    hop_update field (see hop-tabliser.js for an example).           */
/*---------------------------------------------------------------------*/
function hop_update( node ) {
   /* update the children recursively */
   if( node.hop_update != undefined ) {
      node.hop_update();
   }
   /* traverse the all tree */
   for( var i = 0; i < node.childNodes.length; i++ ) {
      hop_update( node.childNodes[ i ] );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_typeof ...                                                   */
/*    -------------------------------------------------------------    */
/*    A wrapper for using typeof as a function in Hop.                 */
/*---------------------------------------------------------------------*/
function hop_find_runtime_type( obj ) {
   if( obj instanceof Object ) {
      if( obj instanceof Date ) {
	 return "date";
      } else {
	 if( obj instanceof RegExp ) {
	    return "regexp";
	 } else {
	    if( typeof item.hop_find_runtime_type == "function" ) 
	       return item.hop_find_runtime_type();
	    else
	       return "object";
	 }
      }
   } else {
      return typeof obj;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_state_history_handler ...                                    */
/*---------------------------------------------------------------------*/
var hop_current_state_history = undefined;
var hop_state_history_handler = {};

/*---------------------------------------------------------------------*/
/*    hop_state_history_register_handler ...                           */
/*---------------------------------------------------------------------*/
function hop_state_history_register_handler( key, reset, proc ) {
   hop_state_history_handler[ key ] = { reset: reset, proc: proc };
}

/*---------------------------------------------------------------------*/
/*    _hop_state_entry ...                                             */
/*    -------------------------------------------------------------    */
/*    Private class.                                                   */
/*---------------------------------------------------------------------*/
function _hop_state_entry( op, val ) {
   this.op = op;
   this.val = val;
   this.close = false;
}

/*---------------------------------------------------------------------*/
/*    hop_state_history_to_location ...                                */
/*---------------------------------------------------------------------*/
function hop_state_history_to_location( state ) {
   var loc = undefined;
   
   for( p in state ) {
      if( state[ p ] instanceof _hop_state_entry ) {
	 if( loc == undefined ) {
	    loc = "#" + p + "=" + state[ p ].op + ":" + state[ p ].val;
	 } else {
	    loc += "," + p + "=" + state[ p ].op + ":" + state[ p ].val;
	 }
      }
   }

   return loc;
}

/*---------------------------------------------------------------------*/
/*    hop_hash_history_regexp ...                                      */
/*---------------------------------------------------------------------*/
var hop_hash_history_regexp = /#?([^=]+)=([^:]+):([^,]+)+/;

/*---------------------------------------------------------------------*/
/*    hop_location_to_state_history ...                                */
/*---------------------------------------------------------------------*/
function hop_location_to_state_history( hash ) {
   var state = {};
   var split = hash.split( "," );
   for( var i = 0; i < split.length; i++ ) {
      var el = split[ i ].match( hop_hash_history_regexp );
      if( el ) {
	 var id = el[ 1 ];
	 var op = el[ 2 ];
	 var val = el [ 3 ];

	 state[ id ] = new _hop_state_entry( op, val );
      }
   }

   return state;
}

/*---------------------------------------------------------------------*/
/*    hop_state_history_push ...                                       */
/*    -------------------------------------------------------------    */
/*    Store the new state but don't add a new location to the          */
/*    browser URL bar.                                                 */
/*---------------------------------------------------------------------*/
function hop_state_history_push( id, op, val ) {
   if( hop_current_state_history == undefined ) {
      /* create a new state */
      hop_current_state_history = {};
      hop_current_state_history[ id ] = new _hop_state_entry( op, val );
   } else {
      /* update the current state */
      var olde = hop_current_state_history[ id ];

      if( olde == undefined ) {
	 /* add a new entry to the current state */
	 hop_current_state_history[ id ] = new _hop_state_entry( op, val );
      } else {
	 if( (olde.op != op) || (olde.val != val) ) {
	    /* update the current state */
	    olde.op = op;
	    olde.val = val;
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_state_history_flush ...                                      */
/*    -------------------------------------------------------------    */
/*    Generates a new browser URL from the current history state.      */
/*---------------------------------------------------------------------*/
function hop_state_history_flush() {
   /* store the new state as a location for bookmarking an history */
   var loc = hop_state_history_to_location( hop_current_state_history );
   var old = window.location.href;
   var i = old.indexOf( "#" );

   /* store the new browser URL */
   if( i == -1 ) {
      hop_active_location_set( document, old + loc );
   } else {
      hop_active_location_set( document, old.substring( 0, i ) + loc );
   }
}
   
/*---------------------------------------------------------------------*/
/*    hop_state_history_transaction ...                                */
/*---------------------------------------------------------------------*/
var hop_state_history_transaction = 0;

/*---------------------------------------------------------------------*/
/*    hop_state_history_add ...                                        */
/*---------------------------------------------------------------------*/
function hop_state_history_add( id, op, val ) {
   /* prepare the new current state */
   hop_state_history_push( id, op, val );

   if( hop_state_history_transaction == 0 ) {
      hop_state_history_flush();
   }
}

/*---------------------------------------------------------------------*/
/*    hop_with_history ...                                             */
/*---------------------------------------------------------------------*/
function hop_with_history( proc ) {
   var res;
   hop_state_history_transaction++;
   try {
      res = proc();
   } finally {
      hop_state_history_transaction--;
   }
   hop_state_history_flush();
   return res;
}
   
/*---------------------------------------------------------------------*/
/*    hop_state_history_reset ...                                      */
/*    -------------------------------------------------------------    */
/*    When there is already an existing state, we have to reset all    */
/*    its entries.                                                     */
/*---------------------------------------------------------------------*/
function hop_state_history_reset() {
   if( hop_current_state_history != undefined ) {
      /* there is a state, we reset all the entries */
      for( p in hop_current_state_history ) {
	 if( hop_current_state_history[ p ] instanceof _hop_state_entry ) {
	    var op = hop_current_state_history[ p ].op;
	    var handler =  hop_state_history_handler[ op ];
	    if( handler != undefined ) {
	       handler.proc( p, handler.reset );
	    }
	 }
      }

      /* and we erase the state itself */
      hop_current_state_history = undefined;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_state_history_update ...                                     */
/*    -------------------------------------------------------------    */
/*    Compare the two states, reset the entries of the old ones        */
/*    that are no longer present in the new one. Execute the           */
/*    entries that are novel in the new state.                         */
/*    -------------------------------------------------------------    */
/*    This function returns the number of entries that have not        */
/*    been correctly updated.                                          */
/*---------------------------------------------------------------------*/
function hop_state_history_update( olds, news ) {
   var res = 0;
   
   if( olds == undefined ) {
      /* set the new values */
      for( p in news ) {
	 var state = news[ p ];
	 if( state instanceof _hop_state_entry ) {
	    var op = state.op;
	    var handler = hop_state_history_handler[ op ];
	    
	    if( (handler != undefined) && !state.close ) {
	       if( handler.proc( p, state.val ) ) {
		  state.close = true;
	       } else {
		  res++;
	       }
	    }
	 }
      }
   } else {
      /* reset all the entries that used to be in old    */
      /* state that are no longer present in the new one */
      for( p in olds ) {
	 if( (olds[ p ] instanceof _hop_state_entry) &&
	     !(news[ p ] instanceof _hop_state_entry) ) {
	    var op = olds[ p ].op;
	    var handler = hop_state_history_handler[ op ];

	    if( handler != undefined ) {
	       handler.proc( p, handler.reset );
	    }
	 }
      }

      /* update all the entries that are not */
      /* present and equal in old state      */
      for( p in news ) {
	 var state = news[ p ];
	 if( state instanceof _hop_state_entry ) {
	    if( !(olds[ p ] instanceof _hop_state_entry) ||
		(state.op != olds[ p ].op) ||
		(state.val != olds[ p ].val) ) {
	       var op = state.op;
	       var handler =  hop_state_history_handler[ op ];
	       
	       if( (handler != undefined) && !state.close ) {
		  if( handler.proc( p, state.val ) ) {
		     state.close = true;
		  } else {
		     res++;
		  }
	       }
	    }
	 }
      }
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    hop_hash_history_check_regexp ...                                */
/*---------------------------------------------------------------------*/
var hop_hash_history_check_regexp = /^#(?:[^=]+=[^:]+:[^,]+,?)+$/;

/*---------------------------------------------------------------------*/
/*    hop_hash_historyp ...                                            */
/*    -------------------------------------------------------------    */
/*    Is a hash value a legal Hop history?                             */
/*---------------------------------------------------------------------*/
function hop_hash_historyp( hash ) {
   return hop_hash_history_check_regexp( hash );
}

/*---------------------------------------------------------------------*/
/*    hop_eval_history_counter ...                                     */
/*---------------------------------------------------------------------*/
var hop_eval_history_interval = false;

/*---------------------------------------------------------------------*/
/*    function                                                         */
/*    hop_retry_eval_history_state ...                                 */
/*---------------------------------------------------------------------*/
function hop_retry_eval_history_state( count, old_state, new_state ) {
   var init = false;
   var fun = function() {
      if( !init ) {
	 /* skip the first call, we are not ready yet */
	 init = true;
	 return;
      }
      var c = hop_state_history_update( old_state, new_state );

      /* the interval is cancelled if any of the following holds: */
      /*   * c == 0: the update complete                          */
      /*   * c == count: no progress has been made                */
      /*   * hop_eval_history_interval.invalid == false: the      */
      /*     has changed again.                                   */
      if( (c == 0) || (c == count) || hop_eval_history_interval.invalid ) {
	 /* no progress as been made, or the update */
	 /* complete, we cancel the interval        */
	 clearInterval( hop_eval_history_interval );
      }
   }
   hop_eval_history_interval = setInterval( fun, 100 );
   hop_eval_history_interval.invalid = false;
}

/*---------------------------------------------------------------------*/
/*    hop_eval_history_state ...                                       */
/*    -------------------------------------------------------------    */
/*    This function is invoked when the location has changed.          */
/*---------------------------------------------------------------------*/
function hop_eval_history_state( location ) {
   var hash = location.hash;

   if( hop_eval_history_interval )
      hop_eval_history_interval.invalid = true;
   
   if( hash.length == 0 ) {
      hop_state_history_reset();
   } else {
      if( hop_hash_historyp( hash ) ) {
	 var new_state = hop_location_to_state_history( hash );
	 var old_state = hop_current_state_history;
	 var count = hop_state_history_update( old_state, new_state );

	 if( count == 0 ) {
	    /* the update is complete, we state the new state and exit */
	    hop_current_state_history = new_state;
	 } else {
	    /* periodically retry to update */
	    hop_retry_eval_history_state( count, old_state, new_state );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    Install the location event listener                              */
/*---------------------------------------------------------------------*/
if( hop_enable_location_event ) {
   hop_window_onload_add( function( e ) {
      hop_add_event_listener( document, "location", hop_eval_history_state );
   } );
}

/*---------------------------------------------------------------------*/
/*    _hop_history ...                                                 */
/*    -------------------------------------------------------------    */
/*    Private constructor.                                             */
/*---------------------------------------------------------------------*/
function _hop_history( key ) {
   this.key = key;
}

/*---------------------------------------------------------------------*/
/*    hop_make_history ...                                             */
/*    -------------------------------------------------------------    */
/*    This is the high level constructor presented to the Hop          */
/*    API.                                                             */
/*---------------------------------------------------------------------*/
function hop_make_history( key, handler, reset ) {
   hop_state_history_register_handler( key, reset, handler );
   return new _hop_history( key );
}

/*---------------------------------------------------------------------*/
/*    hop_history_add ...                                              */
/*    -------------------------------------------------------------    */
/*    This high level function for adding an entry into the history.   */
/*---------------------------------------------------------------------*/
function hop_history_add( history, id, val ) {
   if( !history instanceof _hop_history ) {
      alert( "*** ERROR: Illegal history object -- " + history );
      return false;
   } else {
      return hop_state_history_add( id, history.key, val );
   }
}

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
