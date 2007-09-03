/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop.js                            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec 25 06:57:53 2004                          */
/*    Last change :  Mon Sep  3 11:39:44 2007 (serrano)                */
/*    Copyright   :  2004-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Standard HOP JavaScript library                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_anim_latency ...                                             */
/*    -------------------------------------------------------------    */
/*    The latency before starting a with_hop animation.                */
/*---------------------------------------------------------------------*/
var hop_anim_latency = 400;

/*---------------------------------------------------------------------*/
/*    hop_busy_anim ...                                                */
/*---------------------------------------------------------------------*/
var hop_busy_anim_16_16 = "data:image/gif;base64,R0lGODlhEAAQAOcAAAAAAAEBAQICAgMDAwQEBAUFBQYGBgcHBwgICAkJCQoKCgsLCwwMDA0NDQ4ODg8PDxAQEBERERISEhMTExQUFBUVFRYWFhcXFxgYGBkZGRoaGhsbGxwcHB0dHR4eHh8fHyAgICEhISIiIiMjIyQkJCUlJSYmJicnJygoKCkpKSoqKisrKywsLC0tLS4uLi8vLzAwMDExMTIyMjMzMzQ0NDU1NTY2Njc3Nzg4ODk5OTo6Ojs7Ozw8PD09PT4+Pj8/P0BAQEFBQUJCQkNDQ0REREVFRUZGRkdHR0hISElJSUpKSktLS0xMTE1NTU5OTk9PT1BQUFFRUVJSUlNTU1RUVFVVVVZWVldXV1hYWFlZWVpaWltbW1xcXF1dXV5eXl9fX2BgYGFhYWJiYmNjY2RkZGVlZWZmZmdnZ2hoaGlpaWpqamtra2xsbG1tbW5ubm9vb3BwcHFxcXJycnNzc3R0dHV1dXZ2dnd3d3h4eHl5eXp6ent7e3x8fH19fX5+fn9/f4CAgIGBgYKCgoODg4SEhIWFhYaGhoeHh4iIiImJiYqKiouLi4yMjI2NjY6Ojo+Pj5CQkJGRkZKSkpOTk5SUlJWVlZaWlpeXl5iYmJmZmZqampubm5ycnJ2dnZ6enp+fn6CgoKGhoaKioqOjo6SkpKWlpaampqenp6ioqKmpqaqqqqurq6ysrK2tra6urq+vr7CwsLGxsbKysrOzs7S0tLW1tba2tre3t7i4uLm5ubq6uru7u7y8vL29vb6+vr+/v8DAwMHBwcLCwsPDw8TExMXFxcbGxsfHx8jIyMnJycrKysvLy8zMzM3Nzc7Ozs/Pz9DQ0NHR0dLS0tPT09TU1NXV1dbW1tfX19jY2NnZ2dra2tvb29zc3N3d3d7e3t/f3+Dg4OHh4eLi4uPj4+Tk5OXl5ebm5ufn5+jo6Onp6erq6uvr6+zs7O3t7e7u7u/v7/Dw8PHx8fLy8vPz8/T09PX19fb29vf39/j4+Pn5+fr6+vv7+/z8/P39/f7+/v///yH/C05FVFNDQVBFMi4wAwEAAAAh/hVDcmVhdGVkIHdpdGggVGhlIEdJTVAAIfkEAQoA/wAsAAAAABAAEAAACF8A/wkc+E+ECIIICRo0mHAgESIFF/5jxgzhw4cRJ1Ks6PAiwY0cOyakSNCMmYYDSZo0iRLkypMNQf5jiVIjQUSIRoYUiBPnx403e/5TpUojyaBDiRY92lBp0ZoDiTYMCAAh+QQBCgD/ACwAAAAAEAAQAAAIXgD/CRz4jwgRgggJGjSYcKAZMwUX/hMhAuHDhxEnUqzo8CLBjRw7JqRIEBGihgOZMftn0iRKlSpbnmwIc6VLlP9UElSlKqHOnTx7poQJlGfOlTURGk36kyZRnEMbBgQAIfkEAQoA/wAsAAAAABAAEAAACF8A/wkc+M+MGYIICRo0mHAgIkQFF/4jQgThw4cRJ1Ks6PAiwY0cOyakSFCVqoYDRYj4Z9IkSpUqW55sCHOlS5T/VBJkxiwhT4Q8fw4M2nNo0H8/iQLtqVRoQ6U4jTYMCAAh+QQBCgD/ACwAAAAAEAAQAAAIXgD/CRz4DxEigggJGjSYcKAqVQUX/jNjBuHDhxEnUqzo8CLBjRw7JqRIkBmzhgOJEPln0iRKlSpbnmwIc6VLlP9UlpxJUIQIhDJ7+vw5MKjLoUSLngzqE2dQnEobBgQAIfkEAQoA/wAsAAAAABAAEAAACGAA/wkc+E+VKoIICRo0mHAgM2YFF/5DhAjhw4cRJ1Ks6PAiwY0cOyakSBBjQ4FmzPy7CLFhypQsT75UaVKmSpEIiRCx6HGgTp0lPYoQ8e/nzqD/hg4terShUqInCS5NGBAAIfkEAQoA/wAsAAAAABAAEAAACF8A/wkc+I8ZM4IICRo0mHAgw4UHValCCLFgRIkTHS4kiDGjxoQSFR5sKBARIosMG5o0WVHlSoskS54cKEJEQjNmENasSRAnToI7bRIh8s9nTqBChxL9SVIp0ZgDhzYMCAAh+QQBCgD/ACwAAAAAEAAQAAAIYAD/CRz4jxkzgggJGjSYcCDDhQcZKlxYMCJFhxcFQkQocSJBESIaDlSl6h9IkCJJkjwZsqHKkihF/iNJkAiRhIgQIbRpk2DOnDV5/jNj5t9PnUGHEi0KVOTSojIHEm0YEAAh+QQBCgD/ACwAAAAAEAAQAAAIXwD/CRz4jxkzgggJGjSYcKAIEQUXRkT48OHEhQcdVlQokaBFhAwHEiHScCDDkSNLYkRJsiHGfylLThxoxkxCVaoQ1qxJECdOgjttIkL0z2dOoEKHEv1ZUilRmQOHNgwIADs=";

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
/*    hop_node_eval ...                                                */
/*---------------------------------------------------------------------*/
function hop_node_eval( node, text ) {
   var res;
   var scripts = node.getElementsByTagName( "script" );

   function hop_node_eval_from_text( text ) {
      var res;
      var start_re = /<script[^>]*>/ig;
      var end_re = /<\/script>/i;
      var script;

      while( (script=start_re.exec( text )) != null ) {
	 /* I don't understand why yet, IE 7 does not include */
	 /* SCRIPT nodes in the resulting node!               */
	 var start = script.index + script[0].length;
	 var end = text.indexOf( "</script>", start );
	 if( end == -1 ) end = text.indexOf( "</SCRIPT>", start );
	 if( (end > start) ) {
	    res = eval( text.substr( start, end - start ) );
	 }
      }

      return res;
   }

   try {
      if( scripts.length > 0 ) {
	 for ( var j = 0; j < scripts.length; j++ ) {
	    if( false && scripts[ j ].childNodes.length > 0 ) {
	       res = eval( scripts[ j ].childNodes[ 0 ].nodeValue );
	    } else {
	       /* this is a buggy browser (Opera 8?) that does not */
	       /* correctly implement script nodes                 */
	       return hop_node_eval_from_text( text );
	    }
	 }
      } else {
	 return hop_node_eval_from_text( text );
      }
   } catch( e ) {
      alert( e );
   }

   return res;
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
   node_style_set( vis, "padding-top", "3px" );
   node_style_set( vis, "width", "20px" );
   node_style_set( vis, "height", "20px" );
   node_style_set( vis, "-moz-border-radius", "0.2em" );
   node_style_set( vis, "-moz-opacity", "0.7" );
      
   vis.title = title;
   vis.align = "center";

   var img = document.createElement( "img" );
   img.className = "hop-busy-anim";

   if( hop_msiep() ) {
      img.src = hop_share_directory() + "/icons/busy-anim-16.gif";
   } else {
      img.src = hop_busy_anim_16_16;
   }

   vis.appendChild( img );
 
   return vis;
}

/*---------------------------------------------------------------------*/
/*    hop_anim_vis ...                                                 */
/*---------------------------------------------------------------------*/
var hop_anim_vis = false;
var hop_anim_service = false;

/*---------------------------------------------------------------------*/
/*    hop_anim ...                                                     */
/*---------------------------------------------------------------------*/
function hop_anim( service ) {
   hop_clear_timeout( "hop_anim_timeout" );

   if( hop_anim_service == service ) {
      if( hop_anim_vis ) {
	 hop_anim_vis.title = service;
	 // node_style_set( hop_anim_vis, "visibility", "visible" );
	 node_style_set( hop_anim_vis, "display", "block" );
	 return hop_anim_vis;
      } else {
	 hop_anim_vis = hop_anim_16_16( service );
	 document.body.appendChild( hop_anim_vis );
	 return hop_anim_vis;
      }
   }
   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_send_request ...                                             */
/*    -------------------------------------------------------------    */
/*    In this function SUCCESS and FAILURE are *always* bound to       */
/*    functions.                                                       */
/*    -------------------------------------------------------------    */
/*    This function DOES NOT evaluates its result.                     */
/*---------------------------------------------------------------------*/
function hop_send_request( svc, sync, success, failure, anim, henv ) {
   var http = hop_make_xml_http_request();

   http.open( "GET", svc, (sync != true) );

   http.setRequestHeader( 'Content-Type', 'application/x-www-form-urlencoded; charset=ISO-8859-1' );
   http.setRequestHeader( 'Connection', 'close' );

   if( henv.length > 0 ) {
      http.setRequestHeader( 'Hop-Env', hop_serialize_request_env() );
   }
   
   http.onreadystatechange = function() {
      if( http.readyState == 4 ) {
	 try {
	    var status = http.status;
	    switch( status ) {
	       case 200:
		  if( hop_is_http_json( http ) ) {
		     try {
			var e = eval( http.responseText );
			return success( eval( http.responseText ), http );
		     } catch( e ) {
			alert( "*** Hop JSON error: " + e );
		     }
		  } else {
		     return success( http.responseText, http );
		  }

	       case 202:
		  return success( hop_unserialize( http.responseText ), http );

	       case 204:
		  return false;

	       case 257:
		  return hop_js_eval( http );

	       case 258:
		  if( http.responseText != null )
		     return eval( http.responseText );
		  else
		     return false;

	       case 259:
		  hop_set_cookie( http );
		  return false;

	       case 407:
		  alert( "*** Hop Authentication Error " + status + ": `"
			 + http.responseText + "'" );
		  return false;

	       default:
		  if( (status > 200) && (status < 300) ) {
		     if( success ) {
			return success( http.responseText, http );
		     }
		  } else {
		     if( failure ) {
			return failure( http );
		     } else {
			return hop_default_failure( http );
		     }
		  }
	    }
	 } finally {
	    if( hop_anim_vis != false ) {
	       node_style_set( hop_anim_vis, "display", "none" );
	    }
	    hop_anim_service = false;
	 }
      }

      return false;
   }

   try {
      if( anim ) hop_anim_service = svc;

      http.send( null );
      hop_clear_timeout( "hop_anim_timeout" );
      
      if( anim ) {
	 hop_timeout( "hop_anim_timeout",
		      hop_anim_latency,
		      function() { hop_anim( svc ); },
		      false );
      }
   } catch( e ) {
      alert( "*** HOP send error: " + e );
   }

   return http;
}

/*---------------------------------------------------------------------*/
/*    with_hop ...                                                     */
/*---------------------------------------------------------------------*/
function with_hop( svc, success, failure, sync ) {
   if( !success ) success = function( h ) { return h };
   if( !failure ) failure = hop_default_failure;
   
   return hop_send_request( svc, sync,
			    success, failure,
			    true, hop_serialize_request_env() );
}

/*---------------------------------------------------------------------*/
/*    hop ...                                                          */
/*    -------------------------------------------------------------    */
/*    This is an old deprecated form that is only used in conjunction  */
/*    with JavaScript client codes.  This function is not used by      */
/*    WITH-HOP and it should be used from Hop client-side code.        */
/*---------------------------------------------------------------------*/
function hop( svc, success, failure, sync ) {
   if( success == true ) {
      location.href = svc;
      return true;
   }

   if( !success ) success = function( h ) { return h; }
   if( !failure ) failure = hop_default_failure;

   return hop_send_request( svc, sync,
			    success, failure,
			    true, hop_serialize_request_env() );
}

/*---------------------------------------------------------------------*/
/*    hop_event_hander_set ...                                         */
/*---------------------------------------------------------------------*/
function hop_event_handler_set( svc, evt, success, failure ) {
   var req = hop_make_xml_http_request();
   
   var handler = function ( html, http ) {
      http.eventName = evt;
      if( (http.status == 200) && hop_is_http_json( http ) ) {
	 http.eventValue = eval( http.responseText );
      }
      var res = success( http );

      if( res ) {
	 hop_event_handler_set( svc, evt, success, failure );
      }
			
      return res;
   }

   return hop_send_request( svc( evt ), false,
			    handler, failure,
			    false, [] );
}

/*---------------------------------------------------------------------*/
/*    with_hop_callcc ...                                              */
/*---------------------------------------------------------------------*/
/* function with_hop_callcc( service ) {                               */
/*    var sc_storage = sc_CALLCC_STORAGE;                              */
/*    if (sc_storage.doRestore) {                                      */
/*       var res = sc_callcc();                                        */
/*       if (res.failure)                                              */
/* 	 throw res.value; // TODO                                      */
/*       else                                                          */
/* 	 return res.value;                                             */
/*    } else {                                                         */
/*       sc_callcc(function(k) {                                       */
/* 	 function success(val) {                                       */
/* 	    k({value: val});                                           */
/* 	 };                                                            */
/* 	 function failure(val) {                                       */
/* 	    k({failure: true, value: val});                            */
/* 	 };                                                            */
/* 	 hop( service,                                                 */
/* 	      function( http ) {                                       */
/* 		 switch( http.status ) {                               */
/* 		 case 200:                                             */
/* 		    if( hop_is_http_json( http ) ) {                   */
/* 		       success( eval( http.responseText ) );           */
/* 		    } else {                                           */
/* 		       success( http.responseText );                   */
/* 		    }                                                  */
/* 		    return;                                            */
/* 		 case 202:                                             */
/* 		    success( hop_unserialize( http.responseText ) );   */
/* 		    return;                                            */
/* 		 default:                                              */
/* 		    success( http );                                   */
/* 		    return;                                            */
/* 		 }                                                     */
/* 	      },                                                       */
/* 	      failure );                                               */
/* 	 sc_EMPTY_CALLCC(); // abort execution here.                   */
/*       });                                                           */
/*    }                                                                */
/*    return undefined; // for FF2.0                                   */
/* }                                                                   */

/*---------------------------------------------------------------------*/
/*    hop_innerHTML_set ...                                            */
/*---------------------------------------------------------------------*/
function hop_innerHTML_set( nid, html ) {
   var el;
   
   if( (nid instanceof String) || (typeof nid == "string") ) {
      el = document.getElementById( nid );

      if( el == undefined ) {
	 alert( "*** ERROR:innerHTML-set! -- cannot find element \""
		+ nid + "\"");
	 return;
      }
   } else {
      if( !nid ) {
	 alert( "*** ERROR:innerHTML-set! -- illegal element \"" + nid + "\"");
	 return;
      }
      el = nid;
   }
   
   el.innerHTML = html;
   if( (html instanceof String) || (typeof html == "string") ) {
      hop_node_eval( el, html );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_outerHTML_set ...                                            */
/*---------------------------------------------------------------------*/
function hop_outerHTML_set( nid, html ) {
   var p;

   if( (nid instanceof String) || (typeof nid == "string") ) {
      var el;
      el = document.getElementById( nid );
      
      if( el == undefined ) {
	 alert("*** ERROR:outerHTML-set! -- cannot find element \""
	       + nid + "\"");
	 return;
      }
      p = el.parentNode;
   } else {
      if( !nid ) {
	 alert( "*** ERROR:outerHTML-set! -- illegal element \"" + nid + "\"");
	 return;
      }
      p = nid.parentNode;
   }
   
   p.innerHTML = html;
   if( (html instanceof String) || (typeof html == "string") ) {
      hop_node_eval( p, html );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_replace_inner ...                                            */
/*---------------------------------------------------------------------*/
function hop_replace_inner( el ) {
   if( el != undefined ) {
      return function( html ) {
	 if( html ) {
	    hop_innerHTML_set( el, html );
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
	 if( (typeof hop_request_env[ p ] != "function") &&
	     (hop_request_env[ p ] != undefined) ) {
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
   if( window[ id ] ) {
      clearInterval( window[ id ] );
      window[ id ] = false;
   }
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
	       clearInterval( it );
	    } else {
	       if( timeout > 0 ) {
		  timeout -= hop_load_frequency;
		  if( timeout <= 0 ) {
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
   
   if( item === undefined )
      return ";";
   
   if( item === null )
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
	 var c = ((word >> (s << 3)) & 0xff);

	 if( c <= 127 ) {
	    rw += String.fromCharCode( c );
	 } else {
	    var i1 = (c >> 4);
	    var i2 = (c & 0xf);
	    var c1 = i1 + ((i1 < 10) ? 48 : 55);
	    var c2 = i2 + ((i2 < 10) ? 48 : 55);
	    
	    rw += String.fromCharCode( 37, c1, c2 );
	 }
	 
	 s--;
      }

      return rw;
   }
}

/*---------------------------------------------------------------------*/
/*    ucs2_to_utf8 ...                                                 */
/*---------------------------------------------------------------------*/
function ucs2_to_utf8( s ) {
   var len = s.length;

   for( var i = 0; i < len; i++ ) {
      var c = s.charCodeAt( i );
      if( c >= 128 ) {
	 /* we got one non-ascii, we have to convert */
	 var utf = s.substring( 0, i );

	 for( ; i< len; i++, c = s.charCodeAt( i ) ) {
	    if( c < 128 ) {
	       utf += String.fromCharCode( c );
	    } else {
	       if( (c > 127) && (c < 2048) ) {
		  utf += String.fromCharCode((c >> 6) | 192);
		  utf += String.fromCharCode((c & 63) | 128);
	       } else {
		  utf += String.fromCharCode((c >> 12) | 224);
		  utf += String.fromCharCode(((c >> 6) & 63) | 128);
		  utf += String.fromCharCode((c & 63) | 128);
	       }
	    }
	 }

	 return utf;
      }
   }

   return s;
}

/*---------------------------------------------------------------------*/
/*    utf_length ...                                                   */
/*---------------------------------------------------------------------*/
function utf_length( s ) {
   var len = s.length;
   var res = len;

   for( var i = 0; i < len; i++ ) {
      var c = s.charCodeAt( i );
      
      if( c >= 128 ) {
	 if( (c > 127) && (c < 2048) ) {
	    res++;
	 } else {
	    res += 2;
	 }
      }
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_string ...                                         */
/*---------------------------------------------------------------------*/
function hop_serialize_string( mark, item ) {
   return mark + hop_serialize_word( utf_length( item ) ) + item;
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_number ...                                         */
/*---------------------------------------------------------------------*/
function hop_serialize_number( item ) {
   var sitem = item + "";

   if( sitem.indexOf( "." ) == -1 ) {
      if( item < 0 )
	 return '-' + hop_serialize_word( -item );
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
var hop_hash_history_check_regexp = new RegExp( "^#(?:[^=]+=[^:]+:[^,]+,?)+$" );

/*---------------------------------------------------------------------*/
/*    hop_hash_historyp ...                                            */
/*    -------------------------------------------------------------    */
/*    Is a hash value a legal Hop history?                             */
/*---------------------------------------------------------------------*/
function hop_hash_historyp( hash ) {
   return hop_hash_history_check_regexp.exec( hash );
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
   var fun = function() {
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
   hop_eval_history_interval = setInterval( fun, 200 );
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
/*    hop_current_history ...                                          */
/*---------------------------------------------------------------------*/
function hop_current_history() {
   var hash = location.hash;
   
   if( hash.length == 0 ) {
      return false;
   }

   if( hop_hash_historyp( hash ) ) {
      return hop_location_to_state_history( hash );
   }

   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_replay_history ...                                           */
/*---------------------------------------------------------------------*/
function hop_replay_history( hist ) {
   hop_current_state_history = undefined;
   var loc = function( v ) { this.hash = v; }
   hop_eval_history_state( new loc( hop_state_history_to_location( hist ) ) );
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
