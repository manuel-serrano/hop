/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop.js                            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec 25 06:57:53 2004                          */
/*    Last change :  Mon May 22 13:40:20 2006 (serrano)                */
/*    Copyright   :  2004-06 Manuel Serrano                            */
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
      return service;
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
/*    hop_js_eval ...                                                  */
/*---------------------------------------------------------------------*/
function hop_js_eval( http ) {
   var res = undefined;
   
   if( http.responseText != null ) {
      var node = document.createElement( "div" );
      
      node.innerHTML = http.responseText;

      /* evaluate the inner JS scripts */
      var scripts = node.getElementsByTagName( "script" );

      for ( var j = 0; j < scripts.length; j++ ) {
	 if( scripts[ j ].childNodes.length > 0 ) {
	    res = eval( scripts[ j ].childNodes[ 0 ].nodeValue );
	 }
      }
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    hop_default_failure ...                                          */
/*---------------------------------------------------------------------*/
function hop_default_failure( http ) {
   var t = http.responseText;
   var div = document.getElementById( "hop_default_failure" );

   t = t.replace( /<!DOCTYPE[^>]*>/g, "" );
   t = t.replace( /<(head|meta|link)[^>]*>/g, "<div style='display: none'>" );
   t = t.replace( /<\/(head|meta|link)>/g, "</div>" );
   t = t.replace( /<html[^>]*>/g, "<div style='width: 45em; overflow: auto; cursor: pointer;' onclick='document.body.removeChild( document.getElementById( \"hop_default_failure\" ) )' title='Click to hide this message'>" );
   t = t.replace( /<\/html>/g, "</div>" );
   t = t.replace( /<body[^>]*>/g, "<div style='background: transparent; font-family: sans serif; -moz-opacity: 0.87'>" );
   t = t.replace( /<\/body>/g, "</div>" );
   t = t.replace( /&lt;/g, "<" );
   t = t.replace( /&gt;/g, ">" );
   t = t.replace( /&quot;/g, "\"" );

   if( !div ) {
      div = document.createElement( "div" );
      div.id = "hop_default_failure";
      div.style.setProperty( "position", "absolute", "" );
      div.style.setProperty( "top", "100", "" );
      div.style.setProperty( "z-index", "100", "" );
      div.style.setProperty( "width", "100%", "" );
      div.style.setProperty( "padding", "0", "" );
      div.align = "center";
      div.style.setProperty( "background", "transparent", "" );

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
/*    hop ...                                                          */
/*---------------------------------------------------------------------*/
function hop_inner( method, service, success, failure, sync, mute ) {
   var http = hop_make_xml_http_request();
   var vis = false;

   if( !mute ) {
      vis = document.createElement( "div" );
      vis.style.setProperty( "position", "absolute", "" );
      vis.style.setProperty( "top", "5", "" );
      vis.style.setProperty( "right", "5", "" );
      vis.style.setProperty( "z-index", "100", "" );
      vis.style.setProperty( "background", "#eeeeee", "" );
      vis.style.setProperty( "-moz-opacity", "0.7", "" );
      vis.style.setProperty( "border-color", "black", "" );
      vis.style.setProperty( "border-style", "outset", "" );
      vis.style.setProperty( "border-width", "1px", "" );
      vis.style.setProperty( "padding", "2px", "" );
      vis.title = service;

      var img = document.createElement( "img" );
      img.classname = "hop-busy-anim";
      img.src = hop_busy_anim;
      
      vis.appendChild( img );
      document.body.appendChild( vis );
   }

   http.open( method, service, (sync != true) );

   http.onreadystatechange = function() {
      if( http.readyState == 4 ) {
	 var status;

	 if( !mute ) {
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

   http.setRequestHeader( 'Content-Type', 'application/x-www-form-urlencoded; charset=ISO-8859-1' );
/*    http.setTimeouts = 1000;                                         */
   http.send( null );

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
      success = resume_success;
      failure = resume_failure;
   } else {
      resume_success = success;
      resume_failure = failure;
   }
   
   return hop_inner( "HOP", service, success, failure, sync );
}

/*---------------------------------------------------------------------*/
/*    with_hop ...                                                     */
/*---------------------------------------------------------------------*/
function with_hop( service, success, failure ) {
   if( !success ) success = function( h ) { };
   
   return hop( service,
	       function( http ) {
                 var json;

                 switch( http.status ) {
		    case 200:
		       if( http.propertyIsEnumerable( "getResponseHeader" ) ) {
			  json = http.getResponseHeader( "hop-json" );
		       } else {
			  json = (http.getAllResponseHeaders().indexOf( "hop-json" ) >= 0);
		       }

		       if( json ) {
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
/*    hop_event_hander_set ...                                         */
/*---------------------------------------------------------------------*/
function hop_event_handler_set( svc, evt, success, failure ) {
   return hop_inner( "HOPEVT",
		     svc( evt ),
		     function( http ) {
                        http.eventName = evt;
                        var res = success( http );
			if( res ) 
			   hop_event_handler_set( svc, evt, success, failure );
                        return res;
		     },
		     failure,
		     false,
		     true );
}

/*---------------------------------------------------------------------*/
/*    hop_mozillap ...                                                 */
/*---------------------------------------------------------------------*/
function hop_mozillap() {
   return navigator.userAgent.indexOf( "Mozilla" ) >= 0;
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
	 hop_current_tooltip.style.visibility = "hidden";
      }

      el.style.visibility = "visible";
      var x = ux != undefined ? ux : event.pageX - 200;
      var y = uy != undefined ? uy : event.pageY - 200;
      var b = document.getElementsByTagName( "body" )[ 0 ];
      
      el.style.left = (x < 0) ? 0 : x;
      el.style.top = (y < 0) ? 0: y;

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
      hop_current_tooltip.style.visibility = "hidden";
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
function hop_timeout( id, timeout, proc ,eager ) {
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
   
   if( (item instanceof String) || (tname == "string") )
      return hop_serialize_string( '"', item );

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
   
   if( item instanceof HTMLCollection )
      return hop_serialize_array( item );
      
   if( item instanceof Date )
      return hop_serialize_date( item );

   if( item instanceof HTMLInputElement )
      return hop_serialize( item.value );

   if( item instanceof HTMLTextAreaElement )
      return hop_serialize( item.value );

   if( item instanceof HTMLSelectElement )
      return hop_serialize( item.value );

   if( (item instanceof Object) &&
       (typeof item.hop_bigloo_serialize == "function") )
      return item.hop_bigloo_serialize();

   alert( "*** Hop Error, Can't serialize element: `" + item +
	  "' (" + tname + "). Ignoring value." );
   
   return hop_serialize( false );
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
/*    getElementsByClass document method ...                           */
/*---------------------------------------------------------------------*/
document.getElementsByClass = function( className ) {
   var all = document.getElementsByTagName( "*" );
   var res = new Array();
   var n = 0;
    
   for( var i = 0; i < all.length; i++ ) {
      if( all[ i ].className == className ) {
	 res[ n++ ] = all[ i ];
      }
   }
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    hopBehaviour class ...                                           */
/*---------------------------------------------------------------------*/
var hopBehaviour = {
    behaviours: {},
    
    register: function( className, func ) {
	hopBehaviour.behaviours[ className ] = func;
    },

    plug: function() {
	var all = hopBehaviour.behaviours;

	for( var name in all ) {
	    var list = document.getElementsByClass( name );
	    
	    for( var i in list ) {
		all[ name ]( list[ i ] );
	    }
	}
    },

    start: function() {
	var oldonload = window.onload;

	if( typeof window.onload != 'function' ) {
	    window.onload = hopBehaviour.plug;
	} else {
	    window.onload = function() {
		oldonload();
		hopBehaviour.plug();
	    }
	}
    }
};

hopBehaviour.start();

   
