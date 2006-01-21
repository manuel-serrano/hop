/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop.js                            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec 25 06:57:53 2004                          */
/*    Last change :  Sat Jan 21 17:49:29 2006 (serrano)                */
/*    Copyright   :  2004-06 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Standard HOP JavaScript library                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    DOMFormElement ...                                               */
/*---------------------------------------------------------------------*/
var undefined;

if( window.HTMLFormElement == undefined ) {
   window.HTMLFormElement = window.HTMLForm;
}

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
/*    hop_set_cookie ...                                               */
/*---------------------------------------------------------------------*/
function hop_set_cookie( http ) {
   try {
      var cookie = http.getResponseHeader( "set-cookie" );
      document.cookie = cookie;
   } catch( e ) {
      ;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_replace ...                                                  */
/*---------------------------------------------------------------------*/
function hop_replace( http ) {
   if( http.responseText != null ) {
      hop_set_cookie( http );
      document.open();
      document.write( http.responseText );
      document.close();
   }
}

/*---------------------------------------------------------------------*/
/*    hop_replace_id ...                                               */
/*---------------------------------------------------------------------*/
function hop_replace_id( id ) {
   var el = document.getElementById( id );

   if( el != undefined ) {
      return function( http ) {
	 if( http.responseText != null ) {
	    hop_js_eval( http );
	    el.innerHTML = http.responseText;
	 }
      }
   } else {
      alert( "*** Hop Error, Can't find element: `" + id + "'" );
      return function( http ) { };
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
/*    hop ...                                                          */
/*---------------------------------------------------------------------*/
function hop_inner( method, service, success, failure, sync ) {
   var http = new XMLHttpRequest();
   
   http.open( method, service, (sync != true) );

   http.onreadystatechange = function() {
      if( http.readyState == 4 ) {
	 var status;

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
		     alert( "*** Hop Error " + http.status + ": `"
			    + http.responseText + "'" );
		  }
	       }
	 }
      }
   }

   http.setRequestHeader( 'Content-Type', 'application/x-www-form-urlencoded; charset=ISO-8859-1' );
   http.send( null );

   return http;
}

/*---------------------------------------------------------------------*/
/*    resume_XXX ...                                                   */
/*---------------------------------------------------------------------*/
var resume_success = hop_replace;
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
		     false );
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

   if( id instanceof HTMLElement ) {
      el = id;
   } else {
      if( (id instanceof String) || (typeof id == "string") ) {
	 el = document.getElementById( id );
      } else {
	 alert( "*** ERROR:hop_tooltip_show:Illegal id -- " + id );
      }
   }

   if( (el instanceof HTMLDivElement) &&
       (hop_current_tooltip != el) ) {

      if( hop_current_tooltip instanceof HTMLDivElement ) {
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
   if( hop_current_tooltip instanceof HTMLDivElement ) {
      hop_current_tooltip.style.visibility = "hidden";
      hop_current_tooltip = null;
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
   var cookie = name + "=" + val + "; ";

   if( path instanceof String ) {
      cookie += "path=" + path + ";";
   } else {
      cookie += "path=/;";
   }
   
   if( domain instanceof String ) {
      cookie += "domain=" + domain + ";";
   }

   if( expires instanceof String ) {
      cookie += "expires=" + expires + ";";
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
function hop_timeout( name, proc, timeout, eager ) {
   window[ name ] = setInterval( proc, timeout );
   window[ name ].proc = proc;
   window[ name ].timeout = timeout;
   
   if( eager == true ) proc();
}

/*---------------------------------------------------------------------*/
/*    hop_timeout_reset ...                                            */
/*---------------------------------------------------------------------*/
function hop_timeout_reset( id, proc, timeout ) {
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

   alert( "*** Hop Error, Can't serialize element: `" + item + "'" );
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
   var n   = 0;
    
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
