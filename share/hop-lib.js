/*=====================================================================*/
/*    serrano/prgm/project/hop/1.10.x/share/hop-lib.js                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 20 08:04:30 2007                          */
/*    Last change :  Tue Nov 25 16:10:59 2008 (serrano)                */
/*    Copyright   :  2007-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Various HOP library functions.                                   */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_client_debug ...                                             */
/*---------------------------------------------------------------------*/
var hop_client_debug = 0;

var hop_client_error = "data:image/png;base64,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";
   
/*---------------------------------------------------------------------*/
/*    hop_error_html ...                                               */
/*---------------------------------------------------------------------*/
function hop_error_html( fun, exc, msg ) {
   var el = document.createElement( 'div' );
   var emsg = false;
   
   msg = msg.replace( /</g, "&lt;" );
   msg = msg.replace( />/g, "&gt;" );
   msg = msg.replace( /&quot;/g, "\"" );
   
   if( exc ) {
      if( "message" in exc ) {
	 emsg = exc.message;
      } else {
	 if( "description" in exc ) {
	    emsg = exc.description;
	 } else {
	    emsg = exc.toString();
	 }
      }
   }

   node_style_set( el, "overflow", "hidden" );
   node_style_set( el, "padding", "0" );
   node_style_set( el, "border", "1px solid #ccc" );
   node_style_set( el, "-moz-border-radius", "0.5em" );
   node_style_set( el, "height", "100%" );
   node_style_set( el, "width", "100%" );
   node_style_set( el, "margin", "0" );

   el.innerHTML =
      ("<table style='width: 100%; font-family: arial;'>"
       + "<colgroup><col width='64px'><col></colgroup>"
       + "<tr><td style='height: 64px; vertical-align: top; padding-top: 20px; padding-left: 10px; padding-right: 10px'><img src='" + hop_client_error + "' alt='Error'></td><td>"
       + "<table style='width: 100%; font-size: 90%'><colgroup><col width='0*'></colgroup>"
       + "  <tr><td colspan='2' style='text-align: left; width: 100%; border-bottom: 1px solid #777; font-size: x-large; font-weight: bold; color: red'>Internal Client Side Error</td></tr>"
       + (emsg ? ("<tr><th style='text-align: left; vertical-align: top'>error:</th><td style='text-align: left'>" + emsg + "</td></tr>") : "")
       + "  <tr><th style='text-align: left; vertical-align: top'>function:</th><td style='text-align: left'><tt>" + fun + "</tt></td></tr>"
       + ((exc && "line" in exc ) ? "<tr><th style='text-align: left; vertical-align: top'>line:</th><td style='text-align: left'><tt>" + exc.line + "</tt></td></tr>" : "")
       + "  <tr><td colspan='2' style='text-align: left; vertical-align: top; border-top: 1px solid #777'>"
       + "   <div style='width: " + Math.round( (hop_current_window_width() * 0.75) - 100) + "px; overflow: auto; padding-top: 1em;'><pre style=' text-align: left; background: #ffe; border: 1px dotted #ccc;'>" + msg + "</pre></div>"
       + "  </td></tr>"
       + "</table>"
       + "</td></tr></table>");

   return el;
}

/*---------------------------------------------------------------------*/
/*    hop_error ...                                                    */
/*---------------------------------------------------------------------*/
function hop_error( fun, exc, msg, svc ) {
   var emsg = exc ? exc.toString() : "???";

   if( exc ) {
      if( "message" in exc ) {
	 emsg = exc.message;
      } else {
	 if( "description" in exc ) {
	    emsg = exc.description;
	 }
      }
      
      if( "line" in exc ) {
	 emsg = emsg + " (line " + exc.line + ")";
      }
   }

   if( typeof svc === "string" || svc instanceof String )
      fun = fun + ", " + svc;
   
   alert( "*** ERROR " + fun + ": " + emsg + " -- " + msg );
   
   throw exc;
}
   
/*---------------------------------------------------------------------*/
/*    hop_debug ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
function hop_debug() {
   return hop_client_debug;
}

/*---------------------------------------------------------------------*/
/*    hop_debug_set ...                                                */
/*---------------------------------------------------------------------*/
/*** META ((export hop-client-debug-set!)) */
function hop_debug_set( v ) {
   if( sc_isNumber( v ) ) hop_client_debug = v;
}

/*---------------------------------------------------------------------*/
/*    hop_trace ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export trace)) */
function hop_trace() {
   if( hop_client_debug > 0 ) {
      var svc = hop_service_url( hop_service_base() + "/trace",
				 [ "args" ],
				 new Array( sc_vector2list( arguments ) ) );
      hop_send_request( svc, true, function() {}, function() {}, false, [] );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tprint ...                                                   */
/*---------------------------------------------------------------------*/
function hop_tprint( file, pos, rest ) {
   var svc = hop_service_url( hop_service_base() + "/trace/tprint",
	   	              [ "file", "pos", "args" ],
			      [ file, pos, rest ] );
   hop_send_request( svc, true, function() {}, function() {}, false, [] );
}

/*---------------------------------------------------------------------*/
/*    tprint ...                                                       */
/*---------------------------------------------------------------------*/
/*** META
(define-macro (tprint . rest)
   (if (epair? rest)
       (match-case (cer rest)
	  ((at ?name ?pos)
	   `(hop_tprint ,(relative-file-name name (pwd))
			,(file-position->line pos name)
			(list ,@rest)))
	  (else
	   `(hop_tprint #f #f (list ,@rest))))
       `(hop_tprint #f #f (list ,@rest))))
*/

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
/*** META ((export cookie-remove!)) */
function hop_cookie_remove( name, path, domain ) {
   if( hop_cookie_get_value( name ) ) {
      hop_cookie_set_value( name, "", path, domain );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_cookie_get_value ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export cookie-get)) */
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
/*** META ((export cookie-set!)) */
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
/*** META ((export #t)) */
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
/*    hop_window_onload_add ...                                        */
/*---------------------------------------------------------------------*/
/*** META ((export add-window-onload!)) */
var hop_window_onload_add = function( proc ) {
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

hop_window_onload_add( function( e ) {
      /* once the window is loaded, onload handlers */
      /* must be invoked eargly                     */
      hop_window_onload_add = function( proc ) { proc( e ); }
   } );

/*---------------------------------------------------------------------*/
/*    hop_window_onload_cons ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export add-window-onload-first!)) */
function hop_window_onload_cons( proc ) {
   var oldonload = window.onload;

   if( typeof oldonload != 'function' ) {
      window.onload = proc;
   } else {
      window.onload = function( e ) {
	 proc( e );
	 oldonload( e );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_onunload_add ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export add-window-onunload!)) */
function hop_window_onunload_add( proc ) {
   if( typeof( window.onunload ) != 'function' ) {
      window.onunload = proc;
   } else {
      var oldonunload = window.onunload;

      window.onunload = function( e ) {
	 oldonunload( e );
	 proc( e );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_update ...                                                   */
/*    -------------------------------------------------------------    */
/*    This function is called when a widget selects a new child        */
/*    (e.g., a notepad or a tabslider). It gives a child the           */
/*    opportunity to update (i.e., to re-compute dimensions).          */
/*    Widgets interested have to register by setting their             */
/*    hop_update field (see hop-tabslider.js for an example).          */
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
/*** META ((export find-runtime-type)) */
function hop_find_runtime_type( obj ) {
   if( obj instanceof Object ) {
      if( obj instanceof Date ) {
	 return "date";
      } else {
	 if( obj instanceof RegExp ) {
	    return "regexp";
	 } else {
	    if( typeof obj.hop_find_runtime_type == "function" ) 
	       return obj.hop_find_runtime_type();
	    else
	       return "object";
	 }
      }
   } else {
      var tname = typeof obj;

      if( tname == "string" ) {
	 if( sc_isSymbol( obj ) )
	    return "symbol";
	 if( sc_isKeyword( obj ) )
	    return "keyword";

	 return tname;
      }
      return tname;
   }
}

/*---------------------------------------------------------------------*/
/*    after ...                                                        */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
function after( timeout, proc ) {
   var tm = sc_isNumber( timeout ) ? timeout : 1;
   var i = setInterval( function() { clearInterval( i ); proc() }, tm );
   return true;
}

/*---------------------------------------------------------------------*/
/*    timeout ...                                                      */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
function timeout( tm, proc ) {
   var i = setInterval( function() { if( !proc() ) clearInterval( i )}, tm );
}

/*---------------------------------------------------------------------*/
/*    url-decode ...                                                   */
/*---------------------------------------------------------------------*/
/*** META ((export url-decode)) */
function url_decode( s ) {
   try {
      return decodeURI( s );
   } catch( e ) {
      return s;
   }
}

/*---------------------------------------------------------------------*/
/*    url-encode ...                                                   */
/*---------------------------------------------------------------------*/
/*** META ((export url-encode)) */
function url_encode( s ) {
   return encodeURI( s );
}

/*---------------------------------------------------------------------*/
/*    string-hex-extern ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export string-hex-extern)) */
function string_hex_extern( str ) {
  var res = "";
  var l = str.length;

  for( var i = 0; i < l; i++ ) {
    res += "0123456789abcdef".charAt( (str.charCodeAt( i ) >> 4) & 15 )
         + "0123456789abcdef".charAt( (str.charCodeAt( i ) & 15) );
  }
  
  return res;
}

/*---------------------------------------------------------------------*/
/*    string-hex-intern ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export string-hex-intern)) */
function string_hex_intern( s ) {
   var res = "";
   var l = s.length;
   var z = '0'.charCodeAt( 0 );
   var n = '9'.charCodeAt( 0 );
   var a = 'a'.charCodeAt( 0 );
   var f = 'f'.charCodeAt( 0 );
   var A = 'A'.charCodeAt( 0 );
   
   function hex_to_num( c ) {
      if( (c >= z) && ( c<= n) ) {
	 return c - z;
      }
      if( (c >= a) && ( c<= f) ) {
	 return (c - a) + 10;;
      }
      return (c - A) + 10;
   }
      
   for( var i = 0; i < l; i += 2 ) {
      var d1 = hex_to_num( s.charCodeAt( i ) );
      var d2 = hex_to_num( s.charCodeAt( i + 1 ) );

      res += String.fromCharCode( (d1 << 4) + d2 );
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    date-year ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
function date_year( d ) {
   return d.getYear();
}

/*---------------------------------------------------------------------*/
/*    date-month ...                                                   */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
function date_month( d ) {
   return d.getMonth();
}

/*---------------------------------------------------------------------*/
/*    date-day ...                                                     */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
function date_day( d ) {
   return d.getDay();
}

/*---------------------------------------------------------------------*/
/*    date-hour ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
function date_hour( d ) {
   return d.getHours();
}

/*---------------------------------------------------------------------*/
/*    date-minute ...                                                  */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
function date_minute( d ) {
   return d.getMinutes();
}   

/*---------------------------------------------------------------------*/
/*    date-second ...                                                  */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
function date_second( d ) {
   return d.getSeconds();
}   
