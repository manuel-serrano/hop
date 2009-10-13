/*=====================================================================*/
/*    serrano/prgm/project/hop/2.0.x/share/hop-lib.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 20 08:04:30 2007                          */
/*    Last change :  Mon Oct 12 08:07:12 2009 (serrano)                */
/*    Copyright   :  2007-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Various HOP library functions.                                   */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Standard JS library                                              */
/*---------------------------------------------------------------------*/
/*** META ((export Math) (JS Math)) */
/*** META ((export eval) (JS eval)) */

/*---------------------------------------------------------------------*/
/*    hop_callback ...                                                 */
/*---------------------------------------------------------------------*/
function hop_callback( proc ) {
   if( hop_debug() > 0 ) {
      if( !(typeof proc === "function" ) ) {
	 var hstack = hop_get_stack( 1 );
	 e = new Error( "handler not a procedure: " + proc );
	 
	 e.hopStack = hstack;
	 hop_report_exception( e );

	 return function( e ) {
	    throw( e );
	 }
      } else {
	 var hstack = hop_get_stack( 2 );
	 return function( e ) {
	    try {
	       return proc.apply( this, arguments );
	    } catch( e ) {
	       if( e.hopStack )
		  e.hopStack = sc_append( e.hopStack, hstack );
	       else
		  e.hopStack = hstack;

	       hop_report_exception( e );
	       
	       if( hop_debug() >= 2 ) throw( e );
	    }
	 }
      }
   } else {
      return proc;
   }
}   
   
/*---------------------------------------------------------------------*/
/*    hop_trace ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export trace) (arity -1)) */
function hop_trace() {
   if( hop_debug() > 0 ) {
      var svc = hop_apply_url( hop_service_base() + "/trace",
			       arguments );
      hop_send_request( svc, true, function() {}, function() {}, false, [] );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tprint ...                                                   */
/*---------------------------------------------------------------------*/
function hop_tprint( file, pos, rest ) {
   var svc = hop_apply_url( hop_service_base() + "/trace/tprint",
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
	   `((@ hop_tprint _) ,(relative-file-name name (pwd))
			      ,(file-position->line pos name)
			      (list ,@rest)))
	  (else
	   `((@ hop_tprint _) #f #f (list ,@rest))))
       `((@ hop_tprint _) #f #f (list ,@rest))))
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
      sc_error( "hop_replace_inner", "Can't find element", el );
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
/*** META ((export cookie-remove!) (arity #t)) */
function hop_cookie_remove( name, path, domain ) {
   if( hop_cookie_get_value( name ) ) {
      hop_cookie_set_value( name, "", path, domain );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_cookie_get_value ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export cookie-get) (arity #t)) */
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
/*** META ((export cookie-set!) (arity -3)) */
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
/*    hop_load_frequency ...                                           */
/*---------------------------------------------------------------------*/
var hop_load_frequency = 100;

/*---------------------------------------------------------------------*/
/*    hop_load ...                                                     */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity -2)) */
function hop_load( src, timeout ) {
   var script = document.createElement( "script" );
   script.src = src;
   var loaded = false;
   var holder = document.getElementsByTagName( "head" );

   if( !timeout || (timeout == undefined) ) timeout = -1;

   if( holder != null ) {
      
      if( timeout != 0 ) script.onload = function( e ) { loaded = true; }
      
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
		     sc_error( "hop_load", "Cannot load file", src );
		  }
	       }
	    }
	 };
	 it = setInterval( p, hop_load_frequency );
      }
   } else {
      sc_error( "hop_load", "Can't find HEAD element", src );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_onload_add ...                                        */
/*---------------------------------------------------------------------*/
/*** META ((export add-window-onload!) (arity 1)) */
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
/*** META ((export add-window-onload-first!) (arity #t)) */
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
/*** META ((export add-window-onunload!) (arity #t)) */
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
/*** META ((export find-runtime-type) (arity #t)) */
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
/*** META ((export #t) (arity #t)) */
function after( timeout, proc ) {
   var tm = sc_isNumber( timeout ) ? timeout : 1;
   var wproc = hop_callback( proc );
   
   var i = setInterval( function() { clearInterval( i ); wproc() }, tm );
   return true;
}

/*---------------------------------------------------------------------*/
/*    timeout ...                                                      */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function timeout( tm, proc ) {
   var wproc = hop_callback( proc );

   if( wproc() ) {
      var i = setInterval(
	 function() { if( !wproc() ) clearInterval( i )}, tm );
   }
}

/*---------------------------------------------------------------------*/
/*    url-decode ...                                                   */
/*---------------------------------------------------------------------*/
/*** META ((export url-decode) (arity #t)) */
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
/*** META ((export url-encode) (arity #t)) */
function url_encode( s ) {
   return encodeURI( s );
}

/*---------------------------------------------------------------------*/
/*    string-hex-extern ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export string-hex-extern) (arity #t)) */
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
/*** META ((export string-hex-intern) (arity #t)) */
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
/*    make_date ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity -1)) */
function make_date() {
   var l = arguments.length, i = 0;
   var year = 1970;
   var month = 1;
   var day = 1;
   var hours = 1;
   var minutes = 1;
   var seconds = 1;

   while( i < l ) {
      var k = arguments[ i++ ];
      
      if( sc_isKeyword( k ) ) {
	 var prop = sc_keyword2jsstring( k );
	 if( prop === "year" ) year = arguments[ i++ ]; 
	 else if( prop === "month" ) month = (arguments[ i++ ] - 1); 
	 else if( prop === "day" ) day = arguments[ i++ ]; 
	 else if( prop === "hours" ) hours = arguments[ i++ ]; 
	 else if( prop === "minutes" ) minutes = arguments[ i++ ]; 
	 else if( prop === "seconds" ) seconds = arguments[ i++ ]; 
	 else i++;
      }
   }

   return new Date( year, month, day, hours, minutes, seconds );
}

/*---------------------------------------------------------------------*/
/*    date_copy ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity -2)) */
function date_copy() {
   var l = arguments.length, i = 1;
   var date = arguments[ 0 ];
   var year = date.getFullYear();
   var month = date.getMonth();
   var day = date.getDate();
   var hours = date.getHours();
   var minutes = date.getMinutes();
   var seconds = date.getSeconds();

   while( i < l ) {
      var k = arguments[ i++ ];
      
      if( sc_isKeyword( k ) ) {
	 var prop = sc_keyword2jsstring( k );
	 if( prop === "year" ) year = arguments[ i++ ]; 
	 else if( prop === "month" ) month = (arguments[ i++ ] - 1);  
	 else if( prop === "day" ) day = arguments[ i++ ]; 
	 else if( prop === "hours" ) hours = arguments[ i++ ]; 
	 else if( prop === "minutes" ) minutes = arguments[ i++ ]; 
	 else if( prop === "seconds" ) seconds = arguments[ i++ ]; 
	 else i++;
      }
   }

   return new Date( year, month, day, hours, minutes, seconds );
}

/*---------------------------------------------------------------------*/
/*    date->seconds ...                                                */
/*---------------------------------------------------------------------*/
/*** META ((export date->seconds) (arity #t)) */
function date_seconds( d ) {
   return d.getTime() / 1000;
}

/*---------------------------------------------------------------------*/
/*    seconds_date ...                                                 */
/*---------------------------------------------------------------------*/
/*** META ((export seconds->date) (arity #t)) */
function seconds_date( d ) {
   return new Date( d * 1000 );
}

/*---------------------------------------------------------------------*/
/*    date_to_rfc2822 ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export date->rfc282-date) (arity #t)) */
function date_to_rfc2822( d ) {
   return d.toTimeString();
}

/*---------------------------------------------------------------------*/
/*    date_from_rfc2822 ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export rfc282-date->date) (arity #t)) */
function date_from_rfc2822( s ) {
   return new Date( s );
}

/*---------------------------------------------------------------------*/
/*    date_from_rfc2822 ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export rfc282-date->date) (arity #t)) */
function date_from_rfc2822( s ) {
   return new Date( s );
}

/*---------------------------------------------------------------------*/
/*    day_seconds ...                                                  */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function day_seconds() {
   return 86400;
}

/*---------------------------------------------------------------------*/
/*    date-year ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function date_year( d ) {
   return d.getFullYear();
}

/*---------------------------------------------------------------------*/
/*    date-month ...                                                   */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function date_month( d ) {
   return d.getMonth();
}

/*---------------------------------------------------------------------*/
/*    date-day ...                                                     */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function date_day( d ) {
   return d.getDay();
}

/*---------------------------------------------------------------------*/
/*    date-yday ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function date_yday( d ) {
   var d0 = new Date( d.getFullYear(), 0, 1,
		      d.getHours(), d.getMinutes(),
		      d.getSeconds(), d.getMilliseconds() );

   return Math.round(((d.getTime() - d0.getTime()) / 1000) / day_seconds()) + 1;
}

/*---------------------------------------------------------------------*/
/*    date-wday ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function date_wday( d ) {
   return d.getDay() + 1;
}

/*---------------------------------------------------------------------*/
/*    date-hour ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function date_hour( d ) {
   return d.getHours();
}

/*---------------------------------------------------------------------*/
/*    date-minute ...                                                  */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function date_minute( d ) {
   return d.getMinutes();
}   

/*---------------------------------------------------------------------*/
/*    date-second ...                                                  */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function date_second( d ) {
   return d.getSeconds();
}   
