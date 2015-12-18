/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/share/hop-lib.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 20 08:04:30 2007                          */
/*    Last change :  Thu Dec 17 16:41:32 2015 (serrano)                */
/*    Copyright   :  2007-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Various HOP library functions.                                   */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Standard JS library                                              */
/*---------------------------------------------------------------------*/
/*** META ((export Math) (JS Math)) */
/*** META ((export navigator) (JS navigator)) */
/*** META ((export eval) (JS eval)) */
/*** META ((export confirm) (JS confirm)) */
/*** META ((export setInterval) (JS setInterval)) */
/*** META ((export clearInterval) (JS clearInterval)) */
/*** META ((export arguments) (JS arguments)) */
/*** META ((export hop-session) (JS hop_session)) */
/*** META ((export hop-realm) (JS hop_realm)) */
/*** META ((export md5sum) (JS md5sum)) */
/*** META ((export md5sum-string) (JS hdex_md5)) */
/*** META ((export hop-debug) (JS hop_debug)) */
/*** META ((export hop-update) (JS hop_update)) */
/*** META ((export server) (JS hop_server)) */

/*---------------------------------------------------------------------*/
/*    hop_callback ...                                                 */
/*    -------------------------------------------------------------    */
/*    See HOP-CALLBACK-HANDLER, hop-exception.scm.                     */
/*    See also XML-TILDE->STATEMENT, runtime/xml.scm                   */
/*---------------------------------------------------------------------*/
function hop_callback( proc, ctx, id ) {
   if( !("apply" in proc) ) {
      sc_typeError( id, "procedure", proc, 2 );
   }
   
   var applyCallback = function() {
      var old = hop_current_stack_context;

      try {
	 hop_current_stack_context = ctx;
	 return proc.apply( this, arguments );
      } catch( e ) {
	 hop_callback_handler( e, ctx );
      } finally {
	 hop_current_stack_context = old;
      }
   }

   if( "displayName" in proc ) {
      applyCallback.displayName = proc.displayName;
   }

   return applyCallback;
}
   
/*---------------------------------------------------------------------*/
/*    hop_trace ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export trace) (arity -1)) */
function hop_trace() {
   if( hop_debug() > 0 ) {
      var svc = hop_apply_url( hop_service_base() + "/trace", arguments );
      hop_send_request( svc, true, function() {}, function() {}, false, [] );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tprint ...                                                   */
/*---------------------------------------------------------------------*/
function hop_tprint( file, pos, args ) {
   // client console tprint
   if( hop_config.tprint_mode === "both"
       || hop_config.tprint_mode === "client" ) {
      if( __sc_traceHasConsole ) {
	 if( file || pos ) {
	    console.log( file + ", " + pos + ": " );
	 }
	 console.log.apply( console, sc_list2vector( args ) );
      } else {
	 var str = file + ", " + pos + ": ";
	 
	 while( sc_isPair( args ) ) {
	    str += args.__hop_car;
	    args = args.__hop_cdr;
	 }
	 
	 alert( str );
      }
   }

   // tprint on a server console
   if( hop_config.tprint_mode === "both" 
       || hop_config.tprint_mode === "server" ) {
      var svc = hop_apply_url( hop_service_base() + "/public/server-debug/tprint",
			       arguments );
      hop_send_request( svc, true, function() {}, function() {}, false, [] );
   }
}

/*---------------------------------------------------------------------*/
/*    tprint ...                                                       */
/*    -------------------------------------------------------------    */
/*    The variant used in JavaScript code (for debugging Hop).         */
/*---------------------------------------------------------------------*/
function tprint( file, rest ) {
   var lst = null;
   
   for( i = arguments.length - 1; i >= 1; i-- ) 
      lst = new sc_Pair( arguments[ i ], lst );
   
   hop_tprint( file, 0, lst );
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
/*    hop_for ...                                                      */
/*---------------------------------------------------------------------*/
/*** META ((export js-for) (arity #t)) */
function hop_for( proc, obj ) {
   for( var p in obj ) {
      proc( p, obj[ p ] );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_in ...                                                       */
/*---------------------------------------------------------------------*/
/*** META ((export js-in?) (arity #t)
           (peephole (infix 2 2 " in "))
           (type bool))
*/
function hop_in( field, obj ) {
   return field in obj;
}

/*---------------------------------------------------------------------*/
/*    hop_instanceof ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export js-instanceof? isa?) (arity #t)
           (peephole (infix 2 2 " instanceof "))
           (type bool))
*/
function hop_instanceof( obj, klass ) {
   return obj instanceof klass;
}

/*---------------------------------------------------------------------*/
/*    hop_properties_to_list ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export js-properties->list) (arity #t)) */
function hop_properties_to_list( obj ) {
   var res = null;

   for( var p in obj ) {
      res = sc_cons( sc_cons( p, obj[ p ] ), res );
   }
   
   return sc_reverseBang( res );
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
      var date = new Date();
      date.setTime( date.getTime() + (-1/*days*/*24*60*60*1000) );
      hop_cookie_set_value( name, "", path, domain, date.toGMTString() );
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
   /* backward compatibility */
   return hop_add_event_listener( window, "load", proc );
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
   if( hop_is_html_element( node ) ) {
      if( "hop_update" in node ) {
	 node.hop_update();
      }

      /* traverse the children */
      for( var c in node.childNodes ) {
	 hop_update( c );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_typeof ...                                                   */
/*    -------------------------------------------------------------    */
/*    A wrapper for using typeof as a function in Hop.                 */
/*---------------------------------------------------------------------*/
/*** META ((export typeof) (arity #t)) */
function hop_typeof( obj ) {
   if( obj instanceof Object ) {
      if( obj instanceof Date ) {
	 return "date";
      } else {
	 if( obj instanceof RegExp ) {
	    return "regexp";
	 } else {
	    if( obj instanceof Function ) {
	       return "function";
	    } else {
	       if( typeof obj.hop_typeof == "function" ) 
		  return obj.hop_typeof();
	       else
		  return "object";
	    }
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
/*    BgL_setTimeoutz00 ...                                            */
/*---------------------------------------------------------------------*/
function BgL_setTimeoutz00( proc, timeout ) {
#if HOP_RTS_DEBUG
   var mark = "setTimeout trace:";

   if( hop_debug() > 0 ) {
      if( !sc_isNumber( timeout ) ) {
	 sc_typeError( "setTimeout", "integer", timeout, 2 );
      }

      if( typeof( proc ) == "string" || proc instanceof String ) {
	 proc = eval( "function() { " + proc + "}" );
      } else if( !proc || !("apply" in proc) ) {
	 sc_typeError( "setTimeout", "procedure", proc, 2 );
      }
   
      try {
	 /* raise an error to get the execution stack */
	 throw new Error( "setTimeout" );
      } catch( e ) {
	 var ctx;
	 var estk = hop_get_exception_stack( e );

	 if( !(sc_isPair( hop_current_stack_context )) ||
	     hop_current_stack_context.__hop_car !== mark ) {
	    ctx = sc_cons(
	       mark,
	       sc_append( estk, hop_current_stack_context ) );
	 } else {
	    ctx = hop_current_stack_context;
	    ctx.__hop_cdr.__hop_car = estk;
	 }

	 proc = hop_callback( sc_arity_check( proc, arguments.length - 2 ), ctx,
			      "setTimeout( ..., " + timeout + ")" );
	 arguments[ 0 ] = proc;
      }
   }
#endif

   return setTimeout.apply( this, arguments );
}

/*---------------------------------------------------------------------*/
/*    BgL_setIntervalz00 ...                                           */
/*---------------------------------------------------------------------*/
function BgL_setIntervalz00( proc, timeout ) {
#if HOP_RTS_DEBUG
   var mark = "setInterval trace:";

   if( hop_debug() > 0 ) {
      if( !sc_isNumber( timeout ) ) {
	 sc_typeError( "setInterval", "integer", timeout, 2 );
      }

      if( typeof( proc ) == "string" || proc instanceof String ) {
	 proc = eval( "function() { " + proc + "}" );
      } else if( !proc || !("apply" in proc) ) {
	 sc_typeError( "setInterval", "procedure", proc, 2 );
      }
   
      try {
	 /* raise an error to get the execution stack */
	 throw new Error( "setInterval" );
      } catch( e ) {
	 var ctx;
	 var estk = hop_get_exception_stack( e );

	 if( !(sc_isPair( hop_current_stack_context )) ||
	     hop_current_stack_context.__hop_car !== mark ) {
	    ctx = sc_cons(
	       mark,
	       sc_append( estk, hop_current_stack_context ) );
	 } else {
	    ctx = hop_current_stack_context;
	    ctx.__hop_cdr.__hop_car = estk;
	 }

	 proc = hop_callback( sc_arity_check( proc, arguments.length - 2 ), ctx,
			      "setInterval( ..., " + timeout + ")" );
	 arguments[ 0 ] = proc;
      }
   }
#endif

   return setInterval.apply( this, arguments );
}

/*---------------------------------------------------------------------*/
/*    sc_after ...                                                     */
/*---------------------------------------------------------------------*/
/*** META ((export after) (arity #t)) */
function sc_after( timeout, proc ) {
#if HOP_RTS_DEBUG
   var mark = "After trace:";
   
   if( hop_debug() > 0 ) {
      if( !sc_isNumber( timeout ) ) {
	 sc_typeError( "after", "integer", timeout, 2 );
      }
   
      if( !proc || !("apply" in proc) ) {
	 sc_typeError( "after", "procedure", proc, 2 );
      }
   
      try {
	 /* raise an error to get the execution stack */
	 throw new Error( "after" );
      } catch( e ) {
	 var ctx;
	 var estk = hop_get_exception_stack( e );

	 if( !(sc_isPair( hop_current_stack_context )) ||
	     hop_current_stack_context.__hop_car !== mark ) {
	    ctx = sc_cons(
	       mark,
	       sc_append( estk, hop_current_stack_context ) );
	 } else {
	    ctx = hop_current_stack_context;
	    ctx.__hop_cdr.__hop_car = estk;
	 }

	 proc = hop_callback( sc_arity_check( proc, 0 ), ctx, "after" );
      }
   }
#endif
   
   var i = setInterval( function() { clearInterval( i ); proc() }, timeout );
   
   return true;
}

/*---------------------------------------------------------------------*/
/*    timeout ...                                                      */
/*---------------------------------------------------------------------*/
/*** META ((export timeout) (arity #t)) */
function sc_timeout( tm, proc ) {
#if HOP_RTS_DEBUG
   if( hop_debug() > 0 ) {
      if( !sc_isNumber( tm ) ) {
	 sc_typeError( "timeout", "integer", tm, 2 );
      }
   
      if( !proc || !("apply" in proc) ) {
	 sc_typeError( "timeout", "procedure", proc, 2 );
      }
   
      try {
	 /* raise an error to get the execution stack */
	 throw new Error( "timeout" );
      } catch( e ) {
	 var stk = sc_append( hop_get_exception_stack( e ),
			      hop_current_stack_context );
	 var ctx = sc_cons( "Timeout trace:", stk );

	 proc = hop_callback( sc_arity_check( proc, 0 ), ctx, "timeout" );
      }
   }
#endif
   
   if( proc() ) {
      var i = setInterval(
	 function() { if( !proc() ) clearInterval( i )}, tm );
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
/*** META ((export url-encode) (arity #t))
           (peephole (hole 1 "encodeURI(" s ")"))) */
function url_encode( s ) {
   return encodeURI( s );
}

/*---------------------------------------------------------------------*/
/*    url-path-encode ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export url-path-encode) (arity #t)
           (peephole (hole 1 "encodeURIComponent(" s ")"))) */
function url_path_encode( s ) {
   return encodeURIComponent( s );
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
   return d.getMonth() + 1;
}

/*---------------------------------------------------------------------*/
/*    date-day ...                                                     */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function date_day( d ) {
   return d.getDate();
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

/*---------------------------------------------------------------------*/
/*    hop_day_names ...                                                */
/*---------------------------------------------------------------------*/
var hop_day_names =
   [ "Sunday", "Monday",  "Tuesday", "Wednesday", "Thursday", "Friday",  "Saturday" ];

/*---------------------------------------------------------------------*/
/*    day-name ...                                                     */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function day_name( day ) {
   if( day < 1 ) 
      sc_error( "date-name", "Illegal day number", day );
   else if( day > 7 )
      return sc_jsstring2string( hop_day_names[ day % 7 ] );
   else
      return sc_jsstring2string( hop_day_names[ day - 1 ] );
}

/*---------------------------------------------------------------------*/
/*    day-aname ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function day_aname( day ) {
   if( day < 1 ) 
      sc_error( "date-name", "Illegal day number", day );
   else if( day > 7 )
      return sc_jsstring2string( hop_day_names[ day % 7 ].substring( 0, 2 ) );
   else
      return sc_jsstring2string( hop_day_names[ day - 1 ].substring( 0, 2 ) );
}

/*---------------------------------------------------------------------*/
/*    hop_month_names ...                                              */
/*---------------------------------------------------------------------*/
var hop_month_names =
   [ "January", "February", "March", "April", "May", "June",
     "July", "August", "September", "October", "November", "December" ];

/*---------------------------------------------------------------------*/
/*    month-name ...                                                   */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function month_name( month ) {
   if( month < 1 ) 
      sc_error( "date-name", "Illegal month number", month );
   else if( month > 12 )
      return sc_jsstring2string( hop_month_names[ month % 12 ] );
   else
      return sc_jsstring2string( hop_month_names[ month - 1 ] );
}

/*---------------------------------------------------------------------*/
/*    month-aname ...                                                  */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function month_aname( month ) {
   if( month < 1 ) 
      sc_error( "date-name", "Illegal month number", month );
   else if( month > 12 )
      return sc_jsstring2string( hop_month_names[ month % 12 ].substring( 0, 2 ) );
   else
      return sc_jsstring2string( hop_month_names[ month - 1 ].substring( 0, 2 ) );
}

/*---------------------------------------------------------------------*/
/*    hop_alist2jsobject ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export alist->jsobject) (arity #t)) */
function hop_alist2jsobject( alist ) {
   var o = {};

   while( sc_isPair( alist ) ) {
      if( !sc_isPair( alist.__hop_car ) || !sc_isPair( alist.__hop_car.__hop_cdr ) )
	 sc_error( "alist->object", "Illegal entry", alist.__hop_car );
      if( !sc_isKeyword( alist.__hop_car.__hop_car ) )
	 sc_error( "alist->object", "Illegal key", alist.__hop_car.__hop_car );
      
      o[ sc_keyword2jsstring( alist.__hop_car.__hop_car ) ] = alist.__hop_car.__hop_cdr.__hop_car;
      alist = alist.__hop_cdr;
   }

   return o;
}

/*---------------------------------------------------------------------*/
/*    hop_jsobject2alist ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export jsobject->alist) (arity #t)) */
function hop_jsobject2alist( obj ) {
   var l = null;

   for( p in obj ) {
      var c = sc_cons( sc_jsstring2keyword( p ), sc_cons( obj[ p ], null ) );
      l = sc_cons( c, l );
   }

   return l;
}

/*---------------------------------------------------------------------*/
/*    hop_plist2object ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export plist->jsobject) (arity #t)) */
function hop_plist2jsobject( plist ) {
   var o = {};

   while( sc_isPair( plist ) ) {
      if( !sc_isKeyword( plist.__hop_car ) )
	 sc_error( "plist->object", "Illegal key", plist.__hop_car.__hop_car );
      if( !sc_isPair( plist.__hop_cdr ) ) 
	 sc_error( "plist->object", "Illegal entry", plist );
      
      o[ sc_keyword2jsstring( plist.__hop_car ) ] = plist.__hop_cdr.__hop_car;
      plist = plist.__hop_cdr.__hop_cdr;
   }

   return o;
}

/*---------------------------------------------------------------------*/
/*    hop_jsobject2plist ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export jsobject->plist) (arity #t)) */
function hop_jsobject2plist( obj ) {
   var l = null;

   for( p in obj ) {
      l = sc_cons( sc_jsstring2keyword( p ), sc_cons( o[ p ], l ) );
   }

   return l;
}

/*---------------------------------------------------------------------*/
/*    hop_xml_make_id ...                                              */
/*---------------------------------------------------------------------*/
var hop_id_count = 0;

/*** META ((export xml-make-id) (arity #t)) */
function hop_xml_make_id( obj ) {
   return (obj instanceof String ? obj : "id") + hop_id_count++;
}

/*---------------------------------------------------------------------*/
/*    hop_comprehension ...                                            */
/*---------------------------------------------------------------------*/
function hop_comprehension( iterables, fun, test, _names, _astp, _aste, _astd ) {
   function product( iterables ) {
      return iterables.reduce( function(a, b) {
	 var ret = [];
	 a.forEach( function( a ) {
	    b.forEach( function( b ) {
	       ret.push( a.concat( [ b ] ) );
	    } );
	 } );
	 return ret;
      }, [[]]);
   };

   if( iterables.length == 1 ) {
      // fast path
      if( test === true ) {
	 return iterables[ 0 ].map( fun );
      } else {
	 return iterables[ 0 ].filter( test ).map( fun );
      }
   } else {
      // full path
      var prods = product( iterables );
      var self = iterables[ 0 ];
      if( test === true ) {
	 return prods.map( function( a ) { return fun.apply( self, a ) } );
      } else {
	 return prods.filter( function( a ) { return test.apply( self, a ) } )
	    .map( function( a ) { return fun.apply( self, a ) } );
      }
   }
}
