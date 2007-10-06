/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-lib.js                        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 20 08:04:30 2007                          */
/*    Last change :  Wed Oct  3 14:23:11 2007 (serrano)                */
/*    Copyright   :  2007 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Various HOP library functions.                                   */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    trace ...                                                        */
/*---------------------------------------------------------------------*/
function trace() {
   var svc = hop_service_url( "/hop/trace",
			      [ "args" ],
			      new Array( sc_vector2list( arguments ) ) );
   hop_send_request( svc, true, function() {}, function() {}, false, [] );
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
	    if( typeof obj.hop_find_runtime_type == "function" ) 
	       return obj.hop_find_runtime_type();
	    else
	       return "object";
	 }
      }
   } else {
      var tname = typeof obj;

      if( tname == "string" ) {
	 if( sc_isKeyword( obj ) )
	    return "keyword";
	 if( sc_isSymbol_mutable( obj ) || sc_isSymbol_immutable( obj ) )
	    return "symbol";

	 return tname;
      }
      return tname;
   }
}

/*---------------------------------------------------------------------*/
/*    after ...                                                        */
/*---------------------------------------------------------------------*/
function after( timeout, proc ) {
   var tm = sc_isNumber( timeout ) ? timeout : 1;
   var i = setInterval( function() { clearInterval( i ); proc() }, tm );
   return true;
}

/*---------------------------------------------------------------------*/
/*    timeout ...                                                      */
/*---------------------------------------------------------------------*/
function timeout( tm, proc ) {
   var i = setInterval( function() { if( !proc() ) clearInterval( i )}, tm );
}
