/*=====================================================================*/
/*    serrano/prgm/project/hop/2.4.x/share/hop-request.js              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec 25 06:57:53 2004                          */
/*    Last change :  Fri Jun 29 07:55:05 2012 (serrano)                */
/*    Copyright   :  2004-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    WITH-HOP implementation                                          */
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
var hop_busy_vis_16_16 = false;
var hop_busy_vis_32_32 = false;

var hop_busy_anim_16_16 = "data:image/gif;base64,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";

var hop_busy_anim_32_32_simple = "data:image/gif;base64,R0lGODlhIAAgALMMAOxiJlM8GvrYydTOxu1sNPGJXPjErX5tU15IKJ+Sf/Snhb62qf///wAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh+QQJBQAMACwAAAAAIAAgAAAETJDJSau9OOvNu/9gKI6YQQBoqq4AYXAnK6sEN9+pjd/6Lve+FTCY2xBZw2OSuAw2fc9dFDflGY+q6kz728SwtY0J/CKZz+i0es3ORAAAIfkECQUADAAsAAAAACAAIAAABEWQyUmrvTjrzbv/YCiOmgKcaIoqoeqm7fvGskrX6I0DOt7XP1lwBtrBisbT0LW0IZPN4yepfBqjOZDJyCJ5v+CweEzmRAAAIfkECQUADAAsAAAAACAAIAAABEKQyUmrvTjrzbv/YCiOXQGcKFCQU+qykpvCjIzS9onnu93LvxcsBwjOhjykTwlkCllE4815hCZZJtmKxu16v+AwKwIAIfkECQUADAAsAAAAACAAIAAABDyQyUmrvTjrzbv/YCiOIGCSmQmgmMpe7lvF8kTXzF3rMv/6LCBKSCKOjCJkSFk64STMT9QzfVqv2KzWEwEAIfkEBQUADAAsAAAAACAAIAAABDyQyUmrvTjrzbv/YCiOISCQmolm6nq1bgXH00wzNp3Hu9uvP1SQNBwVRcfS6VZbMnFOZhI0fVqv2Kw2EwEAIfkEBQUADAAsDgAGAAMAFAAABAkwyEmrvTjrGgEAIfkEBQUADAAsCwAGAAkAFAAABBLwhEmDrDjrzbv/YCiOZHhhRwQAIfkEBQUADAAsCAAGAA8AFAAABBYwhUkrlTbrzbv/YCiOZGmeaKpeYBIBACH5BAkFAAwALAYABgAUABQAAAQdcKFAq70BSczv7GAojmRpnmiqrmzrvnD7ndpMahEAIfkECQUADAAsAAAAACAAIAAABEWQyUmrvTjrzbv/YCiOWhKcaIomoeqm7fvGskrX6I0HOt7XP1lwBtrBisbT0LW0IZPN4yepfBqjOZDJyCJ5v+CweEzmRAAAIfkECQUADAAsAAAAACAAIAAABEKQyUmrvTjrzbv/YCiO3RGcaHCQU+qykpvCjIzS9onnu93LvxcsFwjOhjykTwlkCllE4815hCZZJtmKxu16v+AwKwIAIfkECQUADAAsAAAAACAAIAAABDyQyUmrvTjrzbv/YCiOYGCSmRmgmMpe7lvF8kTXzF3rMv/6LCBKSCKOjCJkSFk64STMT9QzfVqv2KzWEwEAIfkEBQUADAAsAAAAACAAIAAABDyQyUmrvTjrzbv/YCiOYTCQmolm6nq1bgXH00wzNp3Hu9uvP1SQNBwVRcfS6VZbMnFOZhI0fVqv2Kw2EwEAIfkEBQUADAAsDgAGAAMAFAAABAkQyEmrvTjrGgEAIfkEBQUADAAsCwAGAAkAFAAABBKwgEmBrDjrzbv/YCiOZHhhRQQAIfkEBQUADAAsCAAGAA8AFAAABBZQgUkrlTbrzbv/YCiOZGmeaKpeoBIBACH5BAUFAAwALAYABgAUABQAAAQd0BBAq72ASMzv7GAojmRpnmiqrmzrvnD7ndpMahEAOw==";

var hop_busy_anim_32_32 = "data:image/gif;base64,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";

/*---------------------------------------------------------------------*/
/*    hop_apply_form_url ...                                           */
/*---------------------------------------------------------------------*/
function hop_apply_form_url( service, args ) {
   var nargs = null;
   var els = args[ 0 ].elements;

   for( i = els.length - 1 ; i >=0 ; i-- ) {
      if( els[ i ].type == "checkbox" ) {
	 nargs = sc_cons( els[ i ].checked ? els[ i ].value : false, nargs );
	 nargs = sc_cons( sc_jsstring2keyword( els[ i ].name ), nargs );
      } else {
	 if( els[ i ].type == "radio" ) {
	    if( els[ i ].checked ) {
	       nargs = sc_cons( els[ i ].value, nargs );
	       nargs = sc_cons( sc_jsstring2keyword( els[ i ].name ), nargs );
	    }
	 } else {
	    if( els[ i ].name !== "" ) {
	       nargs = sc_cons( els[ i ].value, nargs );
	       nargs = sc_cons( sc_jsstring2keyword( els[ i ].name ), nargs );
	    }
	 }
      }
   }

   return hop_apply_url( service, nargs );
}

/*---------------------------------------------------------------------*/
/*    hop_apply_url ...                                                */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function hop_apply_url( service, args ) {
   if( sc_isPair( args ) ) {
      return service
	 + "?hop-encoding=hop"
	 + "&vals=" + hop_bigloo_serialize( args );
   } else {
      if( (args.length == 1) && hop_is_dom_form_element( args[ 0 ] ) ) {
	 return hop_apply_form_url( service, args );
      } else {
	 return service
	    + "?hop-encoding=hop"
	    + "&vals=" + hop_bigloo_serialize( sc_vector2list( args ) );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_default_failure ...                                          */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function hop_default_failure( exc, xhr ) {
   if( !document ) {
      alert( "with-hop failed!" );
      return;
   }

   if( exc instanceof Error ) {
      hop_report_exception( exc );
      return;
   }
   
   if( "exception" in xhr ) {
      hop_report_exception( xhr.exception );
      return;
   }

   var nexc = new Error( "with-hop" );
   var t = xhr.responseText;
   nexc.hopStack = xhr.hopStack;

   if( t ) {
      if( t.match( /<!DOCTYPE[^>]*>/) ) {
	 var m = t.match( /<body id='[^']+' hssclass='hop-error'>((?:.|[\n])*)/ );
	 if( m ) {
	    nexc.element = document.createElement( "div" );
	    nexc.element.innerHTML = m[ 1 ].replace( /<\/body>(?:.|[\n])*/g, "" );
	 } else {
	    /* we have received the complete document */
	    t = t.replace( /<!DOCTYPE[^>]*>/g, "" );
	    t = t.replace( /<head[^>]*>/g, "<div style='display: none;'>" );
	    t = t.replace( /<\/head>/g, "</div>" );
	    t = t.replace( /<(meta|link)[^>]*>/g, "<span style='display: none'></span>" );
	    t = t.replace( /<html[^>]*>/g, "<div style='width: 100%; height: 100%; overflow: auto'>" );
	    t = t.replace( /<\/html>/g, "</div>" );
	    t = t.replace( /<body[^>]*>/g, "<div style='width: 100%; height: 100%; overflow: auto'>" );
	    t = t.replace( /<\/body>/g, "</div>" );
	    t = t.replace( /&quot;/g, "\"" );
	    
	    nexc.message = t;
	 }
      } else {
	 nexc.message = t;
      }
   }

   hop_report_exception( nexc );
}

/*---------------------------------------------------------------------*/
/*    hop_anim_16_16 ...                                               */
/*---------------------------------------------------------------------*/
function hop_anim_16_16( title ) {
   if( !hop_busy_vis_16_16 ) {
      var vis = document.createElement( "div" );
      vis.className = "hop-busy-anim";
      vis.count = false;

      var img = document.createElement( "img" );
      img.className = "hop-busy-anim";

      if( !hop_config.inline_image ) {
	 img.src = hop_share_directory() + "/icons/anims/busy-anim-16.gif";
      } else {
	 img.src = hop_busy_anim_16_16;
      }

      vis.appendChild( img );
      
      hop_busy_vis_16_16 = vis;
   }
   return hop_busy_vis_16_16;
}

/*---------------------------------------------------------------------*/
/*    hop_anim_32_32 ...                                               */
/*---------------------------------------------------------------------*/
function hop_anim_32_32( title ) {
   if( !hop_busy_vis_32_32 ) {
      var vis = document.createElement( "div" );
      vis.className = "hop-busy-anim";
      vis.count = false;

      var img = document.createElement( "img" );
      img.className = "hop-busy-anim";

      if( !hop_config.inline_image ) {
	 img.src = hop_share_directory() + "/icons/anims/busy-anim-32.gif";
      } else {
	 img.src = hop_busy_anim_32_32;
      }

      vis.appendChild( img );
      hop_busy_vis_32_32 = vis;
   }

   return hop_busy_vis_32_32;
}

/*---------------------------------------------------------------------*/
/*    hop_default_anim ...                                             */
/*---------------------------------------------------------------------*/
var hop_default_anim = hop_anim_32_32;
var hop_anim_container = false;

/*---------------------------------------------------------------------*/
/*    hop_default_anim_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export with-hop-default-anim-set!) (arity -2)) */
function hop_default_anim_set( anim, container ) {
   var old = hop_default_anim;
   if( typeof( anim ) == "string" ) {
      var img = hop_anim_32_32( "custom" ).firstChild;

      img.src = anim;
   }

   if( container ) {
      hop_anim_container = container;
   }
   
   return old;
}

/*---------------------------------------------------------------------*/
/*    hop_default_anim_get ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export with-hop-default-anim) (arity #t)) */
function hop_default_anim_get() {
   return hop_default_anim;
}

/*---------------------------------------------------------------------*/
/*    hop_anim_vis ...                                                 */
/*---------------------------------------------------------------------*/
var hop_anim_vis = false;
var hop_anim_fun = false;

/*---------------------------------------------------------------------*/
/*    hop_stop_anim ...                                                */
/*---------------------------------------------------------------------*/
function hop_stop_anim( xhr ) {
   if( xhr.hop_anim ) {
      if( xhr.hop_anim_interval ) {
	 clearInterval( xhr.hop_anim_interval );
	 xhr.hop_anim_interval = false;
      }

      if( xhr.hop_anim != true ) {
	 xhr.hop_anim.count--;
	 if( xhr.hop_anim.count == 1 )
	    node_style_set( xhr.hop_anim, "display", "none" );
      }
   }
}
   
/*---------------------------------------------------------------------*/
/*    hop_start_anim ...                                               */
/*---------------------------------------------------------------------*/
function hop_start_anim( service, user_anim ) {
   var anim = user_anim( service );

   if( !anim.count ) {
      if( typeof( hop_anim_container ) === "string" ) {
	 hop_anim_container = document.getElementById( hop_anim_container );
	 }
      if( !hop_anim_container ) {
	 hop_anim_container = document.body;
      }

      hop_anim_container.appendChild( anim );
      anim.count = 2;
   } else {
      anim.count++;
      anim.title = service;
      node_style_set( anim, "display", "block" );
   }

   return anim;
}

/*---------------------------------------------------------------------*/
/*    hop_default_success ...                                          */
/*---------------------------------------------------------------------*/
function hop_default_success( h, xhr ) {
   return h;
}

/*---------------------------------------------------------------------*/
/*    hop_request_unserialize ...                                      */
/*    -------------------------------------------------------------    */
/*    Unserialize the object contained in the XHR response. The        */
/*    unserialization method depends on the mime type of the response. */
/*---------------------------------------------------------------------*/
function hop_request_unserialize( svc ) {
   var xhr = this;
   var ctype = ("content_type" in xhr) ?
      xhr[ "content_type" ] : hop_header_content_type( xhr );

   if( ctype === "application/x-hop" ) {
      return hop_string_to_obj( hop_json_parse( xhr.responseText ) );
   } if( (ctype === "text/html") || (ctype === "application/xhtml+xml") ) {
      return hop_create_element( xhr.responseText );
   } else if( ctype === "application/x-javascript" ) {
      return eval( xhr.responseText );
   } else if( ctype === "application/json" ) {
      return hop_json_parse( xhr.responseText );
   } else {
      return xhr.responseText;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_request_unserialize_arraybuffer ...                          */
/*    -------------------------------------------------------------    */
/*    This is an alternate protocol for exanching values between the   */
/*    server and the client. This is currently not in used because     */
/*    as of Jan 2012 I (MS) have not found an efficient way to         */
/*    unserialize strings. This protocol could replace hop_request_    */
/*    unserialize if this problem get solved.                          */
/*    -------------------------------------------------------------    */
/*    Unserialize the object contained in the XHR response. The        */
/*    unserialization method depends on the mime type of the response. */
/*---------------------------------------------------------------------*/
function hop_request_unserialize_arraybuffer( svc ) {
   var xhr = this;
   var ctype = ("content_type" in xhr) ?
      xhr[ "content_type" ] : hop_header_content_type( xhr );
   /* MS, 11jan2012, In Chrome, got confused with empty responses */
   var a = (xhr.response instanceof ArrayBuffer) ?
      new Uint8Array( xhr.response ) : new Uint8Array();

   if( ctype === "application/x-hop" ) {
      return hop_string_to_obj( a );
   } if( (ctype === "text/html") || (ctype === "application/xhtml+xml") ) {
      return hop_create_element( hop_uint8array_to_string( a ) );
   } else if( ctype === "application/x-javascript" ) {
      return eval( hop_uint8array_to_string( a ) );
   } else if( ctype === "application/json" ) {
      return hop_json_parse( hop_uint8array_to_string( a ) );
   } else {
      return hop_uint8array_to_string( a );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_duplicate_error ...                                          */
/*---------------------------------------------------------------------*/
function hop_duplicate_error( exc ) {
   try {
      var nexc = new exc.constructor( exc.message );

      for( p in exc ) {
	 nexc[ p ] = exc[ p ];
      }

      return nexc;
   } catch( _ ) {
      return exc;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_request_onready ...                                          */
/*---------------------------------------------------------------------*/
function hop_request_onready( xhr, svc, succ, fail, err ) {
   try {
      switch( xhr.status ) {
      case 200:
	 try {
	    return succ( xhr.unserialize( svc ), xhr );
	  } catch( exc ) {
	     // Exception are read-only in Firefox, duplicate then
	     var frame = sc_cons( succ, sc_cons( xhr, null ) );
	     var nexc = err( exc );

	     nexc.hopLocation = exc.hopLocation;
	     nexc.scObject = exc.scObject;
	     nexc.hopStack = sc_cons( frame, xhr.hopStack );
	     nexc.hopService = svc;
	     
	     xhr.exception = nexc;
	     
	     fail( nexc, xhr );
	     return false;
	  }
	 
      case 204:
	 return false;
	 
      case 259:
	 hop_set_cookie( xhr );
	 return false;
	 
      case 400:
	 fail( xhr.unserialize( svc ), xhr );
	 return false;
	 
      case 407:
	 fail( 407, xhr );
	 return false;
	 
      default:
	 if( (typeof xhr.status === "number") &&
	     (xhr.status > 200) && (xhr.status < 300) ) {
	    return succ( xhr.responseText, xhr );
	 } else {
	    var frame = sc_cons( fail, sc_cons( xhr, null ) );
	    
	    xhr.hopStack = sc_cons( frame, xhr.hopStack );
	    fail( xhr.status, xhr );
	    return false;
	 }
      }
   } catch( exc ) {
      var nexc = err( exc );
      
      nexc.hopStack = xhr.hopStack;
      nexc.hopService = svc;
      
      xhr.exception = nexc;
      
      fail( nexc, xhr );
      return false;
   } finally {
      if( typeof hop_stop_anim === "function" ) { 
	 hop_stop_anim( xhr );
      }
   }
}
   
/*---------------------------------------------------------------------*/
/*    hop_send_request ...                                             */
/*    -------------------------------------------------------------    */
/*    In this function SUCCESS and FAILURE are *always* bound to       */
/*    functions.                                                       */
/*---------------------------------------------------------------------*/
function hop_send_request( svc, sync, success, failure, anim, henv, auth, t, x ) {
   var xhr = x ? x : hop_make_xml_http_request();

   /* MS, 20 Jun 08: I cannot understand why but sometime global functions */
   /* are unbound (at least in Firefox) when used inside a catch! Binding  */
   /* it to a local var eliminates this problem.                           */
   var duperror = hop_duplicate_error;
   var succ = (typeof success === "function") ? success : hop_default_success;
   var fail = (typeof failure === "function") ? failure : hop_default_failure;
   
   xhr.hopStack = hop_debug() > 0 ? hop_get_stack( 1 ) : null;

   function onreadystatechange() {
      if( this.readyState == 4 ) {
	 hop_request_onready( this, svc, succ, fail, duperror );
      }

      return false;
   }

   if( !sync ) {
      xhr.open( "PUT", svc, true );
      
      if( hop_config.uint8array ) {
	 xhr.responseType = "arraybuffer";
	 xhr.unserialize = hop_request_unserialize_arraybuffer;
	 xhr.onload = onreadystatechange;
	 xhr.setRequestHeader( "Hop-Serialize", "arraybuffer" );
      } else {
	 xhr.unserialize = hop_request_unserialize;
	 xhr.onreadystatechange = onreadystatechange;
	 xhr.setRequestHeader( "Hop-Serialize", "text" );
      }
   } else {
      xhr.open( "PUT", svc, false );
      
      xhr.unserialize = hop_request_unserialize;
      xhr.onreadystatechange = onreadystatechange;
      xhr.setRequestHeader( "Hop-Serialize", "text" );
   }

   if( t ) {
      if( "setTimeouts" in xhr ) {
	 xhr.setTimeouts = t;
      } else {
	 xhr.timeout = t;
	 xhr.ontimeout = failure;
      }
   }

   xhr.setRequestHeader( 'Content-Type', 'application/x-www-form-urlencoded; charset=ISO-8859-1' );
   if( hop_config.navigator_family != "safari" &&
       hop_config.navigator_family != "chrome" &&
       hop_config.navigator_family != "webkit" ) {
      xhr.setRequestHeader( 'Connection', 'close' );
   }
   if( henv.length > 0 ) {
      xhr.setRequestHeader( 'Hop-Env', henv );
   }
   if( auth ) {
      xhr.setRequestHeader( 'Authorization', auth );
   }
   if( xhr.multipart === true ) {
      /* This header is needed to let the server */
      /* disable timeout for this connection     */
      xhr.setRequestHeader( 'Xhr-Multipart', "true" );
   }
   
   try {
      xhr.send( null );

      if( anim ) {
	 var a = (anim instanceof Function) ? anim : hop_default_anim_get();
	 
	 if( hop_has_setInterval ) {
	    xhr.hop_anim = true;
	    xhr.hop_anim_interval =
	       setInterval( function() {
		     clearInterval( xhr.hop_anim_interval );
		     if( xhr.hop_anim == true )
			xhr.hop_anim = hop_start_anim( svc, a );
		  }, hop_anim_latency );
	 } else {
	    xhr.hop_anim_interval = false;
	    xhr.hop_anim = hop_start_anim( svc, a );
	 }
      } else {
	 xhr.hop_anim = false;
      }

      if( sync ) {
	 if( xhr.readyState == 4 ) {
	    onreadystatechange();
	 } else {
	    sc_error( svc,
		      "with-hop synchronous call failed",
		      "readyState: " + xhr.readyState );
	 }
      }
   } catch( e ) {
      if( typeof hop_stop_anim === "function" ) { 
	 hop_stop_anim( xhr );
      }

      e.hopObject = svc;
      throw e;
   }

   return xhr;
}

/*---------------------------------------------------------------------*/
/*    hop_send_request_xdomain ...                                     */
/*    -------------------------------------------------------------    */
/*    In this function SUCCESS and FAILURE are *always* bound to       */
/*    functions.                                                       */
/*---------------------------------------------------------------------*/
function hop_send_request_xdomain( e, origin ) {
   var xhr = hop_make_xml_http_request();
   var m = e.data.match( "([^ ]*) ([^ ]*) ([^ ]*)" );
   var svc = m[ 1 ];
   var henv = m[ 2 ];
   var auth = m[ 3 ];

   function onreadystatechange() {
      if( xhr.readyState == 4 ) {
	 var ctype = hop_header_content_type( xhr );
	 var serialize = hop_header_hop_serialize( xhr );
	 
	 e.source.postMessage(
	    xhr.status + " " + ctype + " " + serialize + " " + xhr.responseText,
	    e.origin );
      }
   }

   xhr.onreadystatechange = onreadystatechange;
   xhr.open( "PUT", svc, true );
   
   xhr.setRequestHeader( 'Content-Type', 'application/x-www-form-urlencoded; charset=ISO-8859-1' );
   if( hop_config.navigator_family != "safari" &&
       hop_config.navigator_family != "chrome" &&
       hop_config.navigator_family != "webkit" ) {
      xhr.setRequestHeader( 'Connection', 'close' );
   }
   if( henv.length > 0 ) {
      xhr.setRequestHeader( 'Hop-Env', henv );
   }
   if( auth.length > 0 ) {
      xhr.setRequestHeader( 'Authorization', auth );
   }

   xhr.send( null );

   return xhr;
}

/*---------------------------------------------------------------------*/
/*    hop_xdomain_onmessage ...                                        */
/*---------------------------------------------------------------------*/
function hop_xdomain_onmessage( event, svc, succ, fail ) {
   var xhr = new Object();
   var m = event.data.match( "([0-9]*) ([^ ]*) ([^ ]*) (.*)$");
   xhr.status = parseInt( m[ 1 ], 10 );
   xhr.content_type = m[ 2 ];
   xhr.hop_serialize = m[ 3 ];
   xhr.responseText = m[ 4 ];
   
   hop_request_onready( xhr, svc, succ, fail, hop_duplicate_error );
}

/*---------------------------------------------------------------------*/
/*    with_hop_xdomain ...                                             */
/*---------------------------------------------------------------------*/
function with_hop_xdomain( host, port, svc, sync, success, failure, anim, henv, auth, t, x ) {
   if( !port ) port = 80;
   
   if( sync ) {
      sc_error( svc,
		"cross domain with-hop must be asynchronous",
		host + ":" + port );
   } else {
      var id = "__xdomain:" + host + ":" + port;
      var el = document.getElementById( id );
      var origin = "http://" + host + ":" + port + "/hop/admin/xdomain"
      var msg = svc + " " + hop_serialize_request_env() +
	 " " + (auth ? auth : "");
      
      if( !el ) {
	 el = document.createElement( "iframe" );

	 el.src = origin;
	 el.id = id;

	 hop_add_event_listener( el, "load", function() {
	    el.contentWindow.postMessage( msg, origin );
	 }, true );
	 
	 node_style_set( el, "display", "none" );
	 document.body.appendChild( el );
	 
	 hop_add_event_listener(
	    window, "message",
	    function( event ) {
	       hop_xdomain_onmessage( event, svc, success, failure );
	    }, false );
      } else {
	 el.contentWindow.postMessage( msg, origin )
      }
   }
}

/*---------------------------------------------------------------------*/
/*    with_hop ...                                                     */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity -2)) */
function with_hop( svc, success, failure, sync, anim, timeout ) {
   return hop_send_request( svc, sync,
			    success, failure,
			    anim, hop_serialize_request_env(), false, timeout );
}

/*---------------------------------------------------------------------*/
/*    with_hop ...                                                     */
/*---------------------------------------------------------------------*/
/*** META
(define-macro (with-hop svc . rest)
   (let ((success #f)
	 (fail #f)
	 (sync #f)
	 (anim #t)
	 (user #f)
	 (password #f)
	 (authorization #f)
	 (host #f)
	 (port #f)
	 (timeout #f))
      (let loop ((rest rest))
	 (cond
	    ((null? rest)
	     (if host
		 `((@ with_hop_xdomain _)
		   ,host
		   ,port
		   ,svc
		   ,sync
		   ,(or success '(lambda (h) h))
		   ,(or fail '(@ hop_default_failure _))
		   ,anim
		   ((@ hop_serialize_request_env _))
		   ,(cond
		       (authorization
			  authorization)
		       ((and (string? user) (string? password))
			(string-append "Basic "
			   (base64-encode (string-append user ":" password)))))
		   ,timeout)
		   `((@ hop_send_request _)
		   ,svc
		   ,sync
		   ,(or success '(lambda (h) h))
		   ,(or fail '(@ hop_default_failure _))
		   ,anim
		   ((@ hop_serialize_request_env _))
		   ,(cond
		       (authorization
			  authorization)
		       ((and (string? user) (string? password))
			(string-append "Basic "
			   (base64-encode (string-append user ":" password)))))
		   ,timeout)))
	    ((eq? (car rest) :anim)
	     (if (null? (cdr rest))
		 (error 'with-hop "Illegal :anim argument" rest)
		 (set! anim (cadr rest)))
	     (loop (cddr rest)))
	    ((eq? (car rest) :authorization)
	     (if (null? (cdr rest))
		 (error 'with-hop "Illegal :authorization argument" rest)
		 (set! authorization (cadr rest)))
	     (loop (cddr rest)))
	    ((eq? (car rest) :user)
	     (if (null? (cdr rest))
		 (error 'with-hop "Illegal :user argument" rest)
		 (set! user (cadr rest)))
	     (loop (cddr rest)))
	    ((eq? (car rest) :sync)
	     (if (null? (cdr rest))
		 (error 'with-hop "Illegal :sync argument" rest)
		 (set! sync (cadr rest)))
	     (loop (cddr rest)))
	    ((eq? (car rest) :password)
	     (if (null? (cdr rest))
		 (error 'with-hop "Illegal :password argument" rest)
		 (set! password (cadr rest)))
	     (loop (cddr rest)))
	    ((eq? (car rest) :timeout)
	     (if (null? (cdr rest))
		 (error 'with-hop "Illegal :timeout argument" rest)
		 (set! timeout (cadr rest)))
	     (loop (cddr rest)))
	    ((eq? (car rest) :host)
	     (if (null? (cdr rest))
		 (error 'with-hop "Illegal :host argument" rest)
		 (set! host (cadr rest)))
	     (loop (cddr rest)))
	    ((eq? (car rest) :port)
	     (if (null? (cdr rest))
		 (error 'with-hop "Illegal :port argument" rest)
		 (set! port (cadr rest)))
	     (loop (cddr rest)))
	    ((not success)
	     (set! success (car rest))
	     (loop (cdr rest)))
	    ((not fail)
	     (set! fail (car rest))
	     (loop (cdr rest)))
	    (else
             (error 'with-hop "Illegal argument" rest))))))
*/

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

      hop_request_env_string = hop_bigloo_serialize( tmp );
   }
      
   return hop_request_env_string;
}

/*---------------------------------------------------------------------*/
/*    hop_request_reset ...                                            */
/*    -------------------------------------------------------------    */
/*    Is this really needed?                                           */
/*    I think that if it is, a function that returns the whole list    */
/*    of currently binding cells will also be required. For now,       */
/*    this function is not exported.                                   */
/*---------------------------------------------------------------------*/
function hop_request_reset() {
   hop_request_env_string = "";
   hop_request_env_set = false;
   return null;
}

/*---------------------------------------------------------------------*/
/*    hop_request_set ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export request-set!) (arity #t)) */
function hop_request_set( key, val ) {
   hop_request_env_invalid = true;
   hop_request_env[ key ] = val;
   return val;
}

/*---------------------------------------------------------------------*/
/*    hop_request_get ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export request-get)
           (arity #t)
           (peephole (hole 1 "hop_request[" key"]")))
*/
function hop_request_get( key ) {
   return hop_request[ key ];
}


