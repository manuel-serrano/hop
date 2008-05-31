/*=====================================================================*/
/*    serrano/prgm/project/hop/1.9.x/share/hop-request.js              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec 25 06:57:53 2004                          */
/*    Last change :  Sat May 31 07:06:15 2008 (serrano)                */
/*    Copyright   :  2004-08 Manuel Serrano                            */
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

	       url += "&" + els[ i ].name + "=" + hop_bigloo_serialize( val );
	    } else {
	       if( els[ i ].type == "radio" ) {
		  if( els[ i ].checked ) {
		     url += "&" + els[ i ].name + "=" + hop_bigloo_serialize( els[ i ].value );
		  }
	       } else {
		  url += "&" + els[ i ].name + "=" + hop_bigloo_serialize( els[ i ].value );
	       }
	    }
	 }

	 return url;
      } else {
	 for( i = 0; i < len; i++ ) {
	    url += "&" + formals[ i ] + "=" + hop_bigloo_serialize( args[ i ] );
	 }

	 return url;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_service_url_varargs ...                                      */
/*---------------------------------------------------------------------*/
function hop_service_url_varargs( service, args ) {
   var len = (arguments.length - 1);
   
   if( len == 0 ) {
      return service;
   } else {
      var url = service + "?hop-encoding=hop";
      var i;

      for( i = 0; i < len; i++ ) {
	 url += "&a" + i + "=" + hop_bigloo_serialize( arguments[ i + 1 ] );
      }

      return url;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_default_failure ...                                          */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
function hop_default_failure( http ) {
   var t = http.responseText;
   
   if( !t ) return;
	       
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

   if( !hop_config.inline_image ) {
      img.src = hop_share_directory() + "/icons/busy-anim-16.gif";
   } else {
      img.src = hop_busy_anim_16_16;
   }

   vis.appendChild( img );
 
   return vis;
}

/*---------------------------------------------------------------------*/
/*    hop_default_anim ...                                             */
/*---------------------------------------------------------------------*/
var hop_default_anim = hop_anim_16_16;

/*---------------------------------------------------------------------*/
/*    hop_default_anim_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export with-hop-default-anim-set!)) */
function hop_default_anim_set( anim ) {
   var old = hop_default_anim;
   hop_default_anim = anim;
   return old;
}

/*---------------------------------------------------------------------*/
/*    hop_default_anim_get ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export with-hop-default-anim)) */
function hop_default_anim_get( ) {
   return hop_default_anim;
}

/*---------------------------------------------------------------------*/
/*    hop_anim_vis ...                                                 */
/*---------------------------------------------------------------------*/
var hop_anim_vis = false;
var hop_anim_service = false;
var hop_anim_interval;
var hop_anim_fun = false;

/*---------------------------------------------------------------------*/
/*    hop_stop_anim ...                                                */
/*---------------------------------------------------------------------*/
function hop_stop_anim() {
   if( hop_anim_interval ) {
      clearInterval( hop_anim_interval );
      hop_anim_interval = false;
   }
}
   
/*---------------------------------------------------------------------*/
/*    hop_anim ...                                                     */
/*---------------------------------------------------------------------*/
function hop_anim( service, user_anim ) {
   hop_stop_anim();

   if( hop_anim_service == service ) {
      if( hop_anim_vis && (hop_anim_fun == user_anim) ) {
	 hop_anim_vis.title = service;
	 // node_style_set( hop_anim_vis, "visibility", "visible" );
	 node_style_set( hop_anim_vis, "display", "block" );
	 return hop_anim_vis;
      } else {
	 hop_anim_vis = user_anim( service );
	 document.body.appendChild( hop_anim_vis );
	 return hop_anim_vis;
      }
   }
   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_responsetext_error ...                                       */
/*---------------------------------------------------------------------*/
function hop_responsetext_error( xhr ) {
   if( xhr.responseText.length > 80 )
      return xhr.responseText.substring( 0, 80 );
   else
      return responseText;
}

/*---------------------------------------------------------------------*/
/*    hop_send_request ...                                             */
/*    -------------------------------------------------------------    */
/*    In this function SUCCESS and FAILURE are *always* bound to       */
/*    functions.                                                       */
/*    -------------------------------------------------------------    */
/*    This function DOES NOT evaluates its result.                     */
/*---------------------------------------------------------------------*/
function hop_send_request( svc, sync, success, failure, anim, henv, auth ) {
   var xhr = hop_make_xml_http_request();

   function onreadystatechange() {
      if( xhr.readyState == 4 ) {
	 try {
	    var status = xhr.status;

	    switch( status ) {
	       case 200:
		  try {
		     var ctype = hop_header_content_type( xhr );

		     if( ctype == "application/json" ) {
			var expr;
			try {
			   expr = eval( xhr.responseText );
			} catch( exc ) {
			   hop_error( "with-hop",
				      exc,
				      hop_responsetext_error( xhr ),
				      svc );
			   expr = false;
			}
			return success( expr, xhr );
		     } else if( ctype == "text/html" ) {
			var el = hop_create_element( xhr.responseText );
			return success( el, xhr );
		     } else {
			return success( xhr.responseText, xhr );
		     }
		  } catch( exc ) {
		     hop_error( "with-hop [content-type=" + ctype + "]",
				exc,
				hop_responsetext_error( xhr ),
				svc );
		  }

	       case 202:
		  return success( hop_unserialize( xhr.responseText ), xhr );

	       case 204:
		  return false;

	       case 257:
		  return hop_js_eval( xhr );

	       case 258:
		  if( xhr.responseText != null )
		     return eval( xhr.responseText );
		  else
		     return false;

	       case 259:
		  hop_set_cookie( xhr );
		  return false;

	       case 407:
		  hop_error( "with-hop",
			     "Bad authentication",
			     hop_responsetext_error( xhr ),
			     svc );
		  return false;

	       default:
		  if( (typeof status == "number") &&
		      (status > 200) && (status < 300) ) {
		     if( success ) {
			return success( xhr.responseText, xhr );
		     }
		  } else {
		     if( failure ) {
			return failure( xhr );
		     } else {
			return hop_default_failure( xhr );
		     }
		  }
	    }
	 } catch( e ) {
	    if( typeof failure == "function" ) {
	       failure( xhr );
	    } else {
	       throw e;
	    }
	 } finally {
	    try {
	       if( hop_anim_vis != false ) {
		  node_style_set( hop_anim_vis, "display", "none" );
	       }
	       hop_anim_service = false;
	    } catch( e ) {
	    }
	 }
      }

      return false;
   }

   if( !sync ) {
      xhr.onreadystatechange = onreadystatechange;
   }

   xhr.open( "GET", svc, (sync != true) );

   xhr.setRequestHeader( 'Content-Type', 'application/x-www-form-urlencoded; charset=ISO-8859-1' );
   xhr.setRequestHeader( 'Connection', 'close' );
   // to force the response to be interpreted as latin-1:
   // xhr.overrideMimeType( 'text/html; charset=ISO-8859-1' );
   
   if( henv.length > 0 ) {
      xhr.setRequestHeader( 'Hop-Env', hop_serialize_request_env() );
   }

   if( auth ) {
      xhr.setRequestHeader( 'Authorization', auth );
   }
   
   try {
      if( anim ) hop_anim_service = svc;

      xhr.send( null );

      hop_stop_anim();
      
      if( anim ) {
	 var a = (anim instanceof Function) ? anim : hop_default_anim;

	 hop_anim_interval =
	    setInterval( function() { hop_anim( svc, a ) }, hop_anim_latency );
      }

      if( sync ) {
	 if( xhr.readyState == 4 ) {
	    onreadystatechange();
	 } else {
	    hop_error( "with-hop", 
		       "synchronous call failed",
		       "readyState: " + xhr.readyState,
		       svc );
	 }
      }
   } catch( e ) {
      if( hop_anim_vis != false ) {
	 node_style_set( hop_anim_vis, "display", "none" );
      }
      hop_anim_service = false;

      hop_error( "with-hop", e, "Cannot call server", svc );
   }

   return xhr;
}

/*---------------------------------------------------------------------*/
/*    with_hop ...                                                     */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
function with_hop( svc, success, failure, sync, anim ) {
   if( !success ) success = function( h ) { return h };
   if( !failure ) failure = hop_default_failure;
   
   return hop_send_request( svc, sync,
			    success, failure,
			    true, hop_serialize_request_env(), false );
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
	 (authorization #f))
      (let loop ((rest rest))
	 (cond
	    ((null? rest)
	     `(hop_send_request ,svc
				,sync
				,(or success '(lambda (h) h))
				,(or fail 'hop_default_failure)
				,anim
				(hop_serialize_request_env)
				,(cond
				    (authorization
				     authorization)
				    ((and (string? user)
					  (string? password))
				     (string-append
				      "Basic "
				      (base64-encode
				       (string-append
					user ":" password)))))))
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
	    ((eq? (car rest) :password)
	     (if (null? (cdr rest))
		 (error 'with-hop "Illegal :password argument" rest)
		 (set! password (cadr rest)))
	     (loop (cddr rest)))
	    ((not success)
	     (set! success (car rest))
	     (loop (cdr rest)))
	    ((not fail)
	     (set! fail (car rest))
	     (loop (cdr rest)))
	    (else
	     (set! sync (car rest))
	     (loop (cdr rest)))))))
*/

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
/*** META ((export request-set!)) */
function hop_request_set( key, val ) {
   hop_request_env_invalid = true;
   hop_request_env[ key ] = val;
   return val;
}

/*---------------------------------------------------------------------*/
/*    hop_request_get ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export request-get)
           (peephole: (hole 1 "hop_request[" key"]"))) */
function hop_request_get( key ) {
   return hop_request[ key ];
}

