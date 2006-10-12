/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-autoconf.js                   */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 18 05:26:40 2006                          */
/*    Last change :  Thu Oct 12 14:29:26 2006 (serrano)                */
/*    Copyright   :  2006 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    All non portable components of the HOP runtime system. All other */
/*    HOP script libraries are supposed *not* to check features        */
/*    availabilities.                                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    DOMFormElement ...                                               */
/*---------------------------------------------------------------------*/
var undefined;

if( window.HTMLFormElement == undefined ) {
   window.HTMLFormElement = window.HTMLForm;
}

if( window.HTMLCollection == undefined ) {
   window.HTMLCollection = false;
}

/*---------------------------------------------------------------------*/
/*    hop_properties_to_string ...                                     */
/*---------------------------------------------------------------------*/
function hop_properties_to_string( obj ) {
   var res = "";
   var i = 0;
   for( var p in obj ) {
      if( i == 10 ) {
	 res += p + "\n";
	 i = 0;
      } else {
	 i++;
	 res += p + " ";
      }
   }
   return res;
}

/*---------------------------------------------------------------------*/
/*    hop_is_html_element ...                                          */
/*---------------------------------------------------------------------*/
var hop_is_html_element;

if( window.HTMLElement == undefined ) {
   hop_is_html_element = function hop_is_html_element( obj ) {
      return (((obj instanceof Object) || (typeof obj == "object"))
	      && (typeof obj.innerHTML == "string"));
   } 
} else {
   var ifr = document.createElement( "iframe" );

   if( ifr instanceof HTMLElement ) {
      hop_is_html_element = function hop_is_html_element( obj ) {
	 return (obj instanceof HTMLElement);
      }
   } else {
      /* this is konqueror */
      var ifproto = ifr.__proto__;
      hop_is_html_element = function hop_is_html_element( obj ) {
	 return (obj instanceof HTMLElement) || (obj.__proto__ == ifproto);
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_make_xml_http_request ...                                    */
/*---------------------------------------------------------------------*/
var hop_make_xml_http_request;

if( window.XMLHttpRequest != undefined ) {
   var req = new XMLHttpRequest();
   if( req.overrideMimeType != undefined ) {
      hop_make_xml_http_request = function hop_make_xml_http_request() {
	 var req = new XMLHttpRequest();
	 /* this is required by some versions of Mozilla */
/* 	 req.overrideMimeType( "text/xml" );                           */
	 return req;
      }
   } else {
      hop_make_xml_http_request = function hop_make_xml_http_request() {
	 return new XMLHttpRequest();
      }
   }
} else {
   if( window.ActiveXObject != undefined ) {
      hop_make_xml_http_request = function hop_make_xml_http_request() {
	 try {
	    return new ActiveXObject( "Msxml2.XMLHTTP" );
	 } catch( e ) {
	    return new ActiveXObject( "Microsoft.XMLHTTP" );
	 }
      }
   } else {
      if( XMLHttpRequest != undefined ) {
	 hop_make_xml_http_request = function hop_make_xml_http_request() {
	    return new XMLHttpRequest();
	 }
      } else {
	 hop_make_xml_http_request = function hop_make_xml_http_request() {
	    alert( "*** ERROR: Don't know how to create XMLHttpRequest" );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    function                                                         */
/*    hop_is_http_json ...                                             */
/*---------------------------------------------------------------------*/
var hop_is_http_json;
var hop_is_http_json_autoconf = hop_make_xml_http_request();

if( (hop_is_http_json_autoconf.propertyIsEnumerable != undefined)
    && hop_is_http_json_autoconf.propertyIsEnumerable( "getResponseHeader" )
    && hop_is_http_json_autoconf.getResponseHeader != undefined ) {
   hop_is_http_json = function hop_is_http_json( http ) {
      return http.getResponseHeader( "hop-json" );
   }
} else {
   hop_is_http_json = function hop_is_http_json( http ) {
      return (http.getAllResponseHeaders().indexOf( "hop-json" ) >= 0);
   }
}

/*---------------------------------------------------------------------*/
/*    hop_style_set ...                                                */
/*---------------------------------------------------------------------*/
var hop_style_set = undefined;

function hop_style_set_native( obj, property, value ) {
   obj.style.setProperty( property, value, "" );
}

function hop_style_set_array( obj, property, value ) {
   obj.style[ property ] = value;
}

hop_style_set = function( obj, property, value ) {
   if( obj.style.setProperty != undefined ) {
      hop_style_set = hop_style_set_native;
   } else {
      hop_style_set = hop_style_set_array;
   }

   return hop_style_set( obj, property, value );
}

/*---------------------------------------------------------------------*/
/*    Event handlers                                                   */
/*---------------------------------------------------------------------*/
var hop_add_event_listener = undefined;
var hop_remove_event_listener = undefined;
var hop_stop_propagation = undefined;

if( document.implementation.hasFeature( "Events" , "2.0") ) {
   hop_add_event_listener = function( obj, event, proc, capture ) {
      return obj.addEventListener( event, proc, capture );
   }
   hop_remove_event_listener = function( obj, event, proc, capture ) {
      return obj.removeEventListener( event, proc, capture );
   }
   hop_stop_propagation = function( event, def ) {
      if( !def ) event.preventDefault();
      event.stopPropagation();
   }
} else {
   hop_add_event_listener = function( obj, event, proc, capture ) {
      var p = function(_) {return proc(window.event)};
      var i = "on" + event + "hdl";

      if( obj[ i ] == undefined ) obj[ i ] = [];
      obj[ i ][ proc ] = p;

      return obj.attachEvent( "on" + event, p );
   }
   hop_remove_event_listener = function( obj, event, proc, capture ) {
      var i = "on" + event + "hdl";
      var proc = obj[ i ][ proc ];

      if( proc != undefined ) {
	 obj[ i ][ proc ] = undefined;
	 return obj.detachEvent( "on" + event, proc );
      }
   }
   hop_stop_propagation = function( event, def ) {
      if( !def ) event.cancelBubble = true;
      event.returnValue = false;
   }
}


/*---------------------------------------------------------------------*/
/*    hop_mozillap ...                                                 */
/*---------------------------------------------------------------------*/
function hop_mozillap() {
   return navigator.userAgent.indexOf( "Mozilla" ) >= 0;
}

/*---------------------------------------------------------------------*/
/*    hop_msiep ...                                                    */
/*---------------------------------------------------------------------*/
function hop_msiep() {
   return navigator.userAgent.indexOf( "MSIE" ) >= 0;
}

/*---------------------------------------------------------------------*/
/*    mouse coords ...                                                 */
/*---------------------------------------------------------------------*/
var hop_event_mouse_x = undefined;
var hop_event_mouse_y = undefined;
var hop_event_key_code = undefined;

if( document.implementation.hasFeature( "Events" , "2.0") ) {
   hop_event_mouse_x = function hop_event_mouse_x( event ) {
      return event.pageX;
   }
   hop_event_mouse_y = function hop_event_mouse_y( event ) {
      return event.pageY;
   }
   hop_event_key_code = function hop_event_key_code( event ) {
      return event.which;
   }
} else {
   hop_event_mouse_x = function hop_event_mouse_x( event ) {
      if( (document.body != null) && (document.body.scrollLeft != null) ) {
	 return event.clientX + document.body.scrollLeft;
      } else {
	 return event.clientX + document.documentElement.scrollLeft;
      }
   }
   hop_event_mouse_y = function hop_event_mouse_y( event ) {
      if( (document.body != null) && (document.body.scrollTop != null) ) {
	 return event.clientY + document.body.scrollTop;
      } else {
	 return event.clientY + document.documentElement.scrollTop;
      }
   }
   hop_event_key_code = function hop_event_key_code( event ) {
      return event.keyCode;
   }
}
