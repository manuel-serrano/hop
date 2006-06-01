/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-autoconf.js                   */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 18 05:26:40 2006                          */
/*    Last change :  Mon May 29 14:39:25 2006 (serrano)                */
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

/*    var res = "";                                                    */
/*    for( var p in Object ) {                                         */
/*       res += p + "\n";                                              */
/*    }                                                                */
/*    res += "--------------------------------------------\n\n";       */
/*    document.write( res );                                           */
/* res = "";                                                           */

if( window.HTMLFormElement == undefined ) {
   window.HTMLFormElement = window.HTMLForm;
}

if( window.HTMLCollection == undefined ) {
   window.HTMLCollection = false;
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
   hop_is_html_element = function hop_is_html_element( obj ) {
      return (obj instanceof HTMLElement);
   }
}

/*---------------------------------------------------------------------*/
/*    hop_make_xml_http_request ...                                    */
/*---------------------------------------------------------------------*/
var hop_make_xml_http_request;

if( window.XMLHttpRequest != undefined ) {
   hop_make_xml_http_request = function hop_make_xml_http_request() {
      return new XMLHttpRequest();
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
