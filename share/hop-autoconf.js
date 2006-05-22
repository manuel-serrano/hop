/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-autoconf.js                   */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 18 05:26:40 2006                          */
/*    Last change :  Mon May 22 16:38:29 2006 (serrano)                */
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
   var hop_has_html_element = false;
   
   hop_is_html_element = function hop_is_html_element( obj ) {
      return (obj instanceof Object && (typeof obj.innerHTML == "string"));
   } 
} else {
   var hop_has_html_element = true;
   
   hop_is_html_element = function hop_is_html_element( obj ) {
      return (obj instanceof HTMLElement);
   }
}

/*---------------------------------------------------------------------*/
/*    function                                                         */
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
	 return new ActiveXObject( "Microsoft.XMLHTTP" );
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
