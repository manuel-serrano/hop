/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-autoconf.js                   */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 18 05:26:40 2006                          */
/*    Last change :  Thu May 18 10:04:03 2006 (serrano)                */
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
if( window.HTMLElement == undefined ) {
   var hop_has_html_element = false;
   
   function hop_is_html_element( obj ) {
      return (obj instanceof Object && (typeof obj.innerHTML == "string"));
   } 
} else {
   var hop_has_html_element = true;
   
   function hop_is_html_element( obj ) {
      return (obj instanceof HTMLElement);
   }
}
