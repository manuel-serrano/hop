/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-tooltip.js                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon May 21 09:16:36 2007                          */
/*    Last change :  Mon May 21 09:16:47 2007 (serrano)                */
/*    Copyright   :  2007 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hop tooltip support                                              */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_current_tooltip ...                                          */
/*---------------------------------------------------------------------*/
var hop_current_tooltip;

/*---------------------------------------------------------------------*/
/*    hop_tooltip_show ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export tooltip-show)) */
function hop_tooltip_show( event, id, ux, uy ) {
   var el;

   if( hop_is_html_element( id ) ) {
      el = id;
   } else {
      if( (id instanceof String) || (typeof id == "string") ) {
	 el = document.getElementById( id );
      } else {
	 alert( "*** ERROR:hop_tooltip_show:Illegal id -- " + id );
      }
   }

   if( hop_is_html_element( el ) && (hop_current_tooltip != el) ) {

      var p = el.parentNode;
      
      if( p != document.body ) {
	 p.removeChild( el );
	 document.body.appendChild( el );
      }

      if( hop_is_html_element( hop_current_tooltip ) ) {
	 node_style_set( hop_current_tooltip, "visibility", "hidden" );
      }

      node_style_set( el, "visibility", "visible" );
      
      var x = ux != undefined ? ux : event.pageX - 200;
      var y = uy != undefined ? uy : event.pageY - 200;
      var b = document.getElementsByTagName( "body" )[ 0 ];

      node_style_set( el, "left", (x < 0) ? 0 : (x + "px") );
      node_style_set( el, "top", (y < 0) ? 0: (y + "px") );

      // re-parent the popup
      if( el.parentNode != b ) {
	 el.parentNode.removeChild( el );
	 b.appendChild( el );
      }

      hop_current_tooltip = el;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tooltip_hide ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export tooltip-hide)) */
function hop_tooltip_hide() {
   if( hop_is_html_element( hop_current_tooltip ) ) {
      node_style_set( hop_current_tooltip, "visibility", "hidden" );
      hop_current_tooltip = null;
   }
}

