/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-debug.js                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Aug  3 14:17:43 2006                          */
/*    Last change :  Sat Dec  9 08:40:43 2006 (serrano)                */
/*    Copyright   :  2006 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Simple HOP debug console                                         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_debug_reset ...                                              */
/*---------------------------------------------------------------------*/
function hop_debug_reset() {
   var pre = document.getElementById( "hop_debug_content" );

   if( pre != null ) pre.innerHTML = '';
}

/*---------------------------------------------------------------------*/
/*    hop_debug_get_container ...                                      */
/*---------------------------------------------------------------------*/
function hop_debug_get_container() {
   var pre = document.getElementById( "hop_debug_content" );
   var win = document.getElementById( "hop_debug" );

   if( pre == null ) {
      var div = document.createElement( "div" );
      pre = document.createElement( "pre" );
      
      div.id = "hop_debug_div";
      div.className = "hop_debug";
      
      pre.id = "hop_debug_content";
      pre.className = "hop_debug_pre";

      div.innerHTML = "<DIV class='hop_debug_reset'><BUTTON onclick='hop_debug_reset()'>Reset</BUTTON></DIV>";
      div.appendChild( pre );
      win = hop_iwindow_open( "hop_debug", div, "hop_debug", false, 640, 480, 10, 10 );
   } else {
      win.style.display = "block";
   }
   
   return pre;
}

/*---------------------------------------------------------------------*/
/*    hop_debug ...                                                    */
/*---------------------------------------------------------------------*/
function hop_debug() {
   var pre = hop_debug_get_container();
   var i;
   
   for( i = 0; i < arguments.length; i++ ) {
      var o = arguments[ i ];
      if( (typeof o == "string") || (o instanceof String) ) {
	 var t = document.createTextNode( o );
	 pre.appendChild( t );
      } else {
	 pre.appendChild( o );
      }
   }
}
