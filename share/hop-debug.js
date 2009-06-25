/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-debug.js                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Aug  3 14:17:43 2006                          */
/*    Last change :  Thu Sep 20 16:04:33 2007 (serrano)                */
/*    Copyright   :  2006-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Simple HOP debug console                                         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_debug_reset ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export debug-reset!) (arity #t)) */
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
      win = hop_window_open( sc_jsstring2keyword( "id" ), "hop_debug",
			     sc_jsstring2keyword( "src" ), div,
			     sc_jsstring2keyword( "class" ), "hop_debug",
			     sc_jsstring2keyword( "width" ), 640,
			     sc_jsstring2keyword( "height" ), 480,
			     sc_jsstring2keyword( "left" ), 10,
			     sc_jsstring2keyword( "top" ), 10,
			     sc_jsstring2keyword( "parent" ), document.body );
   } else {
      win.style.display = "block";
   }
   
   return pre;
}

/*---------------------------------------------------------------------*/
/*    hop_debug ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export debug) (arity -1)) */
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
