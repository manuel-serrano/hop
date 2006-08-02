/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-notepad.js                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 17 16:07:08 2005                          */
/*    Last change :  Wed Aug  2 13:17:49 2006 (serrano)                */
/*    Copyright   :  2005-06 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP notepad implementation                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_notepad_inner_toggle ...                                     */
/*---------------------------------------------------------------------*/
function hop_notepad_inner_toggle( np, to, tabs, bodies ) {
   /* disactive last selected tab */
   tabs.childNodes[ np.active_tab ].className = "hop-nptab-inactive hop-notepad-nptab";
   bodies.childNodes[ np.active_tab ].style.display = "none";

   /* active the new selected tab */
   tabs.childNodes[ to ].className = "hop-nptab-active hop-notepad-nptab";
   bodies.childNodes[ to ].style.display = "block";

   /* store for next time */
   np.active_tab = to;
}

/*---------------------------------------------------------------------*/
/*    hop_notepad_inner_select ...                                     */
/*---------------------------------------------------------------------*/
function hop_notepad_inner_select( np, to ) {
   var tabs = np.childNodes[ 1 ];
   var bodies = np.childNodes[ 2 ];

   /* at creation time, tab 0 is active */
   if( np.active_tab == undefined ) np.active_tab = 0;

   /* invoke remote tab */
   if( tabs.childNodes[ to ].lang == "delay" ) {
      hop( np.onkeyup()( to ),
	   function( http ) {
	      hop_replace_inner( bodies.childNodes[ to ] )( http );
	      hop_notepad_inner_toggle( np, to, tabs, bodies );
           } );
   } else {
      hop_notepad_inner_toggle( np, to, tabs, bodies );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_notepad_select ...                                           */
/*    -------------------------------------------------------------    */
/*    This is a user function that might be invoked with NOTEPAD       */
/*    and PAN or IDENTs.                                               */
/*---------------------------------------------------------------------*/
function hop_notepad_select( id1, id2 ) {
   var np = hop_is_html_element( id1 ) ? id1 : document.getElementById( id1 );
   var tab = hop_is_html_element( id2 ) ? id2 : document.getElementById( id2 );
   var tabs = np.childNodes[ 1 ];
   var i;

   for( i = 0; i < tabs.childNodes.length; i++ ) {
      if( tabs.childNodes[ i ] == tab ) {
	 return hop_notepad_inner_select( np, i );
      }
   }

   alert( "*** Hop Error: hop_notepad_select -- Can't find pad `" + id2 + "'");
}
