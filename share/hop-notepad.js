/*=====================================================================*/
/*    serrano/prgm/project/hop/1.9.x/share/hop-notepad.js              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 17 16:07:08 2005                          */
/*    Last change :  Wed Apr 16 15:10:19 2008 (serrano)                */
/*    Copyright   :  2005-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP notepad implementation                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_notepad_inner_toggle ...                                     */
/*---------------------------------------------------------------------*/
function hop_notepad_inner_toggle( np, to, tabs, bodies ) {
   /* disactive last selected tab */
   tabs.childNodes[ np.active_tab ].className =
      tabs.childNodes[ np.active_tab ].className.replace( "hop-nptab-active",
							  "hop-nptab-inactive" );
   bodies.childNodes[ np.active_tab ].style.display = "none";

   /* active the new selected tab */
   tabs.childNodes[ to ].className =
      tabs.childNodes[ to ].className.replace( "hop-nptab-inactive",
					       "hop-nptab-active" );
   bodies.childNodes[ to ].style.display = "block";

   /* update the layout of the children of the new tab */
   hop_update( bodies.childNodes[ to ] );

   /* store for next time */
   np.active_tab = to;
}

/*---------------------------------------------------------------------*/
/*    hop_notepad_inner_select ...                                     */
/*---------------------------------------------------------------------*/
function hop_notepad_inner_select( np, to, callback ) {
   var tabs = document.getElementById( np.id + "-tabs" );
   var bodies = document.getElementById( np.id + "-body" );
   var i;

   /* at creation time, tab 0 is active */
   if( np.active_tab == undefined ) np.active_tab = 0;

   /* invoke remote tab */
   if( tabs != undefined ) {
      if( tabs.childNodes[ to ].lang == "delay" ) {
	 hop( np.onkeyup()( to ),
	      function( html ) {
		 hop_innerHTML_set( bodies.childNodes[ to ], html );
		 hop_notepad_inner_toggle( np, to, tabs, bodies );
		 
		 if( callback ) callback();
		 /* the tab onselect handler */
		 if( tabs.childNodes[ to ].onselect )
		    tabs.childNodes[ to ].onselect();
		 /* the global onchange handler */
		 if( np.onchange )
		    np.onchange( tabs.childNodes[ to ] );
	      } );
      } else {
	 hop_notepad_inner_toggle( np, to, tabs, bodies );
	 
	 if( callback ) callback();
	 /* the tab onselect handler */
	 if( tabs.childNodes[ to ].onselect ) tabs.childNodes[ to ].onselect();
	 /* the global onchange handler */
	 if( np.onchange ) np.onchange( tabs.childNodes[ to ] );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_notepad_select ...                                           */
/*    -------------------------------------------------------------    */
/*    This is a user function that might be invoked with NOTEPAD       */
/*    and PAN or IDENTs.                                               */
/*---------------------------------------------------------------------*/
/*** META ((export notepad-select)) */
function hop_notepad_select( id1, id2, history, callback ) {
   var np = hop_is_html_element( id1 ) ? id1 : document.getElementById( id1 );
   var tab = hop_is_html_element( id2 ) ? id2 : document.getElementById( id2 );
   var tabs = document.getElementById( np.id + "-tabs" );
   var i;

   for( i = 0; i < tabs.childNodes.length; i++ ) {
      if( tabs.childNodes[ i ] == tab ) {
	 if( history != false ) hop_state_history_add( np.id, "np", i );
	 
	 return hop_notepad_inner_select( np, i, callback );
      }
   }

   alert( "*** Hop Error: hop_notepad_select -- Can't find pad: " + id2 );

   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_notepad_selection ...                                        */
/*---------------------------------------------------------------------*/
/*** META ((export notepad-selection)) */
function hop_notepad_selection( id ) {
   var np = hop_is_html_element( id ) ? id : document.getElementById( id );
   var tabs = document.getElementById( np.id + "-tabs" );

   return tabs.childNodes[ np.active_tab ? np.active_tab : 0 ];
}

/*---------------------------------------------------------------------*/
/*    Install the notepad history state handler                        */
/*---------------------------------------------------------------------*/
hop_state_history_register_handler(
   "np", /* key argument */
   "0",  /* reset value  */
   function( id, arg ) {
     var np = document.getElementById( id );
     if( np != undefined ) {
	hop_notepad_inner_select( np, parseInt( arg ) );
	return true;
     } else {
	return false;
     }
} );
