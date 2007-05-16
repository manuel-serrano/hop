/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-tabslider.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio [eg@essi.fr]                       */
/*    Creation    :  14-Sep-2005 09:24 (eg)                            */
/*    Last change :  Wed May 16 10:36:58 2007 (serrano)                */
/*    Copyright   :  2006-07 Inria                                     */
/*    -------------------------------------------------------------    */
/*    HOP tabslider implementation                                     */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_tabslider_select ...                                         */
/*---------------------------------------------------------------------*/
function hop_tabslider_select( item ) {
   var parent = item.parentNode;
   var totalHeight = parent.offsetHeight;
   var titlesHeight = 0;
   var selected;
   var i;

   /* save the bookmark history */
   hop_bookmark_state_set( parent.id, "ts", item.id );

   /* select the correct tab */
   for( i = 0; i < parent.childNodes.length; i += 2 ) {
      var title = parent.childNodes[ i ];
      var content = parent.childNodes[ i + 1 ];

      titlesHeight += title.offsetHeight;
      if( title == item ) {
	 selected = content;
	 title.className = "hop-tabslider-head hop-tabslider-head-active";
	 if( content.lang == "delay" ) {
	    hop( selected.onkeyup()(),
		 function( http ) {
	       selected.innerHTML = http.responseText;
	       selected.style.display = "block";
	    } );
	 } else {
	    selected.style.display = "block";
	 }
      } else {
	 content.style.display = "none";
	 title.className = "hop-tabslider-head hop-tabslider-head-inactive";
      }
   }
    
   /* Set the height of the selected item */
   selected.style.height = totalHeight - titlesHeight;
}

/*---------------------------------------------------------------------*/
/*    hop_tabslider_init ...                                           */
/*---------------------------------------------------------------------*/
function hop_tabslider_init( id, ind ) {
   var ts = document.getElementById( id );

   hop_window_onload_add( function( e ) {
      hop_tabslider_select( ts.childNodes[ 2 * ind ] );
   } );

   hop_bookmark_state_register_handler(
      "ts", /* key argument */
      "",  /* reset value  */
      function( id, arg ) {
        if( arg.length != 0 ) {
	   alert( "ts arg=" + arg + " " + document.getElementById( arg ) );
	   hop_tabslider_select( document.getElementById( arg ) );
	}
   } );
}
