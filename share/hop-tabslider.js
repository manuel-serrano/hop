/*=====================================================================*/
/*    serrano/prgm/project/hop/2.1.x/share/hop-tabslider.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio [eg@essi.fr]                       */
/*    Creation    :  14-Sep-2005 09:24 (eg)                            */
/*    Last change :  Wed Jun 30 18:21:05 2010 (serrano)                */
/*    Copyright   :  2006-10 Inria                                     */
/*    -------------------------------------------------------------    */
/*    HOP tabslider implementation                                     */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_tabslider_user_select ...                                    */
/*---------------------------------------------------------------------*/
/*** META ((export tabslider-select) (arity #t)) ***/
function hop_tabslider_user_select( id1, id2 ) {
   var tab = hop_is_html_element( id2 ) ? id2 : document.getElementById( id2 );
   var tshead = tab.previousSibling;

   return hop_tabslider_select( tshead );
}

/*---------------------------------------------------------------------*/
/*    hop_tabslider_select ...                                         */
/*---------------------------------------------------------------------*/
function hop_tabslider_select( item ) {
   var parent = item.parentNode;

   if( parent.tab_selected != item ) {
      /* generate a new history entry */
      if( parent.history && (parent.tab_selected != item) ) {
	 hop_state_history_add( parent.id, "ts", item.id );
   }
      
      /* select the correct tab */
      hop_tabslider_select_inner( parent, item );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tabslider_select_inner ...                                   */
/*---------------------------------------------------------------------*/
function hop_tabslider_select_inner( parent, item ) {
   var totalHeight = parent.offsetHeight;
   var titlesHeight = 0;
   var selected, old;
   var i;

   /* select the correct tab */
   for( i = 0; i < parent.childNodes.length; i += 2 ) {
      var title = parent.childNodes[ i ];
      var content = parent.childNodes[ i + 1 ];

      titlesHeight += title.offsetHeight;

      if( title == parent.tab_selected ) old = content;
      
      if( title == item ) {
	 selected = content;
	 title.className = "hop-tabslider-head hop-tabslider-head-active";
	 if( content.lang == "delay" ) {
	    with_hop( selected.onkeyup()(),
		      function( html ) {
			 hop_innerHTML_set( selected, html );
			 selected.style.display = "block";
		    
			 /* event handlers */
			 if( selected.onselect ) selected.onselect();
			 if( parent.onchange ) parent.onchange( item );
		      } );
	 } else {
	    selected.style.display = "block";
	    selected.style.height = "0px";
	    /* update the layout of the children of the new tab */
	    hop_update( selected );
	    
	    /* event handlers */
	    if( selected.onselect ) selected.onselect();
	    if( parent.onchange ) parent.onchange( item );
	 }
      } else {
	 content.style.display = "none";
	 title.className = "hop-tabslider-head hop-tabslider-head-inactive";
      }
   }

   /* Set the height of the selected item */
   var i = 0;
   var height = (totalHeight - titlesHeight) - (2*(1+parent.childNodes.length));

   if( old ) {
      old.style.display = "block";
      
      /* if item is already the selected tab, exit */
      if( (parent.tab_selected !== item) && (parent.speed > 0) ) {
	 parent.style.overflow = "hidden";
	 var int = setInterval( function() {
	       if( i < height ) {
		  old.style.height = (height - (i + 1)) + "px";
		  selected.style.height = i + "px";
 		  i += parent.speed;
	       } else {
		  clearInterval( int );
		  old.style.display = "none";
		  selected.style.height = height + "px";
		  parent.tab_selected = item;
		  parent.style.overflow = "auto";
	       }
	    }, 10 );
      } else {
	 selected.style.height = height + "px";
	 old.style.display = "none";
	 parent.tab_selected = item;
      }
   } else {
      selected.style.height = height + "px";
      parent.tab_selected = item;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tabslider_update ...                                         */
/*---------------------------------------------------------------------*/
function hop_tabslider_update() {
   var item = this.tab_selected;
   
   this.tab_selected = false;
   hop_tabslider_select_inner( this, item );
}

/*---------------------------------------------------------------------*/
/*    hop_tabslider_init ...                                           */
/*---------------------------------------------------------------------*/
function hop_tabslider_init( id, ind, history, onchange, speed ) {
   var ts = document.getElementById( id );
   var update = function( e ) {
      ts.tab_selected = ts.childNodes[ 2 * ind ];
      ts.hop_update();
   };

   ts.hop_update = hop_tabslider_update;
   ts.history = (history != false);
   ts.speed = speed;
   
   hop_window_onload_add( update );
   // force an update if the window is already loaded (e.g., if the tabslider
   // is sent via a with-hop call).
   update();

   ts.onchange = onchange;
   
   hop_state_history_register_handler(
      "ts", /* key argument */
      "",  /* reset value  */
      function( id, arg ) {
        if( arg.length != 0 ) {
	   hop_tabslider_select_inner( ts, document.getElementById( arg ) );
	} else {
	   hop_tabslider_select_inner( ts, ts.childNodes[ 2 * ind ] );
	}

	return true;
   } );
}
