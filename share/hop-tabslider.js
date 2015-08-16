/*=====================================================================*/
/*    serrano/prgm/project/hop/2.2.x/share/hop-tabslider.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio [eg@essi.fr]                       */
/*    Creation    :  14-Sep-2005 09:24 (eg)                            */
/*    Last change :  Wed Nov 24 14:06:31 2010 (serrano)                */
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
   var totalHeight = parent.clientHeight;
   var titlesHeight = 0;
   var margins = 0;
   var borders = 0;
   var selected, oldcontent;
   var paddings = 0;
   var i;

   /* select the correct tab */
   for( i = 0; i < parent.childNodes.length; i += 2 ) {
      var title = parent.childNodes[ i ];
      var content = parent.childNodes[ i + 1 ];

      titlesHeight += title.offsetHeight;

      margins += parseInt( node_computed_style_get( title, "marginBottom" ) )
	 + parseInt( node_computed_style_get( title, "marginTop" ) )
	 + parseInt( node_computed_style_get( content, "marginBottom" ) )
	 + parseInt( node_computed_style_get( content, "marginTop" ) );
      borders += parseInt( node_computed_style_get( content, "borderBottomWidth" ) )
	 + parseInt( node_computed_style_get( content, "borderTopWidth" ) );

      if( title == parent.tab_selected )
	 oldcontent = content;
      
      if( title == item ) {
	 selected = content;

	 title.className = "active";
	 content.className = "active";
	 
	 paddings
	    += parseInt( node_computed_style_get( content, "paddingBottom" ) )
	    + parseInt( node_computed_style_get( content, "paddingTop" ) );
	 
	 if( content.lang == "delay" ) {
	    with_hop( selected.onkeyup()(),
		      function( html ) {
			 hop_innerHTML_set( selected, html );
			 selected.className = "active";
		    
			 /* event handlers */
			 if( selected.onselect ) selected.onselect();
			 if( parent.onchange ) parent.onchange( item );
		      } );
	 } else {
	    selected.style.height = "0px";
	    
	    /* update the layout of the children of the new tab */
	    hop_update( selected );
	    
	    /* event handlers */
	    if( selected.onselect ) selected.onselect();
	    if( parent.onchange ) parent.onchange( item );
	 }
      } else {
	 title.className = "inactive";
	 content.className = "inactive";
      }
   }

   /* Set the height of the selected item */
   i = 0;
   var height = totalHeight - titlesHeight - margins - borders - paddings;

   if( oldcontent ) {
      /* if item is already the selected tab, exit */
      if( (parent.tab_selected !== item) && (parent.speed > 0) ) {
	 if( !hop_config.css_transition ) {
	    var int = setInterval( function() {
		  if( i < height ) {
		     oldcontent.style.height = (height - (i + 1)) + "px";
		     selected.style.height = i + "px";
		     i += parent.speed;
		  } else {
		     clearInterval( int );
		     selected.style.height = height + "px";
		     oldcontent.style.height = 0;
		     parent.tab_selected = item;
		  }
	       }, 10 );
	 } else {
	    selected.style.height = height + "px";
	    oldcontent.style.height = 0;
	    parent.tab_selected = item;
	    dom_remove_class( parent, "uninitialized" );	    
	 }
      } else {
	 selected.style.height = height + "px";
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
function hop_tabslider_init( id, ind, history, onchange ) {
   var ts = document.getElementById( id );
   var update = function( e ) {
      ts.tab_selected = ts.childNodes[ 2 * ind ];
      ts.hop_update();
   };

   ts.hop_update = hop_tabslider_update;
   ts.history = (history != false);
   ts.speed = 4;

   hop_add_event_listener(
      window, "ready",
      function( e ) { update(); },
      true );
   
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
