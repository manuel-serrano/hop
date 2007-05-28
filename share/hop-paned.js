/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-paned.js                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 17 16:08:33 2005                          */
/*    Last change :  Mon May 28 10:11:21 2007 (serrano)                */
/*    Copyright   :  2005-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP paned client-side implementation                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_vpaned_mousemove ...                                         */
/*---------------------------------------------------------------------*/
function hop_vpaned_mousemove( e, paned ) {
   var val;

   val = ((hop_event_mouse_x( e ) - hop_element_x( paned ) - 2)
	  / paned.offsetWidth) * 100;
   hop_vpaned_fraction_set( paned, Math.round( val ));
}

/*---------------------------------------------------------------------*/
/*    hop_vpaned_fraction_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_vpaned_fraction_set( paned, fraction ) {
   if( (fraction instanceof String) || (typeof fraction == "string") ) {
      node_style_set( paned.td1, "width", fraction );
      node_style_set( paned.td2, "width", "auto" );
   } else {
      if( (fraction < 0) || (fraction > 100) ) {
	 return;
      }

      node_style_set( paned.td1, "width", fraction + "%" );
      node_style_set( paned.td2 ,"width", "auto" );
   }

   if( paned.fraction != fraction ) {
      paned.fraction = fraction;
      if( paned.onresize != undefined ) {
	 paned.onresize();
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_hpaned_mousemove ...                                         */
/*---------------------------------------------------------------------*/
function hop_hpaned_mousemove( e, paned ) {
   var val = hop_event_mouse_y( e ) - hop_element_y( paned );

   node_style_set( paned.pan1, "height", val + "px" );
}

/*---------------------------------------------------------------------*/
/*    hop_hpaned_fraction_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_hpaned_fraction_set( paned, fraction ) {
   if( (fraction instanceof String) || (typeof fraction == "string") ) {
      node_style_set( paned.pan1, "height", fraction );
   } else {
      if( (fraction < 0) || (fraction > 100) ) {
	 return;
      }

      node_style_set( paned.pan1, "height", fraction + "%" );
   }

   if( paned.fraction != fraction ) {
      paned.fraction = fraction;
      
      if( paned.onresize != undefined ) {
	 paned.onresize();
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_paned_fraction_set ...                                       */
/*---------------------------------------------------------------------*/
function hop_paned_fraction_set( paned, fraction ) {
   if( paned.className == "hop-vpaned" )
      hop_vpaned_fraction_set( paned, fraction );
   else 
      hop_hpaned_fraction_set( paned, fraction );
}

/*---------------------------------------------------------------------*/
/*    hop_paned_fraction_get ...                                       */
/*---------------------------------------------------------------------*/
function hop_paned_fraction_get( paned ) {
   return paned.fraction;
}

/*---------------------------------------------------------------------*/
/*    hop_paned_onresize_get ...                                       */
/*---------------------------------------------------------------------*/
function hop_paned_onresize_get( paned ) {
   return paned.onresize;
}

/*---------------------------------------------------------------------*/
/*    hop_paned_onresize_set ...                                       */
/*---------------------------------------------------------------------*/
function hop_paned_onresize_set( paned, onresize ) {
   paned.onresize = onresize;
   paned.onresize();
}

/*---------------------------------------------------------------------*/
/*    hop_make_vpaned ...                                              */
/*---------------------------------------------------------------------*/
function hop_make_vpaned( parent, id, klass, fraction, pan1, pan2 ) {
   var document = parent.ownerDocument || parent.document;
   var paned, tbody, tr, td1, td2, cursor, div;

   // the paned
   paned = document.createElement( "table" );
   paned.className = klass;
   paned.id = id;
   paned.setAttribute( "onresize", undefined );
   paned.parent = parent;
   paned.width = "100%";
   paned.height = "100%";

   parent.appendChild( paned );

   // the table body
   tbody = document.createElement( "tbody" );
   paned.appendChild( tbody );
   
   // the table row
   tr = document.createElement( "tr" );
   tbody.appendChild( tr );

   // the table cells
   td1 = document.createElement( "td" );
   cursor = document.createElement( "td" );
   td2 = document.createElement( "td" );
   
   tr.appendChild( td1 );
   tr.appendChild( cursor );
   tr.appendChild( td2 );

   td1.height = "100%";
   cursor.height = "100%";
   td2.height = "100%";
   
   td1.className = "hop-vpaned-pan";
   td2.className = "hop-vpaned-pan";
   cursor.className = "hop-paned-cursor hop-paned-cursoroff";
   
/*    div = document.createElement( "div" );                           */
/*    div.className = "hop-paned-cursoroff";                           */
/*    cursor.appendChild( div );                                       */
   
   // cursor event handling
   var mousemove = function( e ) {
      hop_vpaned_mousemove( e, paned );
   };

   var delmousemove = function( e ) {
      hop_remove_event_listener( document, "mousemove", mousemove, true );
   };
   
   var mousedown = function( e ) {
      hop_add_event_listener( document, "mousemove", mousemove, true );
      hop_add_event_listener( document, "mouseup", delmousemove, true );
      hop_add_event_listener( document, "onblur", delmousemove, true );
      
      hop_stop_propagation( e );
   }

   hop_add_event_listener( cursor, "mousedown", mousedown );
   
   var mouseover = function( e ) {
      cursor.className = "hop-paned-cursor hop-paned-cursoron";
   };

   var mouseout = function( e ) {
      cursor.className = "hop-paned-cursor hop-paned-cursoroff";
   };

   hop_add_event_listener( cursor, "mouseover", mouseover, true );
   hop_add_event_listener( cursor, "mouseout", mouseout, true );
   
   // re-parent the two pans
   parent.removeChild( pan1 );
   parent.removeChild( pan2 );

   paned.td1 = td1;
   paned.td2 = td2;
   paned.cursor = cursor;

   td1.appendChild( pan1 );
   td2.appendChild( pan2 );

   node_style_set( pan1, "visibility", "visible" );
   node_style_set( pan2, "visibility", "visible" );
   
   paned.pan1 = pan1;
   paned.pan2 = pan2;

   paned.fraction = -1;
   
   hop_vpaned_fraction_set( paned, fraction );
      
   return paned;
}

/*---------------------------------------------------------------------*/
/*    hop_make_hpaned ...                                              */
/*---------------------------------------------------------------------*/
function hop_make_hpaned( parent, id, klass, fraction, pan1, pan2 ) {
   var document = parent.ownerDocument || parent.document;
   var td1, td2, pcursor, cursor;
   var tr1, tr2, tr3;

   // the paned
   var paned = document.createElement( "div" );
   paned.className = klass,
   node_style_set( paned, "height", "inherit" );

   // the cursor
   pcursor = document.createElement( "div" );
   pcursor.className = "hop-paned-cursor";
   
   // re-parent the two pans
   parent.removeChild( pan1 );
   parent.removeChild( pan2 );

   paned.appendChild( pan1 );
   paned.appendChild( pcursor );
   paned.appendChild( pan2 );

   cursor = document.createElement( "div" );
   cursor.className = "hop-paned-cursoroff";
   node_style_set( cursor, "width", "100%" );
   node_style_set( cursor, "height", "100%" );
   
   pcursor.appendChild( cursor );
   
   node_style_set( pan1, "visibility", "visible" );
   node_style_set( pan2, "visibility", "visible" );

   paned.pan1 = pan1;
   paned.pan2 = pan2;
   paned.cursor = pcursor;
   
   parent.appendChild( paned );

   // cursor event handling
   var mousemove = function( e ) {
      hop_hpaned_mousemove( e, paned );
   };

   var delmousemove = function( e ) {
      hop_remove_event_listener( document, "mousemove", mousemove, true );
   };

   var mousedown = function( e ) {
      hop_add_event_listener( document, "mousemove", mousemove, true );
      hop_add_event_listener( document, "mouseup", delmousemove, true );
      hop_add_event_listener( document, "onblur", delmousemove, true );
   }

   hop_add_event_listener( cursor, "mousedown", mousedown );
   
   var mouseover = function( e ) {
      cursor.className = "hop-paned-cursoron";
   };
   var mouseout = function( e ) {
      cursor.className = "hop-paned-cursoroff";
   };

   hop_add_event_listener( cursor, "mouseover", mouseover, true );
   hop_add_event_listener( cursor, "mouseout", mouseout, true );
   
   return paned;
}

