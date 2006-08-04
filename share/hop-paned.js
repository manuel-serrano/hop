/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-paned.js                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 17 16:08:33 2005                          */
/*    Last change :  Wed Aug  2 16:11:40 2006 (serrano)                */
/*    Copyright   :  2005-06 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP paned client-side implementation                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_vpaned_mousemove ...                                         */
/*---------------------------------------------------------------------*/
function hop_vpaned_mousemove( e, paned ) {
   var val;

   val = ((e.clientX - hop_element_x( paned )) / paned.offsetWidth) * 100;
   hop_vpaned_fraction_set( paned, Math.round( val ));
}

/*---------------------------------------------------------------------*/
/*    hop_vpaned_fraction_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_vpaned_fraction_set( paned, fraction ) {
   if( (fraction instanceof String) || (typeof fraction == "string") ) {
      paned.td1.style.width = fraction;
      paned.td2.style.width = "auto";
   } else {
      if( (fraction < 0) || (fraction > 100) ) {
	 return;
      }

      paned.td1.style.width = fraction + "%";
      paned.td2.style.width = "auto";
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
   var val = e.clientY - hop_element_y( paned );

   paned.pan1.style.height = val;
}

/*---------------------------------------------------------------------*/
/*    hop_hpaned_fraction_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_hpaned_fraction_set( paned, fraction ) {
   if( (fraction instanceof String) || (typeof fraction == "string") ) {
      paned.pan1.style.height = fraction;
   } else {
      if( (fraction < 0) || (fraction > 100) ) {
	 return;
      }

      paned.pan1.style.height = fraction + "%";
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
   cursor.className = "hop-paned-cursor";
   
   div = document.createElement( "div" );
   div.className = "hop-paned-cursoroff";
   cursor.appendChild( div );
   
   // cursor event handling
   var mousemove = function( e ) {
      hop_vpaned_mousemove( e, paned );
   };

   var delmousemove = function( e ) {
      hop_remove_event_listener( document, "mousemove", mousemove, true );
   };
   
   cursor.onmouseover = function( e ) {
      div.className = "hop-paned-cursoron";
   };

   cursor.onmouseout = function( e ) {
      div.className = "hop-paned-cursoroff";
   };

   cursor.onmousedown = function( e ) {
      hop_add_event_listener( document, "mousemove", mousemove, true );
      hop_add_event_listener( document, "mouseup", delmousemove, true );
      hop_add_event_listener( document, "onblur", delmousemove, true );
      e.preventDefault();
      e.stopPropagation();
   }
   
   // re-parent the two pans
   parent.removeChild( pan1 );
   parent.removeChild( pan2 );

   paned.td1 = td1;
   paned.td2 = td2;
   paned.cursor = cursor;

   td1.appendChild( pan1 );
   td2.appendChild( pan2 );

   pan1.style.visibility = "visible";
   pan2.style.visibility = "visible";
   
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
   paned.style.height = "inherit";

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
   cursor.style.width = "100%";
   cursor.style.height = "100%";
   
   pcursor.appendChild( cursor );
   
   pan1.style.visibility = "visible";
   pan2.style.visibility = "visible";

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

   cursor.onmouseover = function( e ) {
      cursor.className = "hop-paned-cursoron";
   };

   cursor.onmouseout = function( e ) {
      cursor.className = "hop-paned-cursoroff";
   };

   cursor.onmousedown = function( e ) {
      hop_add_event_listener( document, "mousemove", mousemove, true );
      hop_add_event_listener( document, "mouseup", delmousemove, true );
      hop_add_event_listener( document, "onblur", delmousemove, true );
   }
      
   return paned;
}

