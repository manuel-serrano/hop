/*=====================================================================*/
/*    serrano/prgm/project/hop/1.11.x/share/hop-paned.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 17 16:08:33 2005                          */
/*    Last change :  Mon Feb 16 09:10:53 2009 (serrano)                */
/*    Copyright   :  2005-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP paned client-side implementation                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_vpaned_mousemove ...                                         */
/*---------------------------------------------------------------------*/
function hop_vpaned_mousemove( e, paned ) {
   var val = ((hop_event_mouse_x( e ) - hop_element_x( paned ) - 2)
	      / paned.offsetWidth) * 100;
   hop_vpaned_fraction_set( paned, Math.round( val ) );
}

/*---------------------------------------------------------------------*/
/*    hop_vpaned_fraction_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_vpaned_fraction_set( paned, fraction ) {
   var frac;

   if( (fraction instanceof String) || (typeof fraction == "string") ) {
      node_style_set( paned.td1, "width", fraction );
      frac = parseInt( fraction );
   } else {
      var w = paned.clientWidth;

      if( (fraction < 0) || (fraction > 100) ) {
	 return false;
      }

      frac = fraction;

      if( w > 0 ) {
	 var lw = Math.round(w*(frac/100));
	 node_style_set( paned.td1, "width", lw + "px" );
	 node_style_set( paned.td2, "width", (w - lw) + "px" );
	 // firefox 3 workaround
	 node_style_set( paned.td2.childNodes[ 0 ], "width", (w - lw) + "px" );
      } else {
	 paned.td1.width = frac + "%";
      }
   }

   if( paned.fraction != frac ) {
      paned.fraction = frac;
      if( paned.onresize != undefined ) {
	 paned.onresize();
      }
   }

   return fraction;
}

/*---------------------------------------------------------------------*/
/*    hop_hpaned_dimension_set ...                                     */
/*---------------------------------------------------------------------*/
function hop_hpaned_dimension_set( paned, val1, height ) {
   node_style_set( paned.pan1, "height", val1 + "px" );
   node_style_set( paned.pan2, "height", (height - val1) + "px" );
   paned.fraction = Math.round( (val1 / height) * 100 );

   if( paned.onresize != undefined ) {
      paned.onresize();
   }

   return;
}

/*---------------------------------------------------------------------*/
/*    hop_hpaned_mousemove ...                                         */
/*---------------------------------------------------------------------*/
function hop_hpaned_mousemove( e, paned ) {
   var height = paned.clientHeight;
   var val1 = hop_event_mouse_y( e ) - hop_element_y( paned );

   return hop_hpaned_dimension_set( paned, val1, height );
}

/*---------------------------------------------------------------------*/
/*    hop_hpaned_fraction_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_hpaned_fraction_set( paned, fraction ) {
   var frac;

   if( (fraction instanceof String) || (typeof fraction == "string") ) {
      frac = parseInt( fraction );
   } else {
      frac = fraction;
   }

   if( (frac < 0) || (frac > 100) ) {
      return false;
   } else {
      // MS: 16 Feb 2009, I have no idea where this 6 is coming from.
      // I presume that it is due to a border, margin, or padding property
      // of some child but I'm not able (at that time) to find which...
      var height = paned.offsetHeight - paned.cursor.offsetHeight - 6;
      
      if( height > 0 ) {
	 var val1 = height * (frac / 100);
	 hop_hpaned_dimension_set( paned, val1, height );
      } else {
	 paned.fraction = fraction;
      }
	 
      return fraction;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_paned_fraction_set ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export paned-fraction-set!)) */
function hop_paned_fraction_set( paned, fraction ) {
   if( (paned instanceof String) || (typeof paned == "string") ) {
      paned = document.getElementById( paned );
   }
   
   if( paned.className == "hop-vpaned" )
      hop_vpaned_fraction_set( paned, fraction );
   else 
      hop_hpaned_fraction_set( paned, fraction );
}

/*---------------------------------------------------------------------*/
/*    hop_paned_fraction_get ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export paned-fraction)
           (peephole: (hole 1 "(" paned ").fraction")))
*/
function hop_paned_fraction_get( paned ) {
   return paned.fraction;
}

/*---------------------------------------------------------------------*/
/*    hop_paned_onresize_get ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export paned-onresize)) */
/// peephole: (hole 1 "(" paned ").onresize")
function hop_paned_onresize_get( paned ) {
   return paned.onresize;
}

/*---------------------------------------------------------------------*/
/*    hop_paned_onresize_set ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export paned-onresize-set!)) */
function hop_paned_onresize_set( paned, onresize ) {
   paned.onresize = onresize;
   paned.onresize();
}

/*---------------------------------------------------------------------*/
/*    hop_init_vpaned ...                                              */
/*---------------------------------------------------------------------*/
function hop_init_vpaned( id, pan1id, pan2id, fraction, onresize ) {
   var paned = document.getElementById( id );
   var cursor = document.getElementById( id + "-vpaned-cursor" );
   var pan1 = document.getElementById( pan1id );
   var pan2 = document.getElementById( pan2id );
   var td1 = document.getElementById( id + "-vpaned-td1" );
   var td2 = document.getElementById( id + "-vpaned-td2" );

   // cursor event handling
   var mousemove = function( e ) {
      hop_vpaned_mousemove( e, paned );
   };

   var delmousemove = function( e ) {
      hop_remove_event_listener( document, "mousemove", mousemove, false );
   };
   
   var mousedown = function( e ) {
      hop_add_event_listener( document, "mousemove", mousemove, false );
      hop_add_event_listener( document, "mouseup", delmousemove, false );
      hop_add_event_listener( document, "onblur", delmousemove, false );
      
      hop_stop_propagation( e );
   }

   hop_add_event_listener( cursor, "mousedown", mousedown );
   
   // quick access to the pan children
   paned.td1 = td1;
   paned.td2 = td2;
   paned.cursor = cursor;
   paned.pan1 = pan1;
   paned.pan2 = pan2;

   // Opera 8 is buggous (at least on PDA). Its does not properly
   // recompute the parent height
   var hpan = node_style_get( paned, "height" );
   node_style_set( paned.parentNode, "height", hpan );

   // setup the initial fraction
   paned.fraction = -1;
   hop_vpaned_fraction_set( paned, fraction );
   hop_paned_onresize_set( paned, onresize );
   
   return paned;
}

/*---------------------------------------------------------------------*/
/*    hop_init_hpaned ...                                              */
/*---------------------------------------------------------------------*/
function hop_init_hpaned( id, pan1id, pan2id, fraction, onresize ) {
   var paned = document.getElementById( id );
   var cursor = document.getElementById( id + "-hpaned-cursor" );
   var pan1 = document.getElementById( pan1id );
   var pan2 = document.getElementById( pan2id );

   paned.pan1 = pan1;
   paned.pan2 = pan2;
   paned.cursor = cursor;
   
   // cursor event handling
   var mousemove = function( e ) {
      hop_hpaned_mousemove( e, paned );
   };

   var delmousemove = function( e ) {
      hop_remove_event_listener( document, "mousemove", mousemove, false );
   };

   var mousedown = function( e ) {
      hop_add_event_listener( document, "mousemove", mousemove, false );
      hop_add_event_listener( document, "mouseup", delmousemove, false );
      hop_add_event_listener( document, "onblur", delmousemove, false );
   }

   hop_add_event_listener( cursor, "mousedown", mousedown );
   
   paned.fraction = -1;
   hop_hpaned_fraction_set( paned, fraction );
   hop_paned_onresize_set( paned, onresize );
      
   return paned;
}
