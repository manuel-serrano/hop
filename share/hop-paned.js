/*=====================================================================*/
/*    serrano/prgm/project/hop/2.1.x/share/hop-paned.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 17 16:08:33 2005                          */
/*    Last change :  Tue Feb 16 09:15:08 2010 (serrano)                */
/*    Copyright   :  2005-10 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP paned client-side implementation                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    resizeEvent ...                                                  */
/*---------------------------------------------------------------------*/
function resizeEvent( fraction ) {
   this.name = 'resize';
   this.fraction = fraction;
}

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
   var cw = paned.cursor.offsetWidth;
   var pw = paned.inner.clientWidth - cw;

   if( (fraction < 0) || (fraction > 100) ) {
      return false;
   }

   if( pw > 0 ) {
      var lw = Math.round( pw * (fraction/100) );

      node_style_set( paned.el1, "right", (pw + cw - lw) + "px" );
      node_style_set( paned.cursor, "left", lw + "px" );
      node_style_set( paned.el2, "left", (lw + cw) + "px" );
   }
   
   if( paned.fraction != fraction ) {
      paned.fraction = fraction;
      if( paned.onresize != undefined ) {
	 paned.onresize( new resizeEvent( fraction ) );
      }
   }

   return fraction;
}

/*---------------------------------------------------------------------*/
/*    hop_hpaned_mousemove ...                                         */
/*---------------------------------------------------------------------*/
function hop_hpaned_mousemove( e, paned ) {
   var val = ((hop_event_mouse_y( e ) - hop_element_y( paned ) - 2)
	      / paned.offsetHeight) * 100;

   hop_hpaned_fraction_set( paned, Math.round( val ) );
}

/*---------------------------------------------------------------------*/
/*    hop_hpaned_fraction_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_hpaned_fraction_set( paned, fraction ) {
   var ch = paned.cursor.offsetHeight;
   var ph = paned.inner.clientHeight - ch;

   if( (fraction < 0) || (fraction > 100) ) {
      return false;
   }

   if( ph > 0 ) {
      var lh = Math.round( ph * (fraction/100) );

      node_style_set( paned.el1, "bottom", (ph + ch - lh) + "px" );
      node_style_set( paned.cursor, "top", lh + "px" );
      node_style_set( paned.el2, "top", (lh + ch) + "px" );
   }
   
   if( paned.fraction != fraction ) {
      paned.fraction = fraction;
      if( paned.onresize != undefined ) {
	 paned.onresize( new resizeEvent( fraction ) );
      }
   }

   return;
}

/*---------------------------------------------------------------------*/
/*    hop_paned_fraction_set ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export paned-fraction-set!) (arity #t)) */
function hop_paned_fraction_set( paned, fraction ) {
   if( (paned instanceof String) || (typeof paned == "string") ) {
      paned = document.getElementById( paned );
   }

   return paned.fraction_set( paned, fraction );
}

/*---------------------------------------------------------------------*/
/*    hop_paned_fraction_get ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export paned-fraction)
           (arity #t)
           (peephole (postfix ".fraction")))
*/
function hop_paned_fraction_get( paned ) {
   return paned.fraction;
}

/*---------------------------------------------------------------------*/
/*    hop_paned_onresize_get ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export paned-onresize)
           (arity #t)
           (peephole (postfix ".onresize")))
*/
function hop_paned_onresize_get( paned ) {
   return paned.onresize;
}

/*---------------------------------------------------------------------*/
/*    hop_paned_onresize_set ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export paned-onresize-set!) (arity #t)) */
function hop_paned_onresize_set( paned, onresize ) {
   paned.onresize = onresize;
   paned.onresize( new resizeEvent( paned.fraction ) );
}

/*---------------------------------------------------------------------*/
/*    hop_init_paned ...                                               */
/*---------------------------------------------------------------------*/
function hop_init_paned( id, fraction, handler ) {
   var paned = document.getElementById( id );
   var inner = document.getElementById( id + "-inner" );
   var cursor = document.getElementById( id + "-cursor" );
   var el1 = document.getElementById( id + "-el1" );
   var el2 = document.getElementById( id + "-el2" );

   // quick access to the pan children
   paned.el1 = el1;
   paned.el2 = el2;
   paned.cursor = cursor;
   paned.inner = inner;
   paned.fraction = -1;

   // cursor event handling
   var mousemove = function( e ) {
      handler( e, paned );
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

   // hop_add_event_listener
   paned.hop_add_event_listener = function( event, proc, capture ) {
      if( event === "resize" ) {
	 paned.onresize = proc;
      } else {
	 hop_add_native_event_listener( obj, event, proc, capture );
      }
   };

   // postponed initialization
   hop_add_event_listener( window, "ready",
			   function( e ) {
			      paned.fraction_set( paned, fraction ); } );
   
   return paned;
}

/*---------------------------------------------------------------------*/
/*    hop_init_paned_vertical ...                                      */
/*---------------------------------------------------------------------*/
function hop_init_paned_vertical( id, fraction, onresize ) {
   var paned = hop_init_paned( id, fraction, hop_vpaned_mousemove );

   // resize event handling
   var resize = function( e ) {
      hop_vpaned_fraction_set( paned, paned.fraction );
   }
   
   hop_add_event_listener( window, "resize", resize, true );
   
   // setup the initial fraction
   paned.fraction_set = hop_vpaned_fraction_set;
   hop_paned_onresize_set( paned, onresize );
   
   return paned;
}

/*---------------------------------------------------------------------*/
/*    hop_init_paned_horizontal ...                                    */
/*---------------------------------------------------------------------*/
function hop_init_paned_horizontal( id, fraction, onresize ) {
   var paned = hop_init_paned( id, fraction, hop_hpaned_mousemove );
   
   // setup the initial fraction
   paned.fraction_set = hop_hpaned_fraction_set;
   hop_paned_onresize_set( paned, onresize );
      
   return paned;
}

