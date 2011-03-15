/*=====================================================================*/
/*    serrano/prgm/project/hop/2.2.x/share/hop-foldlist.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio                                    */
/*    Creation    :  Wed Mar  1 11:56:02 2006                          */
/*    Last change :  Mon Mar 14 16:46:48 2011 (serrano)                */
/*    -------------------------------------------------------------    */
/*    HOP fold-item implementation                                     */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_fold_item_close ...                                          */
/*---------------------------------------------------------------------*/
function hop_fold_item_close( el, img, history ) {
   el.style.display = "none";
   img.className = "hop-fl-img hop-fl-img-close"
   if( history != false ) hop_state_history_add( el.id, "fl", "0" );
}

/*---------------------------------------------------------------------*/
/*    hop_fold_item_open ...                                           */
/*---------------------------------------------------------------------*/
function hop_fold_item_open( el, img, history ) {
   el.style.display = "block";
   img.className = "hop-fl-img hop-fl-img-open"
   if( history != false ) hop_state_history_add( el.id, "fl", "1" );
}
   
/*---------------------------------------------------------------------*/
/*     hop_fold_item_toggle ...                                        */
/*---------------------------------------------------------------------*/
function hop_fold_item_toggle( id, history ) {
   var el = document.getElementById( id );
   var img = document.getElementById( id + "-img" );
   
   if( el.style.display == "block" ) {
      hop_fold_item_close( el, img, history );
   } else {
      hop_fold_item_open( el, img, history );
   }
}

/*---------------------------------------------------------------------*/
/*     hop_fold_item_toggle_service ...                                */
/*---------------------------------------------------------------------*/
function hop_fold_item_toggle_service( id, history, svc ) {
   var el = document.getElementById( id );
   var img = document.getElementById( id + "-img" );

   if( el.style.display == "block" ) {
      hop_fold_item_close( el, img, history );
   } else {
      with_hop( svc, function( h ) { hop_innerHTML_set( id, h ); } );
      hop_fold_item_open( el, img, history );
   }
}

/*---------------------------------------------------------------------*/
/*    Install the foldlist history state handler                       */
/*---------------------------------------------------------------------*/
if( hop_config.history ) {
   hop_add_event_listener(
      window,
      "load",
      function( _ ) {
	 hop_state_history_register_handler(
	    "fl", /* key argument */
	    "0",  /* reset value  */
	    function( id, arg ) {
	       var el = document.getElementById( id );
	       
	       if( el != undefined ) {
		  var imgo= document.getElementById( id + "-img" );
		  
		  if( arg == "0" ) {
		     hop_fold_item_close( el, img, false );
		  } else {
		     hop_fold_item_open( el, img, false );
		  }
		  
		  return true;
	       } else {
		  return false;
	       }
	    } );
      },
      true );
}
