/*=====================================================================*/
/*    serrano/prgm/project/hop/2.0.x/share/hop-foldlist.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio                                    */
/*    Creation    :  Wed Mar  1 11:56:02 2006                          */
/*    Last change :  Sat Jun  6 08:28:23 2009 (serrano)                */
/*    -------------------------------------------------------------    */
/*    HOP fold-item implementation                                     */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_fold_item_close ...                                          */
/*---------------------------------------------------------------------*/
function hop_fold_item_close( el, imgo, imgc, history ) {
   el.style.display = "none";
   imgo.style.display = "none";
   imgc.style.display = "block";
   if( history != false ) hop_state_history_add( el.id, "fl", "0" );
}

/*---------------------------------------------------------------------*/
/*    hop_fold_item_open ...                                           */
/*---------------------------------------------------------------------*/
function hop_fold_item_open(  el, imgo, imgc, history ) {
   el.style.display = "block";
   imgo.style.display = "block";
   imgc.style.display = "none";
   if( history != false ) hop_state_history_add( el.id, "fl", "1" );
}
   
/*---------------------------------------------------------------------*/
/*     hop_fold_item_toggle ...                                        */
/*---------------------------------------------------------------------*/
function hop_fold_item_toggle( id, history ) {
   var el = document.getElementById( id );
   var imgo = document.getElementById( id + "-imgo" );
   var imgc = document.getElementById( id + "-imgc" );
   
   if( el.style.display == "block" ) {
      hop_fold_item_close( el, imgo, imgc, history );
   } else {
      hop_fold_item_open( el, imgo, imgc, history );
   }
}

/*---------------------------------------------------------------------*/
/*     hop_fold_item_toggle_service ...                                */
/*---------------------------------------------------------------------*/
function hop_fold_item_toggle_service( id, history, svc ) {
   var el = document.getElementById( id );
   var imgo = document.getElementById( id + "-imgo" );
   var imgc = document.getElementById( id + "-imgc" );

   if( el.style.display == "block" ) {
      hop_fold_item_close( el, imgo, imgc, history );
   } else {
      with_hop( svc, function( h ) { hop_innerHTML_set( id, h ); } );
      hop_fold_item_open( el, imgo, imgc, history );
   }
}

/*---------------------------------------------------------------------*/
/*    Install the foldlist history state handler                       */
/*---------------------------------------------------------------------*/
hop_state_history_register_handler(
   "fl", /* key argument */
   "0",  /* reset value  */
   function( id, arg ) {
     var el = document.getElementById( id );

     if( el != undefined ) {
	var imgo = document.getElementById( id + "-imgo" );
	var imgc = document.getElementById( id + "-imgc" );

	if( arg == "0" ) {
	   hop_fold_item_close( el, imgo, imgc, false );
	} else {
	   hop_fold_item_open( el, imgo, imgc, false );
	}

	return true;
     } else {
	return false;
     }
} );
