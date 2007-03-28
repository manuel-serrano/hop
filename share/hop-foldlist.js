/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-foldlist.js                   */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio                                    */
/*    Creation    :  Wed Mar  1 11:56:02 2006                          */
/*    Last change :  Wed Mar 28 10:54:13 2007 (serrano)                */
/*    -------------------------------------------------------------    */
/*    HOP fold-item implementation                                     */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*     hop_fold_item_toggle ...                                        */
/*---------------------------------------------------------------------*/
function hop_fold_item_toggle( id, open, close ) {
   var el = document.getElementById( id );
   var img = document.getElementById( id + "-img" );
   
   if( el.style.display == "block" ) {
      el.style.display = "none";
      img.src = close;
   } else {
      el.style.display = "block";
      img.src = open;
   }
}

/*---------------------------------------------------------------------*/
/*     hop_fold_item_toggle_service ...                                */
/*---------------------------------------------------------------------*/
function hop_fold_item_toggle_service( id, open, close, svc ) {
   var el = document.getElementById( id );
   var img = document.getElementById( id + "-img" );

   if( el.style.display == "block" ) {
      el.style.display = "none";
      img.src = close;
   } else {
      hop( svc, function( h ) {
	 hop_js_eval( h );
	 el.innerHTML = h.responseText;
      } );
      el.style.display = "block";
      img.src = open;
   }
}
