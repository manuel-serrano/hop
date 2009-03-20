/*=====================================================================*/
/*    serrano/prgm/project/hop/1.10.x/share/hop-prefs.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 21 11:52:04 2008                          */
/*    Last change :  Wed Oct 29 20:55:13 2008 (serrano)                */
/*    Copyright   :  2008 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    PREFS client-side runtime.                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_prefs_edit_svc ...                                           */
/*    -------------------------------------------------------------    */
/*    The name of the services are defined in runtime/prefs.scm        */
/*---------------------------------------------------------------------*/
var hop_prefs_edit_svc = hop_service_base() + "/admin/preferences/edit";
var hop_prefs_save_svc = hop_service_base() + "/admin/preferences/save";

/*---------------------------------------------------------------------*/
/*    hop_prefs_callback ...                                           */
/*---------------------------------------------------------------------*/
function hop_prefs_callback( obj ) {
   if( (obj instanceof String) || (typeof obj === "string") ) {
      alert( obj );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_prefs_editor_expr ...                                        */
/*---------------------------------------------------------------------*/
function hop_prefs_editor_expr( event, inp, name, parse, type, key ) {
   if( hop_event_key_code( event ) == 13 ) {
      inp.className = inp.className.replace( /hop-pr-changed/, "hop-pr-saved" );
      if( !parse || parse( inp.value ) ) {
	 var svc = hop_prefs_edit_svc +
	    "?name=" + name +
	    "&type=" + type +
	    "&value=" + encodeURIComponent( inp.value ) +
	    "&key=" + key;
      
	 with_hop( svc, hop_prefs_callback );
      }
   } else {
      inp.className = inp.className.replace( /hop-pr-saved/, "hop-pr-changed" );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_prefs_editor_bool ...                                        */
/*---------------------------------------------------------------------*/
function hop_prefs_editor_bool( event, inp, name, parse, type, key ) {
   if( !parse || parse( inp.value ) ) {
      var svc = hop_prefs_edit_svc +
	 "?name=" + name +
	 "&type=" + type +
	 "&value=" + inp +
	 "&key=" + key;
      
      with_hop( svc, hop_prefs_callback );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_prefs_valid ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export preferences-valid?)) */
function hop_prefs_valid( id ) {
   
   function loop( node ) {
      if( !("className" in node) ) return true;
      
      if( node.className.search( /hop-pr-changed/ ) >= 0 ) {
	 return false;
      } else {
	 for( var i = 0; i < node.childNodes.length; i++ ) {
	    if( !loop( node.childNodes[ i ] ) ) return false;
	 }
	 return true;
      }
   }   

   return loop( hop_is_html_element( id ) ? id : document.getElementById( id ) );
}

/*---------------------------------------------------------------------*/
/*    hop_prefs_save ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export preferences-save)) */
function hop_prefs_save( id, file ) {
   var el = hop_is_html_element( id ) ? id : document.getElementById( id );

   if( !el ) hop_error( "preferences-save: cannot find element `" + id + "'" );
   
   if( hop_prefs_valid( el ) ||
       confirm( "Some values or not validated, save anyway?" ) ) {
      var base = hop_prefs_save_svc + "?key=" + el.lang + "&file=" + file;
      var svc = base + "&ov=false";

      with_hop( svc, function( h ) {
	    if( !h ) {
	       svc = base + "&ov=true";
	       if( confirm( file + " has been changed, override it?" ) ) {
		  with_hop( svc );
	       }
	    }
	 } );
   }
}
