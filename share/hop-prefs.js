/*=====================================================================*/
/*    serrano/prgm/project/hop/1.9.x/share/hop-prefs.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 21 11:52:04 2008                          */
/*    Last change :  Mon Apr 21 14:48:29 2008 (serrano)                */
/*    Copyright   :  2008 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    PREFS client-side runtime.                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_prefs_svc ...                                                */
/*    -------------------------------------------------------------    */
/*    The name of the service is defined in runtime/hop-prefs.scm      */
/*---------------------------------------------------------------------*/
var hop_prefs_svc = hop_service_base() + "/prefs/edit";

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
function hop_prefs_editor_expr( event, inp, name, parse, type ) {
   if( hop_event_key_code( event ) == 13 ) {
      inp.className = inp.className.replace( "hop-pr-changed", "hop-pr-saved" );
      if( !parse || parse( value ) ) {
	 var svc = hop_prefs_svc +
	    "?name=" + name +
	    "&type=" + type +
	    "&value=" + inp.value;
      
	 with_hop( svc, hop_prefs_callback );
      }
   } else {
      inp.className = inp.className.replace( "hop-pr-saved", "hop-pr-changed" );
   }
}
