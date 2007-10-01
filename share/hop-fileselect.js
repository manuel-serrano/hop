/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-fileselect.js                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 14 09:43:45 2006                          */
/*    Last change :  Sat Sep 29 19:27:59 2007 (serrano)                */
/*    Copyright   :  2006-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    <FILESELECT> runtime library.                                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_fileselect_count ...                                         */
/*---------------------------------------------------------------------*/
var hop_fileselect_count = -1;
var hop_fileselect_completions = false;
var hop_fileselect_init = false;

/*---------------------------------------------------------------------*/
/*    hop_fileselect_keypress ...                                      */
/*---------------------------------------------------------------------*/
function hop_fileselect_keypress( service, obj, event, onreturn ) {
   if( hop_event_key_code( event ) == 9 ) {
      hop_stop_propagation( event, false );

      if( !hop_fileselect_init ) hop_fileselect_init = obj.value;

      if( hop_fileselect_count == -1 ) {
	 with_hop( service( obj.value ),
		   function( v ) {
	              hop_fileselect_completions = v;
	              hop_fileselect_complete( obj, v );
	           } );
      } else {
	 hop_fileselect_complete( obj, hop_fileselect_completions );
      }
   } else {
      hop_fileselect_count = -1;
      hop_fileselect_completions = false;
      
      if( hop_event_key_code( event ) == 13 ) {
	 hop_stop_propagation( event, false );

	 obj.onreturn = onreturn;
	 obj.onreturn();
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_fileselect_complete ...                                      */
/*---------------------------------------------------------------------*/
function hop_fileselect_complete( obj, l ) {
   if( l.length > 0 ) {
      hop_fileselect_count++;
      
      if( hop_fileselect_count >= l.length ) {
	 hop_fileselect_count = -1;
	 obj.value = hop_fileselect_init;
      } else {
	 obj.value = l[ hop_fileselect_count ];
      }
   } else {
      hop_fileselect_count = -1;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_filebrowse ...                                               */
/*---------------------------------------------------------------------*/
function hop_filebrowse( service, title, ident, label, value, path, multiselect,
			 clientX, clientY, width, height ) {
   var x = window.innerWidth/2 - width/2;
   var y = clientY + 20;
   var wident = ident + "-window";

   if( (y + height) > window.innerHeight ) y = window.innerHeight - height;

   hop_window_open( new sc_Keyword( "id" ), wident,
		    new sc_Keyword( "src" ), service( ident, wident, label, value, path, multiselect ),
		    new sc_Keyword( "title" ), title,
		    new sc_Keyword( "class" ), "hop-file-browse",
		    new sc_Keyword( "width" ), width,
		    new sc_Keyword( "height" ), height,
		    new sc_Keyword( "left" ), x,
		    new sc_Keyword( "top" ), y,
		    new sc_Keyword( "parent" ), document.body );
}   
