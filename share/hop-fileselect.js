/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-fileselect.js                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 14 09:43:45 2006                          */
/*    Last change :  Fri Sep 15 19:24:58 2006 (serrano)                */
/*    Copyright   :  2006 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    <FILESELECT> runtime library.                                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_fileselect_count ...                                         */
/*---------------------------------------------------------------------*/
var hop_fileselect_count = 1;
var hop_fileselect_init = false;

/*---------------------------------------------------------------------*/
/*    hop_fileselect_keypress ...                                      */
/*---------------------------------------------------------------------*/
function hop_fileselect_keypress( service, obj, event ) {
   if( event.which == 9 ) {
      hop_stop_propagation( event, false );

      if( !hop_fileselect_init ) hop_fileselect_init = obj.value;
      
      with_hop( service( obj.value ),
		function( v ) {
	           hop_fileselect_complete( obj, v );
                } );
   } else {
      hop_fileselect_count = -1;
      
      if( event.which == 13 ) {
	 if( obj.onchange ) {
	    obj.onchange();
	 }
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
function hop_filebrowse( service, obj, width, height, event, onselect ) {
   var x = window.innerWidth/2 - width/2;
   var y = event.clientY + 20;

   if( (y + height) > window.innerHeight ) y = window.innerHeight - height;

   obj.onselect = onselect;

   hop_iwindow_open( "file browser", service( obj.id, obj.value ),
		     obj.value,
		     "hop-file-browse",
		     width, height, x, y );
}   
