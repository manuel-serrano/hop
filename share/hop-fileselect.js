/*=====================================================================*/
/*    serrano/prgm/project/hop/1.9.x/share/hop-fileselect.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 14 09:43:45 2006                          */
/*    Last change :  Wed Apr  2 10:33:27 2008 (serrano)                */
/*    Copyright   :  2006-08 Manuel Serrano                            */
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
/*    hop_filebrowse ...                                               */
/*---------------------------------------------------------------------*/
function hop_filebrowse( service, title, ident, label, value, path, multiselect,
			 clientX, clientY, width, height ) {
   var x = window.innerWidth/2 - width/2;
   var y = clientY + 20;
   var wident = ident + "-window";

   if( (y + height) > window.innerHeight ) y = window.innerHeight - height;

   hop_window_open( sc_jsstring2keyword( "id" ), wident,
		    sc_jsstring2keyword( "src" ), service( ident, wident, label(), value(), path(), multiselect ),
		    sc_jsstring2keyword( "title" ), title,
		    sc_jsstring2keyword( "class" ), "hop-file-browse",
		    sc_jsstring2keyword( "width" ), width,
		    sc_jsstring2keyword( "height" ), height,
		    sc_jsstring2keyword( "left" ), x,
		    sc_jsstring2keyword( "top" ), y,
		    sc_jsstring2keyword( "parent" ), document.body );
}   
