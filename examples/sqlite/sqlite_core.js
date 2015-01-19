/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/sqlite/sqlite_core.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 25 09:49:22 2014                          */
/*    Last change :  Sun Jan 18 11:37:11 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    An example of Bigloo/JS connection                               */
/*=====================================================================*/

"(module sqlite_binding (library hop hopscript sqlite) (export hopscript))"

function Sqlite( path ) {
   this.path = path;
   this.builtin = #:pragma( "(lambda (%this path) (instantiate::sqlite (path (js-string->string path))))" )( path );
}

function list2Array( l ) {
   return #:js-vector->jsarray( #:list->vector( #:map( #:js-string->jsstring, l ) ), this );
}

Sqlite.prototype.tables = function() {
   var tables = #:sqlite-name-of-tables( this.builtin );
   return list2Array( tables );
}

Sqlite.prototype.columns = function( table ) {
   var columns = #:sqlite-table-name-of-columns( this.builtin, #:js-jsstring->string( table ) );
   return list2Array( columns );
}

Sqlite.prototype.map = function( f, query ) {
   var l = #:sqlite-map( this.builtin, #:pragma( "(lambda l (js-vector->jsarray (list->vector l) %this))"), #:js-jsstring->string( query ) );
   return list2Array( l ).map( f );
}
   
exports.Sqlite = Sqlite;
