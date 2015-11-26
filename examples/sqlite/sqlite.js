/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/sqlite/sqlite.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 25 09:33:54 2014                          */
/*    Last change :  Tue Sep 15 08:46:13 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    This shows how to import Bigloo code from JS using modules       */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g sqlite.js                                         */
/*    browser: http://localhost:8080/hop/sqlite                        */
/*=====================================================================*/
var Sqlite = require( './sqlite_core.js' ).Sqlite;
var path = require( 'path' );
var np = require( hop.notepad );

var defpath = path.join( path.dirname( module.filename ), "db.sqlite" );

service sqlite( o ) {
   var path = o && "path" in o ? o.path : defpath;
   var db = new Sqlite( path );

   return <html>
     <head css=${np.css} jscript=${np.jscript}/>
     <np.notepad>
	${db.tables().map( function( t ) {
	   return <np.nptab>
	     <np.nptabhead> ${ t } </np.nptabhead>
	      ${dbTable( db, t )}
	   </np.nptab>
	} )}
     </np.notepad>
   </html>
}

function dbTable( db, table ) {
   var cols = db.columns( table );

   return <table>
     <tr>
       <th/>
       ${cols.map( function( c ) { return <th/> })}
     </tr>
      ${db.map( function( row ) {
	 return <tr>${row.map( function( e ) { return <td> ${ e } </td> } )}</tr>
      }, "SELECT rowid, * FROM " + table +" ORDER BY rowid" )}
   </table>
}
   
console.log( "Go to \"http://%s:%d/hop/sqlite\"", hop.hostname, hop.port );
