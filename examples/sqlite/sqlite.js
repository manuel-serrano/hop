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
var Path = require( 'path' );
var Hop = require( 'hop' );
var NP = require( Hop.notepad );

var defpath = Path.join( Path.dirname( module.filename ), "db.sqlite" );

service sqlite( { path: defpath } ) {
   var db = new Sqlite( path );

   return <HTML> {
      <HEAD> {
	 css: NP.css,
	 jscript: NP.jscript
      },
      <NP.NOTEPAD> {
	db.tables().map( function( t ) {
	   return <NP.NPTAB> {
	      <NP.NPTABHEAD> { t },
	      dbTable( db, t )
	   }
	} )
      }
   }
}

function dbTable( db, table ) {
   var cols = db.columns( table );

   return <TABLE> {
      <TR> {
	 <TH> {},
	 cols.map( function( c ) { return <TH> {} } )
      },
      db.map( function( row ) {
	 return <TR> {
	    row.map( function( e ) { return <TD> { e } } )
	 }
      }, "SELECT rowid, * FROM " + table +" ORDER BY rowid" )
   }
}
   
console.log( "Go to \"http://%s:%d/hop/sqlite\"", Hop.hostname, Hop.port );
