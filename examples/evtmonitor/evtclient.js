/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/examples/evtmonitor/evtclient.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Thu Jan 31 14:35:21 2019 (serrano)                */
/*    Copyright   :  2014-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Server monitor example                                           */
/*    -------------------------------------------------------------    */
/*    run: hop -v -- evtclient.js 9999                                 */
/*=====================================================================*/
"use hopscript";

var port = parseInt( process.argv[ process.argv.length - 1 ] );
const srv = new hop.Server( "localhost", port );

console.log( "srv=", srv );

const ltn = function( e ) {
   console.log( "e=", e.name );
}

console.log( "adding server events..." );

srv.addEventListener( "ready", _ => console.log( "server ready" ) );
srv.addEventListener( "down", _ => console.log( "server down" ) );

srv.addEventListener( "foo", ltn );
srv.addEventListener( "bar", ltn );
srv.addEventListener( "gee", e => console.log( "gee.e=", e ) );

setTimeout( _ => {
   srv.removeEventListener( "foo", ltn );
   console.log( "removing listener foo" );
}, 100 );
