/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/examples/evtmonitor/evtserver.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Thu Jan 31 14:32:33 2019 (serrano)                */
/*    Copyright   :  2014-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Server event monitor example                                     */
/*    -------------------------------------------------------------    */
/*    run: hop -p 9999 -v -g evtserver.js                              */
/*=====================================================================*/
"use hopscript";

const monitor = new hop.eventListenerMonitor( "foo" );
monitor.monitor( "bar" );

console.log( "monitor=", monitor );

var o = { x: 1, y: 2 };

monitor.addEventListener( "newListener", e => {
   console.log( "newL=", e.data, e.target.header.host );
   setTimeout( _ => hop.broadcast( e.data, { x: 1, y: 2 } ), 2000 );
   if( e.data == "foo" ) {
      setTimeout( _ => {
	 console.log( "bcast gee" );
	 hop.broadcast( "gee", "dummy" )
      }, 3000 );
   }
} );

monitor.addEventListener( "removeListener", e => console.log( "remL=", e.data, e.target.header.host ) );
