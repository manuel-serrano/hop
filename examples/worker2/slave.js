/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker2/slave.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Tue Jan 13 16:38:07 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Server side worker thread example                                */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker.js                                         */
/*=====================================================================*/

console.log( "starting worker-slave..." );

onexit = function( e ) {
   console.log( "slave sending last message before dying" );
   postMessage( "dying master..." );
}

onmessage = function( e ) {
   console.log( "slave received '%s'", e.data );
   postMessage( "what master?" );
}

console.log( "worker-slave done..." );
