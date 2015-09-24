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

console.log( "Worker: starting" );

onmessage = function( e ) {
   console.log( "Worker received '%s'", e.data );
   console.log( "Worker processing ..." );
   setTimeout( function() {
      console.log( "Worker sending Report" );
      postMessage( "Report" );
   }, 1000 );
};


