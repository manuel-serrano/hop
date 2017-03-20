/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/serv/worker.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep 21 11:43:00 2015                          */
/*    Last change :  Wed Mar  8 18:06:28 2017 (serrano)                */
/*    Copyright   :  2015-17 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Stress test for workers                                          */
/*=====================================================================*/

var runTest = require( './aux/launchWorkers.js' ).runTest;
var clientModule = require.resolve( './aux/workerNOOP.js' );


var NUMCLIENTS = 10; // number of concurrent clients
var NUMCALLS = 0; // not used
var TIMEOUT = 3000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware


runTest( { clientModule: clientModule,
	   numClients: NUMCLIENTS,
	   numCalls: NUMCALLS,
	   timeout: TIMEOUT,
	 } );
