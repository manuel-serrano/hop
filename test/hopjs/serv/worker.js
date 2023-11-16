/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/serv/worker.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep 21 11:43:00 2015                          */
/*    Last change :  Sat May 20 16:20:44 2023 (serrano)                */
/*    Copyright   :  2015-23 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Stress test for workers                                          */
/*=====================================================================*/

var runTest = require( './aux/launchWorkers.js' ).runTest;
var clientModule = require.resolve( './aux/workerNOOP.js' );


var NUMCLIENTS = 10; // number of concurrent clients
var NUMCALLS = 0; // not used
var TIMEOUT = 10000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware


runTest( { clientModule: clientModule,
	   numClients: NUMCLIENTS,
	   numCalls: NUMCALLS,
	   timeout: TIMEOUT,
	 } );
