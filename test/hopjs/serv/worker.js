/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/worker.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep 21 11:43:00 2015                          */
/*    Last change :  Fri Sep 18 16:11:43 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Stress test for workers                                          */
/*=====================================================================*/

var runTest = require( './aux/launchWorkers.js' ).runTest;
var clientModule = require.resolve( './aux/workerNOOP.js' );


var NUMCLIENTS = 10; // number of concurrent clients
var NUMCALLS = 0; // not used
var TIMEOUT = 2000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware


runTest( { clientModule: clientModule,
	   numClients: NUMCLIENTS,
	   numCalls: NUMCALLS,
	   timeout: TIMEOUT,
	 } );
