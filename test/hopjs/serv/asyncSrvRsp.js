/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/serv/asyncSrvRsp.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Tue Sep  15 11:43:00 2015                         */
/*    Last change :  Sat May 22 06:21:25 2021 (serrano)                */
/*    Copyright   :  2015-21 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Test asynchronous responses in services                          */
/*=====================================================================*/
"use hopscript";

const hop = require( 'hop' );
const runTest = require( './aux/launchWorkers.js' ).runTest;
const clientModule = require.resolve( './aux/stdClient.js' );

// number of concurrent clients
const NUMCLIENTS = 1;
// number of service invocations per client
const NUMCALLS = 200;
// set delay for asynchronous response
const DELAY = 1;
//global timeout (test will fail if not completed by then)
const TIMEOUT = 10000; 
// change TIMEOUT value to match your hardware
// (~ 500 requests/s on a laptop for synchronous responses)

let requests = 0;

service toTest( clientId, num ) {
   requests++;
   console.log( "toTest req=", requests );
   
   const r = { clientId: clientId, num: num };
   return r;
}

function onTimeout() {
   console.log( 'timeout (%s ms): server has processed %s requests',
		TIMEOUT, requests );
   process.exit( 1 );
}

function onFailure() {
   console.log( 'failure: server has processed %s requests', requests );
   process.exit( 1 );
}

runTest( { clientModule: clientModule,
	   numClients: NUMCLIENTS,
	   numCalls: NUMCALLS,
	   timeout: TIMEOUT,
	   onTimeout: onTimeout,
	   onFailure: onFailure,
	 } );
