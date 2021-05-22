/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/serv/syncSrvRsp.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Tue Sep  15 11:43:00 2015                         */
/*    Last change :  Sat May 22 07:07:51 2021 (serrano)                */
/*    Copyright   :  2015-21 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Test synchronous responses in services                           */
/*=====================================================================*/
"use hopscript";

const runTest = require( './aux/launchWorkers.js' ).runTest;
const clientModule = require.resolve( './aux/stdClient.js' );

const NUMCLIENTS = 5; // number of concurrent clients
const NUMCALLS = 200; // number of service invocations per client
const TIMEOUT = 10000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware ( ~ 500 requests/s on a laptop)

let requests = 0;
service toTest( clientId, num ) {
   requests++;
   return { clientId: clientId, num: num } ;
}

function onTimeout() {
   console.log( 'timeout (%s ms): server has processed %s requests', TIMEOUT, requests );
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
