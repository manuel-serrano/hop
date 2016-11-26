/*=====================================================================*/
/*    .../project/hop/3.1.x/test/hopjs/serv/aux/launchWorkers.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Tue Sep  15 11:43:00 2015                         */
/*    Last change :  Wed Nov 23 16:27:32 2016 (serrano)                */
/*    Copyright   :  2015-16 Inria                                     */
/*    -------------------------------------------------------------    */
/*    A generic launcher for stress test workers                       */
/*=====================================================================*/


function runTest( args ) {
   var clientModule = args.clientModule;
   var numClients = args.numClients;
   var numCalls = args.numCalls;
   var timeout = args.timeout;
   var onSuccess = args.onSuccess || process.exit;
   var onTimeout = args.onTimeout || function() {
      console.log( 'Timeout: %sms, test failed', timeout );
      console.log( 'Change TIMEOUT value in source file' );
      onFailure();
   };
   var onFailure = args.onFailure || function() {
      process.exit( 1 );
   };
   var clients = [];
   var readyClients = 0;
   var doneWithClients = 0;
   
   console.log( 'Launcher configuration: %s client modules (%s) X %s',
    		numClients,
    		clientModule,
    		numCalls );

   // Set a timeout in case something goes wrong during test
   // configuration. Will be reset afterwards
   var configurationTimeout = setTimeout( function() {
      console.log( 'cannot configure test: timeout', timeout );
      onFailure();
   }, timeout );
   
   function checkReadiness() {
      // console.log( 'checkReadiness', readyClients );
      if (readyClients == numClients ) {
	 console.log( 'All clients ready. Run test' );
	 // run clients
	 clients.forEach( function( client, id ) {
	    // console.log( 'run client', id );
	    client.postMessage( { messageType: 'run' } );
	 });
	 checkCompletion();

	 clearTimeout( configurationTimeout );
	 // timeout is set while running tests
	 setTimeout( onTimeout, timeout ); 
      }
   }
   
   function checkCompletion() {
      // console.log( 'checkCompletion', doneWithClients );
      if ( doneWithClients == numClients ) {
	 console.log( 'All client tests passed. Checking server post flight assertions' );
	 onSuccess();
      };
   }
   
   // Create workers
   for ( var i = 0; i < numClients; i++ ) {
      console.log( "I=", i, " clients=", clients );
      // console.log( 'start client', i );
      var client = new Worker( clientModule );
      clients.push( client );
      client.onmessage = function( e ) {
	 switch (e.data.messageType) {
	    case 'ready':
	       readyClients++;
	       checkReadiness();
	       break;
	    case 'done': 
	       doneWithClients++;
	       checkCompletion();
	       break;
	    case 'failure':
	       onFailure();
	 };
      };
   };
   // set parameters
   console.log( 'setting parameters' );
   clients.forEach( function( client, id ) {
      client.postMessage( { messageType: 'params', clientId: id, num: numCalls } );
   });
   
   checkReadiness();
}


exports.runTest = runTest;
