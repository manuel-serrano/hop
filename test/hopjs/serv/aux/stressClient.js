/*=====================================================================*/
/*    .../project/hop/3.2.x/test/hopjs/serv/aux/stressClient.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep  14 11:43:00 2015                         */
/*    Last change :  Tue Jun  5 13:13:49 2018 (serrano)                */
/*    Copyright   :  2015-18 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Stress test for services: client worker                          */
/*=====================================================================*/

/* This worker iterates service invocations for <numCalls> times, then
 * post a message to inform the main thread of completion */

var hop = require( 'hop' );

service toTest();

var numCalls = 10;
//var numCalls = 20000;
var effectiveCalls = 0;

onmessage = function( e ) {
   var id = e.data;
   console.log( 'client start', id );
   test( id );
};

function test( id ) {
   if (effectiveCalls == numCalls ) {
      postMessage( id );
   } else {
      try {
	 toTest( { id: id } ).post( function( result ) {
	    effectiveCalls++;
	    test( id );
	 }, { fail: function( error ) {
	    console.log( 'Service invocation failed for client %s at %s',
			 id, effectiveCalls, error );
	    postMessage( id );
	 }});
      } catch( e ) {
	 console.log( 'client %s cannot post at %s', id, effectiveCalls );
      }
   };
}


