/*=====================================================================*/
/*    .../prgm/project/hop/hop/test/hopjs/serv/aux/stdClient.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Tue Sep  15 11:43:00 2015                         */
/*    Last change :  Fri Mar  3 15:19:55 2023 (serrano)                */
/*    Copyright   :  2015-23 Inria                                     */
/*    -------------------------------------------------------------    */
/*    simple worker to stress test services                            */
/*=====================================================================*/

// This worker iterates <num> service invocations, then
// post a message to inform the main thread of completion

service toTest();

function test(id, num) {
   if (num === 0) {
      postMessage({ messageType: 'done' });
   } else {
      console.log('client #%s: call #%s url=', id, num, toTest(id, num));
      toTest(id, num)
	 .post(res => test(id, num - 1), 
	       rej => {
		  console.log("stdclient.js ERROR:",  rej);
 		  postMessage({ messageType: 'failure'});
	       });
   }
}


/* Protocol with workers launcher */
onmessage = function(e) {
   switch (e.data.messageType) {
      case 'params':
	 id = e.data.clientId;
	 num = e.data.num;
	 postMessage({ messageType: 'ready' });
	 break;
      case 'run':
	 test(id, num);
   }
};
