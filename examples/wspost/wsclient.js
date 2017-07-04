/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/wspost/wsclient.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Tue Jul  4 09:33:07 2017 (serrano)                */
/*    Copyright   :  2014-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    WebSocket client example                                         */
/*    -------------------------------------------------------------    */
/*    run: hop -v -- wsclient.js 9999                                  */
/*=====================================================================*/
"use hopscript";

var port = parseInt( process.argv[ process.argv.length - 1 ] );
var ws = new WebSocket( "ws://localhost:" + port + "/hop/serv" );

ws.obj = service obj();
ws.asyn = service asyn();
ws.str = service str();

var f1 = ws.obj( { a: 1 } );
var f2 = ws.asyn( { a: 2 } );
var f3 = ws.str( { a: 20 } );
var f4 = ws.str( { a: 2 } );
var f5 = ws.obj( { a: 3 } );

f1.post()
   .then( result => console.log( "obj result=", result ) )
   .catch( reason => console.log( "obj reason=", reason ) );

f2.post()
   .then( result => console.log( "asyn result=", result ) )
   .catch( reason => console.log( "asyn reason=", reason ) );

f3.post()
   .then( result => console.log( "str3 result=", result ) )
   .catch( reason => console.log( "str3 reason=", reason ) );

f4.post()
   .then( result => console.log( "str4 result=", result ) )
   .catch( reason => console.log( "str4 reason (expected)=", reason ) );
