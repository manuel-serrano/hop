/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/syntax/syntax.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep 14 17:00:00 2015                          */
/*    Last change :  Tue Sep  8 16:36:36 2015 (serrano)                */
/*    Copyright   :  2014-15 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Hop.js syntax extensions                                         */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g syntax.js                                         */
/*    browser: http://localhost:8080/hop/svg                           */
/*=====================================================================*/

// service declaration with standard formal arguments
service add( x, y ) {
   console.log( 'add - x: %s (%s), y: %s (%s)', x, typeof( x ), y, typeof( y ) );
   return x + y;
}

function handleResult( result ) {
   console.log( 'add returned: %s (%s)', result, typeof( result ) );
}

add( 2, 3 ).post( handleResult );
// add - x: 2 (number), y: 3 (number)
// add returned: 5 (number)

add( 2 ).post( handleResult );
// add - x: 2 (number), y: undefined (undefined)
// add returned: NaN (number)

add( 'foo', 'bar' ).post( handleResult );
// add - x: foo (string), y: bar (string)
// add returned: foobar (string)

add( 7, ' is a prime number').post( handleResult );
// add -x: 7 (number), y:  is a prime number (string)
// add returned: 7 is a prime number (string)

service list() {
   console.log( arguments ); // arguments is defined like in JavaScript functions
}

list().post(); // {}

list( 'foo', 'bar' ).post(); // { '0': 'foo', '1': 'bar' };


// service declaration with named formal arguments and default values
var scale = 100;

service figure(	{x: 0, y: 0, w: 2* scale, h: scale , r: 20, shape: 'rectangle' } ) {
    switch (shape) {
    case 'square': return { x: x, y: y, w: w, shape: shape };
    case 'rectangle': return {x: x, y: y, w: w, h: h, shape: shape };
    case 'circle': return { x: x, y:y, r: r, shape: shape };
    default: return { x: x, y: y, shape: 'point' };
    };
}


figure( { w: 200, shape: 'rectangle' } ).post( console.log );
// { shape: 'rectangle', x: 0, y: 0, w: 200, h: 100 }

figure( { x: 40, y: 50, shape: 'square' } ).post( console.log );
// { x: 40, y: 50, w: 200, shape: 'square' }

figure( { shape: 'circle', y: 25 } ).post( console.log );
// { x: 0, y: 25, r: 20, shape: 'circle' }


// a service that returns plain HTML

service hello() {
   return <html> <h1> Hello </h1> </html>;
}

// a service that returns an HTML element computed on the server side
var messages = { 'alice': 5, 'bob': 3 } // a database
service pendingMessages( name ) {
   var numMessages = messages[ name ] || 0;
   return <div> Hello ${name},  ${numMessages} messages </div>
}

// a complex service that combines server and client code
service syntax() {
   var info = <div/>;
   var input = <input size="10"/>;
   var button = <button onclick=~{
      var name = ${input}.value;
      ${pendingMessages}( name ).post( function( htmlElement ) {
	 ${info}.appendChild( htmlElement );
      });
   }> login </button>;
   return <html>
     <div> Enter your name (alice, bob, whoever ) 
       ${input}
       ${button}
     </div>
     ${info}
   </html>;
}
