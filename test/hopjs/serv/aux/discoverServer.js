"use hopscript";

const assert = require( 'assert' );

const server = new hop.Server( 'localhost' );

assert.ok( server instanceof hop.Server );

let counter = 0;

const addSvc = service( n ) {
   console.log( 'server: addSvc service' );
   counter += n;
}

server.register = service( clientServer ) {
   console.log( 'server: new registration' );
   server.addSvc = addSvc;
   server.count = service() {
      return counter;
   };
   return server;
}

postMessage( server );
