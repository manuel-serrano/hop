var assert = require( 'assert' );

var server = new hop.Server( 'localhost' );

assert.ok( server instanceof hop.Server );

var counter = 0;

var addSvc = service( n ) {
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
