console.log( "starting worker-slave..." );

onexit = function( e ) {
   postMessage( "dying master..." );
}

onmessage = function( e ) {
   console.log( "slave received '%s'", e.data );
   postMessage( "what master?" );
}

console.log( "worker-slave done..." );
