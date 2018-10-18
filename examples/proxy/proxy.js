/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/proxy/proxy.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Tue Nov 17 16:48:55 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Proxied server-to-server requests                                */
/*    run: hop -v -g -p 8080 -- proxy.js 8888                          */
/*         hop -v -g proxy.js -p 8888                                  */
/*    browser: http://localhost:8080/hop/proxy                         */
/*=====================================================================*/
var port = parseInt( process.argv[ process.argv.length - 1 ] );

service proxy() {
   return <html>
      ~{ function cb( el ) {
	 el.forEach( function( e ) {
	    document.body.appendChild( <div> ${ e } </div> )
	 } ) } }
      <button style="margin-right: 20px"
	       onclick=~{ ${svc}( "Foo", "" ).post( cb ) }>
        direct call
      </button>
      <button onclick=~{ ${proxysvc}( "Foo", "" ).post( cb ) }>
	svc call
      </button>
   </html>
}

function hostId( req ) {
   return req.host + ":" + req.port;
}

service proxysvc( name, path ) {
   var req = this;
   return hop.HTTPResponseAsync(
      function( sendResponse ) {
	 svc( name, path + " via " + hostId( req ) )
	    .post( function( el ) {
	       console.log( "el=", el );
	       sendResponse( el.map( function( e ) { return <DIV> { e } } ) );
	    }, { host: req.host, port: port } );
      }, this );
}

service svc( name, path ) {
   return [ name, name.toUpperCase(), name.toLowerCase(),
	    path + " answered by " + hostId( this ) ];
}

console.log( "Go to \"http://%s:%d/hop/proxy\"", hop.hostname, hop.port );
