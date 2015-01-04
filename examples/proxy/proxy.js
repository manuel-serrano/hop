/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/proxy/proxy.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Sat Dec 20 09:27:32 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Proxied server-to-server requests                                */
/*    run: hop -v -g -p 8080 -- proxy.js 8888                          */
/*         hop -v -g proxy.js -p 8888                                  */
/*    browser: http://localhost:8080/hop/proxy                         */
/*=====================================================================*/
var hop = require( "hop" );

var port = parseInt( process.execArgv[ process.execArgv.length - 1 ] );

service proxy() {
   return <HTML> {
      ~{ function cb( el ) {
	 el.forEach( function( e ) {
	    document.body.appendChild( <DIV> { e } )
	 } ) } },
      <BUTTON> {
	 style: "margin-right: 20px",
	 onclick: ~{ ${svc}( "Foo", "" ).post( cb ) },
	 "direct call"
      },
      <BUTTON> {
	 onclick: ~{ ${proxysvc}( "Foo", "" ).post( cb ) },
	 "svc call"
      }
   }
}

function hostId( req ) {
   return req.host + ":" + req.port;
}

service proxysvc( name, path ) {
   var req = this;
   return hop.HTTPResponseAsync(
      function( reply ) {
	 svc( name, path + " via " + hostId( req ) )
	    .post( function( el ) {
	       console.log( "el=", el );
	       reply( el.map( function( e ) { return <DIV> { e } } ) );
	    }, { host: req.host, port: port } );
      }, this );
}

service svc( name, path ) {
   return [ name, name.toUpperCase(), name.toLowerCase(),
	    path + " answered by " + hostId( this ) ];
}

console.log( "Go to \"http://%s:%d/hop/proxy\"", hop.hostname, hop.port );
