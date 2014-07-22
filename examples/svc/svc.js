/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/svc/svc.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 21 07:50:20 2014                          */
/*    Last change :  Thu Jul  3 15:05:33 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Basic example that illustrates services API.                     */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g svc.js                                            */
/*    browser: http://localhost:8080/hop/svc                           */
/*=====================================================================*/
var hop = require( "hop" );

service svc() {
   var conn = <DIV> {};

   return <HTML> {
      <BUTTON> {
	 onclick: ~{
	    ${svc1}()
	       .post( function( r ) { document.body.appendChild( r ) } ) },
         "add \"10, 11, 12\""
      },
      <BUTTON> {
         onclick: ~{
	    ${svc1}( {c: "c", b: "b", a: "a"} )
	       .post( function( r ) { document.body.appendChild( r ) } ) },
         "add \"a, b, c\""
      },
      <BUTTON> {
         onclick: ~{ ${svc1( {c: 6, b: 5, a: 4} )}
		     .post( function( r ) { document.body.appendChild( r ) } ) },
         "add \"4, 5, 6\""
      },
      <BUTTON> {
         onclick: ~{ ${svc2}( "A", "B", "C" )
		     .post( function( r ) { document.body.appendChild( r ) } ) },
         "add \"A, B, C\""
      },
      <BUTTON> {
         onclick: ~{ ${svc2( 100, 200, 300 ) }
		     .post( function( r ) { document.body.appendChild( r ) } ) },
         "add \"100, 200, 300\""
      },
      <BUTTON> {
         onclick: ~{ 
            document.body.appendChild( <DIV> { ${svc1}.resource( "svc.js" ) } );
            document.body.appendChild( <DIV> { ${svc1.resource( "svc.js" )} } )
         },
         "add source path twice"
      },
      conn
   }
}

service svc1( { a: 10, b: 11, c: 12 } ) {
   return <DIV> {
     <SPAN> { a }, ", ",   
     <SPAN> { b }, ", ",  
     <SPAN> { c }
  }
}

service svc2( a, b, c ) {
   return <DIV> {
     <SPAN> { a }, ", ",    
     <SPAN> { b }, ", ",   
     <SPAN> { c }
  }
}

console.log( "Go to \"http://%s:%d/hop/svc\"", hop.hostname, hop.port );
