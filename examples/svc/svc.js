/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/svc/svc.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 21 07:50:20 2014                          */
/*    Last change :  Thu Aug  6 12:45:31 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Basic example that illustrates services API.                     */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g svc.js                                            */
/*    browser: http://localhost:8080/hop/svc                           */
/*=====================================================================*/
var hop = require( "hop" );

service svc() {
   var conn = <div/>;
   return <html>
     <button onclick=~{
	${svc1}().post( function( r ) { document.body.appendChild( r ) } ) }>
       add "10, 11, 12"
     </button>
     <button onclick=~{
	${svc1}( {c: "c", b: "b", a: "a"} )
	   .post( function( r ) { document.body.appendChild( r ) } ) }>
       add "a, b, c"
     </button>
     <button onclick=~{
	${svc1( {c: 6, b: 5, a: 4} )}
	   .post( function( r ) { document.body.appendChild( r ) } ) }>
       add "4, 5, 6"
     </button>
     <button onclick=~{
	${svc2}( "A", "B", "C" )
	   .post( function( r ) { document.body.appendChild( r ) } ) }>
       add "A, B, C"
     </button>
     <button onclick=~{
	${svc2( 100, 200, 300 ) }
	   .post( function( r ) { document.body.appendChild( r ) } ) }>
       add "100, 200, 300"
     </button>
     <button onclick=~{
        document.body.appendChild( <div>${${svc1}.resource( "svc.js" )}</div> );
        document.body.appendChild( <div>${${svc1.resource( "svc.js" )}}</div> );
     }>
       add source path twice
     </button>
     ${conn}
   </html>;
}

service svc1( { a: 10, b: 11, c: 12 } ) {
   return <div> 
     <span>${a}</span>, 
     <span>${b}</span>, 
     <span>${c}</span>
   </div>;
}

service svc2( a, b, c ) {
   return <div> 
     <span>${a}</span>, 
     <span>${b}</span>, 
     <span>${c}</span>
   </div>;
}

console.log( "Go to \"http://%s:%d/hop/svc\"", hop.hostname, hop.port );

