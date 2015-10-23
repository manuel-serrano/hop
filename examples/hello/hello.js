/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/hello/hello.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Fri Oct 23 11:10:05 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    basic example that shows client/server interaction               */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g hello.js                                          */
/*    browser: http://localhost:8080/hop/hello                         */
/*=====================================================================*/
var hop = require( "hop" );

service helloServerDate() {
   return Date.now();
}

service hello() {
   var sdate = <TD id="sdate">${new Date( Date.now() )}</TD>;
   var cdate = <TD id="cdate">-</TD>;

   return <HTML>
     <BUTTON onclick=~{
	    ${helloServerDate}()
	       .post( function( snow ) {
		  ${sdate}.innerHTML = new Date( snow ).toString();
		  ${cdate}.innerHTML = new Date( Date.now() ).toString();
	       } ) }>
     Click me to update dates...
     </BUTTON>
     <TABLE>
       <TR><TH>server date: ${sdate}</TH></TR>
       <TR><TH>client date: ${cdate}</TH></TR>
     </TABLE>
   </HTML>;
}
   
console.log( "Go to \"http://%s:%d/hop/hello\"", hop.hostname, hop.port );
