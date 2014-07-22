/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/qunit/qunit.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 23 07:42:15 2014                          */
/*    Last change :  Fri Jul  4 17:19:35 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    An example of QUnit + Hop combination                            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g qunit.js                                          */
/*    browser: http://localhost:8080/hop/qunit                         */
/*=====================================================================*/

var hop = require( "hop" );

service qunit( { noglobals: false, notrycatch: false }) {
   return <HTML> {
      <HEAD> {
	 css: "http://code.jquery.com/qunit/qunit-1.14.0.css",
	 jscript: "http://code.jquery.com/qunit/qunit-1.14.0.js"
      },
      <BODY> {
	 <DIV> {
	    id: "qunit"
	 },
	 <DIV> {
	    id: "qunit-fixture"
	 },
	 ~{
	    QUnit.test( "hello test", function( assert ) {
	       assert.ok( 1 == "1", "Passed!" );
	    });
	    QUnit.test( "Pas glop test", function( assert ) {
	       assert.ok( 1 == "0", "Passed!" );
	    })
	 }
      }
   }
}
	 
console.log( "Go to \"http://%s:%d/hop/qunit\"", hop.hostname, hop.port );
	 
	    
