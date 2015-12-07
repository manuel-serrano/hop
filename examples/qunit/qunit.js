/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/qunit/qunit.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 23 07:42:15 2014                          */
/*    Last change :  Thu Nov 26 17:06:04 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    An example of QUnit + Hop combination                            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g qunit.js                                          */
/*    browser: http://localhost:8080/hop/qunit                         */
/*=====================================================================*/
service qunit( o ) {
   var noglobals = o && "noglobals" in o ? o.noglobals : false;
   var notrycatch = o && "notrycatch" in o ? o.notrycatch : false;
   var testNumber = o && "testNumber" in o ? o.testNumber : 0;
   
   return <html>
      <head css="http://code.jquery.com/qunit/qunit-1.14.0.css"
	    jscript= "http://code.jquery.com/qunit/qunit-1.14.0.js"/>
      <body>
	 <div id="qunit"/>
	 <div id="qunit-fixture"/>
	 ~{
	    QUnit.test( "hello test", function( assert ) {
	       assert.ok( 1 == "1", "Passed!" );
	    });
	    QUnit.test( "Pas glop test", function( assert ) {
	       assert.ok( 1 == "0", "Passed!" );
	    })
	 }
      </body>
   </html>
}
	 
console.log( "Go to \"http://%s:%d/hop/qunit\"", hop.hostname, hop.port );
