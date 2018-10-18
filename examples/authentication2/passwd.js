/*=====================================================================*/
/*    .../project/hop/3.0.x/examples/authentication2/passwd.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 19 10:32:06 2014                          */
/*    Last change :  Tue Nov 17 16:44:00 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    User declarations.                                               */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g examples.js                                       */
/*    browser: http://localhost:8080/hop/examples                      */
/*=====================================================================*/
var user = require( hop.user );
var config = require( hop.config );

user.add( {
   name: "anonymous",
   services: ["publicOnly", "publicOrProtected"]
} );

// password hash is computed using user.encryptPassword( 'foo', 'bar' )
user.add( {
   name: "foo",
   password: "+65c3d9a6c38738d995b9f67a0c6b10fa",
   services: ["publicOrProtected", "protectedOnly"],
   directories: ["tmp"]
} );
