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
   services: ["s_public"]
} );

user.add( {
   name: "foo",
   password: "+45fae6f23632b1d2ffe7e7aeab32c333",
   services: ["s_public", "s_private"],
   directories: ["tmp"]
} );
