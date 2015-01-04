/*=====================================================================*/
/*    .../project/hop/3.0.x/examples/authentication2/remote.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Dec  3 07:54:39 2014                          */
/*    Last change :  Sat Dec 20 07:12:23 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    A dummy service that accepts connections for user FOO.           */
/*    -------------------------------------------------------------    */
/*    run: hop -p 9999 remote.js --rc-file passwd.hop                  */
/*         hop authentication2.js -- 9999                              */
/*=====================================================================*/
var hop = require( "hop" );

service private(type) {
   return '"private" authorized with ' + type;
}

service public(type) {
   return '"public" authorized with ' + type;
}

console.log( "Run the other Hop server and go to \"http://%s/hop/authentication2\"", hop.hostname );
