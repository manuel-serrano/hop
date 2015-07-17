/*=====================================================================*/
/*    .../project/hop/3.0.x/examples/authentication2/remote.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Dec  3 07:54:39 2014                          */
/*    Last change :  Fri Jul 17 10:01:12 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    A dummy service that accepts connections for user FOO.           */
/*    -------------------------------------------------------------    */
/*    run: hop -p 9999 remote.js --rc-file passwd.hop                  */
/*         hop authentication2.js -- 9999                              */
/*=====================================================================*/
var hop = require( "hop" );

service s_private(type) {
   return '"private" authorized with ' + type;
}

service s_public(type) {
   console.log( "foo" );
   return '"public" authorized with ' + type;
}

console.log( "Run the other Hop server and go to \"http://%s/hop/authentication2\"", hop.hostname );
