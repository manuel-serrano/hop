/*=====================================================================*/
/*    .../project/hop/3.0.x/examples/authentication2/remote.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Dec  3 07:54:39 2014                          */
/*    Last change :  Tue Nov 17 16:37:24 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    A dummy service that accepts connections for user FOO.           */
/*    -------------------------------------------------------------    */
/*    run: hop -p 9999 remote.js --rc-file passwd.js                   */
/*         hop authentication2.js -- 9999                              */
/*=====================================================================*/
service publicOnly( type ) {
   return '"publicOnly" authorized with ' + type;
}

service protectedOnly( type ) {
   return '"protectedOnly" authorized with ' + type;
}

service publicOrProtected( type ) {
   return '"publicOrProtected" authorized with ' + type;
}

console.log( "Run the other Hop server and go to \"http://%s/hop/authentication2\"", hop.hostname );
