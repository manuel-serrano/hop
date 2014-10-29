/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker3/worker3.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Sun Oct 26 06:55:51 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Services in workers example                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker3.js                                        */
/*    browser: http://localhost:8080/hop/worker3                       */
/*=====================================================================*/

var w = new Worker( "./slave.js" );
var bar = require( "./bar.js" );

service worker3() {
   import service worker3slave();
   
   return <HTML> {
      <IFRAME> { style: "border: 1px solid black", src: worker3master() },
      <IFRAME> { style: "border: 1px solid black", src: worker3slave() }
   }
}
   
service worker3master() {
   return bar.html( "master" );
}

