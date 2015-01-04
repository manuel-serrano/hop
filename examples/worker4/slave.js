/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker4/slave.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Sun Dec 21 11:20:43 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Services in workers example                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker3.js                                        */
/*    browser: http://localhost:8080/hop/worker3                       */
/*=====================================================================*/
var counter = 0;

onmessage = function( e ) {
   e.data( ++counter );
}
