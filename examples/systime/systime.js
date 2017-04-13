/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/systime/systime.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Aug 23 08:07:57 2015                          */
/*    Last change :  Thu Apr 13 18:25:29 2017 (serrano)                */
/*    Copyright   :  2015-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    An example of SYSTIME                                            */
/*    -------------------------------------------------------------    */
/*    run: hop --no-server -v -g systime.js                            */
/*=====================================================================*/
var Systime = require( hop.systime );

function fib( x ) {
   if( x < 2 ) {
      return 1;
   } else {
      return fib( x - 1 ) + fib( x - 2 );
   }
}

let o = Systime.time( function() { return fib( 30 ) } );

console.log( "res=", o.res, " real=", o.rtime, " stime=", o.stime, " utime=", o.utime );
