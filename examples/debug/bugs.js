/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/debug/bugs.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jul 10 16:47:32 2014                          */
/*    Last change :  Fri Jul 11 16:11:25 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Collection of bugs                                               */
/*=====================================================================*/

function raise( msg ) {
   throw new Error( msg );
}

function unbound() {
   return xxxx + 4;
}

function interval() {
   setTimeout( function() { raise( "interval" ) }, 1 );
}

exports.raise = raise;
exports.unbound = unbound;
exports.interval = interval;
   
