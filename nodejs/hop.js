// (module __nodejs_hop (library hopscript hop))
/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/nodejs/hop.js                     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar 22 15:03:30 2014                          */
/*    Last change :  Sat Mar 22 15:16:34 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hopscript/Hop binding.                                           */
/*=====================================================================*/

exports.currentRequest = function() {
   return #:current-request();
}
