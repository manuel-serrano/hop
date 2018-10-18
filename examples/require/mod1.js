/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/require/mod1.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Mon Jan  5 12:02:18 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    basic example require example                                    */
/*    -------------------------------------------------------------    */
/*    see require.js                                                   */
/*=====================================================================*/

var s = "";

s += "hello";

exports.hello = function() {
   return s;
}
