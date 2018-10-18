/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/examples/lang/lang.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun  4 07:54:50 2014                          */
/*    Last change :  Fri Mar  9 15:16:22 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Example of language definition                                   */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g lang.js                                           */
/*=====================================================================*/

"use hopscript";
const csv = require( "./sample.csv", "./csv.js" );

console.log( "csv=", csv );

service lang( o ) {
   return <html>
     <table>
       <tr>${csv[ 0 ].map( h => <th>${h}</th> )}</tr>
       ${csv.slice( 1 ).map( r => <tr>${r.map( e => <td>${e}</td> )}</tr> )}
     </table>
   </html>
}
