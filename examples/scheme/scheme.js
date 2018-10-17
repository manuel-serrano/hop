/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/examples/scheme/scheme.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct  6 09:51:15 2014                          */
/*    Last change :  Fri Mar  9 09:57:35 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    This example shows how to connect JavaScript and Scheme.         */
/*    -------------------------------------------------------------    */
/*    Run with "hop --no-server scheme.js".                            */
/*=====================================================================*/

var scheme = require( "./scheme.hop" );

var str = "my javascript string";
console.log( "md5=", scheme.md5sum( str ) );
console.log( "sha256=", scheme.sha256sum( str ) );

service scheme( o ) {
   return <html>
     <table>
       <tr>
	 <th>md5</th>
	 <td>${scheme.md5sum( str )}</td>
       </tr>
       <tr>
	 <th>sha256</th>
	 <td>${scheme.sha256sum( str )}</td>
       </tr>
     </table>
   </html>
}
