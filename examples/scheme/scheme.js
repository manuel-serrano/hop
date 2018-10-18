/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/scheme/scheme.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct  6 09:51:15 2014                          */
/*    Last change :  Mon Oct  6 11:09:44 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    This example shows how to connect JavaScript and Scheme.         */
/*    -------------------------------------------------------------    */
/*    Run with "hop --no-server scheme.js".                            */
/*=====================================================================*/

var scheme = require( "./scheme.hop" );

var str = "my javascript string";
console.log( "md5=", scheme.md5sum( str ) );
console.log( "sha256=", scheme.sha256sum( str ) );
