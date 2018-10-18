/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/require/require.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Sun Dec 21 11:10:49 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    basic example require example                                    */
/*    -------------------------------------------------------------    */
/*    run: hopc -g require.js && a.out                                 */
/*=====================================================================*/

var mod1 = require( "./mod1.js" );
var mod1b = require( "./mod1.js" );

var mod2 = require( "mod2.js" );
var world = " ";

console.log( mod1b.hello(), world, mod2.world );
