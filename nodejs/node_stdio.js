/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/nodejs/node_stdio.js              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct  9 18:26:51 2014                          */
/*    Last change :  Thu Oct  9 18:40:02 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Stdio initialization                                             */
/*=====================================================================*/
var events = require( 'events' );
var util = require( 'util' );

/*---------------------------------------------------------------------*/
/*    initNodeStdio ...                                                */
/*---------------------------------------------------------------------*/
function initNodeStdio( process ) {
   process.stdout.__proto__ = events.EventEmitter.prototype;
   process.stderr.__proto__ = events.EventEmitter.prototype;
   process.stdin.__proto__ = events.EventEmitter.prototype;
}
   
exports.initNodeStdio = initNodeStdio;
