/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/nodejs/node_timers.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct  9 18:26:51 2014                          */
/*    Last change :  Sat Dec 20 07:21:31 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Timers initialization                                            */
/*=====================================================================*/
var events = require( 'events' );
var util = require( 'util' );

/*---------------------------------------------------------------------*/
/*    initNodeTimers ...                                               */
/*---------------------------------------------------------------------*/
function initNodeTimers() {
   var t = require('timers');
   global.setTimeout = t.setTimeout;
   global.setInterval = t.setInterval;
   global.clearTimeout = t.clearTimeout;
   global.clearInterval = t.clearInterval;
   global.setImmediate = t.setImmediate;
   global.clearImmediate = t.clearImmediate;
};
   
exports.initNodeTimers = initNodeTimers;
