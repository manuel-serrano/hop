/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/nodejs/node_timers.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct  9 18:26:51 2014                          */
/*    Last change :  Tue Nov 25 15:27:36 2014 (serrano)                */
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

/*     global.setTimeout = function() {                                */
/*       var t = require('timers');                                    */
/*       return t.setTimeout.apply(this, arguments);                   */
/*     };                                                              */
/*                                                                     */
/*     global.setInterval = function() {                               */
/*       var t = require('timers');                                    */
/*       return t.setInterval.apply(this, arguments);                  */
/*     };                                                              */
/*                                                                     */
/*     global.clearTimeout = function() {                              */
/*       var t = require('timers');                                    */
/*       return t.clearTimeout.apply(this, arguments);                 */
/*     };                                                              */
/*                                                                     */
/*     global.clearInterval = function() {                             */
/*       var t = require('timers');                                    */
/*       return t.clearInterval.apply(this, arguments);                */
/*     };                                                              */
/*                                                                     */
/*     global.setImmediate = function() {                              */
/*       var t = require('timers');                                    */
/*       return t.setImmediate.apply(this, arguments);                 */
/*     };                                                              */
/*                                                                     */
/*     global.clearImmediate = function() {                            */
/*       var t = require('timers');                                    */
/*       return t.clearImmediate.apply(this, arguments);               */
/*     };                                                              */
};
   
exports.initNodeTimers = initNodeTimers;
