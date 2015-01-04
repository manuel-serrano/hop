/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/nodejs/node_cluster.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jan  2 16:33:54 2015                          */
/*    Last change :  Fri Jan  2 16:38:21 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Cluster initialization                                           */
/*=====================================================================*/
var NativeModule = {};
NativeModule.require = require;

/*---------------------------------------------------------------------*/
/*    initNodeCluster ...                                              */
/*---------------------------------------------------------------------*/
function initNodeCluster() {
   var t = require('cluster');
   // If this is a worker in cluster mode, start up the communiction
   // channel.
   if (process.env.NODE_UNIQUE_ID) {
      var cluster = NativeModule.require('cluster');
      cluster._setupWorker();

      // Make sure it's not accidentally inherited by child processes.
      delete process.env.NODE_UNIQUE_ID;
   }
}

exports.initNodeCluster = initNodeCluster;

