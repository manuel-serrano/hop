/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/https.mod.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Feb 21 09:25:49 2023                          */
/*    Last change :  Tue Feb 21 10:03:27 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    https.js es6 module                                              */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    The nodejs module                                                */
/*---------------------------------------------------------------------*/
const https = require("https");

const server = https.server;
const createServer = https.createServer;
const globalAgent = https.globalAgent;
const Agent = https.Agent;
const request = https.request;
const get = https.get;

/*---------------------------------------------------------------------*/
/*    exports ...                                                      */
/*---------------------------------------------------------------------*/
module.exports = https;
export { 
   server, 
   createServer,
   globalAgent,
   Agent,
   request, 
   get
};
   

