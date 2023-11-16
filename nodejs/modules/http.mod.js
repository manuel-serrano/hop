/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/http.mod.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Feb 21 09:25:49 2023                          */
/*    Last change :  Tue Feb 21 10:01:27 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    http.js es6 module                                               */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    The nodejs module                                                */
/*---------------------------------------------------------------------*/
const http = require("http");

const parsers = http.parsers;
const IncomingMessage = http.IncomingMessage;
const OutgoingMessage = http.OutgoingMessage;
const ServerResponse = http.ServerResponse;
const Agent = http.Agent;
const globalAgent = http.globalAgent;
const ClientRequest = http.ClientRequest;
const request = http.request;
const get = http.get;
const Server = http.Server;
const createServer = http.createServer;
const __connectionListener = http.__connectionListener;
const Client = http.Client;
const createClient = http.createClient;

/*---------------------------------------------------------------------*/
/*    exports ...                                                      */
/*---------------------------------------------------------------------*/
module.exports = http;
export { 
   parsers,
   IncomingMessage,
   OutgoingMessage,
   ServerResponse,
   Agent,
   globalAgent,
   ClientRequest,
   request,
   get,
   Server,
   createServer,
   __connectionListener,
   Client,
   createClient
};
   

