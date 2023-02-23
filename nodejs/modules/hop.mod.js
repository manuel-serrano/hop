/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/hop.mod.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Feb 23 14:26:18 2023                          */
/*    Last change :  Thu Feb 23 14:38:05 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    hop.js es6 module                                                */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    The nodejs module                                                */
/*---------------------------------------------------------------------*/
const hop = require("hop");

const engine = hop.engine;
const debug = hop.debug;
const isServer = hop.isServer;
const isWorker = hop.isWorker;
const hostname = hop.hostname;
const hostip = hop.hostip;
const version = hop.version;
const buildId = hop.buildId;
const buildTag = hop.buildTag;
const buidArch = hop.buidArch;
const rcDirectory = hop.rcDirectory;
const cacheDirectory = hop.cacheDirectory;
const port = hop.port;
const ports = hop.ports;
const standalone = hop.standalone;
const loginCookieCryptKey = hop.loginCookieCryptKey;
const Service = hop.Service;
const HopFrame = hop.HopFrame;
const webService = hop.webService;
const isRecord = hop.isRecord;
const recordOf = hop.recordOf;
const HTTPResponseHop = hop.HTTPResponseHop;
const HTTPResponseXml = hop.HTTPResponseXml;
const HTTPResponseFile = hop.HTTPResponseFile;
const HTTPResponseString = hop.HTTPResponseString;
const HTTPResponseAuthentication = hop.HTTPResponseAuthentication;
const HTTPResponseAsync = hop.HTTPResponseAsync;
const HTTPResponseProxy = hop.HTTPResponseProxy;
const HTTPResponseJson = hop.HTTPResponseJson;
const HTTPResponseError = hop.HTTPResponseError;
const log = hop.log;
const isLocalRequest = hop.isLocalRequest;
const requestHostAddress = hop.requestHostAddress;
const requestLocalAddress = hop.requestLocalAddress;
const addRequestFilter = hop.addRequestFilter;
const addRequestFilterFirst = hop.addRequestFilterFirst;
const addRequestFilterLast = hop.addRequestFilterLast;
const charsetConvert = hop.charsetConvert;
const signal = hop.signal;
const broadcast = hop.broadcast;
const addEventListener = hop.addEventListener;
const Server = hop.Server;
const compileXML = hop.compileXML;
const createElement = hop.createElement;
const encodeURIComponent = hop.encodeURIComponent;
const encodeHTML = hop.encodeHTML;
const decodeURIComponent = hop.decodeURIComponent;
const decodeHTML = hop.decodeHTML;
const md5sum = hop.md5sum;
const sha1sum = hop.sha1sum;
const base64encode = hop.base64encode;
const base64decode = hop.base64decode;
const List = hop.List;
const Cons = hop.Cons;
const eventListenerMonitor = hop.eventListenerMonitor;

/*---------------------------------------------------------------------*/
/*    exports ...                                                      */
/*---------------------------------------------------------------------*/
module.exports = hop;
export { 
   engine,
   debug,
   isServer,
   isWorker,
   hostname,
   hostip,
   version,
   buildId,
   buildTag,
   buidArch,
   rcDirectory,
   cacheDirectory,
   port,
   ports,
   standalone,
   loginCookieCryptKey,
   Service,
   HopFrame,
   webService,
   isRecord,
   recordOf,
   HTTPResponseHop,
   HTTPResponseXml,
   HTTPResponseFile,
   HTTPResponseString,
   HTTPResponseAuthentication,
   HTTPResponseAsync,
   HTTPResponseProxy,
   HTTPResponseJson,
   HTTPResponseError,
   log,
   isLocalRequest,
   requestHostAddress,
   requestLocalAddress,
   addRequestFilter,
   addRequestFilterFirst,
   addRequestFilterLast,
   charsetConvert,
   signal,
   broadcast,
   addEventListener,
   Server,
   compileXML,
   createElement,
   encodeURIComponent,
   encodeHTML,
   decodeURIComponent,
   decodeHTML,
   md5sum,
   sha1sum,
   base64encode,
   base64decode,
   List,
   Cons,
   eventListenerMonitor
}
