/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/hop.js                       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar 22 15:03:30 2014                          */
/*    Last change :  Tue May  9 08:24:52 2023 (serrano)                */
/*    Copyright   :  2014-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hopscript/Hop binding.                                           */
/*=====================================================================*/
"use hopscript";

const hop = process.binding("hop");

/*---------------------------------------------------------------------*/
/*    ES6 Module export                                                */
/*---------------------------------------------------------------------*/
export { Generic };

/*---------------------------------------------------------------------*/
/*    info                                                             */
/*---------------------------------------------------------------------*/
exports.engine = hop.engine;
exports.debug = hop.debug;
exports.isServer = hop.isServer;
exports.isWorker = hop.isWorker;
exports.hostname = hop.hostname;
exports.hostip = hop.hostip;
exports.version = hop.version;
exports.buildId = hop.buildId;
exports.buildTag = hop.buildTag;
exports.buildArch = hop.buildArch;
exports.rcDirectory = hop.rcDir;
exports.cacheDirectory = hop.cacheDir;
exports.__defineGetter__('port', hop.port);
exports.__defineGetter__('ports', hop.ports);
exports.standalone = hop.standalone;
exports.loginCookieCryptKey = hop.loginCookieCryptKey;

/*---------------------------------------------------------------------*/
/*    Server configuration                                             */
/*---------------------------------------------------------------------*/
exports.__defineGetter__('loadPath', 
   hop.loadPathGet,
   hop.loadPathSet);
   
exports.__defineGetter__('httpAuthenticationMethod',
   hop.httpAuthenticationMethodGet,
   hop.httpAuthenticationMethodSet);

/*---------------------------------------------------------------------*/
/*    Services                                                         */
/*---------------------------------------------------------------------*/
exports.Service = hop.Service;
exports.HopFrame = hop.HopFrame;
exports.webService = hop.webService;

/*---------------------------------------------------------------------*/
/*    records                                                          */
/*---------------------------------------------------------------------*/
exports.isRecord = hop.isRecord;
exports.recordOf = hop.recordOf;

/*---------------------------------------------------------------------*/
/*    Responses                                                        */
/*---------------------------------------------------------------------*/
exports.HTTPResponseHop = hop.HTTPResponseHop;
exports.HTTPResponseXml = hop.HTTPResponseXml;
exports.HTTPResponseFile = hop.HTTPResponseFile;
exports.HTTPResponseString = hop.HTTPResponseString;
exports.HTTPResponseAuthentication = hop.HTTPResponseAuthentication;
exports.HTTPResponseAsync = hop.HTTPResponseAsync;
exports.HTTPResponseProxy = hop.HTTPResponseProxy;

const jsonContentType = { "contentType": "application/json" };

exports.HTTPResponseJson = function(obj) {
   return hop.HTTPResponseString(JSON.stringify(obj), jsonContentType);
}

exports.HTTPResponseError = function(obj) {
   return hop.HTTPResponseHop(obj, {
      startLine: "HTTP/1.1 500 Internal Server Error",
      header: { "Hop-Error": "true" }
   });
};

/*---------------------------------------------------------------------*/
/*    debug                                                            */
/*---------------------------------------------------------------------*/
exports.log = hop.log;
exports.tof = hop.tof;
exports.gc = hop.gc;

/*---------------------------------------------------------------------*/
/*    Requests                                                         */
/*---------------------------------------------------------------------*/
exports.isLocalRequest = hop.isLocalRequest;
exports.requestHostAddress = hop.requestHostAddr;
exports.requestLocalAddress = hop.requestLocalAddr;

/*---------------------------------------------------------------------*/
/*    Request Filters                                                  */
/*---------------------------------------------------------------------*/
exports.addRequestFilter = hop.addRequestFilter;
exports.addRequestFilterFirst = hop.addRequestFilterFirst;
exports.addRequestFilterLast = hop.addRequestFilterLast;

/*---------------------------------------------------------------------*/
/*    Charset                                                          */
/*---------------------------------------------------------------------*/
exports.charsetConvert = hop.charsetConvert;

/*---------------------------------------------------------------------*/
/*    Events                                                           */
/*---------------------------------------------------------------------*/
exports.signal = hop.signal;
exports.broadcast = hop.broadcast;
exports.addEventListener = hop.addEventListener;
exports.Server = hop.Server;

/*---------------------------------------------------------------------*/
/*    XML                                                              */
/*---------------------------------------------------------------------*/
exports.compileXML = hop.compileXML;
exports.createElement = hop.createElement;

/*---------------------------------------------------------------------*/
/*    Lib                                                              */
/*---------------------------------------------------------------------*/
exports.encodeURIComponent = hop.encodeURIComponent;
exports.encodeHTML = hop.encodeHTML;
exports.decodeURIComponent = hop.decodeURIComponent;
exports.decodeHTML = hop.decodeHTML;
exports.md5sum = hop.md5sum;
exports.sha1sum = hop.sha1sum;
exports.base64encode = hop.base64encode;
exports.base64decode = hop.base64decode;

/*---------------------------------------------------------------------*/
/*    Lists                                                            */
/*---------------------------------------------------------------------*/
exports.List = hop.List;
exports.Cons = hop.Cons;

/*---------------------------------------------------------------------*/
/*    applyListeners ...                                               */
/*---------------------------------------------------------------------*/
function applyListeners(listeners, e) {
   let ltns = listeners;
   let len = ltns.length;

   for (let i = 0; i < len && true; i++) {
      ltns[ i ](e);
   }
}

/*---------------------------------------------------------------------*/
/*    eventListenerMonitor                                             */
/*---------------------------------------------------------------------*/
function eventListenerMonitor(... events) {
   if (!(this instanceof Object)) {
      throw "this not an object";
   }

   Object.defineProperty(this, "conlisteners", {
      configurable: false, enumerable: false, writable: true,
      value: []
   });
   Object.defineProperty(this, "dislisteners", {
      configurable: false, enumerable: false, writable: true,
      value: []
   });
   Object.defineProperty(this, "events", {
      configurable: false, enumerable: false, writable: true,
      value: []
   });

   // Although the two following methods have no free variables, they
   // must be created for each monitor. If they would be shared by
   // all monitors the removeEventListener would not work properly.
   Object.defineProperty(this, "connectListener", {
      configurable: false, enumerable: false, writable: false,
      value: e => {
	 if (this.events.indexOf(e.data) >= 0) {
	    applyListeners(this.conlisteners, e);
	 }
      }
   });
   Object.defineProperty(this, "disconnectListener", {
      configurable: false, enumerable: false, writable: false,
      value: e => {
	 if (this.events.indexOf(e.data) >= 0) {
	    applyListeners(this.dislisteners, e);
	 }
      }
   });

   events.forEach(this.monitor, this);

   return this;
}

eventListenerMonitor.prototype.monitor = function(evname) {
   if (this.events.indexOf(evname) < 0) {
      this.events.push(evname);
      if (this.events.length === 1) {
	 hop.addEventListener("connect", this.connectListener);
	 hop.addEventListener("disconnect", this.disconnectListener);
      }
   }
}

eventListenerMonitor.prototype.ignore = function(evname) {
   let i = this.events.indexOf(evname);
   if (i >= 0) {
      this.events.splice(i, 1);
   }
   if (this.events.length === 0) {
      hop.removeEventListener("connect", this.connectListener);
      hop.removeEventListener("disconnect", this.disconnectListener);
   }
}

eventListenerMonitor.prototype.addEventListener = function(evname, ltn) {
   switch(evname) {
      case "newListener":
	 this.conlisteners.push(ltn);
	 break;

      case "removeListener":
	 this.dislisteners.push(ltn);
	 break;
   }
}

eventListenerMonitor.prototype.removeEventListener = function(evname, ltn) {
   switch(evname) {
      case "newListener":
	 this.conlisteners.filter(l => l != ltn);
	 break;

      case "removeListener":
	 this.dislisteners.filter(l => l != ltn);
	 break;
   }
}

exports.eventListenerMonitor = eventListenerMonitor;

/*---------------------------------------------------------------------*/
/*    Generic ...                                                      */
/*---------------------------------------------------------------------*/
function Generic(root, body) {
   if (root !== undefined && !hop.isRecord(root)) {
      throw `generic: "${root}" is not a record`;
   }
   if (!(typeof body === "function")) {
      throw `generic: "${body}" is not a function`;
   }
   this.root = root;
   this.body = body;
   this.methods = [];
}

Generic.prototype.__proto__ = Generic.__proto__;

Generic.prototype.addMethod = function(rec, met) {
   if (!hop.isRecord(rec)) {
      throw `method: "${rec}" is not a record`;
   }
   if (!(typeof met === "function")) {
      throw `method: "${met}" is not a function`;
   }
   this.methods.push(hop.recordFrom(rec), met);
}
   
Generic.prototype.dispatch = function(rec) {
   const i = this.methods.indexOf(hop.recordFrom(rec));
   return i >= 0 ? this.methods[i+1] : this.body;
}
				   
Generic.prototype.dispatchValue = function(obj) {
   const i = this.methods.indexOf(hop.recordOf(obj));
   return i >= 0 ? this.methods[i+1] : this.body;
}

exports.Generic = Generic;

/*---------------------------------------------------------------------*/
/*    compilerDriver                                                   */
/*---------------------------------------------------------------------*/
exports.compilerDriver = hop.compilerDriver;

/*---------------------------------------------------------------------*/
/*    Hop sub modules                                                  */
/*---------------------------------------------------------------------*/
exports.config = hop.modulesDir + "/config";
exports.csv = hop.modulesDir + "/csv";
exports.feed = hop.modulesDir + "/feed";
exports.fontifier = hop.modulesDir + "/fontifier";
exports.hss = hop.modulesDir + "/hss";
exports.hopc = hop.modulesDir + "/hopc";
exports.hopdroid = hop.modulesDir + "/hopdroid";
exports.markdown = hop.modulesDir + "/markdown";
exports.notepad = hop.modulesDir + "/notepad";
exports.security = hop.modulesDir + "/security";
exports.spage = hop.modulesDir + "/spage";
exports.syslog = hop.modulesDir + "/syslog";
exports.system = hop.modulesDir + "/system";
exports.systime = hop.modulesDir + "/systime";
exports.texinfo = hop.modulesDir + "/texinfo";
exports.tree = hop.modulesDir + "/tree";
exports.ts = hop.modulesDir + "/ts";
exports.user = hop.modulesDir + "/user";
exports.utils = hop.modulesDir + "/utils";
exports.vcf = hop.modulesDir + "/vcf";
exports.openpgp = hop.modulesDir + "/openpgp";
exports.wiki = hop.modulesDir + "/wiki";
exports.xml = hop.modulesDir + "/xml";

Object.seal(exports);
Object.freeze(exports);
