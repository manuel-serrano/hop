/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/nodejs/hop.js                     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar 22 15:03:30 2014                          */
/*    Last change :  Fri Oct 20 08:16:15 2017 (serrano)                */
/*    Copyright   :  2014-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hopscript/Hop binding.                                           */
/*=====================================================================*/
"use hopscript";

var hop = process.binding( "hop" );

/*---------------------------------------------------------------------*/
/*    info                                                             */
/*---------------------------------------------------------------------*/
exports.hostname = hop.hostname;
exports.version = hop.version;
exports.__defineGetter__( 'port', function() { return hop.port(); } );
exports.standalone = hop.standalone;

/*---------------------------------------------------------------------*/
/*    Services                                                         */
/*---------------------------------------------------------------------*/
exports.Service = hop.Service;
exports.HopFrame = hop.HopFrame;
exports.webService = hop.webService;

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

var jsonContentType = { "contentType": "application/json" };

exports.HTTPResponseJson = function( obj ) {
   return hop.HTTPResponseString( JSON.stringify( obj ), jsonContentType );
}

exports.HTTPResponseError = function( obj ) {
   return hop.HTTPResponseHop( obj, {
      startLine: "HTTP/1.1 500 Internal Server Error",
      header: { "Hop-Error": "true" }
   } );
};

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
exports.md5sum = hop.md5sum;
exports.sha1sum = hop.sha1sum;

/*---------------------------------------------------------------------*/
/*    Lists                                                            */
/*---------------------------------------------------------------------*/
exports.List = hop.List;
exports.Cons = hop.Cons;

/*---------------------------------------------------------------------*/
/*    applyListeners ...                                               */
/*---------------------------------------------------------------------*/
function applyListeners( listeners, e ) {
   let ltns = listeners;
   let len = ltns.length;

   for( let i = 0; i < len && true; i++ ) {
      ltns[ i ]( e );
   }
}

/*---------------------------------------------------------------------*/
/*    eventListenerMonitor                                             */
/*---------------------------------------------------------------------*/
function eventListenerMonitor( ... events ) {
   if( !(this instanceof Object) ) {
      throw "this not an object";
   }

   Object.defineProperty( this, "conlisteners", {
      configurable: false, enumerable: false, writable: true,
      value: []
   } );
   Object.defineProperty( this, "dislisteners", {
      configurable: false, enumerable: false, writable: true,
      value: []
   } );
   Object.defineProperty( this, "events", {
      configurable: false, enumerable: false, writable: true,
      value: []
   } );

   // Although the two following methods have no free variables, they
   // must be created for each monitor. If they would be shared by
   // all monitors the removeEventListener would not work properly.
   Object.defineProperty( eventListenerMonitor.prototype, "connectListener", {
      configurable: false, enumerable: false, writable: false,
      value: e => {
	 if( this.events.indexOf( e.data ) >= 0 ) {
	    applyListeners( this.conlisteners, e );
	 }
      }
   } );
   Object.defineProperty( eventListenerMonitor.prototype, "disconnectListener", {
      configurable: false, enumerable: false, writable: false,
      value: e => {
	 if( this.events.indexOf( e.data ) >= 0 ) {
	    applyListeners( this.dislisteners, e );
	 }
      }
   } );

   events.forEach( this.monitor, this );
   
   return this;
}

eventListenerMonitor.prototype.monitor = function( evname ) {
   if( this.events.indexOf( evname ) < 0 ) {
      this.events.push( evname );
      if( this.events.length == 1 ) {
	 hop.addEventListener( "connect", this.connectListener );
	 hop.addEventListener( "disconnect", this.disconnectListener );
      }
   }
}

eventListenerMonitor.prototype.ignore = function( evname ) {
   let i = this.events.indexOf( evname );
   if( i >= 0 ) {
      this.events.splice( i, 1 );
   }
   if( this.events.length == 0 ) {
      hop.removeEventListener( "connect", this.connectListener );
      hop.removeEventListener( "disconnect", this.disconnectListener );
   }
}

eventListenerMonitor.prototype.addEventListener = function( evname, ltn ) {
   switch( evname ) {
      case "newListener":
	 this.conlisteners.push( ltn );
	 break;
	 
      case "removeListener":
	 this.dislisteners.push( ltn );
	 break;
   }
}

eventListenerMonitor.prototype.removeEventListener = function( evname, ltn ) {
   switch( evname ) {
      case "newListener":
	 this.conlisteners.filter( l => l != ltn );
	 break;
	 
      case "removeListener":
	 this.dislisteners.filter( l => l != ltn );
	 break;
   }
}

exports.eventListenerMonitor = eventListenerMonitor;

/*---------------------------------------------------------------------*/
/*    compilerDriver                                                   */
/*---------------------------------------------------------------------*/
exports.compilerDriver = hop.compilerDriver;

/*---------------------------------------------------------------------*/
/*    Hop sub modules                                                  */
/*---------------------------------------------------------------------*/
exports.tree = hop.modulesDir + "/tree";
exports.notepad = hop.modulesDir + "/notepad";
exports.spage = hop.modulesDir + "/spage";
exports.fontifier = hop.modulesDir + "/fontifier";
exports.wiki = hop.modulesDir + "/wiki";
exports.security = hop.modulesDir + "/security";
exports.config = hop.modulesDir + "/config";
exports.user = hop.modulesDir + "/user";
exports.hss = hop.modulesDir + "/hss";
exports.markdown = hop.modulesDir + "/markdown";
exports.syslog = hop.modulesDir + "/syslog";
exports.systime = hop.modulesDir + "/systime";

Object.seal( exports );
Object.freeze( exports );

