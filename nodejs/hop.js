/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/nodejs/hop.js                     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar 22 15:03:30 2014                          */
/*    Last change :  Fri Apr 21 14:07:02 2017 (serrano)                */
/*    Copyright   :  2014-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hopscript/Hop binding.                                           */
/*=====================================================================*/
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

