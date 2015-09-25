/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/nodejs/hop.js                     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar 22 15:03:30 2014                          */
/*    Last change :  Fri Sep 25 09:25:25 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hopscript/Hop binding.                                           */
/*=====================================================================*/
var hop = process.binding( "hop" );

/*---------------------------------------------------------------------*/
/*    info                                                             */
/*---------------------------------------------------------------------*/
exports.port = hop.port;
exports.hostname = hop.hostname;
exports.version = hop.version;

/*---------------------------------------------------------------------*/
/*    withHOP                                                          */
/*---------------------------------------------------------------------*/
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

/*---------------------------------------------------------------------*/
/*    Xml                                                              */
/*---------------------------------------------------------------------*/
exports.xmlCompile = hop.xmlCompile;

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

Object.seal( exports );
Object.freeze( exports );

