/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/nodejs/hop.js                     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar 22 15:03:30 2014                          */
/*    Last change :  Sat Sep 12 08:43:18 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hopscript/Hop binding.                                           */
/*=====================================================================*/
var hop = process.binding( "hop" );

/*---------------------------------------------------------------------*/
/*    config                                                           */
/*---------------------------------------------------------------------*/
/* exports.shareDir = hop.shareDir;                                    */
/* exports.binDir = hop.binDir;                                        */
/* exports.libDir = hop.libDir;                                        */
/* exports.contribsDir = hop.contribsDir;                              */
/* exports.weblestDir = hop.webletsDir;                                */
/* exports.modulesDir = hop.modulesDir;                                */
/*                                                                     */
/* Object.defineProperty( exports, "debug", {                          */
/*    get: function() { return hop.debug() },                          */
/*    set: function( v ) { return hop.debugSet( v ) },                 */
/*    enumerable: true,                                                */
/*    configurable: false                                              */
/* } );                                                                */
/*                                                                     */
/* Object.defineProperty( exports, "preferredLanguage", {              */
/*    get: function() { return hop.preferredLanguage() },              */
/*    set: function( v ) { return hop.preferredLanguageSet( v ) },     */
/*    enumerable: true,                                                */
/*    configurable: false                                              */
/* } );                                                                */

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
/* exports.srcDir = hop.srcDir;                                        */
/* exports.srcFile = hop.srcFile;                                      */
/*                                                                     */
/* exports.currentThread = hop.currentThread;                          */

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
/* exports.withURL = hop.withURL;                                      */

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

/*---------------------------------------------------------------------*/
/*    Charset                                                          */
/*---------------------------------------------------------------------*/
exports.charsetConvert = hop.charsetConvert;
/* Object.defineProperty(                                              */
/*    exports, "locale", {                                             */
/*       get: function () { return hop.locale; },                      */
/*       set: function( v ) { return hop.localeSet( v ); }             */
/*    } );                                                             */
/* Object.defineProperty(                                              */
/*    exports, "charset", {                                            */
/*       get: function () { return hop.charset; },                     */
/*       set: function( v ) { return hop.charsetSet( v ); }            */
/*    } );                                                             */

/*---------------------------------------------------------------------*/
/*    Events                                                           */
/*---------------------------------------------------------------------*/
exports.signal = hop.signal;
exports.broadcast = hop.broadcast;

/*---------------------------------------------------------------------*/
/*    Xml                                                              */
/*---------------------------------------------------------------------*/
exports.XMLCompile = hop.XMLCompile;

/*---------------------------------------------------------------------*/
/*    Lib                                                              */
/*---------------------------------------------------------------------*/
exports.encodeURIComponent = hop.encodeURIComponent;
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

