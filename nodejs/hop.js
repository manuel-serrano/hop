/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/nodejs/hop.js                     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar 22 15:03:30 2014                          */
/*    Last change :  Sun May 25 05:54:25 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hopscript/Hop binding.                                           */
/*=====================================================================*/
var hop = process.binding( "hop" );

/*---------------------------------------------------------------------*/
/*    exit                                                             */
/*---------------------------------------------------------------------*/
exports.exit = hop.exit;

/*---------------------------------------------------------------------*/
/*    withHOP                                                          */
/*---------------------------------------------------------------------*/
exports.withURL = hop.withURL;
exports.withHOP = hop.withHOP;

exports.currentRequest = hop.currentRequest;

/*---------------------------------------------------------------------*/
/*    Responses                                                        */
/*---------------------------------------------------------------------*/
exports.HTTPResponseHop = hop.HTTPResponseHop;
exports.HTTPResponseFile = hop.HTTPResponseFile;
exports.HTTPResponseString = hop.HTTPResponseString;
exports.HTTPResponseAuthentication = hop.HTTPResponseAuthentication;
exports.HTTPResponseAsync = hop.HTTPResponseAsync;

/*---------------------------------------------------------------------*/
/*    Charset                                                          */
/*---------------------------------------------------------------------*/
exports.charsetConvert = hop.charsetConvert;
Object.defineProperty(
   exports, "locale", {
      get: function () { return hop.locale; },
      set: function( v ) { return hop.localeSet( v ); }
   } );
Object.defineProperty(
   exports, "charset", {
      get: function () { return hop.charset; },
      set: function( v ) { return hop.charsetSet( v ); }
   } );

/*---------------------------------------------------------------------*/
/*    Events                                                           */
/*---------------------------------------------------------------------*/
exports.signal = hop.signal;
exports.broadcast = hop.broadcast;

/*---------------------------------------------------------------------*/
/*    Lib                                                              */
/*---------------------------------------------------------------------*/
exports.parseWebColor = hop.parseWebColor;
exports.makeWebColor = hop.makeWebColor;

/*---------------------------------------------------------------------*/
/*    Lists                                                            */
/*---------------------------------------------------------------------*/
exports.List = hop.List;
exports.Cons = hop.Cons;

Object.seal( exports );
Object.freeze( exports );

