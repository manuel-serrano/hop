/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/url.mod.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Nov 19 08:48:52 2023                          */
/*    Last change :  Fri Mar  8 12:06:44 2024 (serrano)                */
/*    Copyright   :  2023-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    url es6 module                                                   */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    The nodejs module                                                */
/*---------------------------------------------------------------------*/
const url = require("url");

const parse = url.parse;
const resolve = url.resolve;
const resolveObject = url.resolveObject;
const format = url.format;
const Url = url.Url;

/*---------------------------------------------------------------------*/
/*    URL ...                                                          */
/*---------------------------------------------------------------------*/
const URL = function (href) {
   const m = href.match(/([a-zA-Z]+:)\/\/(.*)/);

   this.href = href;
   if (m) {
      this.path = m[2];
      this.protocol = m[1];
   } else {
      this.path = href;
      this.protocol = "file:";
   }
}

const fileURLToPath = url.fileURLToPath;
const pathToFileURL = url.pathToFileURL; 

/*---------------------------------------------------------------------*/
/*    exports ...                                                      */
/*---------------------------------------------------------------------*/
module.exports = url;
export { 
   parse,
   resolve,
   resolveObject,
   format,
   Url,
   URL,
   fileURLToPath,
   pathToFileURL
};
   
