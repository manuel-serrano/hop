/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/url.mod.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Nov 19 08:48:52 2023                          */
/*    Last change :  Thu May 16 17:14:24 2024 (serrano)                */
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
const URL = url.URL;

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
   
