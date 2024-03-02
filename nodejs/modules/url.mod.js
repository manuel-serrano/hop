/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/url.mod.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Nov 19 08:48:52 2023                          */
/*    Last change :  Thu Feb 29 11:44:55 2024 (serrano)                */
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
const URL = url.Url;

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
   
