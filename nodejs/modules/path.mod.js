/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/path.mod.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Feb 21 09:25:49 2023                          */
/*    Last change :  Tue Feb 21 09:28:29 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    path.js es6 module                                               */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    The nodejs module                                                */
/*---------------------------------------------------------------------*/
const path = require("path");

const resolve = path.resolve;
const normalize = path.normalize;
const join = path.join;
const relative = path.relative;
const sep = path.sep;
const delimiter = path.delimiter;
const dirname = path.dirname;
const basename = path.basename;
const extname = path.extname;
const exists = path.exists;
const existsSync = path.existsSync;
const _makeLong = path._makeLong;
const isAbsolute = path.isAbsolute;

/*---------------------------------------------------------------------*/
/*    exports ...                                                      */
/*---------------------------------------------------------------------*/
module.exports = path;
export { 
   resolve, 
   normalize,
   join, 
   relative,
   sep,
   delimiter, 
   dirname, 
   basename,
   extname, 
   exists,
   existsSync, 
   _makeLong,
   isAbsolute
   };
   

