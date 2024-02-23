/*=====================================================================*/
/*    .../prgm/project/hop/hop/nodejs/modules/fs_promises.mod.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Feb 20 20:10:40 2023                          */
/*    Last change :  Fri Feb 23 09:17:11 2024 (serrano)                */
/*    Copyright   :  2023-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    fs.js es6 module                                                 */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    The nodejs module                                                */
/*---------------------------------------------------------------------*/
const fs = require("node:fs/promises");

const appendFile = fs.appendFile;
const lstat = fs.lstat;
const readdir = fs.readdir;
const readFile = fs.readFile;
const rename = fs.rename;
const writeFile = fs.writeFile;

/*---------------------------------------------------------------------*/
/*    exports ...                                                      */
/*---------------------------------------------------------------------*/
module.exports = fs;
export {
   appendFile,
   lstat,
   readdir,
   readFile,
   rename,
   writeFile
};
