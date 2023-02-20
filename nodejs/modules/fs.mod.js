/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/fs.mod.js                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Feb 20 20:10:40 2023                          */
/*    Last change :  Mon Feb 20 20:15:26 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    The exports of fs.js                                             */
/*=====================================================================*/
"use hopscript";

// hopc -Ox -c --js-commonjs-export -t fs.mod.scm fs.mod.js --js-no-header --js-module-name __hop_js --js-module-path _fs --js-worker-slave --bootstrap-mode
// hopc fs.mod.js --js-driver j2s-export-driver -v2 -t fs.mod.obj -s

/*---------------------------------------------------------------------*/
/*    The nodejs module                                                */
/*---------------------------------------------------------------------*/
const fs = require("fs");

const lstatSync = fs.lstatSync;
const lstat = fs.lstat;

/*---------------------------------------------------------------------*/
/*    The ES6 export                                                   */
/*---------------------------------------------------------------------*/
export { lstat, lstatSync };
