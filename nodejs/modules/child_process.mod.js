/*=====================================================================*/
/*    .../prgm/project/hop/hop/nodejs/modules/child_process.mod.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Nov 19 08:48:52 2023                          */
/*    Last change :  Thu May 16 19:50:06 2024 (serrano)                */
/*    Copyright   :  2023-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    CHILD_PROCESS module                                             */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    The nodejs module                                                */
/*---------------------------------------------------------------------*/
const child_process = require("child_process");

const fork = child_process.fork;
const exec = child_process.exec;
const execFile = child_process.execFile;
const spawn = child_process.spawn;

/*---------------------------------------------------------------------*/
/*    exports ...                                                      */
/*---------------------------------------------------------------------*/
module.exports = child_process;
export {
   fork,
   exec,
   execFile,
   spawn
};
   
