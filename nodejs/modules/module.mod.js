/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/module.mod.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 23 17:10:16 2024                          */
/*    Last change :  Fri Feb 23 17:40:07 2024 (serrano)                */
/*    Copyright   :  2024 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Nodejs module API                                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Module declaration                                               */
/*---------------------------------------------------------------------*/
import { fileURLToPath, pathToFileURL } from 'node:url';
export { createRequire };

/*---------------------------------------------------------------------*/
/*    createRequire ...                                                */
/*---------------------------------------------------------------------*/
function createRequire(url) {
   const base = fileURLToPath(url);
   return #:nodejs-require(#:current-thread(), globalThis, module, #:js-jsstring->string("hopscript"));
}
