/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/module.mod.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 23 17:10:16 2024                          */
/*    Last change :  Thu Mar 21 15:50:18 2024 (serrano)                */
/*    Copyright   :  2024 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Nodejs module API                                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Module declaration                                               */
/*---------------------------------------------------------------------*/
const url = require("url");
export { createRequire };

/*---------------------------------------------------------------------*/
/*    createRequire ...                                                */
/*---------------------------------------------------------------------*/
function createRequire(url) {
   const base = typeof url === "string" ? url : url.fileURLToPath(url);
   const mod = {
      id: base,
      exports: {},
      filename: base,
      loaded: false,
      parent: null,
      children: [],
      paths: #:nodejs-file-paths(base, globalThis)
   }
   return #:nodejs-require(#:current-thread(), globalThis, mod, #:js-jsstring->string("hopscript"));
}
