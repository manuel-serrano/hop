/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/fs_promises.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 23 08:17:55 2024                          */
/*    Last change :  Fri Feb 23 08:23:16 2024 (serrano)                */
/*    Copyright   :  2024 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    FS promise wrappers                                              */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    import                                                           */
/*---------------------------------------------------------------------*/
const fs = require("fs");

exports.appendFile = function(path, data, options) {
   return new Promise((res, rej) => {
      fs.appendFile(path, data, options, err => err ? rej(err) : res(true));
   });
}
