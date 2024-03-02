/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/util.mod.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 28 15:16:33 2024                          */
/*    Last change :  Wed Feb 28 15:23:42 2024 (serrano)                */
/*    Copyright   :  2024 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Nodejs util API                                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Module declaration                                               */
/*---------------------------------------------------------------------*/
const util = require("util");

const format = util.format;
const deprecate = util.deprecate;
const print = util.print;
const puts = util.puts;
const debug = util.debug;
const error = util.error;
const inspect = util.inspect;
const isArray = util.isArray;
const isRegExp = util.isRegExp;
const isDate = util.isDate;
const isError = util.isError;
const log = util.log;
const exec = util.exec;
const inherit = util.inherit;
const __extend = util.__extend

/*---------------------------------------------------------------------*/
/*    exports                                                          */
/*---------------------------------------------------------------------*/
module.exports = util;
export {
   format,
   deprecate,
   print,
   puts,
   debug,
   error,
   inspect,
   isArray,
   isRegExp,
   isDate,
   isError,
   log,
   exec,
   inherit,
   __extend
}
