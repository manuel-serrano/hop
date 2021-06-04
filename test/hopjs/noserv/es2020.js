/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/es2020.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 30 17:54:33 2015                          */
/*    Last change :  Fri Jun  4 13:38:00 2021 (serrano)                */
/*    Copyright   :  2015-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2020 features                                 */
/*=====================================================================*/
"use strict";

var assert = require("assert");

/*---------------------------------------------------------------------*/
/*    kangaxNullish                                                    */
/*---------------------------------------------------------------------*/
function kangaxNullish() {
   return (null ?? 42) === 42 &&
      (undefined ?? 42) === 42 &&
      (false ?? 42) === false &&
      ('' ?? 42) === '' &&
      (0 ?? 42) === 0 &&
      isNaN(NaN ?? 42);
}

console.log("   kangaxNullish()");
assert.equal(kangaxNullish(), true, "kangaxNullish");




