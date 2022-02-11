/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/record.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep  2 01:49:55 2017                          */
/*    Last change :  Fri Feb 11 14:39:32 2022 (serrano)                */
/*    Copyright   :  2017-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing HopScript sealed classes                                 */
/*=====================================================================*/
"use hopscript";

import assert from "assert";
import { ARS1, ARC1, ARSS2, ARSC2 } from "./aux/record1.js";

/*---------------------------------------------------------------------*/
/*    local constructors                                               */
/*---------------------------------------------------------------------*/
sealed class RS1 {
   x = 10;
}

sealed class RC1 {
   x = 11;
   xx;
   constructor(xx) {
      this.xx = xx;
   }
}

sealed class RSS2 extends RS1 {
   y = 20;
}

sealed class RSC2 extends RC1 {
   y = 21;
   
}

sealed class RCS2 extends RS1 {
   y = 20;
   yy;
   constructor(yy) {
      super();
      this.yy = yy;
   }
}

sealed class RSSS3 extends RSS2 {
   z = 30;
}

const rs1 = new RS1();
const rss2 = new RSS2();
const rsc2 = new RSC2(12);
const rcs2 = new RCS2(21);
const rsss3 = new RSSS3();

console.log("local constructors...");
assert.deepEqual(rs1, {x: 10}, "new RS1");
assert.deepEqual(rss2, {x: 10, y: 20}, "new RSS2");
assert.deepEqual(rsc2, {x: 11, xx: 12, y: 21}, "new RSC2");
assert.deepEqual(rcs2, {x: 10, y: 20, yy: 21}, "new RCS2");
assert.deepEqual(rsss3, {x: 10, y: 20, z: 30}, "new RSSS3");

/*---------------------------------------------------------------------*/
/*    imported constructors                                            */
/*---------------------------------------------------------------------*/
sealed class IRSS2 extends ARS1 {
   y = 200;
}

sealed class IRCS2 extends ARS1 {
   y = 200;
   yy;
   constructor(_yy) {
      super();
      this.yy = _yy;
   }
}

sealed class IRSSS3 extends ARSS2 {
   z = 300;
}

sealed class IRSSC3 extends ARSC2 {
   z = 303;
}

const irss2 = new IRSS2();
const ircs2 = new IRCS2(201);
const irsss3 = new IRSSS3();
const irssc3 = new IRSSC3(103);

console.log("imported constructors...");
assert.deepEqual(irss2, {x: 100, y: 200}, "new IRSS2");
assert.deepEqual(ircs2, {x: 100, y: 200, yy: 201}, "new IRCS2");
assert.deepEqual(irsss3, {x: 100, y: 200, z: 300}, "new IRSSS3");
assert.deepEqual(irssc3, {x: 103, xx:103, y: 203, z: 303}, "new IRSSC3");

