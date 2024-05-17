/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/serv/frame.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 26 09:36:15 2015                          */
/*    Last change :  Fri May 17 08:21:29 2024 (serrano)                */
/*    Copyright   :  2015-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing HopFrames                                                */
/*=====================================================================*/
"use hopscript";

const assert = require("assert");
const srv = new hop.Server();
let res = 0;

service foo(o) {
   console.error("foo o=", o);
   return o.a + o.b;
}

const o = { a: 1, b: 2 };

const f = foo.call(srv, o);

f.post(function(v) {
   console.error("v=", v);
   assert.ok(v == 3, "post.1");
   res++;

   if(++res === 2) process.exit(0);
   
   o.a = 4;
   f.post(function(v) {
      assert.ok(v == 6, "post.2");
      if(++res === 2) process.exit(0);
   })
});
   
setTimeout(function() {
   try {
      if(hop.compilerDriver.pending > 0) {
	 hop.compilerDriver.addEventListener("all", function(e) {
	    assert.ok(res === 2, "res (after compile)");
	 });
      } else {
	 assert.ok(res === 2, "res");
      }
   } finally {
      process.exit(res === 2 ? 0 : 1);
   }
}, 1500);
  





