/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/es6-async.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue May 15 11:33:27 2018                          */
/*    Last change :  Wed Apr 27 11:40:40 2022 (serrano)                */
/*    Copyright   :  2018-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 async functions                           */
/*=====================================================================*/
"use strict";

var assert = require("assert");

/*---------------------------------------------------------------------*/
/*    async keyword                                                    */
/*---------------------------------------------------------------------*/
function bg() {
   return new Promise(function(resolve, reject) {
      setTimeout(function() { resolve(10) }, 0);
   });
}

async function async() {
   var async = await bg();

   return async == 10;
}

assert.ok(async(), "async");

/*---------------------------------------------------------------------*/
/*    await                                                            */
/*---------------------------------------------------------------------*/
function resolveAfter2Seconds(x) {
   return new Promise(resolve => {
      setTimeout(() => { resolve(x) }, 200);
   });
}

async function asyncAwait(x) {
   var a = resolveAfter2Seconds(20);
   return await a + 20;
}

async function asyncYield(x) {
   var a = resolveAfter2Seconds(20);
   return yield a + 20;
}

asyncAwait(10).then(v => { assert.ok(v, 40, "await parsing") });
asyncYield(10).then(v => { assert.ok(typeof v, "string", "yield parsing") });

/*---------------------------------------------------------------------*/
/*    this                                                             */
/*---------------------------------------------------------------------*/
function asyncCtor() {
   this.myvar = "hello";
}

asyncCtor.prototype.test = async function() {
   assert.ok(this.myvar === "hello", "async this");
   return this.myvar;
} 

new asyncCtor().test();

function asyncCtorReturn(x) {
   if (x > 10) return;
   this.myvar = "hello";
}

asyncCtorReturn.prototype.test = async function() {
   assert.ok(this.myvar === "hello", "async this with return");
   return this.myvar;
} 

new asyncCtorReturn().test();

/*---------------------------------------------------------------------*/
/*    boolean expressions                                              */
/*---------------------------------------------------------------------*/
async function B(v, res) {
   let cnt = 0;
   
   function G() {
      cnt++;
      return new Promise((res, rej) => res(v));
   }

   function H() {
      cnt++;
      return new Promise((res, rej) => res(v));
   }
   
   const x = await G() || await H();
   return cnt == res;
}

B(true, 1).then(v => { if (!v) { console.log("error, boolean true"); process.exit(1);}});
B(false, 2).then(v => { if (!v) { console.log("error, boolean false"); process.exit(1)}});

/*---------------------------------------------------------------------*/
/*    expressions                                                      */
/*---------------------------------------------------------------------*/
let ares;
const ff = async function() { ares = 23 };
const hf = async function(h) { ares = (h === 55) };
const fa = async () => ares = 23;
const ha = async (h) => ares = (h === 55);
const ha2 = async (h,i) => ares = (h === 55); 
const ha3 = async h => ares = (h === 55);

assert.ok(ff() instanceof Promise, "async function expression");
assert.ok(ares, 23, "async function expression");
assert.ok(hf(55) instanceof Promise, "async function expression");
assert.ok(ares, true, "async function expression");
assert.ok(fa() instanceof Promise, "async function expression");
assert.ok(ares, 23, "async function expression");
assert.ok(ha(55) instanceof Promise, "async function expression");
assert.ok(ares, true, "async function expression");
assert.ok(ha2(55, 3) instanceof Promise, "async function expression"); 
assert.ok(ares, true, "async function expression");
assert.ok(ha3(55) instanceof Promise, "async function expression"); 
assert.ok(ares, true, "async function expression");
	  
