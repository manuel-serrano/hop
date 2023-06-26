/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/es2020.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 30 17:54:33 2015                          */
/*    Last change :  Mon Jun 26 08:22:04 2023 (serrano)                */
/*    Copyright   :  2015-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2020 features                                 */
/*=====================================================================*/
"use strict";

var assert = require("assert");

/*---------------------------------------------------------------------*/
/*    chaining operator                                                */
/*---------------------------------------------------------------------*/
function chainingKangaxA() {
   var foo = { baz: 42 };
   var bar = null;
   return foo?.baz === 42 && bar?.baz === void undefined;
}

function chainingKangaxB() {
   var foo = { baz: 42 };
   var bar = null;
   return foo?.['baz'] === 42 && bar?.['baz'] === void undefined;
}

function chainingKangaxC() {
   var foo = { baz: function () { return this.value; }, value: 42 };
   var bar = null;
   return foo?.baz() === 42 && bar?.baz() === void undefined;
}

function chainingKangaxD() {
   var foo = { baz: function () { return 42; } };
   var bar = {};
   function baz() { return 42; };
   var n;
   return foo.baz?.() === 42 && bar.baz?.() === void undefined && baz?.() === 42 && n?.() === void undefined;
}

function chainingKangaxE() {
   var fn = null;
   var n = null;
   var o = {};

   return fn?.(...[], 1) === void undefined && fn?.(...[], ...[]) === void undefined && o.method?.(...[], 1) === void undefined && n?.method(...[], 1) === void undefined;
}

function chainingMdnA() {
   let myMap = new Map();
   myMap.set("foo", {name: "baz", desc: "inga"});

   return myMap.get("bar")?.name === undefined;
}

function chainingMdnB() {
   let potentiallyNullObj = null;
   let x = 0;
   let prop = potentiallyNullObj?.[x++];

   return x === 0; // 0 as x was not incremented
}

function chainingMdnC() {
   let customer = {
      name: "Carl",
      details: {
    	 age: 82,
    	 location: "Paradise Falls" // detailed address is unknown
      }
   };
   let customerCity = customer.details?.address?.city;

   // this also works with optional chaining function call
   return customer.name?.getName?.() === undefined;
}

function chainingMdnD() {
   let customer = {
      name: "Carl",
      details: { age: 82 }
   };
   const customerCity = customer?.city ?? "Unknown city";
   return (customerCity === "Unknown city");
}

function chainingMiscA() {
   var x = {}
   var foo = { baz: function () { return this.value; }, value: 42 };
   var bar = null;
   return x.foo?.baz(555) === undefined;
}

console.log("chaining");
console.log("   KangaxA...");
assert.equal(chainingKangaxA(), true, "chainingKangaxA");
console.log("   KangaxB...");
assert.equal(chainingKangaxB(), true, "chainingKangaxB");
console.log("   KangaxC...");
assert.equal(chainingKangaxC(), true, "chainingKangaxC");
console.log("   KangaxD...");
assert.equal(chainingKangaxD(), true, "chainingKangaxD");
console.log("   KangaxE..." );
assert.equal(chainingKangaxE(), true, "chainingKangaxE");

console.log("   MdnA...");
assert.equal(chainingMdnA(), true, "chainingMdnA");
console.log("   MdnB...");
assert.equal(chainingMdnB(), true, "chainingMdnB");
console.log("   MdnC...");
assert.equal(chainingMdnC(), true, "chainingMdnC");
console.log("   MdnD...");
assert.equal(chainingMdnD(), true, "chainingMdnD");

console.log("   MiscA...");
assert.equal(chainingMiscA(), true, "chainingMdnD");

/*---------------------------------------------------------------------*/
/*    nullish                                                          */
/*---------------------------------------------------------------------*/
function nullishKangax() {
   return (null ?? 42) === 42 &&
      (undefined ?? 42) === 42 &&
      (false ?? 42) === false &&
      ('' ?? 42) === '' &&
      (0 ?? 42) === 0 &&
      isNaN(NaN ?? 42);
}

function nullishMdnA() {
   const nullValue = null;
   const emptyText = ""; // falsy
   const someNumber = 42;

   const valA = nullValue ?? "default for A";
   const valB = emptyText ?? "default for B";
   const valC = someNumber ?? 0;
	
   return (valA === "default for A")
      && (valB === "")
      && (valC === 42);
}

function nullishMdnB() {
   let myText = ''; // An empty string (which is also a falsy value)

   let notFalsyText = myText || 'Hello world';

   let preservingFalsy = myText ?? 'Hi neighborhood';
   
   return notFalsyText === "Hello world"
      && preservingFalsy === "";
}  

function nullishMdnC() {
   let stdout = "";
   function A() { stdout += ('A was called'); return undefined;}
   function B() { stdout += ('B was called'); return false;}
   function C() { stdout += ('C was called'); return "foo";}

   stdout = "";
   const valA = A() ?? C();
   const stdoutA = stdout;
   // logs "A was called" then "C was called" and then "foo"
   // as A() returned undefined so both expressions are evaluated

   stdout = "";
   const valB = B() ?? C();
   const stdoutB = stdout;
   // logs "B was called" then "false"
   // as B() returned false (and not null or undefined), the right
   // hand side expression was not evaluated
   
   return valA === "foo" && stdoutA === "A was calledC was called"
      && valB === false && stdoutB === "B was called";
}

function nullishMdnD() {
   try {
      eval("null || undefined ?? 'foo'"); // raises a SyntaxError
      return false;
   } catch(e) {
      ;
   }
   try {
      eval("true || undefined ?? 'foo'"); // raises a SyntaxError
      return false;
   } catch(e) {
      ;
   }
   const val = (null || undefined) ?? "foo"; // returns "foo"
   return val === "foo";
}

function nullishMdnE(options) {
  options.duration ??= 100;
  options.speed ??= 25;
  return options;
}

console.log("nullish");
console.log("   Kangax...");
assert.equal(nullishKangax(), true, "nullishKangax");

console.log("   MdnA...");
assert.equal(nullishMdnA(), true, "nullishMdnA");
console.log("   MdnB...");
assert.equal(nullishMdnB(), true, "nullishMdnB");
console.log("   MdnC...");
assert.equal(nullishMdnC(), true, "nullishMdnC");
console.log("   MdnD...");
assert.equal(nullishMdnD(), true, "nullishMdnD");
console.log("   MdnE...");
assert.equal(nullishMdnE({ duration: 125 }).speed, 25, "nullishMdnEa");
assert.equal(nullishMdnE({ duration: 125 }).duration, 125, "nullishMdnEb");
assert.equal(nullishMdnE({}).duration, 100, "nullishMdnEc");
assert.equal(nullishMdnE({}).speed, 25, "nullishMdnEd");






