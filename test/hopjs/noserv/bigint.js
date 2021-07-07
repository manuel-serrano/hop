/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/bigint.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 30 17:54:33 2015                          */
/*    Last change :  Wed Jul  7 07:30:57 2021 (serrano)                */
/*    Copyright   :  2015-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript bigint                                        */
/*=====================================================================*/
"use strict";

var assert = require("assert");

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
console.log("misc");

assert.equal(0n.toString(), "0", "toString");
assert.ok(0n == 0, "==");
assert.ok(1n == 1, "==");

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
console.log("mdn");

// constructor
function mdnConstructor() {
   console.log("   constructor...");

   const previouslyMaxSafeInteger = 9007199254740991n;

   const alsoHuge = BigInt(9007199254740991);
   assert.equal(alsoHuge, 9007199254740991n, "alsoHuge");

   const hugeString = BigInt("9007199254740991");
   assert.equal(alsoHuge, 9007199254740991n, "hugeString");

   const hugeHex = BigInt("0x1fffffffffffff");
   assert.equal(alsoHuge, 9007199254740991n), "hugeHex";

   const hugeOctal = BigInt("0o377777777777777777");
   assert.equal(alsoHuge, 9007199254740991n, "hugeOctal");

   const hugeBin = BigInt("0b11111111111111111111111111111111111111111111111111111");
   assert.equal(alsoHuge, 9007199254740991n, "hugeBin");
}

mdnConstructor();
   
// typeof
function mdnTypeof() {
   console.log("   typeof...");

   assert.equal(typeof 1n, "bigint");
   assert.equal(typeof BigInt('1'), 'bigint');

   assert.equal(typeof Object(1n), 'object');
}

mdnTypeof();

// operators
function mdnOperator() {
   console.log("   operator...");
   
   const previousMaxSafe = BigInt(Number.MAX_SAFE_INTEGER);
   assert.equal(previousMaxSafe, 9007199254740991n);

   const maxPlusOne = previousMaxSafe + 1n;
   assert.equal(maxPlusOne,9007199254740992n);

   const theFuture = previousMaxSafe + 2n;
   assert.equal(theFuture, 9007199254740993n);

   const multi = previousMaxSafe * 2n;
   assert.equal(multi,18014398509481982n);

   const subtr = multi - 10n;
   assert.equal(subtr, 18014398509481972n);

   const mod = multi % 10n;
   assert.equal(mod, 2n); 

   const bigN = 2n ** 54n;
   assert.equal(bigN, 18014398509481984n);

   assert.equal(bigN * -1n, -18014398509481984n);

   const expected = 4n / 2n;
   assert.equal(expected, 2n);

   const truncated = 5n / 2n;
   assert.equal(truncated, 2n);
}

mdnOperator();

// comparison
function mdnComparison() {
   console.log("   comparison...");

   assert.equal(0n === 0, false);
   assert.equal(0n == 0, true);

   assert.equal(1n < 2, true);
   assert.equal(2n > 1, true);
   assert.equal(2 > 2, false);
   assert.equal(2n > 2, false);
   assert.equal(2n >= 2, true);

   const mixed = [4n, 6, -12n, 10, 4, 0, 0n];
   assert.deepEqual(mixed, [4n, 6, -12n, 10, 4, 0, 0n], "mixed");

   mixed.sort(); // default sorting behavior
   assert.deepEqual(mixed.map(BigInt), [-12n, 0n, 0n, 10n, 4n, 4n, 6n], "sort");

   assert.throws(() => {
	 mixed.sort((a, b) => a - b);
	 // won't work since subtraction will not work with mixed types
	 // TypeError: can't convert BigInt value to Number value
      });

   // sort with an appropriate numeric comparator
   mixed.sort((a, b) => (a < b) ? -1 : ((a > b) ? 1 : 0));
   assert.deepEqual(mixed.map(BigInt), [-12n, 0n, 0n, 4n, 4n, 6n, 10n]);
   
   assert.equal(0n === Object(0n), false);
   assert.equal(Object(0n) === Object(0n), false);

   const o = Object(0n);
   assert.equal(o === o, true);
}

mdnComparison();
   
// conditional
function mdnConditional() {
   console.log("   conditional...");
   
   if (0n) {
      throw 'Hello from the if!';
   }

   assert.equal(0n || 12n, 12n);

   assert.equal(0n && 12n, 0n);

   assert.equal(Boolean(0n), false);

   assert.equal(Boolean(12n), true);

   assert.equal(!12n, false);

   assert.equal(!0n, true);
}

mdnConditional();
   
// prime
function mdnPrime() {
// Returns true if the passed BigInt value is a prime number
   function isPrime(p) {
      for (let i = 2n; i * i <= p; i++) {
	 if (p % i === 0n) return false;
      }
      return true
	 }

// Takes a BigInt value as an argument, returns nth prime number as a BigInt value
   function nthPrime(nth) {
      let maybePrime = 2n
	 let prime = 0n

	 while (nth >= 0n) {
	    if (isPrime(maybePrime)) {
	       nth--
	       prime = maybePrime
	    }
	    maybePrime++
	 }

      return prime
	 }

   console.log("   prime...");
   return nthPrime(20n);
}

assert.equal(mdnPrime(), 73n);

// clamp
function mdnClamp() {
   const maxu32 = 2n ** 64n - 1n;
   const maxs32 = 2n ** (64n - 1n) - 1n;
   
   console.log("   clamp...");
   assert.equal(BigInt.asUintN(64, maxu32), 18446744073709551615n);
   assert.equal(BigInt.asUintN(64, maxu32 + 1n), 0n);
   assert.equal(BigInt.asIntN(64, maxs32), 9223372036854775807n);
   assert.equal(BigInt.asIntN(64, maxs32 + 1n), -9223372036854775808n);
}

mdnClamp();

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*---------------------------------------------------------------------*/
function kangaxa() {
   return (1n + 2n) === 3n;
}

function kangaxb() {
   return BigInt("3") === 3n;
}

function kangaxc() {
   return typeof BigInt.asUintN === 'function';
}

function kangaxd() {
   return typeof BigInt.asIntN === 'function';
}

/* function kangaxe() {                                                */
/*    var buffer = new ArrayBuffer(64);                                */
/*    var view = new BigInt64Array(buffer);                            */
/*    view[0] = 0x8000000000000000n;                                   */
/*    return view[0] === -0x8000000000000000n;                         */
/* }                                                                   */
/*                                                                     */
/* function kangaxf() {                                                */
/*    var buffer = new ArrayBuffer(64);                                */
/*    var view = new BigUint64Array(buffer);                           */
/*    view[0] = 0x10000000000000000n;                                  */
/*    return view[0] === 0n;                                           */
/* }                                                                   */
/*                                                                     */
/* function kangaxg() {                                                */
/*    var buffer = new ArrayBuffer(64);                                */
/*    var view = new DataView(buffer);                                 */
/*    view.setBigInt64(0, 1n);                                         */
/*    return view.getBigInt64(0) === 1n;                               */
/* }                                                                   */
/*                                                                     */
/* function kangaxh() {                                                */
/*    var buffer = new ArrayBuffer(64);                                */
/*    var view = new DataView(buffer);                                 */
/*    view.setBigUint64(0, 1n);                                        */
/*    return view.getBigUint64(0) === 1n;                              */
/* }                                                                   */

console.log("   Kangax...");
assert.ok(kangaxa(), "kangaxa");
assert.ok(kangaxb(), "kangaxb");
assert.ok(kangaxc(), "kangaxc");
assert.ok(kangaxd(), "kangaxd");
/* assert.ok(kangaxe(), "kangaxe");                                    */
/* assert.ok(kangaxf(), "kangaxf");                                    */
/* assert.ok(kangaxg(), "kangaxg");                                    */
/* assert.ok(kangaxh(), "kangaxh");                                    */



