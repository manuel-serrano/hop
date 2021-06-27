/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/bigint.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 30 17:54:33 2015                          */
/*    Last change :  Sun Jun 27 09:36:45 2021 (serrano)                */
/*    Copyright   :  2015-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript bigint                                        */
/*=====================================================================*/
"use strict";

var assert = require("assert");

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
console.log("mdn");

// constructor
function mdnConstructor() {
   console.log("   constructor...");

   const previouslyMaxSafeInteger = 9007199254740991n;

   const alsoHuge = BigInt(9007199254740991);
   assert.equal(alsoHuge, 9007199254740991n);

   const hugeString = BigInt("9007199254740991");
   assert.equal(alsoHuge, 9007199254740991n);

   const hugeHex = BigInt("0x1fffffffffffff");
   assert.equal(alsoHuge, 9007199254740991n);

   const hugeOctal = BigInt("0o377777777777777777");
   assert.equal(alsoHuge, 9007199254740991n);

   const hugeBin = BigInt("0b11111111111111111111111111111111111111111111111111111");
   assert.equal(alsoHuge, 9007199254740991n);
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
   console.log("   operator...");

   assert.equal(0n === 0, false);
   assert.equal(0n == 0, true);

   assert.equal(1n < 2, true);
   assert.equal(2n > 1, true);
   assert.equal(2 > 2, false);
   assert.equal(2n > 2, false);
   assert.equal(2n >= 2, true);

   const mixed = [4n, 6, -12n, 10, 4, 0, 0n];
   assert.deepEqual(mixed, [4n, 6, -12n, 10, 4, 0, 0n]);

   mixed.sort(); // default sorting behavior
   assert.deepEqual(mixed, [-12n, 0, 0n, 10, 4n, 4, 6]);

   assert.throws(() => {
	 mixed.sort((a, b) => a - b);
	 // won't work since subtraction will not work with mixed types
	 // TypeError: can't convert BigInt value to Number value
      });

   // sort with an appropriate numeric comparator
   mixed.sort((a, b) => (a < b) ? -1 : ((a > b) ? 1 : 0));
   assert.deepEqual(mixed, [-12n, 0, 0n, 4n, 4, 6, 10]);
   
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




