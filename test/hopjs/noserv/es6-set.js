/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/es6-set.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Feb 25 11:33:29 2019                          */
/*    Last change :  Mon Apr  6 08:41:27 2020 (serrano)                */
/*    Copyright   :  2019-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2016 MAP object.                              */
/*=====================================================================*/
"use strict";
"use hopscript";

const assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
function misca() {
   const m = new Set( [1, 4] );
   const k = m.values();
   
   const k0 = k.next().value;
   const k1 = k.next().value;
   return (k0 === 1 && k1 === 4);
}

function miscb() {
   const m = new Set( [1, 11, 4] );
   
   m.delete( 11 );
   const k = m.values();
   
   const k0 = k.next().value;
   const k1 = k.next().value;

   return (k0 === 1 && k1 === 4);
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
console.log( "   miscb()"); assert.ok( miscb(), "miscb" );

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
function mdna() {
   const set1 = new Set([1, 2, 3, 4, 5]);

   if(!set1.has(1)) return false;
   if(!set1.has(5)) return false;
   return !set1.has(6);
}

function mdnb() {
   const set1 = new Set();

   set1.add(42);
   set1.add(42);
   set1.add(13);
   
   let out = "";
   for (let item of set1) {
      out += item;
   }
   
   return out === "4213";
}

function mdnc() {
   const set1 = new Set();
   
   set1.add(1);
   set1.add('foo');

   if(set1.size !== 2) return false;
   set1.clear();
   
   return set1.size === 0;
}

function mdnd() {
   const set1 = new Set();
   
   set1.add({x: 10, y: 20}).add({x: 20, y: 30});
   set1.forEach(function(point, key, s){
      if (point.x > 10) {
    	 set1.delete(point);
      }
   });

   return set1.size === 1;
}

function mdne() {
   const set1 = new Set();

   set1.add(42);
   set1.add('forty two');

   const iterator1 = set1.entries();

   let out = "";
   for (let entry of iterator1) {
      out += entry.join();
   }

   return out === "42,42forty two,forty two";
}

function mdnf() {
   let out = "";
   function logSetElements(value1, value2, set) {
       out += 's[' + value1 + '] = ' + value2;
   }

   new Set(['foo', 'bar', undefined]).forEach(logSetElements);

   return out === "s[foo] = foos[bar] = bars[undefined] = undefined"
}

function mdng() {
   const set1 = new Set([1, 2, 3, 4, 5]);

   if(!set1.has(1)) return false;
   if(!set1.has(5)) return false;
   return !set1.has(6);
}

function mdnh() {
   const set1 = new Set();
   
   set1.add(42);
   set1.add('forty two');

   var iterator1 = set1.values();

   if( iterator1.next().value !== 42) return false;
   return iterator1.next().value === "forty two";
}

function mdni() {
   const set1 = new Set();
   
   set1.add(42);
   set1.add('forty two');

   var iterator1 = set1[Symbol.iterator]();

   if( iterator1.next().value !== 42) return false;
   return iterator1.next().value === "forty two";
}

function mdnj() {
   var mySet = new Set();

   mySet.add(1);
   mySet.add(5);
   mySet.add(5);
   mySet.add('some text');
   var o = {a: 1, b: 2};
   mySet.add(o);

   mySet.add({a: 1, b: 2});

   if(!mySet.has(1)) return false;
   if(mySet.has(3)) return false;
   if(!mySet.has(5)) return false;
   if(!mySet.has(Math.sqrt(25))) return false;
   if(!mySet.has('Some Text'.toLowerCase())) return false;
   if(!mySet.has(o)) return false;

   if(mySet.size!==5) return false;

   mySet.delete(5);
   if(mySet.has(5)) return false;

   if(mySet.size!==4) return false;
   
   let out = "";
   
   for(let v of mySet) {
      if( v instanceof Object ) {
	 for(let w in v ) { out += w + v[ w ]; }
      } else {
	 out += v;
      }
   }
   
   return out === "1some texta1b2a1b2";
}

function mdnk() {
   function isSuperset(set, subset) {
      for (var elem of subset) {
         if (!set.has(elem)) {
            return false;
         }
      }
      return true;
   }

   function union(setA, setB) {
      var _union = new Set(setA);
      for (var elem of setB) {
         _union.add(elem);
      }
      return _union;
   }

   function intersection(setA, setB) {
      var _intersection = new Set();
      for (var elem of setB) {
         if (setA.has(elem)) {
            _intersection.add(elem);
         }
      }
      return _intersection;
   }

   function symmetricDifference(setA, setB) {
      var _difference = new Set(setA);
      for (var elem of setB) {
         if (_difference.has(elem)) {
            _difference.delete(elem);
         } else {
            _difference.add(elem);
         }
      }
      return _difference;
   }

   function difference(setA, setB) {
      var _difference = new Set(setA);
      for (var elem of setB) {
         _difference.delete(elem);
      }
      return _difference;
   }

   //Examples
   var setA = new Set([1, 2, 3, 4]),
      setB = new Set([2, 3]),
      setC = new Set([3, 4, 5, 6]);

   if(!isSuperset(setA, setB)) return false;
   if(union(setA, setC).size !== 6) return false;
   if(intersection(setA, setC).size !== 2) return false;
   if(symmetricDifference(setA, setC).size !== 4) return false;
   return difference(setA, setC).size === 2;
}

function mdnl() {
   const numbers = [2,3,4,4,2,3,3,4,4,5,5,6,6,7,5,32,3,4,5];
   
   return [...new Set(numbers)].join() === [2, 3, 4, 5, 6, 7, 32].join();
}
 
function mdnm() {
   var text = 'India';

   var mySet = new Set(text);  // Set ['I', 'n', 'd', 'i', 'a']
   return mySet.size === 5; 
}

console.log( "mdn" );
console.log( "   mdna()"); assert.ok( mdna(), "mdna" );
console.log( "   mdnb()"); assert.ok( mdnb(), "mdnb" );
console.log( "   mdnc()"); assert.ok( mdnc(), "mdnc" );
console.log( "   mdnd()"); assert.ok( mdnd(), "mdnd" );
console.log( "   mdne()"); assert.ok( mdne(), "mdne" );
console.log( "   mdnf()"); assert.ok( mdnf(), "mdnf" );
console.log( "   mdng()"); assert.ok( mdng(), "mdng" );
console.log( "   mdnh()"); assert.ok( mdnh(), "mdnh" );
console.log( "   mdni()"); assert.ok( mdni(), "mdni" );
console.log( "   mdnj()"); assert.ok( mdnj(), "mdnj" );
console.log( "   mdnk()"); assert.ok( mdnk(), "mdnk" );
console.log( "   mdnl()"); assert.ok( mdnl(), "mdnl" );
console.log( "   mdnm()"); assert.ok( mdnm(), "mdnm" );

/*---------------------------------------------------------------------*/
/*    kangax ...                                                       */
/*---------------------------------------------------------------------*/
function __createIterableObject(arr, methods) {
   methods = methods || {};
   if( typeof Symbol !== 'function' || !Symbol.iterator ) {
      return {};
   }
   arr.length++;
   var iterator = {
      next: function() {
         return { value: arr.shift(), done: arr.length <= 0 };
      },
      'return': methods[ 'return' ],
      'throw': methods[ 'throw' ]
   };
   var iterable = {};
   iterable[ Symbol.iterator ] = function(){ return iterator; }
   return iterable;
}

function kangaxa() {
   var obj = {};
   var set = new Set();

   set.add(123);
   set.add(123);

   return set.has(123);}

function kangaxb() {
   var obj1 = {};
   var obj2 = {};
   var set = new Set([obj1, obj2]);

   return set.has(obj1) && set.has(obj2);}

function kangaxc() {
   new Set();
		   
   try {
      Set();
      return false;
   } catch(e) {
      return true;
   }
}

function kangaxd() {
   new Set(null);
   return true;
}

function kangaxe() {
   var passed = false;
   var _add = Set.prototype.add;

   Set.prototype.add = function(v) {
      passed = true;
   };

   new Set([1]);
   Set.prototype.add = _add;

   return passed;
}

function kangaxf() {
   var closed = false;
   var iter = __createIterableObject([1, 2, 3], {
      'return': function(){ closed = true; return {}; }
   });
   var add = Set.prototype.add;
   Set.prototype.add = function(){ throw 0 };
   try {
      new Set(iter);
   } catch(e){}
   Set.prototype.add = add;
   return closed;
}

function kangaxg() {
   var set = new Set();
   return set.add(0) === set;
}

function kangaxh() {
   var set = new Set();
   set.add(-0);
   var k;
   set.forEach(function (value, key, m) {
      k = 1 / value;
   });
   return k === Infinity && set.has(+0);
}

function kangaxi() {
   var obj = {};
   var set = new Set();

   set.add(123);
   set.add(123);
   set.add(456);

   return set.size === 2;
}

function kangaxj() {
   return typeof Set.prototype.delete === "function";
}

function kangaxk() {
   return typeof Set.prototype.clear === "function";
}

function kangaxl() {
   return typeof Set.prototype.forEach === "function";
}

function kangaxm() {
   return typeof Set.prototype.keys === "function";
}

function kangaxn() {
   return typeof Set.prototype.values === "function";
}

function kangaxo() {
   return typeof Set.prototype.entries === "function";
}

function kangaxp() {
   return typeof Set.prototype[Symbol.iterator] === "function";
}

function kangaxq() {
   new Set();
   var obj = {};
   try {
      Set.prototype.has(obj);
   }
   catch(e) {
      return true;
   }
}

function kangaxr() {
   // Iterator instance
   var iterator = new Set()[Symbol.iterator]();
   // %SetIteratorPrototype%
   var proto1 = Object.getPrototypeOf(iterator);
   // %IteratorPrototype%
   var proto2 = Object.getPrototypeOf(proto1);

   return proto2.hasOwnProperty(Symbol.iterator) &&
      !proto1    .hasOwnProperty(Symbol.iterator) &&
      !iterator  .hasOwnProperty(Symbol.iterator) &&
      iterator[Symbol.iterator]() === iterator;
}

function kangaxs() {
   var prop = Object.getOwnPropertyDescriptor(Set, Symbol.species);
return 'get' in prop && Set[Symbol.species] === Set;
}

console.log( "kangax" );
console.log( "   kangaxa()"); assert.ok( kangaxa(), "kangaxa" );
console.log( "   kangaxb()"); assert.ok( kangaxb(), "kangaxb" );
console.log( "   kangaxc()"); assert.ok( kangaxc(), "kangaxc" );
console.log( "   kangaxd()"); assert.ok( kangaxd(), "kangaxd" );
console.log( "   kangaxe()"); assert.ok( kangaxe(), "kangaxe" );
console.log( "   kangaxf()"); assert.ok( kangaxf(), "kangaxf" );
console.log( "   kangaxg()"); assert.ok( kangaxg(), "kangaxg" );
console.log( "   kangaxh()"); assert.ok( kangaxh(), "kangaxh" );
console.log( "   kangaxi()"); assert.ok( kangaxi(), "kangaxi" );
console.log( "   kangaxj()"); assert.ok( kangaxj(), "kangaxj" );
console.log( "   kangaxk()"); assert.ok( kangaxk(), "kangaxk" );
console.log( "   kangaxl()"); assert.ok( kangaxl(), "kangaxl" );
console.log( "   kangaxm()"); assert.ok( kangaxm(), "kangaxm" );
console.log( "   kangaxn()"); assert.ok( kangaxn(), "kangaxn" );
console.log( "   kangaxo()"); assert.ok( kangaxo(), "kangaxo" );
console.log( "   kangaxp()"); assert.ok( kangaxp(), "kangaxp" );
console.log( "   kangaxq()"); assert.ok( kangaxq(), "kangaxq" );
console.log( "   kangaxr()"); assert.ok( kangaxr(), "kangaxr" );
console.log( "   kangaxs()"); assert.ok( kangaxs(), "kangaxs" );



