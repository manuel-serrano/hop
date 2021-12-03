/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/es6-proxy.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Tue Nov 23 08:28:46 2021 (serrano)                */
/*    Copyright   :  2014-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2016 Proxy objects                            */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
function misca() {
   const o0 = {
      id: "o0",
      get x() { return (this.id + (this === o5)) }
   }
   const o1 = {
      id: "o1",
      get x() { return (this.id + (this === o1)) }
   }
   const o2 = Object.create( o1, { id: {value: "o2"} });
   const o3 = new Proxy( o1, {} );
   const o4 = Object.create( o3, { id: {value: "o4"} } );
   const o5 = new Proxy( o0, {} );
   const o6 = Object.create( o5, { id: {value: "o6"} } );

   return o1.x === "o1true" 
      && o2.x === "o2false"
      && o3.x === "o1false"
      && o4.x === "o4false"
      && o5.x === "o0true"
      && o6.x === "o6false";
}

function miscb() {
   const o1 = {
      id: "o1",
   }
   const o3 = new Proxy( o1, {
      get: function( t, k, r ) {
      	 return ( k === "foo" ) ? 5 : undefined;
      },
      has: function( t, k ) {
      	 return k === "hux";
      }
   } );

   return ("bar" in o3) === false && o3.bar === undefined
      && ("foo" in o3) === false && o3.foo === 5
      && ("hux" in o3) === true && o3.hux === undefined;
}  

function miscc() {
   const o1 = {
      id: "o1",
   }
   const o3 = new Proxy( o1, {
      get: function( t, k, r ) {
      	 return ( k === "foo" ) ? 5 : 10;
      },
      has: function( t, k ) {
      	 return k === "hux";
      }
   } );
   const o4 = Object.create( o3 );
   
   return o4.foo === 5 && o4.hux === 10;
}   

function miscd() {
   const o1 = {
      id: "o1",
      get x() { return (this.id + (this === o1)) }
   }
   const o2 = new Proxy( o1, {
      get: function( t, k, r ) {
	 return ( k === "foo" ) ? 5 : t[ k ];
      },
   } );
   const o3 = new Proxy( o1, {} );
   const o4 = Object.create( o2, { id: { value: "o4" } } );
   const o5 = Object.create( o3, { id: { value: "o5" } } );

   return o4.x === "o1true" && o5.x === "o5false";
}

function misce() {
   var proxied = { };
   var passed = false;
   var prx = new Proxy(proxied, {
   });
   var proxy = Object.create(prx);
   proxy.FOO = "bar";
   return proxy.hasOwnProperty( "FOO" );
}

function miscf() {
   function test( a, b, c ) {
      return a + b + c;
   }
   
   const arr = new Proxy( [], {
      get: function( target, key ) {
	 switch( key ) {
	    case "length": return 3;
	    case "0": return 1000;
	    case "1": return 1001;
	    case "2": return 1003;
	    default: return undefined;
	 }
      }
   } );

   return test.apply( null, arr ) === 3004;
}

function miscg() {   
   function CTOR( a ) {
      this.a = a;
   }

   const handler = { 
      get: function( target, prop ) { 
	 return target[ prop ] },
      set: function( target, prop, v ) { 
	 target[ prop ] = v; return true; }
   }

   var o = new CTOR( 1 );
   var p = new Proxy( o, handler );

   return (o instanceof CTOR) && (p instanceof CTOR);
}

function misch() {
   var o = { a:1 };
   var p = new Proxy( o, {} );

   p.a = 30;
   
   return p.a === 30 && o.hasOwnProperty( "a" );
}

function misci() {
   var o = { 
      get aa() { return 23 },
      set aa( v ) { return 23 },
   }

   var p = new Proxy( o, {} );
   
   var c = Object.create( p );
   var d = Object.create( c );
   
   d.a = 34;
   d.aa = 35;
   
   return d.hasOwnProperty( "a" ) && !d.hasOwnProperty( "aa" );
}

function miscj() {
   var o = { a:1 };
   var getp = { apply: function( target, self, alist ) { return 24; }
   }
   var h = { get: new Proxy( function() {}, getp ) };
   var p = new Proxy( o, h );

   return p.a === 24;
}

function misck() {
   return typeof( new Proxy( function() { return true}, {} ) ) === "function"; 
}

function miscp() {
   let a = [ 1, 2, 3 ];
   let b = [ 4, 5, 6 ];
   let c = new Proxy( [ 7, 8, 9 ], {} );
   
   return a.concat( b, c ).length === 9;
}

function miscm() {
   // get cache test
   function ret( o ) {
      return o.x;
   }

   function test( o ) {
      let v;
   
      for( let i = 0; i < 10; i++ ) {
      	 v = ret( o );
      }
      
      return v;
   }

   const o = { a: 1 };
   let h = { get: function( target, key ) { return 3; } };
   let p = new Proxy( o, h );

   let v0 = test( p );
   h.get = function( target, key ) { return 4; };
   let v1 = test( p );

   return v0 === 3 && v1 === 4;
}

function miscn() {
   // apply cache test
   function ret( o ) {
      return o();
   }

   function test( o ) {
      let v;
   
      for( let i = 0; i < 10; i++ ) {
      	 v = ret( o );
      }
      
      return v;
   }

   function o() { return 1; }
   let h = { apply: function( target, thisa, alist ) { return 3; } };
   let p = new Proxy( o, h );

   let v0 = test( p );
   h.apply = function( target, thisa, alist ) { return 4; };
   let v1 = test( p );

   return v0 === 3 && v1 === 4;
}

function misco() {
   function getProxy( obj ) {
      return obj.x; 
   }

   const noinline = { f: getProxy };
   const get = noinline.f;

   function test( obj ) {
      const g = [ get( obj ), get( obj ), get( obj ), get( obj ), get( obj ) ];
      
      for( let i = 1; i < g.length; i++ ) {
      	 if( g[ i ] !== g[ 0 ] ) 
	    return false;
      }
      
      return g[ 0 ];
   }

   // test 0
   function test0() {
      console.log( "      misco.test0..." );
      const h = {};
      const p = new Proxy( { x : 10 }, h );
      return test( p ) === 10;
   }

   // test 1
   function test1() {
      console.log( "      misco.test1..." );
      const h = { get: function( ... rest ) { return 19; } };
      const p = new Proxy( {}, h );
      return test( p ) === 19;
   }

   // test 2
   function test2() {
      console.log( "      misco.test2..." );
      const h = { get: function(t, p, r) { return 15; } };
      const p = new Proxy( {}, h );
      return test( p ) === 15;
   }

   // test 3
   function test3() {
      console.log( "      misco.test3..." );
      const h = { get: function(t, p) { return 14; } };
      const p = new Proxy( {}, h );
      return test( p ) === 14;
   }

   // test 4
   function test4() {
      console.log( "      misco.test4..." );
      const h = { get: 3 };
      h.get = function( ... rest ) { return 2; };
      
      const p = new Proxy( {}, h );
      return test( p ) === 2;
   }

   // test 5
   function test5() {
      console.log( "      misco.test5..." );
      const ph = {};
      const h = { __proto__: ph };
      const p = new Proxy( { x: 2 }, h );
      
      if( test( p ) !== 2 ) return false;
      
      ph.get = function(t,p,r) { return 5; };
      if( test( p ) !== 5 ) return false;
      
      ph.get = function(t,p,r) { return 6; }
      if( test( p ) !== 6 ) return false;
      
      h.get = function(t,p,r) { return 7; }
      if( test( p ) !== 7 ) return false;
      
      h.get = function(t,p,r) { return 8; }
      if( test( p ) !== 8 ) return false;
      
      h.get = 3;
      h.get = function(t,p,r) { return 9; }
      if( test( p ) !== 9 ) return false;
      
      h.get = 4;
      h.get = function(t,p,r) { return 10; }
      if( test( p ) !== 10 ) return false;
      
      return true;
   }

   // test 6
   function test6() {
      console.log( "      misco.test6..." );
      const ph = {};
      const h = { __proto__: ph };
      const p = new Proxy( { x: 2 }, h );
      
      if( test( p ) !== 2 ) return false;
      
      ph.get = function(t,p,r) { return 5; };
      if( test( p ) !== 5 ) return false;
      
      ph.get = function(t,p,r) { return 6; }
      if( test( p ) !== 6 ) return false;
      
      ph.get = 3;
      ph.get = function(t,p,r) { return 9; }
      if( test( p ) !== 9 ) return false;
      
      return true;
   }

   // test 7
   function test7() {
      console.log( "      misco.test7..." );
      const p = new Proxy( {}, { get: function( ... rest ) { return 4; } } );
      const o = { __proto__: p, y: 18 };
      
      return test( o ) === 4;
   }

   assert.ok( test0(), "misco.test0" );
   assert.ok( test1(), "misco.test1" );
   assert.ok( test2(), "misco.test2" );
   assert.ok( test3(), "misco.test3" );
   assert.ok( test4(), "misco.test4" );
   assert.ok( test5(), "misco.test5" );
   assert.ok( test6(), "misco.test6" );
   assert.ok( test7(), "misco.test7" );
   
   return true;
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
console.log( "   miscb()"); assert.ok( miscb(), "miscb" );
console.log( "   miscc()"); assert.ok( miscc(), "miscc" );
console.log( "   miscd()"); assert.ok( miscd(), "miscd" );
console.log( "   misce()"); assert.ok( misce(), "misce" );
console.log( "   miscf()"); assert.ok( miscf(), "miscf" );
console.log( "   miscg()"); assert.ok( miscg(), "miscg" );
console.log( "   misch()"); assert.ok( misch(), "misch" );
console.log( "   misci()"); assert.ok( misci(), "misci" );
console.log( "   miscj()"); assert.ok( miscj(), "miscj" );
console.log( "   misck()"); assert.ok( misck(), "misck" );
console.log( "   miscl()"); assert.ok( miscp(), "miscl" );
console.log( "   miscm()"); assert.ok( miscm(), "miscm" );
console.log( "   miscn()"); assert.ok( miscn(), "miscn" );
console.log( "   misco()"); assert.ok( misco(), "misco" );
      
/*---------------------------------------------------------------------*/
/*    mdn ...                                                          */
/*---------------------------------------------------------------------*/
function mdna() {
   const monster1 = {
      secret: 'easily scared',
      eyeCount: 4
   };
   
   const handler1 = {
      get: function(target, prop, receiver) {
	 if (prop === 'secret') {
	    return `${target.secret.substr(0, 4)} ... shhhh!`;
	 } else {
	    return Reflect.get(... arguments);
	 }
      }
   }
   
   const proxy1 = new Proxy(monster1, handler1);
   
   return proxy1.eyeCount + proxy1.secret;
}

function mdnb() {
   let res = "";
   
   let o = new Proxy( {}, {} );
   
   var revocable = Proxy.revocable({}, {
      get: function(target, name) {
    	 return "[[" + name + "]]";
      }
   });

   var proxy = revocable.proxy;
   
   res = proxy.foo;

   revocable.revoke();

   try {
      console.log(proxy.foo); // TypeError is thrown
      return false;
   } catch( _e ) {
      res += ".ok";
   }
    
   try {
      proxy.foo = 1           // TypeError again
      return false;
   } catch( _e ) {
      res += ".ok";
   }
   
   try {
      delete proxy.foo;       // still TypeError
      return false;
   } catch( _e ) {
      res += ".ok";
   }
      
   return typeof proxy === "object" && res === "[[foo]].ok.ok.ok";       
}

console.log( "mdn" );
console.log( "   mdna()"); assert.ok( mdna(), "mdna" );
console.log( "   mdnb()"); assert.ok( mdnb(), "mdnb" );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*---------------------------------------------------------------------*/
function kangaxa() {
   new Proxy({}, {});
   try {
      Proxy({}, {});
      return false;
   } catch(e) {
      return true;
   }
}

function kangaxb() {
   new Proxy({}, {});
   return !Proxy.hasOwnProperty('prototype');
}

function kangaxc() {
   var proxied = { };
   var proxy = new Proxy(proxied, {
      get: function (t, k, r) {
	 return t === proxied && k === "foo" && r === proxy && 5;
      }
   });
   return proxy.foo === 5;
}

function kangaxd() {
   var proxied = { };
   var proxy = Object.create(new Proxy(proxied, {
      get: function (t, k, r) {
    	 return t === proxied && k === "foo" && r === proxy && 5;
      }
   }));
   return proxy.foo === 5;
}

function kangaxe() {
   var passed = false;
   var proxied = { };
   var proxy = new Proxy(proxied, {
      get: function ( ... rest ) {
    	 passed = true;
    	 return 4;
      }
   });
   // The value reported for a property must be the same as the value of the corresponding
   // target object property if the target object property is a non-writable,
   // non-configurable own data property.
   Object.defineProperty(proxied, "foo", { value: 5, enumerable: true });
   try {
      proxy.foo;
      return false;
   }
   catch(e) {}
   // The value reported for a property must be undefined if the corresponding target
   // object property is a non-configurable own accessor property that has undefined
   // as its [[Get]] attribute.
   Object.defineProperty(proxied, "bar",
      { set: function(){}, enumerable: true });
   try {
      proxy.bar;
      return false;
   }
   catch(e) {}
   return passed;
}

function kangaxf() {
   var proxied = { };
   var passed = false;
   var proxy = new Proxy(proxied, {
      set: function (t, k, v, r) {
    	 passed = t === proxied && k + v === "foobar" && r === proxy;
	 return true;
      }
   });
   proxy.foo = "bar";
   return passed;
}

function kangaxg() {
   var proxied = { };
   var passed = false;
   var proxy = Object.create(new Proxy(proxied, {
      set: function (t, k, v, r) {
    	 passed = t === proxied && k + v === "foobar" && r === proxy;
	 return true;
      }
   }));
   proxy.foo = "bar";
   return passed;
}

function kangaxh() {
   var passed = false;
   new Proxy({},{});
   // Cannot change the value of a property to be different from the value of
   // the corresponding target object if the corresponding target object
   // property is a non-writable, non-configurable own data property.
   var proxied = {};
   var proxy = new Proxy(proxied, {
      set: function ( ... rest ) {
    	 passed = true;
    	 return true;
      }
   });
   Object.defineProperty(proxied, "foo", { value: 2, enumerable: true });
   proxy.foo = 2;
   try {
      proxy.foo = 4;
      return false;
   } catch(e) {}
   // Cannot set the value of a property if the corresponding target
   // object property is a non-configurable own accessor property
   // that has undefined as its [[Set]] attribute.
   Object.defineProperty(proxied, "bar",
      { get: function(){}, enumerable: true });
   try {
      proxy.bar = 2;
      return false;
   } catch(e) {}
   return passed;
}

function kangaxi() {
   var proxied = {};
   var passed = false;
   "foo" in new Proxy(proxied, {
      has: function (t, k) {
    	 passed = t === proxied && k === "foo";
      }
   });
	 return passed;
}

function kangaxj() {
   var proxied = {};
   var passed = false;
   "foo" in Object.create(new Proxy(proxied, {
      has: function (t, k) {
    	 passed = t === proxied && k === "foo";
      }
   }));
	 return passed;
}

function kangaxk() {
   var passed = false;
   new Proxy({},{});
   // A property cannot be reported as non-existent, if it exists as a
   // non-configurable own property of the target object.
   var proxied = {};
   var proxy = new Proxy(proxied, {
      has: function ( ... rest ) {
    	 passed = true;
    	 return false;
      }
   });
   Object.defineProperty(proxied, "foo", { value: 2, writable: true, enumerable: true });
   try {
      'foo' in proxy;
  	    return false;
   } catch(e) {}
   // A property cannot be reported as non-existent, if it exists as an
   // own property of the target object and the target object is not extensible.
   proxied.bar = 2;
   Object.preventExtensions(proxied);
   try {
      'bar' in proxy;
  	    return false;
   } catch(e) {}
   return passed;
}

function kangaxl() {
   var proxied = {};
   var passed = false;
   delete new Proxy(proxied, {
      deleteProperty: function (t, k) {
    	 passed = t === proxied && k === "foo";
	 return true;
      }
   }).foo;
   return passed;
}

function kangaxm() {
   var passed = false;
   new Proxy({},{});
   // A property cannot be reported as deleted, if it exists as a non-configurable
   // own property of the target object.
   var proxied = {};
   Object.defineProperty(proxied, "foo", { value: 2, writable: true, enumerable: true });
   try {
      delete new Proxy(proxied, {
    	 deleteProperty: function ( ... rest ) {
      	    passed = true;
      	    return true;
    	 }
      }).foo;
      return false;
   } catch(e) {}
   return passed;
}

function kangaxn() {
   var proxied = {};
   var fakeDesc = { value: "foo", configurable: true };
   var returnedDesc = Object.getOwnPropertyDescriptor(
      new Proxy(proxied, {
    	 getOwnPropertyDescriptor: function (t, k) {
      	    return t === proxied && k === "foo" && fakeDesc;
    	 }
      }),
      "foo"
      );
   return (returnedDesc.value     === fakeDesc.value
  	   && returnedDesc.configurable === fakeDesc.configurable
  	   && returnedDesc.writable     === false
  	   && returnedDesc.enumerable   === false);
}

function kangaxo() {
   var passed = false;
   new Proxy({},{});
   // A property cannot be reported as non-existent, if it exists as a non-configurable
   // own property of the target object.
   var proxied = {};
   var proxy = new Proxy(proxied, {
      getOwnPropertyDescriptor: function ( ... rest ) {
    	 passed = true;
    	 return undefined;
      }
   });
   Object.defineProperty(proxied, "foo", { value: 2, writable: true, enumerable: true });
   try {
      Object.getOwnPropertyDescriptor(proxy, "foo");
      return false;
   } catch(e) {}
   // A property cannot be reported as non-existent, if it exists as an own property
   // of the target object and the target object is not extensible.
   proxied.bar = 3;
   Object.preventExtensions(proxied);
   try {
      Object.getOwnPropertyDescriptor(proxy, "bar");
      return false;
   } catch(e) {}
   // A property cannot be reported as existent, if it does not exists as an own property
   // of the target object and the target object is not extensible.
   try {
      Object.getOwnPropertyDescriptor(new Proxy(proxied, {
    	 getOwnPropertyDescriptor: function() {
      	    return { value: 2, configurable: true, writable: true, enumerable: true };
    	 }}), "baz");
      return false;
   } catch(e) {}
   // A property cannot be reported as non-configurable, if it does not exists as an own
   // property of the target object or if it exists as a configurable own property of
   // the target object.
   try {
      Object.getOwnPropertyDescriptor(new Proxy({}, {
    	 getOwnPropertyDescriptor: function() {
      	    return { value: 2, configurable: false, writable: true, enumerable: true };
    	 }}), "baz");
      return false;
   } catch(e) {}
   try {
      Object.getOwnPropertyDescriptor(new Proxy({baz:1}, {
    	 getOwnPropertyDescriptor: function() {
      	    return { value: 1, configurable: false, writable: true, enumerable: true };
    	 }}), "baz");
      return false;
   } catch(e) {}
   return passed;
}

function kangaxp() {
   var proxied = {};
   var passed = false;
   Object.defineProperty(
      new Proxy(proxied, {
    	 defineProperty: function (t, k, d) {
      	    passed = t === proxied && k === "foo" && d.value === 5;
      	    return true;
    	 }
      }),
      "foo",
      { value: 5, configurable: true }
      );
   return passed;
}

function kangaxq() {
   var passed = false;
   new Proxy({},{});
   // A property cannot be added, if the target object is not extensible.
   var proxied = Object.preventExtensions({});
   var proxy = new Proxy(proxied, {
      defineProperty: function( ... rest ) {
    	 passed = true;
    	 return true;
      }
   });
   try {
      Object.defineProperty(proxy, "foo", { value: 2 });
      return false;
   } catch(e) {}
   // A property cannot be non-configurable, unless there exists a corresponding
   // non-configurable own property of the target object.
   try {
      Object.defineProperty(
    	 new Proxy({ bar: true }, {
      	    defineProperty: function ( ... rest ) {
               return true;
      	    }
    	 }),
    	 "bar",
    	 { value: 5, configurable: false, writable: true, enumerable: true }
  	 );
      return false;
   } catch(e) {}
   return passed;
}

function kangaxr() {
   var proxied = {};
   var fakeProto = {};
   var proxy = new Proxy(proxied, {
      getPrototypeOf: function (t) {
    	 return t === proxied && fakeProto;
      }
   });
   return Object.getPrototypeOf(proxy) === fakeProto;
}

function kangaxs() {
   var passed = false;
   new Proxy({},{});
   // If the target object is not extensible, [[GetPrototypeOf]] applied to the proxy object
   // must return the same value as [[GetPrototypeOf]] applied to the proxy object's target object.
   try {
      Object.getPrototypeOf(new Proxy(Object.preventExtensions({}), {
    	 getPrototypeOf: function ( ... rest ) {
      	    passed = true;
      	    return {};
    	 }
      }));
      return false;
   } catch(e) {}
   return passed;
}

function kangaxt() {
   var proxied = {};
   var newProto = {};
   var passed = false;
   Object.setPrototypeOf(
      new Proxy(proxied, {
    	 setPrototypeOf: function (t, p) {
      	    passed = t === proxied && p === newProto;
      	    return true;
    	 }
      }),
      newProto
	 );
   return passed;
}

function kangaxu() {
   var passed = false;
   new Proxy({},{});
   Object.setPrototypeOf({},{});
   // If the target object is not extensible, the argument value must be the
   // same as the result of [[GetPrototypeOf]] applied to target object.
   try {
      Object.setPrototypeOf(
    	 new Proxy(Object.preventExtensions({}), {
      	    setPrototypeOf: function ( ... rest ) {
               passed = true;
               return true;
      	    }
    	 }),{});
      return false;
   } catch(e) {}
   return passed;
}

function kangaxv() {
   var proxied = {};
   var passed = false;
   Object.isExtensible(
      new Proxy(proxied, {
    	 isExtensible: function (t) {
      	    passed = t === proxied; return true;
    	 }
      })
	 );
   return passed;
}

function kangaxw() {
   var passed = false;
   new Proxy({},{});
   // [[IsExtensible]] applied to the proxy object must return the same value
   // as [[IsExtensible]] applied to the proxy object's target object with the same argument.
   try {
      Object.isExtensible(new Proxy({}, {
    	 isExtensible: function (t) {
      	    passed = true;
      	    return false;
    	 }
      }));
      return false;
   } catch(e) {}
   try {
      Object.isExtensible(new Proxy(Object.preventExtensions({}), {
    	 isExtensible: function (t) {
      	    return true;
    	 }
      }));
      return false;
   } catch(e) {}
   return true;
}

function kangaxx() {
   var proxied = {};
   var passed = false;
   Object.preventExtensions(
      new Proxy(proxied, {
    	 preventExtensions: function (t) {
      	    passed = t === proxied;
      	    return Object.preventExtensions(proxied);
    	 }
      })
	 );
   return passed;
}

function kangaxy() {
   var passed = false;
   new Proxy({},{});
   // [[PreventExtensions]] applied to the proxy object only returns true
   // if [[IsExtensible]] applied to the proxy object's target object is false.
   try {
      Object.preventExtensions(new Proxy({}, {
    	 preventExtensions: function ( ... rest ) {
      	    passed = true;
      	    return true;
    	 }
      }));
      return false;
   } catch(e) {}
   return passed;
}

function kangaxz() {
   var proxied = {};
   var passed = false;
   Object.keys(
      new Proxy(proxied, {
    	 ownKeys: function (t) {
      	    passed = t === proxied; return [];
    	 }
      })
	 );
   return passed;
}

function kangaxA() {
   var passed = false;
   new Proxy({},{});
   // The Type of each result List element is either String or Symbol.
   try {
      Object.keys(new Proxy({}, {
    	 ownKeys: function ( ... rest ) {
      	    passed = true;
      	    return [2];
    	 }}));
      return false;
   } catch(e) {}
   // The result List must contain the keys of all non-configurable own properties of the target object.
   var proxied = {};
   Object.defineProperty(proxied, "foo", { value: 2, writable: true, enumerable: true });
   try {
      Object.keys(new Proxy(proxied, {
    	 ownKeys: function ( ... rest ) {
      	    return [];
    	 }}));
      return false;
   } catch(e) {}
   // If the target object is not extensible, then the result List must contain all the keys
   // of the own properties of the target object and no other values.
   try {
      Object.keys(new Proxy(Object.preventExtensions({b:1}), {
    	 ownKeys: function () {
      	    return ['a'];
    	 }}));
      return false;
   } catch(e) {}
   return passed;
}

function kangaxB() {
   var proxied = function(){};
   var passed = false;
   var host = {
      method: new Proxy(proxied, {
    	 apply: function (t, thisArg, args) {
      	    passed = t === proxied && thisArg === host && args + "" === "foo,bar";
    	 }
      })
   };
   host.method("foo", "bar");
   return passed;
}

function kangaxC() {
   var passed = false;
   new Proxy(function(){}, {
      apply: function ( ... rest ) { passed = true; }
   })();
   // A Proxy exotic object only has a [[Call]] internal method if the
   // initial value of its [[ProxyTarget]] internal slot is an object
   // that has a [[Call]] internal method.
   try {
      new Proxy({}, {
    	 apply: function ( ... rest ) {}
      })();
      return false;
   } catch(e) {}
   return passed;
}

function kangaxD() {
   var proxied = function(){};
   var passed = false;
   new new Proxy(proxied, {
      construct: function (t, args) {
    	 passed = t === proxied && args + "" === "foo,bar";
    	 return {};
      }
   })("foo","bar");
   return passed;
}

function kangaxE() {
   var passed = false;
   new Proxy({},{});
   // A Proxy exotic object only has a [[Construct]] internal method if the
   // initial value of its [[ProxyTarget]] internal slot is an object
   // that has a [[Construct]] internal method.
   try {
      new new Proxy({}, {
    	 construct: function (t, args) {
      	    return {};
    	 }
      })();
      return false;
   } catch(e) {}
   // The result of [[Construct]] must be an Object.
   try {
      new new Proxy(function(){}, {
    	 construct: function (t, args) {
      	    passed = true;
      	    return 5;
    	 }
      })();
      return false;
   } catch(e) {}
   return passed;
}

function kangaxF() {
   var obj = Proxy.revocable({}, { get: function( ... rest ) { return 5; } });
   var passed = (obj.proxy.foo === 5);
   obj.revoke();
   try {
      obj.proxy.foo;
   } catch(e) {
      passed &= e instanceof TypeError;
   }
   return passed;
}

function kangaxG() {
   return Array.isArray(new Proxy([], {}));
}

function kangaxH() {
   console.log( JSON.stringify(new Proxy(['foo'], {})) );
   return JSON.stringify(new Proxy(['foo'], {})) === '["foo"]';
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
console.log( "   kangaxt()"); assert.ok( kangaxt(), "kangaxt" );
console.log( "   kangaxu()"); assert.ok( kangaxu(), "kangaxu" );
console.log( "   kangaxv()"); assert.ok( kangaxv(), "kangaxv" );
console.log( "   kangaxw()"); assert.ok( kangaxw(), "kangaxw" );
console.log( "   kangaxx()"); assert.ok( kangaxx(), "kangaxx" );
console.log( "   kangaxy()"); assert.ok( kangaxy(), "kangaxy" );
console.log( "   kangaxz()"); assert.ok( kangaxz(), "kangaxz" );
console.log( "   kangaxA()"); assert.ok( kangaxA(), "kangaxA" );
console.log( "   kangaxB()"); assert.ok( kangaxB(), "kangaxB" );
console.log( "   kangaxC()"); assert.ok( kangaxC(), "kangaxC" );
console.log( "   kangaxD()"); assert.ok( kangaxD(), "kangaxD" );
console.log( "   kangaxE()"); assert.ok( kangaxE(), "kangaxE" );
console.log( "   kangaxF()"); assert.ok( kangaxF(), "kangaxF" );
console.log( "   kangaxG()"); assert.ok( kangaxG(), "kangaxG" );
console.log( "   kangaxH()"); assert.ok( kangaxH(), "kangaxH" );

