/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-proxy.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Wed Dec  5 13:32:31 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2016 Proxy objects                            */
/*=====================================================================*/
"use strict";
"use hopscript";

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

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
console.log( "   miscb()"); assert.ok( miscb(), "miscb" );
console.log( "   miscc()"); assert.ok( miscc(), "miscc" );
console.log( "   miscd()"); assert.ok( miscd(), "miscd" );
      
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

console.log( "mdn" );
console.log( "   mdna()"); mdna();

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
      get: function () {
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

console.log( "kangax" );
console.log( "   kangaxa()"); assert.ok( kangaxa(), "kangaxa" );
console.log( "   kangaxb()"); assert.ok( kangaxb(), "kangaxb" );
console.log( "   kangaxc()"); assert.ok( kangaxc(), "kangaxc" );
console.log( "   kangaxd()"); assert.ok( kangaxd(), "kangaxd" );
console.log( "   kangaxe()"); assert.ok( kangaxe(), "kangaxe" );

