/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-proxy.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Mon Dec  3 11:03:27 2018 (serrano)                */
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
   return true;
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
      
/*---------------------------------------------------------------------*/
/*    mdn ...                                                          */
/*---------------------------------------------------------------------*/
function mdna() {
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

console.log( "kangax" );
console.log( "   kangaxa()"); assert.ok( kangaxa(), "kangaxa" );
console.log( "   kangaxb()"); assert.ok( kangaxb(), "kangaxb" );
console.log( "   kangaxc()"); assert.ok( kangaxc(), "kangaxc" );

