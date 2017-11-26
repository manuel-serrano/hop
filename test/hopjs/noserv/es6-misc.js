/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/noserv/es6-misc.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Nov 25 06:02:02 2017                          */
/*    Last change :  Sat Nov 25 15:54:16 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ES6 misc features                                        */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*    -------------------------------------------------------------    */
/*    http://kangax.github.io/compat-table/es6/                        */
/*---------------------------------------------------------------------*/
function kangaxa() {
   var result = '';
   var target = {};

   "012349 DBACEFGHIJKLMNOPQRST".split('').concat(-1).forEach(function(key){
      Object.defineProperty(target, key, {
	 set: function(){
	    result += key;
	 }
      })
   });

   var obj = {2: 2, 0: 0, 1: 1, ' ': ' ', 9: 9, D: 'D', B: 'B', '-1': '-1'};
   Object.defineProperty(obj, 'A', {value: 'A',  enumerable: true});
   Object.defineProperty(obj, '3', {value: '3',  enumerable: true});
   Object.defineProperty(obj, 'C', {value: 'C',  enumerable: true});
   Object.defineProperty(obj, '4', {value: '4',  enumerable: true});
   delete obj[2];
   obj[2] = true;

   "EFGHIJKLMNOPQRST".split('').forEach(function(key){
      obj[key] = key;
   });

   Object.assign(target, obj);

   return result === "012349 DB-1ACEFGHIJKLMNOPQRST";
}

console.log( "kangax" );

console.log( "   kangaxa()");
assert.equal( kangaxa(), true, "kangaxa" );
