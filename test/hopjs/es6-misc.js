/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/es6-misc.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Nov 25 06:02:02 2017                          */
/*    Last change :  Tue Dec 12 08:57:27 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ES6 misc features                                        */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

function mdnAssign1() {
   var o1 = { a: 1 };
   var o2 = { [Symbol('foo')]: 2 };

   var obj = Object.assign({}, o1, o2);

   return obj.a === 1
      && Object.getOwnPropertySymbols(obj)[0].toString() == Symbol('foo').toString();
}

mdnAssign1();

