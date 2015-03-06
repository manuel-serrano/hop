/*=====================================================================*/
/*    .../project/hop/3.0.x/test/hopjs/noserv/comprehension.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Fri Mar  6 09:56:50 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing Ecmascript 6 Array Comprehension                         */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    one array                                                        */
/*---------------------------------------------------------------------*/
assert.deepEqual( [for (i of [ 1, 2, 3 ]) i * i], [ 1, 4, 9 ] );

var abc = [ "A", "B", "C" ];
assert.deepEqual( [for (letters of abc) letters.toLowerCase()],
		  [ "a", "b", "c" ] );

var years = [ 1954, 1974, 1990, 2006, 2010, 2014 ];
assert.deepEqual( [for (year of years) if (year > 2000) year],
		  [ 2006, 2010, 2014 ] );
assert.deepEqual( [for (year of years) if (year > 2000) if(year < 2010) year],
		  [ 2006 ] );
assert.deepEqual( [for (year of years) if (year > 2000 && year < 2010) year],
		  [ 2006 ] );

var numbers = [ 1, 2, 3 ];
assert.deepEqual( numbers.map(function (i) { return i * i }),
		  [for (i of numbers) i*i ] );
assert.deepEqual( numbers.filter(function (i) { return i < 3 }),
		  [for (i of numbers) if (i < 3) i] );

assert.deepEqual( [for (i of [1, 4, 2, 3, -8]) if (i < 3) i],
		  [1, 2, -8] );
assert.equal(
   [for (i of  Array.apply(0, Array(26)).map(function(x, y) { return y; }))
      String.fromCharCode(65 + i)].join(''),
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ" );

/*---------------------------------------------------------------------*/
/*    two arrays                                                       */
/*---------------------------------------------------------------------*/
var numbers = [ 1, 2, 3 ];
var letters = [ "a", "b", "c" ];

var cross = [for (i of numbers) for (j of letters) i+j];
assert.deepEqual( cross,
		  [ "1a", "1b", "1c", "2a", "2b", "2c", "3a", "3b", "3c" ] );

var grid = [for (i of numbers) [for (j of letters) i+j]];
assert.deepEqual( grid,
		  [ ["1a", "1b", "1c"], ["2a", "2b", "2c"], ["3a", "3b", "3c"] ] );

assert.deepEqual( [for (i of numbers) if (i > 1) for (j of letters) if(j > "a") i+j],
		  ["2b", "2c", "3b", "3c"] );
assert.deepEqual( [for (i of numbers) for (j of letters) if (i > 1) if(j > "a") i+j],
		  ["2b", "2c", "3b", "3c"] );
assert.deepEqual( [for (i of numbers) if (i > 1) [for (j of letters) if(j > "a") i+j]],
		  [["2b", "2c"], ["3b", "3c"]] );
assert.deepEqual( [for (i of numbers) [for (j of letters) if (i > 1) if(j > "a") i+j]],
		  [[], ["2b", "2c"], ["3b", "3c"]] );

/*---------------------------------------------------------------------*/
/*    bindings                                                         */
/*---------------------------------------------------------------------*/
assert.ok( (function () {
   var i = 5;
   [for (i of [ 1, 2, 3 ]) i];
   return i;
})() == 5 );

assert.deepEqual( (function () {
   var i = [1, 2, 3];
   return [for (i of i) i * i];
})(), [ 1, 4, 9 ] );


