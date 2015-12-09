/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/noserv/json.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Wed Nov  4 11:41:02 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing JSON encoding/decoding                                   */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    stringify                                                        */
/*---------------------------------------------------------------------*/
function F() { return this; }
F.prototype = {FOO: 3, BAR: 4 };

var o = new F();
o.FOO = 5;
assert.strictEqual( JSON.stringify( o ), '{"FOO":5}' );

var obj = { "Field 1": "It's Me", "Field 2": "It\'s Me Too" }
assert.deepEqual( obj, JSON.parse( JSON.stringify( obj ) ) );

/*---------------------------------------------------------------------*/
/*    parse                                                            */
/*---------------------------------------------------------------------*/
assert.strictEqual( JSON.parse( "[1,2,3]" ).join( ","), "1,2,3" );
assert.deepEqual( JSON.parse( "[false]" ), [ false ] );
