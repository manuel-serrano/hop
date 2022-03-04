/*=====================================================================*/
/*    serrano/prgm/project/hop/3.5.x/test/hopjs/noserv/json.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Fri Mar  4 12:58:39 2022 (serrano)                */
/*    Copyright   :  2014-22 Manuel Serrano                            */
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

assert.equal( JSON.stringify( 1/0 ), "Infinity" );

var obj2 = [ undefined ];
assert.deepEqual( JSON.stringify( obj2 ), "[null]" );

var obj3 = { foo: undefined, bar: null };
assert.deepEqual( JSON.stringify( obj3 ), '{"bar":null}' );

var obj4 = {a: NaN};
assert.deepEqual( JSON.stringify( obj4 ), '{"a":null}' );

/*---------------------------------------------------------------------*/
/*    parse                                                            */
/*---------------------------------------------------------------------*/
assert.strictEqual( JSON.parse( "[1,2,3]" ).join( ","), "1,2,3" );
assert.deepEqual( JSON.parse( "[false]" ), [ false ] );
