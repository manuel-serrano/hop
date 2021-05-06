/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/es6-module.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Oct 24 11:42:37 2018                          */
/*    Last change :  Thu May  6 16:20:38 2021 (serrano)                */
/*    Copyright   :  2018-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ES6 module                                               */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

console.log( "hop" );

/*---------------------------------------------------------------------*/
/*    init                                                             */
/*---------------------------------------------------------------------*/
import "../mod/init.js";
import { getLog } from "../mod/init-log.js";

console.log( "   init.js");
assert.strictEqual( getLog( "init.js" ), "subinit.js init.js" );

/*---------------------------------------------------------------------*/
/*    evars                                                            */
/*---------------------------------------------------------------------*/
import { checksum, mutator } from "../mod/evars.js";
import { K2 } from "../mod/evars.js";
import * as evars from "../mod/evars.js";
import defevars from "../mod/evars.js";
import defevars2, * as evars2 from "../mod/evars.js";
import { GET as evarsGET } from "../mod/evars.js";

console.log( "   evars.js (let, const)");
assert.strictEqual( checksum, 1088 );

console.log( "   evars.js (class)");
assert.strictEqual( new evars.KLA().type, "KLA" );

console.log( "   evars.js (alias)");
assert.strictEqual( K2, 990 );

console.log( "   evars.js (mutation)");
mutator( 11 );
assert.strictEqual( evars.VAR, 11 );
assert.strictEqual( evars2.VAR, 11 );

console.log( "   evars.js (inline declaration)");
assert.strictEqual( evars.KONST2 + evars.VAR2, evars.GET() );
assert.strictEqual( evars2.KONST2 + evars2.VAR2, evars2.GET() );
assert.strictEqual( evars2.KONST2 + evars2.VAR2, evarsGET() );

console.log( "   evars.js (default)");
assert.strictEqual( defevars.a, 10 );
assert.deepEqual( defevars, {a:10, b:2} );

console.log( "   evars.js (default alias)");
assert.strictEqual( defevars2.a, 10 );
assert.deepEqual( defevars2, {a:10, b:2} );

/*---------------------------------------------------------------------*/
/*    dynamic import                                                   */
/*---------------------------------------------------------------------*/
async function dynload() {
   console.log( "   evars.js (dynload)");

   var mdyn = await import( "../mod/evars.js" );

   assert.strictEqual( mdyn.KONST2, 45 );
}

dynload();

/*---------------------------------------------------------------------*/
/*    default export                                                   */
/*---------------------------------------------------------------------*/
import def, { oo, setX, default as otherDef } from "../mod/def.js";

console.log( "   def.js");
assert.deepEqual( def, { z: 10 } );
console.log( "   def.js (alias)");
assert.deepEqual( oo, { z: 10 } );
assert.deepEqual( def, otherDef );

console.log( "   def.js (setX)");
setX( 100 );
assert.deepEqual( def, {z: 100} );
console.log( "   def.js (setX alias)");
assert.deepEqual( oo, {z: 100} );

console.log( "   def.js (named default)");
import * as ns from "../mod/def.js";
assert.deepEqual( Object.keys( ns ), [ "default", "oo", "setX" ] );

/*---------------------------------------------------------------------*/
/*    redirect                                                         */
/*---------------------------------------------------------------------*/
console.log( "   exporter.js (init)");
import * as rexp from "../mod/exporter.js";
assert.strictEqual( getLog( "exporter.js" ),
		    "exporter2.js exporter3.js exporter.js" );

console.log( "   exporter.js (vars)");
assert.deepEqual( rexp, { dummy: 5555, e3a: "e3a", e3b: "e3b", e5a: "e5a", e5b: "e5b", e5c: "e5c" } );

/*---------------------------------------------------------------------*/
/*    named redirect                                                   */
/*---------------------------------------------------------------------*/
console.log( "   named-exporter.js");
import * as nrexp from "../mod/named-exporter.js";
assert.deepEqual( nrexp, { ndummy: 6666, e3a: "e3a", e5a: "e5a", e5z: "e5c" } );

console.log( "   alias-exporter.js");
import * as arexp from "../mod/alias-exporter.js";
assert.deepEqual( arexp, { adummy: 7777, e3a: "e3a", e6a: "e5a", e6z: "e5b" } );

/*---------------------------------------------------------------------*/
/*    default redirect                                                 */
/*---------------------------------------------------------------------*/
console.log( "   def-exporter.js" );
import * as rdef from "../mod/def-exporter.js";
assert.deepEqual( rdef, { "default": { d5a: "d5a", d5b: "d5b", d5c: "d5c" } } );

/*---------------------------------------------------------------------*/
/*    alias default redirect                                           */
/*---------------------------------------------------------------------*/
console.log( "   def-alias.js" );
import * as adef from "../mod/def-alias.js";
assert.deepEqual( adef, { "foo": { d5a: "d5a", d5b: "d5b", d5c: "d5c" } } );

/*---------------------------------------------------------------------*/
/*    common.js                                                        */
/*---------------------------------------------------------------------*/
console.log( "   common.js" );

import cdef from "../mod/common.js";
assert.deepEqual( cdef, { a: 1, b: 2, c: 3 } );

/*---------------------------------------------------------------------*/
/*    import.meta                                                      */
/*---------------------------------------------------------------------*/
console.log( "   import.meta" );

import.meta.ext = "ext";

assert.ok( typeof import.meta.url === "string" );
assert.equal( import.meta.ext, "ext" );

/*---------------------------------------------------------------------*/
/*    recursive modules                                                */
/*---------------------------------------------------------------------*/
import * as esmod1 from "../mod/esmod1.js";
import * as esmod2 from "../mod/esmod2.js";

assert.equal( esmod1.foo( 1 ), "foo" );
assert.equal( esmod1.foo( 1000 ), "bar" );
assert.equal( esmod2.bar( 1 ), "foo" );
assert.equal( esmod2.bar( 1000 ), "bar" );
