/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/es6-module.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Oct 24 11:42:37 2018                          */
/*    Last change :  Wed Oct 24 18:41:04 2018 (serrano)                */
/*    Copyright   :  2018 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ES6 module                                               */
/*=====================================================================*/
"use hopscript";

var assert = require( "assert" );

console.log( "hop" );

/*---------------------------------------------------------------------*/
/*    init                                                             */
/*---------------------------------------------------------------------*/
import "../mod/init.js";
import { getLog } from "../mod/init-log.js";

/* console.log( "   init.js");                                         */
/* assert.strictEqual( getLog( "init.js" ), "subinit.js init.js" );    */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    evars                                                            *} */
/* {*---------------------------------------------------------------------*} */
/* import { checksum, mutator } from "../mod/evars.js";                */
/* import { K2 } from "../mod/evars.js";                               */
/* import * as evars from "../mod/evars.js";                           */
/* import defevars from "../mod/evars.js";                             */
/* import defevars2, * as evars2 from "../mod/evars.js";               */
/* import { GET as evarsGET } from "../mod/evars.js";                  */
/*                                                                     */
/* console.log( "   evars.js (let, const)");                           */
/* assert.strictEqual( checksum, 1088 );                               */
/*                                                                     */
/* console.log( "   evars.js (alias)");                                */
/* assert.strictEqual( K2, 990 );                                      */
/*                                                                     */
/* console.log( "   evars.js (mutation)");                             */
/* mutator( 11 );                                                      */
/* assert.strictEqual( evars.VAR, 11 );                                */
/* assert.strictEqual( evars2.VAR, 11 );                               */
/*                                                                     */
/* console.log( "   evars.js (inline declaration)");                   */
/* assert.strictEqual( evars.KONST2 + evars.VAR2, evars.GET() );       */
/* assert.strictEqual( evars2.KONST2 + evars2.VAR2, evars2.GET() );    */
/* assert.strictEqual( evars2.KONST2 + evars2.VAR2, evarsGET() );      */
/*                                                                     */
/* console.log( "   evars.js (default)");                              */
/* assert.strictEqual( defevars.a, 10 );                               */
/* assert.deepEqual( defevars, {a:10, b:2} );                          */
/*                                                                     */
/* console.log( "   evars.js (default alias)");                        */
/* assert.strictEqual( defevars2.a, 10 );                              */
/* assert.deepEqual( defevars2, {a:10, b:2} );                         */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    dynamic import                                                   *} */
/* {*---------------------------------------------------------------------*} */
/* async function dynload() {                                          */
/*    console.log( "   evars.js (dynload)");                           */
/*                                                                     */
/*    var mdyn = await import( "../mod/evars.js" );                    */
/*                                                                     */
/*    assert.strictEqual( mdyn.KONST2, 45 );                           */
/* }                                                                   */
/*                                                                     */
/* dynload();                                                          */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    default export                                                   *} */
/* {*---------------------------------------------------------------------*} */
/* import def, { oo, setX } from "../mod/def.js";                      */
/*                                                                     */
/* console.log( "   def.js");                                          */
/* assert.deepEqual( def, {x: 10} );                                   */
/* console.log( "   def.js (alias)");                                  */
/* assert.deepEqual( oo, {x: 10} );                                    */
/*                                                                     */
/* console.log( "   def.js (setX)");                                   */
/* setX( 100 );                                                        */
/* assert.deepEqual( def, {x: 100} );                                  */
/* console.log( "   def.js (setX alias)");                             */
/* assert.deepEqual( oo, {x: 100} );                                   */
/*                                                                     */
/*---------------------------------------------------------------------*/
/*    re-export                                                        */
/*---------------------------------------------------------------------*/
console.log( "   exporter.js (init)");
import * as rexp from "../mod/exporter.js";
assert.strictEqual( getLog( "exporter.js" ), 
		    "exporter2.js exporter3.js exporter.js" );

console.log( "   exporter.js (vars)");
assert.deepEqual( rexp, { e3a: "e3a", e3b: "e3b" } );
