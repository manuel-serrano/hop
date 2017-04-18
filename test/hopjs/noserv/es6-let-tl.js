/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/test/hopjs/noserv/es6-let-tl.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jul  5 07:37:47 2015                          */
/*    Last commit :  2015-12-27 [9d6434f] (Manuel Serrano)             */
/*    Copyright   :  2015-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Top level let tests                                              */
/*=====================================================================*/
"use hopscript";

const assert = require( "assert" );

// test global const
const fs = require( "fs" );
const path = require( "path" );

var _K0 = 1;
const K0 = _K0;
var K1 = 666;

let f = function( x ) { if( x != 34 ) return h( x + K0 + K1 ); };
let h = function( y ) { return i( y + K2 ); };

f( 34 );

const K3 = 10;
let i = function( y ) { return y + K3; };
   
var K2 = 555;

assert.ok( f( 45 ) == (45 + K0 + K1 + K2 + K3), "Ks" );
