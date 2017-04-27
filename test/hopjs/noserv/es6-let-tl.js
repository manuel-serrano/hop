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

let fff = function( x ) { if( x != 34 ) return h( x + K0 + K1 ); };
let h = function( y ) { return q( y + K2 ); };

fff( 34 );

const K3 = 10;
let q = function( y ) { return y + K3; };
   
var K2 = 555;

var X = fff( 45 );
var Y = (45 + K0 + K1 + K2 + K3);

assert.ok( X == Y, "Ks" + " " + X + "/" + Y );
