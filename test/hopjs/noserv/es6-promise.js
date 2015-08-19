/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/test/hopjs/noserv/es6-promise.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 19 11:16:33 2015                          */
/*    Last change :  Wed Aug 19 15:09:08 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ES6 promises.                                            */
/*=====================================================================*/
var assert = require( "assert" );

// simple tests
var p = new Promise( function( resolve, reject ) { resolve( 3 ) } );
p.then( function( val ) { assert.equal( val, 3 ) } );

var p = new Promise( function( resolve, reject ) { resolve( 3 ) } )
    .then( function( val ) { assert.equal( val, 3 ) } )
    .then( function( val ) { assert.equal( val, 3 ) } );

var p = new Promise( function( resolve, reject ) { reject( -3 ) } )
    .then( undefined, function( val ) { assert.equal( val, -3 ) } );

var p = new Promise( function( resolve, reject ) { reject( -3 ) } )
    .then( undefined, function( val ) { assert.equal( val, -3 ) } )
    .then( undefined, function( val ) { assert.equal( val, -3 ) } );

var p = new Promise( function( resolve, reject ) { reject( -3 ) } )
    .catch( function( val ) { assert.equal( val, -3 ) } );

var p = new Promise( function( resolve, reject ) { reject( -3 ) } )
    .catch( function( val ) { assert.equal( val, -3 ) } )
    .catch( function( val ) { assert.equal( val, -3 ) } );
