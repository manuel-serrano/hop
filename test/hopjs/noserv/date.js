/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/noserv/date.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Thu Sep  3 08:37:43 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing Date                                                     */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    parse                                                            */
/*---------------------------------------------------------------------*/
assert.strictEqual(
   Date.parse( "2011-10-10T14:48:00" ),
   new Date( "2011-10-10T14:48:00" ).valueOf() );

assert.strictEqual(
   Date.parse( "2011-05-10T14:48:00" ),
   new Date( "2011-05-10T14:48:00" ).valueOf() );

assert.strictEqual(
   new Date( "2011-10-10T14:48:00.000" ).valueOf(),
   new Date( "2011-10-10T14:48:00" ).valueOf() );
assert.strictEqual( 123, new Date("2011-10-10T14:48:00.123").getMilliseconds() );

/*---------------------------------------------------------------------*/
/*    Month                                                            */
/*---------------------------------------------------------------------*/
var dm = new Date();
dm.setMonth( 0 );

assert.ok( dm.getMonth() == 0 );
assert.ok( dm.getYear() == 115 );

/*---------------------------------------------------------------------*/
/*    ISO string conversions ::Iso string conversion ...               */
/*---------------------------------------------------------------------*/
var date = new Date( "2011-10-10T14:48:00" );
var nowf = new Date();
var now = new Date( nowf.setMilliseconds( 0 ) );

var sdate = date.toISOString();
var snowf = nowf.toISOString();
var snow = now.toISOString();

var redate = new Date( sdate );
var renowf = new Date( snowf );
var renow = new Date( snow );

assert.strictEqual( date.valueOf(), redate.valueOf() );
assert.strictEqual( nowf.valueOf(), renowf.valueOf() );
assert.strictEqual( now.valueOf(), renow.valueOf() );

/*---------------------------------------------------------------------*/
/*    More conversions                                                 */
/*---------------------------------------------------------------------*/
function test( date ) {
   var x = new Date( date );
   var val = x.valueOf();

   assert.strictEqual( val, Date.parse(x.toString()) );
   assert.strictEqual( val, Date.parse(x.toUTCString()) );
   assert.strictEqual( val, Date.parse(x.toISOString()) );
}

test( "1997-07-16T12:00:00+04:00" );
test( "1997-07-16T12:00:00+05:00" );
test( "2000-11-01T12:00:00+04:00" );
test( "2000-11-01T12:00:00+05:00" );

/*---------------------------------------------------------------------*/
/*    creation                                                         */
/*---------------------------------------------------------------------*/
var date = new Date();
date.setDate( 2.9 );

assert.ok( date.getDate() == 2 );








