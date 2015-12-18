/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/noserv/date.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Fri Dec 18 08:51:49 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing Date                                                     */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    prototypes                                                       */
/*---------------------------------------------------------------------*/
assert.strictEqual( (new Date().__proto__), Date.prototype );
assert.strictEqual( (new Date( undefined ).__proto__), Date.prototype );
assert.strictEqual( (new Date( 1245 ).__proto__), Date.prototype );

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
/*    accessors                                                        */
/*---------------------------------------------------------------------*/
var s = Date.parse( "2015-10-20T18:59:05+00:00" ) + 260;
var d = new Date( s );

assert.ok( s == 1445367545260, "parse" );
assert.ok( d.getMonth() == 9, "getMonth" );
assert.ok( d.getDate() == 20, "getDate" );
assert.ok( d.getDay() == 2, "getDay" );
assert.ok( d.getFullYear() == 2015, "getFullYear" );
assert.ok( d.getHours() == 20, "getHours" );
assert.ok( d.getMilliseconds() == 260, "getMilliseconds" );
assert.ok( d.getMinutes() == 59, "getMinutes" );
assert.ok( d.getUTCDate() == 20, "getUTCDate" );
assert.ok( d.getUTCDay() == 2, "getUTCDay" );
assert.ok( d.getUTCFullYear() == 2015, "getUTCFullYear" );
assert.ok( d.getUTCMilliseconds() == 260, "getUTCMilliseconds" );
assert.ok( d.getUTCMinutes() == 59, "getUTCMinutes" );
assert.ok( d.getUTCMonth() == 9, "getUTCMonth" );
assert.ok( d.getUTCSeconds() == 5, "getUTCSeconds" );

/*---------------------------------------------------------------------*/
/*    mutators                                                         */
/*---------------------------------------------------------------------*/
var d = new Date( s );

// Date
assert.ok( d.setDate( 0 ) == 1443639545260, "setDate" );

// FullYear
d = new Date( s );
assert.ok( d.setFullYear( 2015 ) == 1445367545260, "setFullYear" );
assert.ok( d.getFullYear() == 2015, "getFullYear" );

// MilliSeconds
d = new Date( s );
assert.ok( d.setMilliseconds( 0 ) == 1445367545000, "setMilliseconds" );
assert.ok( d.getMilliseconds() == 0, "getMilliseconds" );
assert.ok( d.setMilliseconds( 1000 ) == 1445367546000, "setMilliseconds" );
assert.ok( d.getMilliseconds() == 0, "getMilliseconds" );

// Seconds
d = new Date( s );
assert.ok( d.setSeconds( 0 ) == 1445367540260, "setSeconds" );
assert.ok( d.getSeconds() == 0, "getSeconds" );
assert.ok( d.setSeconds( 60 ) == 1445367600260, "setSeconds" );
assert.ok( d.getSeconds() == 0, "getSeconds" );

// Minutes
d = new Date( s );
assert.ok( d.setMinutes( 0 ) == 1445364005260, "setMinutes" );
assert.ok( d.getMinutes() == 0, "getMinutes" );
assert.ok( d.setMinutes( 60 ) == 1445367605260, "setMinutes" );
assert.ok( d.getMinutes() == 0, "getMinutes" );

// Hours
d = new Date( s );
assert.ok( d.setHours( 0 ) == 1445295545260, "setHours" );
assert.ok( d.getHours() == 0, "getHours" );

assert.ok( d.setHours( 24 ) == 1445381945260, "setHours" );
assert.ok( d.getHours() == 0, "getHours" );

// Month
d = new Date( s );
assert.ok( d.setMonth( 0 ) == 1421783945260, "setMonth" );
assert.ok( d.getMonth() == 0, "getMonth" );
assert.ok( d.setMonth( 12 ) == 1453319945260, "setMonth" );
assert.ok( d.getMonth() == 0, "getMonth" );

// Month + day
d = new Date( s );
assert.ok( d.setMonth( 0, 14 ) == 1421265545260, "setMonthDay" );
assert.ok( d.getMonth() == 0, "getMonth" );
assert.ok( d.setMonth( 12, 14 ) == 1452801545260, "setMonthDay" );
assert.ok( d.getMonth() == 0, "getMonth" );

// UTCDate
d = new Date( s );
assert.ok( d.setUTCDate( 0 ) == 1443639545260, "setUTCDate" );
assert.ok( d.getUTCDate() == 30, "getUTCDate" );
assert.ok( d.setUTCDate( 32 ) == 1443812345260, "setUTCDate" );
assert.ok( d.getUTCDate() == 2, "getUTCDate" );

// UTCFullYear
d = new Date( s );
assert.ok( d.setUTCFullYear( 2015 ) == 1445367545260, "setUTCFullYear" );
assert.ok( d.getUTCFullYear() == 2015, "getUTCFullYear" );

// UTCMilliSeconds
d = new Date( s );
assert.ok( d.setUTCMilliseconds( 0 ) == 1445367545000, "setUTCMilliseconds" );
assert.ok( d.getUTCMilliseconds() == 0, "getUTCMilliseconds" );
assert.ok( d.setUTCMilliseconds( 1000 ) == 1445367546000, "setUTCMilliseconds" );
assert.ok( d.getUTCMilliseconds() == 0, "getUTCMilliseconds" );

// UTCSeconds
d = new Date( s );
assert.ok( d.setUTCSeconds( 0 ) == 1445367540260, "setUTCSeconds" );
assert.ok( d.getUTCSeconds() == 0, "getUTCSeconds" );
assert.ok( d.setUTCSeconds( 60 ) == 1445367600260, "setUTCSeconds" );
assert.ok( d.getUTCSeconds() == 0, "getSeconds" );

// UTCMinutes
d = new Date( s );
assert.ok( d.setUTCMinutes( 0 ) == 1445364005260, "setUTCMinutes" );
assert.ok( d.getUTCMinutes() == 0, "getUTCMinutes" );
assert.ok( d.setUTCMinutes( 60 ) == 1445367605260, "setUTCMinutes" );
assert.ok( d.getUTCMinutes() == 0, "getUTCMinutes" );

// UTCHours
d = new Date( s );
assert.ok( d.setUTCHours( 0 ) == 1445302745260, "setUTCHours" );
assert.ok( d.getUTCHours() == 0, "getUTCHours" );

assert.ok( d.setUTCHours( 24 ) == 1445389145260, "setUTCHours" );
assert.ok( d.getUTCHours() == 0, "getUTCHours" );
assert.ok( d.getHours() == 2, "getHours" );

// UTCMonth
d = new Date( s );
assert.ok( d.setUTCMonth( 0 ) == 1421780345260, "setUTCMonth" );
assert.ok( d.getUTCMonth() == 0, "getUTCMonth" );
assert.ok( d.setUTCMonth( 12 ) == 1453316345260, "setUTCMonth" );
assert.ok( d.getUTCMonth() == 0, "getUTCMonth" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( d.toDateString() == "Tue Oct 20 2015", "toDateString" );
assert.ok( d.toISOString() == "2015-10-20T16:59:05.000Z", "toISOString" );
assert.ok( d.toJSON() == "2015-10-20T16:59:05.000Z", "toJSON" );
assert.ok( d.toUTCString() == "Tue, 20 Oct 2015 16:59:05 GMT", "toUTCString" );

assert.ok( d.valueOf() == 1445360345000, "valueOf" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setDate() ), 'setDate');
assert.ok( isNaN( d.getDate() ), 'getDate');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setFullYear() ), 'setFullYear');
assert.ok( isNaN( d.getFullYear() ), 'getFullYear');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setMilliseconds() ), 'setMilliseconds');
assert.ok( isNaN( d.getMilliseconds() ), 'getMilliseconds');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setSeconds() ), 'setSeconds');
assert.ok( isNaN( d.getSeconds() ), 'getSeconds');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setMinutes() ), 'setMinutes');
assert.ok( isNaN( d.getMinutes() ), 'getMinutes');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setHours() ), 'setHours');
assert.ok( isNaN( d.getHours() ), 'getHours');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setMonth() ), 'setMonth');
assert.ok( isNaN( d.getMonth() ), 'getMonth');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setTime() ), 'setTime');
assert.ok( isNaN( d.getTime() ), 'getTime');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setUTCDate() ), 'setUTCDate');
assert.ok( isNaN( d.getUTCDate() ), 'getUTCDate');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setUTCFullYear() ), 'setUTCFullYear');
assert.ok( isNaN( d.getUTCFullYear() ), 'getUTCFullYear');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setUTCMilliseconds() ), 'setUTCMilliseconds');
assert.ok( isNaN( d.getUTCMilliseconds() ), 'getUTCMilliseconds');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setUTCSeconds() ), 'setUTCSeconds');
assert.ok( isNaN( d.getUTCSeconds() ), 'getUTCSeconds');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setUTCMinutes() ), 'setUTCMinutes');
assert.ok( isNaN( d.getUTCMinutes() ), 'getUTCMinutes');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setUTCHours() ), 'setUTCHours');
assert.ok( isNaN( d.getUTCHours() ), 'getUTCHours');
assert.ok( d.toString(), "Invalid date", "toString" );

d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( isNaN( d.setUTCMonth() ), 'setUTCMonth');
assert.ok( isNaN( d.getUTCMonth() ), 'getUTCMonth');
assert.ok( d.toString(), "Invalid date", "toString" );

// constructors
d = new Date( Date.parse( "2015-10-20T18:59:05+02:00" ) );
assert.ok( d.getTime() === 1445360345000, "parse" );

d = new Date( 1332403882588 );
assert.ok( d.getTime() === 1332403882588, "milliseconds" );

d = new Date( 2015, 9, 19, 12, 22, 44, 999 );
assert.ok( d.getTime() === 1445250164999, "year+month+...+milli" );

d = new Date( 2015, 9, 19, 12, 22, 44 );
assert.ok( d.getTime() === 1445250164000, "year+month+...+seconds" );

d = new Date( 2015, 9, 19, 12, 22 );
assert.ok( d.getTime() === 1445250120000, "year+month+...+minutes" );

d = new Date( 2015, 9, 19, 12 );
assert.ok( d.getTime() === 1445248800000, "year+month+...+hours" );

d = new Date( 2015, 9, 19 );
assert.ok( d.getTime() === 1445205600000, "year+month+day" );

d = new Date( 2015, 9 );
assert.ok( d.getTime() === 1443650400000, "year+month" );

d = new Date( 2015 );
assert.ok( d.getTime() === 2015, "year" );

/*---------------------------------------------------------------------*/
/*    creation                                                         */
/*---------------------------------------------------------------------*/
var date = new Date();
date.setDate( 2.9 );

assert.ok( date.getDate() == 2 );

var invalid = new Date( undefined );
assert.ok( isNaN( invalid ) );
assert.ok( invalid != invalid.valueOf() );

/*---------------------------------------------------------------------*/
/*    utc                                                              */
/*---------------------------------------------------------------------*/
function toPostgresString( date ) {
   return date.getUTCFullYear() + '-'
      + (date.getUTCMonth()+1)
      + '-' + date.getUTCDate()
      + ' ' + date.getUTCHours()
      + ':' + date.getUTCMinutes()
      + ':' + date.getUTCSeconds();
}

var d = new Date( 2014, 10, 4, 00, 22, 45 );

assert.ok( toPostgresString( d ) === "2014-11-3 23:22:45", "UTC date" );

