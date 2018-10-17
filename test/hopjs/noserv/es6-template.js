/*=====================================================================*/
/*    .../prgm/project/hop/3.1.x/test/hopjs/noserv/es6-template.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Aug 14 09:43:13 2015                          */
/*    Last change :  Tue Feb  6 14:36:49 2018 (serrano)                */
/*    Copyright   :  2015-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ES6 template strings                                     */
/*=====================================================================*/
"use strict";
"use hopscript";

var assert = require( "assert" );

console.log( "basic" );

// simple tests
assert.strictEqual( `toto
tutu`, "toto\ntutu" );

var res = "";
var expect = "";
   
for( var i = 0; i < 3; i++ ) {
   res += `res-${i + 10}-${i}`;
   expect += "res-" + ((i + 10) + "") + "-" + i;
}

assert.strictEqual( res, expect );

// tag
function bar( strs, val0, val1, val2 ) {
   assert.deepEqual( strs, [ "foo$", "", "bar$", "" ] );
   assert.equal( val0, 0 );
   assert.equal( val1, 1 );
   assert.equal( val2, 2 );
   return strs;
}
bar`foo$${0}${1}bar$${2}`;

// $$
assert.strictEqual( `foo$`, "foo$" );
assert.strictEqual( `foo$$`, "foo$$" );
assert.strictEqual( `foo$bar`, "foo$bar" );
assert.strictEqual( `foo$$bar`, "foo$$bar" );
assert.strictEqual( `foo$${1+0}`, "foo$1" );
assert.strictEqual( `foo$${1+0}$${2+0}`, "foo$1$2" );
assert.strictEqual( `foo$${1+0}$${2+0}$$`, "foo$1$2$$" );
assert.strictEqual( `foo$${1+0}$${2+0}$`, "foo$1$2$" );

// raw
String.raw( `foo$${1+0}$${2+0}$`, `foo$${1+0}$${2+0}$` );

// errors
assert.throws( function() { eval( "`foo${`" ) } );

// esacape
assert.strictEqual( `\``, "`" );
assert.strictEqual( `\`foo\``, "`foo`" );
assert.strictEqual( `\`${1+2}\``, "`3`" );
assert.strictEqual( `fo\`o$${1+0}$${2+0}$`, "fo`o$1$2$" );

/*---------------------------------------------------------------------*/
/*    Kangax                                                           */
/*    -------------------------------------------------------------    */
/*    https://kangax.github.io/compat-table/es6/                       */
/*---------------------------------------------------------------------*/
console.log( "kangax" );

function kangaxa() {
   var a = "ba", b = "QUX";
   return `foo bar
${a + "z"} ${b.toLowerCase()}` === "foo bar\nbaz qux";
}

function kangaxb() {
   var a = {
      toString: function() { return "foo"; },
      valueOf: function() { return "bar"; },
   };
   return `${a}` === "foo";
}

function kangaxc() {
   var called = false;
   function fn(parts, a, b) {
      called = true;
      return parts instanceof Array &&
	 parts[0]     === "foo"      &&
	 parts[1]     === "bar\n"    &&
	 parts.raw[0] === "foo"      &&
	 parts.raw[1] === "bar\\n"   &&
	 a === 123                   &&
	 b === 456;
   }
   return fn `foo${123}bar\n${456}` && called;
}

function kangaxd() {
   return (function(parts) {
      return Object.isFrozen(parts) && Object.isFrozen(parts.raw);
   }) `foo${0}bar${0}baz`;   
}

function kangaxe() {
   var cr   = eval("`x" + String.fromCharCode(13)    + "y`");
   var lf   = eval("`x" + String.fromCharCode(10)    + "y`");
   var crlf = eval("`x" + String.fromCharCode(13,10) + "y`");

   return cr.length === 3 && lf.length === 3 && crlf.length === 3
      && cr[1] === lf[1] && lf[1] === crlf[1] && crlf[1] === '\n';
}

console.log( "   kangaxa()");
assert.ok( kangaxa() );

console.log( "   kangaxb()");
assert.ok( kangaxb() );

console.log( "   kangaxc()");
assert.ok( kangaxc() );

console.log( "   kangaxd()");
assert.ok( kangaxd() );

console.log( "   kangaxe()");
assert.ok( kangaxe() );

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/template_strings
var a = 5;
var b = 10;

function tag(strings, ...values) {
   var s = ""
   s += (strings[0]); // "Hello "
   s += (strings[1]); // " world "
   s += (values[0]);  // 15
   s += (values[1]);  // 50

  return s + ", Bazinga!";
}

function template(strings, ...keys) {
  return (function(...values) {
    var dict = values[values.length - 1] || {};
    var result = [strings[0]];
     keys.forEach(function(key, i, _) {
      var value = Number.isInteger(key) ? values[key] : dict[key];
      result.push(value, strings[i + 1]);
    });
    return result.join('');
  });
}

function mkraw(strings, ...values) {
   return strings.raw[0];
}

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
console.log( "mdn" );

console.log( "   tag()" );
assert.equal( tag`Hello ${ a + b } world ${ a * b }`,
	      "Hello  world 1550, Bazinga!" );

console.log( "   template()" );
assert.equal( template`${0}${1}${0}!`('Y', 'A'), "YAY!" );
assert.equal( template`${0} ${'foo'}!`('Hello', {foo: 'World'}), "Hello World!" );

console.log( "   raw()" );
assert.equal( mkraw`string text line 1 \n string text line 2`,
	      "string text line 1 \\n string text line 2" );
assert.equal( String.raw`Hi\n${2+3}!`, "Hi\\n5!" );
