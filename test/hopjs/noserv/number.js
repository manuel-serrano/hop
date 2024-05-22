/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/number.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:25 2014                          */
/*    Last change :  Wed May 22 07:19:04 2024 (serrano)                */
/*    Copyright   :  2014-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing JS numbers                                               */
/*=====================================================================*/
var assert = require("assert");

/*---------------------------------------------------------------------*/
/*    associativity                                                    */
/*---------------------------------------------------------------------*/
assert.equal(2+3*4, 2+(3*4), "priority.1");
assert.equal(2*3+4, (2*3)+4, "priority.2");

/*---------------------------------------------------------------------*/
/*    toFixed                                                          */
/*---------------------------------------------------------------------*/
var a = -0.14445826138806367; 
assert.equal(a.toFixed(2), "-0.14", "toFixed1");

var b = 0.14445826138806367; 
assert.equal(b.toFixed(2), "0.14", "toFixed2");

var c = -1.14445826138806367; 
assert.equal(c.toFixed(2), "-1.14", "toFixed3");

var d = 1.14445826138806367; 
assert.equal(d.toFixed(2), "1.14", "toFixed4");

var e = -145564.14445826138806367; 
assert.equal(e.toFixed(2), "-145564.14", "toFixed5");

var f = 6541.14445826138806367; 
assert.equal(f.toFixed(2), "6541.14", "toFixed6");

var g = 0.07;
assert.equal(g.toFixed(3), "0.070", "toFixed7");

var h = -0.07;
assert.equal(h.toFixed(3), "-0.070", "toFixed8");

var i = 0.07;
assert.equal(i.toFixed(1), "0.1", "toFixed9");

var j = -0.07;
assert.equal(h.toFixed(1), "-0.1", "toFixed10");

var k = 0.07;
assert.equal(k.toFixed(2), "0.07", "toFixed11");

var l = -0.07;
assert.equal(l.toFixed(2), "-0.07", "toFixed12");

var m = 0.07;
assert.equal(m.toFixed(4), "0.0700", "toFixed13");

var n = -0.07;
assert.equal(n.toFixed(4), "-0.0700", "toFixed14");

var o = 0.9;
assert.ok(1.0 === o +  0.1, "float.eq");
assert.ok(1 === o + 0.1, "float-int.eq");

var p = 0.51;
assert.ok(p.toFixed(2), "0.51");

var q = 0.49;
assert.ok(q.toFixed(2), "0.49");

assert.ok(Math.abs(null) === 0, "Math.abs(null)");

/*---------------------------------------------------------------------*/
/*    toExponential                                                    */
/*---------------------------------------------------------------------*/
let exp = 123456;
assert.equal(exp.toExponential(2), "1.23e+5");
assert.equal(exp.toExponential(), "1.23456e+5");
assert.equal(Number.parseFloat("oink").toExponential(), "NaN");

exp = 77.1234;
assert.equal(exp.toExponential(), "7.71234e+1");
assert.equal(exp.toExponential(4), "7.7123e+1");
assert.equal(exp.toExponential(5), "7.71234e+1");
assert.equal(exp.toExponential(2), "7.71e+1");

exp = 77;
assert.equal(exp.toExponential(), "7.7e+1");

/*---------------------------------------------------------------------*/
/*    toPrecision                                                      */
/*---------------------------------------------------------------------*/
let pre = 0;
assert.equal(pre.toPrecision(1), "0");
assert.equal(pre.toPrecision(2), "0.0");
assert.equal(pre.toPrecision(10), "0.000000000");

pre = -pre;
assert.equal(pre.toPrecision(10), "0.000000000");

pre = 3.141592;
assert.equal(pre.toPrecision(1), "3");
pre = 31.41592;
assert.equal(pre.toPrecision(2), "31");

pre = 123.456;
assert.equal(pre.toPrecision(1), "1e+2");
assert.equal(pre.toPrecision(3), "123");
assert.equal(pre.toPrecision(4), "123.5");
assert.equal(pre.toPrecision(5), "123.46");

pre = 123123.456;
assert.equal(pre.toPrecision(1), "1e+5");
assert.equal(pre.toPrecision(3), "1.23e+5");
assert.equal(pre.toPrecision(4), "1.231e+5");
assert.equal(pre.toPrecision(5), "1.2312e+5");
assert.equal(pre.toPrecision(10), "123123.4560");

pre = 0.004;
assert.equal(pre.toPrecision(4), "0.004000");

pre = 1.23e5
assert.equal(pre.toPrecision(4), "1.230e+5");

pre = 5.123456;
assert.equal(pre.toPrecision(), "5.123456");
assert.equal(pre.toPrecision(5), "5.1235");
assert.equal(pre.toPrecision(2), "5.1");
assert.equal(pre.toPrecision(1), "5");

pre = 0.000123;
assert.equal(pre.toPrecision(), "0.000123");
assert.equal(pre.toPrecision(5), "0.00012300");
assert.equal(pre.toPrecision(2), "0.00012");
assert.equal(pre.toPrecision(1), "0.0001");

pre = 1234.5;
assert.equal(pre.toPrecision(2), "1.2e+3");

pre = 12345;
assert.equal(pre.toPrecision(3), "1.23e+4");

/*---------------------------------------------------------------------*/
/*    integer limits                                                   */
/*---------------------------------------------------------------------*/
var xxx = 4294967295;

assert.equal(xxx, +xxx, "+x");
assert.equal(xxx, Math.ceil(xxx), "ceil");

assert.ok(Math.floor(2147483647.0) > 0, "floor");

var b = [ 0xff, 0xff, 0xff, 0xff ];

function fooMaxint(b) {
  return ((b[0]) | (b[1] << 8) | (b[2] << 16)) + (b[3] * 0x1000000);
}

assert.ok(fooMaxint(b) === 0xFFFFFFFF, "maxint");

/*---------------------------------------------------------------------*/
/*    parsing                                                          */
/*---------------------------------------------------------------------*/
assert.equal(88888888888888888888888888888888888888888, 8.888888888888888e+40);

/*---------------------------------------------------------------------*/
/*    bitops                                                           */
/*---------------------------------------------------------------------*/
assert.equal(4 << 0, 4, "4 << 0");
assert.equal(1 << 28, 268435456, "1 << 28");
assert.equal(1 << 29, 536870912, "1 << 29");
assert.equal(1 << 31, -2147483648, "1 << 31");
assert.equal(1 << 32, 1, "1 << 32");
assert.equal(-2147483648 << 0, -2147483648, "-2147483648 << 0");
assert.equal(-2147483648 << 1, 0, "-2147483648 << 1");
assert.equal((1 << 31) + 123, -2147483525, "(1 << 31) + 123");
assert.equal(Math.pow(2, 32), 4294967296, "Math.pow(2");
assert.equal(Math.pow(2, 32) + 123, 4294967419, "Math.pow(2");
assert.equal(Math.pow(2, 33), 8589934592, "Math.pow(2");
assert.equal(((1<<29) + 123) << 2, -2147483156, "((1<<29) + 123) << 2");
assert.ok(Math.pow(10, 2.5) > 316, "Math.pow");

/*---------------------------------------------------------------------*/
/*    maxint parsing                                                   */
/*---------------------------------------------------------------------*/
assert.equal(parseInt("4294967295", 10), 4294967295, "parseMaxInt.1");
assert.equal(parseInt("4294967295"), 4294967295, "parseMaxInt.2");
assert.equal(parseInt("ffffffff", 16), 4294967295, "parseMaxInt.3");

/*---------------------------------------------------------------------*/
/*    tostring                                                         */
/*---------------------------------------------------------------------*/
var num1 = 0xFFFFFFFF;
var num2 = parseInt("FFFFFFFF", 16);

assert.equal(num1.toString(16), "ffffffff");
assert.equal(num2.toString(16), "ffffffff");

/*---------------------------------------------------------------------*/
/*    switch                                                           */
/*---------------------------------------------------------------------*/
function testS(a) {
   switch(a) {
      case 0: return 28;
      case 1: return "un";
      case 2: return false;
      default: return true;
   }
}

assert.equal(testS(0), 28);
assert.equal(testS(1), "un");
assert.equal(testS(2), false);
assert.equal(testS(3), true);
assert.equal(testS(4), true);

/*---------------------------------------------------------------------*/
/*    casts                                                            */
/*---------------------------------------------------------------------*/
function fooCast() {
   var a = [1,2,3];
   
   return(a.length/2 >> 0);
}

assert.equal(fooCast(), 1, "integer cast");

/*---------------------------------------------------------------------*/
/*    unary                                                            */
/*---------------------------------------------------------------------*/
function plus(x) {
   return +x;
}

assert.equal(plus(0), 0, "plus 0");
assert.equal(plus(0.0), 0.0, "plus 0.0");
assert.equal(plus(1), 1, "plus 1");
assert.equal(plus(1.2), 1.2, "plus 1.2");
assert.equal(plus("2"), 2, "plus \"2\"");
assert.equal(plus("-1.2"), -1.2, "plus \"-1.2\"");
assert.ok(isNaN(plus("a")), "plus \"a\"");
assert.equal(plus(-1), -1, "plus -1");
assert.equal(plus(-1.2), -1.2, "plus -1.2");

function minus(x) {
   return -x;
}

assert.equal(minus(0), -0.0, "minus 0");
assert.equal(minus(0.0), -0.0, "minus 0.0");
assert.equal(minus(1), -1, "minus -1");
assert.equal(minus(1.2), -1.2, "minus -1.2");
assert.equal(minus("1"), -1, "minus \"1\"");
assert.equal(minus("1.4"), -1.4, "minus \"1.4\"");
assert.ok(isNaN(minus("a")), "minus \"a\"");
assert.equal(minus(-1), 1, "minus 1");
assert.equal(minus(-1.2), 1.2, "minus -1.2");

