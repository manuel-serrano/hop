/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/regexp.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Sun Oct 22 18:35:45 2023 (serrano)                */
/*    Copyright   :  2014-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing REGEXP matching                                          */
/*=====================================================================*/
"use strict";
									 
var assert = require("assert");

assert.strictEqual("toto\\tutu".match(/toto\\tutu/)[ 0 ], "toto\\tutu");
assert.ok(/[\uD800-\uDBFF]/.test('foo') ? true: true);

/*---------------------------------------------------------------------*/
/*    test                                                             */
/*---------------------------------------------------------------------*/
function rxTest(rx) {
   return rx.test("foobar");
}

console.log("rxTest"); 
assert.ok(rxTest(/[bc]/), "rxText literal");

console.log("rxTest"); 
assert.ok(rxTest(new RegExp("[bc]"), "rxText dynamic"));

/*---------------------------------------------------------------------*/
/*    exec ...                                                         */
/*---------------------------------------------------------------------*/
function rxExecMDN() {
   var res = "";
   var myRe = /ab*/g;
   var str = 'abbcdefabh';
   var myArray;
   while ((myArray = myRe.exec(str)) !== null) {
      var msg = 'Found ' + myArray[0] + '. ';
      msg += 'Next match starts at ' + myRe.lastIndex;
      res += msg;
   }
   
   return res === 
	     "Found abb. Next match starts at 3Found ab. Next match starts at 9";
}

function rxExecProps() {
   const r = /a(b)c/.exec("abc");
		  
   for (let k in r) {
      if (k !== "length") {
      	 const p = Object.getOwnPropertyDescriptor(r, k);
		
      	 if (!(p.writable && p.enumerable && p.configurable)) return false;
      }
   }
		  
   return true;
}

console.log("rxExecMDN"); 
assert.ok(rxExecMDN(), "exec");

console.log("rxExecProps"); 
assert.ok(rxExecProps(), "exec result properties");

/*---------------------------------------------------------------------*/
/*    rxReplace                                                        */
/*---------------------------------------------------------------------*/
function rxReplace() {
   const regex ='^(*)((?:[*+-]|\\d+\\.)) [\\s\\S]+?(?:hr|def|\\n{2,}(?!)(?!\\1(?:[*+-]|\\d+\\.))\\n*|\\s*$)';
   const name = 'hr';
   const val = '\\n+(?=\\1?(?:(?:- *){3,}|(?:_ *){3,}|(?:\\* *){3,})(?:\\n+|$))';
   const val2 = val.replace(/(^|[^\[])\^/g, '$1');

   const regex2 = regex.replace(name, val2);

   return val2 === "\\n+(?=\\1?(?:(?:- *){3,}|(?:_ *){3,}|(?:\\* *){3,})(?:\\n+|$))"
      && regex2 === "^(*)((?:[*+-]|\\d+\\.)) [\\s\\S]+?(?:\\n+(?=\\1?(?:(?:- *){3,}|(?:_ *){3,}|(?:\\* *){3,})(?:\\n+|$))|def|\\n{2,}(?!)(?!\\1(?:[*+-]|\\d+\\.))\\n*|\\s*$)";
}

function rxReplace2() {
   const regex ='hr';
   const name = 'hr';
   const val = "\\n+|$)";

   const rx2 = regex.replace(name, val);

   return rx2 === "\\n+|$)";
}

function rxReplace3() {
   const regex ='hr';
   const name = 'hr';
   const val = "n+|$)";

   const rx2 = regex.replace(name, val);

   return rx2 === "n+|$)";
}

function rxReplace4() {
   function escCtrlChars(str) {
      return str.replace(/[\0\t\n\v\f\r\xa0'"!-]/g, function(c) {
      	 return '!' + c.charCodeAt(0) + '!'; });
   } 
   const enc = 
      encodeURI(escCtrlChars(String.fromCharCode(21,219,229,218,160)));
   return enc === "%15%C3%9B%C3%A5%C3%9A!160!";
}



console.log("rxReplace");
assert.ok(rxReplace(), "regexp replace");

console.log("rxReplace2");
assert.ok(rxReplace2(), "regexp replace2");

console.log("rxReplace3");
assert.ok(rxReplace3(), "regexp replace3");

console.log("rxReplace4");
assert.ok(rxReplace4(), "regexp replace4");

/*---------------------------------------------------------------------*/
/*    properties                                                       */
/*---------------------------------------------------------------------*/
function rxProperties() {
   const rx = /foo|bar/;
   
   return !Object.getOwnPropertyDescriptor(rx, "global") 
      && !Object.getOwnPropertyDescriptor(rx, "source")
      && !Object.getOwnPropertyDescriptor(rx, "multiline")
      && !Object.getOwnPropertyDescriptor(rx, "ignoreCase")
      && Object.getOwnPropertyDescriptor(rx, "lastIndex");
}

console.log("rxProperties");
assert.ok(rxProperties(), "regexp properties");

/*---------------------------------------------------------------------*/
/*    rxSticky                                                         */
/*---------------------------------------------------------------------*/
function rxSticky() {
   const re = /[a-z]+/y;
   const buffer = "foo bar gee";

   const m1 = buffer.match(re);
   re.sticky = 4;
   const m2 = buffer.match(re);
   re.sticky = 8;
   const m3 = buffer.match(re);

   return m1[0] === "foo" && m2[0] === "bar" && m3[0] === "gee";
}

console.log("rxSticky");
assert.ok(rxSticky(), "regexp sticky");
   

