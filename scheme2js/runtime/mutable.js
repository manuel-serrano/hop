/*=====================================================================*/
/*    Author      :  Florian Loitsch                                   */
/*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
/*    -------------------------------------------------------------    */
/*    This file is part of Scheme2Js.                                  */
/*                                                                     */
/*   Scheme2Js is distributed in the hope that it will be useful,      */
/*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
/*   LICENSE file for more details.                                    */
/*=====================================================================*/

function sc_String(s) {
    this.val = s;
}
sc_String.prototype.toString = function() {
    return this.val;
};
sc_String.prototype.sc_getHash = function() {
    return "str-" + this.val;
};
sc_String.prototype.sc_toWriteString = function() {
    return '"' + sc_escapeWriteString(this.val) + '"';
};

// Keywords
function sc_Keyword(str) {
    var cached = sc_Keyword.lazy[str];

    if (cached)
	return cached;

    sc_Keyword.lazy[str] = this;
    this.val = str;
    // add return so FF does not complain.
    return undefined;
}
sc_Keyword.lazy = new Object;

sc_Keyword.prototype.toString = function() {
    return ":" + this.val;
};

/*** META ((export #t)
           (arity #t)
           (type bool)
           (peephole (postfix " instanceof sc_Keyword")))
*/
function sc_isKeyword(o) {
    return (o instanceof sc_Keyword);
}



/*** META ((export #t)
           (arity #t)
           (peephole (hole 1 "new sc_String(" s ")")))
*/
function sc_jsstring2string(s) {
    return new sc_String(s);
}

/*** META ((export #t)
           (arity #t)
           (peephole (id)))
*/
function sc_jsstring2symbol(s) {
    return s;
}

/*** META ((export #t)
           (arity #t)
           (peephole (string2jsstring_mutable)))
*/
function sc_string2jsstring(s) {
    return s.val;
}

/*** META ((export #t)
           (arity #t)
           (peephole (id)))
*/
function sc_symbol2jsstring(s) {
    return s;
}

/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".val")))
*/
function sc_keyword2jsstring(o) {
    return o.val;
}

/*** META ((export #t)
           (arity #t)
           (peephole (hole 1 "new sc_Keyword(" s ")")))
*/
function sc_jsstring2keyword(s) {
    return new sc_Keyword(s);
}

/*** META ((export #t) (arity -1)) */
var sc_gensym = function() {
    var counter = 1000;
    return function(sym) {
	counter++;
	if (!sym) sym = "";
	return "s" + counter + "~" + sym + "^sC-GeNsYm ";
    };
}();

/*** META ((export #t)
           (arity #t)
           (type bool))
*/
function sc_isEqual(o1, o2) {
    return ((o1 === o2) ||
	    (sc_isPair(o1) && sc_isPair(o2)
	     && sc_isPairEqual(o1, o2, sc_isEqual)) ||
	    (sc_isVector(o1) && sc_isVector(o2)
	     && sc_isVectorEqual(o1, o2, sc_isEqual)) ||
	    (sc_isString(o1) && sc_isString(o2)
	     && sc_isStringEqual(o1, o2)));
}

/*** META ((export number->symbol integer->symbol)
           (arity -2)
           (peephole (prefix "''+")))
           ;; peephole will only apply if no radix is given.
*/
var sc_number2symbol = sc_number2jsstring;

/*** META ((export number->string integer->string)
           (arity -2)
           (peephole (hole 1 "new sc_String(''+" x ")")))
           ;; peephole will only apply if no radix is given.
*/
function sc_number2string(x, radix) {
    return new sc_String(sc_number2jsstring(x, radix));
}

/*** META ((export #t) (arity -2)) */
var sc_symbol2number = sc_jsstring2number;

/*** META ((export #t) (arity -2)) */
function sc_string2number(s, radix) {
    return sc_symbol2number(s.val, radix);
}

/*** META ((export #t)
           (arity -2)
           (peephole (hole 1 "+" s ".val")))
           ;; peephole will only apply if no radix is given.
*/
function sc_string2integer(s, radix) {
    if (!radix) return +s.val;
    return parseInt(s.val, radix);
}

/*** META ((export #t)
           (arity #t)
           (peephole (hole 1 "+" s ".val")))
*/
function sc_string2real(s) {
    return +s.val;
}

/*** META ((export #t)
           (arity #t)
	   (type bool)
           (peephole (hole 1 "typeof " s " === 'string'")))
*/
function sc_isSymbol(s) {
    return (typeof s === "string");
}

/*** META ((export #t)
           (peephole (hole 1 "new sc_String(" s ")")))
*/
function sc_symbol2string(s) {
    return new sc_String(s);
}

/*** META ((export string->symbol)
           (arity #t)
           (peephole (string2symbol_mutable)))
*/
function sc_string2symbol(s) {
    return s.val;
}

/*** META ((export #t)
           (arity -1)
           (peephole (infix 0 #f "+" "''")))
*/
function sc_symbolAppend() {
    return "".concat.apply("", arguments);
}

/*** META ((export #t)
           (arity #t)
           (peephole (hole "new sc_String(" c ".val)")))
*/
function sc_char2string(c) { return new sc_String(c.val); }

/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".val")))
*/
function sc_char2symbol(c) { return c.val; }

/*** META ((export #t)
           (arity #t)
           (type bool)
           (peephole (postfix " instanceof sc_String")))
*/
function sc_isString(s) { return (s instanceof sc_String); }

/*** META ((export #t) (arity -2)) */
function sc_makeString(k, c) {
    return new sc_String(sc_makejsString(k, c));
}


/*** META ((export #t) (arity -1)) */
function sc_string() {
    for (var i = 0; i < arguments.length; i++)
	arguments[i] = arguments[i].val;
    return new sc_String("".concat.apply("", arguments));
}

/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".val.length"))) */
function sc_stringLength(s) { return s.val.length; }

/*** META ((export #t) (arity #t)) */
function sc_stringRef(s, k) {
    return new sc_Char(s.val.charAt(k));
}

/*** META ((export #t) (arity #t)) */
function sc_stringSetBang(s, k, c) {
    var start = s.val.slice(0, k);
    var end = s.val.slice(k+1);
    s.val = start.concat(c.val, end);
}

/*** META ((export string=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 str1 ".val === " str2 ".val")))
*/
var sc_isStringEqual = sc_isCharStringEqual;
/*** META ((export string<?)
           (arity 2)
           (type bool)
           (peephole (hole 2 str1 ".val < " str2 ".val")))
*/
var sc_isStringLess = sc_isCharStringLess;
/*** META ((export string>?)
           (arity 2)
           (type bool)
           (peephole (hole 2 str1 ".val > " str2 ".val")))
*/
var sc_isStringGreater = sc_isCharStringGreater;
/*** META ((export string<=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 str1 ".val <= " str2 ".val")))
*/
var sc_isStringLessEqual = sc_isCharStringLessEqual;
/*** META ((export string>=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 str1 ".val >= " str2 ".val")))
*/
var sc_isStringGreaterEqual = sc_isCharStringGreaterEqual;
/*** META ((export string-ci=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 str1 ".val.toLowerCase() === " str2 ".val.toLowerCase()")))
*/
var sc_isStringCIEqual = sc_isCharStringCIEqual;
/*** META ((export string-ci<?)
           (arity 2)
           (type bool)
           (peephole (hole 2 str1 ".val.toLowerCase() < " str2 ".val.toLowerCase()")))
*/
var sc_isStringCILess = sc_isCharStringCILess;
/*** META ((export string-ci>?)
           (arity 2)
           (type bool)
           (peephole (hole 2 str1 ".val.toLowerCase() > " str2 ".val.toLowerCase()")))
*/
var sc_isStringCIGreater = sc_isCharStringCIGreater;
/*** META ((export string-ci<=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 str1 ".val.toLowerCase() <= " str2 ".val.toLowerCase()")))
*/
var sc_isStringCILessEqual = sc_isCharStringCILessEqual;
/*** META ((export string-ci>=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 str1 ".val.toLowerCase() >= " str2 ".val.toLowerCase()")))
*/
var sc_isStringCIGreaterEqual = sc_isCharStringCIGreaterEqual;


/*** META ((export #t)
           (arity #t)
           (peephole (hole 3 "new sc_String(" s ".val.substring(" start ", " end "))")))
*/
function sc_substring(s, start, end) {
    return new sc_String(s.val.substring(start, end));
}

/*** META ((export #t) (arity -4))
*/
function sc_isSubstring_at(s1, s2, i, len) {
    var str1 = s1.val;
    var str2 = s2.val;
    if (!len) len = str2.length;
    else if (str2.length < len) return false;
    if (str1.length < len + i) return false;
    return str2.substring(0, len) == str1.substring(i, i+len);
}

/*** META ((export substring=?)
           (arity #t))
*/
function sc_isSubstring(s1, s2, len) {
    if (s1.val.length < len) return false;
    if (s2.val.length < len) return false;
    return s2.val.substring(0, len) == s1.val.substring(0, len);
}

/*** META ((export #t) (arity -1)
           (peephole (stringAppend_mutable)))
*/
function sc_stringAppend() {
    for (var i = 0; i < arguments.length; i++)
	arguments[i] = arguments[i].val;
    return new sc_String("".concat.apply("", arguments));
}

/*** META ((export #t) (arity #t)
           (peephole (hole 1 "sc_jsstring2list(" s ".val)")))
*/
function sc_string2list(s) {
    return sc_jsstring2list(s.val);
}

/*** META ((export list->string) (arity #t)
           (peephole (hole 1 "new sc_String(sc_list2jsstring(" l "))")))
*/
function sc_list2string(l) {
    return new sc_String(sc_list2jsstring(l));
}

/*** META ((export #t) (arity #t)
           (peephole (hole 1 "new sc_String(" s ".val)")))
*/
function sc_stringCopy(s) {
    return new sc_String(s.val);
}


/*** META ((export #t) (arity #t)) */
function sc_stringFillBang(s, c) {
    s.val = sc_makeJSStringOfLength(s.val.length, c.val);
}

/*** META ((export #t) (arity #t)
           (peephole (hole 1 "new sc_String(" o ".val)")))
*/
function sc_keyword2string(o) {
    return new sc_String(o.val);
}

/*** META ((export #t) (arity #t)
           (peephole (hole 1 "new sc_Keyword(" o ".val)")))
*/
function sc_string2keyword(o) {
    return new sc_Keyword(o.val);
}

/*** META ((export #t) (arity #t)
           (peephole (hole 2 1 ".val.indexOf(" 0 ".val) === 0")))
*/
function sc_isStringPrefix(cs1, cs2) {
    return cs2.val.indexOf(cs1.val) === 0;
}

/*** META ((export #t) (arity #t)) */
function sc_isStringSuffix(cs1, cs2) {
    var s1 = cs1.val;
    var s2 = cs2.val;
    var tmp = s2.lastIndexOf(s1);
    return tmp !== false && tmp >= 0 && tmp === s2.length - s1.length;
}

/*** META ((export #t) (arity #t)) */
function sc_stringSplit(s, sep) {
    if (sep.val.length === 1)
	return sc_vector2list(s.val.split(sep.val));
    return sc_vector2list(s.val.split(sc_pregexpCreateCharsetMatcher(sep.val)));
}

/*** META ((export #t) (arity -3)) */
function sc_stringIndex(s, cset, start) {
   var res;
   if (!start) start = 0;
   
   if (cset instanceof sc_Char) {
      res = s.val.indexOf(sc_char2string(cset), start);
      return res >= 0 ? res : false;
   }
   if (cset.val.length == 1) {
      res = s.val.indexOf(cset.val, start);
      return res >= 0 ? res : false;
   } else {
      for (var i = start; i < s.val.length; i++ ) {
	 if (cset.val.indexOf(s.val.charAt(i)))
	    return i;
      }

      return false;
   }
}

/*** META ((export #t) (arity -3)) */
function sc_stringIndexRight(s, cset, start) {
   var res;
   if (!start) start = s.val.length - 1;
   
   if (cset instanceof sc_Char) {
      res = s.val.lastIndexof(sc_char2string(cset), start);
      return res >= 0 ? res : false;
   }
   if (cset.val.length == 1) {
      res = s.val.lastIndexOf(cset.val, start);
      return res >= 0 ? res : false;
   } else {
      for (var i = start; i >= 0; i-- ) {
	 if (cset.val.indexOf(s.val.charAt(i)))
	    return i;
      }

      return false;
   }
}
