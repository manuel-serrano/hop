/*=====================================================================*/
/*    Author      :  Florian Loitsch                                   */
/*    Copyright   :  2007-10 Florian Loitsch, see LICENSE file         */
/*    -------------------------------------------------------------    */
/*    This file is part of Scheme2Js.                                  */
/*                                                                     */
/*   Scheme2Js is distributed in the hope that it will be useful,      */
/*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
/*   LICENSE file for more details.                                    */
/*=====================================================================*/

var sc_SYMBOL_PREFIX = "\uEBAC";
var sc_KEYWORD_PREFIX = "\uEBAD";

/*** META ((export #t) (arity #t)
           (peephole (id))) */
function sc_jsstring2string(s) {
    return s;
}

/*** META ((export #t) (arity #t)
           (peephole (prefix "'\\uEBAC' +")))
*/
function sc_jsstring2symbol(s) {
    return sc_SYMBOL_PREFIX + s;
}

/*** META ((export #t) (arity #t)
           (peephole (id)))
*/
function sc_string2jsstring(s) {
    return s;
}

/*** META ((export #t) (arity #t)
           (peephole (symbol2jsstring_immutable)))
*/
function sc_symbol2jsstring(s) {
    return s.slice(1);
}

/*** META ((export #t) (arity #t)
           (peephole (postfix ".slice(1)")))
*/
function sc_keyword2jsstring(k) {
    return k.slice(1);
}

/*** META ((export #t) (arity #t)
           (peephole (prefix "'\\uEBAD' +")))
*/
function sc_jsstring2keyword(s) {
    return sc_KEYWORD_PREFIX + s;
}

/*** META ((export #t)
           (arity #t)
           (type bool))
*/
function sc_isKeyword(s) {
    return (typeof s === "string") &&
	(s.charAt(0) === sc_KEYWORD_PREFIX);
}


/*** META ((export #t) (arity -1)) */
var sc_gensym = function() {
    var counter = 1000;
    return function(sym) {
	counter++;
	if (!sym) sym = sc_SYMBOL_PREFIX;
	return sym + "s" + counter + "~" + "^sC-GeNsYm ";
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
	     && sc_isVectorEqual(o1, o2, sc_isEqual)));
}

/*** META ((export number->symbol integer->symbol) (arity -2)) */
function sc_number2symbol(x, radix) {
    return sc_SYMBOL_PREFIX + sc_number2jsstring(x, radix);
}
    
/*** META ((export number->string integer->string) (arity -2)) */
var sc_number2string = sc_number2jsstring;

/*** META ((export #t) (arity -2)) */
function sc_symbol2number(s, radix) {
    return sc_jsstring2number(s.slice(1), radix);
}

/*** META ((export #t) (arity -2)) */
var sc_string2number = sc_jsstring2number;

/*** META ((export #t)
           (arity -2)
           (peephole (prefix "+")))
           ;; peephole will only apply if no radix is given.
*/
function sc_string2integer(s, radix) {
    if (!radix) return +s;
    return parseInt(s, radix);
}

/*** META ((export #t)
           (arity #t)
           (peephole (prefix "+")))
*/
function sc_string2real(s) {
    return +s;
}


/*** META ((export #t)
           (arity #t)
           (type bool))
*/
function sc_isSymbol(s) {
    return (typeof s === "string") &&
	(s.charAt(0) === sc_SYMBOL_PREFIX);
}

/*** META ((export #t)
           (arity #t)
           (peephole (symbol2string_immutable)))
*/
function sc_symbol2string(s) {
    return s.slice(1);
}

/*** META ((export #t)
           (arity #t)
           (peephole (prefix "'\\uEBAC' +")))
*/
function sc_string2symbol(s) {
    return sc_SYMBOL_PREFIX + s;
}

/*** META ((export symbol-append)
           (arity -1)
           (peephole (symbolAppend_immutable)))
*/
function sc_symbolAppend() {
    var res = sc_SYMBOL_PREFIX;
    for (var i = 0; i < arguments.length; i++)
	res += arguments[i].slice(1);
    return res;
}

/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".val")))
*/
function sc_char2string(c) { return c.val; }

/*** META ((export #t)
           (arity #t)
           (peephole (hole 1 "'\\uEBAC' + " c ".val")))
*/
function sc_char2symbol(c) { return sc_SYMBOL_PREFIX + c.val; }

/*** META ((export #t)
           (arity #t)
           (type bool))
*/
function sc_isString(s) {
    return (typeof s === "string") &&
	(s.charAt(0) !== sc_SYMBOL_PREFIX);
}

/*** META ((export #t) (arity -2)) */
var sc_makeString = sc_makejsString;


/*** META ((export #t) (arity -1)) */
function sc_string() {
    for (var i = 0; i < arguments.length; i++)
	arguments[i] = arguments[i].val;
    return "".concat.apply("", arguments);
}

/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".length")))
*/
function sc_stringLength(s) { return s.length; }

/*** META ((export #t) (arity #t)) */
function sc_stringRef(s, k) {
    return new sc_Char(s.charAt(k));
}

/* there's no stringSet in the immutable version
function sc_stringSet(s, k, c)
*/


/*** META ((export string=?)
           (arity #t)
	   (type bool)
           (peephole (hole 2 str1 " === " str2)))
*/
function sc_isStringEqual(s1, s2) {
    return s1 === s2;
}
/*** META ((export string<?)
           (arity #t)
	   (type bool)
           (peephole (hole 2 str1 " < " str2)))
*/
function sc_isStringLess(s1, s2) {
    return s1 < s2;
}
/*** META ((export string>?)
           (arity #t)
	   (type bool)
           (peephole (hole 2 str1 " > " str2)))
*/
function sc_isStringGreater(s1, s2) {
    return s1 > s2;
}
/*** META ((export string<=?)
           (arity #t)
	   (type bool)
           (peephole (hole 2 str1 " <= " str2)))
*/
function sc_isStringLessEqual(s1, s2) {
    return s1 <= s2;
}
/*** META ((export string>=?)
           (arity #t)
	   (type bool)
           (peephole (hole 2 str1 " >= " str2)))
*/
function sc_isStringGreaterEqual(s1, s2) {
    return s1 >= s2;
}
/*** META ((export string-ci=?)
           (arity #t)
	   (type bool)
           (peephole (hole 2 str1 ".toLowerCase() === " str2 ".toLowerCase()")))
*/
function sc_isStringCIEqual(s1, s2) {
    return s1.toLowerCase() === s2.toLowerCase();
}
/*** META ((export string-ci<?)
           (arity #t)
	   (type bool)
           (peephole (hole 2 str1 ".toLowerCase() < " str2 ".toLowerCase()")))
*/
function sc_isStringCILess(s1, s2) {
    return s1.toLowerCase() < s2.toLowerCase();
}
/*** META ((export string-ci>?)
           (arity #t)
	   (type bool)
           (peephole (hole 2 str1 ".toLowerCase() > " str2 ".toLowerCase()")))
*/
function sc_isStringCIGreater(s1, s2) {
    return s1.toLowerCase() > s2.toLowerCase();
}
/*** META ((export string-ci<=?)
           (arity #t)
	   (type bool)
           (peephole (hole 2 str1 ".toLowerCase() <= " str2 ".toLowerCase()")))
*/
function sc_isStringCILessEqual(s1, s2) {
    return s1.toLowerCase() <= s2.toLowerCase();
}
/*** META ((export string-ci>=?)
           (arity #t)
	   (type bool)
           (peephole (hole 2 str1 ".toLowerCase() >= " str2 ".toLowerCase()")))
*/
function sc_isStringCIGreaterEqual(s1, s2) {
    return s1.toLowerCase() >= s2.toLowerCase();
}

/*** META ((export string-contains)
           (arity -3)
	   (type bool))
*/
function sc_stringContains(s1,s2,start) {
   return s1.indexOf(s2,start ? start : 0) >= 0;
}

/*** META ((export string-contains-ci)
           (arity -3)
	   (type bool))
*/
function sc_stringCIContains(s1,s2,start) {
   return s1.toLowerCase.indexOf(s2.toLowerCase,start ? start : 0) >= 0;
}

/*** META ((export #t)
           (arity #t)
           (peephole (hole 3 s ".substring(" start ", " end ")")))
*/
function sc_substring(s, start, end) {
    return s.substring(start, end);
}

/*** META ((export #t) (arity -4))
*/
function sc_isSubstring_at(str1, str2, i, len) {
    if (!len) len = str2.length;
    else if (str2.length < len) return false;
    if (str1.length < len + i) return false;
    return str2.substring(0, len) == str1.substring(i, i+len);
    return s2 == s1.substring(i, i+ s2.length);
}

/*** META ((export substring=?) (arity #t))
*/
function sc_isSubstring(s1, s2, len) {
    if (s1.length < len) return false;
    if (s2.length < len) return false;
    return s2.substring(0, len) == s1.substring(0, len);
}

/*** META ((export #t)
           (arity -1)
           (peephole (infix 0 #f "+" "''")))
*/
function sc_stringAppend() {
    return "".concat.apply("", arguments);
}

/*** META ((export #t) (arity 1)) */
var sc_string2list = sc_jsstring2list;

/*** META ((export #t) (arity 1)) */
var sc_list2string = sc_list2jsstring;

/*** META ((export #t)
           (arity #t)
           (peephole (id)))
*/
function sc_stringCopy(s) {
    return s;
}

/* there's no string-fill in the immutable version
function sc_stringFill(s, c)
*/

/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".slice(1)")))
*/
function sc_keyword2string(o) {
    return o.slice(1);
}

/*** META ((export #t)
           (arity #t)
           (peephole (prefix "'\\uEBAD' +")))
*/
function sc_string2keyword(o) {
    return sc_KEYWORD_PREFIX + o;
}

String.prototype.sc_toDisplayString = function() {
    if (this.charAt(0) === sc_SYMBOL_PREFIX)
	// TODO: care for symbols with spaces (escape-chars symbols).
	return this.slice(1);
    else if (this.charAt(0) === sc_KEYWORD_PREFIX)
	return ":" + this.slice(1);
    else
	return this.toString();
};

String.prototype.sc_toWriteString = function() {
    if (this.charAt(0) === sc_SYMBOL_PREFIX)
	// TODO: care for symbols with spaces (escape-chars symbols).
	return this.slice(1);
    else if (this.charAt(0) === sc_KEYWORD_PREFIX)
	return ":" + this.slice(1);
    else
	return '"' + sc_escapeWriteString(this) + '"';
};

/*** META ((export #t)
           (arity #t)
           (peephole (hole 2 1 ".indexOf(" 0 ") === 0")))
*/
function sc_isStringPrefix(cs1, cs2) {
    return cs2.indexOf(cs1) === 0;
}

/*** META ((export #t) (arity #t)) */
function sc_isStringSuffix(cs1, cs2) {
    var tmp = cs2.lastIndexOf(cs1);
    return tmp !== false && tmp >= 0 && tmp === cs2.length - cs1.length;
}

/*** META ((export #t) (arity #t)) */
function sc_stringSplit(s, sep) {
    if (sep.length === 1)
	return sc_vector2list(s.split(sep));
    return sc_vector2list(s.split(sc_pregexpCreateCharsetMatcher(sep)));
}

/*** META ((export #t) (arity -3)) */
function sc_stringIndex(s, cset, start) {
   var res;
   if (!start) start = 0;

   if (cset instanceof sc_Char) {
      res = s.indexOf(sc_char2string(cset), start);
      return res >= 0 ? res : false;
   }
   if (cset.length == 1) {
      res = s.indexOf(cset, start);
      return res >= 0 ? res : false;
   } else {
      for (var i = start; i < s.length; i++ ) {
	 if (cset.indexOf(s.charAt(i)))
	    return i;
      }

      return false;
   }
}

/*** META ((export #t) (arity -3)) */
function sc_stringIndexRight(s, cset, start) {
   var res;
   if (!start) start = s.length - 1;
   
   if (cset instanceof sc_Char) {
      res = s.lastIndexof(sc_char2string(cset), start);
      return res >= 0 ? res : false;
   }
   if (cset.length == 1) {
      res = s.lastIndexOf(cset, start);
      return res >= 0 ? res : false;
   } else {
      for (var i = start; i >= 0; i-- ) {
	 if (cset.indexOf(s.charAt(i)))
	    return i;
      }

      return false;
   }
}

/*** META ((export #t) (arity 1)) */
function sc_string_downcase(s) {
   return s.toLowerCase();
}

/*** META ((export #t) (arity 1)) */
function sc_string_upcase(s) {
   return s.toUpperCase();
}

/*** META ((export #t) (arity 1)) */
function sc_string_capitalize(s) {
   return s.replace(/\w+/g, function (w) {
	 return w.charAt(0).toUpperCase() + w.substr(1).toLowerCase();
      });
}
