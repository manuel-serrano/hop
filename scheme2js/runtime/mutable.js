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
           (type bool)
           (peephole (postfix " instanceof sc_Keyword")))
*/
function sc_isKeyword(o) {
    return (o instanceof sc_Keyword);
}



/*** META ((export #t)
           (peephole (hole 1 "new sc_String(" s ")")))
*/
function sc_jsstring2string(s) {
    return new sc_String(s);
}

/*** META ((export #t)
           (peephole (id)))
*/
function sc_jsstring2symbol(s) {
    return s;
}

/*** META ((export #t)
           (peephole (string2jsstring_mutable)))
*/
function sc_string2jsstring(s) {
    return s.val;
}

/*** META ((export #t)
           (peephole (id)))
*/
function sc_symbol2jsstring(s) {
    return s;
}

/*** META ((export #t)
           (peephole (postfix ".val")))
*/
function sc_keyword2jsstring(o) {
    return o.val;
}

/*** META ((export #t)
           (peephole (hole 1 "new sc_Keyword(" s ")")))
*/
function sc_jsstring2keyword(s) {
    return new sc_Keyword(s);
}

/*** META ((export #t)) */
var sc_gensym = function() {
    var counter = 1000;
    return function(sym) {
	counter++;
	if (!sym) sym = "";
	return "s" + counter + "~" + sym + "^sC-GeNsYm ";
    };
}();

/*** META ((export #t)
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
           (peephole (prefix "''+")))
           ;; peephole will only apply if no radix is given.
*/
var sc_number2symbol = sc_number2jsstring;

/*** META ((export number->string integer->string)
           (peephole (hole 1 "new sc_String(''+" x ")")))
           ;; peephole will only apply if no radix is given.
*/
function sc_number2string(x, radix) {
    return new sc_String(sc_number2jsstring(x, radix));
}

/*** META ((export #t)) */
var sc_symbol2number = sc_jsstring2number;

/*** META ((export #t)) */
function sc_string2number(s, radix) {
    return sc_symbol2number(s.val, radix);
}

/*** META ((export #t)
           (peephole (hole 1 "+" s ".val")))
           ;; peephole will only apply if no radix is given.
*/
function sc_string2integer(s, radix) {
    if (!radix) return +s.val;
    return parseInt(s.val, radix);
}

/*** META ((export #t)
           (peephole (hole 1 "+" s ".val")))
*/
function sc_string2real(s) {
    return +s.val;
}

/*** META ((export #t)
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
           (peephole (string2symbol_mutable)))
*/
function sc_string2symbol(s) {
    return s.val;
}

/*** META ((export #t)
           (peephole (infix 0 #f "+" "''")))
*/
function sc_symbolAppend() {
    return "".concat.apply("", arguments);
}

/*** META ((export #t)
           (peephole (hole "new sc_String(" c ".val)")))
*/
function sc_char2string(c) { return new sc_String(c.val); }

/*** META ((export #t)
           (peephole (postfix ".val")))
*/
function sc_char2symbol(c) { return c.val; }

/*** META ((export #t)
           (type bool)
           (peephole (postfix " instanceof sc_String")))
*/
function sc_isString(s) { return (s instanceof sc_String); }

/*** META ((export #t)) */
function sc_makeString(k, c) {
    return new sc_String(sc_makejsString(k, c));
}


/*** META ((export #t)) */
function sc_string() {
    for (var i = 0; i < arguments.length; i++)
	arguments[i] = arguments[i].val;
    return new sc_String("".concat.apply("", arguments));
}

/*** META ((export #t)
           (peephole (postfix ".val.length"))) */
function sc_stringLength(s) { return s.val.length; }

/*** META ((export #t)) */
function sc_stringRef(s, k) {
    return new sc_Char(s.val.charAt(k));
}

/*** META ((export #t)) */
function sc_stringSetBang(s, k, c) {
    var start = s.val.slice(0, k);
    var end = s.val.slice(k+1);
    s.val = start.concat(c.val, end);
}

/*** META ((export string=?)
           (type bool)
           (peephole (hole 2 str1 ".val === " str2 ".val")))
*/
var sc_isStringEqual = sc_isCharStringEqual;
/*** META ((export string<?)
           (type bool)
           (peephole (hole 2 str1 ".val < " str2 ".val")))
*/
var sc_isStringLess = sc_isCharStringLess;
/*** META ((export string>?)
           (type bool)
           (peephole (hole 2 str1 ".val > " str2 ".val")))
*/
var sc_isStringGreater = sc_isCharStringGreater;
/*** META ((export string<=?)
           (type bool)
           (peephole (hole 2 str1 ".val <= " str2 ".val")))
*/
var sc_isStringLessEqual = sc_isCharStringLessEqual;
/*** META ((export string>=?)
           (type bool)
           (peephole (hole 2 str1 ".val >= " str2 ".val")))
*/
var sc_isStringGreaterEqual = sc_isCharStringGreaterEqual;
/*** META ((export string-ci=?)
           (type bool)
           (peephole (hole 2 str1 ".val.toLowerCase() === " str2 ".val.toLowerCase()")))
*/
var sc_isStringCIEqual = sc_isCharStringCIEqual;
/*** META ((export string-ci<?)
           (type bool)
           (peephole (hole 2 str1 ".val.toLowerCase() < " str2 ".val.toLowerCase()")))
*/
var sc_isStringCILess = sc_isCharStringCILess;
/*** META ((export string-ci>?)
           (type bool)
           (peephole (hole 2 str1 ".val.toLowerCase() > " str2 ".val.toLowerCase()")))
*/
var sc_isStringCIGreater = sc_isCharStringCIGreater;
/*** META ((export string-ci<=?)
           (type bool)
           (peephole (hole 2 str1 ".val.toLowerCase() <= " str2 ".val.toLowerCase()")))
*/
var sc_isStringCILessEqual = sc_isCharStringCILessEqual;
/*** META ((export string-ci>=?)
           (type bool)
           (peephole (hole 2 str1 ".val.toLowerCase() >= " str2 ".val.toLowerCase()")))
*/
var sc_isStringCIGreaterEqual = sc_isCharStringCIGreaterEqual;


/*** META ((export #t)
           (peephole (hole 3 "new sc_String(" s ".val.substring(" start ", " end "))")))
*/
function sc_substring(s, start, end) {
    return new sc_String(s.val.substring(start, end));
}

/*** META ((export #t))
*/
function sc_isSubstring_at(s1, s2, i) {
    return s2.val == s1.val.substring(i, i + s2.val.length);
}

/*** META ((export #t)
           (peephole (stringAppend_mutable)))
*/
function sc_stringAppend() {
    for (var i = 0; i < arguments.length; i++)
	arguments[i] = arguments[i].val;
    return new sc_String("".concat.apply("", arguments));
}

/*** META ((export #t)
           (peephole (hole 1 "sc_jsstring2list(" s ".val)")))
*/
function sc_string2list(s) {
    return sc_jsstring2list(s.val);
}

/*** META ((export list->string)
           (peephole (hole 1 "new sc_String(sc_list2jsstring(" l "))")))
*/
function sc_list2string(l) {
    return new sc_String(sc_list2jsstring(l));
}

/*** META ((export #t)
           (peephole (hole 1 "new sc_String(" s ".val)")))
*/
function sc_stringCopy(s) {
    return new sc_String(s.val);
}


/*** META ((export #t)) */
function sc_stringFillBang(s, c) {
    s.val = sc_makeJSStringOfLength(s.val.length, c.val);
}

/*** META ((export #t)
           (peephole (hole 1 "new sc_String(" o ".val)")))
*/
function sc_keyword2string(o) {
    return new sc_String(o.val);
}

/*** META ((export #t)
           (peephole (hole 1 "new sc_Keyword(" o ".val)")))
*/
function sc_string2keyword(o) {
    return new sc_Keyword(o.val);
}
