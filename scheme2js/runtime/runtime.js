var sc_JS_GLOBALS = this; /// export *js*

var sc_SYMBOL_PREFIX = "\u1E9C"; // "\u1E9D\u1E9E\u1E9F";

var with_hop; /// export with-hop

function sc_alert() { /// export
   var len = arguments.length;
   var s = "";
   var i;

   for( i = 0; i < len; i++ ) {
      s += arguments[ i ];
   }

   return alert( s );
}

function sc_typeof( x ) { /// export
   return typeof x;
}

function sc_error_mutable() {  /// export
    sc_print_mutable("**ERROR**");
    for (var i = 0; i < arguments.length; i++) {
	sc_print_mutable(arguments[i]);
    }
    throw "ERROR";
}

function sc_error_immutable() {  /// export
    sc_print_immutable("**ERROR**");
    for (var i = 0; i < arguments.length; i++) {
	sc_print_immutable(arguments[i]);
    }
    throw "ERROR";
}

var sc_properties = new Object();

function sc_putProp(sym, key, val) { /// export putprop!
    var ht = sc_properties[sym];
    if (!ht) {
	ht = new Object();
	sc_properties[sym] = ht;
    }
    ht[key] = val;
}

function sc_getProp(sym, key) { /// export getprop
    var ht = sc_properties[sym];
    if (ht) {
	if (key in ht)
	    return ht[key];
	else
	    return false;
    } else
	return false;
}

function sc_remProp(sym, key) { /// export remprop!
    var ht = sc_properties[sym];
    if (ht)
	delete ht[key];
}

var sc_gensym_mutable = function() { /// export
    var counter = 1000;
    return function(sym) {
	counter++;
	if (!sym) sym = "";
	return "s" + counter + "~" + sym + "^sC-GeNsYm ";
    };
}();

var sc_gensym_immutable = function() { /// export
    var counter = 1000;
    return function(sym) {
	counter++;
	if (!sym) sym = sc_SYMBOL_PREFIX;
	return sym + "s" + counter + "~" + "^sC-GeNsYm ";
    };
}();

function sc_toString(o) {
    if (o === null)
	return "()";
    else if (o === true)
	return "#t";
    else if (o === false)
	return "#f";
    else if (o === undefined)
	return "#unspecified"
    else
	return o.toString();
}

function sc_isEqv(o1, o2) { /// export
    return (o1 === o2);
}

function sc_isEq(o1, o2) { /// export
    return (o1 === o2);
}

function sc_isEqual(o1, o2) { /// export
    return ((o1 === o2) ||
	    (sc_isPair(o1) && sc_isPair(o2) && sc_isPairEqual(o1, o2)) ||
	    (sc_isVector(o1) && sc_isVector(o2) && sc_isVectorEqual(o1, o2)));
}

function sc_isNumber(n) { /// export
    return (typeof n === "number");
}

function sc_isComplex(n) { /// export
    return sc_isNumber(n);
}

function sc_isReal(n) { /// export
    return sc_isNumber(n);
}

function sc_isRational(n) { /// export
    return sc_isReal(n);
}

function sc_isInteger(n) { /// export
    return (parseInt(n) === n);
}

// we don't have exact numbers...
function sc_isExact(n) { /// export
    return false;
}

function sc_isInexact(n) { /// export
    return true;
}

function sc_equal(x) { /// export = =fx =fl
    for (var i = 1; i < arguments.length; i++)
	if (x !== arguments[i])
	    return false;
    return true;
}

function sc_less(x) { /// export < <fx <fl
    for (var i = 1; i < arguments.length; i++) {
	if (x >= arguments[i])
	    return false;
	x = arguments[i];
    }
    return true;
}

function sc_greater(x, y) { /// export > >fx >fl
    for (var i = 1; i < arguments.length; i++) {
	if (x <= arguments[i])
	    return false;
	x = arguments[i];
    }
    return true;
}

function sc_lessEqual(x, y) { /// export <= <=fx <=fl
    for (var i = 1; i < arguments.length; i++) {
	if (x > arguments[i])
	    return false;
	x = arguments[i];
    }
    return true;
}

function sc_greaterEqual(x, y) { /// export >= >=fl >=fx
    for (var i = 1; i < arguments.length; i++) {
	if (x < arguments[i])
	    return false;
	x = arguments[i];
    }
    return true;
}

function sc_isZero(x) { /// export
    return (x === 0);
}

function sc_isPositive(x) { /// export
    return (x > 0);
}

function sc_isNegative(x) { /// export
    return (x < 0);
}

function sc_isOdd(x) { /// export
    return (x % 2 === 1);
}

function sc_isEven(x) { /// export
    return (x % 2 === 0);
}

var sc_max = Math.max; /// export
var sc_min = Math.min; /// export

function sc_plus() { /// export + +fx +fl
    var sum = 0;
    for (var i = 0; i < arguments.length; i++)
	sum += arguments[i];
    return sum;
}

function sc_multi() { /// export * *fx *fl
    var product = 1;
    for (var i = 0; i < arguments.length; i++)
	product *= arguments[i];
    return product;
}

function sc_minus(x, y) { /// export - -fx -fl
    if (y === undefined)
	return -x;
    else
	return x - y;
}

function sc_div(x, y) { /// export / /fl
    if (y === undefined)
	return 1 / x;
    else
	return x / y;
}

var sc_abs = Math.abs; /// export

function sc_quotient(x, y) { /// export quotient /fx
    return parseInt(x / y);
}

function sc_remainder(x, y) { /// export
    return x % y;
}

function sc_modulo(x, y) { /// export
    var remainder = x % y;
    // if they don't have the same sign
    if ((remainder * y) < 0)
	return remainder + y;
    else
	return remainder;
}

function sc_euclid_gcd(a, b) {
    var temp;
    if (a === 0) return b;
    if (b === 0) return a;
    if (a < 0) {a = -a;};
    if (b < 0) {b = -b;};
    if (b > a) {temp = a; a = b; b = temp;};
    while (true) {
	a %= b;
	if(a === 0) {return b;};
	b %= a;
	if(b === 0) {return a;};
    };
    return b;
}

function sc_gcd() { /// export
    var gcd = 0;
    for (var i = 0; i < arguments.length; i++)
	gcd = sc_euclid_gcd(gcd, arguments[i]);
    return gcd;
}

function sc_lcm() { /// export
    var lcm = 1;
    for (var i = 0; i < arguments.length; i++) {
	var f = Math.round(arguments[i] / sc_euclid_gcd(arguments[i], lcm));
	lcm *= Math.abs(f);
    }
    return lcm;
}

var SC_MAX_DECIMALS = 1000000

function sc_numerator(x) { /// export
    var rounded = Math.round(x * SC_MAX_DECIMALS);
    return Math.round(rounded / sc_euclid_gcd(rounded, SC_MAX_DECIMALS));
}

function sc_denominator(x) { /// export
    var rounded = Math.round(x * SC_MAX_DECIMALS);
    return Math.round(SC_MAX_DECIMALS / sc_euclid_gcd(rounded, SC_MAX_DECIMALS));
}

var sc_floor = Math.floor; /// export
var sc_ceiling = Math.ceil; /// export
var sc_truncate = parseInt; /// export
var sc_round = Math.round; /// export

// LIMITATION: sc_rationalize doesn't make sense in a floating point world.

var sc_exp = Math.exp; /// export
var sc_log = Math.log; /// export
var sc_sin = Math.sin; /// export
var sc_cos = Math.cos; /// export
var sc_tan = Math.tan; /// export
var sc_asin = Math.asin; /// export
var sc_acos = Math.acos; /// export
var sc_atan = Math.atan; /// export

var sc_sqrt = Math.sqrt; /// export
var sc_expt = Math.pow; /// export

// LIMITATION: we don't have complex numbers.
// LIMITATION: the following functions are hence not implemented.
// LIMITATION: make-rectangular, make-polar, real-part, imag-part, magnitude, angle
// LIMITATION: 2 argument atan

function sc_exact2inexact(x) { /// export
    return x;
}

function sc_inexact2exact(x) { /// export
    return x;
}

function sc_number2symbol_mutable(x, radix) { /// export
    if (radix)
	return x.toString(radix);
    else
	return x.toString();
}

function sc_number2string_mutable(x, radix) { /// export
    return new sc_String(sc_number2symbol_mutable(x, radix));
}

function sc_number2symbol_immutable(x, radix) { /// export
    return sc_SYMBOL_PREFIX + sc_number2string_immutable(x, radix);
}
    
var sc_number2string_immutable = sc_number2symbol_mutable; /// export


function sc_symbol2number_mutable(s, radix) { /// export
    // first test, if there aren't any trailing chars
    // (which are ignored by parseFloat/parseInt)
    if (+s || +s === 0) {
	if (radix) {
	    var t = parseInt(s, radix);
	    if (t || t === 0) return t; else return false;
	} else return parseFloat(s);
    } else
	return false;
}

function sc_string2number_mutable(s, radix) { /// export
    sc_symbol2number_mutable(s.val, radix);
}

function sc_symbol2number_immutable(s, radix) { /// export
    return sc_SYMBOL_PREFIX + sc_string2number_immutable(s, radix);
}

var sc_string2number_immutable = sc_symbol2number_mutable; /// export

function sc_not(b) { /// export
    return b === false;
}

function sc_isBoolean(b) { /// export
    return (b === true) || (b === false);
}

function sc_Pair(car, cdr) {
    this.car = car;
    this.cdr = cdr;
}

sc_Pair.prototype.toString = function(inList) {
    var isP = sc_isPair(this.cdr);
    var tmp = sc_toString(this.car);
    if (sc_isPair(this.cdr))
	tmp += " " + this.cdr.toString(true);
    else if (this.cdr !== null)
	tmp += " . " + sc_toString(this.cdr);
    if (inList)
	return tmp;
    else
	return "(" + tmp + ")";
}

function sc_isPair(p) { /// export
    return (p instanceof sc_Pair);
}

function sc_isPairEqual(p1, p2) {
    return (sc_isEqual(p1.car, p2.car) && sc_isEqual(p1.cdr, p2.cdr));
}

function sc_cons(car, cdr) { /// export
    return new sc_Pair(car, cdr);
}

function sc_car(p) { /// export
    return p.car;
}

function sc_cdr(p) { /// export
    return p.cdr;
}

function sc_setCar(p, o) { /// export set-car!
    p.car = o;
}

function sc_setCdr(p, o) { /// export set-cdr!
    p.cdr = o;
}

function sc_caar(p) { return p.car.car; } /// export
function sc_cdar(p) { return p.cdr.car; } /// export
function sc_cadr(p) { return p.car.cdr; } /// export
function sc_cddr(p) { return p.cdr.cdr; } /// export
function sc_caaar(p) { return p.car.car.car; } /// export
function sc_cadar(p) { return p.car.cdr.car; } /// export
function sc_caadr(p) { return p.car.car.cdr; } /// export
function sc_caddr(p) { return p.car.cdr.cdr; } /// export
function sc_cdaar(p) { return p.cdr.car.car; } /// export
function sc_cddar(p) { return p.cdr.cdr.car; } /// export
function sc_cdadr(p) { return p.cdr.car.cdr; } /// export
function sc_cdddr(p) { return p.cdr.cdr.cdr; } /// export
function sc_caaaar(p) { return p.car.car.car.car; } /// export
function sc_caadar(p) { return p.car.car.cdr.car; } /// export
function sc_caaadr(p) { return p.car.car.car.cdr; } /// export
function sc_caaddr(p) { return p.car.car.cdr.cdr; } /// export
function sc_cadaar(p) { return p.car.cdr.car.car; } /// export
function sc_caddar(p) { return p.car.cdr.cdr.car; } /// export
function sc_cadadr(p) { return p.car.cdr.car.cdr; } /// export
function sc_cadddr(p) { return p.car.cdr.cdr.cdr; } /// export
function sc_cdaaar(p) { return p.cdr.car.car.car; } /// export
function sc_cdadar(p) { return p.cdr.car.cdr.car; } /// export
function sc_cdaadr(p) { return p.cdr.car.car.cdr; } /// export
function sc_cdaddr(p) { return p.cdr.car.cdr.cdr; } /// export
function sc_cddaar(p) { return p.cdr.cdr.car.car; } /// export
function sc_cdddar(p) { return p.cdr.cdr.cdr.car; } /// export
function sc_cddadr(p) { return p.cdr.cdr.car.cdr; } /// export
function sc_cddddr(p) { return p.cdr.cdr.cdr.cdr; } /// export

function sc_lastPair(l) { /// export
    if (!sc_isPair(l)) sc_error("sc_lastPair: pair expected");
    var res = l;
    var cdr = l.cdr;
    while (sc_isPair(cdr)) {
	res = cdr;
	cdr = res.cdr;
    }
    return res;
}

function sc_isNull(o) { /// export
    return (o === null);
}

function sc_isList(o) { /// export
    var rabbit;
    var turtle;

    var rabbit = o;
    var turtle = o;
    while (rabbit != null) {
	if (sc_isPair(rabbit) &&
	    sc_isPair(rabbit.cdr)) {
	    rabbit = rabbit.cdr.cdr;
	    turtle = turtle.cdr;
	    if (rabbit === turtle) return false; // cycle
	} else
	    return false; // not pair
    }
    return true;
}

function sc_list() { /// export
    var res = null;
    var a = arguments;
    for (var i = a.length-1; i >= 0; i--)
	res = sc_cons(a[i], res);
    return res;
}

function sc_makeList(nbEls, fill) { /// export
    var res = null;
    for (var i = 0; i < nbEls; i++)
	res = sc_cons(fill, res);
    return res;
}

function sc_length(l) { /// export
    res = 0;
    while (l != null) {
	res++;
	l = l.cdr;
    }
    return res;
}

function sc_destReverseAppend(l1, l2) {
    var res = l2;
    while (l1 != null) {
	var tmp = res;
	res = l1;
	l1 = l1.cdr;
	res.cdr = tmp;
    }
    return res;
}
	
function sc_dualAppend(l1, l2) {
    if (l1 === null) return l2;
    if (l2 === null) return l1;
    var rev = sc_reverse(l1);
    return sc_destReverseAppend(rev, l2);
}

function sc_append() { /// export
    if (arguments.length === 0)
	return null;
    var res = arguments[arguments.length - 1];
    for (var i = arguments.length - 2; i >= 0; i--)
	res = sc_dualAppend(arguments[i], res);
    return res;
}

function sc_destDualAppend(l1, l2) {
    if (l1 === null) return l2;
    if (l2 === null) return l1;
    var tmp = l1;
    while (tmp.cdr != null) tmp=tmp.cdr;
    tmp.cdr = l2;
    return l1;
}
    
function sc_destAppend() { /// export append!
    var res = null;
    for (var i = 0; i < arguments.length; i++)
	res = sc_destDualAppend(res, arguments[i]);
    return res;
}

function sc_reverse(l1) { /// export
    var res = null;
    while (l1 != null) {
	res = sc_cons(l1.car, res);
	l1 = l1.cdr;
    }
    return res;
}

function sc_destReverse(l) { /// export reverse!
    return sc_destReverseAppend(l, null);
}

function sc_listTail(l, k) { /// export
    var res = l;
    for (var i = 0; i < k; i++) {
	res = res.cdr;
    }
    return res;
}

function sc_listRef(l, k) { /// export
    return sc_listTail(l, k).car;
}

/* // unoptimized generic versions
function sc_memX(o, l, comp) {
    while (l != null) {
	if (comp(l.car, o))
	    return l;
	l = l.cdr;
    }
    return false;
}
function sc_memq(o, l) { return sc_memX(o, l, sc_isEq); }
function sc_memv(o, l) { return sc_memX(o, l, sc_isEqv); }
function sc_member(o, l) { return sc_memX(o, l, sc_isEqual); }
*/

/* optimized versions */
function sc_memq(o, l) { /// export
    while (l != null) {
	if (l.car === o)
	    return l;
	l = l.cdr;
    }
    return false;
}
function sc_memv(o, l) { /// export
    while (l != null) {
	if (l.car === o)
	    return l;
	l = l.cdr;
    }
    return false;
}
function sc_member(o, l) { /// export
    while (l != null) {
	if (sc_isEqual(l.car,o))
	    return l;
	l = l.cdr;
    }
    return false;
}


/* // generic unoptimized versions
function sc_assX(o, al, comp) {
    while (al != null) {
	if (comp(al.car.car, o))
	    return al.car;
	al = al.cdr;
    }
    return false;
}
function sc_assq(o, al) { return sc_assX(o, al, sc_isEq); }
function sc_assv(o, al) { return sc_assX(o, al, sc_isEqv); }
function sc_assoc(o, al) { return sc_assX(o, al, sc_isEqual); }
*/
// optimized versions
function sc_assq(o, al) { /// export
    while (al != null) {
	if (al.car.car === o)
	    return al.car;
	al = al.cdr;
    }
    return false;
}
function sc_assv(o, al) { /// export
    while (al != null) {
	if (al.car.car === o)
	    return al.car;
	al = al.cdr;
    }
    return false;
}
function sc_assoc(o, al) { /// export
    while (al != null) {
	if (is_Equal(al.car.car, o))
	    return al.car;
	al = al.cdr;
    }
    return false;
}

function sc_isSymbol_mutable(s) { /// export
    return (typeof s === "string");
}

function sc_isSymbol_immutable(s) { /// export
    return (typeof s === "string") &&
	(s.charAt(0) === sc_SYMBOL_PREFIX);
}

function sc_symbol2string_mutable(s) { /// export
    return new sc_String(s);
}

function sc_symbol2string_immutable(s) { /// export
    return s.slice(1);
}

function sc_string2symbol_mutable(s) { /// export
    return s.val;
}

function sc_string2symbol_immutable(s) { /// export
    return sc_SYMBOL_PREFIX + s;
}

function sc_symbolAppend_mutable() { /// export
    return "".concat.apply("", arguments);
}

function sc_symbolAppend_immutable() { /// export
    var res = sc_SYMBOL_PREFIX;
    for (var i = 0; i < arguments.length; i++)
	res += arguments[i].slice(1);
    return res;
}

/* can be used for mutable strings and characters */
function sc_isCharStringEqual(cs1, cs2) { return cs1.val === cs2.val; }
function sc_isCharStringLess(cs1, cs2) { return cs1.val < cs2.val; }
function sc_isCharStringGreater(cs1, cs2) { return cs1.val > cs2.val; }
function sc_isCharStringLessEqual(cs1, cs2) { return cs1.val <= cs2.val; }
function sc_isCharStringGreaterEqual(cs1, cs2) { return cs1.val >= cs2.val; }
function sc_isCharStringCIEqual(cs1, cs2)
    { return cs1.val.toLowerCase() === cs2.val.toLowerCase(); }
function sc_isCharStringCILess(cs1, cs2)
    { return cs1.val.toLowerCase() < cs2.val.toLowerCase(); }
function sc_isCharStringCIGreater(cs1, cs2)
    { return cs1.val.toLowerCase() > cs2.val.toLowerCase(); }
function sc_isCharStringCILessEqual(cs1, cs2)
    { return cs1.val.toLowerCase() <= cs2.val.toLowerCase(); }
function sc_isCharStringCIGreaterEqual(cs1, cs2)
    { return cs1.val.toLowerCase() >= cs2.val.toLowerCase(); }




function sc_Char(c) {
    var cached = sc_Char.lazy[c];
    if (cached)
	return cached;
    this.val = c;
    sc_Char.lazy[c] = this;
}
sc_Char.lazy = new Object();
// thanks to Eric
sc_Char.char2readable = {
    "\000": "#\\null",
    "\007": "#\\bell",
    "\010": "#\\backspace",
    "\011": "#\\tab",
    "\012": "#\\newline",
    "\014": "#\\page",
    "\015": "#\\return",
    "\033": "#\\escape",
    "\040": "#\\space",
    "\177": "#\\delete",

  /* poeticless names */
    "\001": "#\\soh",
    "\002": "#\\stx",
    "\003": "#\\etx",
    "\004": "#\\eot",
    "\005": "#\\enq",
    "\006": "#\\ack",

    "\013": "#\\vt",
    "\016": "#\\so",
    "\017": "#\\si",

    "\020": "#\\dle",
    "\021": "#\\dc1",
    "\022": "#\\dc2",
    "\023": "#\\dc3",
    "\024": "#\\dc4",
    "\025": "#\\nak",
    "\026": "#\\syn",
    "\027": "#\\etb",

    "\030": "#\\can",
    "\031": "#\\em",
    "\032": "#\\sub",
    "\033": "#\\esc",
    "\034": "#\\fs",
    "\035": "#\\gs",
    "\036": "#\\rs",
    "\037": "#\\us"};

sc_Char.readable2char = {
    "null": "\000",
    "bell": "\007",
    "backspace": "\010",
    "tab": "\011",
    "newline": "\012",
    "page": "\014",
    "return": "\015",
    "escape": "\033",
    "space": "\040",
    "delete": "\000",
    "soh": "\001",
    "stx": "\002",
    "etx": "\003",
    "eot": "\004",
    "enq": "\005",
    "ack": "\006",
    "bel": "\007",
    "bs": "\010",
    "ht": "\011",
    "nl": "\012",
    "vt": "\013",
    "np": "\014",
    "cr": "\015",
    "so": "\016",
    "si": "\017",
    "dle": "\020",
    "dc1": "\021",
    "dc2": "\022",
    "dc3": "\023",
    "dc4": "\024",
    "nak": "\025",
    "syn": "\026",
    "etb": "\027",
    "can": "\030",
    "em": "\031",
    "sub": "\032",
    "esc": "\033",
    "fs": "\034",
    "gs": "\035",
    "rs": "\036",
    "us": "\037",
    "sp": "\040",
    "del": "\177"};
    
sc_Char.prototype.toString = function() {
    return this.val;
}

function sc_isChar(c) { /// export
    return (c instanceof sc_Char);
}

var sc_isCharEqual = sc_isCharStringEqual; /// export char=?
var sc_isCharLess = sc_isCharStringLess; /// export char<?
var sc_isCharGreater = sc_isCharStringGreater; /// export char>?
var sc_isCharLessEqual = sc_isCharStringLessEqual; /// export char<=?
var sc_isCharGreaterEqual = sc_isCharStringGreaterEqual; /// export char>=?
var sc_isCharCIEqual = sc_isCharStringCIEqual; /// export char-ci=?
var sc_isCharCILess = sc_isCharStringCILess; /// export char-ci<?
var sc_isCharCIGreater = sc_isCharStringCIGreater; /// export char-ci>?
var sc_isCharCILessEqual = sc_isCharStringCILessEqual; /// export char-ci<=?
var sc_isCharCIGreaterEqual = sc_isCharStringCIGreaterEqual; /// export char-ci>=?

var SC_NUMBER_CLASS = "0123456789";
var SC_WHITESPACE_CLASS = ' \r\n\t\f';
var SC_LOWER_CLASS = 'abcdefghijklmnopqrstuvwxyz';
var SC_UPPER_CLASS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

function sc_isCharOfClass(c, cl) { return (cl.indexOf(c) != -1); }
function sc_isCharAlphabetic(c)  /// export
    { return sc_isCharOfClass(c.val, SC_LOWER_CLASS) ||
	  sc_isCharOfClass(c.val, SC_UPPER_CLASS); }
function sc_isCharNumeric(c) /// export
    { return sc_isCharOfClass(c.val, SC_NUMBER_CLASS); }
function sc_isCharWhitespace(c) /// export
    {
	var tmp = c.val;
	return tmp === " " || tmp === "\r" || tmp === "\n" || tmp === "\t" || tmp === "\f";
    }
function sc_isCharUpperCase(c) /// export
    { return sc_isCharOfClass(c.val, SC_UPPER_CLASS); }
function sc_isCharLowerCase(c) /// export
    { return sc_isCharOfClass(c.val, SC_LOWER_CLASS); }

function sc_char2integer(c) /// export
    { return c.val.charCodeAt(0); }
function sc_char2string_mutable(c) /// export
    { return new sc_String(c.val); }
function sc_char2string_immutable(c) /// export
    { return c.val; }
function sc_char2symbol_mutable(c) /// export
    { return c.val; }
function sc_char2symbol_immutable(c) /// export
    { return sc_SYMBOL_PREFIX + c.val; }
function sc_integer2char(n) /// export
    { return new sc_Char(String.fromCharCode(n)); }

function sc_charUpcase(c) /// export
    { return new sc_Char(c.val.toUpperCase()); }
function sc_charDowncase(c) /// export
    { return new sc_Char(c.val.toLowerCase()); }

function sc_makeJSStringOfLength(k, c) {
    var fill;
    if (c === undefined)
	fill = " ";
    else
	fill = c;
    var res = "";
    var len = 1;
    // every round doubles the size of fill.
    while (k >= len) {
	if (k & len)
	    res = res.concat(fill);
	fill = fill.concat(fill);
	len *= 2;
    }
    return res;
}

function sc_String(s) {
    this.val = s;
}

sc_String.prototype.toString = function() {
    return this.val;
}

sc_String.prototype.hop_bigloo_serialize = function() {
   return hop_bigloo_serialize( this.val );
}

function sc_isString_mutable(s) { /// export
    return (s instanceof sc_String);
}

function sc_isString_immutable(s) { /// export
    return (typeof s === "string") &&
	(s.charAt(0) !== sc_SYMBOL_PREFIX);
}

function sc_makeString_mutable(k, c) { /// export
    return new sc_String(sc_makeString_immutable(k, c));
}

function sc_makeString_immutable(k, c) { /// export
    var fill;
    if (c)
	fill = c.val;
    else
	fill = " ";
    return sc_makeJSStringOfLength(k, fill);
}

function sc_string_mutable() { /// export
    for (var i = 0; i < arguments.length; i++)
	arguments[i] = arguments[i].val;
    return new sc_String("".concat.apply("", arguments));
}

function sc_string_immutable() { /// export
    for (var i = 0; i < arguments.length; i++)
	arguments[i] = arguments[i].val;
    return "".concat.apply("", arguments);
}

function sc_stringLength_mutable(s) { /// export
    return s.val.length;
}

function sc_stringLength_immutable(s) { /// export
    return s.length;
}

function sc_stringRef_mutable(s, k) { /// export
    return new sc_Char(s.val.charAt(k));
}

function sc_stringRef_immutable(s, k) { /// export
    return new sc_Char(s.charAt(k));
}

function sc_stringSet_mutable(s, k, c) { /// export string-set!
    var start = s.val.slice(0, k);
    var end = s.val.slice(k+1);
    s.val = start.concat(c.val, end);
}

/* there's no stringSet in the immutable version
function sc_stringSet_immutable(s, k, c)
*/

var sc_isStringEqual_mutable = sc_isCharStringEqual; /// export string=?
var sc_isStringLess_mutable = sc_isCharStringLess; /// export string<?
var sc_isStringGreater_mutable = sc_isCharStringGreater; /// export string>?
var sc_isStringLessEqual_mutable = sc_isCharStringLessEqual; /// export string<=?
var sc_isStringGreaterEqual_mutable = sc_isCharStringGreaterEqual; /// export string>=?
var sc_isStringCIEqual_mutable = sc_isCharStringCIEqual; /// export string-ci=?
var sc_isStringCILess_mutable = sc_isCharStringCILess; /// export string-ci<?
var sc_isStringCIGreater_mutable = sc_isCharStringCIGreater; /// export string-ci>?
var sc_isStringCILessEqual_mutable = sc_isCharStringCILessEqual; /// export string-ci<=?
var sc_isStringCIGreaterEqual_mutable = sc_isCharStringCIGreaterEqual; /// export string-ci>=?

function sc_isStringEqual_immutable(s1, s2) { /// export string=?
    return s1 === s2;
}
function sc_isStringLess_immutable(s1, s2) { /// export string<?
    return s1 < s2;
}
function sc_isStringGreater_immutable(s1, s2) { /// export string>?
    return s1 > s2;
}
function sc_isStringLessEqual_immutable(s1, s2) { /// export string<=?
    return s1 <= s2;
}
function sc_isStringGreaterEqual_immutable(s1, s2) { /// export string>=?
    return s1 >= s2;
}
function sc_isStringCIEqual_immutable(s1, s2) { /// export string-ci=?
    return s1.toLowerCase() === s2.toLowerCase();
}
function sc_isStringCILess_immutable(s1, s2) { /// export string-ci<?
    return s1.toLowerCase() < s2.toLowerCase();
}
function sc_isStringCIGreater_immutable(s1, s2) { /// export string-ci>?
    return s1.toLowerCase() > s2.toLowerCase();
}
function sc_isStringCILessEqual_immutable(s1, s2) { /// export string-ci<=?
    return s1.toLowerCase() <= s2.toLowerCase();
}
function sc_isStringCIGreaterEqual_immutable(s1, s2) { /// export string-ci>=?
    return s1.toLowerCase() >= s2.toLowerCase();
}

function sc_substring_mutable(s, start, end) { /// export
    return new sc_String(s.val.substring(start, end));
}

function sc_substring_immutable(s, start, end) { /// export
    return s.substring(start, end);
}

function sc_stringAppend_mutable() { /// export
    for (var i = 0; i < arguments.length; i++)
	arguments[i] = arguments[i].val;
    return new sc_String("".concat.apply("", arguments));
}

function sc_stringAppend_immutable() { /// export
    return "".concat.apply("", arguments);
}

function sc_string2list_mutable(s) { /// export
    return sc_string2list_immutable(s.val);
}

function sc_string2list_immutable(s) { /// export
    var res = null;
    for (var i = s.length - 1; i >= 0; i--)
	res = sc_cons(new sc_Char(s.charAt(i)), res);
    return res;
}

function sc_list2string_mutable(l) { /// export
    return new sc_String(sc_list2string_immutable(l));
}

function sc_list2string_immutable(l) { /// export
    var a = new Array();
    while(l != null) {
	a.push(l.car.val);
	l = l.cdr;
    }
    return "".concat.apply("", a);
}

function sc_stringCopy_mutable(s) { /// export
    return new sc_String(s.val);
}

function sc_stringCopy_immutable(s) { /// export
    return s;
}

function sc_stringFill_mutable(s, c) { /// export string-fill!
    s.val = sc_makeJSStringOfLength(s.val.length, c.val);
}

/* there's no string-fill in the immutable version
function sc_stringFill_immutable(s, c)
*/

var sc_Vector = Array;

sc_Vector.prototype.toString = function() {
    var res = "#(";
    for (var i = 0; i < this.length; i++)
	res += this[i] + " ";
    return res + ")";
}

function sc_isVector(v) { /// export vector? array?
    return (v instanceof sc_Vector);
}

// only applies to vectors
function sc_isVectorEqual(v1, v2) {
    if (v1.length != v2.length) return false;
    for (var i = 0; i < v1.length; i++)
	if (!sc_isEqual(v1[i], v2[i])) return false;
    return true;
}

function sc_makeVector(size, fill) { /// export make-vector make-array
    var a = new sc_Vector(size);
    if (fill != undefined)
	sc_vectorFill(a, fill);
    return a;
}

function sc_vector() { /// export vector array
    var a = new sc_Vector();
    for (var i = 0; i < arguments.length; i++)
	a.push(arguments[i]);
    return a;
}

function sc_vectorLength(v) { /// export vector-length array-length
    return v.length;
}

function sc_vectorRef(v, pos) { /// export vector-ref array-ref
    return v[pos];
}

function sc_vectorSet(v, pos, val) { /// export vector-set! array-set!
    v[pos] = val;
}

function sc_vector2list(a) { /// export vector->list array->list
    var res = null;
    for (var i = a.length-1; i >= 0; i--)
	res = sc_cons(a[i], res);
    return res;
}

function sc_list2vector(l) { /// export list->vector list->array
    var a = new sc_Vector();
    while(l != null) {
	a.push(l.car);
	l = l.cdr;
    }
    return a;
}

function sc_vectorFill(a, fill) { /// export vector-fill! array-fill!
    for (var i = 0; i < a.length; i++)
	a[i] = fill;
}

function sc_isProcedure(o) { /// export
    return (typeof o === "function");
}

function sc_apply(proc) { /// export
    var args = new Array();
    // first part of arguments are not in list-form.
    for (var i = 1; i < arguments.length - 1; i++)
	args.push(arguments[i]);
    var l = arguments[arguments.length - 1];
    while (l != null) {
	args.push(l.car);
	l = l.cdr;
    }
    return proc.apply(null, args);
}

function sc_map1(proc, l1) {
    var revres = null;
    while (l1 !== null) {
	revres = sc_cons(proc(l1.car), revres);
	l1 = l1.cdr;
    }
    return sc_destReverseAppend(revres, null);
}
function sc_map2(proc, l1, l2) {
    var revres = null;
    while (l1 !== null) {
	var revres = sc_cons(proc(l1.car, l2.car), revres);
	l1 = l1.cdr;
	l2 = l2.cdr
    }
    return sc_destReverseAppend(revres, null);
}
function sc_map(proc, l1, l2) { /// export
    if (arguments.length == 2)
	return sc_map1(proc, l1);
    else if (arguments.length == 3)
	return sc_map2(proc, l1, l2);
    // else
    var nbApplyArgs = arguments.length - 1;
    var applyArgs = new Array(nbApplyArgs);
    var revres = null;
    while (l1 !== null) {
	for (var i = 0; i < nbApplyArgs; i++) {
	    applyArgs[i] = arguments[i + 1].car;
	    arguments[i + 1] = arguments[i + 1].cdr;
	}
	revres = sc_cons(proc.apply(null, applyArgs), revres);
    }
    return sc_destReverseAppend(revres, null);
}

function sc_forEach1(proc, l1) {
    while (l1 !== null) {
	proc(l1.car);
	l1 = l1.cdr;
    }
}
function sc_forEach2(proc, l1, l2) {
    while (l1 !== null) {
	proc(l1.car, l2.car);
	l1 = l1.cdr;
	l2 = l2.cdr;
    }
}
function sc_forEach(proc, l1) { /// export
    if (arguments.length == 2)
	return sc_forEach1(proc, l1);
    else if (arguments.length == 3)
	return sc_forEach2(proc, l1, l2);
    // else
    var nbApplyArgs = arguments.length - 1;
    var applyArgs = new Array(nbApplyArgs);
    while (l1 !== null) {
	for (var i = 0; i < nbApplyArgs; i++) {
	    applyArgs[i] = arguments[i + 1].car;
	    arguments[i + 1] = arguments[i + 1].cdr;
	}
	proc.apply(null, applyArgs);
    }
}

function sc_force(o) { /// export
    return o();
}
function sc_makePromise(proc) { /// export
    var isResultReady = false;
    var result = undefined;
    return function() {
	if (!isResultReady) {
	    var tmp = proc();
	    if (!isResultReady) {
		isResultReady = true;
		result = tmp;
	    }
	}
	return result;
    };
}

// TODO: call-with-current-continuation (and adapt dynamic-wind)

function sc_values() { /// export
    return arguments;
}

function sc_callWithValues(producer, consumer) { /// export
    consumer.apply(null, producer());
}

function sc_dynamicWind(before thunk after) { /// export
    before();
    var res = thunk();
    after();
    return res;
}
    
// TODO: eval/scheme-report-environment/null-environment/interaction-environment

// LIMITATION: 'load' doesn't exist without files.
// LIMITATION: transcript-on/transcript-off doesn't exist without files.


function sc_Struct(name) {
    this.name = name;
}

function sc_makeStruct(name) { /// export
    return new sc_Struct(name);
}

function sc_isStruct(o) { /// export
    return (o instanceof sc_Struct);
}

function sc_isStructNamed(name, s) { /// export
    return ((s instanceof sc_Struct) && (s.name === name));
}

function sc_getStructField(s, name, field) { /// export struct-field
    return s[field];
}

function sc_setStructField(s, name, field, val) { /// export struct-field-set!
    s[field] = val;
}

function sc_bitAnd(x, y) { /// export
    return x & y;
}

function sc_bitOr(x, y) { /// export
    return x | y;
}

function sc_bitXor(x, y) { /// export
    return x ^ y;
}

function sc_bitLsh(x, y) { /// export
    return x << y;
}

function sc_bitRsh(x, y) { /// export
    return x >> y;
}

function sc_bitUrsh(x, y) { /// export
    return x >>> y;
}

function sc_jsField(o, field) { /// export
    return o[field];
}

function sc_setJsField(o, field, val) { /// export js-field-set!
    return o[field] = val;
}

function sc_deleteJsField(o, field) { /// export js-field-delete!
    delete o[field];
}

function sc_jsCall(o, fun) { /// export
    var args = new Array();
    for (var i = 2; i < arguments.length; i++)
	args[i-2] = arguments[i];
    return fun.apply(o, args);
}

function sc_jsMethodCall(o, field) { /// export
    var args = new Array();
    for (var i = 2; i < arguments.length; i++)
	args[i-2] = arguments[i];
    return o[field].apply(o, args);
}

function sc_jsNew(c) { /// export new js-new
    var evalStr = "new c(";
    evalStr +=arguments.length > 1? "arguments[1]": "";
    for (var i = 2; i < arguments.length; i++)
	evalStr += ", arguments[" + i + "]";
    evalStr +=")";
    return eval(evalStr);
}    

// Keywords
function sc_Keyword(str) {
    var cached = sc_Keyword.lazy[str];

    if (cached)
	return cached;

    sc_Keyword.lazy[str] = this;
    this.val = str;
}
sc_Keyword.lazy = new Object;

sc_Keyword.prototype.toString = function() {
    return ":" + this.val;
}
sc_Keyword.prototype.toJSString = function() {
    return this.val;
}

function sc_isKeyword(o) { /// export
    return (o instanceof sc_Keyword);
}

function sc_keyword2string_mutable(o) { /// export
    return new sc_String(o.val);
}

function sc_keyword2string_immutable(o) { /// export
    return o.val;
}

function sc_string2keyword_mutable(o) { /// export
    return new sc_Keyword(o.val);
}

function sc_string2keyword_immutable(o) { /// export
    return new sc_Keyword(o);
}


// ======================== I/O =======================

/*------------------------------------------------------------------*/

function sc_EOF() {
}
var SC_EOF_OBJECT = new sc_EOF();

function sc_Port() {
}

/* --------------- Input ports -------------------------------------*/

function sc_InputPort() {
}
sc_InputPort.prototype = new sc_Port;

sc_InputPort.prototype.peekChar = function() {
    if (!("peeked" in this))
	this.peeked = this.getNextChar();
    return this.peeked;
}
sc_InputPort.prototype.readChar = function() {
    var tmp = this.peekChar();
    delete this.peeked;
    return tmp;
}
sc_InputPort.prototype.isCharReady = function() {
    return true;
}
sc_InputPort.prototype.close = function() {
    // do nothing
}

/* .............. String port ..........................*/
function sc_ErrorInputPort() {
};
sc_ErrorInputPort.prototype = new sc_InputPort();
sc_ErrorInputPort.getNextChar = function() {
    throw "can't read from error-port.";
};
sc_ErrorInputPort.isCharReady = function() {
    return false;
};
    

/* .............. String port ..........................*/

function sc_StringInputPort(jsStr) {
    // we are going to do some charAts on the str.
    // instead of recreating all the time a String-object, we
    // create one in the beginning. (not sure, if this is really an optim)
    str = new String(jsStr);
    this.str = str;
    this.pos = 0;
}
sc_StringInputPort.prototype = new sc_InputPort;
sc_StringInputPort.prototype.getNextChar = function() {
    if (this.pos >= this.str.length)
	return SC_EOF_OBJECT;
    return this.str.charAt(this.pos++);
};

/* ------------- Read and other lib-funs  -------------------------------*/
function sc_Token(type, val, pos) {
    this.type = type;
    this.val = val;
    this.pos = pos;
}
sc_Token.EOF = 0/*EOF*/;
sc_Token.OPEN_PAR = 1/*OPEN_PAR*/;
sc_Token.CLOSE_PAR = 2/*CLOSE_PAR*/;
sc_Token.OPEN_BRACE = 3/*OPEN_BRACE*/;
sc_Token.CLOSE_BRACE = 4/*CLOSE_BRACE*/;
sc_Token.OPEN_BRACKET = 5/*OPEN_BRACKET*/;
sc_Token.CLOSE_BRACKET = 6/*CLOSE_BRACKET*/;
sc_Token.WHITESPACE = 7/*WHITESPACE*/;
sc_Token.QUOTE = 8/*QUOTE*/;
sc_Token.ID = 9/*ID*/;
sc_Token.DOT = 10/*DOT*/;
sc_Token.STRING = 11/*STRING*/;
sc_Token.NUMBER = 12/*NUMBER*/;
sc_Token.ERROR = 13/*ERROR*/;
sc_Token.VECTOR_BEGIN = 14/*VECTOR_BEGIN*/;
sc_Token.TRUE = 15/*TRUE*/;
sc_Token.FALSE = 16/*FALSE*/;
sc_Token.UNSPECIFIED = 17/*UNSPECIFIED*/;
sc_Token.REFERENCE = 18/*REFERENCE*/;
sc_Token.STORE = 19/*STORE*/;
sc_Token.CHAR = 20/*CHAR*/;

var SC_ID_CLASS = SC_LOWER_CLASS + SC_UPPER_CLASS + "!$%*+-./:<=>?@^_~";
function sc_Tokenizer(port) {
    this.port = port;
}
sc_Tokenizer.prototype.peekToken = function() {
    if (this.peeked)
	return this.peeked;
    var newToken = this.nextToken();
    this.peeked = newToken;
    return newToken;
};
sc_Tokenizer.prototype.readToken = function() {
    var tmp = this.peekToken();
    delete this.peeked;
    return tmp;
};
sc_Tokenizer.prototype.nextToken = function() {
    var port = this.port;
    
    function isNumberChar(c) {
	return (c >= "0" && c <= "9");
    };
    function isIdOrNumberChar(c) {
	return SC_ID_CLASS.indexOf(c) != -1 || // ID-char
	    (c >= "0" && c <= "9");
    }
    function isWhitespace(c) {
	return c === " " || c === "\r" || c === "\n" || c === "\t" || c === "\f";
    };
    function isWhitespaceOrEOF(c) {
	return isWhitespace(c) || c === SC_EOF_OBJECT;
    };

    function readString() {
	res = "";
	while (true) {
	    var c = port.readChar();
	    switch (c) {
	    case '"':
		return new sc_Token(11/*STRING*/, res);
	    case "\\":
		var tmp = port.readChar();
		switch (tmp) {
		case '0': res += "\0"; break;
		case 'a': res += "\a"; break;
		case 'b': res += "\b"; break;
		case 'f': res += "\f"; break;
		case 'n': res += "\n"; break;
		case 'r': res += "\r"; break;
		case 't': res += "\t"; break;
		case 'v': res += "\v"; break;
		case '"': res += '"'; break;
		case '\\': res += '\\'; break;
		case 'x':
		    /* hexa-number */
		    var nb = 0;
		    while (true) {
			var hexC = port.peekChar();
			if (hexC >= '0' && hexC <= '9') {
			    port.readChar();
			    nb = nb * 16 + hexC.charCodeAt(0) - '0'.charCodeAt(0);
			} else if (hexC >= 'a' && hexC <= 'f') {
			    port.readChar();
			    nb = nb * 16 + hexC.charCodeAt(0) - 'a'.charCodeAt(0);
			} else if (hexC >= 'A' && hexC <= 'F') {
			    port.readChar();
			    nb = nb * 16 + hexC.charCodeAt(0) - 'A'.charCodeAt(0);
			} else {
			    // next char isn't part of hex.
			    res += String.fromCharCode(nb);
			    break;
			}
		    }
		    break;
		default:
		    if (tmp === SC_EOF_OBJECT) {
			return new sc_Token(13/*ERROR*/, "unclosed string-literal" + res);
		    }
		    res += tmp;
		}
		break;
	    default:
		if (c === SC_EOF_OBJECT) {
		    return new sc_Token(13/*ERROR*/, "unclosed string-literal" + res);
		}
		res += c;
	    }
	}
    };
    function readIdOrNumber(firstChar) {
	var res = firstChar;
	while (isIdOrNumberChar(port.peekChar()))
	    res += port.readChar();
	if (isNaN(res))
	    return new sc_Token(9/*ID*/, res);
	else
	    return new sc_Token(12/*NUMBER*/, res - 0);
    };
    
    function skipWhitespaceAndComments() {
	var done = false;
	while (!done) {
	    done = true;
	    while (isWhitespace(port.peekChar()))
		port.readChar();
	    if (port.peekChar() === ';') {
		port.readChar();
		done = false;
		while (true) {
		    curChar = port.readChar();
		    if (curChar === SC_EOF_OBJECT ||
			curChar === '\n')
			break;
		}
	    }
	}
    };
    
    function readDot() {
	if (isWhitespace(port.peekChar()))
	    return new sc_Token(10/*DOT*/);
	else
	    return readIdOrNumber(".");
    };

    function readSharp() {
	var c = port.readChar();
	if (isWhitespace(c))
	    return new sc_Token(13/*ERROR*/, "bad #-pattern0.");

	// reference
	if (isNumberChar(c)) {
	    var nb = c - 0;
	    while (isNumberChar(port.peekChar()))
		nb = nb*10 + (port.readChar() - 0);
	    switch (port.readChar()) {
	    case '#':
		return new sc_Token(18/*REFERENCE*/, nb);
	    case '=':
		return new sc_Token(19/*STORE*/, nb);
	    default:
		return new sc_Token(13/*ERROR*/, "bad #-pattern1." + nb);
	    }
	}

	if (c === "(")
	    return new sc_Token(14/*VECTOR_BEGIN*/);
	
	if (c === "\\") { // character
	    var tmp = ""
	    while (!isWhitespaceOrEOF(port.peekChar()))
		tmp += port.readChar();
	    switch (tmp.length) {
	    case 0: // it's escaping a whitespace char:
		if (sc_isEOFObject(port.peekChar))
		    return new sc_Token(13/*ERROR*/, "bad #-pattern2.");
		else
		    return new sc_Token(20/*CHAR*/, port.readChar());
	    case 1:
		return new sc_Token(20/*CHAR*/, tmp);
	    default:
		var entry = sc_Char.readable2char[tmp.toLowerCase()];
		if (entry)
		    return new sc_Token(20/*CHAR*/, entry);
		else
		    return new sc_Token(13/*ERROR*/, "unknown character description: #\\" + tmp);
	    }
	}

	// some constants (#t, #f, #unspecified)
	var res;
	var needing;
	switch (c) {
	case 't': res = new sc_Token(15/*TRUE*/, true); needing = ""; break;
	case 'f': res = new sc_Token(16/*FALSE*/, false); needing = ""; break;
	case 'u': res = new sc_Token(17/*UNSPECIFIED*/, undefined); needing = "nspecified"; break;
	default:
	    return new sc_Token(13/*ERROR*/, "bad #-pattern3: " + c);
	}
	while(true) {
	    c = port.peekChar();
	    if ((isWhitespaceOrEOF(c) || c === ')') &&
		needing == "")
		return res;
	    else if (isWhitespace(c) || needing == "")
		return new sc_Token(13/*ERROR*/, "bad #-pattern4 " + c + " " + needing);
	    else if (needing.charAt(0) == c) {
		port.readChar(); // consume
		needing = needing.slice(1);
	    } else
		return new sc_Token(13/*ERROR*/, "bad #-pattern5");
	}
	
    };

    skipWhitespaceAndComments();
    var curChar = port.readChar();
    if (curChar === SC_EOF_OBJECT)
	return new sc_Token(0/*EOF*/, curChar);
    switch (curChar)
    {
    case " ":
    case "\n":
    case "\t":
	return readWhitespace();
    case "(":
	return new sc_Token(1/*OPEN_PAR*/);
    case ")":
	return new sc_Token(2/*CLOSE_PAR*/);
    case "{":
	return new sc_Token(3/*OPEN_BRACE*/);
    case "}":
	return new sc_Token(4/*CLOSE_BRACE*/);
    case "[":
	return new sc_Token(5/*OPEN_BRACKET*/);
    case "]":
	return new sc_Token(6/*CLOSE_BRACKET*/);
    case "'":
	return new sc_Token(8/*QUOTE*/);
    case "#":
	return readSharp();
    case ".":
	return readDot();
    case '"':
	return readString();
    default:
	if (isIdOrNumberChar(curChar))
	    return readIdOrNumber(curChar);
	throw "unexpected character: " + curChar;
    }
};

function sc_Reader(tokenizer) {
    this.tokenizer = tokenizer;
    this.backref = new Array();
}
sc_Reader.prototype.read = function() {
    function readList(listBeginType) {
	function matchesPeer(open, close) {
	    return open === 1/*OPEN_PAR*/ && close === 2/*CLOSE_PAR*/
	    	|| open === 3/*OPEN_BRACE*/ && close === 4/*CLOSE_BRACE*/
		|| open === 5/*OPEN_BRACKET*/ && close === 6/*CLOSE_BRACKET*/;
	};
	var token = tokenizer.peekToken();
	switch (token.type) {
	case 2/*CLOSE_PAR*/:
	case 4/*CLOSE_BRACE*/:
	case 6/*CLOSE_BRACKET*/:
	    if (matchesPeer(listBeginType, token.type)) {
		tokenizer.readToken(); // consume token
		return null;
	    } else
		throw "closing par doesn't match: " + listBeginType
		    + " " + listEndType;

	case 0/*EOF*/:
	    throw "unexpected end of file";

	case 10/*DOT*/:
	    tokenizer.readToken(); // consume token
	    var cdr = this.read();
	    var par = tokenizer.readToken();
	    if (!matchesPeer(listBeginType, par.type))
		throw "closing par doesn't match: " + listBeginType
		    + " " + par.type;
	    else
		return cdr;
		

	default:
	    return sc_cons(this.read(), readList.call(this, listBeginType))
	}
    };
    function readQuote() {
	return sc_cons("quote", sc_cons(this.read(), null));
    };
    function readVector() {
	// opening-parenthesis is already consumed
	var a = new Array();
	while (true) {
	    var token = tokenizer.peekToken();
	    switch (token.type) {
	    case 2/*CLOSE_PAR*/:
		tokenizer.readToken();
		return a;
		
	    default:
		a.push(this.read());
	    }
	}
    };

    function storeRefence(nb) {
	var tmp = this.read();
	this.backref[nb] = tmp;
	return tmp;
    };
	
    function readReference(nb) {
	if (nb in this.backref)
	    return this.backref[nb];
	else
	    throw "bad reference: " + nb;
    };
    
    var tokenizer = this.tokenizer;

    var token = tokenizer.readToken();

    // handle error
    if (token.type === 13/*ERROR*/)
	throw token.val;
    
    switch (token.type) {
    case 1/*OPEN_PAR*/:
    case 3/*OPEN_BRACE*/:
    case 5/*OPEN_BRACKET*/:
	return readList.call(this, token.type);
    case 8/*QUOTE*/:
	return readQuote.call(this);
    case 11/*STRING*/:
	if (this.mutableStrings)
	    return new sc_String(token.val);
	else
	    return token.val;
    case 20/*CHAR*/:
	return new sc_Char(token.val);
    case 14/*VECTOR_BEGIN*/:
	return readVector.call(this);
    case 18/*REFERENCE*/:
	return readReference.call(this, token.val);
    case 19/*STORE*/:
	return storeRefence.call(this, token.val);
    case 9/*ID*/:
	if (this.mutableStrings)
	    return token.val;
	else
	    return sc_SYMBOL_PREFIX + token.val;
    case 0/*EOF*/:
    case 12/*NUMBER*/:
    case 15/*TRUE*/:
    case 16/*FALSE*/:
    case 17/*UNSPECIFIED*/:
	return token.val;
    default:
	throw "unexpected token " + token.type + " " + token.val;
    }
};

function sc_read_mutable(port) { /// export
    if (port === undefined) // we assume the port hasn't been given.
	port = SC_DEFAULT_IN; // THREAD: shared var...
    var reader = new sc_Reader(new sc_Tokenizer(port));
    reader.mutableStrings = true;
    return reader.read();
}
function sc_read_immutable(port) { /// export
    if (port === undefined) // we assume the port hasn't been given.
	port = SC_DEFAULT_IN; // THREAD: shared var...
    var reader = new sc_Reader(new sc_Tokenizer(port));
    return reader.read();
}
function sc_readChar(port) { /// export
    if (port === undefined) // we assume the port hasn't been given.
	port = SC_DEFAULT_IN; // THREAD: shared var...
    return port.readChar();
}
function sc_peekChar(port) { /// export
    if (port === undefined) // we assume the port hasn't been given.
	port = SC_DEFAULT_IN; // THREAD: shared var...
    return port.peekChar();
}    
function sc_isCharReady(port) { /// export
    if (port === undefined) // we assume the port hasn't been given.
	port = SC_DEFAULT_IN; // THREAD: shared var...
    return port.isCharReady();
}
function sc_closeInputPort(p) { /// export
    return p.close();
}

function sc_isInputPort(o) { /// export
    return (o instanceof sc_InputPort);
}

function sc_isEOFObject(o) { /// export eof-object?
    return o === SC_EOF_OBJECT;
}

function sc_currentInputPort() { /// export
    return SC_DEFAULT_IN;
}

/* ------------ file operations are not supported -----------*/
function sc_callWithInputFile(s, proc) { /// export
    throw "can't open " + s;
}

function sc_callWithOutputFile(s, proc) { /// export
    throw "can't open " + s;
}

function sc_withInputFromFile(s, thunk) { /// export
    throw "can't open " + s;
}

function sc_withOutputToFile(s, thunk) { /// export
    throw "can't open " + s;
}

function sc_openInputFile(s) { /// export
    throw "can't open " + s;
}

function sc_openOutputFile(s) { /// export
    throw "can't open " + s;
}

/* ----------------------------------------------------------------------------*/

function sc_withInputFromString_mutable(s, thunk) { /// export
    return sc_withInputFromString_immutable(s.val, thunk);
}

function sc_withInputFromString_immutable(s, thunk) { /// export
    var tmp = SC_DEFAULT_IN; // THREAD: shared var.
    SC_DEFAULT_IN = new sc_StringInputPort(s);
    var tmp2 = thunk();
    SC_DEFAULT_IN.close(); // just to make sure...
    SC_DEFAULT_IN = tmp;
    return tmp2;
}


function sc_withOutputToString_mutable(thunk) { /// export
    var tmp = SC_DEFAULT_OUT; // THREAD: shared var.
    var outp = new sc_StringOutputPort_mutable();
    SC_DEFAULT_OUT = outp;
    var tmp2 = thunk();
    SC_DEFAULT_OUT = tmp;
    return outp.close();
}

function sc_withOutputToString_immutable(thunk) { /// export
    var tmp = SC_DEFAULT_OUT; // THREAD: shared var.
    var outp = new sc_StringOutputPort_immutable();
    SC_DEFAULT_OUT = outp;
    var tmp2 = thunk();
    SC_DEFAULT_OUT = tmp;
    return outp.close();
}

function sc_openOutputString_mutable() { /// export
    return new sc_StringOutputPort_mutable();
}
function sc_openOutputString_immutable() { /// export
    return new sc_StringOutputPort_immutable();
}

function sc_openInputString_mutable(str) { /// export
    return new sc_StringInputPort(str.val);
}

function sc_openInputString_immutable(str) { /// export
    return new sc_StringInputPort(str);
}

/* ----------------------------------------------------------------------------*/

function sc_OutputPort() {
}
sc_OutputPort.prototype = new sc_Port();
sc_OutputPort.prototype.appendJSString = function(obj) {
    /* do nothing */
}
sc_OutputPort.prototype.close = function() {
    /* do nothing */
}

function sc_StringOutputPort_mutable() {
    this.res = "";
}
sc_StringOutputPort_mutable.prototype = new sc_OutputPort();
sc_StringOutputPort_mutable.prototype.appendJSString = function(s) {
    this.res += s;
}
sc_StringOutputPort_mutable.prototype.close = function() {
    return new sc_String(this.res);
}

function sc_StringOutputPort_immutable() {
    this.res = "";
}
sc_StringOutputPort_immutable.prototype = new sc_OutputPort();
sc_StringOutputPort_immutable.prototype.appendJSString = function(s) {
    this.res += s;
}
sc_StringOutputPort_immutable.prototype.close = function() {
    return this.res;
}

function sc_ErrorOutputPort() {
}
sc_ErrorOutputPort.prototype = new sc_OutputPort();
sc_ErrorOutputPort.prototype.appendJSString = function(s) {
    throw "don't write on ErrorPort!";
}
sc_ErrorOutputPort.prototype.close = function() {
    /* do nothing */
}

function sc_GenericOutputPort(appendJSString, close) {
    this.appendJSString = appendJSString;
    if (close)
	this.close = close;
}
sc_GenericOutputPort.prototype = new sc_OutputPort();

function sc_isOutputPort(o) { /// export
    return (o instanceof sc_OutputPort);
}

function sc_closeOutputPort(p) { /// export
    return p.close();
}

function hop_bigloo_serialize_pair( l ) {
   var res = "";
   var len = 0;
   
   while (sc_isPair( l ) ) {
      res += hop_serialize( l.car );
      l = l.cdr;
      len++;
   }

   if( l == null ) {
      return hop_serialize_word( len + 1 ) + res + ".";
   } else {
      return hop_serialize_word( len + 1 ) + res + hop_serialize( l );
   }
}

sc_Pair.prototype.hop_bigloo_serialize = function() {
   return '(' + hop_bigloo_serialize_pair( this );
}

sc_Pair.prototype.writeOrDisplay = function(p, writeOrDisplay, inList) {
    var isP = sc_isPair(this.cdr);
    if (!inList)
	p.appendJSString("(");
    writeOrDisplay(p, this.car);
    if (sc_isPair(this.cdr)) {
	p.appendJSString(" ");
	this.cdr.writeOrDisplay(p, writeOrDisplay, true);
    } else if (this.cdr !== null) {
	p.appendJSString(" . ");
	writeOrDisplay(p, this.cdr);
    }
    if (!inList)
	p.appendJSString(")");
};
sc_Vector.prototype.writeOrDisplay = function(p, writeOrDisplay) {
    if (this.length === 0)
	p.appendJSString("#()");

    p.appendJSString("#(");
    writeOrDisplay(p, this[0]);
    for (var i = 1; i < this.length; i++) {
	p.appendJSString(" ");
	writeOrDisplay(p, this[i]);
    }
    p.appendJSString(")");
};

/* ------------------ write ---------------------------------------------------*/

// write
function sc_write_mutable(o, p) { /// export
    String.prototype.doWrite = String_prototype_doWrite_mutable;
    if (p === undefined) // we assume not given
	p = SC_DEFAULT_OUT;
    sc_doWrite(p, o);
}

function sc_write_immutable(o, p) { /// export
    String.prototype.doWrite = String_prototype_doWrite_immutable;
    if (p === undefined) // we assume not given
	p = SC_DEFAULT_OUT;
    sc_doWrite(p, o);
}

function sc_doWrite(p, o) {
    if (o === null)
	p.appendJSString("()");
    else if (o === true)
	p.appendJSString("#t");
    else if (o === false)
	p.appendJSString("#f");
    else if (o === undefined)
	p.appendJSString("#unspecified");
    else
	return o.doWrite(p);
}

function sc_escapeWriteString(s) {
    var res = "";
    var j = 0;
    for (i = 0; i < s.length; i++) {
	switch (s.charAt(i)) {
	case "\0": res += s.substring(j, i) + "\\0"; j = i + 1; break;
	    /* \a is not recognized and will escape all 'a' chars.
	case "\a": res += s.substring(j, i) + "\\a"; j = i + 1; break;
	    */
	case "\b": res += s.substring(j, i) + "\\b"; j = i + 1; break;
	case "\f": res += s.substring(j, i) + "\\f"; j = i + 1; break;
	case "\n": res += s.substring(j, i) + "\\n"; j = i + 1; break;
	case "\r": res += s.substring(j, i) + "\\r"; j = i + 1; break;
	case "\t": res += s.substring(j, i) + "\\t"; j = i + 1; break;
	case "\v": res += s.substring(j, i) + "\\v"; j = i + 1; break;
	case '"': res += s.substring(j, i) + '\\"'; j = i + 1; break;
	case "\\": res += s.substring(j, i) + "\\\\"; j = i + 1; break;
	default:
	    if (s.charAt(i) < ' ' || s.charCodeAt(i) > 127) {
		/* non printable character and special chars */
		res += s.substring(j, i) + "\\x" + charCodeAt(i).toString(16);
		j = i + 1;
	    }
	    // else just let i increase...
	}
    }
    res += s.substring(j, i);
    return res;
}

Number.prototype.doWrite = function(p) {
    p.appendJSString(this.toString());
}
String_prototype_doWrite_mutable = function(p) {
    p.appendJSString(this);
}
String_prototype_doWrite_immutable = function(p) {
    // TODO: handle escape-chars symbols

    if (this.charAt(0) !== sc_SYMBOL_PREFIX)
	p.appendJSString('"' + sc_escapeWriteString(this) + '"');
    else
	p.appendJSString(this.slice(1));
}
Function.prototype.doWrite = function(p) {
    p.appendJSString("#<procedure " + this.getHash() + ">");
}
Boolean.prototype.doWrite = function(p) {
    p.appendJSString(this.toString());
}
sc_Pair.prototype.doWrite = function(p) {
    this.writeOrDisplay(p, sc_doWrite);
}
sc_String.prototype.doWrite = function(p) {
    p.appendJSString('"' + sc_escapeWriteString(this.val) + '"');
}
sc_Char.prototype.doWrite = function(p) {
    var entry = sc_Char.char2readable[this.val];
    if (entry)
	p.appendJSString(entry);
    else
	p.appendJSString("#\\" + this.val);
}
sc_Vector.prototype.doWrite = function(p) {
    this.writeOrDisplay(p, sc_doWrite);
}
sc_Struct.prototype.doWrite = function(p) {
    p.appendJSString("#<struct" + this.getHash() + ">");
}
sc_Keyword.prototype.doWrite = function(p) {
    p.appendJSString(":" + this.val);
}

/* ------------------ display ---------------------------------------------------*/

// display
function sc_display_mutable(o, p) { /// export
    String.prototype.doDisplay = String_prototype_doDisplay_mutable;
    if (p === undefined) // we assume not given
	p = SC_DEFAULT_OUT;
    sc_doDisplay(p, o);
}

function sc_display_immutable(o, p) { /// export
    String.prototype.doDisplay = String_prototype_doDisplay_immutable;
    if (p === undefined) // we assume not given
	p = SC_DEFAULT_OUT;
    sc_doDisplay(p, o);
}

function sc_doDisplay(p, o) {
    if (o === null)
	p.appendJSString("()");
    else if (o === true)
	p.appendJSString("#t");
    else if (o === false)
	p.appendJSString("#f");
    else if (o === undefined)
	p.appendJSString("#unspecified");
    else
	return o.doDisplay(p);
}

Number.prototype.doDisplay = Number.prototype.doWrite;

String_prototype_doDisplay_mutable = String_prototype_doWrite_mutable;
String_prototype_doDisplay_immutable = function(p) {
    if (this.charAt(0) !== sc_SYMBOL_PREFIX)
	p.appendJSString(this);
    else
	p.appendJSString(this.slice(1));
}
Function.prototype.doDisplay = Function.prototype.doWrite;
Boolean.prototype.doDisplay = Boolean.prototype.doWrite;
sc_Pair.prototype.doDisplay = function(p) {
    this.writeOrDisplay(p, sc_doDisplay);
};
sc_String.prototype.doDisplay = function(p) {
    p.appendJSString(this.val);
}
sc_Char.prototype.doDisplay = function(p) {
    p.appendJSString(this.val);
}
sc_Vector.prototype.doDisplay = function(p) {
    this.writeOrDisplay(p, sc_doDisplay);
}
sc_Struct.prototype.doDisplay = sc_Struct.prototype.doWrite;
sc_Keyword.prototype.doDisplay = sc_Keyword.prototype.doWrite;

/* ------------------ newline ---------------------------------------------------*/

function sc_newline(p) { /// export
    if (p === undefined) // we assume not given
	p = SC_DEFAULT_OUT;
    p.appendJSString("\n");
}
    
/* ------------------ write-char ---------------------------------------------------*/

function sc_writeChar(c, p) { /// export
    if (p === undefined) // we assume not given
	p = SC_DEFAULT_OUT;
    p.appendJSString(c.val);
}

/* ------------------ write-circle ---------------------------------------------------*/

function sc_writeCircle_immutable(o, p) { /// export
    String.prototype.doWriteCircle = String_prototype_doWriteCirce_immutable;
    sc_writeCirce(o, p);
}
function sc_writeCircle_mutable(o, p) { /// export
    String.prototype.doWriteCircle = String_prototype_doWriteCirce_mutable;
    sc_writeCirce(o, p);
}

function sc_writeCircle(o, p) {
    var symb = sc_gensym("writeCircle");
    var nbPointer = new Object();
    nbPointer.nb = 0;
    sc_prepWriteCircle(o, symb, nbPointer);
    
    if (p === undefined) // we assume not given
	p = SC_DEFAULT_OUT;
    sc_doWriteCircle(p, o, symb);
}

function sc_prepWriteCircle(o, symb, nbPointer) {
    if (o !== null &&
	o !== undefined &&
	o !== true &&
	o !== false)
	o.prepWriteCircle(symb, nbPointer);
}
var doNothing = function() {};
Number.prototype.prepWriteCircle = doNothing;
String.prototype.prepWriteCircle = doNothing;
Function.prototype.prepWriteCircle = doNothing;
Boolean.prototype.prepWriteCircle = doNothing;
sc_Pair.prototype.prepWriteCircle = function(symb, nbPointer) {
    if (this[symb] !== undefined) {
	this[symb]++;
	this[symb + "nb"] = nbPointer.nb++;
    } else {
	this[symb] = 0;
	sc_prepWriteCircle(this.car, symb, nbPointer);
	sc_prepWriteCircle(this.cdr, symb, nbPointer);
    }
};
sc_String.prototype.prepWriteCircle = doNothing;
sc_Char.prototype.prepWriteCircle = doNothing;
sc_Vector.prototype.prepWriteCircle = function(symb, nbPointer) {
    if (this[symb] !== undefined) {
	this[symb]++;
	this[symb + "nb"] = nbPointer.nb++;
    } else {
	this[symb] = 0;
	for (var i = 0; i < this.length; i++)
	    sc_prepWriteCircle(this[i], symb, nbPointer);
    }
};
// TODO: not correct
sc_Struct.prototype.prepWriteCircle = doNothing;

function sc_doWriteCircle(p, o, symb) {
    if (o === null)
	p.appendJSString("()");
    else if (o === true)
	p.appendJSString("#t");
    else if (o === false)
	p.appendJSString("#f");
    else if (o === undefined)
	p.appendJSString("#unspecified");
    else
	o.doWriteCircle(p, symb);
}

// extra arguments (in our case 'symb') are going to be lost. so no prob.
Number.prototype.doWriteCircle = Number.prototype.doWrite;
String_prototype_doWriteCircle_mutable = String_prototype_doWrite_mutable;
String_prototype_doWriteCircle_immutable = String_prototype_doWrite_immutable;
Function.prototype.doWriteCircle = Function.prototype.doWrite;
Boolean.prototype.doWriteCircle = Boolean.prototype.doWrite;

sc_Pair.prototype.doWriteCircle = function(p, symb, inList) {
    if (this[symb + "use"]) { // use-flag is set. Just use it.
	var nb = this[symb + "nb"];
	if (this[symb]-- === 0) { // if we are the last use. remove all fields.
	    delete this[symb];
	    delete this[symb + "nb"];
	    delete this[symb + "use"];
	}
	if (inList)
	    p.appendJSString('. #' + nb + '#');
	else
	    p.appendJSString('#' + nb + '#');
	
	return;
    }
    if (this[symb]-- === 0) { // if we are the last use. remove all fields.
	delete this[symb];
	delete this[symb + "nb"];
	delete this[symb + "use"];
    }

    if (this[symb] !== undefined) { // implies > 0
	this[symb + "use"] = true;
	if (inList)
	    p.appendJSString('. #' + this[symb + "nb"] + '=');
	else
	    p.appendJSString('#' + this[symb + "nb"] + '=');
	inList = false;
    }

    if (!inList)
	p.appendJSString("(");
    
    // print car
    sc_doWriteCircle(p, this.car, symb);
    
    if (sc_isPair(this.cdr)) {
	p.appendJSString(" ");
	this.cdr.doWriteCircle(p, symb, true);
    } else if (this.cdr !== null) {
	p.appendJSString(" . ");
	sc_doWriteCircle(p, this.cdr);
    }
    if (!inList)
	p.appendJSString(")");
}
sc_String.prototype.doWriteCircle = sc_String.prototype.doWrite;
sc_Char.prototype.doWriteCircle = sc_Char.prototype.doWrite;

sc_Vector.prototype.doWriteCircle = function(p, symb) {
    if (this[symb + "use"]) { // use-flag is set. Just use it.
	var nb = this[symb + "nb"];
	if (this[symb]-- === 0) { // if we are the last use. remove all fields.
	    delete this[symb];
	    delete this[symb + "nb"];
	    delete this[symb + "use"];
	}
	p.appendJSString('#' + nb + '#');
	return;
    }
    if (this[symb]-- === 0) { // if we are the last use. remove all fields.
	delete this[symb];
	delete this[symb + "nb"];
	delete this[symb + "use"];
    }
    if (this[symb] !== undefined) { // implies > 0
	this[symb + "use"] = true;
	p.appendJSString('#' + this[symb + "nb"] + '=');
    }
    p.appendJSString("#(");
    for (var i = 0; i < this.length; i++) {
	sc_doWriteCircle(p, this[i], symb);
	p.appendJSString(" ");
    }
    p.appendJSString(")");
}
sc_Struct.prototype.doWriteCircle = sc_Struct.prototype.doWrite;


/* ------------------ print ---------------------------------------------------*/

function sc_print_mutable(s) { /// export
    if (arguments.length === 1) {
	sc_display_mutable(s);
	sc_newline();
    }
    else {
	for (var i = 0; i < arguments.length; i++)
	    sc_display_mutable(arguments[i]);
	sc_newline();
    }
}

function sc_print_immutable(s) { /// export
    if (arguments.length === 1) {
	sc_display_immutable(s);
	sc_newline();
    }
    else {
	for (var i = 0; i < arguments.length; i++)
	    sc_display_immutable(arguments[i]);
	sc_newline();
    }
}

/* ------------------ global ports ---------------------------------------------------*/

var SC_DEFAULT_IN = new sc_ErrorInputPort();
var SC_DEFAULT_OUT = new sc_ErrorOutputPort();
var SC_ERROR_OUT = new sc_ErrorOutputPort();

/* ------------------ initRuntime ---------------------------------------------------*/

function initRuntime() {
    function escapeHTML(s) {
	var tmp = s;
	tmp = tmp.replace(/&/g, "&amp;");
	tmp = tmp.replace(/</g, "&lt;");
	tmp = tmp.replace(/>/g, "&gt;");
	tmp = tmp.replace(/ /g, "&nbsp;");
	tmp = tmp.replace(/\n/g, "<br />");
	tmp = tmp.replace(/\t/g, "&nbsp;&nbsp;&nbsp;&nbsp");
	return tmp;
	
    }

    document.write("<div id='stdout'></div>");
    SC_DEFAULT_OUT = new sc_GenericOutputPort(
	function(s) {
	    var stdout = document.getElementById('stdout');
	    stdout.innerHTML = stdout.innerHTML + escapeHTML(s);
	});
    SC_ERROR_OUT = SC_DEFAULT_OUT;
}



/* =========================================================================== */
/* Other library stuff */
/* =========================================================================== */


function sc_Hashtable() {
}
sc_Hashtable.prototype.toString = function() {
    return "#{%hashtable}";
}
function sc_HashtableElement(key, val) {
    this.key = key;
    this.val = val;
}

function sc_makeHashtable() { /// export
    return new sc_Hashtable();
}

function sc_hashtablePut(ht, key, val) { /// export hashtable-put!
    var hash = sc_hash(key);
    ht[hash] = new sc_HashtableElement(key, val);
}

function sc_hashtableGet(ht, key) { /// export
    var hash = sc_hash(key);
    if (hash in ht)
	return ht[hash].val;
    else
	return false;
}

function sc_hashtableForEach(ht, f) { /// export
    for (var v in ht) {
	if (ht[v] instanceof sc_HashtableElement)
	    f(ht[v].key, ht[v].val);
    }
}

function sc_hashtableContains(ht, key) { /// export hashtable-contains?
    var hash = sc_hash(key);
    if (hash in ht)
	return true;
    else
	return false;
}

var SC_HASH_COUNTER = 0;

function sc_hash(o) {
    if (o === null)
	return "null";
    else if (o === undefined)
	return "undefined";
    else
	return o.getHash();
}
function sc_counterHash() {
    if (!this.hash) {
	this.hash = "hash-" + SC_HASH_COUNTER;
	SC_HASH_COUNTER++;
    }
    return this.hash;
}
Number.prototype.getHash = function() {
    return "num-" + this;
}
String.prototype.getHash = function() {
    return "sym-" + this;
}
Boolean.prototype.getHash = function() {
    return "" + this;
}
Function.prototype.getHash = sc_counterHash;
sc_Pair.prototype.getHash = sc_counterHash;
sc_Vector.prototype.getHash = sc_counterHash;
sc_Struct.prototype.getHash = sc_counterHash;
sc_String.prototype.getHash = function() {
    return "str-" + this.val;
}
sc_Char.prototype.getHash = sc_counterHash;
sc_Hashtable.prototype.getHash = sc_counterHash;
