/*=====================================================================*/
/*    Author      :  Florian Loitsch                                   */
/*    Copyright   :  2007-15 Florian Loitsch, see LICENSE file         */
/*    -------------------------------------------------------------    */
/*    This file is part of Scheme2Js.                                  */
/*                                                                     */
/*   Scheme2Js is distributed in the hope that it will be useful,      */
/*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
/*   LICENSE file for more details.                                    */
/*=====================================================================*/

/*
 * To use write/prints/... the default-output port has to be set first.
 * Simply setting SC_DEFAULT_OUT and SC_ERROR_OUT to the desired values
 * should do the trick.
 * In the following example the std-out and error-port are redirected to
 * a DIV.
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
*/

var sc_lambda, sc_let, sc_context;

function sc_print_debug() {
    sc_print.apply(null, arguments);
}
/*** META ((export *js*)) */
var sc_JS_GLOBALS = this;

var __sc_LINE=-1;
var __sc_FILE="";

/*** META ((export #t)
           (arity -1)) */
function sc_alert() {
   var len = arguments.length;
   var s = "";
   var i;

   for( i = 0; i < len; i++ ) {
       s += sc_toDisplayString(arguments[ i ]);
   }

   return alert( s );
}

/*** META ((export #t) (arity #t)) */
function sc_typeof( x ) {
   if( sc_isSymbol( x ) ) {
      return "symbol";
   } else if( sc_isVector( x ) ) {
      return "vector";
   } else {
      return typeof x;
   }
}

var __sc_errorHook = false;

/*** META ((export error-hook-set!) (arity #t)) */
function sc_errorHookSet( h ) {
   __sc_errorHook = h;
}

/*** META ((export error-hook) (arity #t)) */
function sc_errorHook() {
   return __sc_errorHook;
}

/*---------------------------------------------------------------------*/
/*    sc_error ...                                                     */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity -1)) */
function sc_error() {
   var e = new Error( "sc_error" );

   if( arguments.length >= 1 ) {
      e.name = arguments[ 0 ];
      if( arguments.length >= 2 ) {
	 e.message = arguments[ 1 ];
	 if( arguments.length >= 3 ) {
	    e.scObject = arguments[ 2 ];
	    if( arguments.length >= 4 ) {
	       e.scOffset = arguments[ 3 ];
	    } else {
	       e.scOffset = 1;
	    }
	 }
      }
   }

   throw __sc_errorHook ? __sc_errorHook( e, arguments ) : e;
}

/*---------------------------------------------------------------------*/
/*    sc_typeError ...                                                 */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity 3)) */
function sc_typeError( proc, type, obj ) {
   var msg = "Type \"" + type + "\" expected, "
      + "\"" + sc_typeof( obj ) + "\" provided";

   return sc_error( proc, msg, obj, arguments.length >= 4 ? arguments[ 3 ] : 2 );
}

/*---------------------------------------------------------------------*/
/*    sc_function_name ...                                             */
/*---------------------------------------------------------------------*/
function sc_function_name( fun ) {
   return ("displayName" in fun) ? fun.displayName : fun;
}

/*---------------------------------------------------------------------*/
/*    sc_arity_check ...                                               */
/*---------------------------------------------------------------------*/
function sc_arity_check( fun, nbargs ) {
   if( !("sc_arity" in fun) ) {
      return fun;
   } else {
      var arity = fun.sc_arity;
      var glop = fun;

      if( (arity == nbargs) || ((arity < 0) && (nbargs >= -1-arity)) ) {
	 // arity correct
	 return fun;
      } else {
	 // arity error
	 var msg = "Wrong number of arguments: " + arity + " expected, "
	    + nbargs + " provided";
	 var obj = sc_function_name( fun );

	 sc_error( "funcall", msg, obj, 2 );
      }
   }
}
   
/*** META ((export #t) (arity 1)) */
function sc_raise(obj) {
    throw obj;
}

/* with-trace and trace-item JS machinery */
var __sc_traceHasConsole =
   (( "console" in window )
    && ( "log" in window[ "console" ])
    && ( "groupCollapsed" in window[ "console" ])
    && ( "groupEnd" in window[ "console" ]));

var __sc_traceLevel = ( "hop_debug" in window ) ? window[ "hop_debug" ]() : 0;
var __sc_traceBlockStack = null

var sc_withTrace =
   __sc_traceHasConsole ?
   sc_withTraceConsole : function( level, name, thunk ) { return thunk(); };

function sc_withTraceConsole( level, name, thunk ) {
   // full console api described at
   // http://getfirebug.com/wiki/index.php/Console_API
   var tracep = __sc_traceLevel >= level;
   var stack = __sc_traceBlockStack;

   __sc_traceBlockStack = sc_cons( tracep, __sc_traceBlockStack );
   
   if( tracep ) console.group( name );
   
   try {
      return thunk();
   } finally {
      if( tracep ) console.groupEnd();
      __sc_traceBlockStack = stack;
   }
}

/*** META ((export #t) (arity -1)) */
function sc_traceItem() {
    if( __sc_traceBlockStack != null && __sc_traceBlockStack.__hop_car ) {
	if( arguments.length > 0 ) {
	    console.log.apply( console, arguments );
	}
    }
}

/*** META ((export with-handler-lambda) (arity #t)) */
function sc_withHandlerLambda(handler, body) {
    try {
	return body();
    } catch(e) {
	if (!e._internalException)
	    return handler(e);
	else
	    throw e;
    }
}

/*
 * Unserialization
 */
var sc_circle_cache = new Array;

function sc_circle( len, proc, flat ) {
   if( flat ) {
      return proc( undefined );
   } else {
      for( var i = 0; i < len; i++ ) {
	 sc_circle_cache[ i ] = false;
      }
      return sc_circle_force( sc_circle_cache, proc( sc_circle_cache ) );
   }
}

function sc_circle_delay( i ) {
   this.index = i;
}

function sc_circle_force( cache, obj ) {
   if( !(obj instanceof Object) ) {
      return obj;
   } else if( obj instanceof sc_circle_delay ) {
      return cache[ obj.index ];
   } if( sc_isPair( obj ) ) {
      obj.__hop_car = sc_circle_force( cache, obj.__hop_car );
      obj.__hop_cdr = sc_circle_force( cache, obj.__hop_cdr );
      return obj;
   } else if( sc_isVector( obj ) ) {
      for( var i = 0; i < obj.length; i++ ) {
	 obj[ i ] = sc_circle_force( cache, obj[ i ] );
      }
      return obj;
   } else if( obj instanceof sc_Object ) {
      if( !obj.hop_circle_forced ) {
	 var clazz = sc_object_class( obj );
	 var f = sc_class_all_fields( clazz );

	 obj.hop_circle_forced = true;
	 
	 for( i = 0; i < f.length; i++ ) {
	    var n = sc_symbol2jsstring( f[ i ].sc_name );
	    obj[ n ] = sc_circle_force( cache, obj[ n ] );
	 }
	 return obj;
      }
   } else {
      if( "hop_classname" in obj ) {
	 if( !obj.hop_circle_forced ) {
	    obj.hop_circle_forced = true;
	    for( f in obj ) {
	       obj[ f ] = sc_circle_force( cache, obj[ f ] );
	    }
	 }
      }
	 
      return obj;
   }
}
      
function sc_circle_ref( cache, i ) {
   if( cache[ i ] ) {
      return cache[ i ];
   } else {
      return new sc_circle_delay( i );
   }
}

function sc_circle_def( cache, i, v ) {
   cache[ i ] = v;
   return v;
}
   
var sc_properties = new Object();

/*** META ((export #t) (arity #t)) */
function sc_putpropBang(sym, key, val) {
    var ht = sc_properties[sym];
    if (!ht) {
	ht = new Object();
	sc_properties[sym] = ht;
    }
    ht[key] = val;
}

/*** META ((export #t) (arity #t)) */
function sc_getprop(sym, key) {
    var ht = sc_properties[sym];
    if (ht) {
	if (key in ht)
	    return ht[key];
	else
	    return false;
    } else
	return false;
}

/*** META ((export #t) (arity #t)) */
function sc_rempropBang(sym, key) {
    var ht = sc_properties[sym];
    if (ht)
	delete ht[key];
}

/*** META ((export #t) (arity #t)) */
function sc_any2String(o) {
    return sc_jsstring2string(sc_toDisplayString(o));
}    

/*** META ((export #t) (arity #t)
           (peephole (safe-infix 2 2 "==="))
           (type bool))
*/
function sc_isEqv(o1, o2) {
    return (o1 === o2);
}

/*** META ((export #t) (arity #t)
           (peephole (safe-infix 2 2 "==="))
           (type bool))
*/
function sc_isEq(o1, o2) {
    return (o1 === o2);
}

/*** META ((export #t) (arity #t)
           (type bool)
	   (peephole (safe-hole 1 "(typeof (" n ") === 'number')")))
*/
function sc_isNumber(n) {
    return (typeof n === "number");
}

/*** META ((export #t) (arity #t)
           (type bool))
*/
function sc_isComplex(n) {
    return sc_isNumber(n);
}

/*** META ((export #t) (arity #t)
           (type bool))
*/
function sc_isReal(n) {
    return sc_isNumber(n);
}

/*** META ((export #t) (arity #t)
           (type bool))
*/
function sc_isRational(n) {
    return sc_isReal(n);
}

/*** META ((export #t) (arity #t)
           (type bool))
*/
function sc_isInteger(n) {
    return (parseInt(n) === n);
}

/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (postfix ", false")))
*/
// we don't have exact numbers...
function sc_isExact(n) {
    return false;
}

/*** META ((export #t) (arity #t)
           (peephole (postfix ", true"))
	   (type bool))
*/
function sc_isInexact(n) {
    return true;
}

/*** META ((export = =fx =fl)
           (type bool)
           (peephole (safe-infix 2 2 "==="))
           (arity -3))
*/
function sc_equal(x) {
   // This is not the implementation of equal? which is defined in immutable.js
   // and mutable.js
   for (var i = 1; i < arguments.length; i++)
      if (x !== arguments[i])
	 return false;
   return true;
}

/*---------------------------------------------------------------------*/
/*    sc_less ...                                                      */
/*---------------------------------------------------------------------*/
/*** META ((export <)
           (type bool)
           (peephole (infix 2 2 "<") (safe-binary sc_less2))
           (arity -3))
*/
function sc_less(x) {
#if HOP_RTS_DEBUG
   if (typeof arguments[0] !== "number") {
      sc_typeError( "<", "number", arguments[0], 3 );
   }
#endif       
    for (var i = 1; i < arguments.length; i++) {
#if HOP_RTS_DEBUG
       if (typeof arguments[i] !== "number") {
	  sc_typeError( "<", "number", arguments[i], 3 );
       }
#endif       
	if (x >= arguments[i])
	    return false;
	x = arguments[i];
    }
    return true;
}

/*** META ((export <fx <fl)
           (type bool)
           (peephole (infix 2 2 "<") (safe-binary sc_less2))
           (arity 2))
*/
function sc_less2( x, y ) {
#if HOP_RTS_DEBUG
   if( typeof x !== "number" ) {
      sc_typeError( "<", "number", x, 3 );
   }
   if( typeof y !== "number" ) {
      sc_typeError( "<", "number", y, 3 );
   }
#endif

   return x < y;
}

/*---------------------------------------------------------------------*/
/*    sc_greater ...                                                   */
/*---------------------------------------------------------------------*/
/*** META ((export >)
           (type bool)
           (peephole (infix 2 2 ">") (safe-binary sc_greater2))
           (arity -3))
*/
function sc_greater(x, y) {
#if HOP_RTS_DEBUG
   if (typeof x !== "number") {
      sc_typeError( ">", "number", x, 3 );
   }
#endif       
    for (var i = 1; i < arguments.length; i++) {
#if HOP_RTS_DEBUG
       if (typeof arguments[i] !== "number") {
	  sc_typeError( ">", "number", arguments[i], 3 );
       }
#endif       
	if (x <= arguments[i])
	    return false;
	x = arguments[i];
    }
    return true;
}

/*** META ((export >fx >fl)
           (type bool)
           (peephole (infix 2 2 ">"))
           (arity 2))
*/
function sc_greater2( x, y ) {
#if HOP_RTS_DEBUG
   if( typeof x !== "number" ) {
      sc_typeError( ">", "number", x, 3 );
   }
   if( typeof y !== "number" ) {
      sc_typeError( ">", "number", y, 3 );
   }
#endif

   return x > y;
}

/*---------------------------------------------------------------------*/
/*    sc_lessEqual ...                                                 */
/*---------------------------------------------------------------------*/
/*** META ((export <=)
           (type bool)
           (peephole (infix 2 2 "<=") (safe-binary sc_lessEqual2))
           (arity -3))
*/
function sc_lessEqual(x, y) {
#if HOP_RTS_DEBUG
   if (typeof arguments[0] !== "number") {
      sc_typeError( "<=", "number", arguments[0], 3 );
   }
#endif       
    for (var i = 1; i < arguments.length; i++) {
#if HOP_RTS_DEBUG
       if (typeof arguments[i] !== "number") {
	  sc_typeError( "<=", "number", arguments[i], 3 );
       }
#endif       
	if (x > arguments[i])
	    return false;
	x = arguments[i];
    }
    return true;
}

/*** META ((export <=fx <=fl)
           (type bool)
           (peephole (infix 2 2 "<="))
           (arity 2))
*/
function sc_lessEqual2( x, y ) {
#if HOP_RTS_DEBUG
   if( typeof x !== "number" ) {
      sc_typeError( "<=", "number", x, 3 );
   }
   if( typeof y !== "number" ) {
      sc_typeError( "<=", "number", y, 3 );
   }
#endif

   return x <= y;
}

/*---------------------------------------------------------------------*/
/*    sc_greaterEqual ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export >=)
           (type bool)
           (peephole (infix 2 2 ">=") (safe-binary sc_greaterEqual2))
           (arity -3))
*/
function sc_greaterEqual(x, y) {
#if HOP_RTS_DEBUG
   if (typeof arguments[0] !== "number") {
      sc_typeError( ">=", "number", arguments[0], 3 );
   }
#endif       
    for (var i = 1; i < arguments.length; i++) {
#if HOP_RTS_DEBUG
       if (typeof arguments[i] !== "number") {
	  sc_typeError( ">=", "number", arguments[i], 3 );
       }
#endif       
	if (x < arguments[i])
	    return false;
	x = arguments[i];
    }
    return true;
}

/*** META ((export >=fx >=fl)
           (type bool)
           (peephole (infix 2 2 ">="))
           (arity 2))
*/
function sc_greaterEqual2( x, y ) {
#if HOP_RTS_DEBUG
   if( typeof x !== "number" ) {
      sc_typeError( ">=", "number", x, 3 );
   }
   if( typeof y !== "number" ) {
      sc_typeError( ">=", "number", y, 3 );
   }
#endif

   return x >= y;
}

/*---------------------------------------------------------------------*/
/*    sc_isZero ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export zero? zerofx? zerofl?) (arity #t)
           (type bool)
           (peephole (postfix "=== 0")))
*/
function sc_isZero(x) {
#if HOP_RTS_DEBUG
   if (typeof x !== "number") {
      sc_typeError( "zero?", "number", x, 3 );
   }
#endif       
    return (x === 0);
}

/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (postfix "> 0")))
*/
function sc_isPositive(x) {
#if HOP_RTS_DEBUG
   if (typeof x !== "number") {
      sc_typeError( "positive?", "number", x, 3 );
   }
#endif       
    return (x > 0);
}

/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (postfix "< 0")))
*/
function sc_isNegative(x) {
#if HOP_RTS_DEBUG
   if (typeof x !== "number") {
      sc_typeError( "negative?", "number", x, 3 );
   }
#endif       
    return (x < 0);
}

/*** META ((export odd? oddfx? evenfl?) (arity #t)
           (type bool)
           (peephole (postfix "%2===1")))
*/
function sc_isOdd(x) {
#if HOP_RTS_DEBUG
   if (typeof x !== "number") {
      sc_typeError( "odd?", "number", x, 3 );
   }
#endif       
    return (x % 2 === 1);
}

/*** META ((export even? evenfx? evenfl?) (arity #t)
           (type bool)
           (peephole (postfix "%2===0")))
*/
function sc_isEven(x) {
#if HOP_RTS_DEBUG
   if (typeof x !== "number") {
      sc_typeError( "even?", "number", x, 3 );
   }
#endif       
    return (x % 2 === 0);
}

/*** META ((export max maxfl maxfx)
           (arity -2)) */
var sc_max = Math.max;
/*** META ((export min minfl minfx)
           (arity -2)) */
var sc_min = Math.min;


/*---------------------------------------------------------------------*/
/*    HOP_RTS_DEBUG_NUMERIC_TYPE                                       */
/*    -------------------------------------------------------------    */
/*    If set, the type checking of the safe runtime system is          */
/*    implemented with explicit type checks. Otherwise, JavaScript     */
/*    isNaN is used.                                                   */
/*---------------------------------------------------------------------*/
#define HOP_RTS_DEBUG_NUMERIC_TYPE 1
//#define HOP_RTS_DEBUG_NUMERIC_ISNAN 1

/*---------------------------------------------------------------------*/
/*    sc_checkNumericTypes ...                                         */
/*---------------------------------------------------------------------*/
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
function sc_checkNumericTypes(fun, res, args) {
   for (var i = 0; i < args.length; i++) {
      if (typeof args[i] !== "number") {
	 sc_typeError( fun, "number", args[i], 4 );
      }
   }

   return res;
}
#endif
#endif

/*---------------------------------------------------------------------*/
/*    sc_plus ...                                                      */
/*---------------------------------------------------------------------*/
/*** META ((export +)
           (peephole (infix 0 #f "+" "0") (safe-binary sc_plus2))
           (arity -1))
*/
function sc_plus() {
    var res = 0;
    for (var i = 0; i < arguments.length; i++) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
       if (typeof arguments[i] !== "number") {
	  sc_typeError( "+", "number", arguments[i], 3 );
       }
#endif       
#endif       
       res += arguments[i];
    }

#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else      
   if( res !== res ) {
#endif      
      return sc_checkNumericTypes("+",res,arguments);
   }
#endif
#endif
   return res;
}

/*** META ((export +fx +fl)
           (peephole (infix 0 #f "+" "0"))
           (arity 2))
*/
function sc_plus2(x, y) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if( typeof x !== "number" ) {
      sc_typeError( "+", "number", x, 3 );
   }
   if( typeof y !== "number" ) {
      sc_typeError( "+", "number", y, 3 );
   }
#endif   
#endif
   var res = x + y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "+", res, x, y );
   }
#endif
#endif
   return res;
}

/*---------------------------------------------------------------------*/
/*    sc_minus ...                                                     */
/*---------------------------------------------------------------------*/
/*** META ((export - negfx negfl)
           (peephole (minus) (safe-binary sc_minus2))
           (arity -2))
*/
function sc_minus(x) {
   if (arguments.length === 1) {
#if HOP_RTS_DEBUG
   if (typeof x !== "number") {
      sc_typeError( "-", "number", x, 3 );
   }
#endif
      return -x;
   } else {
       var res = x;
      
       for (var i = 1; i < arguments.length; i++) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
	  if (typeof arguments[i] !== "number") {
	     sc_typeError( "-", "number", arguments[i], 3 );
	  }
#endif
#endif       
	  res -= arguments[i];
       }

#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
	 return sc_checkNumericTypes("-",res,arguments);
      }
#endif
#endif

      return res;
   }
}

/*** META ((export -fx -fl)
           (peephole (minus))
           (arity 2))
*/
function sc_minus2( x, y ) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if( typeof x !== "number" ) {
      sc_typeError( "-", "number", x, 3 );
   }
   if( typeof y !== "number" ) {
      sc_typeError( "-", "number", y, 3 );
   }
#endif   
#endif

   var res = x - y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN
   if( isNaN( res ) ) {
#else
      if( res !== res ) {
#endif
      return sc_checkNumericTypes( "-", res, x, y );
   }
#endif
#endif

   return res;
}

/*---------------------------------------------------------------------*/
/*    sc_multi ...                                                     */
/*---------------------------------------------------------------------*/
/*** META ((export *)
           (peephole (infix 0 #f "*" "1") (safe-binary sc_multi2))
           (arity -1))
*/
function sc_multi() {
    var res = 1;
    for (var i = 0; i < arguments.length; i++) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
       if (typeof arguments[i] !== "number") {
	  sc_typeError( "*", "number", arguments[i], 3 );
       }
#endif
#endif
	res *= arguments[i];
    }
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes("+",res,arguments);
   }
#endif
#endif
    return res;
}

/*** META ((export *fx *fl)
           (peephole (infix 0 #f "*" "1"))
           (arity 2))
*/
function sc_multi2( x, y ) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if( typeof x !== "number" ) {
      sc_typeError( "*", "number", x, 3 );
   }
   if( typeof y !== "number" ) {
      sc_typeError( "*", "number", y, 3 );
   }
#endif   
#endif
   var res = x * y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "*", res, x, y );
   }
#endif
#endif
   return res;
}

/*---------------------------------------------------------------------*/
/*    sc_div ...                                                       */
/*---------------------------------------------------------------------*/
/*** META ((export /)
           (peephole (div) (safe-binary sc_div2))
           (arity -2))
*/
function sc_div(x) {
    if (arguments.length === 1)
	return 1/x;
    else {
	var res = x;
	for (var i = 1; i < arguments.length; i++) {
#if HOP_RTS_DEBUG
	  if (typeof arguments[i] !== "number") {
	     sc_typeError( "-", "number", arguments[i], 3 );
	  }
#endif       
	   res /= arguments[i];
	}
	return res;
    }
}

/*** META ((export /fl)
           (peephole (div))
           (arity 2))
*/
function sc_div2(x, y) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if( typeof x !== "number" ) {
      sc_typeError( "/", "number", x, 3 );
   }
   if( typeof y !== "number" ) {
      sc_typeError( "/", "number", y, 3 );
   }
#endif   
#endif
   var res = x / y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "/", res, x, y );
   }
#endif
#endif
   return res;
}


/*---------------------------------------------------------------------*/
/*    abs ...                                                          */
/*---------------------------------------------------------------------*/
/*** META ((export abs absfx absfl)
           (arity 1))
*/
var sc_abs = Math.abs;

/*---------------------------------------------------------------------*/
/*    sc_quotient ...                                                  */
/*---------------------------------------------------------------------*/
/*** META ((export quotient /fx) (arity #t)
           (peephole (hole 2 "parseInt(" x "/" y ")")))
*/
function sc_quotient(x, y) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if (typeof x !== "number") {
      sc_typeError( "quotient", "number", x, 3 );
   }
   if (typeof y !== "number") {
      sc_typeError( "quotient", "number", y, 3 );
   }
#endif
#endif
   var res = (x / y);
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "/fx", res, x, y );
   }
#endif
#endif
   
   return parseInt( res );
}

/*---------------------------------------------------------------------*/
/*    sc_remainder ...                                                 */
/*---------------------------------------------------------------------*/
/*** META ((export remainder remainderfx remainderfl) (arity #t)
           (peephole (infix 2 2 "%")))
*/
function sc_remainder(x, y) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if (typeof x !== "number") {
      sc_typeError( "remainder", "number", x, 3 );
   }
   if (typeof y !== "number") {
      sc_typeError( "remainder", "number", y, 3 );
   }
#endif
#endif

   var res = x % y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "remainder", res, x, y );
   }
#endif
#endif

   return res;
}

/*---------------------------------------------------------------------*/
/*    sc_modulo ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export modulo modulofx) (arity #t))
*/
function sc_modulo(x, y) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if (typeof x !== "number") {
      sc_typeError( "modulo", "number", x, 3 );
   }
   if (typeof y !== "number") {
      sc_typeError( "modulo", "number", y, 3 );
   }
#endif
#endif
   
    var remainder = x % y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( remainder ) ) {
#else
   if( remainder !== remainder ) {
#endif
      return sc_checkNumericTypes( "modulo", remainder, x, y );
   }
#endif
#endif
    // if they don't have the same sign
    if ((remainder * y) < 0)
	return remainder + y;
    else
	return remainder;
}

/*---------------------------------------------------------------------*/
/*    sc_euclid_gcd ...                                                */
/*---------------------------------------------------------------------*/
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
}

/*---------------------------------------------------------------------*/
/*    sc_gcd ...                                                       */
/*---------------------------------------------------------------------*/
/*** META ((export #t)
           (arity -1))
*/
function sc_gcd() {
    var gcd = 0;
    for (var i = 0; i < arguments.length; i++) {
#if HOP_RTS_DEBUG
       if (typeof arguments[i] !== "number") {
	  sc_typeError( "gcd", "number", arguments[i], 3 );
       }
#endif       
       gcd = sc_euclid_gcd(gcd, arguments[i]);
    }
    return gcd;
}

/*---------------------------------------------------------------------*/
/*    sc_lcm ...                                                       */
/*---------------------------------------------------------------------*/
/*** META ((export #t)
           (arity -1))
*/
function sc_lcm() {
    var lcm = 1;
    for (var i = 0; i < arguments.length; i++) {
#if HOP_RTS_DEBUG
       if (typeof arguments[i] !== "number") {
	  sc_typeError( "lcm", "number", arguments[i], 3 );
       }
#endif       
       var f = Math.round(arguments[i] / sc_euclid_gcd(arguments[i], lcm));
       lcm *= Math.abs(f);
    }
    return lcm;
}

// LIMITATION: numerator and denominator don't make sense in floating point world.
//var SC_MAX_DECIMALS = 1000000
//
// function sc_numerator(x) {
//     var rounded = Math.round(x * SC_MAX_DECIMALS);
//     return Math.round(rounded / sc_euclid_gcd(rounded, SC_MAX_DECIMALS));
// }

// function sc_denominator(x) {
//     var rounded = Math.round(x * SC_MAX_DECIMALS);
//     return Math.round(SC_MAX_DECIMALS / sc_euclid_gcd(rounded, SC_MAX_DECIMALS));
// }

/*** META ((export #t)
           (arity 1))
*/
var sc_floor = Math.floor;
/*** META ((export #t)
           (arity 1))
*/
var sc_ceiling = Math.ceil;
/*** META ((export #t)
           (arity 1))
*/
var sc_truncate = parseInt;
/*** META ((export #t)
           (arity 1))
*/
var sc_round = Math.round;

// LIMITATION: sc_rationalize doesn't make sense in a floating point world.

/*** META ((export #t)
           (arity 1))
*/
var sc_exp = Math.exp;
/*** META ((export #t)
           (arity 1))
*/
var sc_log = Math.log;
/*** META ((export #t)
           (arity 1))
*/
var sc_sin = Math.sin;
/*** META ((export #t)
           (arity 1))
*/
var sc_cos = Math.cos;
/*** META ((export #t)
           (arity 1))
*/
var sc_tan = Math.tan;
/*** META ((export #t)
           (arity 1))
*/
var sc_asin = Math.asin;
/*** META ((export #t)
           (arity 1))
*/
var sc_acos = Math.acos;
/*** META ((export #t)
           (arity -2))
*/
var sc_atan = Math.atan;

/*** META ((export #t)
           (arity 1))
*/
var sc_sqrt = Math.sqrt;
/*** META ((export #t)
           (arity 2))
*/
var sc_expt = Math.pow;

// LIMITATION: we don't have complex numbers.
// LIMITATION: the following functions are hence not implemented.
// LIMITATION: make-rectangular, make-polar, real-part, imag-part, magnitude, angle
// LIMITATION: 2 argument atan

/*** META ((export exact->inexact fixnum->flonum) (arity #t)
           (peephole (id)))
*/
function sc_exact2inexact(x) {
#if HOP_RTS_DEBUG
       if (!typeof x === "number") {
	  sc_typeError( "exact->inexact", "number", x, 3 );
       }
#endif       
    return x;
}

/*** META ((export inexact->exact flonum->fixnum) (arity #t)
           (peephole (postfix "<< 0")))
*/
function sc_inexact2exact(x) {
#if HOP_RTS_DEBUG
       if (!typeof x === "number") {
	  sc_typeError( "inexact->exact", "number", x, 3 );
       }
#endif       
    return x << 0;
}

function sc_number2jsstring(x, radix) {
#if HOP_RTS_DEBUG
       if (!typeof x === "number") {
	  sc_typeError( "number->string", "number", x, 3 );
       }
#endif       
    if (radix) {
#if HOP_RTS_DEBUG
       if (!typeof radix === "number") {
	  sc_typeError( "number->string", "number", radix, 3 );
       }
#endif       
       return x.toString(radix);
    } else {
       return x.toString();
    }
}

function sc_jsstring2number(s, radix) {
    if (s === "") return false;

    if (radix) {
	var t = parseInt(s, radix);
	if (!t && t !== 0) return false;
	// verify that each char is in range. (parseInt ignores leading
	// white and trailing chars)
	var allowedChars = "01234567890abcdefghijklmnopqrstuvwxyz".substring(0, radix+1);
	if ((new RegExp("^["+allowedChars+"]*$", "i")).test(s))
	    return t;
	else return false;
    } else {
	var t = +s; // does not ignore trailing chars.
	if (!t && t !== 0) return false;
	// simply verify that first char is not whitespace.
	var c = s.charAt(0);
	// if +c is 0, but the char is not "0", then we have a whitespace.
	if (+c === 0 && c !== "0") return false;
	return t;
    }
}

/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (safe-not)))
*/
function sc_not(b) {
    return b === false;
}

/*** META ((export #t) (arity #t)
           (type bool))
*/
function sc_isBoolean(b) {
    return (b === true) || (b === false);
}

#if HOP_RTS_DEBUG
var dynamic_type_check = ((hop_debug() >= 1) && ("defineProperty" in Object));

function sc_Pair(car, cdr) {
   if( dynamic_type_check ) {
      this.__safe_hop_car = car;
      this.__safe_hop_cdr = cdr;
   } else {
      this.__hop_car = car;
      this.__hop_cdr = cdr;
   }
}

if( dynamic_type_check ) {
   Object.defineProperty( Object.prototype, "__hop_car", {
      enumerable: false,
      get: function() { sc_typeError( "car", "pair", this, 4 ); },
      set: function( v ) { sc_typeError( "set-car!", "pair", this, 4 ); }
   } );

   Object.defineProperty( Object.prototype, "__hop_cdr", {
      enumerable: false,
      get: function() { sc_typeError( "cdr", "pair", this, 4 ); },
      set: function( v ) { sc_typeError( "set-cdr!", "pair", this, 4 ); }
   } );

   Object.defineProperty( sc_Pair.prototype, "__hop_car", {
      enumerable: false,
      get: function() { return this.__safe_hop_car; },
      set: function( v ) { this.__safe_hop_car = v; }
   } );

   Object.defineProperty( sc_Pair.prototype, "__hop_cdr", {
      enumerable: false,
      get: function() { return this.__safe_hop_cdr; },
      set: function( v ) { this.__safe_hop_cdr = v; }
   } );
   
   Object.defineProperty( sc_Pair.prototype, "car", {
      enumerable: true,
      get: function() { return this.__safe_hop_car; },
      set: function( v ) { this.__safe_hop_car = v; }
   } );

   Object.defineProperty( sc_Pair.prototype, "cdr", {
      enumerable: true,
      get: function() { return this.__safe_hop_cdr; },
      set: function( v ) { this.__safe_hop_cdr = v; }
   } );
}
#else
function sc_Pair(car, cdr) {
   this.__hop_car = car;
   this.__hop_cdr = cdr;
}

   // MS TO BE FIXED, if pair are to be bound in JS, __hop_car/car
   Object.defineProperty( sc_Pair.prototype, "car", {
      enumerable: true,
      get: function() { return this.__hop_car; },
      set: function( v ) { this.__hop_car = v; }
   } );

   Object.defineProperty( sc_Pair.prototype, "cdr", {
      enumerable: true,
      get: function() { return this.__hop_cdr; },
      set: function( v ) { this.__hop_cdr = v; }
   } );
   
#endif

sc_Pair.prototype.toString = function() {
    return sc_toDisplayString(this);
};
sc_Pair.prototype.sc_toWriteOrDisplayString = function(writeOrDisplay) {
    var current = this;

    var res = "(";

    while(true) {
	res += writeOrDisplay(current.__hop_car);
	if (sc_isPair(current.__hop_cdr)) {
	    res += " ";
	    current = current.__hop_cdr;
	} else if (current.__hop_cdr !== null) {
	    res += " . " + writeOrDisplay(current.__hop_cdr);
	    break;
	} else // current.__hop_cdr == null
	    break;
    }
	
    res += ")";

    return res;
};
sc_Pair.prototype.sc_toDisplayString = function() {
    return this.sc_toWriteOrDisplayString(sc_toDisplayString);
};
sc_Pair.prototype.sc_toWriteString = function() {
    return this.sc_toWriteOrDisplayString(sc_toWriteString);
};
// sc_Pair.prototype.sc_toWriteCircleString in IO.js

sc_Pair.prototype.length = function(){
   return sc_length(this);
}
sc_Pair.prototype.reverse = function() {
   return sc_reverse(this);
}
sc_Pair.prototype.forEach = function(p) {
   return sc_forEach(p,this);
}
sc_Pair.prototype.assoc = function(o) {
   return sc_assoc(o,this);
}
sc_Pair.prototype.concat = function() {
   return sc_dualAppend(this,sc_append.apply(this, arguments));
}
/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (safe-postfix " instanceof sc_Pair")))
*/
function sc_isPair(p) {
    return (p instanceof sc_Pair);
}

/*** META ((export #t) (arity #t)
           (type bool)) */
function sc_isEpair(p) {
    return (p instanceof sc_Pair) && ("cer" in p);
}

function sc_isPairEqual(p1, p2, comp) {
    return (comp(p1.__hop_car, p2.__hop_car) && comp(p1.__hop_cdr, p2.__hop_cdr));
}

/*** META ((export #t) (arity #t)
           (peephole (hole 2 "new sc_Pair(" _car ", " cdr ")")))
*/
function sc_cons(car, cdr) {
    return new sc_Pair(car, cdr);
}

/*** META ((export #t) (arity #t)) */
function sc_econs(car, cdr, cer) {
   var p = new sc_Pair(car, cdr);
   p.cer = cer;
   return p;
}

/*** META ((export cons*)
           (arity -2))
*/
function sc_consStar() {
    var res = arguments[arguments.length - 1];
    for (var i = arguments.length-2; i >= 0; i--)
	res = new sc_Pair(arguments[i], res);
    return res;
}

/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car")))
*/
function sc_car(p) {
   return p.__hop_car;
}

/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr")))
*/
function sc_cdr(p) {
   return p.__hop_cdr;
}

/*** META ((export #t) (arity #t)
           (peephole (postfix ".cer")))
*/
function sc_cer(p) {
   return p.cer;
}

/*** META ((export #t) (arity #t)
           (peephole (safe-hole 2 p ".__hop_car = " val)))
*/
function sc_setCarBang(p, val) {
   p.__hop_car = val;
}

/*** META ((export #t) (arity #t)
           (peephole (safe-hole 2 p ".__hop_cdr = " val)))
*/
function sc_setCdrBang(p, val) {
   p.__hop_cdr = val;
}

/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_car")))
*/
function sc_caar(p) { return p.__hop_car.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_car")))
*/
function sc_cadr(p) { return p.__hop_cdr.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_cdr")))
*/
function sc_cdar(p) { return p.__hop_car.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_cdr")))
*/
function sc_cddr(p) { return p.__hop_cdr.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_car.__hop_car")))
*/
function sc_caaar(p) { return p.__hop_car.__hop_car.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_cdr.__hop_car")))
*/
function sc_cadar(p) { return p.__hop_car.__hop_cdr.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_car.__hop_car")))
*/
function sc_caadr(p) { return p.__hop_cdr.__hop_car.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_cdr.__hop_car")))
*/
function sc_caddr(p) { return p.__hop_cdr.__hop_cdr.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_car.__hop_cdr")))
*/
function sc_cdaar(p) { return p.__hop_car.__hop_car.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_car.__hop_cdr")))
*/
function sc_cdadr(p) { return p.__hop_cdr.__hop_car.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_cdr.__hop_cdr")))
*/
function sc_cddar(p) { return p.__hop_car.__hop_cdr.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_cdr.__hop_cdr")))
*/
function sc_cdddr(p) { return p.__hop_cdr.__hop_cdr.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_car.__hop_car.__hop_car")))
*/
function sc_caaaar(p) { return p.__hop_car.__hop_car.__hop_car.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_cdr.__hop_car.__hop_car")))
*/
function sc_caadar(p) { return p.__hop_car.__hop_cdr.__hop_car.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_car.__hop_car.__hop_car")))
*/
function sc_caaadr(p) { return p.__hop_cdr.__hop_car.__hop_car.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_cdr.__hop_car.__hop_car")))
*/
function sc_caaddr(p) { return p.__hop_cdr.__hop_cdr.__hop_car.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_car.__hop_car.__hop_cdr")))
*/
function sc_cdaaar(p) { return p.__hop_car.__hop_car.__hop_car.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_cdr.__hop_car.__hop_cdr")))
*/
function sc_cdadar(p) { return p.__hop_car.__hop_cdr.__hop_car.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_car.__hop_car.__hop_cdr")))
*/
function sc_cdaadr(p) { return p.__hop_cdr.__hop_car.__hop_car.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_cdr.__hop_car.__hop_cdr")))
*/
function sc_cdaddr(p) { return p.__hop_cdr.__hop_cdr.__hop_car.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_car.__hop_cdr.__hop_car")))
*/
function sc_cadaar(p) { return p.__hop_car.__hop_car.__hop_cdr.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_cdr.__hop_cdr.__hop_car")))
*/
function sc_caddar(p) { return p.__hop_car.__hop_cdr.__hop_cdr.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_car.__hop_cdr.__hop_car")))
*/
function sc_cadadr(p) { return p.__hop_cdr.__hop_car.__hop_cdr.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_cdr.__hop_cdr.__hop_car")))
*/
function sc_cadddr(p) { return p.__hop_cdr.__hop_cdr.__hop_cdr.__hop_car; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_car.__hop_cdr.__hop_cdr")))
*/
function sc_cddaar(p) { return p.__hop_car.__hop_car.__hop_cdr.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_car.__hop_cdr.__hop_cdr.__hop_cdr")))
*/
function sc_cdddar(p) { return p.__hop_car.__hop_cdr.__hop_cdr.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_car.__hop_cdr.__hop_cdr")))
*/
function sc_cddadr(p) { return p.__hop_cdr.__hop_car.__hop_cdr.__hop_cdr; }
/*** META ((export #t) (arity #t)
           (peephole (safe-postfix ".__hop_cdr.__hop_cdr.__hop_cdr.__hop_cdr")))
*/
function sc_cddddr(p) { return p.__hop_cdr.__hop_cdr.__hop_cdr.__hop_cdr; }

/*** META ((export #t) (arity #t)) */
function sc_lastPair(l) {
    if (!sc_isPair(l)) sc_error("sc_lastPair: pair expected");
    var res = l;
    var cdr = l.__hop_cdr;
    while (sc_isPair(cdr)) {
	res = cdr;
	cdr = res.__hop_cdr;
    }
    return res;
}

/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (safe-postfix " === null")))
*/
function sc_isNull(o) {
    return (o === null);
}

/*** META ((export #t) (arity #t)
           (type bool))
*/
function sc_isList(o) {
   var rabbit = o;
   var turtle = o;

   while (true) {
       if (rabbit === null ||
	   (rabbit instanceof sc_Pair && rabbit.__hop_cdr === null))
	   return true;  // end of list
       else {
	   if ((rabbit instanceof sc_Pair) &&
	       (rabbit.__hop_cdr instanceof sc_Pair)) {
	       rabbit = rabbit.__hop_cdr.__hop_cdr;
	       turtle = turtle.__hop_cdr;
	       if (rabbit === turtle) return false; // cycle
	   } else
	       return false; // not pair
       }
   }
}

/*** META ((export #t)
           (arity -1))
 */
function sc_list() {
    var res = null;
    var a = arguments;
    for (var i = a.length-1; i >= 0; i--)
	res = new sc_Pair(a[i], res);
    return res;
}

/*** META ((export #t)
           (arity -2))
*/
function sc_iota(num, init, step) {
   var res = null;
   if (!init) init = 0;
   if (!step) step = 1;
   var v = step * (num - 1) + init;
   for (var i = num - 1; i >= 0; i--, v-=step)
      res = new sc_Pair(v, res);
   return res;
}

/*** META ((export #t)
           (arity -2))
*/
function sc_makeList(nbEls, fill) {
    var res = null;
    for (var i = 0; i < nbEls; i++)
	res = new sc_Pair(fill, res);
    return res;
}

/*** META ((export #t) (arity #t)) */
function sc_length(l) {
    var res = 0;
    while (l !== null) {
	res++;
	l = l.__hop_cdr;
    }
    return res;
}

/*** META ((export #t) (arity #t)) */
function sc_remq(o, l) {
    var dummy = { __hop_cdr : null };
    var tail = dummy;
    while (l !== null) {
	if (l.__hop_car !== o) {
	    tail.__hop_cdr = sc_cons(l.__hop_car, null);
	    tail = tail.__hop_cdr;
	}
	l = l.__hop_cdr;
    }
    return dummy.__hop_cdr;
}

/*** META ((export #t) (arity #t)) */
function sc_remqBang(o, l) {
    var dummy = { __hop_cdr : null };
    var tail = dummy;
    var needsAssig = true;
    while (l !== null) {
	if (l.__hop_car === o) {
	    needsAssig = true;
	} else {
	    if (needsAssig) {
		tail.__hop_cdr = l;
		needsAssig = false;
	    }
	    tail = l;
	}
	l = l.__hop_cdr;
    }
    tail.__hop_cdr = null;
    return dummy.__hop_cdr;
}

/*** META ((export #t) (arity #t)) */
function sc_delete(o, l) {
    var dummy = { __hop_cdr : null };
    var tail = dummy;
    while (l !== null) {
	if (!sc_isEqual(l.__hop_car, o)) {
	    tail.__hop_cdr = sc_cons(l.__hop_car, null);
	    tail = tail.__hop_cdr;
	}
	l = l.__hop_cdr;
    }
    return dummy.__hop_cdr;
}

/*** META ((export #t) (arity #t)) */
function sc_deleteBang(o, l) {
    var dummy = { __hop_cdr : null };
    var tail = dummy;
    var needsAssig = true;
    while (l !== null) {
	if (sc_isEqual(l.__hop_car, o)) {
	    needsAssig = true;
	} else {
	    if (needsAssig) {
		tail.__hop_cdr = l;
		needsAssig = false;
	    }
	    tail = l;
	}
	l = l.__hop_cdr;
    }
    tail.__hop_cdr = null;
    return dummy.__hop_cdr;
}

function sc_reverseAppendBang(l1, l2) {
    var res = l2;
    while (l1 !== null) {
	var tmp = res;
	res = l1;
	l1 = l1.__hop_cdr;
	res.__hop_cdr = tmp;
    }
    return res;
}
	
function sc_dualAppend(l1, l2) {
    if (l1 === null) return l2;
    if (l2 === null) return l1;
    var rev = sc_reverse(l1);
    return sc_reverseAppendBang(rev, l2);
}

/*** META ((export append eappend) ;; we want eappend for the quasiquotes.
           (arity -1))
*/
function sc_append() {
    if (arguments.length === 0)
	return null;
    var res = arguments[arguments.length - 1];
    for (var i = arguments.length - 2; i >= 0; i--)
	res = sc_dualAppend(arguments[i], res);
    return res;
}

function sc_dualAppendBang(l1, l2) {
    if (l1 === null) return l2;
    if (l2 === null) return l1;
    var tmp = l1;
    while (tmp.__hop_cdr !== null) tmp=tmp.__hop_cdr;
    tmp.__hop_cdr = l2;
    return l1;
}
    
/*** META ((export #t)
           (arity -1))
*/
function sc_appendBang() {
    var res = null;
    for (var i = 0; i < arguments.length; i++)
	res = sc_dualAppendBang(res, arguments[i]);
    return res;
}

/*** META ((export #t) (arity #t)) */
function sc_reverse(l1) {
    var res = null;
    while (l1 !== null) {
	res = sc_cons(l1.__hop_car, res);
	l1 = l1.__hop_cdr;
    }
    return res;
}

/*** META ((export #t) (arity #t)) */
function sc_reverseBang(l) {
    return sc_reverseAppendBang(l, null);
}

/*** META ((export #t) (arity #t)) */
function sc_take(l, k) {
   var res = null;
   while (k-- > 0) {
      res = sc_cons(l.__hop_car, res);
   } 

   return sc_reverse(res);
}
   
/*** META ((export #t) (arity #t)) */
function sc_listTail(l, k) {
    var res = l;
    for (var i = 0; i < k; i++) {
	res = res.__hop_cdr;
    }
    return res;
}

/*** META ((export #t) (arity #t)) */
function sc_listRef(l, k) {
    return sc_listTail(l, k).__hop_car;
}

/* // unoptimized generic versions
function sc_memX(o, l, comp) {
    while (l != null) {
	if (comp(l.__hop_car, o))
	    return l;
	l = l.__hop_cdr;
    }
    return false;
}
function sc_memq(o, l) { return sc_memX(o, l, sc_isEq); }
function sc_memv(o, l) { return sc_memX(o, l, sc_isEqv); }
function sc_member(o, l) { return sc_memX(o, l, sc_isEqual); }
*/

/* optimized versions */
/*** META ((export #t) (arity #t)) */
function sc_memq(o, l) {
    while (l !== null) {
	if (l.__hop_car === o)
	    return l;
	l = l.__hop_cdr;
    }
    return false;
}
/*** META ((export #t) (arity #t)) */
function sc_memv(o, l) {
    while (l !== null) {
	if (l.__hop_car === o)
	    return l;
	l = l.__hop_cdr;
    }
    return false;
}
/*** META ((export #t) (arity #t)) */
function sc_member(o, l) {
    while (l !== null) {
	if (sc_isEqual(l.__hop_car,o))
	    return l;
	l = l.__hop_cdr;
    }
    return false;
}

/* // generic unoptimized versions
function sc_assX(o, al, comp) {
    while (al != null) {
	if (comp(al.__hop_car.__hop_car, o))
	    return al.__hop_car;
	al = al.__hop_cdr;
    }
    return false;
}
function sc_assq(o, al) { return sc_assX(o, al, sc_isEq); }
function sc_assv(o, al) { return sc_assX(o, al, sc_isEqv); }
function sc_assoc(o, al) { return sc_assX(o, al, sc_isEqual); }
*/
// optimized versions
/*** META ((export #t) (arity #t)) */
function sc_assq(o, al) {
    while (al !== null) {
	if (al.__hop_car.__hop_car === o)
	    return al.__hop_car;
	al = al.__hop_cdr;
    }
    return false;
}
/*** META ((export #t) (arity #t)) */
function sc_assv(o, al) {
    while (al !== null) {
	if (al.__hop_car.__hop_car === o)
	    return al.__hop_car;
	al = al.__hop_cdr;
    }
    return false;
}
/*** META ((export #t) (arity #t)) */
function sc_assoc(o, al) {
    while (al !== null) {
	if (sc_isEqual(al.__hop_car.__hop_car, o))
	    return al.__hop_car;
	al = al.__hop_cdr;
    }
    return false;
}

/*** META ((export #t) (arity #t)) */
function sc_reduce(f, ridentify, l) {
    if (l === null) {
        return ridentify;
    }
    var res = l.__hop_car;
    l = l.__hop_cdr;
    while (l !== null) {
        res = f(l.__hop_car, res);
        l = l.__hop_cdr;
    }
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
    // add return, so FF does not complain.
    return undefined;
}
sc_Char.lazy = new Object();
// thanks to Eric
sc_Char.char2readable = {
    "\x00": "#\\null",
    "\x07": "#\\bell",
    "\x08": "#\\backspace",
    "\x09": "#\\tab",
    "\x0a": "#\\newline",
    "\x0c": "#\\page",
    "\x0d": "#\\return",
    "\x1b": "#\\escape",
    "\x20": "#\\space",
    "\x7f": "#\\delete",

  /* poeticless names */
    "\x01": "#\\soh",
    "\x02": "#\\stx",
    "\x03": "#\\etx",
    "\x04": "#\\eot",
    "\x05": "#\\enq",
    "\x06": "#\\ack",

    "\x0b": "#\\vt",
    "\x0e": "#\\so",
    "\x0f": "#\\si",

    "\x10": "#\\dle",
    "\x11": "#\\dc1",
    "\x12": "#\\dc2",
    "\x13": "#\\dc3",
    "\x14": "#\\dc4",
    "\x15": "#\\nak",
    "\x16": "#\\syn",
    "\x17": "#\\etb",

    "\x18": "#\\can",
    "\x19": "#\\em",
    "\x1a": "#\\sub",
    "\x1c": "#\\fs",
    "\x1d": "#\\gs",
    "\x1e": "#\\rs",
    "\x1f": "#\\us"};

sc_Char.readable2char = {
    "null": "\x00",
    "bell": "\x07",
    "backspace": "\x08",
    "tab": "\x08",
    "newline": "\x0a",
    "page": "\x0c",
    "return": "\x0d",
    "escape": "\x1b",
    "space": "\x20",
    "delete": "\x00",
    "soh": "\x01",
    "stx": "\x02",
    "etx": "\x03",
    "eot": "\x04",
    "enq": "\x05",
    "ack": "\x06",
    "bel": "\x07",
    "bs": "\x08",
    "ht": "\x09",
    "nl": "\x0a",
    "vt": "\x0b",
    "np": "\x0c",
    "cr": "\x0d",
    "so": "\x0e",
    "si": "\x0f",
    "dle": "\x10",
    "dc1": "\x11",
    "dc2": "\x12",
    "dc3": "\x13",
    "dc4": "\x14",
    "nak": "\x15",
    "syn": "\x16",
    "etb": "\x17",
    "can": "\x18",
    "em": "\x19",
    "sub": "\x1a",
    "esc": "\x1b",
    "fs": "\x1c",
    "gs": "\x1d",
    "rs": "\x1e",
    "us": "\x1f",
    "sp": "\x20",
    "del": "\x7f"};
    
sc_Char.prototype.toString = function() {
    return this.val;
};
// sc_toDisplayString == toString
sc_Char.prototype.sc_toWriteString = function() {
    var entry = sc_Char.char2readable[this.val];
    if (entry)
	return entry;
    else
	return "#\\" + this.val;
};

/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (safe-postfix "instanceof sc_Char")))
*/
function sc_isChar(c) {
    return (c instanceof sc_Char);
}

/*** META ((export char=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 c1 ".val === " c2 ".val")))
*/
var sc_isCharEqual = sc_isCharStringEqual;
/*** META ((export char<?)
           (arity 2)
           (type bool)
           (peephole (hole 2 c1 ".val < " c2 ".val")))
*/
var sc_isCharLess = sc_isCharStringLess;
/*** META ((export char>?)
           (arity 2)
           (type bool)
           (peephole (hole 2 c1 ".val > " c2 ".val")))
*/
var sc_isCharGreater = sc_isCharStringGreater;
/*** META ((export char<=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 c1 ".val <= " c2 ".val")))
*/
var sc_isCharLessEqual = sc_isCharStringLessEqual;
/*** META ((export char>=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 c1 ".val >= " c2 ".val")))
*/
var sc_isCharGreaterEqual = sc_isCharStringGreaterEqual;
/*** META ((export char-ci=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 c1 ".val.toLowerCase() === " c2 ".val.toLowerCase()")))
*/
var sc_isCharCIEqual = sc_isCharStringCIEqual;
/*** META ((export char-ci<?)
           (arity 2)
           (type bool)
           (peephole (hole 2 c1 ".val.toLowerCase() < " c2 ".val.toLowerCase()")))
*/
var sc_isCharCILess = sc_isCharStringCILess;
/*** META ((export char-ci>?)
           (arity 2)
           (type bool)
           (peephole (hole 2 c1 ".val.toLowerCase() > " c2 ".val.toLowerCase()")))
*/
var sc_isCharCIGreater = sc_isCharStringCIGreater;
/*** META ((export char-ci<=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 c1 ".val.toLowerCase() <= " c2 ".val.toLowerCase()")))
*/
var sc_isCharCILessEqual = sc_isCharStringCILessEqual;
/*** META ((export char-ci>=?)
           (arity 2)
           (type bool)
           (peephole (hole 2 c1 ".val.toLowerCase() >= " c2 ".val.toLowerCase()")))
*/
var sc_isCharCIGreaterEqual = sc_isCharStringCIGreaterEqual;

var SC_NUMBER_CLASS = "0123456789";
var SC_WHITESPACE_CLASS = ' \r\n\t\f';
var SC_LOWER_CLASS = 'abcdefghijklmnopqrstuvwxyz';
var SC_UPPER_CLASS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

function sc_isCharOfClass(c, cl) { return (cl.indexOf(c) != -1); }
/*** META ((export #t) (arity #t)
           (type bool))
*/
function sc_isCharAlphabetic(c)
    { return sc_isCharOfClass(c.val, SC_LOWER_CLASS) ||
	  sc_isCharOfClass(c.val, SC_UPPER_CLASS); }
/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (hole 1 "SC_NUMBER_CLASS.indexOf(" c ".val) != -1")))
*/
function sc_isCharNumeric(c)
    { return sc_isCharOfClass(c.val, SC_NUMBER_CLASS); }
/*** META ((export #t) (arity #t)
           (type bool))
*/
function sc_isCharWhitespace(c) {
    var tmp = c.val;
    return tmp === " " || tmp === "\r" || tmp === "\n" || tmp === "\t" || tmp === "\f";
}
/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (hole 1 "SC_UPPER_CLASS.indexOf(" c ".val) != -1")))
*/
function sc_isCharUpperCase(c)
    { return sc_isCharOfClass(c.val, SC_UPPER_CLASS); }
/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (hole 1 "SC_LOWER_CLASS.indexOf(" c ".val) != -1")))
*/
function sc_isCharLowerCase(c)
    { return sc_isCharOfClass(c.val, SC_LOWER_CLASS); }

/*** META ((export #t) (arity #t)
           (peephole (postfix ".val.charCodeAt(0)")))
*/
function sc_char2integer(c)
    { return c.val.charCodeAt(0); }
/*** META ((export #t) (arity #t)
           (peephole (hole 1 "new sc_Char(String.fromCharCode(" n "))")))
*/
function sc_integer2char(n)
    { return new sc_Char(String.fromCharCode(n)); }

/*** META ((export #t) (arity #t)
           (peephole (hole 1 "new sc_Char(" c ".val.toUpperCase())")))
*/
function sc_charUpcase(c)
    { return new sc_Char(c.val.toUpperCase()); }
/*** META ((export #t) (arity #t)
           (peephole (hole 1 "new sc_Char(" c ".val.toLowerCase())")))
*/
function sc_charDowncase(c)
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

function sc_makejsString(k, c) {
    var fill;
    if (c)
	fill = c.val;
    else
	fill = " ";
    return sc_makeJSStringOfLength(k, fill);
}

function sc_jsstring2list(s) {
    var res = null;
    for (var i = s.length - 1; i >= 0; i--)
	res = sc_cons(new sc_Char(s.charAt(i)), res);
    return res;
}

function sc_list2jsstring(l) {
    var a = new Array();
    while(l !== null) {
	a.push(l.__hop_car.val);
	l = l.__hop_cdr;
    }
    return "".concat.apply("", a);
}

var sc_Vector = Array;

/* Dont' removed, needed for the JS unmarshalling, (see runtime/json.scm) */
function sc_vector2array(v) {
   return v;
}
   
function sc_VectorToWriteOrDisplayString(writeOrDisplay) {
   if (this.length === 0) return "#()";

   var res = "#(" + writeOrDisplay(this[0]);
   for (var i = 1; i < this.length; i++)
      res += " " + writeOrDisplay(this[i]);
   res += ")";
   return res;
}
   
function sc_VectorToDisplayString() {
   return this.sc_toWriteOrDisplayString(sc_toDisplayString);
}

function sc_VectorToWriteString() {
    return this.sc_toWriteOrDisplayString(sc_toWriteString);
}

if( "defineProperty" in Object ) {
   Object.defineProperty( sc_Vector, "sc_toWriteOrDisplayString", {
      value: sc_VectorToWriteOrDisplayString,
      enumerable: false
   } );
   Object.defineProperty( sc_Vector, "sc_toDisplayString", {
      value: sc_VectorToDisplayString,
      enumerable: false
   } );
   Object.defineProperty( sc_Vector, "sc_toWriteString", {
      value: sc_VectorToWriteString,
      enumerable: false
   } );
} else {
   sc_Vector.prototype.sc_toWriteOrDisplayString = sc_VectorToWriteOrDisplayString;
   sc_Vector.prototype.sc_toDisplayString = sc_VectorToDisplayString;
   sc_Vector.prototype.sc_toWriteString = sc_VectorToWriteString;
}


/*** META ((export vector? array?) (arity #t)
           (type bool))
*/
function sc_isVector(v) {
   if (v instanceof sc_Vector) {
      if ("Float32Array" in window) {
	 return !(v instanceof Float32Array);
      } else {
	 return true;
      }
   } else {
      return false;
   }
}

/*** META ((export vector array)
           (arity -1)
           (peephole (vector)))
*/
function sc_vector() {
    var a = new sc_Vector();
    for (var i = 0; i < arguments.length; i++)
	a.push(arguments[i]);
    return a;
}

/*** META ((export vector-length array-length) (arity #t)
           (peephole (postfix ".length")))
*/
function sc_vectorLength(v) {
#if HOP_RTS_DEBUG
   if (!(v instanceof sc_Vector) ) {
      sc_typeError( "vector-length", "vector", v, 3 );
   }
#endif   
   return v.length;
}

/*** META ((export vector-ref array-ref) (arity #t)
           (peephole (hole 2 v "[" pos "]")))
*/
function sc_vectorRef(v, pos) {
#if HOP_RTS_DEBUG
   if (!(v instanceof sc_Vector)) {
      sc_typeError( "vector-ref", "vector", v, 3 );
   }
   if (typeof pos !== "number") {
      sc_typeError( "vector-ref", "number", pos, 3 );
   }
   if( pos >= v.length || pos < 0 ) {
      sc_error( "vector-ref", "index out of bounds [0.." + v.length + "]", pos );
   }
#endif   
    return v[pos];
}


/*** META ((export vector-set! array-set!) (arity #t)
           (peephole (hole 3 v "[" pos "] = " val)))
*/
function sc_vectorSetBang(v, pos, val) {
#if HOP_RTS_DEBUG
   if (!(v instanceof sc_Vector)) {
      sc_typeError( "vector-set!", "vector", v, 3 );
   }
   if (typeof pos !== "number") {
      sc_typeError( "vector-set!", "number", pos, 3 );
   }
   if( pos >= v.length || pos < 0 ) {
      sc_error( "vector-set!", "index out of bounds [0.." + v.length + "]", pos );
   }
#endif   
    v[pos] = val;
}

// only applies to vectors
function sc_isVectorEqual(v1, v2, comp) {
#if HOP_RTS_DEBUG
   if (!(v1 instanceof sc_Vector) ) {
      sc_typeError( "vector-equal", "vector", v1, 3 );
   }
   if (!(v2 instanceof sc_Vector) ) {
      sc_typeError( "vector-equal", "vector", v2, 3 );
   }
#endif   
    if (v1.length !== v2.length) return false;
    for (var i = 0; i < v1.length; i++)
	if (!comp(v1[i], v2[i])) return false;
    return true;
}

/*** META ((export f32vector?) (arity #t)
           (type bool))
*/
function sc_isF32Vector(v) {
   if ("Float32Array" in window) {
      return (v instanceof Float32Array);
   } else {
      return (v instanceof sc_Vector);
   }
}

/*** META ((export make-vector make-array)
           (arity -2))
*/
function sc_makeVector(sz, fill) {
#if HOP_RTS_DEBUG
   if (typeof sz !== "number") {
      sc_typeError( "make-vector", "number", sz, 3 );
   }
#endif   
    var a = new sc_Vector(sz);
    if (fill !== undefined)
	sc_vectorFillBang(a, fill);
    return a;
}

/*** META ((export make-f32vector)
           (arity -2))
*/
function sc_makeF32Vector(sz, fill) {
#if HOP_RTS_DEBUG
   if (typeof sz !== "number") {
      sc_typeError( "make-f32vector", "number", sz, 3 );
   }
   if (typeof fill !== "number") {
      sc_typeError( "make-f32vector", "number", fill, 3 );
   }
#endif   
   var a = ("Float32Array" in window) ? new Float32Array(sz):new sc_Vector(sz);
      
   if (fill !== undefined)
      sc_vectorFillBang(a, fill);
   return a;
}

/*** META ((export f32vector)
           (arity -1))
*/
function sc_F32vector() {
   var sz = arguments.length;
   var a = ("Float32Array" in window) ? new Float32Array(sz):new sc_Vector(sz);
   
   for (var i = 0; i < arguments.length; i++) {
#if HOP_RTS_DEBUG
      if (typeof arguments[i] !== "number" ) {
	 sc_typeError( "f32vector", "number", arguments[i], 3 );
      }
#endif   
      a[i] = arguments[i];
   }
   
   return a;
}

/*** META ((export u8vector-length) (arity #t)
           (peephole (postfix ".length")))
*/
function sc_u8vectorLength(v) {
#if HOP_RTS_DEBUG
   if (!(v instanceof Uint8Array) && !(v instanceof Uint8ClampedArray)) {
      sc_typeError( "u8vector-length", "u8vector", v, 3 );
   }
#endif   
   return v.length;
}

/*** META ((export u8vector-set!) (arity #t)
           (peephole (hole 3 v "[" pos "] = " val)))
*/
function sc_u8vectorSetBang(v, pos, val) {
#if HOP_RTS_DEBUG
   if (!(v instanceof Uint8Array) && !(v instanceof Uint8ClampedArray)) {
      sc_typeError( "u8vector-set!", "u8vector", v, 3 );
   }
   if (typeof pos !== "number") {
      sc_typeError( "u8vector-set!", "number", pos, 3 );
   }
   if (typeof val !== "number") {
      sc_typeError( "u8vector-set!", "number", pos, 3 );
   }
   if( pos >= v.length || pos < 0 ) {
      sc_error( "u8vector-set!", "index out of bounds [0.." + v.length + "]", pos );
   }
#endif   
    v[pos] = val;
}

/*** META ((export u8vector-ref) (arity #t)
           (peephole (hole 2 v "[" pos "]")))
*/
function sc_u8vectorRef(v, pos) {
#if HOP_RTS_DEBUG
   if (!(v instanceof Uint8Array) && !(v instanceof Uint8ClampedArray)) {
      sc_typeError( "u8vector-ref", "u8vector", v, 3 );
   }
   if (typeof pos !== "number") {
      sc_typeError( "u8vector-ref", "number", pos, 3 );
   }
   if( pos >= v.length || pos < 0 ) {
      sc_error( "u8vector-ref", "index out of bounds [0.." + v.length + "]", pos );
   }
#endif   
    return v[pos];
}

/*** META ((export s8vector-length) (arity #t)
           (peephole (postfix ".length")))
*/
function sc_s8vectorLength(v) {
#if HOP_RTS_DEBUG
   if (!(v instanceof Int8Array) ) {
      sc_typeError( "s8vector-length", "s8vector", v, 3 );
   }
#endif   
   return v.length;
}

/*** META ((export s8vector-set!) (arity #t)
           (peephole (hole 3 v "[" pos "] = " val)))
*/
function sc_s8vectorSetBang(v, pos, val) {
#if HOP_RTS_DEBUG
   if (!(v instanceof Int8Array) ) {
      sc_typeError( "s8vector-set!", "s8vector", v, 3 );
   }
   if (typeof pos !== "number") {
      sc_typeError( "s8vector-set!", "number", pos, 3 );
   }
   if (typeof val !== "number") {
      sc_typeError( "s8vector-set!", "number", pos, 3 );
   }
   if( pos >= v.length || pos < 0 ) {
      sc_error( "s8vector-set!", "index out of bounds [0.." + v.length + "]", pos );
   }
#endif   
    v[pos] = val;
}

/*** META ((export s8vector-ref) (arity #t)
           (peephole (hole 2 v "[" pos "]")))
*/
function sc_s8vectorRef(v, pos) {
#if HOP_RTS_DEBUG
   if (!(v instanceof Int8Array) ) {
      sc_typeError( "s8vector-ref", "s8vector", v, 3 );
   }
   if (typeof pos !== "number") {
      sc_typeError( "s8vector-ref", "number", pos, 3 );
   }
   if( pos >= v.length || pos < 0 ) {
      sc_error( "s8vector-ref", "index out of bounds [0.." + v.length + "]", pos );
   }
#endif   
    return v[pos];
}

/*** META ((export f32vector-length) (arity #t)
           (peephole (postfix ".length")))
*/
function sc_f32vectorLength(v) {
#if HOP_RTS_DEBUG
   if (!(v instanceof Int8Array) ) {
      sc_typeError( "f32vector-length", "f32vector", v, 3 );
   }
#endif   
   return v.length;
}

/*** META ((export f32vector-set!) (arity #t)
           (peephole (hole 3 v "[" pos "] = " val)))
*/
function sc_f32vectorSetBang(v, pos, val) {
#if HOP_RTS_DEBUG
   if (!(v instanceof Int8Array) ) {
      sc_typeError( "f32vector-set!", "f32vector", v, 3 );
   }
   if (typeof pos !== "number") {
      sc_typeError( "f32vector-set!", "number", pos, 3 );
   }
   if (typeof val !== "number") {
      sc_typeError( "f32vector-set!", "number", pos, 3 );
   }
   if( pos >= v.length || pos < 0 ) {
      sc_error( "f32vector-set!", "index out of bounds [0.." + v.length + "]", pos );
   }
#endif   
    v[pos] = val;
}

/*** META ((export f32vector-ref) (arity #t)
           (peephole (hole 2 v "[" pos "]")))
*/
function sc_f32vectorRef(v, pos) {
#if HOP_RTS_DEBUG
   if (!(v instanceof Int8Array) ) {
      sc_typeError( "f32vector-ref", "f32vector", v, 3 );
   }
   if (typeof pos !== "number") {
      sc_typeError( "f32vector-ref", "number", pos, 3 );
   }
   if( pos >= v.length || pos < 0 ) {
      sc_error( "f32vector-ref", "index out of bounds [0.." + v.length + "]", pos );
   }
#endif   
    return v[pos];
}

/*** META ((export vector->list f32vector->list array->list) (arity #t)) */
function sc_vector2list(a) {
    var res = null;
    for (var i = a.length-1; i >= 0; i--)
	res = sc_cons(a[i], res);
    return res;
}

/*** META ((export list->vector list->array) (arity #t)) */
function sc_list2vector(l) {
    var a = new sc_Vector();
    while(l !== null) {
	a.push(l.__hop_car);
	l = l.__hop_cdr;
    }
    return a;
}

/*** META ((export vector-fill! array-fill!) (arity #t)) */
function sc_vectorFillBang(a, fill) {
#if HOP_RTS_DEBUG
   if (!(a instanceof sc_Vector) ) {
      sc_typeError( "vector-fill!", "vector", a, 3 );
   }
#endif   
    for (var i = 0; i < a.length; i++)
	a[i] = fill;
}


/*** META ((export #t) (arity #t)) */
function sc_copyVector(a, len) {
#if HOP_RTS_DEBUG
   if (!(a instanceof sc_Vector) ) {
      sc_typeError( "copy-vector", "vector", a, 3 );
   }
   if (typeof len !== "number") {
      sc_typeError( "copy-vector", "number", len, 3 );
   }
#endif   
    if (len <= a.length)
	return a.slice(0, len);
    else {
	var tmp = a.concat();
	tmp.length = len;
	return tmp;
    }
}

/*** META ((export #t) (arity -2)
           (peephole (hole 3 a ".slice(" start "," end ")")))
*/
function sc_vectorCopy(a, start, end) {
#if HOP_RTS_DEBUG
   if (!(v instanceof sc_Vector) ) {
      sc_typeError( "vector-copy", "vector", v, 3 );
   }
#endif   
   return a.slice(start, end);
}

/*** META ((export #t) (arity -4)) */
function sc_vectorCopyBang(target, tstart, source, sstart, send) {
#if HOP_RTS_DEBUG
   if (!(target instanceof sc_Vector) ) {
      sc_typeError( "vector-copy!", "vector", target, 3 );
   }
   if (!(source instanceof sc_Vector) ) {
      sc_typeError( "vector-copy!", "vector", source, 3 );
   }
   if (typeof tstart !== "number") {
      sc_typeError( "vector-copy!", "number", tstart, 3 );
   }
   if (typeof sstart !== "number") {
      sc_typeError( "vector-copy!", "number", sstart, 3 );
   }
   if (typeof send !== "number") {
      sc_typeError( "vector-copy!", "number", ssend, 3 );
   }
#endif   
    if (!sstart) sstart = 0;
    if (!send) send = source.length;

    // if target == source we don't want to overwrite not yet copied elements.
    if (tstart <= sstart) {
	for (var i = tstart, j = sstart; j < send; i++, j++) {
	    target[i] = source[j];
	}
    } else {
	var diff = send - sstart;
	for (var i = tstart + diff - 1, j = send - 1;
	     j >= sstart;
	     i--, j--) {
	    target[i] = source[j];
	}
    }
    return target;
}

/*** META ((export #t) (arity -1)) */
function sc_vectorAppend() {
   if( arguments.length === 0 ) {
      return new sc_Vector( 0 );
   }

   if( arguments.length === 1 ) {
#if HOP_RTS_DEBUG
      if (!(arguments[0] instanceof sc_Vector) ) {
	 sc_typeError( "vector-append", "vector", arguments[0], 3 );
      }
#endif
   
      return arguments[ 0 ];
   } else {
      var len = 0;
      var i = 0;
      var j = 0;
      
      for( i = 0; i < arguments.length; i++ ) {
#if HOP_RTS_DEBUG
	 if (!(arguments[i] instanceof sc_Vector) ) {
	    sc_typeError( "vector-append", "vector", arguments[i], 3 );
	 }
#endif
	 len += arguments[ i ].length;
      }

      var res = new sc_Vector( len );

      for( i = 0; i < arguments.length; i++ ) {
	 var v = arguments[ i ];
	 sc_vectorCopyBang( res, j, v, 0, v.length );
	 j += v.length;
      }

      return res;
   }
}

function sc_vectorMapRes(res, proc, args) {
   var nbApplyArgs = args.length - 1;
   var applyArgs = new Array(nbApplyArgs);
   var len = res.length;
   
#if HOP_RTS_DEBUG
   if (!(res instanceof sc_Vector) ) {
      sc_typeError( "vector-map", "vector", res, 3 );
   }
   for (var i = 1; i <= nbApplyArgs; i++) {
      if (!(args[i] instanceof sc_Vector) ) {
	 sc_typeError( "vector-map", "vector", args[i], 3 );
      }
      if (args[i].length != res.length) {
	 sc_error( "vector-map", "wrong vector argument legnth", args[i] );
      }
   }
#endif
   for (var i = 0; i < len; i++) {
      for (var j = 0; j < nbApplyArgs; j++) {
	 applyArgs[j] = args[j + 1][ i ];
      }
      res[ i ] = proc.apply(null, applyArgs);
   }
   return res;
}

/*** META ((export #t) (arity -2)) */
function sc_vectorMap(proc, v1) {
   if (v1 === undefined) {
      return sc_makeVector(0);
   } else {
      return sc_vectorMapRes(new sc_Vector(v1.length), proc, arguments);
   }
}

/*** META ((export #t) (arity -2)) */
function sc_vectorMapBang(proc, v1) {
   if (v1 === undefined) {
      return false;
   } else {
      return sc_vectorMapRes(v1, proc, arguments);
   }
}

/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (safe-hole 1 "typeof " o " === 'function'")))
*/
function sc_isProcedure(o) {
    return (typeof o === "function");
}

/*** META ((export #t) (arity -3)) */
function sc_apply(proc) {
   var args = new Array();
   
   // first part of arguments are not in list-form.
   for (var i = 1; i < arguments.length - 1; i++) {
      args.push(arguments[i]);
   }

   var l = arguments[arguments.length - 1];
   while (l !== null) {
      args.push(l.__hop_car);
      l = l.__hop_cdr;
   }
   
#if HOP_RTS_DEBUG
   sc_arity_check( proc, args.length );
#endif       
   
   return proc.apply(null, args);
}

/*** META ((export #t) (arity -2)) */
function sc_map(proc, l1) {
   if (l1 === undefined) {
      return null;
   } else {
      var nbApplyArgs = arguments.length - 1;
      var applyArgs = new Array(nbApplyArgs);
      var revres = null;
      while (l1 !== null) {
	 for (var i = 0; i < nbApplyArgs; i++) {
	    applyArgs[i] = arguments[i + 1].__hop_car;
	    arguments[i + 1] = arguments[i + 1].__hop_cdr;
	 }
	 revres = sc_cons(proc.apply(null, applyArgs), revres);
      }
      return sc_reverseAppendBang(revres, null);
   }
}

/*** META ((export #t) (arity -2)) */
function sc_mapBang(proc, l1) {
    if (l1 === undefined) {
       return null;
    } else {
       var l1_orig = l1;
       var nbApplyArgs = arguments.length - 1;
       var applyArgs = new Array(nbApplyArgs);
      /* firebug seems to break the alias l1=arguments[1] */
       while (arguments[1] !== null) {
	  var tmp = l1;
	  for (var i = 0; i < nbApplyArgs; i++) {
	     applyArgs[i] = arguments[i + 1].__hop_car;
	     arguments[i + 1] = arguments[i + 1].__hop_cdr;
	  }
	  tmp.__hop_car = proc.apply(null, applyArgs);
       }
       return l1_orig;
    }
}
     
/*** META ((export #t) (arity -2)) */
function sc_forEach(proc, l1) {
   if (l1 === undefined) {
      return undefined;
   } else {
      var nbApplyArgs = arguments.length - 1;
      var applyArgs = new Array(nbApplyArgs);
      /* firebug seems to break the alias l1=arguments[1] */
      while (arguments[1] !== null) {
	 for (var i = 0; i < nbApplyArgs; i++) {
	    applyArgs[i] = arguments[i + 1].__hop_car;
	    arguments[i + 1] = arguments[i + 1].__hop_cdr;
	 }
	 proc.apply(null, applyArgs);
      }
      // add return so FF does not complain.
      return undefined;
   }
}

/*** META ((export #t) (arity #t)) */
function sc_filter(proc, l1) {
    var dummy = { __hop_cdr : null };
    var tail = dummy;
    while (l1 !== null) {
	if (proc(l1.__hop_car) !== false) {
	    tail.__hop_cdr = sc_cons(l1.__hop_car, null);
	    tail = tail.__hop_cdr;
	}
	l1 = l1.__hop_cdr;
    }
    return dummy.__hop_cdr;
}

/*** META ((export #t) (arity #t)) */
function sc_findTail(proc, l1) {
   while (l1 !== null) {
      if (proc(l1.__hop_car)) {
	 return l1;
      } else {
	 l1 = l1.__hop_cdr;
      }
   }
}

/*** META ((export #t) (arity #t)) */
function sc_find(proc, l1) {
   var l = sc_findTail(proc, l1);

   return l ? l.__hop_car : false;
}
	 
/*** META ((export #t) (arity #t)) */
function sc_filterBang(proc, l1) {
    var head = sc_cons("dummy", l1);
    var it = head;
    var next = l1;
    while (next !== null) {
        if (proc(next.__hop_car) !== false) {
	    it.__hop_cdr = next
	    it = next;
	}
	next = next.__hop_cdr;
    }
    it.__hop_cdr = null;
    return head.__hop_cdr;
}

function sc_filterMap1(proc, l1) {
    var revres = null;
    while (l1 !== null) {
        var tmp = proc(l1.__hop_car)
        if (tmp !== false) revres = sc_cons(tmp, revres);
        l1 = l1.__hop_cdr;
    }
    return sc_reverseAppendBang(revres, null);
}
function sc_filterMap2(proc, l1, l2) {
    var revres = null;
    while (l1 !== null) {
        var tmp = proc(l1.__hop_car, l2.__hop_car);
        if(tmp !== false) revres = sc_cons(tmp, revres);
	l1 = l1.__hop_cdr;
	l2 = l2.__hop_cdr
    }
    return sc_reverseAppendBang(revres, null);
}

/*** META ((export #t) (arity -2)) */
function sc_filterMap(proc, l1, l2, l3) {
    if (l2 === undefined)
	return sc_filterMap1(proc, l1);
    else if (l3 === undefined)
	return sc_filterMap2(proc, l1, l2);
    // else
    var nbApplyArgs = arguments.length - 1;
    var applyArgs = new Array(nbApplyArgs);
    var revres = null;
    while (l1 !== null) {
	for (var i = 0; i < nbApplyArgs; i++) {
	    applyArgs[i] = arguments[i + 1].__hop_car;
	    arguments[i + 1] = arguments[i + 1].__hop_cdr;
	}
	var tmp = proc.apply(null, applyArgs);
	if(tmp !== false) revres = sc_cons(tmp, revres);
    }
    return sc_reverseAppendBang(revres, null);
}

function sc_any1(proc, l) {
    var revres = null;
    while (l !== null) {
        var tmp = proc(l.__hop_car);
        if(tmp !== false) return tmp;
	l = l.__hop_cdr;
    }
    return false;
}

/*** META ((export #t) (arity -2)) */
function sc_any(proc, l1, l2) {
    if (l1 === undefined)
	return false;
    if (l2 === undefined)
	return sc_any1(proc, l1);
    // else
    var nbApplyArgs = arguments.length - 1;
    var applyArgs = new Array(nbApplyArgs);
    while (l1 !== null) {
	for (var i = 0; i < nbApplyArgs; i++) {
	    applyArgs[i] = arguments[i + 1].__hop_car;
	    arguments[i + 1] = arguments[i + 1].__hop_cdr;
	}
	var tmp =  proc.apply(null, applyArgs);
	if (tmp !== false) return tmp;
    }
    return false;
}

function sc_every1(proc, l) {
    var revres = null;
    var tmp = true;
    while (l !== null) {
        tmp = proc(l.__hop_car);
        if (tmp === false) return false;
	l = l.__hop_cdr;
    }
    return tmp;
}

/*** META ((export #t) (arity -2)) */
function sc_every(proc, l1, l2) {
    if (l1 === undefined)
	return true;
    if (l2 === undefined)
	return sc_every1(proc, l1);
    // else
    var nbApplyArgs = arguments.length - 1;
    var applyArgs = new Array(nbApplyArgs);
    var tmp = true;
    while (l1 !== null) {
	for (var i = 0; i < nbApplyArgs; i++) {
	    applyArgs[i] = arguments[i + 1].__hop_car;
	    arguments[i + 1] = arguments[i + 1].__hop_cdr;
	}
	var tmp = proc.apply(null, applyArgs);
	if (tmp === false) return false;
    }
    return tmp;
}

/*** META ((export #t) (arity #t)
           (peephole (postfix "()")))
*/
function sc_force(o) {
    return o();
}

/*** META ((export #t) (arity #t)) */
function sc_makePromise(proc) {
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

function sc_Values(values) {
    this.values = values;
}

/*** META ((export #t) (arity -1)
           (peephole (values)))
*/
function sc_values() {
    if (arguments.length === 1)
	return arguments[0];
    else
	return new sc_Values(arguments);
}

/*** META ((export #t) (arity #t)) */
function sc_callWithValues(producer, consumer) {
   if( !sc_isProcedure(producer) )
      sc_error( "callWithValue", "producer not a procedure", producer );
      
    var produced = producer();
    if (produced instanceof sc_Values)
	return consumer.apply(null, produced.values);
    else
	return consumer(produced);
}

/*** META ((export #t) (arity #t)) */
function sc_dynamicWind(before, thunk, after) {
    before();
    try {
	var res = thunk();
	return res;
    } finally {
	after();
    }
}


// TODO: eval/scheme-report-environment/null-environment/interaction-environment

// LIMITATION: 'load' doesn't exist without files.
// LIMITATION: transcript-on/transcript-off doesn't exist without files.


function sc_Struct(name) {
    this['sc_struct name'] = name;
}
sc_Struct.prototype.sc_toDisplayString = function() {
    return "#<struct" + sc_hash(this) + ">";
};
sc_Struct.prototype.sc_toWriteString = sc_Struct.prototype.sc_toDisplayString;

/*** META ((export #t) (arity #t)
           (peephole (hole 1 "new sc_Struct(" name ")")))
*/
function sc_makeStruct(name) {
    return new sc_Struct(name);
}

/*** META ((export #t) (arity 1)
           (type bool)
           (peephole (safe-postfix " instanceof sc_Struct")))
*/
function sc_isStruct(o) {
    return (o instanceof sc_Struct);
}

/*** META ((export #t) (arity #t)
           (type bool)
           (peephole (safe-hole 2 "(" 1 " instanceof sc_Struct) && ( " 1 "['sc_struct name'] === " 0 ")")))
*/
function sc_isStructNamed(name, s) {
    return ((s instanceof sc_Struct) && (s['sc_struct name'] === name));
}

/*** META ((export struct-field) (arity #t)
           (peephole (hole 3 0 "[" 2 "]")))
*/
function sc_getStructField(s, name, field) {
#if HOP_RTS_DEBUG
   if (!(s instanceof sc_Struct) ) {
      sc_typeError( "struct-ref", "struct", s, 3 );
   }
#endif
    return s[field];
}

/*** META ((export struct-field-set!) (arity #t)
           (peephole (hole 4 0 "[" 2 "] = " 3)))
*/
function sc_setStructFieldBang(s, name, field, val) {
#if HOP_RTS_DEBUG
   if (!(s instanceof sc_Struct) ) {
      sc_typeError( "struct-set!", "struct", s, 3 );
   }
#endif
    s[field] = val;
}

/*---------------------------------------------------------------------*/
/*    sc_bitNot ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)
           (peephole (prefix "~")))
*/
function sc_bitNot(x) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if (typeof x !== "number") {
      sc_typeError( "bit-not", "number", x, 3 );
   }
#endif
#endif
   var res = ~x;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "bit-not", res, x );
   }
#endif
#endif
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    sc_bitAnd ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)
           (peephole (infix 2 2 "&")))
*/
function sc_bitAnd(x, y) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if (typeof x !== "number") {
      sc_typeError( "bit-and", "number", x, 3 );
   }
   if (typeof y !== "number") {
      sc_typeError( "bit-and", "number", y, 3 );
   }
#endif
#endif
   var res = x & y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "bit-and", res, x, y );
   }
#endif
#endif   

   return res;
}

/*---------------------------------------------------------------------*/
/*    bit-or ...                                                       */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)
           (peephole (infix 2 2 "|")))
*/
function sc_bitOr(x, y) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if (typeof x !== "number") {
      sc_typeError( "bit-or", "number", x, 3 );
   }
   if (typeof y !== "number") {
      sc_typeError( "bit-or", "number", y, 3 );
   }
#endif
#endif   
   var res = x | y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "bit-or", res, x, y );
   }
#endif
#endif   

   return res;
}

/*---------------------------------------------------------------------*/
/*    bit-xor ...                                                      */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)
           (peephole (infix 2 2 "^")))
*/
function sc_bitXor(x, y) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if (typeof x !== "number") {
      sc_typeError( "bit-xor", "number", x, 3 );
   }
   if (typeof y !== "number") {
      sc_typeError( "bit-xor", "number", y, 3 );
   }
#endif       
#endif   
   var res = x ^ y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "bit-xor", res, x, y );
   }
#endif
#endif   

   return res;
}

/*---------------------------------------------------------------------*/
/*    sc_bitLsh ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)
           (peephole (infix 2 2 "<<")))
*/
function sc_bitLsh(x, y) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if (typeof x !== "number") {
      sc_typeError( "bit-lsh", "number", x, 3 );
   }
   if (typeof y !== "number") {
      sc_typeError( "bit-lsh", "number", y, 3 );
   }
#endif
#endif
   var res = x << y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "bit-lsh", res, x, y );
   }
#endif
#endif   

   return res;
}

/*---------------------------------------------------------------------*/
/*    sc_bitRsh ...                                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)
           (peephole (infix 2 2 ">>")))
*/
function sc_bitRsh(x, y) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if (typeof x !== "number") {
      sc_typeError( "bit-rsh", "number", x, 3 );
   }
   if (typeof y !== "number") {
      sc_typeError( "bit-rsh", "number", y, 3 );
   }
#endif
#endif   
   var res = x >> y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "bit-rsh", res, x, y );
   }
#endif
#endif   

   return res;
}

/*---------------------------------------------------------------------*/
/*    sc_bitUrsh ...                                                   */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)
           (peephole (infix 2 2 ">>>")))
*/
function sc_bitUrsh(x, y) {
#if HOP_RTS_DEBUG
#if HOP_RTS_DEBUG_NUMERIC_TYPE
   if (typeof x !== "number") {
      sc_typeError( "bit-ursh", "number", x, 3 );
   }
   if (typeof y !== "number") {
      sc_typeError( "bit-ursh", "number", y, 3 );
   }
#endif
#endif
   var res = x >>> y;
   
#if HOP_RTS_DEBUG
#if !HOP_RTS_DEBUG_NUMERIC_TYPE
#if HOP_RTS_DEBUG_NUMERIC_ISNAN   
   if( isNaN( res ) ) {
#else
   if( res !== res ) {
#endif
      return sc_checkNumericTypes( "bit-ursh", res, x, y );
   }
#endif
#endif   

   return res;
}

/*** META ((export js-field js-property js-ref) (arity #t)
           (peephole (hole 2 o "[" field "]")))
*/
function sc_jsField(o, field) {
    return o[field];
}

/*** META ((export js-field-set! js-property-set! js-set!)
           (arity #t)
           (peephole (hole 3 o "[" field "] = " val)))
*/
function sc_setJsFieldBang(o, field, val) {
    return o[field] = val;
}

/*** META ((export js-field-delete! js-property-delete!)
           (arity #t)
           (peephole (hole 2 "delete " o "[" field "]")))
*/
function sc_deleteJsFieldBang(o, field) {
    delete o[field];
}

/*** META ((export #t)
           (arity -3)
           (peephole (jsCall)))
*/
function sc_jsCall(o, fun) {
    var args = new Array();
    for (var i = 2; i < arguments.length; i++)
	args[i-2] = arguments[i];
    return fun.apply(o, args);
}

/*** META ((export #t)
           (arity -3)
           (peephole (jsMethodCall)))
*/
function sc_jsMethodCall(o, field) {
    var args = new Array();
    for (var i = 2; i < arguments.length; i++)
	args[i-2] = arguments[i];
    return o[field].apply(o, args);
}

/*** META ((export new js-new)
           (arity -2)
           (peephole (jsNew)))
*/
function sc_jsNew(c) {
    var evalStr = "new c(";
    evalStr +=arguments.length > 1? "arguments[1]": "";
    for (var i = 2; i < arguments.length; i++)
	evalStr += ", arguments[" + i + "]";
    evalStr +=")";
    return eval(evalStr);
}    

// ======================== RegExp ====================
/*** META ((export #t) (arity #t)) */
function sc_pregexp(re) {
    return new RegExp(sc_string2jsstring(re));
}

/*** META ((export #t) (arity #t)) */
function sc_pregexpMatch(re, s) {
    var reg = (re instanceof RegExp) ? re : sc_pregexp(re);
    var tmp = reg.exec(sc_string2jsstring(s));
    
    if (tmp == null) return false;
    
    var res = null;
    for (var i = tmp.length-1; i >= 0; i--) {
	if (tmp[i] !== null) {
	    res = sc_cons(sc_jsstring2string(tmp[i]), res);
	} else {
	    res = sc_cons(false, res);
	}
    }
    return res;
}
   
/*** META ((export #t) (arity #t)) */
function sc_pregexpReplace(re, s1, s2) {
   var reg;
   var jss1 = sc_string2jsstring(s1);
   var jss2 = sc_string2jsstring(s2);

   if (re instanceof RegExp) {
       if (re.global)
	   reg = re;
       else
	   reg = new RegExp(re.source);
   } else {
       reg = new RegExp(sc_string2jsstring(re));
   }

   return jss1.replace(reg, jss2);
}
   
/*** META ((export pregexp-replace*) (arity #t)) */
function sc_pregexpReplaceAll(re, s1, s2) {
   var reg;
   var jss1 = sc_string2jsstring(s1);
   var jss2 = sc_string2jsstring(s2);

   if (re instanceof RegExp) {
      if (re.global)
	  reg = re;
      else
	  reg = new RegExp(re.source, "g");
   } else {
       reg = new RegExp(sc_string2jsstring(re), "g");
   }

   return jss1.replace(reg, jss2);
}

/*** META ((export #t) (arity #t)) */
function sc_pregexpSplit(re, s) {
   var reg = ((re instanceof RegExp) ?
	      re :
	      new RegExp(sc_string2jsstring(re)));
   var jss = sc_string2jsstring(s);
   var tmp = jss.split(reg);

   if (tmp == null) return false;

   return sc_vector2list(tmp);
}
   
function sc_pregexpCreateCharsetMatcher(set) {
    if (set.length === 0 || set.length === 1) return new RegExp("[" + set + "]");
    var res = "[";
    for (var i = 0; i < set.length; i++) {
	var c = set.charAt(i);
	if (c === "]")
	    res += "\\]";
	else if (c === "^")
	    res += "\\^";
	else if (c === "\\")
	    res += "\\\\";
	else if (c === "-")
	    res += "\\-";
	else res += c;
    }
    return new RegExp(res + "]");
}

/* =========================================================================== */
/* Other library stuff */
/* =========================================================================== */

/*** META ((export #t) (arity #t)
           (peephole (hole 1 "Math.floor(Math.random()*" 'n ")")))
*/
function sc_random(n) {
    return Math.floor(Math.random()*n);
}

/*** META ((export current-date) (arity #t)
           (peephole (hole 0 "new Date()")))
*/
function sc_currentDate() {
   return new Date();
}

/*** META ((export date->string) (arity #t))
*/
function sc_date2jsstring(d) {
   return sc_string2jsstring( d.toString() );
}

/*** META ((export current-seconds) (arity #t)) 
*/
function sc_currentSeconds() {
   return Math.round((new Date()).getTime() / 1000);
}

/*** META ((export current-microseconds) (arity #t)) 
*/
function sc_currentMicroseconds() {
   return (new Date()).getTime();
}

/*** META ((export #t) (arity #t)) 
*/
function sc_time(proc) {
   var start = sc_currentMicroseconds();
   var res = proc();
   var stop = sc_currentMicroseconds();

   return sc_values( res, stop - start, 0, 0 );
}

function sc_Hashtable() {
}
sc_Hashtable.prototype.toString = function() {
    return "#{%hashtable}";
};
// sc_toWriteString == sc_toDisplayString == toString

function sc_HashtableElement(key, val) {
    this.key = key;
    this.val = val;
}

// the arity of make-hashtable inside Bigloo is -1. However we don't use it
// here. So for now simply don't give the arity...
/*** META ((export #t)
           (peephole (hole 0 "new sc_Hashtable()")))
*/
function sc_makeHashtable() {
    return new sc_Hashtable();
}

/*** META ((export #t) (arity #t)
           (type bool)) */
function sc_isHashtable(o) {
    return o instanceof sc_Hashtable;
}

/*** META ((export #t) (arity #t)) */
function sc_hashtableSize(ht) {
#if HOP_RTS_DEBUG
   if (!(ht instanceof sc_Hashtable)) {
      sc_typeError( "hashtable-size", "hashtable", ht, 3 );
   }
#endif       
    var count = 0
    for (var hash in ht) {
	if (ht[hash] instanceof sc_HashtableElement)
	    count++;
    }
    return count;
}

/*** META ((export #t) (arity #t)) */
function sc_hashtablePutBang(ht, key, val) {
#if HOP_RTS_DEBUG
   if (!(ht instanceof sc_Hashtable)) {
      sc_typeError( "hashtable-put!", "hashtable", ht, 3 );
   }
#endif       
    var hash = sc_hash(key);
    ht[hash] = new sc_HashtableElement(key, val);
}

/*** META ((export #t) (arity #t)) */
function sc_hashtableGet(ht, key) {
#if HOP_RTS_DEBUG
   if (!(ht instanceof sc_Hashtable)) {
      sc_typeError( "hashtable-get", "hashtable", ht, 3 );
   }
#endif       
    var hash = sc_hash(key);
    if (hash in ht)
	return ht[hash].val;
    else
	return false;
}

/*** META ((export #t) (arity #t)) */
function sc_hashtableRemoveBang(ht, key) {
#if HOP_RTS_DEBUG
   if (!(ht instanceof sc_Hashtable)) {
      sc_typeError( "hashtable-remove!", "hashtable", ht, 3 );
   }
#endif       
    var hash = sc_hash(key);
    if (hash in ht) {
	delete ht[hash];
	return true;
    }
    else
	return false;
}

/*** META ((export #t) (arity #t)) */
function sc_hashtableForEach(ht, f) {
#if HOP_RTS_DEBUG
   if (!(ht instanceof sc_Hashtable)) {
      sc_typeError( "hashtable-for-each", "hashtable", ht, 3 );
   }
#endif       
    for (var v in ht) {
	if (ht[v] instanceof sc_HashtableElement)
	    f(ht[v].key, ht[v].val);
    }
}


/*** META ((export #t) (arity #t)) */
function sc_hashtableMap(ht, f) {
#if HOP_RTS_DEBUG
   if (!(ht instanceof sc_Hashtable)) {
      sc_typeError( "hashtable-map", "hashtable", ht, 3 );
   }
#endif       
   var hd = sc_cons( null, null );
   var res = hd;
   
   for (var v in ht) {
      if (ht[v] instanceof sc_HashtableElement) {
	 res.__hop_cdr = sc_cons( f(ht[v].key, ht[v].val), null );
	 res = res.__hop_cdr;
      }
   }

   return hd.__hop_cdr;
}

/*** META ((export #t) (arity #t)) */
function sc_hashtableKeyList(ht) {
#if HOP_RTS_DEBUG
   if (!(ht instanceof sc_Hashtable)) {
      sc_typeError( "hashtable-key-list", "hashtable", ht, 3 );
   }
#endif       
   var hd = sc_cons( null, null );
   var res = hd;
   
   for (var v in ht) {
      if (ht[v] instanceof sc_HashtableElement) {
	 res.__hop_cdr = sc_cons( ht[v].key, null );
	 res = res.__hop_cdr;
      }
   }

   return hd.__hop_cdr;
}

/*** META ((export #t) (arity #t)) */
function sc_hashtable2list(ht) {
#if HOP_RTS_DEBUG
   if (!(ht instanceof sc_Hashtable)) {
      sc_typeError( "hashtable->list", "hashtable", ht, 3 );
   }
#endif       
   var hd = sc_cons( null, null );
   var res = hd;
   
   for (var v in ht) {
      if (ht[v] instanceof sc_HashtableElement) {
	 res.__hop_cdr = sc_cons( ht[v].val, null );
	 res = res.__hop_cdr;
      }
   }

   return hd.__hop_cdr;
}

/*** META ((export #t) (arity #t)) */
function sc_hashtable2vector(ht) {
#if HOP_RTS_DEBUG
   if (!(ht instanceof sc_Hashtable)) {
      sc_typeError( "hashtable->list", "hashtable", ht, 3 );
   }
#endif
   var res = sc_vector(sc_hashtableSize(ht));
   var i = 0;
   
   for (var v in ht) {
      if (ht[v] instanceof sc_HashtableElement) {
	 res[i++]=ht[v].val;
      }
   }

   return res;
}

/*** META ((export hashtable-contains?)
           (arity #t)
           (peephole (hole 2 "sc_hash(" 1 ") in " 0)))
*/
function sc_hashtableContains(ht, key) {
#if HOP_RTS_DEBUG
   if (!(ht instanceof sc_Hashtable)) {
      sc_typeError( "hashtable-contains", "hashtable", ht, 3 );
   }
#endif       
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
    else if (o === true)
	return "true";
    else if (o === false)
	return "false";
    else if (typeof o === "number")
	return "num-" + o;
    else if (typeof o === "string")
	return "jsstr-" + o;
    else if (o.sc_getHash)
	return o.sc_getHash();
    else
	return sc_counterHash.call(o);
}
function sc_counterHash() {
    if (!this.sc_hash) {
	this.sc_hash = "hash-" + SC_HASH_COUNTER;
	SC_HASH_COUNTER++;
    }
    return this.sc_hash;
}

function sc_Trampoline() {
}

sc_Trampoline.prototype.restart = function() {
    while (true) {
	this.calls = this.MAX_TAIL_CALLs-1;
	var res = this.f.apply(this, this.args);
	if (res !== this)
	    return res;
    }
}

/*** META ((export bind-exit-lambda) (arity #t)) */
function sc_bindExitLambda(proc) {
    var escape_obj = new sc_BindExitException();
    var escape = function(res) {
	escape_obj.res = res;
	throw escape_obj;
    };
    try {
	return proc(escape);
    } catch(e) {
	if (e === escape_obj) {
	    return e.res;
	}
	throw e;
    }
}
function sc_BindExitException() {
    this._internalException = true;
}

/*** META ((export unwind-protect-lambda) (arity #t)) */
function sc_unwindProtectLambda(proc1, proc2) {
   try {
      return proc1();
   } finally {
      proc2();
   }
}

var SC_SCM2JS_GLOBALS = new Object();

var SC_TAIL_OBJECT = new sc_Trampoline();  // (used in runtime_callcc.)
SC_SCM2JS_GLOBALS.TAIL_OBJECT = SC_TAIL_OBJECT;

/*---------------------------------------------------------------------*/
/*    OO layer                                                         */
/*---------------------------------------------------------------------*/
var sc_allClasses = {};

function sc_Class() {
   ;
}

/*** META ((export #t)) */
function sc_Object() {
}

sc_Class.prototype.toString = function() {
   return "#<class:" + sc_symbol2jsstring( sc_class_name( this ) ) + ">";
}
sc_Class.prototype.sc_toWriteOrDisplayString = sc_Class.prototype.toString;

sc_Object.prototype.toString = function() {
   var clazz = sc_object_class( this );
   var res = "#|" + sc_symbol2jsstring( clazz.sc_name );

   if( sc_isNil( this ) ) {
      return res + " nil|"
   } else {
      var fields = sc_class_all_fields( clazz );

      for( var i = 0; i < fields.length; i++ ) {
	 res += " ["
	    + sc_symbol2jsstring( fields[ i ].sc_name )
	    + ": "
	    + fields[ i ].sc_getter( this )
	    + "]";
      }
      
      return res + "|";
   }
}

function sc_register_class( clazz, name, zuper, hash, allocator, constructor, fields ) {
   var ftable = {};

   clazz.toString = sc_Class.prototype.toString;
   clazz.toWriteOrDisplayString = sc_Class.prototype.toString;
   clazz.sc_name = name;
   clazz.sc_super = zuper;
   clazz.sc_hash = hash;
   clazz.sc_constructor = constructor;
   clazz.sc_fields = fields;
   clazz.sc_fields_table = ftable;
   clazz.sc_allocator = allocator;

   for( var i = 0; i < fields.length; i++ ) {
      var f = fields[ i ];
      
      ftable[ f.sc_name ] = f;
   }

   if( zuper != clazz ) {
      var constr = ("prototype" in clazz) ? clazz.prototype.constructor
	  : function( c ) { return c; };
      
      clazz.prototype = sc_class_creator( zuper );
      clazz.prototype.constructor = constr;

      for( f in zuper.sc_fields_table ) {
	 ftable[ zuper.sc_fields_table[ f ].sc_name ] = zuper.sc_fields_table[ f ];
      }
      
      clazz.sc_all_fields = sc_vectorAppend( zuper.sc_all_fields, fields );
   } else {
      clazz.sc_all_fields = fields;
   }

   sc_allClasses[ name ] = clazz;

   return clazz;
}

/*** META ((export #t) (arity #t) (type bool)) */
function sc_isClass( o ) {
   return (o.toString === sc_Class.prototype.toString);
}

/*** META ((export #t) (arity #t)) */
function sc_class_exists( cname ) {
   if( cname in sc_allClasses ) {
      return sc_allClasses[ cname ];
   } else {
      return false;
   }
}

/*** META ((export #t) (arity #t)) */
function sc_find_class( cname ) {
   var c = sc_class_exists( cname );

   if( c ) {
      return c;
   } else {
      sc_error( "find-class", "Can't find class", cname );
   }
}

/*** META ((export #t) (arity #t)) */
function sc_class_nil( clazz ) {
   if( !clazz.sc_nil ) clazz.sc_nil = clazz.sc_allocator();
   
   return clazz.sc_nil;
}
   
/*** META ((export #t) (arity #t)) */
function sc_class_name( clazz ) {
#if HOP_RTS_DEBUG
   if (!sc_isClass(clazz)) {
      sc_typeError( "class-fields", "class", clazz, 3 );
   }
#endif       
   return clazz.sc_name;
}

/*** META ((export #t) (arity #t)) */
function sc_class_super( clazz ) {
   return clazz.sc_super;
}
   
/*** META ((export #t) (arity #t)) */
function sc_class_hash( clazz ) {
   return clazz.sc_hash;
}

/*** META ((export #t) (arity #t)) */
function sc_class_allocator( clazz ) {
   return clazz.sc_allocator;
}

/*** META ((export #t) (arity #t)) */
function sc_class_creator( clazz ) {
   return function() {
      var o =  clazz.sc_allocator();
      var f = sc_class_all_fields( clazz );

      for( i = 0; i < f.length; i++ ) {
	 o[ sc_symbol2jsstring( f[ i ].sc_name ) ] = arguments[ i ];
      }

      return o;
   }
}

/*** META ((export #t) (arity #t)) */
function sc_isA( o, c ) {
   return o instanceof c.allocator;
}

/*** META ((export #t) (arity #t)) */
function sc_isNil( o ) {
   var clazz = sc_object_class( o );
   return sc_class_nil( clazz ) === o;
}

function sc_Field( name, getter, setter, ronly, virtual, info, def, type ) {
   this.sc_name = name;
   this.sc_getter = getter;
   this.sc_setter = setter;
   this.sc_ronly = ronly;
   this.sc_virtual = virtual;
   this.sc_info = info;
   this.sc_def = def;
   this.sc_type = type;
}

function sc_getprototype( o ) {
   if( o instanceof sc_Object ) {
      if( "__proto__" in o ) {
	 return o.__proto__;
      } else {
	 return Object.GetPrototypeOf( o );
      }
   } else {
      return false;
   }
}

/*** META ((export #t) (arity #t)) */
function sc_object_class( o ) {
   var proto = sc_getprototype( o );

   if( proto ) {
      return proto.constructor;
   } else {
      return sc_Object;
   }
}

/*** META ((export #t) (arity #t)) */
function sc_class_fields( clazz ) {
#if HOP_RTS_DEBUG
   if (!sc_isClass(clazz)) {
      sc_typeError( "class-fields", "class", clazz, 3 );
   }
#endif       
   return clazz.sc_fields;
}

/*** META ((export #t) (arity #t)) */
function sc_class_all_fields( clazz ) {
#if HOP_RTS_DEBUG
   if (!sc_isClass(clazz)) {
      sc_typeError( "class-fields", "class", clazz, 3 );
   }
#endif       
   return clazz.sc_all_fields;
}

/*** META ((export #t) (arity #t)) */
function sc_find_class_field( clazz, id ) {
#if HOP_RTS_DEBUG
   if (!sc_isClass(clazz)) {
      sc_typeError( "class-field", "class", clazz, 3 );
   }
#endif       
   return clazz.sc_fields_table[ id ];
}

/*** META ((export #t) (arity #t)) */
function sc_class_field_name( field ) {
   return field.sc_name;
}

/*** META ((export #t) (arity #t)) */
function sc_class_field_default_value( field ) {
   return field.sc_def();
}

function sc_add_method( clazz, generic, proc ) {
   var name = sc_symbol2jsstring( generic );

   if( !clazz ) {
      Object.prototype[ name ] = proc;
   } else {
      clazz.prototype[ name ] = proc;
   }
}

/*** META ((export #t) (arity #t)) */
function sc_class_field_accessor( field ) {
   return field.sc_getter;
}

/*** META ((export #t) (arity #t)) */
function sc_class_field_mutator( field ) {
   return field.sc_setter;
}

/* OO bootstrap in mutable.js and immutable.js */
