/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/share/hopscheme.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 24 14:35:05 2007                          */
/*    Last change :  Wed Apr  6 19:24:38 2016 (serrano)                */
/*    Copyright   :  2007-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop adpatation of the scheme2js runtime.                         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Global parameters                                                */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
var hop_config = {
   uint8array: false
}
#if HOP_RTS_DEBUG
var sc_context = null;
#endif
#endif

/*---------------------------------------------------------------------*/
/*    typeof                                                           */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
sc_Pair.prototype.hop_typeof = function() {
   return "pair";
};

if( "defineProperty" in Object ) {
   Object.defineProperty( sc_Vector, "hop_typeof", {
      value: function() { return "vector"; },
      enumerable: false
   } );
   Object.defineProperty( String, "hop_typeof", {
      value: function() { return hop_typeof(this.toString()); },
      enumerable: false
   } );
   Object.defineProperty( Boolean, "hop_typeof", {
      value: function() { return "bbool"; },
      enumerable: false
   } );
} else {
   sc_Vector.prototype.hop_typeof = function() {
      return "vector";
   };
   String.prototype.hop_typeof = function() {
      return hop_typeof(this.toString());
   };
   Boolean.prototype.hop_typeof = function() {
      return "bbool";
   };
}

sc_Struct.prototype.hop_typeof = function() {
   return "struct";
};

sc_OutputPort.prototype.hop_typeof = function() {
   return "output-port";
};

sc_StringOutputPort.prototype.hop_typeof = function() {
   return "output-port";
};

sc_GenericOutputPort.prototype.hop_typeof = function() {
   return "output-port";
};

sc_InputPort.prototype.hop_typeof = function() {
   return "input-port";
};

sc_Char.prototype.hop_typeof = function() {
   return "bchar";
};
#endif

/*---------------------------------------------------------------------*/
/*    hop_plist2jsobject ...                                           */
/*---------------------------------------------------------------------*/
#if HOP_NOBROWSER
function hop_plist2jsobject( plist ) {
   var o = {};

   while( sc_isPair( plist ) ) {
      o[ sc_keyword2string( plist.__hop_car ) ] = plist.__hop_cdr.__hop_car;
      plist = plist.__hop_cdr.__hop_cdr;
   }

   return o;
}
#endif

/*---------------------------------------------------------------------*/
/*    lists ...                                                        */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
function sc_Pair( car, cdr ) {
   this.__hop_car = car;
   this.__hop_cdr = cdr;
}

function sc_isPair( p ) {
   return p instanceof sc_Pair;
}

function sc_car( p ) {
   return p.__hop_car;
}

function sc_cdr( p ) {
   return p.__hop_cdr;
}

function sc_cons( car, cdr ) {
   return new sc_Pair( car, cdr );
}

#if HOP_RTS_DEBUG
function sc_consStar() {
    var res = arguments[arguments.length - 1];
    for (var i = arguments.length-2; i >= 0; i--)
	res = new sc_Pair(arguments[i], res);
    return res;
}
#endif

#if HOP_RTS_DEBUG
function sc_reverse( l1 ) {
   var res = null;
   while( l1 !== null ) {
      res = sc_cons( l1.__hop_car, res );
      l1 = l1.__hop_cdr;
   }
   return res;
}

function sc_append() {
   if( arguments.length === 0 ) {
      return null;
   }
   var res = arguments[ arguments.length - 1 ];
   for( var i = arguments.length - 2; i >= 0; i-- ) {
      res = sc_dualAppend( arguments[ i ], res );
   }
   return res;
}

function sc_reverseAppendBang( l1, l2 ) {
   var res = l2;
   while( l1 !== null ) {
      var tmp = res;
      res = l1;
      l1 = l1.__hop_cdr;
      res.__hop_cdr = tmp;
   }
   return res;
}

function sc_dualAppend( l1, l2 ) {
   if( l1 === null ) return l2;
   if( l2 === null ) return l1;
   var rev = sc_reverse( l1 );
   return sc_reverseAppendBang( rev, l2 );
}

function sc_dualAppendBang( l1, l2 ) {
   if( l1 === null ) return l2;
   if( l2 === null ) return l1;
   var tmp = l1;
   while( tmp.__hop_cdr !== null ) tmp = tmp.__hop_cdr;
   tmp.__hop_cdr = l2;
   return l1;
}
#endif

function sc_list() {
   var res = null;
   var a = arguments;
   for( var i = a.length-1; i >= 0; i-- ) {
      res = new sc_Pair( a[i], res  );
   }
   return res;
}
#endif

/*---------------------------------------------------------------------*/
/*    regexp                                                           */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
#if HOP_RTS_DEBUG
function sc_pregexp( re ) {
   return new RegExp( re );
}
#endif
#endif

/*---------------------------------------------------------------------*/
/*    hashtables                                                       */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
#if HOP_RTS_DEBUG
function sc_Hashtable() {
}

function sc_HashtableElement( key, val ) {
   this.key = key;
   this.val = val;
}

function sc_hashtableGet( ht, key ) {
   var hash = key.toString();
   return (hash in ht) ? ht[ hash ].val : false;
}

function sc_hashtablePutBang(ht, key, val) {
   var hash = key.toString();
   ht[ hash ] = new sc_HashtableElement( key, val );
}
#endif
#endif

/*---------------------------------------------------------------------*/
/*    Serialization                                                    */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_pair( l ) {
   var res = "";
   var len = 0;

   while( sc_isPair( l ) ) {
      res += hop_bigloo_serialize_context( l.__hop_car );
      l = l.__hop_cdr;
      len++;
   }

   if( l == null ) {
      return hop_serialize_word( len + 1 ) + res + ".";
   } else {
      return hop_serialize_word( len + 1 ) + res + hop_bigloo_serialize_context( l );
   }
}

sc_Pair.prototype.hop_bigloo_serialize = function() {
   return '(' + hop_bigloo_serialize_pair( this );
};

/*---------------------------------------------------------------------*/
/*    keywords & symbols                                               */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
var sc_SYMBOL_PREFIX = "\uEBAC";
var sc_KEYWORD_PREFIX = "\uEBAD";

function sc_isSymbol( s ) {
   return (typeof s === "string") && (s.charAt(0) === sc_SYMBOL_PREFIX);
}

function sc_symbol2string( s ) {
   return s.slice( 1 );
}

function sc_string2symbol( s ) {
   return sc_SYMBOL_PREFIX + s;
}

function sc_jsstring2symbol( s ) {
   return sc_SYMBOL_PREFIX + s;
}

function sc_isKeyword( s ) {
   return (typeof s === "string") && (s.charAt(0) === sc_KEYWORD_PREFIX);
}

function sc_keyword2string( s ) {
   return s.slice( 1 );
}

function sc_string2keyword( s ) {
   return sc_KEYWORD_PREFIX + s;
}
function sc_jsstring2keyword( s ) {
   return sc_KEYWORD_PREFIX + s;
}
#endif

/*---------------------------------------------------------------------*/
/*    chars                                                            */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
function sc_Char( c ) {
   this.val = c;
}
#endif

#if HOP_SCHEME
sc_Char.prototype.hop_bigloo_serialize = function() {
   return 'a' + hop_serialize_word( this.val.charCodeAt( 0 ) );
};
#endif

/*---------------------------------------------------------------------*/
/*    Objects                                                          */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
function sc_Object() {
}
#endif

/*---------------------------------------------------------------------*/
/*    Arrays                                                           */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
function sc_makeVector( sz, fill ) {
   return new Array( sz ).fill( fill );
}

function sc_vector2array( a ) {
   return a;
}
#endif

/*---------------------------------------------------------------------*/
/*    Misc                                                             */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
#if HOP_RTS_DEBUG
function sc_isNumber( x ) {
   return typeof( x ) === "number";
}

function sc_isString( s ) {
   return (typeof s === "string") &&
      (s.charAt(0) !== sc_SYMBOL_PREFIX) &&
      (s.charAt(0) !== sc_KEYWORD_PREFIX);
}

function sc_alert() {
   var len = arguments.length;
   var s = "";
   var i;

   for( i = 0; i < len; i++ ) {
      s += arguments[ i ].toString();
   }

   return alert( s );
}

function sc_arity_check( fun, arity ) {
   return fun;
}
#endif
#endif
