/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/share/hopscheme.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 24 14:35:05 2007                          */
/*    Last change :  Fri Aug 19 15:49:44 2016 (serrano)                */
/*    Copyright   :  2007-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop adpatation of the scheme2js runtime.                         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Global parameters                                                */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
hop_config.uint8array = false;

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
/*    Misc                                                             */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
function dom_get_element_by_id( id ) {
   return document.getElementById( id );
}

function dom_child_nodes( node ) {
   return sc_vector2list( node.childNodes );
}

function dom_first_child( node ) {
   return node.firstChild;
}

function dom_last_child( node ) {
   return node.lastChild;
}

function dom_parent_node( node ) {
   return node.parentNode;
}

function node_computed_style_get( obj, prop ) {
   var el = obj;
   if( (obj instanceof String) || (typeof obj === "string") )
      el = document.getElementById( obj );
   if( sc_isKeyword( prop ) )
      prop = sc_keyword2jsstring( prop );
   var t = window.getComputedStyle( el, null );
   if( t != null && (prop in t) )
      return t[ prop ];
   else
      return false;
}

function sc_jsNew( c ) {
   var evalStr = "new c(";
   evalStr += arguments.length > 1? "arguments[1]": "";
   for( var i = 2; i < arguments.length; i++ ) {
      evalStr += ", arguments[" + i + "]";
   }
   evalStr +=")";
   return eval( evalStr );
}

function sc_jsField( o, field ) {
    return o[ field ];
}

function sc_setJsFieldBang( o, field, val ) {
   return o[ field ] = val;
}

function hop_in( field, obj ) {
   return field in obj;
}

function sc_jsMethodCall( o, field ) {
   return o[ field ].apply( o, Array.prototype.slice.call( arguments, 2 ) );
}

function hop_typeof( obj ) {
   if( obj instanceof Object ) {
      if( obj instanceof Date ) {
	 return "date";
      } else {
	 if( obj instanceof RegExp ) {
	    return "regexp";
	 } else {
	    if( obj instanceof Function ) {
	       return "function";
	    } else {
	       if( typeof obj.hop_typeof == "function" ) 
		  return obj.hop_typeof();
	       else
		  return "object";
	    }
	 }
      }
   } else {
      var tname = typeof obj;
      if( tname == "string" ) {
	 if( sc_isSymbol( obj ) )
	    return "symbol";
	 if( sc_isKeyword( obj ) )
	    return "keyword";
	 return tname;
      }
      return tname;
   }
}

#if HOP_RTS_DEBUG
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
   throw e;
}
function sc_typeError( proc, type, obj ) {
   var msg = "Type \"" + type + "\" expected, "
      + "\"" + hop_typeof( obj ) + "\" provided";
   return sc_error( proc, msg, obj, arguments.length >= 4 ? arguments[ 3 ] : 2 );
}
#endif
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

function sc_forEach( proc, l1 ) {
   if (l1 === undefined) {
      return undefined;
   } else {
      var nbApplyArgs = arguments.length - 1;
      var applyArgs = new Array( nbApplyArgs );
      while( arguments[1] !== null ) {
	 for( var i = 0; i < nbApplyArgs; i++ ) {
	    applyArgs[ i ] = arguments[ i + 1 ].__hop_car;
	    arguments[ i + 1 ] = arguments[ i + 1 ].__hop_cdr;
	 }
	 proc.apply( null, applyArgs );
      }
      return undefined;
   }
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
function sc_isPairEqual(p1, p2, comp) {
    return (comp(p1.__hop_car, p2.__hop_car) && comp(p1.__hop_cdr, p2.__hop_cdr));
}

function sc_reverse( l1 ) {
   var res = null;
   while( l1 ) {
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

function sc_reverseBang( l ) {
   return sc_reverseAppendBang( l, null );
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

function sc_dualAppendBang( l1, l2 ) {
   if( l1 === null ) return l2;
   if( l2 === null ) return l1;
   var tmp = l1;
   while( tmp.__hop_cdr !== null ) tmp=tmp.__hop_cdr;
   tmp.__hop_cdr = l2;
   return l1;
}
    
function sc_appendBang() {
   var res = null;
   for( var i = 0; i < arguments.length; i++ )
      res = sc_dualAppendBang( res, arguments[ i ] );
   return res;
}

function sc_assq( o, al ) {
   while( al !== null ) {
      if( al.__hop_car.__hop_car === o )
	 return al.__hop_car;
      al = al.__hop_cdr;
   }
   return false;
}

function sc_assoc( o, al ) {
   while( al !== null ) {
      if( sc_isEqual( al.__hop_car.__hop_car, o ) )
	 return al.__hop_car;
      al = al.__hop_cdr;
   }
   return false;
}

function sc_filterMap1( proc, l1 ) {
   var revres = null;
   while( l1 !== null ) {
      var tmp = proc( l1.__hop_car )
      if( tmp !== false ) revres = sc_cons( tmp, revres );
      l1 = l1.__hop_cdr;
   }
   return sc_reverseAppendBang( revres, null );
}

function sc_filterMap2( proc, l1, l2 ) {
    var revres = null;
    while( l1 !== null ) {
        var tmp = proc( l1.__hop_car, l2.__hop_car );
        if( tmp !== false ) revres = sc_cons( tmp, revres );
	l1 = l1.__hop_cdr;
	l2 = l2.__hop_cdr
    }
    return sc_reverseAppendBang( revres, null );
}

function sc_filterMap( proc, l1, l2, l3 ) {
   if( l2 === undefined )
      return sc_filterMap1( proc, l1 );
   else if( l3 === undefined )
      return sc_filterMap2( proc, l1, l2 );
   // else
   var nbApplyArgs = arguments.length - 1;
   var applyArgs = new Array( nbApplyArgs );
   var revres = null;
   while( l1 !== null ) {
      for( var i = 0; i < nbApplyArgs; i++ ) {
	 applyArgs[ i ] = arguments[ i + 1 ].__hop_car;
	 arguments[ i + 1 ] = arguments[ i + 1 ].__hop_cdr;
      }
      var tmp = proc.apply( null, applyArgs );
      if( tmp !== false ) revres = sc_cons( tmp, revres );
   }
   return sc_reverseAppendBang( revres, null );
}

function sc_listTail( l, k ) {
   var res = l;
   for( var i = 0; i < k; i++ ) {
      res = res.__hop_cdr;
   }
   return res;
}

#endif

function sc_remqBang( o, l ) {
   var dummy = { __hop_cdr : null };
   var tail = dummy;
   var needsAssig = true;
   while( l !== null ) {
      if( l.__hop_car === o ) {
	 needsAssig = true;
      } else {
	 if( needsAssig ) {
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
/*    Regexp                                                           */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
#if HOP_RTS_DEBUG
function sc_pregexp( re ) {
   return new RegExp( re );
}

function sc_pregexpMatch( re, s ) {
   var reg = (re instanceof RegExp) ? re : new RegExp( re );
   var tmp = reg.exec( s );
   
   if( tmp == null ) return false;
   
   var res = null;
   for( var i = tmp.length-1; i >= 0; i-- ) {
      if( tmp[ i ] !== null ) {
	 res = sc_cons( tmp[ i ], res );
      } else {
	 res = sc_cons( false, res );
      }
   }
   return res;
}

function sc_pregexpReplace( re, s1, s2 ) {
   var reg;
   var jss1 = s1;
   var jss2 = s2;

   if( re instanceof RegExp ) {
      if( re.global )
	 reg = re;
      else
	 reg = new RegExp( re.source );
   } else {
      reg = new RegExp( re );
   }

   return jss1.replace( reg, jss2 );
}
#endif
#endif

/*---------------------------------------------------------------------*/
/*    Hashtables                                                       */
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
/*    Keywords & Symbols                                               */
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

function sc_symbol2jsstring( s ) {
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

function sc_keyword2jsstring( s ) {
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
/*    Strings                                                          */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT

function sc_stringAppend() {
   return "".concat.apply( "", arguments );
}

function sc_stringRef( s, k ) {
   return new sc_Char( s.charAt( k ) );
}

function sc_number2string( x, radix ) {
   if( radix ) {
      return x.toString( radix );
   } else {
      return x.toString();
   }
}

#if HOP_RTS_DEBUG
function sc_isStringEqual( s1, s2 ) {
    return s1 === s2;
}

function sc_jsstring2string( s ) {
   return s;
}

function sc_isString( s ) {
   return (typeof s === "string") &&
      (s.charAt(0) !== sc_SYMBOL_PREFIX) &&
      (s.charAt(0) !== sc_KEYWORD_PREFIX);
}

function sc_stringSplit( s, sep ) {
   if( arguments.length === 1 )
      return sc_vector2list( s.split( " " ) );
   if( sep.length === 1 )
      return sc_vector2list( s.split( sep ) );
   return sc_vector2list( s.split( sep ) );
}

function sc_isSubstring( s1, s2, len ) {
    if( s1.length < len ) return false;
    if( s2.length < len ) return false;
    return s2.substring( 0, len ) == s1.substring( 0, len );
}

function sc_substring( s, start, end ) {
   return s.substring (start, (end == undefined || end < 0) ? s.length : end );
}

function sc_string2integer( s, radix ) {
   return parseInt( s, radix );
}

function sc_stringIndex( s, cset, start ) {
   var res;
   if( !start ) start = 0;

   if( cset instanceof sc_Char ) {
      res = s.indexOf( sc_char2string( cset ), start );
      return res >= 0 ? res : false;
   }
   if( cset.length == 1 ) {
      res = s.indexOf( cset, start );
      return res >= 0 ? res : false;
   } else {
      for( var i = start; i < s.length; i++ ) {
	 if( cset.indexOf( s.charAt( i ) ) >= 0 ) {
	    return i;
	 }
      }

      return false;
   }
}

function sc_string2list( s ) {
   var res = null;
   for( var i = s.length - 1; i >= 0; i-- )
      res = sc_cons( new sc_Char( s.charAt( i ) ), res );
   return res;
}

function sc_list2string( l ) {
   var a = new Array();
   while( l !== null ) {
      a.push( l.__hop_car.val );
      l = l.__hop_cdr;
   }
   return String.prototype.concat.apply( "", a );
}

function sc_isStringPrefix( cs1, cs2 ) {
   return cs2.indexOf( cs1 ) === 0;
}

function sc_isStringSuffix( cs1, cs2 ) {
   var tmp = cs2.lastIndexOf( cs1 );
   return tmp !== false && tmp >= 0 && tmp === cs2.length - cs1.length;
}

function sc_stringSkip( s, cset, start ) {
   var set = (cset instanceof sc_Char) ? sc_char2string( cset ) : cset;

   for( var i = start; i < s.length; i++ ) {
      if( set.indexOf( s.charAt( i ) ) < 0 ) {
	 return i;
      }
   }

   return false;
}
#endif
#endif

/*---------------------------------------------------------------------*/
/*    Chars                                                            */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
function sc_Char( c ) {
   this.val = c;
}

var SC_NUMBER_CLASS = "0123456789";
var SC_WHITESPACE_CLASS = ' \r\n\t\f';
var SC_LOWER_CLASS = 'abcdefghijklmnopqrstuvwxyz';
var SC_UPPER_CLASS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

function sc_isCharOfClass( c, cl ) {
   return (cl.indexOf( c ) != -1);
}

function sc_isCharAlphabetic(c) {
   return sc_isCharOfClass( c.val, SC_LOWER_CLASS ) ||
      sc_isCharOfClass( c.val, SC_UPPER_CLASS );
}
#endif

#if HOP_RTS_DEBUG
var SC_NUMBER_CLASS = "0123456789";
var SC_WHITESPACE_CLASS = ' \r\n\t\f';
var SC_LOWER_CLASS = 'abcdefghijklmnopqrstuvwxyz';
var SC_UPPER_CLASS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

function sc_char2string(c) {
   return c.val;
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

#if HOP_RTS_DEBUG
function sc_vector2list( a ) {
   var res = null;
   for( var i = a.length-1; i >= 0; i-- )
      res = sc_cons( a[ i ], res );
   return res;
}

function sc_isVectorEqual( v1, v2, comp ) {
    if( v1.length !== v2.length ) return false;
    for( var i = 0; i < v1.length; i++ )
	if( !comp( v1[ i ], v2[ i ] ) ) return false;
    return true;
}

#endif

function sc_isVector( v )  {
   if( v instanceof Array ) {
      if( "Float32Array" in window ) {
	 return !(v instanceof Float32Array);
      } else {
	 return true;
      }
   } else {
      return false;
   }
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

function sc_isEqual( o1, o2 ) {
   return ((o1 === o2) ||
	   (sc_isPair( o1 ) && sc_isPair( o2 )
	    && sc_isPairEqual( o1, o2, sc_isEqual ) ) ||
	   (sc_isVector( o1 ) && sc_isVector( o2 )
	    && sc_isVectorEqual( o1, o2, sc_isEqual ) ) ||
	   false );
}

function sc_Values( values ) {
   this.values = values;
}

function sc_values() {
   if( arguments.length === 1 )
      return arguments[ 0 ];
   else
      return new sc_Values( arguments );
}

function sc_callWithValues( producer, consumer ) {
   var produced = producer();
   if( produced instanceof sc_Values )
      return consumer.apply( null, produced.values );
   else
      return consumer( produced );
}

function sc_raise( val ) {
   throw val;
}

function sc_withHandlerLambda( handler, body ) {
   try {
      return body();
   } catch( e ) {
      if( !e._internalException )
	 return handler( e );
      else
	 throw e;
   }
}

function sc_plus2( x, y ) {
   var res = x + y;
   return res;
}

function sc_minus( x ) {
   if( arguments.length === 1 ) {
      return -x;
   } else {
      var res = x;
      for( var i = 1; i < arguments.length; i++ ) {
	 res -= arguments[ i ];
      }
      return res;
   }
}

function sc_minus2( x, y ) {
   var res = x - y;
   return res;
}

function sc_multi2( x, y ) {
   var res = x * y;
   return res;
}

function sc_isInteger( n ) {
    return (parseInt( n ) === n);
}

var sc_lambda = undefined;
#endif
#endif
