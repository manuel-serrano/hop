/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/share/hopscheme.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 24 14:35:05 2007                          */
/*    Last change :  Thu Mar  3 08:32:50 2016 (serrano)                */
/*    Copyright   :  2007-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop adpatation of the scheme2js runtime.                         */
/*=====================================================================*/

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

sc_Char.prototype.hop_bigloo_serialize = function() {
   return 'a' + hop_serialize_word( this.val.charCodeAt( 0 ) );
};

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
/*    lists ...                                                        */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
function sc_Pair( car, cdr ) {
   this.car = car;
   this.cdr = cdr;
}

function sc_isPair( p ) {
   return p instanceof sc_Pair;
}

function sc_car( p ) {
   return p.car;
}

function sc_cdr( p ) {
   return p.cdr;
}

function sc_cons( car, cdr ) {
   return new sc_Pair( car, cdr );
}

function sc_list() {
   var res = null;
   var a = arguments;
   for( var i = a.length-1; i >= 0; i-- ) {
      res = new sc_Pair(a[i], res);
   }
   return res;
}
#endif

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

function sc_isKeyword( s ) {
   return (typeof s === "string") && (s.charAt(0) === sc_KEYWORD_PREFIX);
}

function sc_keyword2string( s ) {
   return s.slice( 1 );
}

function sc_string2keyword( s ) {
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

/*---------------------------------------------------------------------*/
/*    Objects                                                          */
/*---------------------------------------------------------------------*/
#if HOP_JAVASCRIPT
function sc_Object() {
}
#endif
