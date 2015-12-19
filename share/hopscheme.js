/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/share/hopscheme.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 24 14:35:05 2007                          */
/*    Last change :  Thu Dec 10 12:03:52 2015 (serrano)                */
/*    Copyright   :  2007-15 Manuel Serrano                            */
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

