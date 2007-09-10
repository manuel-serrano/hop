/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hopscheme.js                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 24 14:35:05 2007                          */
/*    Last change :  Thu Sep  6 13:23:43 2007 (serrano)                */
/*    Copyright   :  2007 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hop adpatation of the scheme2js runtime.                         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Serialization                                                    */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_pair( l ) {
   var res = "";
   var len = 0;

   while (sc_isPair( l ) ) {
      res += hop_bigloo_serialize( l.car );
      l = l.cdr;
      len++;
   }

   if( l == null ) {
      return hop_serialize_word( len + 1 ) + res + ".";
   } else {
      return hop_serialize_word( len + 1 ) + res + hop_bigloo_serialize( l );
   }
}

sc_Pair.prototype.hop_bigloo_serialize = function() {
   return '(' + hop_bigloo_serialize_pair( this );
}

sc_String.prototype.hop_bigloo_serialize = function() {
   return hop_bigloo_serialize( this.val );
}

/*---------------------------------------------------------------------*/
/*    find-runtime-type                                                */
/*---------------------------------------------------------------------*/
sc_Pair.prototype.hop_find_runtime_type = function() {
   return "pair";
}

sc_String.prototype.hop_find_runtime_type = function() {
   if( sc_isSymbol_immutable( s ) ) {
      return "symbol"
   } else {
      return "bstring";
   }
}

sc_Vector.prototype.hop_find_runtime_type = function() {
   return "vector";
}

sc_Struct.prototype.hop_find_runtime_type = function() {
   return "struct";
}

sc_Keyword.prototype.hop_find_runtime_type = function() {
   return "keyword";
}

sc_OutputPort.prototype.hop_find_runtime_type = function() {
   return "output-port";
}

sc_StringOutputPort_immutable.prototype.hop_find_runtime_type = function() {
   return "output-port";
}

sc_StringOutputPort_mutable.prototype.hop_find_runtime_type = function() {
   return "output-port";
}

sc_GenericOutputPort.prototype.hop_find_runtime_type = function() {
   return "output-port";
}

sc_InputPort.prototype.hop_find_runtime_type = function() {
   return "input-port";
}

Boolean.prototype.hop_find_runtime_type = function() {
   return "bbool";
}

sc_Char.prototype.hop_find_runtime_type = function() {
   return "bchar";
}

