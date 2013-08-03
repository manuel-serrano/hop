/*=====================================================================*/
/*    serrano/prgm/project/hop/2.5.x/share/hopscheme.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 24 14:35:05 2007                          */
/*    Last change :  Fri Jul 26 09:13:38 2013 (serrano)                */
/*    Copyright   :  2007-13 Manuel Serrano                            */
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
      res += hop_bigloo_serialize_context( l.car );
      l = l.cdr;
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

sc_Vector.prototype.hop_typeof = function() {
   return "vector";
};

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

Boolean.prototype.hop_typeof = function() {
   return "bbool";
};

String.prototype.hop_typeof = function() {
    return hop_typeof(this.toString());
};

sc_Char.prototype.hop_typeof = function() {
   return "bchar";
};

