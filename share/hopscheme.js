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
