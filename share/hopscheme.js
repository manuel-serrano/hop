/*=====================================================================*/
/*    serrano/prgm/project/hop/1.9.x/share/hopscheme.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 24 14:35:05 2007                          */
/*    Last change :  Mon Aug 18 08:11:15 2008 (serrano)                */
/*    Copyright   :  2007-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop adpatation of the scheme2js runtime.                         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_error ...                                                    */
/*---------------------------------------------------------------------*/
function hop_error( fun, exc, msg, svc ) {
   var emsg = exc ? exc.toString() : "???";

   if( exc ) {
      if( "message" in exc ) {
	 emsg = exc.message;
      } else {
	 if( "description" in exc ) {
	    emsg = exc.description;
	 }
      }
      
      if( "line" in exc ) {
	 emsg = emsg + " (line " + exc.line + ")";
      }
   }

   if( typeof svc === "string" || svc instanceof String )
      fun = fun + ", " + svc;
   
   alert( "*** ERROR " + fun + ": " + emsg + " -- " + msg );
   
   throw exc;
}
   
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
};

/*---------------------------------------------------------------------*/
/*    find-runtime-type                                                */
/*---------------------------------------------------------------------*/
sc_Pair.prototype.hop_find_runtime_type = function() {
   return "pair";
};

sc_Vector.prototype.hop_find_runtime_type = function() {
   return "vector";
};

sc_Struct.prototype.hop_find_runtime_type = function() {
   return "struct";
};

sc_OutputPort.prototype.hop_find_runtime_type = function() {
   return "output-port";
};

sc_StringOutputPort.prototype.hop_find_runtime_type = function() {
   return "output-port";
};

sc_GenericOutputPort.prototype.hop_find_runtime_type = function() {
   return "output-port";
};

sc_InputPort.prototype.hop_find_runtime_type = function() {
   return "input-port";
};

Boolean.prototype.hop_find_runtime_type = function() {
   return "bbool";
};

String.prototype.hop_find_runtime_type = function() {
    return hop_find_runtime_type(this.toString());
};

sc_Char.prototype.hop_find_runtime_type = function() {
   return "bchar";
};

