/*=====================================================================*/
/*    serrano/prgm/project/hop/2.2.x/share/hop-serialize.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 20 07:55:51 2007                          */
/*    Last change :  Sat Jan 15 19:47:41 2011 (serrano)                */
/*    Copyright   :  2007-11 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP serialization (Bigloo compatible).                           */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize ...                                         */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize( item ) {
   var tname = typeof item;

   if( (item instanceof String) || (tname == "string") ) {
      if( sc_isSymbol( item ) ) {
	 return "%27"
	    + hop_serialize_string( '%22', sc_symbol2jsstring( item ) );
      } else if( sc_isKeyword( item ) ) {
	 return "%3a"
	    + hop_serialize_string( '%22', sc_keyword2jsstring( item ) );
      } else {
	 return hop_serialize_string( '%22', item );
      }
   }

   if( tname == "number" )
      return hop_serialize_number( item );
      
   if( (item instanceof Boolean) || (tname == "boolean") )
      return hop_serialize_boolean( item );
      
   if( item instanceof Array )
      return hop_serialize_array( item );
   
   if( item === undefined )
      return ";";
   
   if( item === null )
      return ".";

   if( item instanceof Date )
      return hop_serialize_date( item );

   if( (item instanceof Object) && ("hop_bigloo_serialize" in item) )
      return item.hop_bigloo_serialize();
   
   if( (HTMLCollection != undefined) && (item instanceof HTMLCollection) )
      return hop_serialize_array( item );
      
   if( (HTMLInputElement != undefined) && (item instanceof HTMLInputElement) )
      return hop_bigloo_serialize( item.value );

   if( (HTMLTextAreaElement != undefined) && (item instanceof HTMLTextAreaElement) )
      return hop_bigloo_serialize( item.value );

   if( (HTMLSelectElement != undefined) && (item instanceof HTMLSelectElement) )
      return hop_bigloo_serialize( item.value );

   if( (item.callee != undefined) && (item.length > -1) )
      return hop_serialize_array( item );

   if( hop_is_html_element( item ) )
      return hop_serialize_html( item );

   return hop_bigloo_serialize_alist( item );
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_object ...                                  */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_object() {
   var o = this;
   var classname = hop_demangle( o.hop_classname );
   var classfields = o.hop_classfields;
   var str = "|" + "%27" + hop_serialize_string( '%22', classname );
   var args = "";
   var len = 1;

   for( var i = 0; i < classfields.length; i++ ) {
      len++;
      args += hop_bigloo_serialize( o[ classfields[ i ] ] );
   }

   str += hop_serialize_word( len );
   str += hop_serialize_boolean( false );
   str += args;
   str += hop_bigloo_serialize( o.hop_classhash );

   return str;
}

/*---------------------------------------------------------------------*/
/*    hop_size_of_word ...                                             */
/*---------------------------------------------------------------------*/
function hop_size_of_word( word ) {
   var s = 0;

   while( word > 0 ) {
      s++;
      word >>= 8;
   }

   return s;
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_word ...                                           */
/*---------------------------------------------------------------------*/
function hop_serialize_word( word ) {
   var s = hop_size_of_word( word );

   if( s == 0 ) {
      return "%00";
   } else {
      var i1 = (s >> 4);
      var i2 = (s & 0xf);
      var c1 = i1 + ((i1 < 10) ? 48 : 55);
      var c2 = i2 + ((i2 < 10) ? 48 : 55);
      var rw = String.fromCharCode( 37, c1, c2 );

      s--;
      while( s >= 0 ) {
         var c = ((word >> (s << 3)) & 0xff);

         if( (c < 127) && (c >= 46) ) {
	    rw += String.fromCharCode( c );
         } else {
            var i1 = (c >> 4);
            var i2 = (c & 0xf);
            var c1 = i1 + ((i1 < 10) ? 48 : 55);
            var c2 = i2 + ((i2 < 10) ? 48 : 55);
            
            rw += String.fromCharCode( 37, c1, c2 );
         }
         
         s--;
      }

      return rw;
   }
}

/*---------------------------------------------------------------------*/
/*    ucs2_to_utf8 ...                                                 */
/*---------------------------------------------------------------------*/
/*** META ((export ucs2-string->utf8-string) (arity #t)) */
function ucs2_to_utf8( s ) {
   var len = s.length;

   for( var i = 0; i < len; i++ ) {
      var c = s.charCodeAt( i );
      if( c >= 128 ) {
	 /* we got one non-ascii, we have to convert */
	 var utf = s.substring( 0, i );

	 for( ; i< len; i++, c = s.charCodeAt( i ) ) {
	    if( c < 128 ) {
	       utf += String.fromCharCode( c );
	    } else {
	       if( (c > 127) && (c < 2048) ) {
		  utf += String.fromCharCode((c >> 6) | 192);
		  utf += String.fromCharCode((c & 63) | 128);
	       } else {
		  utf += String.fromCharCode((c >> 12) | 224);
		  utf += String.fromCharCode(((c >> 6) & 63) | 128);
		  utf += String.fromCharCode((c & 63) | 128);
	       }
	    }
	 }

	 return utf;
      }
   }

   return s;
}

/*---------------------------------------------------------------------*/
/*    utf_length ...                                                   */
/*---------------------------------------------------------------------*/
function utf_length( s ) {
   var len = s.length;
   var res = len;

   for( var i = 0; i < len; i++ ) {
      var c = s.charCodeAt( i );
      
      if( c >= 128 ) {
	 if( (c > 127) && (c < 2048) ) {
	    res++;
	 } else {
	    res += 2;
	 }
      }
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_string ...                                         */
/*---------------------------------------------------------------------*/
function hop_serialize_string( mark, item ) {
   return mark +
      hop_serialize_word( utf_length( item ) ) +
      encodeURIComponent( item );
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_number ...                                         */
/*---------------------------------------------------------------------*/
function hop_serialize_number( item ) {
   var sitem = item + "";

   if( sitem.indexOf( "." ) == -1 ) {
      if( item < 0 ) {
	 if( item >= -536870912 ) {
	    return '-' + hop_serialize_word( -item );
	 } else if( item >= 2147483648 ) {
	    return hop_serialize_string( 'E', item + "" );
	 } else {
	    return hop_serialize_string( 'L', item + "" );
	 }
      } else {
	 if( item <= 536870911 ) {
	    return hop_serialize_word( item );
	 } else if( item <= 2147483647 ) {
	    return hop_serialize_string( 'E', item + "" );
	 } else {
	    return hop_serialize_string( 'L', item + "" );
	 }
      }
   } else {
      return 'f' + hop_serialize_word( sitem.length ) + sitem;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_boolean ...                                        */
/*---------------------------------------------------------------------*/
function hop_serialize_boolean( item ) {
   return item ? 'T' : 'F';
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_array ...                                          */
/*---------------------------------------------------------------------*/
function hop_serialize_array( item ) {
   var l = item.length;
   var ra = '[' + hop_serialize_word( l );
   var i = 0;

   for( i = 0; i < l; i++ ) {
      ra += hop_bigloo_serialize( item[ i ] );
   }

   return ra;
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_date ...                                           */
/*---------------------------------------------------------------------*/
function hop_serialize_date( item ) {
   var utc = Date.UTC( item.getUTCFullYear(),
		       item.getUTCMonth(),
		       item.getUTCDate(),
		       item.getUTCHours(),
		       item.getUTCMinutes(),
		       item.getUTCSeconds() ) + "";
   var ms = utc.substring( 0, utc.length - 3 );

   return 'd' + hop_serialize_word( ms.length ) + encodeURIComponent( ms );
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_html ...                                           */
/*---------------------------------------------------------------------*/
function hop_serialize_html( item ) {
   if( "outerHTML" in item ) {
      return hop_serialize_string( '%22', item.outHTML );
   } else {
      if( item.nodeType == 1 ) {
	 var str = "<" + item.tagName + " id='" + item.id + "' "
	    + (item.className ? ("class='" + item.className + "'") : "")
	    + ">" + item.innerHTML + "</" + item.tagName + ">";
	 return hop_serialize_string( '%22', str );
      } else {
	 if( item.nodeType == 3 ) {
	    return hop_serialize_string( '%22', item.nodeValue );
	 } else {
	    return hop_bigloo_serialize( "#<" + tname + ">" );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_alist ...                                          */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_alist( item ) {
   var alist = null;
   
   for( p in item ) {
      var k = sc_jsstring2keyword( p );
      alist = sc_cons( sc_cons( k, sc_cons( item[ p ] ) ), alist );
   }

   return hop_bigloo_serialize( alist );
}
   
/*---------------------------------------------------------------------*/
/*    hop_obj_to_string ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export obj->string) (arity #t)) */
function hop_obj_to_string( item ) {
   return decodeURIComponent( hop_bigloo_serialize( item ) );
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_unserialize ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export string->obj) (arity #t)) */
function hop_string_to_obj( s ) {
   var pointer = 0;
   var definitions = [];
   var defining = -1;

   function read_integer( s ) {
      return read_size( s );
   }

   function read_float( s ) {
      var szf = read_size( s );
      var res = s.substring( pointer, pointer + szf );
      pointer += szf;

      return +res;
   }
   
   function read_char( s ) {
      new sc_Char(String.fromCharCode(n));
   }

   function read_word( s, sz ) {
      var acc = 0;

      for( var iw = 0; iw < sz; iw++ ) {
	 acc = (256 * acc) + s.charCodeAt( pointer++ );
      }

      return acc;
   }

   function read_long_word( s, szlw ) {
      return read_word( szlw );
   }


   function read_size( s ) {
      var szs = s.charCodeAt( pointer++ );
      return read_word( s, szs );
   }
   
   function read_string( s ) {
      var sz = read_size( s );
      var res = s.substring( pointer, pointer + sz );

      if( defining >= 0 ) {
	 definitions[ defining ] = res;
	 defining = -1;
      }
      pointer += sz;

      return res;
   }

   function read_definition() {
      defining = read_item();
      return read_item();
   }

   function read_reference() {
      return definitions[ read_item() ];
   }

   function read_symbol() {
      return sc_jsstring2symbol( read_item() );
   }

   function read_keyword() {
      return sc_jsstring2keyword( read_item() );
   }

   function read_cnst() {
      switch( read_integer( s ) ) {
	 default: alert( "read_cnst: not implemented: " + s );
      }
   }

   function read_vector( sz ) {
      var res = sc_makeVector( sz );

      if( defining >= 0 ) {
	 definitions[ defining ] = res;
	 defining = -1;
      }

      for( var iv = 0; iv < sz; iv++ ) {
	 res[ iv ] = read_item();
      }

      return res;
   }

   function read_list( sz ) {
      var res = sc_cons( null, null );
      var hd = res;

      if( defining >=0 ) {
	 definitions[ defining ] = res;
	 defining = -1;
      }

      for( var i = 0; i < (sz - 2); i++, hd = hd.cdr ) {
	 hd.car = read_item();
	 hd.cdr = sc_cons( null, null );
      }

      hd.car = read_item();
      hd.cdr = read_item();

      return res;
   }

   function read_extended_list( sz ) {
      var res = sc_cons( null, null );
      var hd = res;

      if( defining >= 0 ) {
	 definitions[ defining ] = res;
	 defining = -1;
      }

      for( var i = 0; i < (sz - 2); i++, hd = hd.cdr ) {
	 hd.car = read_item();
	 // skip the cer
	 read_item();
	 hd.cdr = sc_cons( null, null );
      }

      hd.car = read_item();
      // skip the cer
      read_item();
      hd.cdr = read_item();

      return res;
   }

   function read_item() {
      switch( s.charCodeAt( pointer++ ) ) {
	 case 0x3d /* = */: return read_definition();
	 case 0x23 /* # */: return read_reference();
	 case 0x27 /* ' */: return read_symbol();
	 case 0x3a /* : */: return read_keyword();
	 case 0x61 /* a */: return read_char( s );
	 case 0x46 /* F */: return false;
	 case 0x54 /* T */: return true;
	 case 0x3b /* ; */: return undefined;
	 case 0x2e /* . */: return null;
	 case 0x3c /* < */: return read_cnst();
	 case 0x22 /* " */: return read_string( s )
	 case 0x28 /* ( */: return read_list( read_size( s ) );
	 case 0x53 /* ^ */: return read_extended_list( read_size( s ) );
	 case 0x5b /* [ */: return read_vector( read_size( s ) );
	 case 0x66 /* f */: return read_float( s );
	 case 0x2d /* - */: return -read_integer( s );
	 default: pointer--; return read_integer( s );
      }
   }

   if( s.charAt( pointer ) == 'c' ) {
      pointer++;
      definitions = new Array( read_size( s ) );
   }

   return read_item();
}

/*---------------------------------------------------------------------*/
/*    unjson ...                                                       */
/*---------------------------------------------------------------------*/
var unjson = {
   "pair": function( o ) {
      return sc_cons( hop_unjson( o.car ), hop_unjson( o.cdr ) );
   }
}
 
/*---------------------------------------------------------------------*/
/*    hop_unjson ...                                                   */
/*---------------------------------------------------------------------*/
function hop_unjson( o ) {
   var tname = typeof o;

   if( ((o instanceof String) || (tname == "string")) ||
       ((typeof o) == "number") ||
       (o instanceof Boolean) || (tname == "boolean") ||
       (o === null) ) {
      return o;
   }

   if( o instanceof Array ) {
      for( var i = 0; i < o.length; i++ ) {
	 o[ i ] = hop_unjson( o [ i ] );
      }

      return o;
   }
   
   if( "__uuid" in o )
      return unjson[ o.__uuid ]( o );
   else
      return o;
}


