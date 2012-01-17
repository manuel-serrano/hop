/*=====================================================================*/
/*    serrano/prgm/project/hop/2.3.x/share/hop-serialize.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 20 07:55:51 2007                          */
/*    Last change :  Tue Jan 17 10:25:10 2012 (serrano)                */
/*    Copyright   :  2007-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP serialization (Bigloo compatible).                           */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_serialize_context ...                                        */
/*---------------------------------------------------------------------*/
var hop_serialize_context = new Object();
hop_serialize_context.def = 0;
hop_serialize_context.ref = 0;
hop_serialize_context.active = false;
hop_serialize_context.key = 1;

/*---------------------------------------------------------------------*/
/*    object serialization                                             */
/*---------------------------------------------------------------------*/
sc_Object.prototype.hop_bigloo_serialize = hop_bigloo_serialize_sc_object;

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize ...                                         */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize( item ) {
   if( hop_serialize_context.active ) {
      return hop_bigloo_serialize_context( item );
   } else {
      hop_serialize_context.active = true;
      hop_serialize_context.ref = 0;
      hop_serialize_context.def = 0;
      hop_serialize_context.key++;

      var str = hop_bigloo_serialize_context( item );

      hop_serialize_context.active = false;
      
      return "c" + hop_serialize_number( hop_serialize_context.def ) + str;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_context ...                                 */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_context( item ) {
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

   if( item === undefined )
      return ";";
   
   if( item === null )
      return ".";

   if( "hop_serialize_context_key" in item &&
       item[ "hop_serialize_context_key" ] === hop_serialize_context.key ) {
      hop_serialize_context.ref++;
      return "%23" + hop_serialize_word( item.hop_serialize_context_def );
   }

   if( item instanceof Array )
      return hop_serialize_array( item );
   
   if( item instanceof Date )
      return hop_serialize_date( item );

   if( item && ("hop_bigloo_serialize" in item) ) {
      return hop_bigloo_serialize_custom( item );
   }

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

   if( sc_isClass( item ) )
      return hop_bigloo_serialize_sc_class( item );
      
   return hop_bigloo_serialize_alist( item );
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_custom ...                                  */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_custom( item ) {
   item.hop_serialize_context_key = hop_serialize_context.key;
   item.hop_serialize_context_def = hop_serialize_context.def++;

   return "=" + hop_serialize_word( item.hop_serialize_context_def ) +
      item.hop_bigloo_serialize();
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_object ...                                  */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_object() {
   var item = this;
   var classname = hop_demangle( item.hop_classname );
   var classfields = item.hop_classfields;
   var str = "|" + "%27" + hop_serialize_string( '%22', classname );
   var args = "";
   var len = 1;

   item.hop_serialize_context_key = hop_serialize_context.key;
   item.hop_serialize_context_def = hop_serialize_context.def++;

   for( p in item ) {
      if( p !== "hop_bigloo_serialize" &&
	  p !== "hop_classname" &&
	  p !== "hop_serialize_context_key" &&
	  p !== "hop_serialize_context_def" &&
	  p !== "hop_classhash" ) {
	 len++;
	 args += hop_bigloo_serialize( item[ p ] );
      }
   }

   str += hop_serialize_word( len );
   str += hop_serialize_boolean( false );
   str += args;
   str += hop_bigloo_serialize( item.hop_classhash );

   str = "=" +  hop_serialize_word( item.hop_serialize_context_def ) + str;

   return str;
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_sc_object ...                               */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_sc_object() {
   var item = this;
   var clazz = sc_object_class( item );
   var classname = sc_symbol2jsstring( sc_class_name( clazz ) );
   var str = "|" + "%27" + hop_serialize_string( '%22', classname );
   var args = "";
   var fields = sc_class_all_fields( clazz );
   var len = 1 + fields.length;

   item.hop_serialize_context_key = hop_serialize_context.key;
   item.hop_serialize_context_def = hop_serialize_context.def++;

   for( var i = 0; i < fields.length; i++ ) {
      args += hop_bigloo_serialize( fields[ i ].sc_getter( item ) );
   }

   str += hop_serialize_word( len );
   str += hop_serialize_boolean( false );
   str += args;
   str += hop_bigloo_serialize( sc_class_hash( clazz ) );

   str = "=" +  hop_serialize_word( item.hop_serialize_context_def ) + str;

   return str;
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_sc_class ...                                */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_sc_class( clazz ) {
   var classname = sc_symbol2jsstring( sc_class_name( clazz ) );
   return "k" + hop_serialize_string( '%22', classname );
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

         if( (c >= 46) && (c < 127) ) {
	    if( c == 92 )
	       // Chrome patch:
	       // Chrome (at least Chrome <= 12) escapes \ characters when
	       // sending the request via an XHR. The consequence is the
	       // client-side DIGEST authentication is computed on a different
	       // string from the one actually sent to the client. As a
	       // workaround we explicitly escape \ character using the URL
	       // escaping mechanism.
	       rw += "%5c";
	    else
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
/*    -------------------------------------------------------------    */
/*    MS 25may2011: I'm not sure this is the correct way of encoding   */
/*    strings. In the current implementation, the UCS2 is transformed  */
/*    into a uri-encoded string. This works fine for sending the       */
/*    value to the server. The problem is when that encoding is used   */
/*    by hop_obj_to_string. That function will apply decodeURI to the  */
/*    result of the encoding which will change the length of the       */
/*    encoded character. Imaging a string which contains the two       */
/*    latin character e' such as te'. In UCS2, this string has a       */
/*    length of 2, encoded in UTF8 it has a length of 3, thus          */
/*    the function hop_serialize_string will strink the encoded        */
/*    string. So the serialization has marked a string of length 3     */
/*    but the actual string produced by obj_to_string only contains    */
/*    a string of length 2! For the decoding to work, the decoder has  */
/*    to be aware that the substring it extracts might be larger than  */
/*    the expected value. This is implemented by a loop inside         */
/*    the decoding of the string (see read_string).                    */
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

   item.hop_serialize_context_key = hop_serialize_context.key;
   item.hop_serialize_context_def = hop_serialize_context.def++;

   for( i = 0; i < l; i++ ) {
      ra += hop_bigloo_serialize( item[ i ] );
   }

   return "=" + hop_serialize_word( item.hop_serialize_context_def ) + ra;
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
   var s = hop_bigloo_serialize( item );

   try {
      return decodeURIComponent( s );
   } catch( e ) {
      /* decoding has hitted an illegal UTF-8 surrogate, deocde by hand */
      var i = 0;
      var l = s.length;
      var r = "";

      while( i < l ) {
	 var j = s.indexOf( '%', i );

	 if( j == -1 ) {
	    return r + s.substring( i );
	 } else {
	    if( j > l - 3 )
	       return r + s.substring( i );
	    
	    if( j > i )
	       r += s.substring( i, j );
	    
	    r += string_hex_intern( s.substring( j + 1, j + 3 ) );

	    i = j + 3;
	 }
      }

      return r;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_unserialize ...                                       */
/*---------------------------------------------------------------------*/
/*** META ((export string->obj) (arity #t)) */
function hop_string_to_obj( s ) {
   var pointer = 0;
   var definitions = [];
   var defining = -1;

   function substring( s, beg, end ) {
      if( s instanceof Array ) {
	 return String.fromCharCode.apply( null, s.slice( beg, end ) );
      } else {
	 return hop_uint8array_to_string( s.subarray( beg, end ) );
      }
   }

   function utf8substring( s, beg, end ) {
      var codes = new Array();
      var j = 0;

      while( beg < end ) {
	 var code = s[ beg++ ];
	 
	 if( code < 128 ) {
	    codes[ j++ ] = code;
	 } else {
	    var code2 = s[ beg++ ];
	    
	    if( code < 224 ) {
	       codes[ j++ ] = ((code - 192) << 6) + (code2 - 128);
	    } else {
	       var code3 = s[ beg++ ];

	       if( code < 240 ) {
		  codes[ j++ ] = ((code - 224) << 12)
		     + ((code2 - 128) << 6) + (code3 - 128);
	       } else {
		  code4 = s[ beg++ ];

		  codes[ j++ ] = ((code - 240) << 18)
		     + ((code2 - 128) << 12)
		     + ((code3 - 128) << 6)
		     + (code4 - 128);
	       }
	    }
	 }
      }

      return String.fromCharCode.apply( null, codes );
   }
   
   function read_integer( s ) {
      return read_size( s );
   }

   function read_float( s ) {
      var szf = read_size( s );
      var res = substring( s, pointer, pointer + szf );
      
      pointer += szf;

      return +res;
   }
   
   function read_char( s ) {
      new sc_Char( String.fromCharCode( read_integer( s ) ) );
   }

   function read_word( s, sz ) {
      var acc = 0;

      for( var iw = 0; iw < sz; iw++ ) {
	 acc = (256 * acc) + s[ pointer++ ];
      }

      return acc;
   }

   function read_long_word( s, szlw ) {
      return read_word( szlw );
   }


   function read_size( s ) {
      var szs = s[ pointer++ ];
      return read_word( s, szs );
   }
   
   function read_string( s ) {
      var ulen = read_size( s );
      var sz = ((ulen + pointer) > s.length) ? s.length - pointer : ulen;
      var res = utf8substring( s, pointer, pointer + sz );
      
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

   function read_structure() {
      var old_defining = defining;
      var key, sz, res;
      
      defining = -1;
      
      key = read_item();
      sz = read_item();
      res = sc_makeStruct( key );

      if( old_defining >= 0 )
	 definitions[ old_defining ] = res;

      for( var i = 0; i < sz; i++ ) {
	 sc_setStructFieldBang( res, key, i, read_item() );
      }

      return res;
   }

   function read_object() {
      var old_defining = defining;
      var key, sz, clazz, fields;
      var res;
      
      defining = -1;
      
      key = read_item();
      sz = read_item();
      clazz = sc_class_exists( key );
      cinfo = read_item();
      sz--;

      if( clazz ) {
	 res = sc_class_allocator( clazz )();
	 fields = sc_class_all_fields( clazz );
      
	 if( old_defining >= 0 )
	    definitions[ old_defining ] = res;

	 for( var i = 0; i < sz; i++ ) {
	    fields[ i ].sc_setter( res, read_item() );
	 }

	 if( read_item() === clazz.sc_hash ) {
	    return res;
	 } else {
	    sc_error( "string->obj", "corrupted class", key );
	 }
      } else {
	 res = new Object();

	 for( var i = 0; i < sz; i++ ) {
	    res[ cinfo[ i ] ] = read_item();
	 }
	 
	 return res;
      }
   }

   function read_custom_object() {
      var old_defining = defining;
      var obj, hash, unserializer;
      var res;
      
      defining = -1;
      
      obj = read_item();
      hash = read_item();
      unserializer = hop_find_class_unserializer( hash );

      res = unserializer( obj );
      
      if( old_defining >= 0 ) {
	 definitions[ old_defining ] = res;
      }
      
      clazz = sc_object_class( res );

      if( !(res instanceof sc_Object) || (hash === clazz.sc_hash) ) {
	 return res;
      } else {
	 sc_error( "string->obj", "corrupted custom class", hash );
      }
   }
   
   function read_elong( sz ) {
      return read_word( s, sz );
   }
      
   function read_llong( sz ) {
      return read_word( s, sz );
   }

   function read_unsupported( type ) {
      return sc_error( "hop_js_to_object", 
		       type + " unsupported on client-side",
		       s );
   }

   function read_date() {
      return seconds_date( parseInt( read_string( s ), 10 ) );
   }

   function read_class() {
      var cname = read_symbol();
      var cinfo = read_item();
     
      return sc_class_exists( cname ) || cinfo;
   }

   function string_to_array( s ) {
      var a = hop_config.uint8array ?
	 new Uint8Array( s.length ) : new Array( s.length );

      for( var i = 0, len = s.length; i < len; ++i ) {
	 a[ i ] = (s.charCodeAt(i)) & 0xff;
      }

      return a;
   }
   
   function read_item() {
      switch( s[ pointer++ ] ) {
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
         case 0x22 /* " */: return read_string( s );
         case 0x55 /* U */: return read_string( s );
	 case 0x5b /* [ */: return read_vector( read_size( s ) );
	 case 0x28 /* ( */: return read_list( read_size( s ) );
	 case 0x5e /* ^ */: return read_extended_list( read_size( s ) );
         case 0x7b /* { */: return read_structure();
         case 0x7c /* | */: return read_object();
         case 0x4f /* O */: return read_custom_object();
	 case 0x66 /* f */: return read_float( s );
	 case 0x2d /* - */: return -read_integer( s );
         case 0x45 /* E */: return read_elong( read_size( s ) );
         case 0x4c /* L */: return read_llong( read_size( s ) );
         case 0x64 /* d */: return read_date();
         case 0x6b /* k */: return read_class();
         case 0x72 /* r */: return sc_pregexp( read_string( s ) );
         case 0x68 /* h */: return read_unsupported( "homogeneous-vector" );
	 case 0x56 /* V */: return read_unsupported( "typed-vector" );
	 case 0x21 /* ! */: return read_unsupported( "cell" );
         case 0x75 /* u */: return read_unsupported( "ucs2" );
	 case 0x7a /* z */: return read_unsupported( "bignum" );
         case 0x2b /* + */: return read_unsupported( "custom" );
	 case 0x77 /* w */: return read_unsupported( "weak-ptr" );
         case 0x74 /* t */: return read_unsupported( "tagged vectors" );
         case 0x70 /* p */: return read_unsupported( "process" );
         case 0x65 /* e */: return read_unsupported( "process" );
         case 0x6f /* o */: return read_unsupported( "opaque" );
	 default: pointer--; return read_integer( s );
      }
   }

   if( (typeof s) === "string" ) {
      s = string_to_array( s );
   }
   
   if( s[ pointer ] === 0x63 /* c */ ) {
      pointer++;
      definitions = new Array( read_size( s ) );
   }

   return read_item();
}

/*---------------------------------------------------------------------*/
/*    hop_custom_object_regexp ...                                     */
/*---------------------------------------------------------------------*/
var hop_custom_object_regexp =
   new RegExp( "hop_create_encoded_element[(][ ]*\"([^\"]*)\"[ ]*[)]" );
	       
/*---------------------------------------------------------------------*/
/*    hop_find_class_unserializer ...                                  */
/*---------------------------------------------------------------------*/
function hop_find_class_unserializer( hash ) {
   return function( o ) {
      if( typeof( o ) === "string" ) {
	 var m = o.match( hop_custom_object_regexp );

	 if( m ) {
	    return hop_create_element( decodeURIComponent( m [ 1 ] ) );
	 } else {
	    return sc_error( "string->obj", "Cannot find custom class unserializer", o );
	 }
      } else {
	 return sc_error( "string->obj", "Cannot find custom class unserializer", hash );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_js_to_object ...                                             */
/*---------------------------------------------------------------------*/
function hop_js_to_object( cname, hash, o ) {
   var klass = sc_class_exists( sc_string2symbol( sc_jsstring2string( cname ) ) );

   if( !klass ) {
      o.hop_bigloo_serialize = hop_bigloo_serialize_object;
      o.hop_classname = cname;
      o.hop_classhash = hash;

      return o;
   } else {
      if( sc_class_hash( klass ) !== hash ) {
	 sc_error( "hop_js_to_object", "incomptabile class versions", cname );
      } else {
	 o.__proto__ = klass.prototype;
	 o.__proto__.constructor = klass;

	 return o;
      }
   }
}
