/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/share/hop-serialize.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 20 07:55:51 2007                          */
/*    Last change :  Wed Dec 16 21:36:56 2015 (serrano)                */
/*    Copyright   :  2007-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP serialization (Bigloo compatible).                           */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_serialize_context ...                                        */
/*---------------------------------------------------------------------*/
var hop_serialize_context = {
   def: 0,
   ref: 0,
   active: false,
   key: 1
 }

/*---------------------------------------------------------------------*/
/*    object serialization                                             */
/*---------------------------------------------------------------------*/
sc_Object.prototype.hop_bigloo_serialize = hop_bigloo_serialize_sc_object;
Int8Array.prototype.hop_bigloo_serialize = hop_bigloo_serialize_s8vector;
Uint8Array.prototype.hop_bigloo_serialize = hop_bigloo_serialize_u8vector;
Int16Array.prototype.hop_bigloo_serialize = hop_bigloo_serialize_s16vector;
Uint16Array.prototype.hop_bigloo_serialize = hop_bigloo_serialize_u16vector;
Int32Array.prototype.hop_bigloo_serialize = hop_bigloo_serialize_s32vector;
Uint32Array.prototype.hop_bigloo_serialize = hop_bigloo_serialize_u32vector;
Float32Array.prototype.hop_bigloo_serialize = hop_bigloo_serialize_f32vector;
Float64Array.prototype.hop_bigloo_serialize = hop_bigloo_serialize_f64vector;

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize ...                                         */
/*    -------------------------------------------------------------    */
/*    Returns a URL encode Bigloo serialization.                       */
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

      return "c" + hop_serialize_number( hop_serialize_context.def + 1 ) + str;
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
	    + hop_serialize_string( sc_symbol2jsstring( item ) );
      } else if( sc_isKeyword( item ) ) {
	 return "%3a"
	    + hop_serialize_string( sc_keyword2jsstring( item ) );
      } else {
	 return hop_serialize_string( item );
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

   if( item && ("hop_bigloo_serialize" in item) ) {
      return hop_bigloo_serialize_custom( item );
   }

   if( item instanceof Array )
      return hop_serialize_array( item );
   
   if( item instanceof Date )
      return hop_serialize_date( item );

   if( (HTMLCollection != undefined) && (item instanceof HTMLCollection) )
      return hop_serialize_array( item );
      
   if( (HTMLInputElement != undefined) && (item instanceof HTMLInputElement) )
      return hop_bigloo_serialize_context( item.value );

   if( (HTMLTextAreaElement != undefined) && (item instanceof HTMLTextAreaElement) )
      return hop_bigloo_serialize_context( item.value );

   if( (HTMLSelectElement != undefined) && (item instanceof HTMLSelectElement) )
      return hop_bigloo_serialize_context( item.value );

   if( (item.callee != undefined) && (item.length > -1) )
      return hop_serialize_array( item );

   if( hop_is_html_element( item ) )
      return hop_serialize_html( item );

   if( item instanceof Error ) {
      return hop_bigloo_serialize_error( item );
   }
   
   if( sc_isClass( item ) )
      return hop_bigloo_serialize_sc_class( item );
      
   return "X" + hop_bigloo_serialize_plist( item );
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
   var str = "|" + "%27" + hop_serialize_string( classname );
   var args = "";
   var len = 1;

   item.hop_serialize_context_key = hop_serialize_context.key;
   item.hop_serialize_context_def = hop_serialize_context.def++;

   for( var p in item ) {
      if( p !== "hop_bigloo_serialize" &&
	  p !== "hop_classname" &&
	  p !== "hop_circle_forced" &&
	  p !== "hop_serialize_context_key" &&
	  p !== "hop_serialize_context_def" &&
	  p !== "hop_classhash" ) {
	 len++;
	 args += hop_bigloo_serialize_context( item[ p ] );
      }
   }

   str += hop_serialize_word( len );
   str += hop_serialize_boolean( false );
   str += args;
   str += hop_bigloo_serialize_context( item.hop_classhash );

   str = "=" + hop_serialize_word( item.hop_serialize_context_def ) + str;

   return str;
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_sc_object ...                               */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_sc_object() {
   var item = this;
   var clazz = sc_object_class( item );
   var classname = sc_symbol2jsstring( sc_class_name( clazz ) );
   var str = "|" + "%27" + hop_serialize_string( classname );
   var args = "";
   var fields = sc_class_all_fields( clazz );
   var len = 1 + fields.length;

   if( "defineProperty" in Object ) {
      Object.defineProperty( item, "hop_serialize_context_key", {
	 value: hop_serialize_context.key,
	 enumerable: false,
	 configurable: true
      } );
      Object.defineProperty( item, "hop_serialize_context_def", {
	 value: hop_serialize_context.def++,
	 enumerable: false,
	 configurable: true
      } );
   } else {
      item.hop_serialize_context_key = hop_serialize_context.key;
      item.hop_serialize_context_def = hop_serialize_context.def++;
   }

   for( var i = 0; i < fields.length; i++ ) {
      args += hop_bigloo_serialize_context( fields[ i ].sc_getter( item ) );
   }

   str += hop_serialize_word( len );
   str += hop_serialize_boolean( false );
   str += args;
   str += hop_bigloo_serialize_context( sc_class_hash( clazz ) );

   str = "=" + hop_serialize_word( item.hop_serialize_context_def ) + str;

   return str;
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_hopframe ...                                */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_hopframe() {
   var item = this;
   var classname = "JsHopFrame";
   var hash = HopFrame.prototype.hash;
   var str = "O";

   if( "defineProperty" in Object ) {
      Object.defineProperty( item, "hop_serialize_context_key", {
	 value: hop_serialize_context.key,
	 enumerable: false,
	 configurable: true
      } );
      Object.defineProperty( item, "hop_serialize_context_def", {
	 value: hop_serialize_context.def++,
	 enumerable: false,
	 configurable: true
      } );
   } else {
      item.hop_serialize_context_key = hop_serialize_context.key;
      item.hop_serialize_context_def = hop_serialize_context.def++;
   }

   var obj = [ this.srv, this.path, this.args, this.options, this.header ];
   str += hop_bigloo_serialize_context( sc_cons( hash, obj ) );
   str += hop_bigloo_serialize_context( 0 );

   str = "=" + hop_serialize_word( item.hop_serialize_context_def ) + str;

   return str;
}   

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_service ...                                 */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_service() {
   var item = this;
   var classname = "JsService";
   var hash = HopService.prototype.hash;
   var str = "O";

   if( "defineProperty" in Object ) {
      Object.defineProperty( item, "hop_serialize_context_key", {
	 value: hop_serialize_context.key,
	 enumerable: false,
	 configurable: true
      } );
      Object.defineProperty( item, "hop_serialize_context_def", {
	 value: hop_serialize_context.def++,
	 enumerable: false,
	 configurable: true
      } );
   } else {
      item.hop_serialize_context_key = hop_serialize_context.key;
      item.hop_serialize_context_def = hop_serialize_context.def++;
   }

   str += hop_bigloo_serialize_context( sc_cons( hash, [ this.base, dir.dir ] ) );
   str += hop_bigloo_serialize_context( 0 );

   str = "=" + hop_serialize_word( item.hop_serialize_context_def ) + str;

   return str;
}

/*---------------------------------------------------------------------*/
/*    hop_error_hash ...                                               */
/*---------------------------------------------------------------------*/
var hop_error_hash = 0;

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_error ...                                   */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_error( item ) {
   var classname = "JsError";
   var hash = hop_error_hash;
   var str = "O";

   if( "defineProperty" in Object ) {
      Object.defineProperty( item, "hop_serialize_context_key", {
	 value: hop_serialize_context.key,
	 enumerable: false,
	 configurable: true
      } );
      Object.defineProperty( item, "hop_serialize_context_def", {
	 value: hop_serialize_context.def++,
	 enumerable: false,
	 configurable: true
      } );
   } else {
      item.hop_serialize_context_key = hop_serialize_context.key;
      item.hop_serialize_context_def = hop_serialize_context.def++;
   }

   var obj = [ item.constructor.name.toString(),
	       item.message.toString(),
	       hop_bigloo_serialize_context( item.stack ),
	       "fileName" in item ? item.fileName.toString() : "",
	       "lineNumber" in item ? ~~item.lineNumber : 0 ];
   str += hop_bigloo_serialize_context( sc_cons( hash, obj ) );
   str += hop_bigloo_serialize_context( 0 );

   str = "=" + hop_serialize_word( item.hop_serialize_context_def ) + str;

   return str;
}   

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_sc_class ...                                */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_sc_class( clazz ) {
   var classname = sc_symbol2jsstring( sc_class_name( clazz ) );
   return "k" + hop_serialize_string( classname );
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
/*    hop_serialize_bytes ...                                          */
/*---------------------------------------------------------------------*/
function hop_serialize_bytes( word, size ) {
   var rw = "";

   while( --size >= 0 ) {
      var c = ((word >> (size << 3)) & 0xff);

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
      
      size--;
   }
   return rw;
}
   
/*---------------------------------------------------------------------*/
/*    hop_serialize_word_size ...                                      */
/*---------------------------------------------------------------------*/
function hop_serialize_word_size( word, size ) {
   if( size == 0 ) {
      return "%00";
   } else {
      var i1 = (size >> 4);
      var i2 = (size & 0xf);
      var c1 = i1 + ((i1 < 10) ? 48 : 55);
      var c2 = i2 + ((i2 < 10) ? 48 : 55);
      var rw = String.fromCharCode( 37, c1, c2 );

      size--;
      while( size >= 0 ) {
         var c = ((word >> (size << 3)) & 0xff);

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
         
         size--;
      }

      return rw;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_word ...                                           */
/*---------------------------------------------------------------------*/
function hop_serialize_word( word ) {
   return hop_serialize_word_size( word, hop_size_of_word( word ) );
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
function hop_serialize_string( item ) {
   var url = encodeURIComponent( item );

   if( url.length > item.length ) {
      var enc = encodeURIComponent( url );
      
      return '%25' + hop_serialize_word( url.length ) + enc;
   } else {
      return '%22' + hop_serialize_word( item.length ) + url;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_serialize_number ...                                         */
/*---------------------------------------------------------------------*/
function hop_serialize_number( item ) {
   function hop_serialize_number_string( mark, sitem ) {
      return mark + hop_serialize_word( sitem.length ) + sitem;
   }
   
   var sitem = item + "";

   if( sitem.indexOf( "." ) == -1 ) {
      if( item < 0 ) {
	 if( item >= -536870912 ) {
	    return '-' + hop_serialize_word( -item );
	 } else if( item >= 2147483648 ) {
	    return hop_serialize_number_string( 'E', sitem );
	 } else {
	    return hop_serialize_number_string( 'L', sitem );
	 }
      } else {
	 if( item <= 536870911 ) {
	    return hop_serialize_word( item );
	 } else if( item <= 2147483647 ) {
	    return hop_serialize_number_string( 'E', sitem );
	 } else {
	    return hop_serialize_number_string( 'L', sitem );
	 }
      }
   } else {
      return hop_serialize_number_string( 'f', sitem );
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

   if( "defineProperty" in Object ) {
      Object.defineProperty( item, "hop_serialize_context_key", {
	 value: hop_serialize_context.key,
	 enumerable: false,
	 configurable: true
      } );
      Object.defineProperty( item, "hop_serialize_context_def", {
	 value: hop_serialize_context.def++,
	 enumerable: false,
	 configurable: true
      } );
   } else {
      item.hop_serialize_context_key = hop_serialize_context.key;
      item.hop_serialize_context_def = hop_serialize_context.def++;
   } 

   for( i = 0; i < l; i++ ) {
      ra += hop_bigloo_serialize_context( item[ i ] );
   }

   return "=" + hop_serialize_word( item.hop_serialize_context_def ) + ra;
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_hvector ...                                 */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_hvector( item, tag, size ) {
   var l = item.length;
   var ra = 'h' + hop_serialize_word( l ) + hop_serialize_word( size ) + hop_serialize_string( tag );
   var i;

   console.log( "serialize item=", item);
   if( "defineProperty" in Object ) {
      Object.defineProperty( item, "hop_serialize_context_key", {
	 value: hop_serialize_context.key,
	 enumerable: false,
	 configurable: true
      } );
      Object.defineProperty( item, "hop_serialize_context_def", {
	 value: hop_serialize_context.def++,
	 enumerable: false,
	 configurable: true
      } );
   } else {
      item.hop_serialize_context_key = hop_serialize_context.key;
      item.hop_serialize_context_def = hop_serialize_context.def++;
   }

   for( i = 0; i < l; i++ ) {
      ra += hop_serialize_bytes( item[ i ], size );
   }

   console.log( "ra=", ra );
   return "=" + hop_serialize_word( item.hop_serialize_context_def ) + ra;
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_fvector ...                                 */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_fvector( item, tag, size ) {
   var l = item.length;
   var ra = 'h' + hop_serialize_word( l ) + hop_serialize_word( size ) + hop_serialize_string( tag );
   var i;

   if( "defineProperty" in Object ) {
      Object.defineProperty( item, "hop_serialize_context_key", {
	 value: hop_serialize_context.key,
	 enumerable: false,
	 configurable: true
      } );
      Object.defineProperty( item, "hop_serialize_context_def", {
	 value: hop_serialize_context.def++,
	 enumerable: false,
	 configurable: true
      } );
   } else {
      item.hop_serialize_context_key = hop_serialize_context.key;
      item.hop_serialize_context_def = hop_serialize_context.def++;
   }

   for( i = 0; i < l; i++ ) {
      var s = item[ i ].toString();
      ra += hop_serialize_word( s.length ) + s;
   }

   return "=" + hop_serialize_word( item.hop_serialize_context_def ) + ra;
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_s8vector ...                                */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_s8vector() {
   return hop_bigloo_serialize_hvector( this, 's8', 1 );
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_u8vector ...                                */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_u8vector() {
   return hop_bigloo_serialize_hvector( this, 'u8', 1 );
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_s16vector ...                               */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_s16vector() {
   return hop_bigloo_serialize_hvector( this, 's16', 2 );
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_u16vector ...                               */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_u16vector() {
   return hop_bigloo_serialize_hvector( this, 'u16', 2 );
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_s32vector ...                               */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_s32vector() {
   return hop_bigloo_serialize_hvector( this, 's32', 4 );
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_u32vector ...                               */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_u32vector() {
   return hop_bigloo_serialize_hvector( this, 'u32', 4 );
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_s64vector ...                               */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_s64vector() {
   return hop_bigloo_serialize_hvector( this, 's64', 8 );
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_u64vector ...                               */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_u64vector() {
   return hop_bigloo_serialize_hvector( this, 'u64', 8 );
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_f32vector ...                               */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_f32vector() {
   return hop_bigloo_serialize_fvector( this, 'f32', 4 );
}
   
/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_f64vector ...                               */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_f64vector() {
   return hop_bigloo_serialize_fvector( this, 'f64', 8 );
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
      return hop_serialize_string( item.outerHTML );
   } else {
      if( item.nodeType == 1 ) {
	 var str = "<" + item.tagName + " id='" + item.id + "' "
	    + (item.className ? ("class='" + item.className + "'") : "")
	    + ">" + item.innerHTML + "</" + item.tagName + ">";
	 return hop_serialize_string( str );
      } else {
	 if( item.nodeType == 3 ) {
	    return hop_serialize_string( item.nodeValue );
	 } else {
	    return hop_bigloo_serialize_string( "#<" + tname + ">" );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_bigloo_serialize_plist ...                                   */
/*---------------------------------------------------------------------*/
function hop_bigloo_serialize_plist( item ) {
   var plist = null;
   
   for( var p in item ) {
      plist = sc_cons( sc_jsstring2keyword( p ), sc_cons( item[ p ], plist ) );
   }

   return hop_bigloo_serialize_context( plist );
}

/*---------------------------------------------------------------------*/
/*    safe_decode_uri ...                                              */
/*---------------------------------------------------------------------*/
function safe_decode_uri( s ) {
   try {
      return decodeURIComponent( s );
   } catch( e ) {
      /* decoding has hitted an illegal UTF-8 surrogate, decode by hand */
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
/*    hop_obj_to_string ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export obj->string) (arity #t)) */
function hop_obj_to_string( item ) {
   hop_serialize_context.active = false;
   return safe_decode_uri( hop_bigloo_serialize( item ) );
}

/*---------------------------------------------------------------------*/
/*    hop_string_to_obj ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export string->obj) (arity -2)) */
function hop_string_to_obj( s, extension ) {
   var a = hop_config.uint8array ?
      new Uint8Array( s.length ) : new Array( s.length );

   for( var i = 0, len = s.length; i < len; ++i ) {
      a[ i ] = (s.charCodeAt( i )) & 0xff;
   }

   return hop_bytearray_to_obj( a, extension );
}

/*---------------------------------------------------------------------*/
/*    hop_url_encoded_to_obj ...                                       */
/*---------------------------------------------------------------------*/
function hop_url_encoded_to_obj( s ) {
   
   function hex_to_dec( x ) {
      if( x <= 0x39 ) {
	 return x - 0x30;
      } else {
	 if( x <= 0x46 ) {
	    return x - 0x41 + 10;
	 } else {
	    return x - 0x61 + 10;
	 }
      }
   }
      
   var len = s.length;
   
   /* compute the destination length */
   for( var i = 0; i < len; ++i ) {
      if( s.charCodeAt( i ) == 0x25 ) {
	 i += 2;
	 len -= 2;
      }
   }

   /* create the temporary byte array */
   var a = hop_config.uint8array ? new Uint8Array( len ) : new Array( len );

   for( var i = 0, j = 0, len = s.length; i < len; ++i, ++j ) {
      if( s.charCodeAt( i ) == 0x25 ) {
	 var n1 = hex_to_dec( s.charCodeAt( ++i ) );
	 var n2 = hex_to_dec( s.charCodeAt( ++i ) );

	 a[ j ] = (n1 << 4) + n2;
      } else {
	 a[ j ] = (s.charCodeAt(i)) & 0xff;
      }
   }

   /* decode the object */
   return hop_bytearray_to_obj( a );
}

/*---------------------------------------------------------------------*/
/*    hop_bytearray_to_obj ...                                         */
/*---------------------------------------------------------------------*/
function hop_bytearray_to_obj( s, extension ) {
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

   function utf8substring( s, end ) {
      var res = "";

      while( pointer < end ) {
	 var code = s[ pointer++ ];
	 if( code < 128 ) {
	    res += String.fromCharCode( code );
	 } else {
	    var code2 = s[ pointer++ ];
	    if( code < 224 ) {
	       code2 = ((code - 192) << 6) + (code2 - 128);
	       res += String.fromCharCode( code2 );
	    } else {
	       var code3 = s[ pointer++ ];
	       if( code < 240 ) {
		  code3 = ((code - 224) << 12)
		     + ((code2 - 128) << 6) + (code3 - 128);
		  res += String.fromCharCode( code3 );
	       } else {
		  var code4 = s[ pointer++ ];
		  code4 = ((code - 240) << 18)
		     + ((code2 - 128) << 12)
		     + ((code3 - 128) << 6)
		     + (code4 - 128);
		  res += String.fromCharCode( code4 );
	       }
	    }
	 }
      }
      
      return res;
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
      var res = utf8substring( s, pointer + sz );

      if( defining >= 0 ) {
	 definitions[ defining ] = res;
	 defining = -1;
      }

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

      for( var i = 0; i < (sz - 2); i++, hd = hd.__hop_cdr ) {
	 hd.__hop_car = read_item();
	 hd.__hop_cdr = sc_cons( null, null );
      }

      hd.__hop_car = read_item();
      hd.__hop_cdr = read_item();
      
      return res;
   }

   function read_extended_list( sz ) {
      var res = sc_cons( null, null );
      var hd = res;

      if( defining >= 0 ) {
	 definitions[ defining ] = res;
	 defining = -1;
      }

      for( var i = 0; i < (sz - 2); i++, hd = hd.__hop_cdr ) {
	 hd.__hop_car = read_item();
	 // skip the cer
	 read_item();
	 hd.__hop_cdr = sc_cons( null, null );
      }

      hd.__hop_car = read_item();
      // skip the cer
      read_item();
      hd.__hop_cdr = read_item();

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
      var key, sz, clazz, fields, cinfo;
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
	    res[ sc_keyword2jsstring( cinfo[ i ] ) ] = read_item();
	 }

	 // consume the hash number
	 read_item();

	 return res;
      }
   }

   function read_extension() {
      var item = read_item();
      if( extension ) {
	 return extension( item );
      } else {
	 return item;
      }
   }

   function read_custom_object() {
      var old_defining = defining;
      var obj, hashobj, hash_, hash, unserializer, clazz;
      var res;
      
      defining = -1;
      
      hashobj = read_item();
      hash_ = read_item();
      hash = hashobj.__hop_car;
      obj = hashobj.__hop_cdr;

      res = hop_find_class_unserializer( hash )( obj );
      
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

   function read_nanoseconds() {
      return new Date( parseInt( read_string( s ), 10 ) / 1000000 );
   }

   function read_class() {
      var cname = read_symbol();
      var cinfo = read_item();
     
      return sc_class_exists( cname ) || cinfo;
   }

   function read_hvector() {
      var len = read_size( s );
      var bsize = read_size( s );
      var sym = read_item();
      var res;
      var cntr;

      switch( sym ) {
        case "s8": cntr = Int8Array; break;
        case "u8": cntr = Uint8Array; break;
        case "s16": cntr = Int16Array; break;
        case "u16": cntr = Uint16Array; break;
        case "s32": cntr = Int32Array; break;
        case "u32": cntr = Uint32Array; break;
        case "f32": cntr = Float32Array; break;
        case "f64": cntr = Float64Array; break;
        default: cntr = Uint8Array; 
      }
      
      res = new cntr( len );
      for( var i = 0; i < len; i++ ) {
	 res[ i ] = read_word( s, bsize );
      }
      return res;
   }

   function read_procedure() {
      var svc = read_item();

      if( svc == undefined ) {
	 return undefined;
      } else {
	 var rsc = svc.resource ? svc.resource : "/hop";
	 if( svc.javascript) {
	    return eval( sc_format( svc.javascript, svc.path, rsc ) );
	 } else {
	    return undefined;
	 }
      }
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
         case 0x25 /* % */: return decodeURIComponent( read_string( s ) );
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
         case 0x44 /* D */: return read_nanoseconds();
         case 0x6b /* k */: return read_class();
         case 0x72 /* r */: return sc_pregexp( read_string( s ) );
         case 0x58 /* X */: return read_extension();
         case 0x68 /* h */: return read_hvector();
         case 0x70 /* p */: return read_procedure();
	 case 0x56 /* V */: return read_unsupported( "typed-vector" );
	 case 0x21 /* ! */: return read_unsupported( "cell" );
         case 0x75 /* u */: return read_unsupported( "ucs2" );
	 case 0x7a /* z */: return read_unsupported( "bignum" );
         case 0x2b /* + */: return read_unsupported( "custom" );
	 case 0x77 /* w */: return read_unsupported( "weak-ptr" );
         case 0x74 /* t */: return read_unsupported( "tagged vectors" );
         case 0x65 /* e */: return read_unsupported( "process" );
         case 0x6f /* o */: return read_unsupported( "opaque" );
	 default: pointer--; return read_integer( s );
      }
   }

   if( s[ pointer ] === 0x63 /* c */ ) {
      pointer++;
      definitions = new Array( read_size( s ) );
   }

   return read_item();
}

/*---------------------------------------------------------------------*/
/*    hop_custom_object_regexp ...                                     */
/*    -------------------------------------------------------------    */
/*    See HOP_CREATE_ENCODED_ELEMENT (hop-dom.js).                     */
/*---------------------------------------------------------------------*/
var hop_custom_object_regexp =
   new RegExp( "hop_create_encoded_element[(][ ]*\"([^\"]*)\"[ ]*[)]" );

/*---------------------------------------------------------------------*/
/*    hop_class_register_serializer ...                                */
/*---------------------------------------------------------------------*/
/*** META ((export register-class-serialization!) (arity #t)) */
function hop_class_register_serializer( clazz, serializer, unserializer ) {
   hop_class_serializers[ clazz.sc_hash ] = {
      serializer: serializer,
      unserializer: unserializer
   }
}

/*---------------------------------------------------------------------*/
/*    sc__class__ ...                                                  */
/*---------------------------------------------------------------------*/
var sc__class__ = sc_string2keyword( "__class__" );

/*---------------------------------------------------------------------*/
/*    hop_find_class_unserializer ...                                  */
/*---------------------------------------------------------------------*/
function hop_find_class_unserializer( hash ) {
   var custom = hop_class_serializers[ hash ];
   
   if( custom ) {
      return custom.unserializer;
   } else {
      return function( o ) {
	 if( typeof( o ) === "string" ) {
	    var m = o.match( hop_custom_object_regexp );

	    if( m ) {
	       /* kind of specialized eval */
	       return hop_create_encoded_element( m [ 1 ] );
	    } else {
	       return sc_error( "string->obj",
				"Cannot find custom class ("
				+ hash
				+ ") unserializer", o );
	    }
	 } else {
	    if( sc_isPair( o ) && sc_car( o ) == sc__class__ ) {
	       var clazz = { sc_hash: hash };
	       hop_class_register_serializer( clazz, hop_js_to_object, hop_plist2jsobject );
	       return hop_plist2jsobject( o );
	    } else {
	       return sc_error( "string->obj",
				"Cannot find custom class ("
				+ hash
				+ ") unserializer", hash );
	    }
	 }
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

/*---------------------------------------------------------------------*/
/*    hop_hextobuf ...                                                 */
/*---------------------------------------------------------------------*/
function hop_hextobuf( str ) {
   var buf = new Uint8Array( str.length / 2 );
   for( var i = 0; i < str.length; i += 2 ) {
      buf[ i / 2 ] = parseInt( str.substr( i, 2 ), 16 );
   }
   return buf;
}

/*---------------------------------------------------------------------*/
/*    hop_buffer ...                                                   */
/*---------------------------------------------------------------------*/
function hop_buffer( name, _ ) {
   switch( name ) {
   case "JsSlowBuffer":
      return hop_hextobuf( arguments[ 1 ] );
      
   case "JsFastBuffer":
      return new Uint8Array( arguments[ 5 ], arguments[ 2 ], arguments[ 3 ] );
      
   case "JsInt8Array":
      return new Int8Array( arguments[ 5 ], arguments[ 2 ], arguments[ 3 ] );
   case "JsUint8Array":
      return new Uint8Array( arguments[ 5 ], arguments[ 2 ], arguments[ 3 ] );
   case "JsUint8ClampedArray":
      return new Uint8ClampedArray( arguments[ 5 ], arguments[ 2 ], arguments[ 3 ] );
      
   case "JsInt16Array":
      return new Int16Array( arguments[ 5 ], arguments[ 2 ], arguments[ 3 ] );
   case "JsUint16Array":
      return new Uint16Array( arguments[ 5 ], arguments[ 2 ], arguments[ 3 ] );
      
   case "JsInt32Array":
      return new Int32Array( arguments[ 5 ], arguments[ 2 ], arguments[ 3 ] );
   case "JsUint32Array":
      return new Uint32Array( arguments[ 5 ], arguments[ 2 ], arguments[ 3 ] );
      
   case "JsFloat32Array":
      return new Float32Array( arguments[ 5 ], arguments[ 2 ], arguments[ 3 ] );
   case "JsFloat64Array":
      return new Float64Array( arguments[ 5 ], arguments[ 2 ], arguments[ 3 ] );
      
   case "JsDataView":
      return new DataView( arguments[ 3 ], arguments[ 2 ], arguments[ 3 ].length );

   case "JsArrayBuffer":
      return hop_hextobuf( arguments[ 2 ] ).buffer;
   }
}
