/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopPluginLocale.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Nov 30 17:35:50 2010                          */
/*    Last change :  Thu Nov 22 16:43:29 2012 (serrano)                */
/*    Copyright   :  2010-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Get the phone locales                                            */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.os.Bundle;
import android.util.*;
import android.speech.tts.TextToSpeech;
import android.util.Log;

import java.util.Locale;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginLocale extends HopPlugin {
   static boolean localeInitp = false;
   private String inittext = null;
   TextToSpeech locale;
   String string;

   // constructor
   public HopPluginLocale( HopDroid h, String n ) {
      super( h, n );
   }

   // server
   void server( InputStream ip, OutputStream op ) throws IOException {
       switch( HopDroid.read_int( ip ) ) {
	 // list locales
	 case (byte)'l':
	    writeLocales( op, Locale.getAvailableLocales() );
	    return;
	 // current locale
	 case (byte)'c':
	    writeLocale( op, Locale.getDefault() );
	    return;
	 // set current locale
	 case (byte)'s':
	    Locale.setDefault( HopPluginLocale.read_locale( ip ) );
	    return;
      }
   }

   // read_locale
   public static Locale read_locale( InputStream ip ) throws IOException {
      String[] v = HopDroid.read_stringv( ip );
      switch( v.length ) {
	 case 1:
	    return new Locale( v[ 0 ] );
	 case 2:
	    if( v[ 1 ].length() == 0 ) {
	       return new Locale( v[ 0 ] );
	    } else {
	       return new Locale( v[ 0 ], v[ 1 ] );
	    }
	 default:
	    if( v[ 1 ].length() == 0 ) {
	       return new Locale( v[ 0 ] );
	    } else {
	       if( v[ 2 ].length() == 0 ) {
		  return new Locale( v[ 0 ], v[ 1 ] );
	       } else {
		  return new Locale( v[ 0 ], v[ 1 ], v[ 3 ] );
	       }
	    }
      }
   }
   
   // writeLocales
   void writeLocales( OutputStream op, Locale[] locales ) throws IOException {
      op.write( "(\n".getBytes() );
      
      for( int i = 0; i < locales.length; i++ ) {
	 writeLocale( op, locales[ i ] );
      }
      
      op.write( ")".getBytes() );
   }
   
   // writeLocale
   public static void writeLocale( OutputStream op, Locale l )
      throws IOException {
      op.write( "(\"".getBytes() );
      op.write( l.getLanguage().getBytes() );
      op.write( "\" \"".getBytes() );
      op.write( l.getCountry().getBytes() );
      op.write( "\" \"".getBytes() );
      op.write( l.getVariant().getBytes() );
      op.write( "\" \"".getBytes() );
      op.write( l.getDisplayName().getBytes() );
      op.write( "\")".getBytes() );
   }
   
}
      
	 

