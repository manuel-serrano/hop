/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopPluginLocale.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Nov 30 17:35:50 2010                          */
/*    Last change :  Wed Dec  1 08:19:52 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
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
   public HopPluginLocale( HopDroid h, Activity a, String n ) {
      super( h, a, n );
   }

   // server
   void server( InputStream ip, OutputStream op ) throws IOException {
      
      switch( ip.read() ) {
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
	    return;
      }
   }

   // writeLocale
   void writeLocale( OutputStream op, Locale l ) throws IOException {
      op.write( " (".getBytes() );
/*       op.write( l.getISOLanguages().getBytes() );                   */
/*       op.write( "_".getBytes() );                                   */
/*       op.write( l.getISOCountries().getBytes() );                   */
/*       op.write( " ".getBytes() );                                   */
      op.write( l.getDisplayName().getBytes() );
      op.write( ")\n".getBytes() );
   }
   
   // writeLocales
   void writeLocales( OutputStream op, Locale[] locales ) throws IOException {
      op.write( "(\n".getBytes() );
      
      for( int i = 0; i < locales.length; i++ ) {
	 writeLocale( op, locales[ i ] );
      }
      
      op.write( ")".getBytes() );
   }
}
      
	 

