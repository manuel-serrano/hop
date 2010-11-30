/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopPluginLocale.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Nov 30 17:35:50 2010                          */
/*    Last change :  Tue Nov 30 17:46:45 2010 (serrano)                */
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
public class HopPluginLocale extends HopPlugin
   implements TextToSpeech.OnInitListener {
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
	 // begin
	 case (byte)'l':
	    writeLocales( op );
	    return;
      }
   }

   // writeLocales
   void writeLocales( OutputStream op ) throws IOException {
      Locale[] locales = getAvailableLocales();

      op.write( "(\n".getgetBytes() );
      
      for( int i = 0; i < locales.length; i++ ) {
	 op.write( " (".getBytes() );
/* 	 op.write( locales[ i ].getIOSLanguages() );                   */
/* 	 op.write( "_".getBytes() );                                   */
/* 	 op.write( locales[ i ].getIOSCountries() );                   */
/* 	 op.write( " ".getBytes() );                                   */
	 op.write( locales[ i ].getDisplayName() );
	 op.write( ")\n".getBytes() );
      }
      
      op.write( ")".getgetBytes() );
   }
}
      
	 

