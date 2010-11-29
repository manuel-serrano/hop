/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopPluginTts.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 25 17:50:30 2010                          */
/*    Last change :  Mon Nov 29 16:35:11 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Text-to-speech facilities                                        */
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
import java.util.Locale;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginTts extends HopPlugin {
   static boolean ttsInitp = false;
   private String inittext = null;

   // constructor
   public HopPluginTts( HopDroid h, Activity a, String n ) {
      super( h, a, n );
   }

   // initialization on demand
   private void ttsInit( String s ) {
      Intent checkIntent = new Intent();
      checkIntent.setAction( TextToSpeech.Engine.ACTION_CHECK_TTS_DATA );
      startHopActivityForResult( checkIntent );

      inittext = s;
   }

   // server
   void server( InputStream ip, OutputStream op ) throws IOException {
      
      switch( ip.read() ) {
	 // begin
	 case (byte)'s':
	    String s = HopDroid.read_string( ip );

	    if( ttsInitp ) {
	       speak( s );
	    } else {
	       ttsInit( s );
	    }
	    return;
      }
   }

   // onActivityResult
   public void onHopActivityResult( int result, Intent intent ) {
      if( result == TextToSpeech.Engine.CHECK_VOICE_DATA_PASS ) {
	 String s = inittext;
	 inittext = null;
	 speak( s );
      } else {
	 // missing data, install it
	 Intent installIntent = new Intent();
	 installIntent.setAction(
	    TextToSpeech.Engine.ACTION_INSTALL_TTS_DATA );
	 activity.startActivity( installIntent );
      }
   }

   // speak
   private void speak( String s ) {
      // success, create the TTS instance
      TextToSpeech tts = new TextToSpeech( activity, new TextToSpeech.OnInitListener() {
	    public void onInit( int status ) {
	       ;
	    }
	 } );
      
      tts.setLanguage( Locale.FRANCE );

      tts.speak( s, TextToSpeech.QUEUE_FLUSH, null );
      
      tts.shutdown();
   }
}

 
