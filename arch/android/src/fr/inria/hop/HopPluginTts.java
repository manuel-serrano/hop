/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopPluginTts.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 25 17:50:30 2010                          */
/*    Last change :  Tue Nov 30 15:58:40 2010 (serrano)                */
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
import android.util.Log;

import java.util.Locale;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginTts extends HopPlugin
   implements TextToSpeech.OnInitListener {
   static boolean ttsInitp = false;
   private String inittext = null;
   TextToSpeech tts;
   String string;

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
	 Log.v( "onHopActivityResult", "check voice data pass" );
	 speak( s );
      } else {
	 Log.v( "onHopActivityResult", "missing data" );
	 // missing data, install it
	 Intent installIntent = new Intent();
	 installIntent.setAction(
	    TextToSpeech.Engine.ACTION_INSTALL_TTS_DATA );
	 activity.startActivity( installIntent );
      }
   }

   // oninit
   public void onInit( int status ) {
      if( status == TextToSpeech.SUCCESS ) {
	 Log.v( "HopPluginTts", "oninit, success" );
	 tts.setLanguage( Locale.FRANCE );
		  
	 Log.v( "HopPluginTts", "say [" + string + "]" );
	 tts.speak( string, TextToSpeech.QUEUE_FLUSH, null );
		  
      } else {
	 Log.v( "HopPluginTts", "could not initialize tts" );
      }
   }
   
   // speak
   private void speak( final String s ) {
      // success, create the TTS instance
      string = s;
      tts = new TextToSpeech( activity, this );
      tts.shutdown();
   }
}

 
