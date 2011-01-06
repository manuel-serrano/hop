/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopPluginTts.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 25 17:50:30 2010                          */
/*    Last change :  Thu Jan  6 12:17:04 2011 (serrano)                */
/*    Copyright   :  2010-11 Manuel Serrano                            */
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
import android.media.AudioManager;

import java.util.Locale;
import java.util.HashMap;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginTts extends HopPlugin
   implements TextToSpeech.OnInitListener,
   TextToSpeech.OnUtteranceCompletedListener {
   static boolean pushevent = false;
   static int[] streams = {
      0,
      0,
      AudioManager.STREAM_ALARM,
      AudioManager.STREAM_DTMF,
      AudioManager.STREAM_MUSIC,
      AudioManager.STREAM_NOTIFICATION,
      AudioManager.STREAM_RING,
      AudioManager.STREAM_SYSTEM,
      AudioManager.STREAM_VOICE_CALL
   };
			    
   TextToSpeech tts = null;
   Object condv = new Object();
   String initstatus = null;

   // constructor
   public HopPluginTts( HopDroid h, Activity a, String n ) {
      super( h, a, n );
   }

   // ondemand initialization
   private void initTts() {
      Log.v( "HopPluginTts", "initTts" );
      synchronized( condv ) {
	 Intent checkIntent = new Intent();
	 checkIntent.setAction( TextToSpeech.Engine.ACTION_CHECK_TTS_DATA );
	 Log.v( "HopPluginTts", "initTts: starting activity..." );
	 startHopActivityForResult( checkIntent );
	 try {
	    Log.v( "HopPluginTts", "initTts: waiting for activity..." );
	    condv.wait();
	    Log.v( "HopPluginTts", "initTts completed" + initstatus );
	 } catch( InterruptedException _ ) {
	    initstatus = "initialization interrupted";
	 }
      }
   }

   // onActivityResult
   public void onHopActivityResult( int result, Intent intent ) {
      Log.v( "HopPluginTts", "initTts: activity started" );
      if( result == TextToSpeech.Engine.CHECK_VOICE_DATA_PASS ) {
	 Log.v( "HopPluginTts", "initTts: creating TextToSpeech" );
	 tts = new TextToSpeech( activity, this );
      } else {
	 synchronized( condv ) {
	    // missing data, install it
	    Log.v( "HopPluginTts", "initTts: missing data..." );
	    Intent installIntent = new Intent();
	    installIntent.setAction( TextToSpeech.Engine.ACTION_INSTALL_TTS_DATA );
	    Log.v( "HopPluginTts", "initTts: starting activity for install..." );
	    activity.startActivity( installIntent );
	    initstatus = "missing data";
	    condv.notify();
	 }
      }
   }

   // oninit
   public void onInit( int status ) {
      Log.v( "HopPluginTts", "initTts: onInit..." );
      synchronized( condv ) {
	 if( status == TextToSpeech.SUCCESS ) {
	    initstatus = "success";
	 } else {
	    tts = null;
	    initstatus = "could not initialize tts";
	 }
	 condv.notify();
      }
   }

   // speak completed
   public void onUtteranceCompleted( String value ) {
      handroid.pushEvent( "tts-completed", value );
   }

   // server
   synchronized void server( InputStream ip, OutputStream op )
      throws IOException {

      switch( ip.read() ) {
	 case (byte)'i':
	    // init
	    initTts();
	    op.write( "\"".getBytes() );
	    op.write( initstatus.getBytes() );
	    op.write( "\"".getBytes() );
	    return;
		  
	 case (byte)'c':
	    // close
	    if( tts != null ) {
	       tts.shutdown();
	    }
	    return;
	    
	 case (byte)'b':
	    // start pushing events
	    pushevent = true;
	    return;
	    
	 case (byte)'e':
	    // end pushing events
	    pushevent = false;
	    return;
	    
	 case (byte)'l':
	    // get locale
	    if( tts != null ) {
	       HopPluginLocale.writeLocale( op, tts.getLanguage() );
	    }
	    return;
	    
	 case (byte)'L':
	    // set locale
	    if( tts != null ) {
	       tts.setLanguage( HopPluginLocale.read_locale( ip ) );
	    }
	    return;
	    
	 case (byte)'a':
	    // locale available
	    if( tts != null ) {
	       switch( tts.isLanguageAvailable( HopPluginLocale.read_locale( ip ) ) ) {
		  case TextToSpeech.LANG_AVAILABLE:
		     op.write( "lang".getBytes() );
		     return;

		  case TextToSpeech.LANG_COUNTRY_AVAILABLE:
		     op.write( "lang-country".getBytes() );
		     return;

		  case TextToSpeech.LANG_COUNTRY_VAR_AVAILABLE:
		     op.write( "lang-country-var".getBytes() );
		     return;

		  case TextToSpeech.LANG_MISSING_DATA:
		     op.write( "missing-data".getBytes() );
		     return;
		     
		  case TextToSpeech.LANG_NOT_SUPPORTED:
		     op.write( "lang-not-supported".getBytes() );
		     return;
		     
		  default:
		     op.write( "error".getBytes() );
		     return;
	       }
	    } else {
	       op.write( "error".getBytes() );
	       return;
	    }

	 case (byte)'r':
	    // set rate
	    if( tts != null ) {
	       tts.setSpeechRate( HopDroid.read_float( ip ) );
	    }
	    return;
	    
	 case (byte)'p':
	    // set pitch
	    if( tts != null ) {
	       tts.setPitch( HopDroid.read_float( ip ) );
	    }
	    return;
	    
	 case (byte)'s':
	    // speak
	    if( tts != null ) {
	       HashMap<String, String> opt = null;
	       String s = HopDroid.read_string( ip );
	       int qm = ip.read() == 1 ?
		  TextToSpeech.QUEUE_ADD : TextToSpeech.QUEUE_FLUSH;
	       int stream = ip.read();

	       Log.v( "HopPlugTts", "speak [" + s + "] qm=" + qm + " stream="
		      + stream );

	       if( pushevent ) {
		  opt = new HashMap();
		  opt.put( TextToSpeech.Engine.KEY_PARAM_UTTERANCE_ID, s );
	       }
		  
	       if( (stream > 1) && (stream < streams.length) ) {
		  if( opt == null ) opt = new HashMap();
		  opt.put( TextToSpeech.Engine.KEY_PARAM_STREAM,
			   String.valueOf( streams[ stream ] ) );
	       }
	       
	       tts.speak( s, qm, opt );
	    }
	    return;
	    
	 case (byte)'z':
	    // synthesize
	    if( tts != null ) {
	       HashMap<String, String> opt = null;
	       String s = HopDroid.read_string( ip );
	       String p = HopDroid.read_string( ip );

	       if( pushevent ) {
		  opt = new HashMap();
		  opt.put( TextToSpeech.Engine.KEY_PARAM_UTTERANCE_ID, s );
	       }
	       tts.synthesizeToFile( s, opt, p );
	       tts.addSpeech( s, p );
	    }
	    return;
	    
	 case (byte)' ':
	    // silence
	    if( tts != null ) {
	       HashMap<String, String> opt = null;
	       int ms = HopDroid.read_int32( ip );
	       int qm = ip.read() == 1 ?
		  TextToSpeech.QUEUE_ADD : TextToSpeech.QUEUE_FLUSH;
	       int stream = ip.read();
	       
	       if( pushevent ) {
		  opt = new HashMap();
		  opt.put( TextToSpeech.Engine.KEY_PARAM_UTTERANCE_ID, " " );
	       }

	       if( (stream > 1) && (stream < streams.length) ) {
		  if( opt == null ) opt = new HashMap();
		  opt.put( TextToSpeech.Engine.KEY_PARAM_STREAM,
			   String.valueOf( streams[ stream ] ) );
	       }
	       
	       tts.playSilence( ms, qm, opt );
	    }
	    return;
	    
	 case (byte)'?':
	    // is speaking
	    if( tts != null && tts.isSpeaking() ) {
	       op.write( "#t".getBytes() );
	    } else {
	       op.write( "#f".getBytes() );
	    }
	    return;
	    
	 case (byte)'h':
	    // stop
	    if( tts != null ) {
	       tts.stop();
	    }
	    return;
      }
   }
}

 
