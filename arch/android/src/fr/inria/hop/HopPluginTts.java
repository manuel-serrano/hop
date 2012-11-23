/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopPluginTts.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 25 17:50:30 2010                          */
/*    Last change :  Fri Nov 23 09:10:50 2012 (serrano)                */
/*    Copyright   :  2010-12 Manuel Serrano                            */
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
   public HopPluginTts( HopDroid h, String n ) {
      super( h, n );
   }

   // cleanup
   public void kill() {
      super.kill();

      if( tts != null ) tts.shutdown();
   }
   
   // TTS initialization, delayed to onConnect because it needs that actitivy
   // to be fully initialized
   public void onConnect() {
      Intent checkIntent = new Intent();

      checkIntent.setAction( TextToSpeech.Engine.ACTION_CHECK_TTS_DATA );
      checkIntent.setFlags( Intent.FLAG_ACTIVITY_NEW_TASK );
      checkIntent.setFlags( Intent.FLAG_ACTIVITY_REORDER_TO_FRONT );

      initstatus = "initializing";
      startHopActivityForResult( checkIntent );
   }

   // onActivityResult
   public void onHopActivityResult( int result, Intent intent ) {
      Log.v( "HopPluginTts", "onHopActivityResult.1: activity started" );
      if( true || result == TextToSpeech.Engine.CHECK_VOICE_DATA_PASS ) {
	 Log.v( "HopPluginTts", "onHopActivityResult.2: creating TextToSpeech" );
	 tts = new TextToSpeech( hopdroid.service, this );
	 return;
      } else if( result == TextToSpeech.Engine.CHECK_VOICE_DATA_MISSING_DATA  ) {
	 // missing data, install it
	 Intent installIntent = new Intent();
	 installIntent.setAction( TextToSpeech.Engine.ACTION_INSTALL_TTS_DATA );
	 installIntent.setFlags( Intent.FLAG_ACTIVITY_NEW_TASK );
	 hopdroid.service.startActivity( installIntent );
	 initstatus = "missing data";
      } else if( result == TextToSpeech.Engine.CHECK_VOICE_DATA_BAD_DATA ) {
	 initstatus = "bad data";
      } else if( result == TextToSpeech.Engine.CHECK_VOICE_DATA_MISSING_VOLUME ) {
	 initstatus = "missing volume";
      } else if( result == TextToSpeech.Engine.CHECK_VOICE_DATA_FAIL ) {
	 initstatus = "data fail";
      } else {
	 initstatus = "tts error";
      }
   }

   // onInit
   public void onInit( int status ) {
      Log.v( "HopPluginTts", "onInit.1" );
      synchronized( condv ) {
	 if( status == TextToSpeech.SUCCESS ) {
	    Log.v( "HopPluginTts", "onInit.3: success" );
	    initstatus = "success";
	 } else {
	    tts = null;
	    Log.v( "HopPluginTts", "onInit.4: fail" );
	    initstatus = "could not initialize tts";
	 }
	 condv.notify();
      }
   }

   // speak completed
   public void onUtteranceCompleted( String value ) {
      hopdroid.pushEvent( "tts-completed", value );
   }

   // server
   synchronized void server( InputStream ip, OutputStream op )
      throws IOException {

      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'i':
	    // init
//	    initTts();
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
	       HopPluginLocale.read_locale( ip );
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
	       int qm = HopDroid.read_int( ip ) == 1 ?
		  TextToSpeech.QUEUE_ADD : TextToSpeech.QUEUE_FLUSH;
	       int stream = HopDroid.read_int( ip );

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
	       int qm = HopDroid.read_int( ip ) == 1 ?
		  TextToSpeech.QUEUE_ADD : TextToSpeech.QUEUE_FLUSH;
	       int stream = HopDroid.read_int( ip );
	       
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

 
