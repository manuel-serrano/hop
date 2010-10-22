/*=====================================================================*/
/*    .../2.2.x/arch/android/androidvoice/etc/HopPluginVoice.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 22 10:05:43 2010                          */
/*    Last change :  Fri Oct 22 10:49:07 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Voice Recognition for Hop                                        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop.demo;

import fr.inria.hop.*;

import android.app.Activity;
import android.content.pm.PackageManager;
import android.speech.RecognizerIntent;
import android.util.Log;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginVoice extends HopPlugin {
   // Class variables
   static boolean voiceRecognitionEnabled = false;
      
   static {
      PackageManager pm;
      List<ResolveInfo> activities;
      
      pm = getPackageManager();
      actitivies = pm.queryIntentActivities(
	 new Intent( RecognizerIntent.ACTION_RECOGNIZE_SPEECH ), 0 );
      voiceRecognitionEnabled = (activities.size() != 0);
      
      Log.v( "HopPluginVoice", "activities size=" + activities.size() );
   }
   
   // constructor
   public HopPluginVoice( HopAndroid h, Activity a, String n ) {
      super( h, a, n );
   }

   // server
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( ip.read() ) {
	 // begin
	 case (byte)'b':
	    if( voiceRecognitionEnabled ) {
	       op.write( "#f".getBytes() );
	    } else {
	       startVoiceRecognitionActivity();
	       op.write( "#t".getBytes() );
	    }
	    return;
      }
   }

}


/*---------------------------------------------------------------------*/
/*    Activity                                                         */
/*---------------------------------------------------------------------*/
class HopPluginVoiceActivity extends Activity {
   private static final int VOICE_RECOGNITION_REQUEST_CODE = 1234;
   
   HopPluginVoiceActivity() {
      super();
      
      Intent intent = new Intent( RecognizerIntent.ACTION_RECOGNIZE_SPEECH );
      intent.putExtra( RecognizerIntent.EXTRA_LANGUAGE_MODEL,
		       RecognizerIntent.LANGUAGE_MODEL_FREE_FORM );
      intent.putExtra( RecognizerIntent.EXTRA_PROMPT,
		       "Speech recognition demo" );
      
      Log.v( "HopPluginVoice", "starting activity..." );
      startActivityForResult( intent, VOICE_RECOGNITION_REQUEST_CODE );
   }

    @Override protected void onActivityResult(
       int requestCode, int resultCode, Intent data ) {
       Log.v( "HopPluginVoice", "Activity Result..."
	      + requestCode
	      + " " + resultCode );
       
        if( requestCode == VOICE_RECOGNITION_REQUEST_CODE
	    && resultCode == RESULT_OK ) {
	   String[] matches = data.getStringExtra(
	      RecognizerIntent.EXTRA_RESULTS );
	   String s = "";

	   for( int i = 0; i < matches.length(); i++ ) {
	      s += "\"" + matches[ i ] + "\"";
	   }
	      
	   handroid.pushEvent( "voice", "(" + s + ")" );
        }

        super.onActivityResult( requestCode, resultCode, data );
    } 
}
