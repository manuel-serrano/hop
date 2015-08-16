/*=====================================================================*/
/*    .../android/src/fr/inria/hop/HopAndroidAudioRecorder.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 14 09:43:58 2010                          */
/*    Last change :  Mon Oct 25 10:48:13 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Use the embedded microphone                                      */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.os.*;
import android.util.Log;
import android.media.*;
import android.net.*;

import java.net.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopAndroidAudioRecorder {
   // instance variables
   Activity activity;
   MediaRecorder recorder = null;
   ContentValues contentvalues;
   
   // constructor
   public HopAndroidAudioRecorder( Activity a ) {
      activity = a;
   }

   public MediaRecorder make_recorder() {
      recorder = new MediaRecorder();
/*       contentvalues = new ContentValues( 3 );                       */

      recorder.setAudioSource( MediaRecorder.AudioSource.MIC );
      recorder.setOutputFormat( MediaRecorder.OutputFormat.MPEG_4 );

      return recorder;
   }
      
   // music player
   protected void server( InputStream ip, OutputStream op )
      throws IOException {
      switch( ip.read() ) {
	 case (byte)'x':
	    // exit
	    if( recorder != null ) {
	       recorder.reset();
	       recorder.release();
	       recorder = null;
	       return;
	    }

	 case (byte)'b':
	    // start
	    Log.v( "HopAndroidAudioRecorder", "start" );
	    if( recorder == null ) {
	       recorder = make_recorder();
	    }

	    String path = HopDroid.read_string( ip );
	    int quality = ip.read();

	    // get the quality
	    switch( quality ) {
	       case 0:
		  recorder.setAudioEncoder( MediaRecorder.AudioEncoder.AMR_NB );
		  break;
		  
	       default:
		  recorder.setAudioEncoder( MediaRecorder.AudioEncoder.DEFAULT );
	    }

	    // the record information
/* 	    contentvalues.put( MediaStore.MediaColumns.TITLE, title ); */
/* 	    contentvalues.put( MediaStore.MediaColumns.TIMESTAMP,      */
/* 			       System.currentTimeMillis() );           */
/* 	    contentvalues.put( MediaStore.MediaColumns.MIME_TYPE,      */
/* 			       recorder.getMimeContentType() );        */

	    recorder.setOutputFile( path );

	    // start recording
	    recorder.prepare();
	    recorder.start();

	    return;

	 case (byte)'e':
	    // stop
	    Log.v( "HopAndroidAudioRecorder", "stop" );
	    if( recorder != null ) {
	       recorder.stop();
	       recorder.reset();
	       return;
	    }
      }
   }
}
