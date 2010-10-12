/*=====================================================================*/
/*    .../hop/2.2.x/arch/android/src/fr/inria/hop/HopAndroid.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 11 16:16:28 2010                          */
/*    Last change :  Tue Oct 12 17:47:44 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    A small proxy used by Hop to access the resources of the phone.  */
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
public class HopAndroid extends Thread {
   // instance variables
   Activity activity;
   int port;
   ServerSocket serv;
   Handler handler;

   // constructor
   public HopAndroid( Activity a, int p, Handler h ) {
      super();

      activity = a;
      port = p;
      handler = h;

      try {
	 Log.i( "HopAndroid", "starting server port=" + p );
	 serv = new ServerSocket( p );
      } catch( IOException e ) {
	 Log.v( "HopAndroid", "server error" + e.toString() );
	 handler.sendMessage( android.os.Message.obtain( handler, HopLauncher.MSG_HOPANDROID_FAIL, e ) );
      }
   }
      
   // run hop
   public void run() {
      Socket sock = null;
      Log.i( "HopAndroid", "run" );
      
      try {
	 while( true ) {
	    sock = serv.accept();
	    Log.i( "HopAndroid", "accept" + sock );
	    InputStream ip = sock.getInputStream();
	    OutputStream op = sock.getOutputStream();

	    // get the protocol version
	    int version = ip.read();
	    int cmd = ip.read();

	    while( cmd != -1 ) {
	       // get the command
	       switch( cmd ) {
		  case (byte) 'V':
		     vibrate();
		     break;
		     
		  case (byte) 'M':
		     music();
		     break;
		     
		  default:
		     ;
	       }
	       
	       cmd = ip.read();
	    }

	    sock.close();
	 }
      } catch( IOException e ) {
	 ;
      } finally {
	 try {
	    serv.close();
	 } catch( IOException e ) {
	    ;
	 }
      }
   }

   private void vibrate() {
      Vibrator v = (Vibrator)activity.getSystemService( Context.VIBRATOR_SERVICE );

      long milliseconds = 1000;
      v.vibrate( milliseconds );
   }

   private void music() {
      MediaPlayer mp = MediaPlayer.create( activity, Uri.fromFile( new File( "/data/data/fr.inria.hop/hoplib/hop/2.2.0/weblets/test/sound-test.mp3" ) ) );
      mp.start();
   }
}
      
