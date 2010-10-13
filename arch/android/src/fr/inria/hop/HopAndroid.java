/*=====================================================================*/
/*    .../hop/2.2.x/arch/android/src/fr/inria/hop/HopAndroid.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 11 16:16:28 2010                          */
/*    Last change :  Wed Oct 13 16:55:48 2010 (serrano)                */
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
   MediaPlayer mplayer = null;

   // constructor
   public HopAndroid( Activity a, int p, Handler h ) {
      super();

      activity = a;
      port = p;
      handler = h;

      try {
	 Log.i( "HopAndroid", "starting server port=" + p );
	 serv = new ServerSocket( p );
      } catch( BindException e ) {
	 Log.v( "HopAndroid", "server error" + e.toString() );
      } catch( IOException e ) {
	 Log.v( "HopAndroid", "server error" + e.toString() + " exception=" +
	    e.getClass().getName() );
	 handler.sendMessage( android.os.Message.obtain( handler, HopLauncher.MSG_HOPANDROID_FAIL, e ) );
      }
   }
      
   // run hop
   public void run() {
      if( serv != null ) {
	 Log.i( "HopAndroid", "run" );
	 try {
	    while( true ) {
	       final Socket sock = serv.accept();

	       Log.i( "HopAndroid", "accept" + sock );
	       // handle the session in a background thread (normally very
	       // few of these threads are created so there is no need
	       // to use a complexe machinery based on thread pool).
	       new Thread( new Runnable() {
		     public void run() {
			server( sock );
		     }
		  } ).start();
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
   }

   // handle a session with one client connected to the HopAndroid server
   private void server( Socket sock ) {
      Log.i( "HopAndroid", "server" + sock );
      try {
	 InputStream ip = sock.getInputStream();
	 OutputStream op = sock.getOutputStream();

	 while( true ) {
	    int version = ip.read();
	    int svc = ip.read();

	    if( version == -1 || svc == -1 ) return;
						
	    switch( svc ) {
	       case (byte)'V':
		  // vibrate
		  Log.v( "HopAndroid", "vibrate..." ); 
		  vibrate( ip );
		  break;
		     
	       case (byte)'M':
		  // music
		  Log.v( "HopAndroid", "music..." ); 
		  music( ip, op );
		  break;
		     
	       case (byte)'X':
		  // reset
		  Log.v( "HopAndroid", "reset..." ); 
		  return;
	       
	       case (byte)'P':
		  // ping
		  Log.v( "HopAndroid", "ping..." ); 
		  op.write( 'G' );
		  op.flush();
		  return;
	       
	       default:
		  Log.v( "HopAndroid", "unknown service svc="
			 + Integer.toHexString( svc ) );
	    }
	 }
      } catch( IOException e ) {
	 ;
      } finally {
	 try {
	    sock.close();
	 } catch( IOException _ ) {
	    ;
	 }
      }
   }
   
   private void vibrate( InputStream ip ) throws IOException {
      switch( ip.read() ) {
	 default:
	    Vibrator v = (Vibrator)activity.getSystemService( Context.VIBRATOR_SERVICE );

	    long milliseconds = 1000;
	    v.vibrate( milliseconds );
      }
   }

   // music service
   private void music( InputStream ip, OutputStream op ) throws IOException {
      switch( ip.read() ) {
	 case (byte)'x':
	    // exit
	    if( mplayer != null ) {
	       mplayer.release();
	       mplayer = null;
	       return;
	    }

	 case (byte)'b':
	    // start
	    Log.v( "HopAndroid", "mediaplayer start" );
	    if( mplayer != null ) {
	       mplayer.start();
	       return;
	    }

	 case (byte)'e':
	    // stop
	    Log.v( "HopAndroid", "mediaplayer stop" );
	    if( mplayer != null ) {
	       mplayer.stop();
	       return;
	    }

	 case (byte)'p':
	    // pause
	    Log.v( "HopAndroid", "mediaplayer pause" );
	    if( mplayer != null ) {
	       mplayer.pause();
	       return;
	    }

	 case (byte)'u':
	    // url
	    if( mplayer == null ) {
	       mplayer = new MediaPlayer();
	    } else {
	       mplayer.reset();
	    }
	    String uri = read_string( ip );

	    Log.v( "HopAndroid", "mediaplayer src=" + uri );

	    File file = new File( uri );

	    if( file.exists() ) {
	       mplayer.setDataSource( activity, Uri.fromFile( file ) );
	       mplayer.prepare();
	    } else {
	       mplayer.setDataSource( activity, Uri.parse( uri ) );
	       mplayer.prepareAsync();
	    }
	    mplayer.start();
	    return;

	 case (byte)'v':
	    // set volume
	    if( mplayer == null ) {
	       mplayer = new MediaPlayer();
	    }

	    int voll = read_int32( ip );
	    int volr = read_int32( ip );

	    mplayer.setVolume( (float)voll/100, (float)volr/100 );
	    return;
	    
	 case (byte)'S':
	    // get status: state, songlength, songpos
	    if( mplayer == null ) {
	       op.write( "(unspecified 0 0)".getBytes() );
	    } else {
	       Log.v( "HopAndroid", "player is playing: " + mplayer.isPlaying() );
	       if( mplayer.isPlaying() ) {
		  op.write( "(play ".getBytes() );
		  op.write( Integer.toString( mplayer.getDuration() ).getBytes() );
		  op.write( " ".getBytes() );
		  op.write( Integer.toString( mplayer.getCurrentPosition() ).getBytes() );
		  op.write( ")".getBytes() );
	       } else {
		  op.write( "(unspecified 0 0)".getBytes() );
	       }
	       op.flush();
	    }
	    return;
      }
   }

   // read_int32
   private int read_int32( InputStream ip ) throws IOException {
      int b0 = ip.read();
      int b1 = ip.read();
      int b2 = ip.read();
      int b3 = ip.read();

      return (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
   }

   // read_string
   private String read_string( InputStream ip ) throws IOException {
      int sz = read_int32( ip );
      byte[] buf = new byte[ sz ];

      ip.read( buf, 0, sz );

      return new String( buf );
   }
}
      
