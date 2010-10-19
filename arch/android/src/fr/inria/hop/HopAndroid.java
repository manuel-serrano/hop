/*=====================================================================*/
/*    .../hop/2.2.x/arch/android/src/fr/inria/hop/HopAndroid.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 11 16:16:28 2010                          */
/*    Last change :  Tue Oct 19 11:09:22 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    A small proxy used by Hop to access the resources of the phone.  */
/*    -------------------------------------------------------------    */
/*    The protocol it uses is as follows:                              */
/*      byte 0 = protocol-version                                      */
/*      byte 1 = service (typically 'V', 'M', 'R', 'X', ...)           */
/*                                                                     */
/*    The following bytes depends on the service. Some expect          */
/*    extra parameter. Some don't.                                     */
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
import java.lang.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopAndroid extends Thread {
   // static variables
   static Vector plugins = new Vector( 10 );

   // instance variables
   Activity activity;
   int port;
   ServerSocket serv;
   Handler handler;
   
   HopAndroidMusicPlayer hopmplayer = null;
   HopAndroidAudioRecorder hoparecorder = null;
   HopAndroidVibrator hopvibrator = null;
   HopAndroidSensor hopsensor = null;
   HopAndroidSms hopsms = null;

   // constructor
   public HopAndroid( Activity a, int p, Handler h ) {
      super();

      activity = a;
      port = p;
      handler = h;

      try {
	 Log.i( "HopAndroid", "starting servers port=" + p );
	 serv = new ServerSocket( p );

	 // register the initial plugins
	 registerPlugin( new HopPluginInit( activity, "init" ) );
	 registerPlugin( new HopPluginVibrate( activity, "vibrate" ) );
	 registerPlugin( new HopPluginSensor( activity, "sensor" ) );
	 registerPlugin( new HopPluginMusicPlayer( activity, "musicplayer" ) );
      } catch( Exception e ) {
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

   // get plugin
   static protected int getPlugin( String name ) {
      synchronized( plugins ) {
	 int s = plugins.size() - 1;

	 for( int i = 0; i < s; i++ ) {
	    HopPlugin p = (HopPlugin)plugins.get( i );
	    if( name.equals( p.name ) )
	       return i;
	 }

	 return -1;
      }
   }
   
   // register plugin
   static protected int registerPlugin( HopPlugin p ) {
      synchronized( plugins ) {
	 plugins.add( p );

	 return plugins.size() - 1;
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
	    int id = read_int32( ip );

	    Log.v( "HopAndroid", "plugin number: " + id );
	    try {
	       HopPlugin p = (HopPlugin)plugins.get( id );

	       Log.v( "HopAndroid", "plugin: " + p.name );
	       p.server( ip, op );
	    } catch( ArrayIndexOutOfBoundsException _ ) {
	       Log.e( "HopAndroid", "plugin not found: " + id );
	       ;
	    }
	    
/* 	    if( version == -1 || svc == -1 ) return;                   */
/* 						                       */
/* 	    switch( svc ) {                                            */
/* 	       case (byte)'V':                                         */
/* 		  // vibrate                                           */
/* 		  if( hopvibrator == null ) {                          */
/* 		     hopvibrator = new HopAndroidVibrator( activity ); */
/* 		  }                                                    */
/* 		  hopvibrator.server( ip, op );                        */
/* 		  break;                                               */
/* 		                                                       */
/* 	       case (byte)'M':                                         */
/* 		  // musicplayer                                       */
/* 		  if( hopmplayer == null ) {                           */
/* 		     hopmplayer = new HopAndroidMusicPlayer( activity ); */
/* 		  }                                                    */
/*                                                                     */
/* 		  hopmplayer.server( ip, op );                         */
/* 		  break;                                               */
/* 		                                                       */
/* 	       case (byte)'R':                                         */
/* 		  // musicrecorder                                     */
/* 		  if( hoparecorder == null ) {                         */
/* 		     hoparecorder = new HopAndroidAudioRecorder( activity ); */
/* 		  }                                                    */
/*                                                                     */
/* 		  hoparecorder.server( ip, op );                       */
/* 		  break;                                               */
/* 		                                                       */
/* 	       case (byte)'S':                                         */
/* 		  // sensor                                            */
/* 		  if( hopsensor == null ) {                            */
/* 		     hopsensor = new HopAndroidSensor( activity );     */
/* 		  }                                                    */
/*                                                                     */
/* 		  hopsensor.server( ip, op );                          */
/* 		  break;                                               */
/*                                                                     */
/* 	       case (byte)'T':                                         */
/* 		  // SMS                                               */
/* 		  // sensor                                            */
/* 		  if( hopsms == null ) {                               */
/* 		     hopsms = new HopAndroidSms( activity );           */
/* 		  }                                                    */
/*                                                                     */
/* 		  hopsms.server( ip, op );                             */
/* 		  break;                                               */
/* 	                                                               */
/* 	       case (byte)'P':                                         */
/* 		  // ping                                              */
/* 		  Log.v( "HopAndroid", "ping..." );                    */
/* 		  op.write( 'G' );                                     */
/* 		  op.flush();                                          */
/* 		  break;                                               */
/* 	                                                               */
/* 	       case (byte)'X':                                         */
/* 		  // reset                                             */
/* 		  Log.v( "HopAndroid", "reset..." );                   */
/* 		  return;                                              */
/* 	                                                               */
/* 	       default:                                                */
/* 		  Log.v( "HopAndroid", "unknown service svc="          */
/* 			 + Integer.toHexString( svc ) );               */
/* 	    }                                                          */
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

   // read_int32
   protected static int read_int32( InputStream ip ) throws IOException {
      int b0 = ip.read();
      int b1 = ip.read();
      int b2 = ip.read();
      int b3 = ip.read();

      return (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
   }

   // read_int64
   protected static long read_int64( InputStream ip ) throws IOException {
      int i0 = read_int32( ip );
      int i1 = read_int32( ip );

      return ((long)i0) << 32 | i1;
   }

   // read_string
   protected static String read_string( InputStream ip ) throws IOException {
      int sz = read_int32( ip );
      byte[] buf = new byte[ sz ];

      ip.read( buf, 0, sz );

      return new String( buf );
   }
}
      
