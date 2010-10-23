/*=====================================================================*/
/*    .../hop/2.2.x/arch/android/src/fr/inria/hop/HopAndroid.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 11 16:16:28 2010                          */
/*    Last change :  Sat Oct 23 07:53:25 2010 (serrano)                */
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
import java.lang.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopAndroid extends Thread {
   // static variables
   static Vector plugins = new Vector( 10 );
   static HopAndroid hopandroid = null;

   // instance variables
   Activity activity;
   int port;
   ServerSocket serv1;
   ServerSocket serv2;
   Handler handler;
   final Hashtable eventtable = new Hashtable();
   
   // constructor
   public HopAndroid( Activity a, int p, Handler h ) {
      super();

      activity = a;
      port = p;
      handler = h;

      hopandroid = this;

      try {
	 Log.i( "HopAndroid", "starting servers port=" + p );
	 try {
	    serv1 = new ServerSocket( p );
	 } catch( Exception e ) {
	    Log.e( "HopAndroid", "Cannot start server localhost:" + p );
	    throw e;
	 }
	 try {
	    serv2 = new ServerSocket( p + 1 );
	 } catch( Exception e ) {
	    Log.e( "HopAndroid", "Cannot start server localhost:" + (p + 1) );
	    throw e;
	 }

	 // register the initial plugins
	 registerPlugin( new HopPluginInit( this, activity, "init" ) );
	 registerPlugin( new HopPluginVibrate( this, activity, "vibrate" ) );
	 registerPlugin( new HopPluginSensor( this, activity, "sensor" ) );
	 registerPlugin( new HopPluginMusicPlayer( this, activity, "musicplayer" ) );
	 registerPlugin( new HopPluginSms( this, activity, "sms" ) );
      } catch( Exception e ) {
	 Log.v( "HopAndroid", "server error" + e.toString() + " exception=" +
	    e.getClass().getName() );
	 handler.sendMessage( android.os.Message.obtain( handler, HopLauncher.MSG_HOPANDROID_FAIL, e ) );
      }
   }

   // kill
   public void kill() {
      try {
	 if( !serv1.isClosed() ) serv1.close();
	 if( !serv2.isClosed() ) serv2.close();
      } catch( Exception _ ) {
	 ;
      }
   }
      
   // run hop
   public void run() {
      if( serv1 != null ) {
	 Log.i( "HopAndroid", "run" );
	 runPushEvent();
	 try {
	    while( true ) {
	       final Socket sock = serv1.accept();

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
	    kill();
	 }
      }
   }

   // Android push event
   public void runPushEvent() {
      new Thread( new Runnable () {
	    public void run() {
	       try {
		  while( true ) {
		     final Socket sock2 = serv2.accept();
		     Log.i( "HopAndroid", "accept2 " + sock2 );
		     // handle the session in a background thread (normally very
		     // few of these threads are created so there is no need
		     // to use a complexe machinery based on thread pool).
		     new Thread( new Runnable() {
			   public void run() {
			      serverEvent( sock2 );
			   }
			} ).start();
		  }
	       } catch( IOException e ) {
		  ;
	       } finally {
		  kill();
	       }
	    }
	 } ).start();
   }

   // get plugin
   static protected int getPlugin( String name ) {
      synchronized( plugins ) {
	 int s = plugins.size();

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
      Log.i( "HopAndroid", "server " + sock );
      try {
	 InputStream ip = sock.getInputStream();
	 OutputStream op = sock.getOutputStream();

	 while( true ) {
	    int version = ip.read();
	    int id = read_int32( ip );

	    try {
	       HopPlugin p = (HopPlugin)plugins.get( id );

	       p.server( ip, op );
	       op.write( " ".getBytes() );
	       op.flush();
	    } catch( ArrayIndexOutOfBoundsException _ ) {
	       Log.e( "HopAndroid", "plugin not found: " + id );
	       // we got an eof, escape from here
	       if( id == -1 ) return;
	       ;
	    }
	 }
      } catch( IOException e ) {
	 Log.v( "HopAndroid", "Plugin IOException: " + e.getMessage() );
      } finally {
	 try {
	    sock.close();
	 } catch( IOException _ ) {
	    ;
	 }
      }
   }
	    
   // registerEvent
   private void serverEvent( Socket sock2 ) {
      Log.i( "HopAndroid", "serverEvent " + sock2 );
      try {
	 InputStream ip = sock2.getInputStream();

	 while( true ) {
	    String event = read_string( ip );
	    int a = ip.read();
	    
	    synchronized( eventtable ) {
	       Hashtable ht = (Hashtable)eventtable.get( event );
	    
	       Log.i( "HopAndroid", (a == 1 ? "register" : "unregister") +
		      " event [" + event + "]" );
	       // a == 1, add an event listener. a == 0, remove listener
	       if( ht == null ) {
		  if( a == 1 ) {
		     ht = new Hashtable( 2 );
		     ht.put( sock2, new Integer( 1 ) );
		     eventtable.put( event, ht );
		  }
	       } else {
		  Integer i = (Integer)ht.get( sock2 );
	       
		  if( i == null ) {
		     if( a == 1 ) {
			ht.put( sock2, new Integer( 1 ) );
		     }
		  } else {
		     int ni = i + a == 1 ? 1 : -1;
		  
		     if( i == 0 ) {
			ht.remove( sock2 );
		     } else {
			ht.put( sock2, new Integer( ni ) );
		     }
		  }
	       }
	    }
	 }
      } catch( IOException e ) {
	 Log.v( "HopAndroid", "Plugin IOException: " + e.getMessage() );
      } finally {
	 try {
	    sock2.close();
	 } catch( IOException _ ) {
	    ;
	 }
      }
   }

   // hopPushEvent
   static void hopPushEvent( String event, String value ) {
      hopandroid.pushEvent( event, value );
   }
   
   // pushEvent
   public void pushEvent( String event, String value ) {
      synchronized( eventtable ) {
	 Hashtable ht = (Hashtable)eventtable.get( event );

	 if( ht != null ) {
	    Enumeration s = ht.keys();

	    while( s.hasMoreElements() ) {
	       Socket sock = (Socket)s.nextElement();
	       try {
		  OutputStream op = sock.getOutputStream();
		  op.write( "\"".getBytes() );
		  op.write( event.getBytes() );
		  op.write( "\" ".getBytes() );
		  op.write( value.getBytes() );
		  op.write( " ".getBytes() );
		  op.flush();
	       } catch( IOException e ) {
		  Log.e( "HopAndroid", "pushEvent error: "
			 + sock + " " + e.getMessage() );
		  ht.remove( sock );
	       }
	    }
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
      
