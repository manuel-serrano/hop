/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopDroid.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 11 16:16:28 2010                          */
/*    Last change :  Tue Sep 18 16:49:47 2012 (serrano)                */
/*    Copyright   :  2010-12 Manuel Serrano                            */
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
public class HopDroid extends Thread {
   // static variables
   static Vector plugins = null;
   static HopDroid hopdroid = null;

   // instance variables
   boolean killed = false;
   boolean inkill = false;
   Activity activity = null;
   Service service;
   LocalServerSocket serv1;
   LocalServerSocket serv2;
   Vector serv1conn;
   Vector serv2conn;
   Thread thread1 = null;
   Thread thread2 = null;
   Handler handler = null;
   final Hashtable eventtable = new Hashtable();
   
   // constructor
   public HopDroid( int port, int porte, Service s ) {
      super();

      service = s;
      hopdroid = this;
      
      plugins = new Vector( 16 );

      try {
	 Log.i( "HopDroid", "starting servers port=" + port + ", " + porte );

	 serv1 = new LocalServerSocket( "hop-" + port );
	 serv2 = new LocalServerSocket( "hop-" + porte );
	 
	 serv1conn = new Vector();
	 serv2conn = new Vector();

	 // register the initial plugins
	 registerPlugin( new HopPluginInit( this, "init" ) );
	 
	 registerPlugin( new HopPluginBuild( this, "build" ) );
	 registerPlugin( new HopPluginLocale( this, "locale" ) );
	 registerPlugin( new HopPluginVibrate( this, "vibrate" ) );
	 registerPlugin( new HopPluginMusicPlayer( this, "musicplayer" ) );
	 registerPlugin( new HopPluginMediaAudio( this, "mediaaudio" ) );
	 registerPlugin( new HopPluginSensor( this, "sensor" ) );
	 registerPlugin( new HopPluginBattery( this, "battery" ) );
	 registerPlugin( new HopPluginSms( this, "sms" ) );
	 registerPlugin( new HopPluginWifi( this, "wifi" ) );
	 registerPlugin( new HopPluginConnectivity( this, "connectivity" ) );
	 registerPlugin( new HopPluginContact( this, "contact" ) );
	 registerPlugin( new HopPluginZeroconf( this, "zeroconf" ) );
	 
	 registerPlugin( new HopPluginCall( this, "call" ) );
	 registerPlugin( new HopPluginTts( this, "tts" ) );
      } catch( Exception e ) {
	 abortError( e, "HopDroid" );
      }
   }

   // abort
   public void abortError( Throwable e, String proc ) {
      if( !killed && !inkill ) {
	 Log.e( "HopDroid", "error(" + proc + "): " + e.toString() + " exception=" + e.getClass().getName(), e );
      
	 kill();

	 try {
	    if( handler != null ) {
	       handler.sendMessage( android.os.Message.obtain( handler, HopLauncher.MSG_HOPDROID_FAILED, e ) );
	    }
	 } catch( Throwable _ ) {
	    ;
	 }
      }
   }
   
   // kill
   public synchronized void kill() {
      if( !killed ) {
	 Log.i( "HopDroid", ">>> kill serv1=" + serv1 + " serv2=" + serv2 );
	 killed = true;

	 killServers();
	 killPlugins();
	 
	 Log.i( "HopDroid", "<<< kill" );
      }
   }
      
   // isRunning()
   public boolean isRunning() {
      return !killed;
   }

   // close all background connections
   private void closeConnections( Vector conn ) {
      synchronized( conn ) {
	 Enumeration socks = conn.elements();

	 while( socks.hasMoreElements() ) {
	    LocalSocket s = (LocalSocket)socks.nextElement();
	    
	    if( !s.isClosed() ) {
	       try {
		  s.close();
	       } catch( Throwable _ ) {
		  ;
	       }
	    }
	 }
      }
   }
      
   // run hop
   public void run() {
      // handle the session in a background thread (normally very
      // few of these threads are created so there is no need
      // to use a complexe machinery based on thread pool).
      if( serv1 != null ) {
	 thread1 = new Thread( new Runnable () {
	       public void run() {
		  try {
		     while( true ) {
			final LocalSocket sock = serv1.accept();

			synchronized( serv1conn ) {
			   serv1conn.add( sock );
			}

			new Thread( new Runnable() {
			      public void run() {
				 try {
				    server( sock );
				 } finally {
				    synchronized( serv1conn ) {
				       serv1conn.remove( sock );
				    }
				 }
			      }
			   } ).start();
		     }
		  } catch( Throwable e ) {
		     abortError( e, "run" );
		  } finally {
		     closeConnections( serv1conn );
		     thread1 = null;
		  }
	       }
	    } );
	 thread1.start();
      }

      if( serv2 != null ) {
	 thread2 = new Thread( new Runnable () {
	       public void run() {
		  try {
		     while( true ) {
			final LocalSocket sock2 = serv2.accept();
		     
			synchronized( serv2conn ) {
			   serv2conn.add( sock2 );
			}
			
			new Thread( new Runnable() {
			      public void run() {
				 try {
				    serverEvent( sock2 );
				 } finally {
				    synchronized( serv2conn ) {
				       serv2conn.remove( sock2 );
				    }
				 }
			      }
			   } ).start();
		     }
		  } catch( Throwable e ) {
		     abortError( e, "runPushEvent" );
		  } finally {
		     Log.i( "HopDroid", ">>> Closing Connections serv2..." );
		     closeConnections( serv2conn );
		     Log.i( "HopDroid", ">>> Closing Connections serv2..." );
		     thread2 = null;
		  }
	       }
	    } );
	 thread2.start();
      }

      try {
	 thread1.join();
      } catch( Throwable _ ) {
	 ;
      }
      try {
	 thread2.join();
      } catch( Throwable _ ) {
	 ;
      }
   }

   // get plugin
   static protected synchronized int getPlugin( String name ) {
      int s = plugins.size();

      for( int i = 0; i < s; i++ ) {
	 HopPlugin p = (HopPlugin)plugins.get( i );
	 if( name.equals( p.name ) )
	    return i;
      }

      return -1;
   }
   
   // register plugin
   static protected synchronized int registerPlugin( HopPlugin p ) {
      synchronized( plugins ) {
	 plugins.add( p );

	 return plugins.size() - 1;
      }
   }

   // kill plugin
   private synchronized void killPlugins() {
      Log.v( "HopDroid", "killing...plugins" );
      
      if( plugins != null ) {
	 int s = plugins.size();

	 for( int i = 0; i < s; i++ ) {
	    HopPlugin p = (HopPlugin)plugins.get( i );
	    p.kill();
	 }

	 plugins = null;
      }
   }
   
   // handle a session with one client connected to the HopDroid server
   private void server( LocalSocket sock ) {
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

	       if( ip.read() != 127 ) {
		  Log.e( "HopDroid", "Plugin protocol error: " + p.name );
	       }
	       op.flush();
	    } catch( ArrayIndexOutOfBoundsException _ ) {
	       Log.e( "HopDroid", "plugin not found: " + id );
	       // we got an eof, escape from here
	       if( id == -1 ) return;
	       ;
	    }
	 }
      } catch( Throwable e ) {
	 Log.d( "HopDroid", "server error, socket=" + sock + " " +
		e.toString() + " exception=" + e.getClass().getName(),
		e );
	 abortError( e, "server" );
      }
   }
	    
   // registerEvent
   private void serverEvent( LocalSocket sock2 ) {
      Log.i( "HopDroid", "serverEvent " + sock2 );
      try {
	 InputStream ip = sock2.getInputStream();

	 while( true ) {
	    String event = read_string( ip );
	    int a = ip.read();

	    // eof?
	    if( a == -1 ) break;
	    
	    synchronized( eventtable ) {
	       Hashtable ht = (Hashtable)eventtable.get( event );
	    
	       Log.i( "HopDroid", (a == 1 ? "register" : "unregister") +
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
      } catch( Throwable e ) {
	 Log.d( "HopDroid", "serverEvent error: " +
		e.toString() + " exception=" + e.getClass().getName(),
		e );
	 abortError( e, "serverEvent" );
      }
   }

   // killServers
   private synchronized void killServers() {
      Log.i( "HopDroid", ">>> killing servers..." );
      
      try {
	 if( serv1 != null ) {
	    Log.i( "HopDroid", ">>> killing server1..." + serv1 );
	    serv1.close();
	    serv1 = null;
	    if( thread1 != null ) {
	       thread1.join();
	       thread1 = null;
	    }
	    Log.i( "HopDroid", "<<< server1...killed" );
	 }
	 if( serv2 != null  ) {
	    Log.i( "HopDroid", ">>> killing server1..." + serv1 );
	    serv2.close();
	    serv2 = null;
	    if( thread2 != null ) {
	       thread2.join();
	       thread2 = null;
	    }
	    Log.i( "HopDroid", "<<< server2...killed" );
	 }
      } catch( Exception e ) {
	 Log.e( "HopDroid", "closing error: " + e.toString() + " exception=" + e.getClass().getName() );
      }
      
      Log.i( "HopDroid", "<<< servers killed" );
   }
	 
   // hopPushEvent
   static void hopPushEvent( String event, String value ) {
      hopdroid.pushEvent( event, value );
   }
   
   // pushEvent
   public void pushEvent( String event, String value ) {
      // Log.d( "HopDroid", "pushEvent " + event + " " + value );
      synchronized( eventtable ) {
	 Hashtable ht = (Hashtable)eventtable.get( event );

	 if( ht != null ) {
	    Enumeration s = ht.keys();

	    while( s.hasMoreElements() ) {
	       LocalSocket sock = (LocalSocket)s.nextElement();
	       try {
		  OutputStream op = sock.getOutputStream();
		  
		  op.write( "\"".getBytes() );
		  op.write( event.getBytes() );
		  op.write( "\" ".getBytes() );
		  op.write( value.getBytes() );
		  op.write( " ".getBytes() );
		  op.flush();
	       } catch( IOException e ) {
		  Log.e( "HopDroid", "pushEvent error: "
			 + sock + " " + e.getMessage() );
		  ht.remove( sock );
	       }
	    }
	 }
      }
   }

   // read_int
   public static int read_int( InputStream ip ) throws IOException {
      int c = ip.read();

      if( c < 0 ) {
	 throw new EOFException( "read_int: end-of-file" );
      }

      return c;
   }

   // read_int32
   public static int read_int32( InputStream ip ) throws IOException {
      int b0 = ip.read();
      int b1 = ip.read();
      int b2 = ip.read();
      int b3 = ip.read();

      if( b0 < 0 || b1 < 0 || b2 < 0 || b3 < 0 ) {
	 throw new EOFException( "read_int32: end-of-file" );
      }
	 
      return (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
   }

   // read_int64
   public static long read_int64( InputStream ip ) throws IOException {
      int i0 = read_int32( ip );
      int i1 = read_int32( ip );

      return ((long)i0) << 32 | i1;
   }

   // read_float
   public static float read_float( InputStream ip ) throws IOException {
      return Float.valueOf( read_string( ip ) );
   }
   
   // read_string
   public static String read_string( InputStream ip ) throws IOException {
      int sz = read_int32( ip );
      byte[] buf = new byte[ sz ];

      if( ip.read( buf, 0, sz ) < 0 )
	 throw new IOException( "read_string: end-of-file" );

      return new String( buf );
   }

   // read_int64v
   public static long[] read_int64v( InputStream ip ) throws IOException {
      int sz = read_int32( ip );
      long[] v = new long[ sz ];
      
      for( int i = 0; i < sz; i++ ) {
	 v[ i ] = HopDroid.read_int64( ip );
      }

      return v;
   }
   
   // read_stringv
   public static String[] read_stringv( InputStream ip ) throws IOException {
      int sz = read_int32( ip );
      String[] v = new String[ sz ];
      
      for( int i = 0; i < sz; i++ ) {
	 v[ i ] = HopDroid.read_string( ip );
      }

      return v;
   }
}
      
