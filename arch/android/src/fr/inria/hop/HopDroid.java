/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopDroid.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 11 16:16:28 2010                          */
/*    Last change :  Sun Nov  4 18:32:47 2012 (serrano)                */
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
   static final Vector plugins = new Vector( 16 );
   static HopDroid hopdroid = null;
   static final int protocol = 2;

   static final byte SERVER_STOP_CMD = 0;
   static final byte SERVER_PING_CMD = 1;
   static final byte SERVER_EXEC_CMD = 2;

   static final int HOPDROID_STATE_NULL = 0;
   static final int HOPDROID_STATE_INIT = 1;
   static final int HOPDROID_STATE_END = 2;
   static final int HOPDROID_STATE_ERROR = 3;
   static final int HOPDROID_STATE_RUN = 4;
   
   // instance variables
   int state = HOPDROID_STATE_NULL;
   Activity activity = null;
   Service service;
   
   LocalServerSocket pluginserv = null;
   
   LocalServerSocket eventserv = null;
   
   Handler handler = null;
   
   final Hashtable eventtable = new Hashtable();
   
   // constructor
   public HopDroid( Service s ) {
      super();

      service = s;
      hopdroid = this;
      
      try {
	 Log.i( "HopDroid", ">>> starting servers port=" + Hop.port );

	 // register the initial plugins
	 synchronized( plugins ) {
	    if( plugins.size() == 0 ) {
	       registerPlugin( null );
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
	    }
	 }

	 // create the two servers
	 pluginserv = new LocalServerSocket( "hop-plugin:" + Hop.port );
	 eventserv = new LocalServerSocket( "hop-event:" + Hop.port );
	 
	 Log.i( "HopDroid", "<<< servers started..." );
	 
	 state = HOPDROID_STATE_INIT;
      } catch( Exception e ) {
	 state = HOPDROID_STATE_ERROR;
	 abortError( e, "HopDroid" );
      }
   }

   // kill
   public synchronized void kill() {
      if( state != HOPDROID_STATE_END ) {
	 state = HOPDROID_STATE_END;
	 
	 Log.i( "HopDroid", ">>> kill servers, pluginserv=" + pluginserv + " eventserv=" + eventserv );

	 killServers();
	 killPlugins();
	 
	 Log.i( "HopDroid", "<<< kill servers" );
      }
   }
      
   // abort
   public void abortError( Throwable e, String proc ) {
      if( state != HOPDROID_STATE_END ) {
	 Log.e( "HopDroid", "error(" + proc + "): "
		+ e.toString() + " exception=" +
		e.getClass().getName(), e );
      
	 kill();

	 try {
	    if( handler != null ) {
	       handler.sendMessage(
		  android.os.Message.obtain(
		     handler, HopLauncher.MSG_HOPDROID_FAILED, e ) );
	    }
	 } catch( Throwable _ ) {
	    ;
	 }
      }
   }
   
   // close all background connections
   private void closeConnections( Vector conn ) {
      Log.d( "HopDroid", ">>> closeConnections..." );
      synchronized( conn ) {
	 Enumeration socks = conn.elements();

	 while( socks.hasMoreElements() ) {
	    LocalSocket s = (LocalSocket)socks.nextElement();
	    
	    Log.d( "HopDroid", "closeConnections closing connection: " + s );
	    try {
	       if( !s.isClosed() ) {
		  s.close();
	       }
	    } catch( Throwable _ ) {
	       ;
	    }
	 }
      }
      Log.d( "HopDroid", "<<< closeConnections..." );
   }
      
   // isRunning()
   public synchronized boolean isRunning() {
      return state == HOPDROID_STATE_RUN;
   }

   // run hop
   public void run() {
      state = HOPDROID_STATE_RUN;
      
      // the event server handles add-event-listener! registration
      Log.d( "HopDroid", "run eventserv=" + eventserv );
      if( eventserv != null ) {
	 final LocalServerSocket serv = eventserv;
	 new Thread( new Runnable () {
	       public void run() {
		  try {
		     while( true ) {
			final LocalSocket sock = serv.accept();
		     
			new Thread( new Runnable() {
			      public void run() {
				 serverEvent( sock );
			      }
			   } ).start();
		     }
		  } catch( Throwable e ) {
		     abortError( e, "eventserv" );
		  }
	       }
	    } ).start();
      } else {
	 Log.e( "HopDroid", "eventserv null" );
	 kill();
	 return;
      }

      Log.d( "HopDroid", "run pluginserv=" + pluginserv );
      // the plugin server handles plugins actions
      if( pluginserv != null ) {
	 try {
	    serverPlugin();
	    kill();
	 } catch( Throwable e ) {
	    abortError( e, "pluginserv" );
	 }
      } else {
	 Log.e( "HopDroid", "pluginserv null" );
	 kill();
	 return;
      }
   }

   // kill plugin
   private synchronized void killPlugins() {
      int s = plugins.size();

      Log.v( "HopDroid", ">>> killing...plugins" );

      // plugin 0 is null
      for( int i = 1; i < s; i++ ) {
	 HopPlugin p = (HopPlugin)plugins.get( i );
	 if( p != null ) {
	    p.kill();
	 }
      }
      Log.v( "HopDroid", "<<< killing...plugins" );
   }
   
   // handle a session with one client connected to the HopDroid server
   private void serverPlugin() throws Exception {
      final LocalSocket sock = pluginserv.accept();

      try {
	 final InputStream ip = sock.getInputStream();
	 final OutputStream op = sock.getOutputStream();

	 while( true ) {
	    final int version = ip.read();

	    if( version != protocol ) {
	       if( version != -1 ) {
		  Log.e( "HopDroid", "protocol error: incompatible version " +
			 version );
	       }
	       return;
	    }

	    final int cmd = ip.read();

	    if( cmd == HopDroid.SERVER_STOP_CMD ) {
	       return;
	    } else if( cmd == HopDroid.SERVER_PING_CMD ) {
	       final int m = ip.read();
	       if( m != 127 ) {
		  Log.e( "HopDroid", "protocol error: illegal ping mark " + m );
		  return;
	       }
	    } else if( cmd != HopDroid.SERVER_EXEC_CMD ) {
	       Log.e( "HopDroid", "protocol error: illegal command " + cmd );
	       return;
	    } else {
	       final int id = read_int32( ip );
	       
	       try {
		  HopPlugin p = (HopPlugin)plugins.get( id );
		  
		  p.server( ip, op );
		  op.write( " ".getBytes() );

		  final int m = ip.read();
		  
		  if( m != 127 ) {
		     Log.e( "HopDroid", "protocol error: illegal " 
			    + p.name + " mark: " );
		  }
		  op.flush();
	       } catch( ArrayIndexOutOfBoundsException _ ) {
		  Log.e( "HopDroid", "plugin not found: " + id );
		  // we got an eof, escape from here
		  return;
	       }
	    }
	 }
      } catch( Throwable e ) {
	 sock.close();
	 abortError( e, "serverPluging" );
      }
   }
	    
   // registerEvent
   private void serverEvent( LocalSocket sock2 ) {
      try {
	 final InputStream ip = sock2.getInputStream();
      
	 while( true ) {
	    final int version = ip.read();
	 
	    if( version != protocol ) {
	       if( version != -1 ) {
		  Log.e( "HopDroid", "protocol error: incompatible version " +
			 version );
	       }
	       return;
	    }
	    
	    final int cmd = ip.read();

	    if( cmd == HopDroid.SERVER_STOP_CMD ) {
	       sock2.close();
	       return;
	    } else if( cmd == HopDroid.SERVER_PING_CMD ) {
	       final int m = ip.read();
	       sock2.close();
	       if( m != 127 ) {
		  Log.e( "HopDroid", "protocol error: illegal ping mark " + m );
		  return;
	       }
	    } else if( cmd != HopDroid.SERVER_EXEC_CMD ) {
	       Log.e( "HopDroid", "protocol error: illegal command " + cmd );
	       sock2.close();
	       return;
	    } else {
	       String event = read_string( ip );
	       int a = ip.read();
	    
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
	       
	       int m = ip.read();
      
	       if( m != 127 ) {
		  Log.e( "HopDroid", "protocol error: illegal ping mark " + m );
		  return;
	       }
	    }
	 }
      } catch( Throwable e ) {
	 abortError( e, "server2" );
      } finally {
	 // close all the pending connections
	 synchronized( eventtable ) {
	    Enumeration tables = eventtable.elements();

	    while( tables.hasMoreElements() ) {
	       Hashtable t = (Hashtable)tables.nextElement();
	       Enumeration socks = t.keys();

	       while( socks.hasMoreElements() ) {
		  LocalSocket s = (LocalSocket)socks.nextElement();
	    
		  try {
		     if( !s.isClosed() ) {
			s.close();
		     }
		  } catch( Throwable _ ) {
		     ;
		  }
	       }
	    }
	 }
      }
   }

   // killServers
   private synchronized void killServers() {
      Log.d( "HopDroid", ">>> killing servers..." );
      
      try {
	 if( eventserv != null  ) {
	    Log.d( "HopDroid", ">>> server2 killing..." );
	    // connect a socket otherwise accept will not throw an exception
	    serverClose( eventserv );
	    Log.d( "HopDroid", "<<< server2...killed" );
	 }
	 if( pluginserv != null ) {
	    Log.d( "HopDroid", ">>> server1 killing..." );
	    // connect a socket otherwise accept will not throw an exception
	    serverClose( pluginserv );
	    Log.d( "HopDroid", "<<< server1...killed" );
	 }
      } catch( Exception e ) {
	 Log.e( "HopDroid", "closing error: " + e.toString() + " exception=" + e.getClass().getName() );
      }
      
      Log.d( "HopDroid", "<<< servers killed" );
   }

   private static void serverCmd( LocalSocketAddress addr, byte cmd )
      throws Exception {
      LocalSocket ls = new LocalSocket();
      
      ls.connect( addr );
      OutputStream os = ls.getOutputStream();

      // protocol version
      os.write( protocol );

      // command
      os.write( cmd );
	 
      // end mark
      os.write( 127 );
	 
      ls.close();
   }
      
   // serverStop
   private void serverStop( LocalSocketAddress addr ) {
      Log.i( "HopDroid", ">>> stopping server " + addr);
      try {
	 serverCmd( addr, SERVER_STOP_CMD );
      } catch( Exception _ ) {
	 ;
      }
   }

   // serverClose
   private synchronized void serverClose( LocalServerSocket serv ) {
      Log.i( "HopDroid", ">>> stopping server " + serv);
      try {
	 serverCmd( serv.getLocalSocketAddress(), SERVER_STOP_CMD );
	 serv.close();
      } catch( Exception _ ) {
	 ;
      }
   }

   // serverStop
   private static boolean serverPing( LocalSocketAddress addr ) {
      Log.i( "HopDroid", ">>> pinging server " + addr);
      try {
	  serverCmd( addr, SERVER_PING_CMD );
	  return true;
      } catch( Exception _ ) {
	 return false;
      }
   }

   // isServerBackground
   protected static boolean isBackground() {
      LocalSocketAddress addrp =
	 new LocalSocketAddress( "hop-plugin:" + Hop.port );
      LocalSocketAddress addre =
	 new LocalSocketAddress( "hop-event:" + Hop.port );
      
      return serverPing( addrp ) && serverPing( addre );
   }
      
   // get plugin
   static protected synchronized int getPlugin( String name ) {
      int s = plugins.size();

      // plugin 0 is null
      for( int i = 1; i < s; i++ ) {
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
      
