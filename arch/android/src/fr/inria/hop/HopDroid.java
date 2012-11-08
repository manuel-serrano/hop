/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopDroid.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 11 16:16:28 2010                          */
/*    Last change :  Thu Nov  8 09:56:33 2012 (serrano)                */
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
   LocalSocket eventclient = null;
   
   LocalServerSocket cmdserv = null;
   
   Handler handler = null;
   
   final Hashtable eventtable = new Hashtable();
   
   // constructor
   public HopDroid( Service s ) {
      super();

      service = s;
      hopdroid = this;
      plugins = new Vector( 16 );
      
      try {
	 Log.i( "HopDroid", "create hopdroid=" + hopdroid );

	 // register the initial plugins
	 synchronized( plugins ) {
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

	 // create the two servers
	 pluginserv = new LocalServerSocket( "hopdroid-plugin:" + Hop.port );
	 eventserv = new LocalServerSocket( "hopdroid-event:" + Hop.port );
	 cmdserv = new LocalServerSocket( "hopdroid-cmd:" + Hop.port );
	 
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
	 
	 Log.i( "HopDroid", ">>> kill droid" );

	 killServers();
	 killPlugins();

	 hopdroid = null;
	 plugins = null;
	 
	 Log.i( "HopDroid", "<<< kill droid" );
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
   
   private static boolean execCmd( LocalSocketAddress addr, byte cmd )
      throws Exception {
      LocalSocket ls = new LocalSocket();
      
      Log.d( "HopDroid", ">>> execCmd..." + cmd );
      ls.connect( addr );
      Log.d( "HopDroid", "<<< execCmd..." + cmd + " connected" );
      
      final OutputStream op = ls.getOutputStream();

      // protocol version
      op.write( protocol );

      // command
      op.write( cmd );
         
      // end mark
      op.write( 127 );

      ls.close();
      Log.d( "HopDroid", "<<< execCmd..." + cmd + " close" );

      return true;
   }
      
   // serverStop
   private void serverStop( LocalSocketAddress addr ) {
      try {
         execCmd( addr, SERVER_STOP_CMD );
      } catch( Exception _ ) {
         ;
      }
   }

   // serverStop
   private static boolean serverPing( LocalSocketAddress addr ) {
      Log.d( "HopDroid", ">>> ping..." );
      try {
          return execCmd( addr, SERVER_PING_CMD );
      } catch( Exception _ ) {
	 Log.d( "HopDroid", "<<< ping ... false" );
         return false;
      }
   }

   // isBackground()
   protected static boolean isBackground() {
      LocalSocketAddress addr =
         new LocalSocketAddress( "hopdroid-cmd:" + Hop.port );
      
      return serverPing( addr );
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
	 new Thread( new Runnable () {
	       public void run() {
		  try {
		     serverEvent();
		     kill();
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

      // the plugin server handles plugins actions
      Log.d( "HopDroid", "run pluginserv=" + pluginserv );
      if( pluginserv != null ) {
	 new Thread( new Runnable () {
	       public void run() {
		  try {
		     serverPlugin();
		     kill();
		  } catch( Throwable e ) {
		     abortError( e, "pluginserv" );
		  }
	       }
	    } ).start();
      } else {
	 Log.e( "HopDroid", "pluginserv null" );
	 kill();
	 return;
      }

      // the cmd server handles ping and stop commands
      try {
	 serverCmd();
      } catch( Throwable e ) {
	 abortError( e, "cmdserv" );
      } finally {
	 Log.i( "HopDroid", "serverCmd loop exited..." );
      }
      
      kill();
   }

   // handle a session with one client connected to the HopDroid server
   private void serverPlugin() throws Exception {
      final LocalSocket sock = pluginserv.accept();
      final InputStream ip = sock.getInputStream();
      final OutputStream op = sock.getOutputStream();

      Log.i( "HopDroid", "serverPlugin connected sock=" + sock );

      try {
	 while( true ) {
	    final int version = ip.read();

	    if( version != protocol ) {
	       if( version != -1 ) {
		  Log.e( "HopDroid", "protocol error: incompatible version " +
			 version );
	       }
	       return;
	    }

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
      } catch( Throwable e ) {
	 sock.close();
	 abortError( e, "serverPlugin" );
      } finally {
	 Log.i( "HopDroid", "serverPlugin loop exited..." );
      }
   }
	    
   // registerEvent
   private void serverEvent() throws Exception {
      eventclient = eventserv.accept();
      final InputStream ip = eventclient.getInputStream();

      Log.i( "HopDroid", "serverEvent connected eventserv=" +
	     eventserv + " client=" + eventclient );
      
      try {
	 while( true ) {
	    final int version = ip.read();
	 
	    if( version != protocol ) {
	       if( version != -1 ) {
		  Log.e( "HopDroid", "protocol error: incompatible version " +
			 version );
	       }
	       return;
	    }
	    
	    String event = read_string( ip );
	    int a = ip.read();
	    
	    synchronized( eventtable ) {
	       Integer i = (Integer)eventtable.get( event );
	    
	       Log.i( "HopDroid", (a == 1 ? "register" : "unregister") +
		      " event [" + event + "]" );
	       // a == 1, add an event listener. a == 0, remove listener
	       if( i == null ) {
		  if( a == 1 ) {
		     eventtable.put( event, new Integer( 1 ) );
		  }
	       } else {
		  int ni = i + a == 1 ? 1 : -1;
		  
		  if( ni == 0 ) {
		     eventtable.remove( event );
		  } else {
		     eventtable.put( eventtable, new Integer( ni ) );
		  }
	       }
	    }
	       
	    int m = ip.read();
      
	    if( m != 127 ) {
	       Log.e( "HopDroid", "protocol error: illegal ping mark " + m );
	       return;
	    }
	 }
      } catch( Throwable e ) {
	 eventclient.close();
	 abortError( e, "eventserv" );
      } finally {
	 Log.d( "HopDroid", "serverEvent loop exited..." );
      }
   }

   // serverCmd
   private void serverCmd() throws Exception {
      while( true ) {
	 final LocalSocket sock = cmdserv.accept();
	 final InputStream ip = sock.getInputStream();

	 final int version = ip.read();

	 if( version != protocol ) {
	    if( version != -1 ) {
	       Log.e( "HopDroid", "protocol error: incompatible version " +
		      version );
	    }
	    return;
	 }

	 final int cmd = ip.read();
	 final int m = ip.read();

	 if( m != 127 ) {
	    Log.e( "HopDroid", "protocol error: illegal ping mark " + m );
	    return;
	 }
	 
	 if( cmd == HopDroid.SERVER_STOP_CMD ) {
	    Log.d( "HopDroid", "SERVER_STOP_CMD" );
	    sock.close();
	    return;
	 } else if( cmd == HopDroid.SERVER_PING_CMD ) {
	    Log.d( "HopDroid", "SERVER_PING_CMD" );
	    sock.close();
	 } else {
	    Log.e( "HopDroid", "protocol error: illegal command " + cmd );
	    sock.close();
	    return;
	 }
      }
   }

   // kill plugin
   private synchronized void killPlugins() {
      int s = plugins.size();

      Log.d( "HopDroid", ">>> killing...plugins" );

      // plugin 0 is null
      for( int i = 1; i < s; i++ ) {
	 HopPlugin p = (HopPlugin)plugins.get( i );
	 if( p != null ) {
	    p.kill();
	 }
      }
      Log.d( "HopDroid", "<<< killing...plugins" );
   }

   // killServers
   private synchronized void killServers() {
      Log.d( "HopDroid", ">>> killing servers..." );
      
      // event server
      if( eventserv != null ) {
	 try { eventserv.close(); } catch( Exception _ ) { ; }
	 eventserv = null;
      }

      if( eventclient != null ) {
	 try { eventclient.close(); } catch( Exception _ ) { ; }
	 eventclient = null;
      }
      
      // plugin server
      if( pluginserv != null ) {
	 try { pluginserv.close(); } catch( Exception _ ) { ; }
	 pluginserv = null;
      }

      // cmd server
      if( cmdserv != null ) {
	 serverStop( cmdserv.getLocalSocketAddress() );
	 try { cmdserv.close(); } catch( Exception _ ) { ; }
	 cmdserv = null;
      }
      
      Log.d( "HopDroid", "<<< servers killed" );
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
      synchronized( eventtable ) {
	 Integer i = (Integer)eventtable.get( event );

	 if( (i != null) && (i > 0) ) {
	    try {
	       final OutputStream op = eventclient.getOutputStream();

	       if( op != null ) {
		  op.write( "\"".getBytes() );
		  op.write( event.getBytes() );
		  op.write( "\" ".getBytes() );
		  op.write( value.getBytes() );
		  op.write( " ".getBytes() );
		  op.flush();
	       }
	    } catch( Exception e ) {
	       Log.e( "HopDroid", "pushEvent error: " + e.getMessage() );
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
      
