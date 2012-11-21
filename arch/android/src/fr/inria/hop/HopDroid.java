/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopDroid.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 11 16:16:28 2010                          */
/*    Last change :  Wed Nov 21 11:37:44 2012 (serrano)                */
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

   static final byte SERVER_STOP_CMD = 10;
   static final byte SERVER_PING_CMD = 11;
   static final byte SERVER_EXEC_CMD = 12;

   static final int HOPDROID_STATE_NULL = 0;
   static final int HOPDROID_STATE_INIT = 1;
   static final int HOPDROID_STATE_END = 2;
   static final int HOPDROID_STATE_ERROR = 3;
   static final int HOPDROID_STATE_RUN = 4;
   
   // instance variables
   int state = HOPDROID_STATE_NULL;
   Activity activity = null;
   HopService service;
   
   LocalServerSocket pluginserv = null;
   LocalSocket pluginclient = null;
   
   LocalServerSocket eventserv = null;
   LocalSocket eventclient = null;
   
   LocalServerSocket cmdserv = null;
   
   final Hashtable eventtable = new Hashtable();
   
   // constructor
   public HopDroid( HopService s ) {
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
	    registerPlugin( new HopPluginSystem( this, "system" ) );
	 
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
	 
	 Log.i( "HopDroid", ">>> kill..." );

	 killServers();
	 killPlugins();

	 hopdroid = null;
	 plugins = null;
	 
	 Log.i( "HopDroid", "<<< kill" );
      }
   }
      
   // abort
   public void abortError( Throwable e, String proc ) {
      if( state != HOPDROID_STATE_END ) {
	 Log.e( "HopDroid", "error(" + proc + "): "
		+ e.toString() + " exception=" +
		e.getClass().getName(), e );

	 Log.i( "HopDroid", "killing after error..." );
	 
	 kill();

	 try {
	    if( service.handler != null ) {
	       service.handler.sendMessage(
		  android.os.Message.obtain(
		     service.handler, HopLauncher.MSG_HOPDROID_FAILED, e ) );
	    }
	 } catch( Throwable _ ) {
	    ;
	 }
      }
   }
   
   private static boolean execCmd( LocalSocketAddress addr, byte cmd )
      throws Exception {
      Log.d( "HopDroid", ">>> execCmd cmd=" + cmd );
      LocalSocket ls = new LocalSocket();
      
      ls.connect( addr );
      
      final OutputStream op = ls.getOutputStream();

      // protocol version
      op.write( protocol );

      // command
      op.write( cmd );
         
      // end mark
      op.write( 127 );
      op.flush();

      ls.close();
      Log.d( "HopDroid", "<<< execCmd cmd=" + cmd );

      return true;
   }
      
   // serverStop
   private static void serverStop( LocalSocketAddress addr ) {
      try {
         execCmd( addr, SERVER_STOP_CMD );
      } catch( Exception _ ) {
         ;
      }
   }

   // serverStop
   private static boolean serverPing( LocalSocketAddress addr ) {
      try {
          return execCmd( addr, SERVER_PING_CMD );
      } catch( Exception _ ) {
         return false;
      }
   }

   // killBackground()
   protected static void killBackground() {
      LocalSocketAddress addr =
         new LocalSocketAddress( "hopdroid-cmd:" + Hop.port );
      
      serverStop( addr );
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
      Log.d( "HopDroid", "starting eventServ=" + eventserv + " " 
	     + eventserv.getLocalSocketAddress().getName() );
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
      Log.d( "HopDroid", "starting pluginServ=" + pluginserv + " "
	     + pluginserv.getLocalSocketAddress().getName() );
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
      pluginclient = pluginserv.accept();
      final InputStream ip = pluginclient.getInputStream();
      final OutputStream op = pluginclient.getOutputStream();

      Log.i( "HopDroid", "serverPlugin connected sock=" + pluginclient );

      try {
	 while( true ) {
	    final int version = ip.read();

	    if( version != protocol ) {
	       if( version != -1 ) {
		  Log.e( "HopDroid", "serverPlugin protocol error: incompatible version " +
			 version );
	       } else {
		  Log.d( "HopDroid", "serverPlugin, connection reset by peer" );
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
	 try {
	    pluginclient.close();
	 } catch( Throwable ne ) {
	    ;
	 }

	 if( service.inrestart || service.inkill ) {
	    kill();
	 } else {
	    abortError( e, "serverPlugin" );
	 }
      } finally {
	 Log.d( "HopDroid", "serverPlugin loop exited..." );
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
      Log.d( "HopDroid", "starting serverCmd="
	     + cmdserv.getLocalSocketAddress() + " " 
	     + cmdserv.getLocalSocketAddress().getName() );
      
      while( true ) {
	 final LocalSocket sock = cmdserv.accept();
	 final InputStream ip = sock.getInputStream();

	 Log.d( "HopDroid", "serverCmd connection established " + sock );

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
	    Log.d( "HopDroid", "serverCmd cmd=SERVER_STOP_CMD" );
	    sock.close();
	    return;
	 } else if( cmd == HopDroid.SERVER_PING_CMD ) {
	    Log.d( "HopDroid", "serverCmd cmd=SERVER_PING_CMD" );
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

   // safeclose
   private void safeClose( LocalServerSocket serv, LocalSocket client ) {
      // Android is buggous, closing a server that has not accepted 
      // any connection yet, raises no exceptions and then does not
      // unblock the pending accept call. Our function safeclose establishes
      // a fake connection and closes it at once to work around this
      // Android error.
      if( client == null ) {
	 try {
	    LocalSocket ls = new LocalSocket();

	    ls.connect( serv.getLocalSocketAddress() );
	    ls.close();
	 } catch( Throwable e ) {
	    Log.d( "HopDroid", "safeClose error: " + e );
	 }
      } else {
	 try {
	    client.close();
	 } catch( Throwable e ) {
	    Log.d( "HopDroid", "safeClose error: " + e );
	 }
      }

      try {
	 serv.close();
      } catch( Exception e ) {
	 Log.d( "HopDroid", "Cannot close serv socket: " + e );
      }
   }
      
   // killServers
   private synchronized void killServers() {
      Log.d( "HopDroid", ">>> killServers..." );
      
      // event server
      Log.d( "HopDroid", "--- killing eventclient..." );
      safeClose( eventserv, eventclient );
      eventclient = null;
      eventserv = null;

      // plugin server
      Log.d( "HopDroid", "--- killing pluginclient..." );
      safeClose( pluginserv, pluginclient );
      pluginclient = null;
      pluginserv = null;

      // cmd server
      if( cmdserv != null ) {
	 Log.d( "HopDroid", "--- killing cmdserv..." );
	 serverStop( cmdserv.getLocalSocketAddress() );
	 try {
	    cmdserv.close();
	 } catch( Exception e ) {
	    Log.d( "HopDroid", "Cannot close cmdserv socket: " + e );
	 }
	 cmdserv = null;
      }
      
      Log.d( "HopDroid", "<<< killServers" );
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
      
