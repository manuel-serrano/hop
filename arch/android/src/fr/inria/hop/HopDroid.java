/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopDroid.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 11 16:16:28 2010                          */
/*    Last change :  Wed Nov 21 19:54:54 2012 (serrano)                */
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
   
   HopLocalServerSocket pluginserv = null;
   LocalSocket pluginclient = null;
   
   HopLocalServerSocket eventserv = null;
   LocalSocket eventclient = null;
   
   HopLocalServerSocket cmdserv = null;
   
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
	 pluginserv = new HopLocalServerSocket( "hopdroid-plugin:" + Hop.port );
	 eventserv = new HopLocalServerSocket( "hopdroid-event:" + Hop.port );
	 cmdserv = new HopLocalServerSocket( "hopdroid-cmd:" + Hop.port );

	 state = HOPDROID_STATE_INIT;
      } catch( Exception e ) {
	 Log.d( "HopDroid", "initialization failed" );
	    
	 state = HOPDROID_STATE_ERROR;
	 killError( e, "HopDroid" );
      }
   }

   // waitCmdServSocket
   private void waitCmdServSocket( int timeout ) {
      // Here again Android is buggous (at least <= 4.1). There is a
      // latency after a close socket during which the socket, while closed,
      // still accept new connections! This function waits the socket to
      // be effectively closed.
      
      for( ; timeout >= 0; timeout-- ) {
	 Log.d( "HopDroid", "checking ping...bg="
		+ isBackground()
		+ " ping=" + serverPing( cmdserv.getLocalSocketAddress() )
		+ " cmd.isClosed=" + cmdserv.isClosed()
		+ " timeout=" + timeout );
	 
	 if( isBackground() ) {
	    try {
	       if( service.handler != null ) {
		  service.queue.put( "waiting Hop: " + timeout );
		  service.handler.sendEmptyMessage(
		     HopLauncher.MSG_HOP_OUTPUT_AVAILABLE );
	       }
	       Thread.sleep( 1000 );
	    } catch( Throwable _) {
	       ;
	    }
	 } else {
	    return;
	 }
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
	 
	 if( service.handler != null ) {
	    waitCmdServSocket( 10 );
	    Log.d( "HopDroid", "sending MSG_HOPDROID_ENDED" );
	    service.handler.sendEmptyMessage( HopLauncher.MSG_HOPDROID_ENDED );
	 }
      
	 Log.i( "HopDroid", "<<< kill" );
      }
   }
      
   // killError
   public void killError( Throwable e, String proc ) {
      if( state != HOPDROID_STATE_END ) {

	 Log.e( "HopDroid", "killError state=" + state
		+ " proc=" + proc
		+ " exception="
		+ e.getClass().getName(), e );
	 e.printStackTrace();

	 try {
	    if( service.handler != null ) {
	       Log.d( "HopDroid", "sending MSG_HOPDROID_FAILED" );
	       service.handler.sendMessage(
		  android.os.Message.obtain(
		     service.handler, HopLauncher.MSG_HOPDROID_FAILED, e ) );
	    }
	 } catch( Throwable _ ) {
	    ;
	 }
      }
	 
      Log.i( "HopDroid", "killing after error..." );
      kill();
   }
   
   private static boolean execCmd( LocalSocketAddress addr, byte cmd )
      throws Exception {
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

      return true;
   }
      
   // serverStop
   private static void serverStop( LocalSocketAddress addr ) {
      try {
         execCmd( addr, SERVER_STOP_CMD );
      } catch( Exception e ) {
	 Log.d( "HopDroid", "serverStop failed: " + e );
	 e.printStackTrace();
      }
   }

   // serverPing
   private static boolean serverPing( LocalSocketAddress addr ) {
      try {
	 Log.d( "HopDroid", "serverPing " + addr.getName() );
	 return execCmd( addr, SERVER_PING_CMD );
      } catch( Exception _ ) {
	 Log.d( "HopDroid", "serverPing failed " + addr.getName() );
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
		  } catch( Throwable e ) {
		     ;
		  } finally {
		     killCmdServer();
		  }
	       }
	    } ).start();
      } else {
	 Log.e( "HopDroid", "eventserv null" );
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
		  } catch( Throwable e ) {
		     ;
		  } finally {
		     killCmdServer();
		  }
	       }
	    } ).start();
      } else {
	 Log.e( "HopDroid", "pluginserv null" );
	 return;
      }

      // the cmd server handles ping and stop commands
      serverCmd();
      
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
	 if( !pluginserv.isClosed() ) {
	    try {
	       pluginclient.close();
	       pluginclient = null;
	    } catch( Throwable ne ) {
	       ;
	    }
	 }
      } finally {
	 pluginserv.close();
	 killCmdServer();
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
	 if( !eventserv.isClosed() ) {
	    eventclient.close();
	    eventclient = null;
	 }
      } finally {
	 eventserv.close();
	 killCmdServer();
      }
   }

   // serverCmd
   private void serverCmd() {
      LocalSocket sock = null;
      
      Log.d( "HopDroid", "starting serverCmd="
	     + cmdserv.getLocalSocketAddress() + " " 
	     + cmdserv.getLocalSocketAddress().getName() );

      try {
	 while( true ) {
	    sock = cmdserv.accept();
	    final InputStream ip = sock.getInputStream();
	    
	    try {
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
		  return;
	       } else if( cmd == HopDroid.SERVER_PING_CMD ) {
		  Log.d( "HopDroid", "serverCmd cmd=SERVER_PING_CMD" );
	       } else {
		  Log.e( "HopDroid", "protocol error: illegal command " + cmd );
		  return;
	       }
	    } finally {
	       try {
		  if( sock != null ) {
		     sock.close();
		     Log.d( "HopDroid", "close cmd client" );
		  }
	       } catch( Throwable t ) {
		  Log.d( "HopDroid", "Cannot close cmd client: " + t );
		  t.printStackTrace();
	       }
	    }
	 }
      } catch( Throwable e ) {
	 Log.e( "HopDroid", "cmdserv error" );
	 e.printStackTrace();
      } finally {
	 cmdserv.close();
      }
   }

   // kill plugin
   private synchronized void killPlugins() {
      int s = plugins.size();

      Log.d( "HopDroid", ">>> killingPlugins" );

      // plugin 0 is null
      for( int i = 1; i < s; i++ ) {
	 HopPlugin p = (HopPlugin)plugins.get( i );
	 if( p != null ) {
	    p.kill();
	 }
      }
      Log.d( "HopDroid", "<<< killingPlugins" );
   }

   // safeclose
   private void safeClose( HopLocalServerSocket serv, LocalSocket client ) {
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
	    Log.d( "HopDroid", "safeClose error, cannot connect, serv="
		   + serv + " exc=" + e );
	 }
      } else {
	 try {
	    client.close();
	 } catch( Throwable e ) {
	    Log.d( "HopDroid", "safeClose error, serv="
		   + serv + " client=" + client + " exc=" + e );
	 }
      }

      serv.close();
   }

   // killCmdServer
   private synchronized void killCmdServer() {
      // cmd server
      if( cmdserv != null ) {
	 synchronized( cmdserv ) {
	    if( !cmdserv.isClosed() ) {
	       serverStop( cmdserv.getLocalSocketAddress() );
	    }
	 }
      }
   }
      
   // killServers
   private synchronized void killServers() {
      Log.d( "HopDroid", ">>> killServers..." );
      
      // event server
      if( eventserv != null ) {
	 safeClose( eventserv, eventclient );
	 eventclient = null;
      }

      // plugin server
      if( pluginserv != null ) {
	 safeClose( pluginserv, pluginclient );
	 pluginclient = null;
      }

      // cmd server
      killCmdServer();
      
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
      
/*---------------------------------------------------------------------*/
/*    HopLocalServerSocket                                             */
/*---------------------------------------------------------------------*/
class HopLocalServerSocket extends LocalServerSocket {
   boolean closed = false;
   
   HopLocalServerSocket( String name ) throws IOException {
      super( name );
      Log.d( "HopDroid", "creating server name=" + name + " this=" + this );
   }

   public synchronized void close() {
      try {
	 if( !closed ) {
	    
	    super.close();
	    
	    closed = true;
	    Log.d( "HopDroid", "server "
		   + this.getLocalSocketAddress().getName() + " ("
		   + this + ") closed" );
	 }
      } catch( Throwable e ) {
	 Log.d( "HopDroid", "cannot close server "
		+ this.getLocalSocketAddress().getName() + " ("
		+ this + "): " + e );
      }
   }

   synchronized boolean isClosed() {
      return closed;
   }
}
