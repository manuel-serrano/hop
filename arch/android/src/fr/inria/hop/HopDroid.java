/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopDroid.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 11 16:16:28 2010                          */
/*    Last change :  Fri Nov 20 07:06:36 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
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
   Exception exn = null;
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
      plugins = new Vector( 16 );
      
      try {
	 Log.d( "HopDroid", "init plugins app=" + HopConfig.APP );
	 String app = HopConfig.APP;
	 
	 state = HOPDROID_STATE_INIT;

	 // register the initial plugins
	 synchronized( plugins ) {
	    registerPlugin( null );
	    registerPlugin( new HopPluginInit( this, "init" ) );

	    if( HopConfig.PLUGINBUILD ) {
	       registerPlugin( new HopPluginBuild( this, "build" ) );
	    }
	    if( HopConfig.PLUGINLOCALE ) {
	       registerPlugin( new HopPluginLocale( this, "locale" ) );
	    }
	    if( HopConfig.PLUGINVIBRATE ) {
	       registerPlugin( new HopPluginVibrate( this, "vibrate" ) );
	    }
	    if( HopConfig.PLUGINMUSICPLAYER ) {
	       registerPlugin( new HopPluginMusicPlayer( this, "musicplayer" ) );
	    }
	    if( HopConfig.PLUGINMEDIAAUDIO ) {
	       registerPlugin( new HopPluginMediaAudio( this, "mediaaudio" ) );
	    }
	    if( HopConfig.PLUGINSENSOR ) {
	       registerPlugin( new HopPluginSensor( this, "sensor" ) );
	    }
	    if( HopConfig.PLUGINBATTERY ) {
	       registerPlugin( new HopPluginBattery( this, "battery" ) );
	    }
	    if( HopConfig.PLUGINSMS ) {
	       registerPlugin( new HopPluginSms( this, "sms" ) );
	    }
	    if( HopConfig.PLUGINWIFI ) {
	       registerPlugin( new HopPluginWifi( this, "wifi" ) );
	    }
	    if( HopConfig.PLUGINCONNECTIVITY ) {
	       registerPlugin( new HopPluginConnectivity( this, "connectivity" ) );
	    }
	    if( HopConfig.PLUGINCONTACT ) {
	       registerPlugin( new HopPluginContact( this, "contact" ) );
	    }
	    if( HopConfig.PLUGINZEROCONF ) {
	       registerPlugin( new HopPluginZeroconf( this, "zeroconf" ) );
	    }
	    if( HopConfig.PLUGINSYSTEM ) {
	       registerPlugin( new HopPluginSystem( this, "system" ) );
	    }
	    if( HopConfig.PLUGINTTS ) {
	       registerPlugin( new HopPluginTts( this, "tts" ) );
	    }
	    if( HopConfig.PLUGINCALL ) {
	       registerPlugin( new HopPluginCall( this, "call" ) );
	    }
	    if( HopConfig.PLUGINUI ) {
	       registerPlugin( new HopPluginUi( this, "ui" ) );
	    }
	 }

	 // previous HopDroid instances might have left open local
	 // server sockets. this closes them.
	 String appPluginName = app + "-plugin:" + Hop.port;
	 String appEventName = app + "-event:" + Hop.port;
	 String appCmdName = app + "-cmd:" + Hop.port;

	 Log.d( "HopDroid", "stopping previous sockets port=" + Hop.port );
	 serverStop( new LocalSocketAddress( appPluginName ), false );
	 serverStop( new LocalSocketAddress( appEventName ), false );
	 serverStop( new LocalSocketAddress( appCmdName ), false );
	 
	 // create the three servers
	 Log.d( "HopDroid", "creating new sockets port=" + Hop.port );
	 pluginserv = new HopLocalServerSocket( appPluginName );
	 eventserv = new HopLocalServerSocket( appEventName );
	 cmdserv = new HopLocalServerSocket( appCmdName  );

      } catch( Exception e ) {
	 state = HOPDROID_STATE_ERROR;

	 Log.e( "HopDroid", "killError state=" + state
		+ " exception="
		+ e.getClass().getName(), e );
	 e.printStackTrace();

	 killServers();
	 killPlugins();

	 try {
	    if( service.handler != null ) {
	       service.handler.sendMessage(
		  android.os.Message.obtain(
		     service.handler, HopLauncher.MSG_HOPDROID_FAIL, e ) );
	    }
	 } catch( Throwable t ) {
	    Log.e( "HopDroid", "error while initializing plugins: " + e );
	    e.printStackTrace();
	 }
	 
      }
   }

   // onConnect
   public void onConnect() {
      // bind the plugins that need the activity
      synchronized( plugins ) {
	 if( HopConfig.PLUGINPREFS ) {
	    registerPlugin( new HopPluginPrefs( this, "prefs" ) );
	 }
      }
      
      // called by HopService.onConnect()
      synchronized( plugins ) {
	 int s = plugins.size();

	 // plugin 0 is null
	 for( int i = 1; i < s; i++ ) {
	    HopPlugin p = (HopPlugin)plugins.get( i );
	    if( p != null ) {
	       p.onConnect();
	    }
	 }
      }
   }
	 
   // waitCmd
   private void waitCmd( int timeout ) {
      // Here again Android is buggous (at least <= 4.1). There is a
      // latency after a close socket during which the socket, while closed,
      // still accept new connections! This function waits the socket to
      // be effectively closed.
      
      for( ;timeout >= 0; timeout-- ) {
	 if( isBackground() && (cmdserv != null) ) {
	    Log.d( "HopDroid", "waitCmd, background="
		   + isBackground()
		   + " ping=" + serverPing( cmdserv.getLocalSocketAddress() )
		   + " cmd.isClosed=" + cmdserv.isClosed()
		   + " timeout=" + timeout );
	 
	    try {
	       if( service.handler != null ) {
		  service.queue.put( "waiting Hop: " + timeout + "\n" );
		  service.handler.sendEmptyMessage(
		     HopLauncher.MSG_HOP_OUTPUT_AVAILABLE );
	       }
	       Thread.sleep( 1000 );
	    } catch( Throwable t ) {
	       Log.e( "HopDroid", "waitCmd error: " + t );
	       t.printStackTrace();
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

	 plugins = null;
	 
	 waitCmd( 10 );
	 
	 if( service.handler != null ) {
	    service.handler.sendEmptyMessage( HopLauncher.MSG_HOPDROID_ENDED );
	 }
      
	 Log.i( "HopDroid", "<<< kill" );
      }
   }
      
   // killError
   private static boolean execCmd( LocalSocketAddress addr, byte cmd )
      throws Exception {
      LocalSocket ls = new LocalSocket();
      
      Log.d( "HopDroid", ">>> execCmd addr=" + addr.getName() + " cmd=" + cmd );
      
      ls.connect( addr );
      Log.d( "HopDroid", "connected" );
      final OutputStream op = ls.getOutputStream();

      // protocol version
      op.write( protocol );

      // command
      op.write( cmd );
         
      // end mark
      op.write( 127 );
      op.flush();
      Log.d( "HopDroid", "flushed" );
      
      ls.close();
      Log.d( "HopDroid", "close" );

      Log.d( "HopDroid", "<<< execCmd addr=" + addr.getName() + " cmd=" + cmd );
      return true;
   }
      
   // serverStop
   private static void serverStop( LocalSocketAddress addr, boolean notify ) {
      try {
         execCmd( addr, SERVER_STOP_CMD );
      } catch( Exception e ) {
	 if( notify ) {
	    Log.d( "HopDroid", "serverStop failed: " + e );
	    e.printStackTrace();
	 }
      }
   }

   // serverPing
   private static boolean serverPing( LocalSocketAddress addr ) {
      try {
	 return execCmd( addr, SERVER_PING_CMD );
      } catch( Exception e ) {
	 Log.e( "HopDroid", "serverPing error: " + e );
	 e.printStackTrace();
         return false;
      }
   }

   // killBackground
   protected static void killBackground() {
      LocalSocketAddress addr =
         new LocalSocketAddress( HopConfig.APP + "-cmd:" + Hop.port );

      Log.i( "HopDroid", "KillBackground, serverStop: " + addr.getName() );

      serverStop( addr, true );
   }

   // emergencyExit
   protected static void emergencyExit() {
      Log.i( "HopDroid", ">>> emergencyExit..." );
      if( isBackground() ) {
	 Log.i( "HopDroid", ">>> emergencyExit, killBackground..." );
	 killBackground();
	 Log.i( "HopDroid", "<<< emergencyExit, isBackground..." + isBackground() );
      }
      Log.i( "HopDroid", "<<< emergencyExit" );
   }
   
   // isBackground()
   protected static boolean isBackground() {
      LocalSocketAddress addr =
         new LocalSocketAddress( "hopdroid-cmd:" + Hop.port );

      return serverPing( addr );
   }

/*    // isRunning()                                                   */
/*    public synchronized boolean isRunning() {                        */
/*       return state == HOPDROID_STATE_RUN;                           */
/*    }                                                                */

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
		     Log.e( "HopDroid", "eventserv error: " + e );
		     e.printStackTrace();
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
		     Log.e( "HopDroid", "pluginserv error: " + e );
		     e.printStackTrace();
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
		  Log.d( "HopDroid", "serverPlugin, connection reset by peer protocol=" + version );
	       }
	       return;
	    }

	    final int id = read_int32( ip );
	       
	    try {
	       HopPlugin p = (HopPlugin)plugins.get( id );

	       Log.d( "HopDroid", "plugin=" + p + " id=" + id );
	       p.server( ip, op );

	       final int m = ip.read();
		  
	       if( m != 127 ) {
		  Log.e( "HopDroid", "protocol error: illegal " 
			 + p.name + " mark: " + m );
	       }
	       op.flush();
	    } catch( ArrayIndexOutOfBoundsException a ) {
	       Log.e( "HopDroid", "plugin not found: " + id );
	       // we got an eof, escape from here
	       return;
	    }
	 }
      } catch( Throwable e ) {
	 Log.e( "HopDroid", "serverPlugin error (catched)  " + e );
	 e.printStackTrace();
	 
	 if( !pluginserv.isClosed() ) {
	    try {
	       pluginclient.close();
	       pluginclient = null;
	    } catch( Throwable ne ) {
	       Log.e( "HopDroid", "error while closing plugin client: " + e );
	       e.printStackTrace();
	    }
	 }
      } finally {
	 Log.d( "HopDroid", "exiting server loop!" );
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
	 Log.e( "HopDroid", "serverEvent error " + e );
	 e.printStackTrace();
	 
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
		  Log.e( "HopDroid", "Cannot close cmd client: " + t );
		  t.printStackTrace();
	       }
	    }
	 }
      } catch( Throwable e ) {
	 Log.e( "HopDroid", "cmdserv error: " + e );
	 e.printStackTrace();
      } finally {
	 cmdserv.close();
	 Log.d( "HopDroid", "serverCmd ended." );
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
      Log.d( "HopDroid", "safeClose" );
      if( client == null ) {
	 try {
	    LocalSocket ls = new LocalSocket();

	    ls.connect( serv.getLocalSocketAddress() );
	    ls.close();
	 } catch( Throwable e ) {
	    Log.e( "HopDroid", "safeClose error, cannot connect, serv="
		   + serv + " exc=" + e );
	    e.printStackTrace();
	 }
      } else {
	 try {
	    client.close();
	 } catch( Throwable e ) {
	    Log.e( "HopDroid", "safeClose error, serv="
		   + serv + " client=" + client + " exc=" + e );
	    e.printStackTrace();
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
	       serverStop( cmdserv.getLocalSocketAddress(), true );
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

   // pushEvent
   public void pushEvent( String event, String value ) {
      synchronized( eventtable ) {
	 Integer i = (Integer)eventtable.get( event );

	 Log.i( "HopDroid", "pushEvent event=[" + event + "] value=["
		+ value + "] i=" + i );
	 if( (i != null) && (i > 0) ) {
	    try {
	       final OutputStream op = eventclient.getOutputStream();
	       Log.i( "HopDroid", "pushEvent event=[" + event + "] op=" + op );

	       if( op != null ) {
		  op.write( "\"".getBytes() );
		  op.write( event.getBytes() );
		  op.write( "\" ".getBytes() );
		  op.write( value.getBytes() );
		  op.write( " ".getBytes() );
		  op.flush();
	       }
	    } catch( Exception e ) {
	       Log.e( "HopDroid", "pushEvent error: " + e );
	       e.printStackTrace();
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
	 Log.e( "HopDroid", "cannot close server "
		+ this.getLocalSocketAddress().getName() + " ("
		+ this + "): " + e );
	 e.printStackTrace();
      }
   }

   synchronized boolean isClosed() {
      return closed;
   }
}
