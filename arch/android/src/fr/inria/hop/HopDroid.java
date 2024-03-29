/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopDroid.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 11 16:16:28 2010                          */
/*    Last change :  Sun Mar  6 10:02:51 2022 (serrano)                */
/*    Copyright   :  2010-22 Manuel Serrano                            */
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
   static final byte SERVER_ABORT_CMD = 13;
   static final byte SERVER_RESET_CMD = 14;

   static final int HOPDROID_STATE_NULL = 0;
   static final int HOPDROID_STATE_INIT = 1;
   static final int HOPDROID_STATE_END = 2;
   static final int HOPDROID_STATE_ERROR = 3;
   static final int HOPDROID_STATE_RUN = 4;
   static final int HOPDROID_STATE_REBOOT = 5;

   static String HOPDROID = "HopDroid";
   
   // instance variables
   int state = HOPDROID_STATE_NULL;
   Exception exn = null;
   Activity activity = null;
   HopService service;
   Class activityclass = null;
   
   HopLocalServerSocket pluginserv = null;
   LocalSocket pluginclient = null;
   
   HopLocalServerSocket eventserv = null;
   LocalSocket eventclient = null;
   
   HopLocalServerSocket cmdserv = null;
   
   final Hashtable eventtable = new Hashtable();

   boolean delayed_start_notification = false;
   
   // constructor
   public HopDroid( HopService s, Class c ) {
      super();

      service = s;
      plugins = new Vector( 16 );
      activityclass = c;

      HOPDROID = "HopDroid/" + c.getName();
      
      try {
	 state = HOPDROID_STATE_INIT;

	 initPlugins();
	 initServers();
      } catch( Exception e ) {
	 Log.e( HOPDROID, "error in constructor state=" + state
		+ " exception="
		+ e.getClass().getName(), e );
	 e.printStackTrace();

	 state = HOPDROID_STATE_ERROR;
	 
	 killServers();
	 killPlugins();

	 try {
	    if( service != null && service.handler != null ) {
	       service.handler.sendMessage(
		  android.os.Message.obtain(
		     service.handler, HopLauncher.MSG_HOPDROID_FAIL, e ) );
	    }
	 } catch( Throwable t ) {
	    Log.e( HOPDROID, "error while initializing plugins: " + e );
	    e.printStackTrace();
	 }
	 
      }
   }

   // initPlugins
   void initPlugins() throws Exception {
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
	 if( HopConfig.PLUGININTENT ) {
	    registerPlugin( new HopPluginIntent( this, "intent" ) );
	 }
      }
   }

   // initServers
   void initServers() throws Exception {
      Log.d( HOPDROID, "initServers app=" + HopConfig.APP );
      String app = HopConfig.APP;
	 
      // previous HopDroid instances might have left open local
      // server sockets. this closes them.
      String appPluginName = app + "-plugin:" + Hop.port;
      String appEventName = app + "-event:" + Hop.port;
      String appCmdName = app + "-cmd:" + Hop.port;

      // create the three servers
      pluginserv = new HopLocalServerSocket( appPluginName );
      Log.d( HOPDROID, "pluginserv=" + pluginserv.toString() );
      
      eventserv = new HopLocalServerSocket( appEventName );
      Log.d( HOPDROID, "eventserv=" + eventserv.toString() );
      
      cmdserv = new HopLocalServerSocket( appCmdName  );
      Log.d( HOPDROID, "cmdserv=" + cmdserv.toString() );
   }

   // run hopdroid
   public void run() {
      state = HOPDROID_STATE_RUN;

      // the event server handles add-event-listener! registration
      Log.d( HOPDROID, "run..." );

      // spawn eventserv
      new Thread( new Runnable () {
	    public void run() {
	       String abortreason = "server teminated";
	       
	       try {
		  serverEvent();
	       } catch( Throwable e ) {
		  Log.e( HOPDROID, "eventserv error: " + e );
		  e.printStackTrace();
		  
		  abortreason = "server error: " + e.toString();
		  
		  synchronized( this ) {
		     if( eventserv != null ) {
			// might have been set to null by safeclose
			eventserv.close();
		     }
		  }
	       } finally {
		  abortCmdServer( abortreason );
	       }
	    }
	 } ).start();

      // spawn pluginserv
      new Thread( new Runnable () {
	    public void run() {
	       try {
		  serverPlugin();
	       } catch( Throwable e ) {
		  Log.e( HOPDROID, "pluginserv error: " + e );
		  e.printStackTrace();
		  
		  // might have been set to null by safeclose
		  synchronized( this ) {
		     if( pluginserv != null ) {
			pluginserv.close();
		     }
		  }
	       } finally {
		  abortCmdServer( "pluginserv error" );
	       }
	    }
	 } ).start();

      synchronized( this ) {
	 if( service.handler != null ) {
	    service.handler.sendEmptyMessage( HopLauncher.MSG_HOPDROID_START );
	 } else {
	    delayed_start_notification = true;
	 }
      }
      
      // the cmd server handles ping and stop commands
      try {
	 LocalSocket sock = null;

	 while( true ) {
	    sock = cmdserv.accept();
	    final InputStream ip = sock.getInputStream();
	    
	    try {
	       final int version = ip.read();

	       if( version != protocol ) {
		  throw new IOException( "hopdroid: protocol error: incompatible version " + version );
	       }

	       final int cmd = ip.read();
	       final int m = ip.read();

	       if( m != 127 ) {
		  throw new IOException( "HopDroid: protocol error: illegal ping mark " + m );
	       }

	       switch( cmd ) {
		  case HopDroid.SERVER_STOP_CMD:
		     Log.d( HOPDROID, "serverCmd cmd=SERVER_STOP_CMD" );
		     return;

		  case HopDroid.SERVER_PING_CMD:
		     Log.d( HOPDROID, "serverCmd cmd=SERVER_PING_CMD" );
		     break;

		  case HopDroid.SERVER_ABORT_CMD:
		     Log.d( HOPDROID, "serverCmd cmd=SERVER_ABORT_CMD" );
		     if( state != HOPDROID_STATE_REBOOT ) {
			throw new IOException( "force hopdroid abort" );
		     }
		     break;

		  default:
		     throw new IOException( "hopdroid: protocol error: illegal command " + cmd );
	       }
	    } finally {
	       try {
		  if( sock != null ) {
		     sock.close();
		  }
	       } catch( Throwable t ) {
		  Log.e( HOPDROID, "Cannot close cmd client: " + t );
		  t.printStackTrace();
	       }
	    }
	 }
      } catch( IOException e ) {
	 Log.e( HOPDROID, "cmdserv error: " + e + " state=" + state );
	 e.printStackTrace();

	 // kill everything on error
	 kill();
      }

      Log.d( HOPDROID, "thread ended." );
   }

   // onConnect
   public void onConnect() {
      // notify that HopDroid is ready
      // service.handler has been initialized before onConnect got called
      service.handler.sendEmptyMessage( HopLauncher.MSG_HOPDROID_CONNECT );

      synchronized( this ) {
	 if( delayed_start_notification ) {
	    service.handler.sendEmptyMessage( HopLauncher.MSG_HOPDROID_START );
	 }
      }
      
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
	 
   // kill
   public synchronized void kill() {
      if( state != HOPDROID_STATE_END ) {
	 state = HOPDROID_STATE_END;
	 
	 Log.i( HOPDROID, "kill..." );

	 killServers();
	 killPlugins();
	 
	 if( service != null && service.handler != null ) {
	    service.handler.sendEmptyMessage( HopLauncher.MSG_HOPDROID_ENDED );
	 }
      }
   }

   // reboot
   public void reboot() {
      Log.i( HOPDROID, "reboot..." );

      state = HOPDROID_STATE_REBOOT;

      synchronized( this ) {
	 if( pluginclient != null ) {
	    pluginserv.reset = true;
	    try {
	       pluginclient.close();
	    } catch( Throwable e ) {
	    }
	 }
	 if( eventclient != null ) {
	    eventserv.reset = true;
	    pushClientCloseEvent();
	    try {
	       eventclient.close();
	    } catch( Throwable e ) {
	    }
	 }
      }
   }
   
   // execCmd
   private static boolean execCmd( LocalSocketAddress addr, byte cmd )
      throws Exception {
      LocalSocket ls = new LocalSocket();
      
      Log.d( HOPDROID, "execCmd addr=" + addr.getName() + " cmd=" + cmd );
      
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
   private static void serverStop( LocalSocketAddress addr, boolean notify ) {
      try {
         execCmd( addr, SERVER_STOP_CMD );
      } catch( Exception e ) {
	 if( notify ) {
	    Log.d( HOPDROID, "serverStop failed: " + e );
	    e.printStackTrace();
	 }
      }
   }

   // serverPing
   private static boolean serverPing( LocalSocketAddress addr ) {
      try {
	 Log.d( HOPDROID, "serverPing..." );
	 return execCmd( addr, SERVER_PING_CMD );
      } catch( Exception e ) {
	 Log.e( HOPDROID, "serverPing error: " + e );
	 e.printStackTrace();
         return false;
      }
   }

   // isBackground()
   protected static boolean isBackground() {
      LocalSocketAddress addr =
         new LocalSocketAddress( "hopdroid-cmd:" + Hop.port );

      return serverPing( addr );
   }

   // handle a session with one client connected to the HopDroid server
   private void serverPlugin() throws Exception {
      pluginclient = pluginserv.accept();
      final InputStream ip = pluginclient.getInputStream();
      final OutputStream op = pluginclient.getOutputStream();

      Log.i( HOPDROID, "serverPlugin (" + HopConfig.APP + ") connected sock=" + pluginclient );

      try {
	 while( true ) {
	    final int version = ip.read();

	    if( version != protocol ) {
	       if( version != -1 ) {
		  Log.e( HOPDROID, "serverPlugin protocol error: incompatible version " +
			 version );
	       } else {
		  Log.e( HOPDROID, "serverPlugin, connection reset by peer" );
	       }
	       return;
	    }

	    final int id = read_int32( ip );

	    try {
	       HopPlugin p = (HopPlugin)plugins.get( id );

	       Log.d( HOPDROID, "executing plugin " + p.name + "...(activity="
		      + (activity == null ? "null" : activity.toString() )
		      + ")" );
	       p.server( ip, op );

	       final int m = ip.read();
		  
	       if( m != 127 ) {
		  Log.e( HOPDROID, "protocol error: illegal plugin \"" 
			 + p.name + "\" mark: " + m );
	       }
	       op.flush();
	    } catch( ArrayIndexOutOfBoundsException a ) {
	       Log.e( HOPDROID, "plugin not found: " + id );
	       // we got an eof, escape from here
	       return;
	    }
	 }
      } catch( Throwable e ) {
	 Log.e( HOPDROID, "serverPlugin error (catched)  " + e );
	 e.printStackTrace();
	 
	 synchronized( this ) {
	    if( pluginserv != null && !pluginserv.isClosed() ) {
	       try {
		  pluginclient.close();
		  pluginclient = null;
	       } catch( Throwable ne ) {
		  Log.e( HOPDROID, "error while closing plugin client: " + e );
		  e.printStackTrace();
	       }
	    }

	    if( pluginserv != null && pluginserv.reset ) {
	       pluginserv.reset = false;
	       serverPlugin();
	    }
	 }
      }
   }
	    
   // serverEvent
   private void serverEvent() throws Exception {
      eventclient = eventserv.accept();
      final InputStream ip = eventclient.getInputStream();

      Log.i( HOPDROID, "serverEvent (" + HopConfig.APP + ") connected eventserv=" +
	     eventserv + " client=" + eventclient );

      // initialize the eventable
      synchronized( eventtable ) {
	 eventtable.put( "phone", new Integer( 1 ) );
      }
      
      try {
	 while( true ) {
	    final int version = ip.read();
	 
	    if( version != protocol ) {
	       if( version != -1 ) {
		  Log.e( HOPDROID, "protocol error: incompatible version " +
			 version );
	       }
	       return;
	    }
	    
	    String event = read_string( ip );
	    int a = ip.read();

	    synchronized( eventtable ) {
	       Integer i = (Integer)eventtable.get( event );
	    
	       Log.i( HOPDROID, (a == 1 ? "register" : "unregister") +
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
		     eventtable.put( event, new Integer( ni ) );
		  }
	       }
	    }
	       
	    int m = ip.read();
      
	    if( m != 127 ) {
	       Log.e( HOPDROID, "protocol error: illegal ping mark " + m );
	       return;
	    }
	 }
      } catch( Throwable e ) {
	 Log.e( HOPDROID, "serverEvent error " + e );
	 e.printStackTrace();
	 
	 synchronized( this ) {
	    if( eventserv != null && !eventserv.isClosed() ) {
	       pushClientCloseEvent();
	       eventclient.close();
	       eventclient = null;
	    }
	 }

	 if( eventserv != null && eventserv.reset ) {
	    eventserv.reset = false;
	    serverEvent();
	 }
      }
   }

   // kill plugin
   private synchronized void killPlugins() {
      int s = plugins.size();

      Log.d( HOPDROID, "killPlugins..." );

      // plugin 0 is null
      for( int i = 1; i < s; i++ ) {
	 HopPlugin p = (HopPlugin)plugins.get( i );
	 if( p != null ) {
	    p.kill();
	 }
      }

      plugins = null;
   }

   // pushClientClose
   private void pushClientCloseEvent() {
      pushEvent( "phone", "client-stop" );
   }
      
   // safeclose
   private void safeClose( HopLocalServerSocket serv, LocalSocket client ) {
      // Android is buggous, closing a server that has not accepted 
      // any connection yet, raises no exceptions and then does not
      // unblock the pending accept call. Our function safeclose establishes
      // a fake connection and closes it at once to work around this
      // Android error.
      Log.d( HOPDROID, "safeClose" );
      try {
	 if( client == null ) {
	    try {
	       LocalSocket ls = new LocalSocket();

	       ls.connect( serv.getLocalSocketAddress() );
	       ls.close();
	    } catch( Throwable e ) {
	       Log.d( HOPDROID, "safeClose error, cannot connect, serv="
		      + serv + " exc=" + e );
	    }
	 } else {
	    client.close();
	 }
      } catch( Throwable e ) {
	 Log.e( HOPDROID, "cannot close " + e );
	 e.printStackTrace();
      }
      if( serv != null ) serv.close();
   }

   // abortCmdServer
   private synchronized void abortCmdServer( String reason ) {
      Log.d( HOPDROID, "abortCmdServer " + reason );
      
      if( cmdserv != null ) {
	 synchronized( cmdserv ) {
	    if( !cmdserv.isClosed() ) {
	       try {
		  execCmd( cmdserv.getLocalSocketAddress(), SERVER_ABORT_CMD );
	       } catch( Exception e ) {
		  Log.e( HOPDROID, "serverThrow failed: " + e );
		  e.printStackTrace();
	       }
	    }
	 }
      }
   }

   // killServers
   private synchronized void killServers() {
      Log.d( HOPDROID, "killServers..." );
      
      // event server
      if( eventserv != null ) {
	 if( eventclient != null ) {
	    pushClientCloseEvent();
	 }
	 safeClose( eventserv, eventclient );
	 eventclient = null;
	 eventserv = null;
      }

      // plugin server
      if( pluginserv != null ) {
	 safeClose( pluginserv, pluginclient );
	 pluginclient = null;
	 pluginserv = null;
      }

      // cmd server
      if( cmdserv != null ) {
	 safeClose( cmdserv, null );
	 cmdserv = null;
      }
      
      Log.d( HOPDROID, "killServers...all servers stopped" );
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
	       Log.e( HOPDROID, "pushEvent error: " + e );
	       e.printStackTrace();
	    }
	 } else {
	    Log.i( HOPDROID, "pushEvent event=[" + event + "] value=["
		   + value + "] i=" + i );
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
   boolean reset = false;
   
   HopLocalServerSocket( String name ) throws IOException {
      super( name );
      Log.d( "HopLocalServerSocket", "creating server name=" + name + " this=" + this );
   }

   public synchronized void close() {
      try {
	 if( !closed ) {
	    
	    super.close();
	    
	    closed = true;
	    Log.d( "HopDroid:HopLocalServerSocket", "server "
		   + this.getLocalSocketAddress().getName() + " ("
		   + this + ") closed" );
	 }
      } catch( Throwable e ) {
	 Log.e( "HopDroid:HopLocalServerSocket", "cannot close server "
		+ this.getLocalSocketAddress().getName() + " ("
		+ this + "): " + e );
	 e.printStackTrace();
      }
   }

   synchronized boolean isClosed() {
      return closed;
   }
}
