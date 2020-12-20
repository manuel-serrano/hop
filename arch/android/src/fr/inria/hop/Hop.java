/*=====================================================================*/
/*    .../project/hop/hop/arch/android/src/fr/inria/hop/Hop.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Fri Oct  1 09:08:17 2010                          */
/*    Last change :  Sat Dec 19 17:13:52 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Android manager for Hop                                          */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.os.*;
import android.util.Log;
import android.content.*;
import android.widget.TextView;
import android.content.res.*;
import android.content.Context;
import android.preference.*;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.concurrent.ArrayBlockingQueue;
import java.lang.String;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class Hop extends Thread {
   // global constants
   private static File _HOME = null;
   final static String HOP = "/bin/hop";
   final static String HOPARGS = "--no-color";
   final static String SHELL = "/system/bin/sh";
   final static int HOP_RESTART = 5;

   // global variables
   static String root = HopConfig.ROOT;
   static String debug = HopConfig.DEBUG;
   static String maxthreads = HopConfig.MAXTHREADS;
   static String url = HopConfig.APP;
   static boolean zeroconf = true;
   static boolean webdav = false;
   static boolean jobs = false;

   // see setHopActivityParams
   static String port;
   static String rcdir;
   static String args;

   // instance variables
   private boolean killed = false;
   public boolean inkill = false;
   String msg;
   FileDescriptor HopFd;
   final int[] currentpid = new int[ 1 ];
   boolean log = false;
   Thread logger = null;
   Thread watcher = null;
   
   HopService service;
   
   // constructor
   public Hop( HopService s ) {
      super();

      service = s;
      currentpid[ 0 ] = 0;
   }

   // prefs fetch compatible with the hop encoding (using S: as prefix)
   static String getPrefString( SharedPreferences sp, String key, String def ) {
      String tmp = sp.getString( key, "" );

      return (tmp.length() <= 2) ? def : tmp.substring( 2 );
   }

   // setHopActivityParams
   static void setHopActivityParams( Activity activity ) {
      Resources res = activity.getResources();
      SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences( activity );
      port = getPrefString( sp, HopConfig.APP + "-port", HopConfig.PORT );
      rcdir = HOME().getAbsolutePath() + "/"
	 + getPrefString( sp, HopConfig.APP + "-rcdir", ".config/" + HopConfig.APP );
      args = getPrefString( sp, HopConfig.ARGS, "" );
      rcdir = activity.getApplicationInfo().dataDir + "/assets/rcdir";
      rcdir = HOME().getAbsolutePath() + "/.config/" + HopConfig.APP;
   }
   
   // HOME
   public static File HOME() {
      if( _HOME == null ) {
	 // try to find an actual directory
	 File sdcard = new File( "/mnt/sdcard" );
	 if( sdcard.exists() ) {
	    Log.d( "Hop", "HOME, /mnt/sdcard exists..." );
	    _HOME = new File( sdcard, "home" );

	    if( !_HOME.canWrite() ) {
	       _HOME = null;
	    }
		   
	 }

	 if( _HOME == null ) {
	    // fallback
	    _HOME = new File( Environment.getExternalStorageDirectory(), "home" );
	 }
      }
      
      return _HOME;
   }
      
   // is hop already configured
   public boolean configured() {
      return HOME().exists();
   }

   // run hop
   public void run() {
      final int[] pid = new int[ 1 ];
      String sh = SHELL;
      final String[] ahost = new String[ 1 ];
      final Boolean[] ready = new Boolean[ 1 ];

      Log.d( "Hop", "=================================================" );
      Log.d( "Hop", "run..." );

      // acknowledge server
      Thread th = new Thread( new Runnable() {
	    public void run() {
	       ServerSocket asrv;
	       try {
		  synchronized( ahost ) {
		     asrv = new ServerSocket( 0 );
		     ahost[ 0 ] = "127.0.0.1" + ":" + asrv.getLocalPort();
		     // notify the acknowledge host server address and port
		     ahost.notify();
		  }
		  
		  Socket sock = asrv.accept();
		     
		  try {
		     final InputStream ip = sock.getInputStream();
		     Log.d( "Hop", "Acknowledge server connected" );
			
		     ready[ 0 ] = new Boolean(
			ip.read() == 0x68
			&& ip.read() == 0x6f
			&& ip.read() == 0x70 );
			
		     // notify the acknowledge
		     synchronized( ready ) {
			ready.notify();
		     }
		  } catch( IOException exc ) {
		     Log.e( "Hop", "Acknowledge server error!" + exc );
		     ready[ 0 ] = new Boolean( false );
		     ready.notify();
		  } finally {
		     asrv.close();
		  }
	       } catch( IOException exc ) {
		  Log.e( "Hop", "Cannot spawn client acknowledge server! " + exc );
		  ahost[ 0 ] = null;
		  ahost.notify();
		  return;
	       }
	    }
	 } );

      // 1. the acknowledge server is started, it will be used only
      //    to wait for the Hop acknowledge.
      // 2. the Hop server is started with the --acknowledge option
      //    and the port number of the acknowledge server.
      // 3. the acknowledge server waits for the acknowledge
      try {
	 ready[ 0 ] = null;
	 
	 synchronized( ahost ) {
	    th.start();

	    // wait for the acknowledge port number
	    ahost.wait();
	 }

	 String cmd = "export HOME=" + HOME().getAbsolutePath() + "; "
	    + "export LD_LIBRARY_PATH="
	    + root + "/lib/bigloo/" + HopConfig.BIGLOORELEASE + ":"
	    + root + "/lib/hop/" + HopConfig.HOPRELEASE + ":$LD_LIBRARY_PATH;"
	    + "exec " + root + HOP + " " + HOPARGS
	    + " -p " + port
	    + " " + debug
	    + " --max-threads " + maxthreads
	    + (zeroconf ? " -z" : " --no-zeroconf")
	    + (webdav ? " -d" : "")
	    + (jobs ? " --jobs" : " --no-jobs")
	    + " --rc-dir " + rcdir
	    + " --acknowledge " + ahost[ 0 ]
	    + " --so-policy none"
	    + " -v2 " + args;

	 Log.i( "Hop", HopConfig.APP + " exec [" + sh + " -c \"" + cmd + "\"]");
	 HopFd = HopExec.createSubprocess( sh, "-c", cmd, null, null, null, pid );
	 Log.v( "Hop", "Hop process started, pid=" + pid[ 0 ] + ", HopFd=" +  HopFd );
      } catch( Throwable exc ) {
	 Log.e( "Hop", "Error while waiting for Hop server acknowledge " + exc );
	 exc.printStackTrace();
      }

      synchronized( currentpid ) {
	 currentpid[ 0 ] = pid[ 0 ];
	 currentpid.notifyAll();
      }

      // background threads
      watcher = new Thread( new Runnable() {
	    public void run() {
	       // wait for the termination of the Hop process
	       int result = HopExec.waitFor( pid[ 0 ] );
	       Log.i( "Hop", "process exit (pid="
		      + pid[ 0 ] + ") with result=" + result
		      + " (HOP_RESTART=" + HOP_RESTART + ")" );

	       synchronized( currentpid ) {
		  currentpid[ 0 ] = -1;

		  if( result == HOP_RESTART ) {
		     HopLauncher.hop_resuscitate = true;
		  } else {
		     // the process has stopped unexpectidly
		     if( result == 6 ) {
			Log.e( "Hop", "hop suicide" );
			service.handler.sendMessage(
			   android.os.Message.obtain(
			      service.handler, HopLauncher.MSG_KILL_HOP_SERVICE, result ) );
		     } else {
			if( !inkill && service.handler != null ) {
			   Log.e( "Hop", "hop stopped unexpectidly" );
			   service.handler.sendMessage(
			      android.os.Message.obtain(
				 service.handler, HopLauncher.MSG_HOP_FAILED, result ) );
			}
		     }
		  }
	       }
	       
	    }
	 } );

      logger = new Thread( new Runnable() {
	    FileInputStream fin = new FileInputStream( HopFd );
	    
	    public void run() {
	       byte[] buffer = new byte[ 8192 ];
	       int l;

	       try {
		  for( l = fin.read( buffer ); l > 0; l = fin.read( buffer ) ) {
		     if( service.handler != null ) {
			String s = new String( buffer, 0, l );
			service.queue.put( s );
			service.handler.sendEmptyMessage( HopLauncher.MSG_HOP_OUTPUT_AVAILABLE );
		     }
		  }
	       } catch( Throwable e ) {
		  if( !inkill ) {
		     Log.e( "Hop", "Error in the thread logger: " + e );
		     e.printStackTrace();
		     synchronized( currentpid ) {
			if( currentpid[ 0 ] > 0 ) {
			   Log.e( "Hop", "process exception (pid=" + pid[ 0 ]
				  + ") exception="
				  +  e.getClass().getName(), e );
			}
		     }
		  }
	       }
	    }
	 } );

      // MS WARNING: this should be improved as there is a potential deadlock on
      // boot. If the logger queue is fulled, the main thread might be blocked
      // waiting Hop to start, which could never be completed because of the logger
      // thread being stuck.
      Log.d( "Hop", "watcher started..." );
      watcher.start();
      Log.d( "Hop", "logger started..." );
      logger.start();

      // wait for Hop to acknowledge
      try {
	 synchronized( ready ) {
	    if( ready[ 0 ] == null ) {
	       ready.wait();
	    }
	 }
      } catch( Exception e ) {
	 Log.e( "Hop", "Acknowledge error " + e );
	 e.printStackTrace();
	 ;
      }
      
      Log.d( "Hop", "acknowledge received... server is ready" );
      // spawn the initial Hop service
      service.handler.sendEmptyMessage( HopLauncher.MSG_HOP_START );
   }

   // reboot
   public void reboot() {
      Log.i( "Hop", "reboot..." );
      kill();
      // wait the socket server to be cleanup by the system
      try {
	 Thread.sleep( 2000 );
      } catch( Exception e ) {
      }
      run();
   }
   
   // kill (kill the running Hop process)
   public void kill() {
      Log.i( "Hop", "kill..." + ((currentpid != null) ? currentpid[ 0 ] : "null") );

      synchronized( currentpid ) {
	 
	 if( logger != null ) {
	    logger.interrupt();
	    logger = null;
	 }
	 if( watcher != null ) {
	    watcher.interrupt();
	    watcher = null;
	 }
	 
	 if( currentpid[ 0 ] != 0 ) {
	    inkill = true;
	    android.os.Process.killProcess( currentpid[ 0 ] );
	 }
      }
   }

   // ping
   public static boolean ping( String port, int errcnt, String svc ) {
      Log.d( "Hop", "ping ping port=" + port + " svc=" + svc );
      while( true ) {
	 try {
	    URL pingURL = new URL( "http://localhost:" + port + svc );
	    HttpURLConnection conn = (HttpURLConnection)pingURL.openConnection();

	    conn.setRequestMethod( "HEAD" );

	    Log.d( "Hop", "  >>> ping, HEAD " + pingURL.toString() );

	    conn.setConnectTimeout( 500 );
	 
	    int status = conn.getResponseCode();
	    conn.disconnect();

	    Log.d( "Hop", "  <<< ping, status=" + status );
	    return (status == HttpURLConnection.HTTP_OK)
	       || (status == HttpURLConnection.HTTP_NOT_FOUND)
	       || (status == HttpURLConnection.HTTP_UNAUTHORIZED);
	 } catch( Exception e ) {
	    Log.d( "Hop", "  !!! ping, exn=" + e.toString() );
	    if( errcnt-- > 0 ) {
	       try {
		  Thread.sleep( 500 );
	       } catch( Exception ee ) {
		  return false;
	       }
	    } else {
	       return false;
	    }
	 }
      }
   }

   public static boolean ping( String port, int errcnt ) {
      return ping( port, errcnt, "/hop" );
   }

   // isRunning()
   public boolean isRunning( int timeout ) {
      int errcount = 10;
      
      while( true ) {
	 synchronized( currentpid ) {
	    Log.d( "Hop", "isRunning currentpid=" + currentpid[ 0 ] );
	    if( currentpid[ 0 ] > 0 ) {
	       return ping( port, 10 );
	    } else if( currentpid[ 0 ] == -1 ) {
	       return false;
	    } else {
	       try {
		  currentpid.wait( timeout );

		  if( currentpid[ 0 ] <= 0 ) {
		     return false;
		  }
	       } catch( Exception e ) {
		  return false;
	       }
	    }
	 }
      }
   }
}
   
