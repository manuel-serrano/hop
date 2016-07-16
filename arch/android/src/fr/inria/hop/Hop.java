// -*- java -*-
/*=====================================================================*/
/*    .../hop/3.1.x/arch/android/src/fr/inria/hop/Hop.java.in          */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Fri Oct  1 09:08:17 2010                          */
/*    Last change :  Sat Jul  2 10:15:23 2016 (serrano)                */
/*    Copyright   :  2010-16 Manuel Serrano                            */
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
   static String root = "/data/data/fr.inria.hop/assets";
   static String verbose = "";
   static String debug = "";
   static boolean zeroconf = false;
   static boolean webdav = false;
   static boolean jobs = false;

   static String port = "@HOPPORT@";
   static String maxthreads = "@HOPTHREADS@";

   // instance variables
   private boolean killed = false;
   public boolean inkill = false;
   String msg;
   FileDescriptor HopFd;
   final int[] currentpid = new int[ 1 ];
   boolean log = false;
   String extra = "";

   HopService service;
   
   // constructor
   public Hop( HopService s, String args ) {
      super();

      service = s;
      extra = args;
   }

   // HOME
   public static File HOME() {
      if( _HOME == null ) {
	 // try to find an actual directory
	 File sdcard = new File( "/mnt/sdcard" );
	 if( sdcard.exists() ) {
	    Log.d( "Hop", "HOME, /mnt/sdcard exists..." );
	    _HOME = new File( sdcard, "home" );
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

   // startWithArg
   public void startWithArg( String arg ) {
      extra = arg;
      start();
   }

   // run hop
   public void run() {
      final int[] pid = new int[ 1 ];
      String sh = SHELL;

      String cmd = "export HOME=" + HOME().getAbsolutePath() + "; "
	 + "export LD_LIBRARY_PATH="
	 + root + "/lib/bigloo/@BIGLOOVERSION@:"
	 + root + "/lib/hop/@HOPVERSION@:$LD_LIBRARY_PATH;"
	 + "exec " + root + HOP + " " + HOPARGS
	 + " -p " + port
	 + " " + verbose
	 + " " + debug
	 + " --max-threads " + maxthreads
	 + (zeroconf ? " -z" : " --no-zeroconf")
	 + (webdav ? " -d" : "")
	 + (jobs ? " --jobs" : " --no-jobs")
	 + " " + extra;

      Log.i( "Hop", "executing [" + sh + " -c " + cmd + "]");
      HopFd = HopExec.createSubprocess( sh, "-c", cmd, null, null, null, pid );
      Log.i( "Hop", "Hop process started, pid=" + pid[ 0 ] + ", HopFd=" +  HopFd );

      extra = "";
      
      synchronized( currentpid ) {
	 currentpid[ 0 ] = pid[ 0 ];
      }

      // background threads
      Thread watcher = new Thread( new Runnable() {
	    public void run() {
	       // wait for the termination of the Hop process
	       int result = HopExec.waitFor( pid[ 0 ] );
	       Log.i( "Hop", "process exit (pid="
		      + pid[ 0 ] + ") with result=" + result
		      + " (HOP_RESTART=" + HOP_RESTART + ")" );

	       synchronized( currentpid ) {
		  currentpid[ 0 ] = 0;

		  if( result == HOP_RESTART ) {
		     HopLauncher.hop_resuscitate = true;
		  } else {
		     // the process has stopped unexpectidly
		     if( !inkill && service.handler != null ) {
			Log.d( "Hop", "hop stopped unexpectidly" );
			service.handler.sendMessage(
			   android.os.Message.obtain(
			      service.handler, HopLauncher.MSG_HOP_FAILED, result ) );
		     }
		  }
	       }
	       
	    }
	 } );

      Thread logger = new Thread( new Runnable() {
	    FileInputStream fin = new FileInputStream( HopFd );
	    
	    public void run() {
	       byte[] buffer = new byte[ 255 ];
	       int l;

	       try {
		  for( l = fin.read( buffer ); l > 0; l = fin.read( buffer ) ) {
		     if( service.handler != null ) {
			String s = new String( buffer, 0, l );
			if( HopLauncher.hop_log ) Log.v( "HopConsole", s );
			service.queue.put( s );
			service.handler.sendEmptyMessage( HopLauncher.MSG_HOP_OUTPUT_AVAILABLE );
		     }
		  }
	       } catch( Throwable e ) {
		  Log.e( "Hop", "Error in the thread logger: " + e );
		  if( !inkill ) {
		     synchronized( currentpid ) {
			if( currentpid[ 0 ] != 0 ) {
			   Log.e( "Hop", "process exception (pid=" + pid[ 0 ]
				  + ") exception="
				  +  e.getClass().getName(), e );
			}
		     }
		  }
	       }
	    }
	 } );
   
      watcher.start();
      logger.start();
   }

   // rerun
   private void rerun() {
      synchronized( currentpid ) {
	 currentpid[ 0 ] = 0;
      }
      run();
   }
   
   // restart
   public void restart() {
      kill();
      run();
   }
   
   // kill
   public void kill() {
      Log.d( "Hop", ">>> kill..." );
      
      synchronized( currentpid ) {
	 if( currentpid[ 0 ] != 0 ) {
	    Log.i( "Hop", ">>> kill hop (pid=" + currentpid[ 0 ] + ")..." );

	    inkill = true;
	    android.os.Process.killProcess( currentpid[ 0 ] );
	    
	    Log.i( "Hop", "<<< kill hop" );
	 }
      }
      
      Log.d( "Hop", "<<< kill" );
   }

   // isRunning()
   public boolean isRunning() {
      Log.e( "Hop", "isRunning killed=" + killed + " pid=" + currentpid[ 0 ] );
      return !killed && currentpid[ 0 ] != 0;
   }

   // emergencyExit
   protected static void emergencyExit() {
      // Try to kill a running background Hop process. This function is called
      // after the HopDroid interface has been shutdown to all local sockets
      // are already closed. We then, just emit a request to the server
      // that will make it fails... and exit.
      Log.i( "Hop", ">>> emergencyExit..." );
      try {
	 Socket sock = new Socket( "localhost", Integer.parseInt( Hop.port ) );
	 OutputStream op = sock.getOutputStream();

	 op.write( "GET /hop/androidemo HTTP/1.1\r\n".getBytes() );
	 op.write( "Host: localhost\r\n".getBytes() );
	 op.write( "\r\n\r\n".getBytes() );

	 sock.close();
      } catch( Throwable e ) {
	 Log.e( "Hop", "emergencyExit error=" + e );
      }
      Log.i( "Hop", "<<< emergencyExit" );
   }
}
   
