/*=====================================================================*/
/*    .../project/hop/2.4.x/arch/android/src/fr/inria/hop/Hop.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Fri Oct  1 09:08:17 2010                          */
/*    Last change :  Fri Nov  9 09:42:54 2012 (serrano)                */
/*    Copyright   :  2010-12 Manuel Serrano                            */
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
   final static File HOME = new File( Environment.getExternalStorageDirectory(), "home" );
   final static String HOP = "/bin/hop";
   final static String HOPARGS = "-v --no-color";
   final static String SHELL = "/system/bin/sh";
   final static int HOP_RESTART = 5;

   // global variables
   static String root = "/data/data/fr.inria.hop";
   static String debug = "";
   static boolean zeroconf = false;
   static boolean webdav = false;

   static String port = "8080";

   // instance variables
   private boolean killed = false;
   public boolean inkill = false;
   String msg;
   FileDescriptor HopFd;
   Handler handler;
   ArrayBlockingQueue<String> queue;
   final int[] currentpid = new int[ 1 ];
   boolean log = false;
   String extra = "";

   // constructor
   public Hop( ArrayBlockingQueue<String>q, Handler h ) {
      super();

      queue = q;
      handler = h;
   }

   // is hop already configured
   public boolean configured() {
      return HOME.exists();
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

      String cmd = "export HOME=" + HOME.getAbsolutePath() +
	 "; exec " + root + HOP + " " + HOPARGS
	 + " -p " + port
	 + " " + debug
	 + (zeroconf ? " -z" : "")
	 + (webdav ? " -d" : "")
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
	       Log.i( "Hop", "process exited (pid="
		      + pid[ 0 ] + ") with result=" + result
		      + " (/HOP_RESTART=" + HOP_RESTART + ")" );

	       if( !killed ) {
		  if( result == HOP_RESTART ) {
		     if( handler != null ) {
			handler.sendEmptyMessage( HopLauncher.MSG_START_HOP_SERVICE );
		     }

		  } else {
		     boolean tosend = false;
		  
		     synchronized( currentpid ) {
			if( currentpid[ 0 ] == pid[ 0 ] ) {
			   tosend = true;
			}
		     }
		     if( tosend && handler != null ) {
			handler.sendEmptyMessage( HopLauncher.MSG_HOP_ENDED );
		     }
		  };
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
		     if( handler != null ) {
			String s = new String( buffer, 0, l );
			if( HopLauncher.hop_log ) Log.v( "HopConsole", s );
			queue.put( s );
			handler.sendEmptyMessage( HopLauncher.MSG_HOP_OUTPUT_AVAILABLE );
		     }
		  }
	       } catch( Throwable e ) {
		  if( !killed && !inkill ) {
		     boolean tosend = false;
		  
		     Log.e( "Hop", "process exception (pid=" + pid[ 0 ]
			    + " currentpid=" + currentpid[ 0 ] 
			    + ") exception=" +  e.getClass().getName(), e );
		     synchronized( currentpid ) {
			if( currentpid[ 0 ] == pid[ 0 ] ) {
			   tosend = true;
			}
		     }

		     if( tosend && handler != null ) {
			handler.sendMessage( android.os.Message.obtain( handler, HopLauncher.MSG_HOP_FAILED, e ) );
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
      inkill = true;
      
      if( !killed ) {
	 killed = true;
	 
	 synchronized( currentpid ) {
	    if( currentpid[ 0 ] != 0 ) {
	       Log.i( "Hop", ">>> kill hop (pid=" + currentpid[ 0 ] + ")" );
	    
	       android.os.Process.killProcess( currentpid[ 0 ] );
	       currentpid[ 0 ] = 0;
	    
	       Log.i( "Hop", "<<< kill hop" );
	    }
	 }
      }
   }

   // isRunning()
   public boolean isRunning() {
      Log.e( "Hop", "isRunning killed=" + killed + " pid=" + currentpid[ 0 ] );
      return !killed && currentpid[ 0 ] != 0;
   }
   
   // isBackground()
   public static boolean isBackground() {
      return HopService.isBackground();
   }
}
   
