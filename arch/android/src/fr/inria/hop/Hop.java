/*=====================================================================*/
/*    .../project/hop/2.2.x/arch/android/src/fr/inria/hop/Hop.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Fri Oct  1 09:08:17 2010                          */
/*    Last change :  Sat Oct 23 07:42:38 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
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
   final static String ROOT = "/data/data/fr.inria.hop";
   final static File HOME = new File( Environment.getExternalStorageDirectory(), "home" );
   final static String HOP = ROOT + "/bin/hop";
   final static String HOPARGS = "-v2 --no-color";
   final static String APKPATH = "/data/app/fr.inria.hop.apk";
   final static String SHELL = "/system/bin/sh";
   final static int HOP_RESTART = 5;

   // instance variables
   Activity activity;
   File home;
   String root;
   String apk;
   String msg;
   String port = "8080";
   FileDescriptor HopFd;
   Handler handler;
   ArrayBlockingQueue<String> queue;
   final int[] currentpid = new int[ 1 ];

   // constructor
   public Hop( Activity a, ArrayBlockingQueue<String>q, Handler h ) {
      super();
      
      activity = a;
      home = HOME;
      root = ROOT;
      apk = APKPATH;
      queue = q;
      handler = h;
   }

   // is hop already configured
   public boolean configured() {
      return HOME.exists();
   }

   // run hop
   public void run() {
      final int[] pid = new int[ 1 ];
      String sh = SHELL;
      String cmd = "export HOME=" + HOME.getAbsolutePath() +
	 "; exec " + HOP + " " + HOPARGS + " -p " + port;

      Log.i( "Hop", "executing [" + sh + " -c " + cmd );
      HopFd = HopExec.createSubprocess( sh, "-c", cmd, null, null, null, pid );

      synchronized( currentpid ) {
	 Log.i( "Hop", "new hop process start pid=" + pid[ 0 ] );
	 currentpid[ 0 ] = pid[ 0 ];
      }

      // background threads
      Thread watcher = new Thread( new Runnable() {
	    public void run() {
	       int result = HopExec.waitFor( pid[ 0 ] );
	       Log.i( "Hop", "process exited (pid="
		      + pid[ 0 ] + ") with result=" + result );
	       if( result == HOP_RESTART ) {
		  Log.i( "Hop", "restarting hop..." );
		  rerun();
	       } else {
		  boolean tosend = false;
		  
		  synchronized( currentpid ) {
		     if( currentpid[ 0 ] == pid[ 0 ] ) {
			tosend = true;
		     }
		  }
		  if( tosend ) {
		     handler.sendEmptyMessage( HopLauncher.MSG_PROC_END );
		  }
	       };
	    }
	 } );

      Thread logger = new Thread( new Runnable() {
	    FileInputStream fin = new FileInputStream( HopFd );

	    public void run() {
	       byte[] buffer = new byte[ 80 ];
	       int l;

	       try {
		  for( l = fin.read( buffer ); l > 0; l = fin.read( buffer ) ) {
		     String s = new String( buffer, 0, l );
		     // Log.v( "Hop", s );
		     queue.put( s );
		     handler.sendEmptyMessage( HopLauncher.MSG_OUTPUT_AVAILABLE );
		  }
	       } catch( Exception e ) {
		  boolean tosend = false;
		  
		  Log.e( "Hop", "process exception (pid=" + pid[ 0 ]
			 + " currentpid=" + currentpid[ 0 ] 
			 + ") exception=" +  e.getClass().getName() );
		  synchronized( currentpid ) {
		     if( currentpid[ 0 ] == pid[ 0 ] ) {
			tosend = true;
		     }
		  }

		  if( tosend ) {
		     handler.sendMessage( android.os.Message.obtain( handler, HopLauncher.MSG_RUN_FAIL, e ) );
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
      Log.v( "Hop", "killing..." );
      synchronized( currentpid ) {
	 if( currentpid[ 0 ] != 0 ) {
	    Log.i( "Hop", "kill (pid=" + currentpid[ 0 ] + ")" );
	    android.os.Process.killProcess( currentpid[ 0 ] );
	    currentpid[ 0 ] = 0;
	 }
      }
      Log.v( "Hop", "killed." );
   }
}
   
