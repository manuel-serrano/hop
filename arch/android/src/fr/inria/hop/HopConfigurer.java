/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopConfigurer.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct  8 15:35:26 2010                          */
/*    Last change :  Mon Mar 25 11:36:46 2013 (serrano)                */
/*    Copyright   :  2010-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Configuring Hop                                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.Activity;
import android.util.Log;
import android.app.ProgressDialog;
import android.content.DialogInterface;
import android.os.*;

import java.util.*;
import java.util.zip.*;
import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopConfigurer extends Thread {
   // global constants
  final static String CHMOD = "/system/bin/chmod 777"; 

   // instance variables
   Handler handler;
   String wizard_url;
   
   // constructor
   public HopConfigurer( Handler d, String u ) {
      super();

      Log.v( "HopConfigurer", "wizard_url=" + u );
      handler = d;
      wizard_url = u;
   }

   // is hop already configured
   public static boolean configured( File home ) {
      File path = new File( home, ".config/hop/wizard.hop" );
      Log.v( "HopConfigurer", "checking file \"" + path + "\"..." +
	     (path.exists() ? "exists" : "missing") );
      return path.exists();
   }

   public void run() {
      try {
	 Log.v( "HopConfigurer", "opening connrection \"" + wizard_url + "\"..." );
	 HttpURLConnection conn = (HttpURLConnection)new URL( wizard_url ).openConnection();

	 // Wait for Hop to be up...
	 while( true ) {
	    try {
	       conn.connect();
	       Thread.sleep( 1000 );
	       break;
	    } catch( IOException e ) {
	       Log.e( "HopConfigurer", "connection failed (" + e + ") retrying..." );
	       Thread.sleep( 5000 );
	       conn = (HttpURLConnection)new URL( wizard_url ).openConnection();
	    }
	 }
	 
	 // notify that we can start a web browser
	 handler.sendEmptyMessage( HopLauncher.MSG_RUN_WIZARD );
      } catch( Exception e ) {
	 String msg = e.getMessage();
	 if( msg == null ) msg = e.getClass().getName();
	 
	 Log.e( "HopInstaller", msg );
	 handler.sendMessage( android.os.Message.obtain( handler, HopLauncher.MSG_CONFIGURE_FAIL, e ) );
      }
   }
}

