/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopConfigurer.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct  8 15:35:26 2010                          */
/*    Last change :  Thu Oct 14 15:49:28 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
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
   Hop hop;
   Handler handler;
   String url;
   
   // constructor
   public HopConfigurer( Hop h, Handler d, String u ) {
      super();
      
      hop = h;
      handler = d;
      url = u;
   }

   // is hop already configured
   public static boolean configured( Hop hop ) {
      File path = new File( hop.home, ".config/hop/wizard.hop" );
      Log.v( "HopConfigurer", "checking file: " + path + "..." +
	     (path.exists() ? "exists" : "missing") );
      return path.exists();
   }

   public void run() {
      try {
	 HttpURLConnection conn = (HttpURLConnection)new URL( url ).openConnection();

	 while( true ) {
	    try {
	       conn.connect();
	       Log.i( "HopConfigurer", "connection established" );
	       Thread.sleep( 1000 );
	       break;
	    } catch( IOException e ) {
	       ;
	    }
	 }
	 // notify that we can start a web browser
	 handler.sendEmptyMessage( HopLauncher.MSG_RUN_WIZARD );
      } catch( Exception e ) {
	 String msg = e.getMessage();
	 if( msg == null ) msg = e.getClass().getName();
	 
	 Log.e( "HopInstaller", msg );
	 hop.handler.sendMessage( android.os.Message.obtain( hop.handler, HopLauncher.MSG_CONFIGURE_FAIL, e ) );
      }
   }
}

