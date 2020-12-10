/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopConfigurer.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct  8 15:35:26 2010                          */
/*    Last change :  Thu Dec 10 09:40:54 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
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
import android.content.res.*;
import android.content.Context;
import android.os.*;

import java.util.*;
import java.util.zip.*;
import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopConfigurer implements HopStage {
   // instance variables
   Handler handler;
   File home;
   Activity activity;
   
   Boolean abort = false;

   // constructor
   public HopConfigurer( Activity a, Handler hdl, File hme ) {
      super();

      handler = hdl;
      home = hme;
      activity = a;
   }

   // is hop already configured
   public static boolean configured( Context context, File home ) {
      final Resources res = context.getResources();
      String app = res.getString( R.string.hopapp );
      
      File path = new File( home, ".config/" + app + "/wizard.hop" );
      if( path.exists() ) return true;
      
      path = new File( home, ".config/" + app + "/hoprc.js" );
      return( path.exists() );
   }

   // hopapprc
   void hopapprc( Context context ) throws Exception {
      final Resources res = context.getResources();
      String app = res.getString( R.string.hopapp );
      File rcdir = new File( home, ".config/" + app );

      Log.i( "HopConfigurer", "hopapprc rcdir=" + rcdir );
      synchronized( abort ) {
	 if( !abort ) {
	    if( !rcdir.isDirectory() ) {
	       Log.i( "HopConfigurer", "mkdir " +  rcdir );
	       rcdir.mkdirs();
	    }

	    // wizard.hop
	    if( HopConfig.DEFLANG.equals( "scheme" ) ) {
	       FileOutputStream out = new FileOutputStream( new File( rcdir, "wizard.hop" ) );
	       try {
		  Log.i( "HopConfigurer", "generating " +  out );
		  out.write( (";; generated file, HopConfigurer " + new Date() + "\n").getBytes() );
		  out.write( ";; anonymous user\n".getBytes() );
		  out.write( ("(add-user! \"anonymous\" :services '(public " + app + ") :directories '*)\n").getBytes() );
	       } finally {
		  out.close();
	       }

	       // hoprc.hop
	       out = new FileOutputStream( new File( rcdir, "hoprc.hop" ) );

	       try {
		  Log.i( "HopConfigurer", "generating " +  out );
		  out.write( (";; generated file, HopConfigurer " + new Date() + "\n").getBytes() );
		  out.write( ";; default rc file\n".getBytes() );
		  out.write( "(let ((path (make-file-name (hop-etc-directory) \"hoprc.hop\"))) (when (file-exists? path) (hop-load path)))\n".getBytes() );
		  out.write( ";; wizard file\n".getBytes() );
		  out.write( "(hop-load-rc \"wizard.hop\")\n".getBytes() );
	       } finally {
		  out.close();
	       }
	    } else if( HopConfig.DEFLANG.equals( "javascript" ) ) {
	       // hoprc.js
	       FileOutputStream out = new FileOutputStream( new File( rcdir, "hoprc.js" ) );

	       try {
		  Log.i( "HopConfigurer", "generating " + out );
      
		  out.write( "// generated file (HopInstaller), edit at your own risk\n".getBytes() );
		  out.write( "require( \"".getBytes() );
		  out.write( activity.getApplicationInfo().dataDir.getBytes() );
		  out.write( "/assets/rcdir/hoprc.js".getBytes() );
		  out.write( "\" );\n".getBytes() );
	       } finally {
		  out.close();
	       }
	    } else {
	       throw new Exception( "Unknown default language: " +
		  HopConfig.DEFLANG );
	    }
	 }
      }
   }
      
   private void raise( Exception e ) {
      String msg = e.getMessage();
      
      Log.e( "HopConfigurer", e.toString() );
      e.printStackTrace();
      
      if( msg == null ) msg = e.getClass().getName();
      
      handler.sendMessage( android.os.Message.obtain( handler, HopLauncher.MSG_CONFIGURE_FAIL, e ) );
   }
      
   public void exec( Context context ) {
      Log.d( "HopConfigurer", "exec configured(" + home + ")=" + configured( context, home ) );

      if( !configured( context, home ) ) {
	 try {
	    this.hopapprc( context );
	    handler.sendEmptyMessage( HopLauncher.MSG_INSTALL_CONFIGURED );
	 } catch( Exception e ) {
	    Log.e( "HopConfigurer", "Cannot configure " + e.toString() );
	    raise( e );
	 }
      } else {
	 handler.sendEmptyMessage( HopLauncher.MSG_INSTALL_CONFIGURED );
      }
   }

   public void abort() {
      Log.d( "HopConfigurer", "abort" );
      
      try {
	 synchronized( abort ) {
	    abort = false;
	 }
      } catch( Exception e ) {
	 raise( e );
      }
   }
}

