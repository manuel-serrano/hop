/*=====================================================================*/
/*    .../hopdac/arch/android/src/fr/inria/hop/HopConfigurer.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct  8 15:35:26 2010                          */
/*    Last change :  Tue Jul  5 18:09:14 2016 (serrano)                */
/*    Copyright   :  2010-16 Manuel Serrano                            */
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
public class HopConfigurer implements HopStage {
   // global constants
  final static String CHMOD = "/system/bin/chmod 777"; 

   // instance variables
   Handler handler;
   File home;
   
   Boolean abort = false;

   // constructor
   public HopConfigurer( Handler hdl, File hme ) {
      super();

      handler = hdl;
      home = hme;
   }

   // is hop already configured
   public static boolean configured( File home ) {
      File path = new File( home, ".config/hopdac/wizard.hop" );
      return path.exists();
   }

   // hopdacrc
   void hopdacrc() throws Exception {
      File rcdir = new File( home, ".config/hopdac" );

      synchronized( abort ) {
	 if( !abort ) {
	    if( !rcdir.isDirectory() ) {
	       Log.i( "HopConfigurer", "mkdir " +  rcdir );
	       rcdir.mkdirs();
	    }

	    // wizard.hop
	    FileOutputStream out = new FileOutputStream( new File( rcdir, "wizard.hop" ) );

	    try {
	       Log.i( "HopConfigurer", "generating " +  out );
	       out.write( (";; generated file, HopConfigurer " + new Date() + "\n").getBytes() );
	       out.write( ";; anonymous user\n".getBytes() );
	       out.write( "(add-user! \"anonymous\" :services '(public hopdac) :directories '*)\n".getBytes() );
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
      
   public void exec() {
      Log.d( "HopConfigurer", "exec configured(" + home + ")=" + configured( home ) );

      if( !configured( home ) ) {
	 try {
	    this.hopdacrc();
	    handler.sendEmptyMessage( HopLauncher.MSG_STATE_NEXT );
	 } catch( Exception e ) {
	    raise( e );
	 }
      } else {
	 handler.sendEmptyMessage( HopLauncher.MSG_STATE_NEXT );
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

