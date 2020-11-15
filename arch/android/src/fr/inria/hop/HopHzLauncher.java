/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopHzLauncher.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Sun Nov 15 15:47:53 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop Hz Launcher (used to launch an Hop client app).              */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import java.util.*;
import java.net.*;
import java.io.*;

import android.app.*;
import android.preference.*;
import android.os.*;
import android.util.Log;
import android.content.*;
import android.content.res.*;
import android.widget.*;
import android.view.*;
import android.view.View.*;
import android.webkit.*;
import android.net.*;
import android.text.*;
import android.provider.*;
import android.graphics.drawable.*; 
import android.graphics.*; 

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopHzLauncher extends HopLauncher {

   // static initialization
   static {
      HopUtils.initWifiPolicy();
   }

   // onLaunch
   @Override public void onLaunch() {
      String hopapk = activity.getApplicationInfo().sourceDir;
      String hopdir = activity.getApplicationInfo().dataDir + "/assets";

      HopInstaller installer = new HopInstaller( activity, handler, hopapk, hopdir, false );
      installer.exec( hopctx );
   }

   // start the Hop Android activity
   private void startHopActivity() {
      new Thread( new Runnable() {
	    public void run() {
	       if( Hop.ping( Hop.port, 5 ) ) {
		  handler.sendEmptyMessage( MSG_INSTALL_ACTIVITY_READY );
	       } else {
		  Log.i( "HopHzLauncher", "Hop not running..." );
		  Intent launchIntent = getPackageManager().getLaunchIntentForPackage( HopConfig.HOPAPK );
		  if( launchIntent != null ) {
		     Log.i( "HopHzLauncher", "Starting Hop activity..." );

		     startActivity( launchIntent );
		     if( Hop.ping( Hop.port, 20 ) ) {
			handler.sendEmptyMessage( MSG_INSTALL_ACTIVITY_READY );
		     } else {
			handler.sendEmptyMessage( MSG_INSTALL_ACTIVITY_ERROR );
		     }
		  } else {
		     Log.e( "hopclientlauncher", "Cannot find Hop activity \"" + HopConfig.HOPAPK + "\"" );
		     Toast.makeText( HopHzLauncher.this, "Hop package \"" + HopConfig.HOPAPK + "\"" + " not available", Toast.LENGTH_LONG ).show();
		     handler.sendEmptyMessage( MSG_INSTALL_ACTIVITY_NOTFOUND );
		  }
	       }
	    }
	 } ).start();
   }

   // request the running Hop server to install the HZ package
   private void installHopHz() {
      Log.d( "HopHzLauncher", "Install hz " + HopConfig.HOPHZ );
      
      new Thread( new Runnable() {
	    public void run() {
	       try {
		  String hopdir = activity.getApplicationInfo().dataDir + "/assets";
		  String hophzdir = activity.getApplicationInfo().dataDir + "/assets/hz/";
		  String path = hophzdir + HopConfig.HOPHZ;
		  URL hzURL = new URL( "http://localhost:" + Hop.port + "/hop/hz/install?url=" + path );
		  Log.i( "HopHzLauncher", "Install URL=" + hzURL.toString() );

		  HopUtils.chmod( hopdir, 555 );
		  HopUtils.chmod( hophzdir, 555 );
		  HopUtils.chmod( path, 644 );
		  HttpURLConnection conn = (HttpURLConnection)hzURL.openConnection();

		  conn.setRequestMethod( "GET" );

		  conn.setConnectTimeout( 500 );
	 
		  int status = conn.getResponseCode();
		  conn.disconnect();

		  if( status == HttpURLConnection.HTTP_OK ) {
		     handler.sendEmptyMessage( MSG_INSTALL_HZ_READY );
		  } else {
		     Log.d( "HopHzLauncher", "hz/install status=" + status );
		     handler.sendEmptyMessage( MSG_INSTALL_HZ_ERROR );
		  }
	       } catch( Exception e ) {
		  Log.d( "HopHzLauncher", "Cannot install hz: " + e );
		  e.printStackTrace();
		  handler.sendEmptyMessage( MSG_INSTALL_HZ_ERROR );
	       }
	    }
	 } ).start();
   }

   // hzInstalledp
   static boolean hzInstalledp( String port, String service ) {
      return Hop.ping( port, 0, service );
   }

   // raiseHzActivity
   void raiseHzActivity() {
      Intent i = new Intent( activity.getApplicationContext(), HopHzLauncher.class );
      i.addFlags( Intent.FLAG_ACTIVITY_CLEAR_TOP );
      i.addFlags( Intent.FLAG_ACTIVITY_REORDER_TO_FRONT );
      startActivity( i );      
   }
   
   // install handlers
   @Override protected void onInstallUnpacked() {
      Log.d( "HopHzLauncher", "===== onInstallUnpacked" );
      
      splashScreen( "splash.html" );
      startHopActivity();
   }

   @Override protected void onInstallActivityReady() {
      Log.d( "HopHzLauncher", "===== onInstallActivityReady" );
      
      if( hzInstalledp( Hop.port, HopConfig.SERVICE ) ) {
	 onInstallHzReady();
      } else {
	 installHopHz();
      }
   }

   @Override protected void onInstallActivityError() {
      Log.d( "HopHzLauncher", "===== onInstallActivityError" );
      
      splashScreen( "hoperror.html" );
   }
   
   @Override protected void onInstallActivityNotFound() {
      Log.d( "HopHzLauncher", "===== onInstallActivityNotFound" );
      
      splashScreen( "hopnotfound" );
   }

   @Override protected void onInstallHzReady() {
      Log.d( "HopHzLauncher", "===== onInstallHzReady" );
      
      webview.loadUrl( "http://localhost:" + Hop.port + HopConfig.SERVICE );
      raiseHzActivity();
   }
   
   @Override protected void onInstallHzError() {
      Log.d( "HopHzLauncher", "===== onInstallHzError" );
      
      splashScreen( "hoperror.html" );
      raiseHzActivity();
   }
}
