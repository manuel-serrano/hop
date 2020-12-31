/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopHzLauncher.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Thu Dec 31 15:23:53 2020 (serrano)                */
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
import java.security.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*    -------------------------------------------------------------    */
/*    Spawning a Hop client (a Hop HZ) goes through the following      */
/*    steps:                                                           */
/*                                                                     */
/*      _:                                                             */
/*         HopInstaller                                                */
/*            -> MSG_INSTALL_UNPACK                                    */
/*      MSG_INSTALL_UNPACK:                                            */
/*         splash                                                      */
/*         startHopActivity                                            */
/*            -> MSG_INSTALL_ACTIVITY_READY                            */
/*             | MSG_INSTALL_ACTIVITY_ERROR                            */
/*             | MSG_INSTALL_ACTIVITY_NOT_FOUND                        */
/*      MSG_INSTALL_ACTIVITY_READY:			               */
/*          HopPermission                                              */
/*      MSG_INSTALL_PERMISSION:                                        */
/*          HopIntenter                                                */
/*          HopService                                                 */
/*          HopDroid                                                   */
/*            -> MSG_HOPDROID_CONNECT                                  */
/*            -> MSG_HOPDROID_START                                    */
/*      MSG_HOPDROID_START:                                            */
/*          installHopHz, if not installed                             */
/*            -> MSG_INSTALL_HZ_READY                                  */
/*             | MSG_INSTALL_HZ_ERROR                                  */
/*      MSG_INSTALL_HZ_READY:                                          */
/*          loadUrl                                                    */
/*          raiseHzActivity                                            */
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

      HopInstaller installer = new HopInstaller( activity, handler, hopapk, hopdir, true );
      installer.exec( hopctx, null );
   }

   // start the Hop Android activity
   private void startHopActivity() {
      Log.i( "HopHzLauncher", "startHopActivity..." );
      
      new Thread( new Runnable() {
	    public void run() {
	       
	       if( Hop.ping( Hop.port, 5 ) > 0 ) {
		  Log.i( "HopHzLauncher", "Hop ready..." );
		  handler.sendEmptyMessage( MSG_INSTALL_ACTIVITY_READY );
	       } else {
		  Log.i( "HopHzLauncher", "Hop not running..." );
		  Intent launchIntent = getPackageManager().getLaunchIntentForPackage( HopConfig.HOPAPK );
		  if( launchIntent != null ) {
		     Log.i( "HopHzLauncher", "Starting Hop activity..." );
		     
		     launchIntent.addFlags( Intent.FLAG_ACTIVITY_PREVIOUS_IS_TOP );
		     startActivity( launchIntent );
		     
		     if( Hop.ping( Hop.port, 20 ) > 0 ) {
			handler.sendEmptyMessage( MSG_INSTALL_ACTIVITY_READY );
		     } else {
			handler.sendEmptyMessage( MSG_INSTALL_ACTIVITY_ERROR );
		     }
		  } else {
		     Log.e( "hopHzlauncher", "Cannot find Hop activity \"" + HopConfig.HOPAPK + "\"" );
		     Toast.makeText( HopHzLauncher.this, "Hop package \"" + HopConfig.HOPAPK + "\"" + " not available", Toast.LENGTH_LONG ).show();
		     handler.sendEmptyMessage( MSG_INSTALL_ACTIVITY_NOTFOUND );
		  }
	       }
	    }
	 } ).start();
   }

   private static String md5sum( String path ) {
      try {
	 MessageDigest md = MessageDigest.getInstance("MD5");
	 InputStream is = new FileInputStream( path );
	 DigestInputStream dis = new DigestInputStream( is, md );
	 byte[] buffer = new byte[1024];
	 String result = "&checksum=";
	 
	 while( dis.read( buffer ) > 0 );
       
	 byte[] b = md.digest();

	 for( int i=0; i < b.length; i++ ) {
	    result += Integer.toString( ( b[i] & 0xff ) + 0x100, 16).substring( 1 );
	 }
	 
	 return result;
      } catch( Throwable e ) {
	 return "";
      }
   }

   static void writeFile( OutputStream op, String source ) throws IOException {
      final int BUFSIZE = 8192;
      FileInputStream is = new FileInputStream( source );
      byte data[] = new byte[ BUFSIZE ];
      int count;
	 
      while( (count = is.read( data, 0, BUFSIZE) ) != -1 ) {
	 op.write( data, 0, count );
      }
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
		  
		  URL hzURL = new URL( "http://localhost:" + Hop.port + "/hop/hz/install" );

		  HttpURLConnection conn = (HttpURLConnection)hzURL.openConnection();

		  final String boundary = "----HZDROID";
		  
		  conn.setRequestMethod( "POST" );
		  conn.setDoOutput( true );
		  conn.setUseCaches( false );
		  
		  conn.setRequestProperty( "Content-Type", "multipart/form-data; boundary=" + boundary );
		  
		  try {
		     OutputStream op = conn.getOutputStream();

		     // op.write( "\r\n".getBytes() );

		     op.write( "--".getBytes() );
		     op.write( boundary.getBytes() );
		     op.write( "\r\n".getBytes() );
		     op.write( "Content-Disposition: form-data; name=\"url\"; filename=\"".getBytes() );
		     op.write( HopConfig.HOPHZ.getBytes() );
		     op.write( "\"\r\n".getBytes() );
		     op.write( "Content-Type: text/plain\r\n".getBytes() );
		     op.write( "\r\n".getBytes() );
		     writeFile( op, path );
		     op.write( "\r\n".getBytes() );
		     
		     op.write( "--".getBytes() );
		     op.write( boundary.getBytes() );
		     op.write( "\r\n".getBytes() );
		     op.write( "Content-Disposition: form-data; name=\"checksum\"\r\n".getBytes() );
		     op.write( "\r\n".getBytes() );
		     op.write( md5sum( path ).toString().getBytes() );
		     op.write( "\r\n".getBytes() );
		     
		     op.write( "--".getBytes() );
		     op.write( boundary.getBytes() );
		     op.write( "--\r\n".getBytes() );

		     op.flush();
		     op.close();
		  } catch( Exception e ) {
		     Log.e( "HopHzLauncher", "Cannot post file:" + e.toString() );
		     e.printStackTrace();
		  }
		     
		     
		  int status = conn.getResponseCode();
		  conn.disconnect();

		  Log.d( "HopHzLauncher", "hz/install status=" + status );
		  
		  if( status == HttpURLConnection.HTTP_OK ) {
		     handler.sendEmptyMessage( MSG_INSTALL_HZ_READY );
		  } else {
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

   // request the running Hop server to uninstall the HZ package
   public static void unInstallHopHz( String id ) {
      new Thread( new Runnable() {
	    public void run() {
	       try {
		  URL hzURL = new URL( "http://localhost:" + Hop.port + "/hop/hz/uninstall?id=" + id );
		  Log.i( "HopHzLauncher", "Uninstall id=" + id );
		  HttpURLConnection conn = (HttpURLConnection)hzURL.openConnection();

		  conn.setRequestMethod( "GET" );

		  conn.setConnectTimeout( 500 );
	 
		  int status = conn.getResponseCode();
		  conn.disconnect();

		  if( status != HttpURLConnection.HTTP_OK ) {
		     Log.d( "HopHzLauncher", "hz/uninstall status=" + status );
		  }
	       } catch( Exception e ) {
		  Log.d( "HopHzLauncher", "cannot uninstall: " + e );
		  e.printStackTrace();
	       }
	    }
	 } ).start();
   }
   
   // request the running Hop server to remove the HZ package
   public static void removeHopHz( String id ) {
      Log.d( "HopHzLauncher", "remove hz" );

      new Thread ( new Runnable() {
	    public void run() {
	       try {
		  URL hzURL = new URL( "http://localhost:" + Hop.port + "/hop/hz/remove?id=" + id );
		  Log.i( "HopHzLauncher", "Remove id=" + id );
		  HttpURLConnection conn = (HttpURLConnection)hzURL.openConnection();

		  conn.setRequestMethod( "GET" );

		  conn.setConnectTimeout( 500 );
	 
		  int status = conn.getResponseCode();
		  conn.disconnect();

		  if( status != HttpURLConnection.HTTP_OK ) {
		     Log.d( "HopHzLauncher", "hz/remove status=" + status );
		  }
	       } catch( Exception e ) {
		  Log.d( "HopHzLauncher", "cannot remove: " + e );
		  e.printStackTrace();
	       }
	    }
	 } ).start();
   }
   
   // raiseHzActivity
   void raiseHzActivity() {
      Log.i( "HopHzLauncher", "raiseHzActivity" );
      
/*       Intent i = new Intent( activity.getApplicationContext(), HopHzLauncher.class ); */
/*       i.addFlags( Intent.FLAG_ACTIVITY_CLEAR_TOP );                 */
/*       i.addFlags( Intent.FLAG_ACTIVITY_REORDER_TO_FRONT );          */
/*                                                                     */
/*       startActivity( i );                                           */
   }
   
   // install handlers
   @Override protected void onInstallUnpacked() {
      Log.d( "HopHzLauncher", "===== onInstallUnpacked" );
      
      hoppermission = new HopPermission( activity, handler );
      hoppermission.exec( hopctx, null );
   }

   @Override protected void onPermission() {
      Log.d( "HopHzLauncher", "===== onPermission" );
      
      splashScreen( "splash.html" );
      startHopActivity();
   }

   @Override protected void onInstallActivityReady() {
      Log.d( "HopHzLauncher", "===== onInstallActivityReady" );

      hopintenter = new HopIntenter( activity, handler, queue );
      hopintenter.exec( hopctx, HopHzService.class );
   }
   
   @Override protected void onHopDroidStart() {
      Log.d( "HopHzLauncher", "===== onHopDroidStart" );

      new Thread( new Runnable() {
	    public void run() {
	       int status = Hop.ping( Hop.port, 0, HopConfig.SERVICE );
	       
	       if( status >= 200 && status != 404 && status < 500 ) { 
		  Log.i( "HopHzLauncher", "Hz service ready..." );
		  handler.sendEmptyMessage( MSG_INSTALL_HZ_READY );
	       } else {
		  installHopHz();
	       }
	    }
	 } ).start();
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

   @Override public void onStop() {
      super.onStop();

      if( hopservice !=null ) hopservice.hopdroid.pushEvent( "phone", "stop" );
   }
}

