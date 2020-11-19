/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopUiUtils.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct  1 09:13:38 2010                          */
/*    Last change :  Thu Nov 19 14:50:47 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    UI Utility functions                                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import java.io.*;

import android.app.*;
import android.content.DialogInterface;
import android.util.Log;
import android.view.*;
import android.view.View.*;
import android.webkit.*;
import android.graphics.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopUiUtils {

   // Show a simple message
   public static void alert( final Activity activity,
			     final String msg,
			     final String ok,
			     final boolean exit ) {
      try {
	 AlertDialog.Builder builder = new AlertDialog.Builder( activity );
	 builder.setMessage( msg )
	    .setCancelable( false )
	    .setPositiveButton( ok, new DialogInterface.OnClickListener() {
		  public void onClick( DialogInterface dialog, int id ) {
		     dialog.dismiss();
		     if( exit ) {
			activity.setResult( activity.RESULT_CANCELED );
			activity.finish();
		     }
		  }
	       } );
	 AlertDialog alert = builder.create();
	 alert.show();
      } catch( Exception e ) {
	 Log.e( "HopUiUtils", "failure in alert e=" + e.toString() );
	 e.printStackTrace();
      }
   }
   
   // Show a simple message
   public static void alert( final Activity activity,
			     final String msg ) {
      alert( activity, msg, "ok", false );
   }
   
   // A failure
   public static void failExit( final Activity activity,
				final String task,
				final String msg,
				final Object o ) {
      if( o instanceof Exception ) {
	 Exception e = (Exception)o;
	 String emsg = e.getClass().getName() + ": " + e.getMessage();
	 String m = task + " " + msg + ": " + emsg;

	 Log.e( task, m );
	 alert( activity, m, "ok", true );
      } else {
	 Log.d( "HopUiUtils", "msg=" + msg );
	 Log.d( "HopUiUtils", "o=" + o.toString() );
	 Log.d( "HopUiUtils", "task=" + task );
	 alert( activity, "Hop error \"" + msg + "\" (" + o.toString() + ")",
		"ok", true );
      }
   }

   protected static WebView initUI( Activity a ) {
      WebView webview;

      final String BOOT_PAGE = "<!DOCTYPE html><html><head><meta http-equiv='Content-Type' content='text/html; charset=UTF-8'><meta name='viewport' content='width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=no'></head><body style='background-color: #222; color: #eee'>" + a.getApplicationContext().getString( R.string.hopapp ) + " booting...</body></html>";
      
      // remove title bar
      if( HopConfig.NOTITLE ) {
	 a.requestWindowFeature( Window.FEATURE_NO_TITLE );
      }
      
      // action bar color
      //this.requestWindowFeature( Window.FEATURE_ACTION_BAR );
      //ActionBar bar = getActionBar();
      //bar.setBackgroundDrawable(new ColorDrawable(Color.parseColor("#3e3e3e")));

      a.setContentView( R.layout.main );

      // grab the view
      webview = (WebView)a.findViewById( R.id.webview );
      WebSettings webSettings = webview.getSettings();
      webSettings.setJavaScriptEnabled( true );
      webSettings.setBuiltInZoomControls( true );
      webSettings.setAppCacheEnabled( false );

      webview.requestFocusFromTouch();

      webview.setWebViewClient( new WebViewClient() );
      webview.setWebChromeClient( new WebChromeClient() );

      // start with the splash message
      webview.loadData( BOOT_PAGE, "text/html; charset=UTF-8", null );

      return webview;
   }

   protected static void splashScreen( Activity activity, WebView webview, String name ) {
      String hopdir = activity.getApplicationInfo().dataDir + "/assets";
      String path = hopdir + "/etc/hop/" + HopConfig.HOPRELEASE + "/splash/" + name + ".html";
      String url = "file://" + path;
      File f = new File( path );

      if( f.exists() ) {
	 Log.d( "HopClientLauncher", "splash url=" + url );
	 webview.loadUrl( url );
      } else {
	 Log.e( "HopClientLauncher", "splash does not exist: " + f.toString() );
      }
   }
}
