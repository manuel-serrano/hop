/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopUiUtils.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct  1 09:13:38 2010                          */
/*    Last change :  Sat Nov 21 18:33:22 2020 (serrano)                */
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

   // setStatusBarColor
   protected static void setStatusBarColor( Activity a, String color ) {
      Window window = a.getWindow();

      Log.d( "HopUiUtils", "setStatusBarColor: " + color );
      
      // clear FLAG_TRANSLUCENT_STATUS flag:
      window.clearFlags( WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS );

      // add FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS flag to the window
      window.addFlags( WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS );

      // finally change the color
      window.setStatusBarColor( Color.parseColor( color ) );
   }

   // setWindowFlag
   public static void setWindowFlag( Activity a, final int bits, boolean on ) {
	Window win = a.getWindow();
	WindowManager.LayoutParams winParams = win.getAttributes();
	if( on ) {
	   winParams.flags |= bits;
	} else {
	   winParams.flags &= ~bits;
	}
	win.setAttributes( winParams );
   }
   
   // setStatusBarTransparent
   protected static void setStatusBarTransparent( Activity a ) {
      Log.d( "HopUiUtils", "setStatusBarTransparent" );

/*       if( android.os.Build.VERSION.SDK_INT >= 29 ) {                */
/* 	 Window w = a.getWindow();                                     */
/* 	 w.setFlags( WindowManager.LayoutParams.FLAG_LAYOUT_NO_LIMITS, WindowManager.LayoutParams.FLAG_LAYOUT_NO_LIMITS ); */
/*       } else if( android.os.Build.VERSION.SDK_INT >= 19 && android.os.Build.VERSION.SDK_INT < 21 ) { */
/* 	 setWindowFlag( a, WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS, true ); */
/*       } else                                                        */
/* 	   if( android.os.Build.VERSION.SDK_INT >= 21 ) {              */
/* 	 setWindowFlag( a, WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS, false ); */
/* 	 a.getWindow().setStatusBarColor( Color.TRANSPARENT );         */
/*       } else if( android.os.Build.VERSION.SDK_INT >= 19 ) {         */
/* 	 a.getWindow().getDecorView().setSystemUiVisibility( View.SYSTEM_UI_FLAG_LAYOUT_STABLE | View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN ); */
/*       }                                                             */

      setStatusBarColor( a, "#33006666" );
      Log.d( "HopUiUtils", "SDK=" + android.os.Build.VERSION.SDK_INT );
   }

   // unitUI
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

      // statusbar
      Log.d( "HopUiUtils", "statusbarcolor=" + HopConfig.UISTATUSBARCOLOR );
      if( HopConfig.UISTATUSBARCOLOR.equals( "transparent" ) ) {
	 setStatusBarTransparent( a );
      } else if( !HopConfig.UISTATUSBARCOLOR.equals( "default" ) ) {
	 setStatusBarColor( a, HopConfig.UISTATUSBARCOLOR );
      }

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
