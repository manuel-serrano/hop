/*=====================================================================*/
/*    .../hop/linux/android/src/fr/inria/hop/HopLauncher.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Tue Oct 12 07:38:43 2010 (serrano)                */
/*    Copyright   :  2010 Marcos Dione & Manuel Serrano                */
/*    -------------------------------------------------------------    */
/*    Hop Launcher (and installer)                                     */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.os.*;
import android.util.Log;
import android.content.*;
import android.widget.*;
import android.view.*;
import android.view.View.*;
import android.webkit.*;
import android.net.*;

import java.util.concurrent.ArrayBlockingQueue;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopLauncher extends Activity {
   // Global constants
   public static final int MSG_OUTPUT_AVAILABLE = 1;
   public static final int MSG_PROC_END = 2;
   public static final int MSG_RUN_WIZARD = 3;
   public static final int MSG_INSTALL_FAIL = 4;
   public static final int MSG_CONFIGURE_FAIL = 5;
   public static final int MSG_RUN_FAIL = 6;
   public static final int MSG_CONFIGURE = 7;
   public static final int MSG_RESTART = 8;
   
   final Activity activity = this;
   HopInstaller hopinstaller;
   ProgressDialog progress = null;
   TextView textview;
   ScrollView scrollview;
   final ArrayBlockingQueue<String> queue =
      new ArrayBlockingQueue<String>( 10 );
   final Handler handler = new Handler() {
	 @Override public void handleMessage( Message msg ) {
	    
	    if( msg.what != MSG_OUTPUT_AVAILABLE )
	       Log.i( "Hop", "message=" + msg.what );
	    
	    switch( msg.what ) {
	       case MSG_OUTPUT_AVAILABLE:
		  try {
		     String line = queue.take();
		     synchronized( textview ) {
			textview.append( line );
			scrollview.smoothScrollTo( 0, scrollview.getHeight() );
		     }
		  } catch (InterruptedException e) {
		     ;
		  }
		  break;

	       case MSG_PROC_END:
		  activity.finish();
		  break;

	       case MSG_RUN_WIZARD:
		  progress.dismiss();
		  progress = null;

		  Uri uri = Uri.parse( "http://localhost:" + hop.port + "/hop/wizard" );
		  Intent intent = new Intent(Intent.ACTION_VIEW, uri);
		  startActivity( intent );

/* 		  WebView webview = new WebView( activity );           */
/* 		  webview.getSettings().setJavaScriptEnabled( true );  */
/* 		  setContentView( webview );                           */
/*                                                                     */
/* 		  webview.loadUrl( "http://localhost:" + hop.port + "/hop/wizard" ); */
		  break;

	       case MSG_INSTALL_FAIL:
		  HopUiUtils.fail( hop.activity, "Installation", "failed", (Exception)msg.obj );
		  break;

	       case MSG_CONFIGURE_FAIL:
		  HopUiUtils.fail( hop.activity, "Configuration", "failed", (Exception)msg.obj );
		  break;

	       case MSG_RUN_FAIL:
		  HopUiUtils.fail( hop.activity, "Run", "failed", (Exception)msg.obj );
		  break;

	       case MSG_CONFIGURE:
		  progress.setMessage( "Configuring..." );
		  break;

	       case MSG_RESTART:
		  setContentView( R.layout.main );
		  break;

	       default:
	    }
	 }
      };
   final Hop hop = new Hop( activity, queue, handler );
   final HopAndroid hopandroid = new HopAndroid( activity, 8081, handler );

   @Override public void onCreate( Bundle bundle ) {
      super.onCreate( bundle );
      Log.i( "HopLauncher", "onCreate" );

      // switch to fullscreen
      this.getWindow().setFlags(
	 WindowManager.LayoutParams.FLAG_FULLSCREEN, 
	 WindowManager.LayoutParams.FLAG_FULLSCREEN );

      // install our view
      setContentView( R.layout.main );

      // setup the hostname and hostip
      setupHostname();

      // setup the restart button
      Button buttonr = (Button)findViewById( R.id.restart );
      buttonr.setOnClickListener( new OnClickListener() {
	    public void onClick( View v ) {
	       Log.i( "HopLauncher", "killing background hop" );
	       
	       synchronized( textview ) {
		  textview.append( "killing session..." );
		  hop.restart( true );
	       }
	    }
	 } );

      // setup the exit button
      Button buttone = (Button)findViewById( R.id.exit );
      buttone.setOnClickListener( new OnClickListener() {
	    public void onClick( View v ) {
	       Log.i( "HopLauncher", "exiting..." );
	       
	       synchronized( textview ) {
		  textview.append( "Exiting..." );

		  activity.finish();
	       }
	    }
	 } );

      // grab the text for the output log
      textview = (TextView)activity.findViewById( R.id.textview );
      scrollview = (ScrollView)activity.findViewById( R.id.scrollview );
      Log.i( "HopLauncher", textview + "" );
	 
      try {
	 if( !HopInstaller.installed( hop ) ) {
	    // The install scheduler is a mere thread that waits for
	    // the  installer to complete. It then notifies the application.
	    Thread installscheduler = new Thread( new Runnable () {
		  public void run() {
		     Log.v( "HopLauncher", "waiting installer" );
		     // wait for the installer to complete the installation
		     try {
			hopinstaller.join();
			configure();
		     } catch( Exception e ) {
			HopUiUtils.fail( activity, "HopLauncher", " failed:", e );
		     }

		     Log.v( "HopLauncher", "installation complete" );

		     TextView port = (TextView)activity.findViewById( R.id.port );
		     hop.port = port.getText().toString();
		     hop.start();
		  }
	       } );
      
	    // the installation progress bar
	    progress = new ProgressDialog( this );
	    progress.setTitle( "Hop Installer" );
	    progress.setMessage( "Unpacking..." );
	    progress.setMax( 100 );
	    progress.setProgressStyle( ProgressDialog.STYLE_HORIZONTAL );
	    progress.show();

	    // start the installed and the installscheduler
	    hopinstaller = new HopInstaller( hop, progress );
	    hopinstaller.start();
	    installscheduler.start();
	 } else {
	    hop.start();
	    configure();
	 }

	 hopandroid.start();
      } catch( Exception e ) {
	 HopUiUtils.fail( activity, "HopLauncher", " failed", e );
      }
   }

   private void configure() {
      // configure Hop if needed
      if( !HopConfigurer.configured( hop ) ) {
	 HopConfigurer hopconfigurer =
	    new HopConfigurer( hop, handler,
			       "http://localhost:" + hop.port + "/hop/wizard" );
	 
	 handler.sendEmptyMessage( MSG_CONFIGURE );
	 hopconfigurer.start();
      } else {
	 if( progress != null ) {
	    progress.dismiss();
	    progress = null;
	 }
      }
   }
   
   public void onStart() {
      super.onStart();

      if( progress != null ) progress.show();
   }

   public void onResume() {
      super.onResume();
   }

   public void onDestroy() {
      Log.i( "HopLauncher", "onDestroy" );
      hop.kill();
      super.onDestroy();
   }

   public void setupHostname() {
      TextView hostname = (TextView)activity.findViewById( R.id.hostname );
      TextView hostip = (TextView)activity.findViewById( R.id.hostip );
      
      try {
	 InetAddress addr = java.net.InetAddress.getLocalHost();
	 hostname.append( addr.getCanonicalHostName() );
	 hostip.append( addr.getHostAddress() );
      } catch( Exception _ ) {
	 hostname.append( "" );
	 hostip.append( "" );
      }
   }
}
