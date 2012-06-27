/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopLauncher.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Wed Jun 27 09:15:22 2012 (serrano)                */
/*    Copyright   :  2010-12 Marcos Dione & Manuel Serrano             */
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
import android.net.*;

import java.util.concurrent.ArrayBlockingQueue;
import java.net.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopLauncher extends Activity {
   // Global constants
   public static final int MSG_HOP_OUTPUT_AVAILABLE = 1;
   public static final int MSG_HOP_ENDED = 2;
   public static final int MSG_RUN_WIZARD = 3;
   public static final int MSG_INSTALL_FAILED = 4;
   public static final int MSG_CONFIGURE_FAIL = 5;
   public static final int MSG_HOP_FAILED = 6;
   public static final int MSG_CONFIGURE = 7;
   public static final int MSG_RESTART = 8;
   public static final int MSG_HOPDROID_FAILED = 9;
   public static final int MSG_START_HOP_SERVICE = 10;
   public static final int MSG_RESTART_HOP_SERVICE = 11;

   // hop configuration class variable
   static boolean hop_log = true;
   static String hop_wizard_url;
   
   // instance variables
   boolean killed = false;
   
   final Activity activity = this;
   HopInstaller hopinstaller;
   Intent hopintent = null;
   HopService hopservice = null;
   Hop hopconf = null;
   boolean hopconnected = false;

   ProgressDialog progress = null;
   
   int maxlines = 0;
   StringBuffer textbuffer = new StringBuffer( 2048 );
   CheckBox checkbox, checkbox2, checkbox3;
   TextView textview;
   ScrollView scrollview;
   final ArrayBlockingQueue<String> queue =
      new ArrayBlockingQueue<String>( 10 );
   
   private void startProgressBar() {
      progress = new ProgressDialog( this );
      progress.setTitle( "Hop Installer" );
      progress.setMessage( "Unpacking..." );
      progress.setMax( 100 );
      progress.setProgressStyle( ProgressDialog.STYLE_HORIZONTAL );
      progress.show();
   }
   
   private void cleanupProgressBar() {
      if( progress != null ) {
	 progress.dismiss();
	 progress = null;
      }
   }

   // write a line on Hop console
   private void write_console( String line ) {
      synchronized( textview ) {
	 textbuffer.append( line );
	 textview.setText( textbuffer );
			   
	 int lc = textview.getLineCount();

	 if( lc > maxlines - 20 ) {
	    int index = 0;

	    for( int counter = 0; counter < 20; counter++ ) {
	       int i = textbuffer.indexOf( "\n", index );

	       if( i > 0 ) {
		  index = i;
	       } else {
		  break;
	       }
	    }

	    // if no lines found remove a bunch of chars
	    if( index == 0 ) index = 1024;

	    textbuffer.delete( 0, index );
	    textview.setText( textbuffer );
	    lc = textview.getLineCount();
	 }
			   
	 if( checkbox.isChecked() ) {
	    int y = textview.getLineHeight() * lc;
	    scrollview.scrollTo( 0, y );
	 }
	 
	 hop_log = checkbox2.isChecked();
      }
   }
   
   final ServiceConnection hopconnection = new ServiceConnection() {
	 public void onServiceConnected( ComponentName className, IBinder service ) {
	    hopservice = ((HopService.HopBinder)service).getService();
	    hopservice.hop.handler = handler;
	    hopservice.hop.queue = queue;
	    hopservice.hopdroid.handler = handler;
	    hopservice.hopdroid.activity = activity;
	    Log.i( "HopLauncher", "Launcher connected..." );
	 }

	 public void onServiceDisconnected( ComponentName className ) {
	    hopservice = null;
	    Log.i( "HopLauncher", "Launcher disconnected..." );
	 }
      };

   final Handler handler = new Handler() {
	 @Override public void handleMessage( Message msg ) {

	    synchronized( activity ) {
	       if( !killed ) {
		  switch( msg.what ) {
		     case MSG_HOP_OUTPUT_AVAILABLE:
			try {
			   write_console( queue.take() );
			} catch( InterruptedException _ ) {
			   ;
			}
			break;

		     case MSG_HOP_ENDED:
			write_console( "Hop ended...\n" );
			kill( 4000 );
			break;

		     case MSG_HOP_FAILED:
			Log.e( "HopLauncher", "hop failed..." );
			HopUiUtils.fail( activity, "Hop", "failed", (Exception)msg.obj );
			kill( 4000 );
			break;

		     case MSG_HOPDROID_FAILED:
			Log.e( "HopLauncher", "hopdroid failed..." );
			HopUiUtils.fail( activity, "HopDroid", "failed", (Exception)msg.obj );
			kill( 4000 );
			break;

		     case MSG_RUN_WIZARD:
			cleanupProgressBar();

			Uri uri = Uri.parse( hop_wizard_url );
			Intent intent = new Intent( Intent.ACTION_VIEW, uri );
			// give time to Hop to be ready
			try {
			   Thread.sleep( 2000 );
			} catch( Exception _ ) {
			   ;
			}
			startActivity( intent );
			break;

		     case MSG_INSTALL_FAILED:
			Log.e( "HopLauncher", "installation failed..." );
			HopUiUtils.fail( activity, "HopInstaller", "failed", (Exception)msg.obj );
			break;

		     case MSG_CONFIGURE_FAIL:
			Log.e( "HopLauncher", "configuration failed..." );
			HopUiUtils.fail( activity, "HopConfigurer", "failed", (Exception)msg.obj );
			break;

		     case MSG_CONFIGURE:
			progress.setMessage( "Configuring..." );
			break;

		     case MSG_START_HOP_SERVICE:
			Log.i( "HopLauncher", "Starting Hop Service" );
			if( hopintent == null ) {
			   hopintent = new Intent( getApplicationContext(), HopService.class );
			}

			startService( hopintent );
			bindService( hopintent, hopconnection, Context.BIND_AUTO_CREATE );
			hopconnected = true;
			break;

		     case MSG_RESTART_HOP_SERVICE:
			Log.i( "HopLauncher", "Re-Starting Hop Service" );
			stopService( hopintent );
			startService( hopintent );
			
			break;

		     default:
		  }
	       }
	    }
	 }
      };
   
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
	       Log.i( "HopLauncher", "restarting" );

	       write_console( "\n\nRestarting session...\n" );
	       handler.sendEmptyMessage( MSG_RESTART_HOP_SERVICE );
	    }
	 } );

      // setup the exit button
      Button buttone = (Button)findViewById( R.id.exit );
      buttone.setOnClickListener( new OnClickListener() {
	    public void onClick( View v ) {
	       // the documentation for onDestroy says that the method is
	       // not necessarily invoked when the application is finished
	       // so we force killing hop and hopdroid first.
	       kill( 0 );
	    }
	 } );

      // setup the clear button
      Button buttonc = (Button)findViewById( R.id.clearconsole );
      buttonc.setOnClickListener( new OnClickListener() {
	    public void onClick( View v ) {
	       textbuffer.delete( 0, textbuffer.length() - 1 );
	       write_console( "" );
	    }
	 } );
      
      // setup the scroll button
      checkbox = (CheckBox)findViewById( R.id.scrollconsole );
      checkbox.setChecked( true );
      
      // setup the log button
      checkbox2 = (CheckBox)findViewById( R.id.log );
      checkbox2.setChecked( hop_log );
      
      // setup the debug button
      checkbox3 = (CheckBox)findViewById( R.id.debug );
      checkbox3.setChecked( Hop.debug );
      
      // grab the text for the output log
      textview = (TextView)activity.findViewById( R.id.textview );
      scrollview = (ScrollView)activity.findViewById( R.id.scrollview );
      
      //maxlines = textview.getResources().getInteger( R.styleable.TextView_maxLines );
      maxlines = 500;

      try {
	 // now that the activity is fully initialized, it's possible
	 // to get the disk location of the package
	 String apk = activity.getApplicationInfo().sourceDir;
	 Hop.root = activity.getApplicationInfo().dataDir;

	 setHopPort( Hop.port );

	 if( !HopInstaller.installed( Hop.root ) ) {

	    // The install scheduler is a mere thread that waits for
	    // the  installer to complete. It then notifies the application.
	    Thread installscheduler = new Thread( new Runnable () {
		  public void run() {
		     Log.v( "HopLauncher", "waiting installer" );
		     // wait for the installer to complete the installation
		     try {
			hopinstaller.join();
		     } catch( Exception e ) {
			HopUiUtils.fail( activity, "HopLauncher", " failed:", e );
		     }

		     Log.v( "HopLauncher", "installation complete" );

		     TextView port = (TextView)activity.findViewById( R.id.port );
		     setHopPort( port.getText().toString() );

		     Hop.debug = checkbox3.isChecked();
		     
		     if( !HopConfigurer.configured( Hop.HOME ) ) {
			Log.v( "HopLauncher", "progress=" + progress );
			handler.sendEmptyMessage( MSG_CONFIGURE );
			configure();
		     } else {
			cleanupProgressBar();
			handler.sendEmptyMessage( MSG_START_HOP_SERVICE );
		     }
		  }
	       } );
      
	    // the installation progress bar
	    startProgressBar();

	    // start the installed and the installscheduler
	    hopinstaller = new HopInstaller( handler, progress, apk, Hop.root );
	    hopinstaller.start();
	    installscheduler.start();
	 } else {
	    if( !HopConfigurer.configured( Hop.HOME ) ) {
	       configure();
	    } else {
	       handler.sendEmptyMessage( MSG_START_HOP_SERVICE );
	    }
	 }
      } catch( Exception e ) {
	 HopUiUtils.fail( activity, "HopLauncher", " failed", e );
      }
   }

   private void setHopPort( String port ) {
      Hop.port = port;
      hop_wizard_url = "http://localhost:" + Hop.port + "/hop/wizard";
   }
   
   private void configure() {
      Log.v( "HopLauncher", "configure..." );
      // start a background Hop
      if( hopconf == null || !hopconf.isRunning() ) {
	 hopconf = new Hop( queue, handler );
	 hopconf.startWithArg( "--accept-kill" );
      }
      
      // configure an installed Hop
      HopConfigurer hopconfigurer = new HopConfigurer( handler, hop_wizard_url );
      hopconfigurer.start();
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

   public void onStart() {
      super.onStart();

      if( progress != null ) progress.show();
   }

   public void onResume() {
      super.onResume();
   }

   public void onDestroy() {
/*       hop.kill();                                                   */
/*       hopdroid.kill();                                              */
/*       stopService( hopintent );                                     */
      super.onDestroy();
   }

   protected void onActivityResult( int reqcode, int rescode, Intent intent ) {
      Log.v( "HopLauncher", "onActivityResult reqcode=" + reqcode );
      HopPlugin.onActivityResult( reqcode, rescode, intent );
      super.onActivityResult( reqcode, rescode, intent );
   }

   synchronized private void kill( int waitms ) {
      if( !killed ) {
	 Log.i( "HopLauncher", ">>> kill" );
	 killed = true;
	 
	 // give time to read the console messages
	 if( waitms > 0 ) {
	    try {
	       Thread.sleep( waitms );
	    } catch( Exception _ ) {
	       ;
	    }
	 }

	 if( hopconf != null && hopconf.isRunning() ) {
	    hopconf.kill();
	 }
	 if( hopconnected ) {
	    Log.i( "HopLauncher", "Unbinding service..." );
	    unbindService( hopconnection );
	 }
      
	 Log.i( "HopLauncher", "Stopping service..." );
	 if( hopintent != null ) {
	    stopService( hopintent );
	 }
	 Log.i( "HopLauncher", "Finishing activity..." );
	 finish();
      
	 Log.i( "HopLauncher", "<<< kill" );
      }
   }
}
