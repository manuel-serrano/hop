/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopLauncher.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Mon Nov  5 09:26:04 2012 (serrano)                */
/*    Copyright   :  2010-12 Marcos Dione & Manuel Serrano             */
/*    -------------------------------------------------------------    */
/*    Hop Launcher (and installer)                                     */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.preference.*;
import android.os.*;
import android.util.Log;
import android.content.*;
import android.content.res.*;
import android.widget.*;
import android.view.*;
import android.view.View.*;
import android.net.*;
import android.text.*;

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
   public static final int MSG_HOPDROID_FAILED = 8;
   public static final int MSG_START_HOP_SERVICE = 9;
   public static final int MSG_RESTART_HOP_SERVICE = 10;

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

   // Preferences listeners are stored in a weakhash tables! To prevent
   // the Hop preference listener to be collected we store it into an
   // instance variable
   SharedPreferences.OnSharedPreferenceChangeListener prefslistener = null;
   
   int maxlines = 0;
   StringBuffer textbuffer = new StringBuffer( 2048 );
   CheckBox checkbox, checkbox2, checkbox3, checkbox4;
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
      int lineh = textview.getLineHeight();

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
	 textview.setTextColor( getResources().getColor( R.color.scrollColor ) );
	 scrollview.fullScroll( View.FOCUS_DOWN );
      } else {
	 textview.setTextColor( getResources().getColor( R.color.noScrollColor ) );
      }
   }
   
   final ServiceConnection hopconnection = new ServiceConnection() {
	 public void onServiceConnected( ComponentName className, IBinder service ) {
	    hopservice = ((HopService.HopBinder)service).getService();
	    hopservice.hop.handler = handler;
	    hopservice.hop.queue = queue;
	    hopservice.hopdroid.handler = handler;
	    hopservice.hopdroid.activity = activity;
	 }

	 public void onServiceDisconnected( ComponentName className ) {
	    hopservice = null;
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

			if( !Hop.isBackground() ) {
			   startService( hopintent );
			} else {
			   write_console( "Hop connected...\n" );
			}
			
			bindService( hopintent, hopconnection, Context.BIND_AUTO_CREATE );
			hopconnected = true;
			break;

		     case MSG_RESTART_HOP_SERVICE:
			Log.i( "HopLauncher", "Stopping Hop Service" );
			hopconnected = false;
			unbindService( hopconnection );
			stopService( hopintent );
			hopintent = null;

			handler.sendEmptyMessage( MSG_START_HOP_SERVICE );
			break;

		     default:
		  }
	       }
	    }
	 }
      };
   
   @Override public void onCreate( Bundle bundle ) {
      super.onCreate( bundle );

      // switch to fullscreen
      this.getWindow().setFlags(
	 WindowManager.LayoutParams.FLAG_FULLSCREEN, 
	 WindowManager.LayoutParams.FLAG_FULLSCREEN );

      // install our view
      requestWindowFeature( Window.FEATURE_LEFT_ICON );
      setContentView( R.layout.main );
      getWindow().setFeatureDrawableResource(
	 Window.FEATURE_LEFT_ICON,
	 R.drawable.favicon );
      
      // setup the scroll button
      checkbox = (CheckBox)findViewById( R.id.scrollconsole );
      checkbox.setChecked( true );
      
      // grab the text for the output log
      textview = (TextView)activity.findViewById( R.id.textview );
      scrollview = (ScrollView)activity.findViewById( R.id.scrollview );
      
      //maxlines = textview.getResources().getInteger( R.styleable.TextView_maxLines );
      maxlines = 500;

      // loadPreferences
      loadPreferences();
      
      try {
	 // now that the activity is fully initialized, it's possible
	 // to get the disk location of the package
	 String apk = activity.getApplicationInfo().sourceDir;
	 Hop.root = activity.getApplicationInfo().dataDir;
/* 	 final TextView port = (TextView)activity.findViewById( R.id.port ); */

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

   @Override public boolean onCreateOptionsMenu( Menu menu ) {
      MenuInflater inflater = getMenuInflater();
      inflater.inflate( R.menu.hop_menu, menu );
      return true;
   }

   @Override
   public boolean onOptionsItemSelected( MenuItem item ) {
      // Handle item selection
      switch( item.getItemId() ) {
	 case R.id.menu_settings:
	    Log.d( "HopLauncher", "starting preference activity" );
	    Intent intent = new Intent( getBaseContext(), HopSettings.class );
	    startActivity( intent );
	    return true;
	    
	 case R.id.menu_quit:
 	    kill( 0 );
            return true;
	    
	 case R.id.menu_clear:
            textbuffer.delete( 0, textbuffer.length() - 1 );
	    write_console( "" );
            return true;
	    
	 case R.id.menu_restart:
	    restart();
            return true;

	 case R.id.menu_about:
	    about();
	    return true;
	    
	 default:
            return super.onOptionsItemSelected( item );
      }
   }
   
   private void setHopPort( String port ) {
      Log.d( "HopLauncher", "set port=" + port );
      Hop.port = port;
      hop_wizard_url = "http://localhost:" + port + "/hop/wizard";
   }

   private void about() {
      AlertDialog.Builder builder = new AlertDialog.Builder( this );
      final View aview = getLayoutInflater().inflate(R.layout.about, null, false);
      builder.setView( aview );
      final AlertDialog dialog = builder.create();

      Button dialogButton = (Button)aview.findViewById( R.id.dialogButtonOK );

      dialogButton.setOnClickListener (new OnClickListener() {
	    @Override
	    public void onClick( View v ) {
	       dialog.dismiss();
	    }
	 } );
 
      dialog.show();
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


   @Override
   public void onStart() {
      super.onStart();

      if( progress != null ) progress.show();
   }

   @Override
   public void onStop() {
      super.onStop();
   }

   private void loadPreferences() {
      final Resources res = getResources();
      final SharedPreferences sp =
	 PreferenceManager.getDefaultSharedPreferences( this );

      final String defaultport = res.getString( R.string.hopport );
      
      setHopPort( sp.getString( "hop_port", defaultport ) );
      Hop.zeroconf = sp.getBoolean( "hop_zeroconf", true );
      hop_log = sp.getBoolean( "hop_log", false );


      if( prefslistener == null ) {
	 prefslistener = new SharedPreferences.OnSharedPreferenceChangeListener() {
	       public void onSharedPreferenceChanged( SharedPreferences sp, String key ) {
		  if( key.equals( "hop_port" ) ) {
		     setHopPort( sp.getString( "hop_port", defaultport ) );
		     return;
		  } 
		  if( key.equals( "hop_zeroconf" ) ) {
		     Hop.zeroconf = sp.getBoolean( "hop_zeroconf", true );
		     return;
		  }
		  if( key.equals( "hop_log" ) ) {
		     hop_log = sp.getBoolean( "hop_log", false );
		     return;
		  }
		  if( key.equals( "hop_debug" ) ) {
		     Hop.debug = sp.getString( "hop_debug", "" );
		     return;
		  }
	       }
	    };
	 
	 sp.registerOnSharedPreferenceChangeListener( prefslistener );
      }
   }
   
   @Override
   public void onResume() {
      super.onResume();
   }

   @Override
   public void startActivityForResult( Intent intent, int requestCode ) {
      super.startActivityForResult( intent, requestCode );
   }
   
   protected void onActivityResult( int reqcode, int rescode, Intent intent ) {
      Log.v( "HopLauncher", "onActivityResult reqcode=" + reqcode + " rescode=" + rescode + " intent=" + intent + " activity=" + this );
      HopPlugin.onActivityResult( reqcode, rescode, intent );
      super.onActivityResult( reqcode, rescode, intent );
   }

   private synchronized void kill( int waitms ) {
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
	    hopconnected = false;
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

   private synchronized void restart() {
      textbuffer.delete( 0, textbuffer.length() );
      write_console( "Restarting Hop...\n" );
      
      handler.sendEmptyMessage( MSG_RESTART_HOP_SERVICE );
   }
}
