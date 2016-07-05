/*=====================================================================*/
/*    .../hop/3.1.x/arch/android/src/fr/inria/hop/HopLauncher.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Fri Jul  1 14:01:29 2016 (serrano)                */
/*    Copyright   :  2010-16 Marcos Dione & Manuel Serrano             */
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
import android.provider.*;
import android.media.AudioManager;

import java.util.concurrent.ArrayBlockingQueue;
import java.net.*;
import java.io.*;
import java.lang.reflect.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopLauncher extends Activity {
   // Global constants
   public static final int MSG_HOP_OUTPUT_AVAILABLE = 1;
   public static final int MSG_HOPDROID_ENDED = 2;
   public static final int MSG_RUN_WIZARD = 3;
   public static final int MSG_INSTALL_FAILED = 4;
   public static final int MSG_CONFIGURE_FAIL = 5;
   public static final int MSG_HOP_FAILED = 6;
   public static final int MSG_CONFIGURE = 7;
   public static final int MSG_HOPDROID_FAILED = 8;
   public static final int MSG_START_HOP_SERVICE = 9;
   public static final int MSG_RESTART_HOP_SERVICE = 10;
   public static final int MSG_KILL_HOP_SERVICE = 11;
   public static final int MSG_REBIND_HOP_SERVICE = 12;

   private static String WIFI_SLEEP_POLICY = null;
   private static int WIFI_SLEEP_POLICY_NEVER = -1;

   static {
      // bind the WIFI_SLEEP constants
      Class clazz = findSystemClass( "android.provider.Settings.Global" );

      if( clazz == null ) {
	 clazz = findSystemClass( "android.provider.Settings" );
      }

      try {
	 Field fs = clazz.getField( "WIFI_SLEEP_POLICY" );
	 WIFI_SLEEP_POLICY = (String)fs.get( clazz );
	 
	 Field fi = clazz.getField( "WIFI_SLEEP_POLICY_NEVER" );
	 WIFI_SLEEP_POLICY_NEVER = fi.getInt( clazz );
      } catch( Throwable e3 ) {
	 // Fall back API < 17
	 WIFI_SLEEP_POLICY =
	    android.provider.Settings.System.WIFI_SLEEP_POLICY;
	 WIFI_SLEEP_POLICY_NEVER =
	    android.provider.Settings.System.WIFI_SLEEP_POLICY_NEVER;
      }
   }

   // findSystem
   static Class findSystemClass( String name ) {
      try {
	 Class clazz = Class.forName( name );
	 Class[] clazzes = clazz.getClasses();
	 String global_name = clazz.getName() + "$Global";
	 String system_name = clazz.getName() + "$System";

	 for( int i = 0; i < clazzes.length; i++ ) {
	    if( clazzes[ i ].getName().equals( global_name ) ) {
	       return clazzes[ i ];
	    }
	    if( clazzes[ i ].getName().equals( system_name ) ) {
	       return clazzes[ i ];
	    }
	 }
      } catch( ClassNotFoundException c ) {
	 ;
      }

      return null;
   }

   // hop configuration class variable
   static boolean hop_log = true;
   static String hop_wizard_url;
   static boolean hop_resuscitate = false;
   
   // instance variables
   boolean killed = false;
   
   final Activity activity = this;
   HopInstaller hopinstaller;
   Intent hopintent = null;
   HopService hopservice = null;
   Hop hopconf = null;
   boolean hopconnected = false;
   int onresume_wifi_policy;

   ProgressDialog progress = null;

   // Preferences listeners are stored in a weakhash tables! To prevent
   // the Hop preference listener to be collected we store it into an
   // instance variable (thank you Android)
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

      try {
	 textbuffer.append( line );
	 textview.setText( textbuffer );
      } catch( Throwable e ) {
	 Log.e( "HopLauncher", "e=" + e );
      }
			   
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

	    try {
	       hopservice.handler = handler;
	       hopservice.queue = queue;
	       hopservice.hopdroid.activity = activity;
	       hopconnected = true;
	       hopservice.onConnect();
	    } catch( Exception e ) {
	       Log.e( "HopLauncher", "error while connecting to service: " +
		      e.toString() );
	       e.printStackTrace();
	       Log.e( "HopLauncher", "killing background hop because of error..." );
	       hopconnected = false;
	       unbindService( hopconnection );
	       HopService.emergencyExit();
	       kill( 4000 );
	    }
	 }

	 public void onServiceDisconnected( ComponentName className ) {
	    hopconnected = false;
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
			} catch( InterruptedException i ) {
			   ;
			}
			break;

		     case MSG_REBIND_HOP_SERVICE:
			write_console( "Hop reconnected...\n" );
			break;

		     case MSG_HOPDROID_ENDED:
			Log.i( "HopLauncher", "===== MSG_HOPDROID_ENDED" );
			write_console( "Hop ended...\n" );
			if( hop_resuscitate ) {
			   hop_resuscitate = false;
			   start( "" );
			}
			break;

		     case MSG_HOP_FAILED:
   		        Log.i( "HopLauncher", "===== MSG_HOP_FAILED: " + msg.obj );
			HopUiUtils.failExit( activity, "Hop", "failed", msg.obj );
			break;

		     case MSG_HOPDROID_FAILED:
			Log.i( "HopLauncher", "===== MSG_HOPDROID_FAILED: " + msg.obj );
			HopUiUtils.failExit( activity, "HopDroid", "failed", msg.obj );
			break;

		     case MSG_RUN_WIZARD:
			Log.i( "HopLauncher", "===== MSG_RUN_WIZARD" );
			cleanupProgressBar();

			Uri uri = Uri.parse( hop_wizard_url );
			Intent intent = new Intent( Intent.ACTION_VIEW, uri );
			// give time to Hop to be ready
			try {
			   Thread.sleep( 2000 );
			} catch( Exception e ) {
			   ;
			}
			startActivity( intent );
			break;

		     case MSG_INSTALL_FAILED:
			Log.e( "HopLauncher", "installation failed..." );
			HopUiUtils.failExit( activity, "HopInstaller", "failed", msg.obj );
			break;

		     case MSG_CONFIGURE_FAIL:
			Log.e( "HopLauncher", "configuration failed..." );
			HopUiUtils.failExit( activity, "HopConfigurer", "failed", msg.obj );
			break;

		     case MSG_CONFIGURE:
			progress.setMessage( "Configuring..." );
			break;

		     case MSG_START_HOP_SERVICE:
			Log.i( "HopLauncher", "===== MSG_START_HOP_SERVICE" );
			start( "" );
			break;

		     case MSG_RESTART_HOP_SERVICE:
			Log.i( "HopLauncher", "===== MSG_RESTART_HOP_SERVICE" );
			
			if( hopservice != null ) hopservice.inrestart = true;
			stop();

			break;

		     case MSG_KILL_HOP_SERVICE:
			Log.i( "HopLauncher", "===== MSG_KILL_HOP_SERVICE" );
			kill( 0 );
			break;

		     default:
		  }
	       }
	    }
	 }
      };
   
   @Override public void onCreate( Bundle bundle ) {
      super.onCreate( bundle );

      // install our view
      if( android.os.Build.VERSION.SDK_INT < 11 ) {
	 // adding an inco to SDK >= 11 prevent the action bar to
	 // be displayed
	 requestWindowFeature( Window.FEATURE_LEFT_ICON );
      }
      
      setContentView( R.layout.main );

      if( android.os.Build.VERSION.SDK_INT < 11 ) {
	 getWindow().setFeatureDrawableResource(
	    Window.FEATURE_LEFT_ICON,
	    R.drawable.logo );
      }

      // control the volume key when the console has the focus
      setVolumeControlStream( AudioManager.STREAM_MUSIC );
      
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
	 Hop.root = activity.getApplicationInfo().dataDir + "/assets";

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
			HopUiUtils.failExit( activity, "HopLauncher", " failed:", e );
		     }

		     Log.v( "HopLauncher", "installation complete" );

		     if( !HopConfigurer.configured( Hop.HOME() ) ) {
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
	    if( !HopConfigurer.configured( Hop.HOME() ) ) {
	       configure();
	    } else {
	       handler.sendEmptyMessage( MSG_START_HOP_SERVICE );
	    }
	 }
      } catch( Exception e ) {
	 HopUiUtils.failExit( activity, "HopLauncher", " failed", e );
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
	    handler.sendEmptyMessage( MSG_KILL_HOP_SERVICE );
            return true;

	 case R.id.menu_clear:
            textbuffer.delete( 0, textbuffer.length() - 1 );
	    write_console( "" );
            return true;

	 case R.id.menu_detach:
	    finish();
            return true;

	 case R.id.menu_restart:
	    handler.sendEmptyMessage( MSG_RESTART_HOP_SERVICE );
            return true;

	 case R.id.menu_info:
	    info();
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

   private void info() {
      AlertDialog.Builder builder = new AlertDialog.Builder( this );
      final View aview = getLayoutInflater().inflate( R.layout.info, null, false );
      builder.setView( aview );
      final AlertDialog dialog = builder.create();

      Button dialogButton = (Button)aview.findViewById( R.id.dialogButtonOK );

      dialogButton.setOnClickListener (new OnClickListener() {
	    @Override
	    public void onClick( View v ) {
	       activity.openOptionsMenu();
	       dialog.dismiss();
	    }
	 } );
 
      dialog.show();
   }
   
   private void configure() {
      Log.v( "HopLauncher", "configure..." );

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
   public void onResume() {
      Log.d( "HopLauncher", "onResume" );
      super.onResume();

      // get the current wifi policy
      try {
	 onresume_wifi_policy =
	    Settings.System.getInt( getContentResolver(), WIFI_SLEEP_POLICY );

	 // never switch off wifi when the hop console is on top
	 setWifiPolicy( WIFI_SLEEP_POLICY_NEVER );
      } catch( Throwable t ) {
	 onresume_wifi_policy = 0;
      }

      // notify the client
      if( hopservice != null && hopservice.hopdroid != null ) {
	 hopservice.hopdroid.pushEvent( "resume" , "" );
      }
   }

   @Override
   public void onPause() {
      Log.d( "HopLauncher", "onPause isFinishing=" + isFinishing() );
      
      // restore the wifi policy
      if( onresume_wifi_policy != 0 ) {
	 setWifiPolicy( onresume_wifi_policy );
      }
      
      // Notify the client
      if( hopservice != null && hopservice.hopdroid != null ) {
	 hopservice.hopdroid.pushEvent( "pause" , "" );
      }
      
      super.onPause();
   }

   @Override
   public void onDestroy() {
      Log.d( "HopLauncher", "onDestroy" );
      
      super.onDestroy();
   }

   @Override
   public void onStop() {
      if( hopconnected ) {
	 hopconnected = false;
	 unbindService( hopconnection );
      }
      super.onStop();
   }


   private void setWifiPolicy( int policy ) {
      Settings.System.putInt( getContentResolver(), WIFI_SLEEP_POLICY, policy );
   }
      
   private void loadPreferences() {
      Log.d( "HopLauncher", "loadPreferences" );
      try {
	 final Resources res = getResources();
	 final SharedPreferences sp =
	    PreferenceManager.getDefaultSharedPreferences( this );

	 final int initial_wifi_policy =
	    Settings.System.getInt( getContentResolver(), WIFI_SLEEP_POLICY );

	 final String defaultverbose = res.getString( R.string.hopverbose );
	 final String defaultport = res.getString( R.string.hopport );
	 final String defaultthreads = res.getString( R.string.hopthreads );
	 final String defaultdebug = res.getString( R.string.hopdebug );
	 final String defaultroot = res.getString( R.string.hoproot );
	 final boolean defaultlog = res.getString( R.string.hoplog ).equals( "true" );
	 final boolean defaultzeroconf = res.getString( R.string.hopzeroconf ).equals( "true" );
      
	 setHopPort( sp.getString( "hop_port", defaultport ) );
	 Hop.maxthreads = sp.getString( "hop_threads", defaultthreads );
	 Hop.zeroconf = sp.getBoolean( "hop_zeroconf", defaultzeroconf );
	 Hop.webdav = sp.getBoolean( "hop_webdav", false );
	 Hop.jobs = sp.getBoolean( "hop_jobs", false );
	 Hop.debug = sp.getString( "hop_debug", defaultdebug );
	 Hop.verbose = sp.getString( "hop_verbose", defaultverbose );
	 Hop.root = sp.getString( "hop_root", defaultroot );
	 hop_log = sp.getBoolean( "hop_log", defaultlog );

	 // keep wifi alive
	 if( sp.getBoolean( "hop_wifi", false ) ) {
	    setWifiPolicy( WIFI_SLEEP_POLICY_NEVER );
	 }
      
	 if( prefslistener == null ) {
	    prefslistener = new SharedPreferences.OnSharedPreferenceChangeListener() {
		  public void onSharedPreferenceChanged( SharedPreferences sp, String key ) {
		     if( key.equals( "hop_port" ) ) {
			setHopPort( sp.getString( "hop_port", defaultport ) );
			return;
		     } 
		     if( key.equals( "hop_threads" ) ) {
			Hop.maxthreads = sp.getString( "hop_threads", defaultthreads );
			return;
		     } 
		     if( key.equals( "hop_zeroconf" ) ) {
			Hop.zeroconf = sp.getBoolean( "hop_zeroconf", true );
			return;
		     }
		     if( key.equals( "hop_wifi" ) ) {
			if( sp.getBoolean( "hop_wifi", false ) ) {
			   setWifiPolicy( WIFI_SLEEP_POLICY_NEVER );
			} else {
			   setWifiPolicy( initial_wifi_policy );
			}
			return;
		     }
		     if( key.equals( "hop_webdav" ) ) {
			Hop.webdav = sp.getBoolean( "hop_webdav", false );
			return;
		     }
		     if( key.equals( "hop_jobs" ) ) {
			Hop.jobs = sp.getBoolean( "hop_jobs", false );
			return;
		     }
		     if( key.equals( "hop_log" ) ) {
			hop_log = sp.getBoolean( "hop_log", false );
			return;
		     }
		     if( key.equals( "hop_debug" ) ) {
			Hop.debug = sp.getString( "hop_debug", defaultdebug );
			return;
		     }
		     if( key.equals( "hop_verbose" ) ) {
			Hop.verbose = sp.getString( "hop_verbose", defaultverbose );
			return;
		     }
		  }
	       };
	 
	    sp.registerOnSharedPreferenceChangeListener( prefslistener );
	 }
      }
      catch( Throwable e ) {
	 Log.d( "HopLauncher", "loadPreferences exception: " + e );
	 e.printStackTrace();
      }
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
	 Log.i( "HopLauncher", ">>> kill launcher " + waitms );
	 killed = true;
	 
	 // give time to read the console messages
	 if( waitms > 0 ) {
	    try {
	       Thread.sleep( waitms );
	    } catch( Exception e ) {
	       ;
	    }
	 }

	 if( hopconf != null && hopconf.isRunning() ) {
	    hopconf.kill();
	 }
	 
	 if( hopconnected ) {
	    Log.d( "HopLauncher", "unbinding service..." );
	    hopconnected = false;
	    unbindService( hopconnection );
	 }
      
	 if( hopintent != null ) {
	    stopService( hopintent );
	 }
	 
	 Log.d( "HopLauncher", "finishing activity..." );
	 finish();
      
	 Log.i( "HopLauncher", "<<< kill launcher" );
      }
   }

   private void start( String hopargs ) {
      Log.i( "HopLauncher", "starting Hop Service" );
      
      if( hopintent == null ) {
	 HopService.hopargs = hopargs;
	 hopintent = new Intent( getApplicationContext(), HopService.class );
      }

      if( !HopService.isBackground() ) {
	 Log.d( "HopLauncher", "starting new service..." );
	 startService( hopintent );
	 
	 write_console( "Starting Hop...\n" );
      } else {
	 Log.d( "HopLauncher", "background service already running..." );
      }
			
      Log.d( "HopLauncher", "binding the service..." );
      bindService( hopintent, hopconnection, Context.BIND_AUTO_CREATE );
   }


   private void stop() {
      Log.i( "HopLauncher", ">>> stop..." );
      
      textbuffer.delete( 0, textbuffer.length() );
      write_console( "Stopping Hop...\n" );
      
      if( hopconnected ) {
	 Log.i( "HopLauncher", ">>> stop, unbindService..." );
	 hopconnected = false;
	 unbindService( hopconnection );
	 Log.i( "HopLauncher", "<<< stop, unbindService..." );
      }

      if( hopintent != null ) {
	 Log.i( "HopLauncher", ">>> stop, stopService..." );
	 stopService( hopintent );
	 hopintent = null;
	 Log.i( "HopLauncher", "<<< stop, stopService..." );
      }
      Log.i( "HopLauncher", "<<< stop" );
   }
}
