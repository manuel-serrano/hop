/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopLauncher.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Sun May 17 12:26:05 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop Launcher                                                     */
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
import android.webkit.*;
import android.net.*;
import android.text.*;
import android.provider.*;
import android.media.AudioManager;
import android.graphics.drawable.*; 
import android.graphics.*; 

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
   public static final int MSG_INSTALL_FAIL = 4;
   public static final int MSG_CONFIGURE_FAIL = 5;
   public static final int MSG_HOP_FAIL = 6;
   public static final int MSG_HOP_FAILED = 6;
   public static final int MSG_CONFIGURE = 7;
   public static final int MSG_HOPDROID_FAILED = 8;
   public static final int MSG_START_HOP_SERVICE = 9;
   public static final int MSG_RESTART_HOP_SERVICE = 10;
   public static final int MSG_KILL_HOP_SERVICE = 11;
   public static final int MSG_REBIND_HOP_SERVICE = 12;
   public static final int MSG_STATE_INSTALL = 13;
   public static final int MSG_STATE_CONFIGURE = 14;
   public static final int MSG_STATE_NEXT = 15;
   public static final int MSG_PING = 16;
   public static final int MSG_HOP_START = 17;
   public static final int MSG_HOPDROID_FAIL = 18;
   public static final int MSG_HOP_CANNOT = 19;
   public static final int MSG_UNPACKED = 20;

   private static String WIFI_SLEEP_POLICY = null;
   private static int WIFI_SLEEP_POLICY_NEVER = -1;

   // ui elements
   WebView webview;
   
   // staging
   private HopStage currentStage;
   private HopInstaller installer;
   private HopConfigurer configurer;
   private HopIntenter intenter;
   
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

   // Preferences listeners are stored in a weakhash tables! To prevent
   // the Hop preference listener to be collected we store it into an
   // instance variable (thank you Android)
   SharedPreferences.OnSharedPreferenceChangeListener prefslistener = null;
   
   int maxlines = 0;
   StringBuffer textbuffer = new StringBuffer( 2048 );
   CheckBox checkbox, checkbox2, checkbox3, checkbox4;
   final ArrayBlockingQueue<String> queue =
      new ArrayBlockingQueue<String>( 1024 );
   // MS WARNING, there is a potential deadlock when
   // starting the application. If the queue is too
   // small the logger thread (see Hop.java) might be
   // blocked while the main thread is waiting for
   // Hop to start...
   
   // write a line on Hop console
   private void write_console( String line ) {
      Log.v( "HopLauncher", line );
   }

   final Handler handler = new Handler() {
	 @Override public void handleMessage( Message msg ) {

	    switch( msg.what ) {
	       case MSG_UNPACKED:
		  splashScreen();
		  break;
		  
	       case MSG_STATE_NEXT:
		  Log.i( "HopLauncher", "===== MSG_STATE_NEXT" );
		  execStage();
		  break;
			   
	       case MSG_HOP_OUTPUT_AVAILABLE:
		  // Log.i( "HopLauncher", "===== MSG_HOP_OUTOUT_AVAILABLE" );
		  try {
		     Log.v( "HopConsole", queue.take() );
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
		  } else {
		     kill( 0 );
		  }
		  break;

	       case MSG_HOP_FAIL:
		  Log.i( "HopLauncher", "===== MSG_HOP_FAIL: " + msg.obj );
		  HopUiUtils.failExit( activity, "Hop", "failed", msg.obj );
		  break;

	       case MSG_HOP_CANNOT:
		  Log.i( "HopLauncher", "===== MSG_HOP_CANNOT: " + msg.obj );
		  HopUiUtils.failExit( activity, "Hop", "cannot start", msg.obj );
		  break;

	       case MSG_HOP_START:
		  Log.i( "HopLauncher", "===== MSG_HOP_START" );
		  webview.loadUrl( "http://localhost:" + Hop.port + "/hop/" + HopConfig.APP );
		  break;

	       case MSG_HOPDROID_FAIL:
	       case MSG_HOPDROID_FAILED:
		  Log.i( "HopLauncher", "===== MSG_HOPDROID_FAIL: " + msg.obj );
		  HopUiUtils.failExit( activity, "HopDroid", "failed", msg.obj );
		  break;

	       case MSG_INSTALL_FAIL:
		  Log.e( "HopLauncher", "installation failed..." );
		  HopUiUtils.failExit( activity, "HopInstaller", "failed", msg.obj );
		  break;

	       case MSG_CONFIGURE_FAIL:
		  Log.e( "HopLauncher", "configuration failed..." );
		  HopUiUtils.failExit( activity, "HopConfigurer", "failed", msg.obj );
		  break;

	       case MSG_CONFIGURE:
		  break;

	       case MSG_START_HOP_SERVICE:
		  Log.i( "HopLauncher", "===== MSG_START_HOP_SERVICE" );
		  webview.loadUrl( "http://localhost:" + Hop.port + "/hop" );
		  break;

	       case MSG_RESTART_HOP_SERVICE:
		  Log.i( "HopLauncher", "===== MSG_RESTART_HOP_SERVICE" );
			
		  if( hopservice != null ) hopservice.inrestart = true;
		  stop();

		  break;

	       case MSG_KILL_HOP_SERVICE:
		  Log.i( "HopLauncher", "===== MSG_KILL_HOP_SERVICE" );
		  hop_resuscitate = false;
		  hopservice.hopdroid.kill();
		  break;

	       case MSG_PING:
		  Log.i( "HopLauncher", "===== MSG_PING" );
		  break;

	       default:
		  Log.i( "HopLauncher", "===== MSG_UNKNOWN: " + msg.what );
	    }
	 }
      };

   private void initUI() {
      Log.d( "HopLauncher", "initUI" );

      final String BOOT_PAGE = "<!DOCTYPE html><html><head><meta http-equiv='Content-Type' content='text/html; charset=UTF-8'><meta name='viewport' content='width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=no'></head><body style='background-color: #222; color: #eee'>" + getApplicationContext().getString( R.string.hopapp ) + " booting...</body></html>";
      
      // remove title bar
      this.requestWindowFeature( Window.FEATURE_NO_TITLE );

      // action bar color
      //this.requestWindowFeature( Window.FEATURE_ACTION_BAR );
      //ActionBar bar = getActionBar();
      //bar.setBackgroundDrawable(new ColorDrawable(Color.parseColor("#3e3e3e")));
      

      setContentView( R.layout.main );

      // grab the view
      webview = (WebView)findViewById( R.id.webview );
      WebSettings webSettings = webview.getSettings();
      webSettings.setJavaScriptEnabled( true );
      webSettings.setBuiltInZoomControls( true );
      webSettings.setAppCacheEnabled( false );

      webview.requestFocusFromTouch();

      webview.setWebViewClient( new WebViewClient() );
      webview.setWebChromeClient( new WebChromeClient() );

      // start with the splash message
      Log.d( "HopLauncher", "loading boot page" );
      webview.loadData( BOOT_PAGE, "text/html; charset=UTF-8", null );

      // control the volume key when the console has the focus
      setVolumeControlStream( AudioManager.STREAM_MUSIC );
   }

   private void splashScreen() {
      String hopdir = activity.getApplicationInfo().dataDir + "/assets";
      String url = "file://" + hopdir + "/etc/hop/" + HopConfig.HOPRELEASE + "/splash/splash.html";
      File f = new File( hopdir + "/etc/hop/" + HopConfig.HOPRELEASE + "/splash/splash.html" );
      Log.d( "HopLauncher", "splash url=" + url );

      if( f.exists() ) {
	 webview.loadUrl( url );
      } else {
	 Log.e( "HopLauncher", "splash does not exist: " + f.toString() );
      }
   }
      
   @Override public void onCreate( Bundle bundle ) {
      super.onCreate( bundle );

      Log.d( "HopLauncher", "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" );
      Log.d( "HopLauncher", "Hopdac (" + System.currentTimeMillis()/1000 + ")" );
      Log.d( "HopLauncher", "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" );
      Log.d( "HopLauncher", "" );
      Log.d( "HopLauncher", "onCreate" );
      String hopapk = activity.getApplicationInfo().sourceDir;
      String hopdir = activity.getApplicationInfo().dataDir + "/assets";

      HopConfig.init( getApplicationContext() );

      Log.d( "HopLauncher", "onCreate apk=" + hopapk + " dir=" + hopdir );

      initUI();
      installer = new HopInstaller( activity, handler, hopapk, hopdir );
      configurer = new HopConfigurer( handler, Hop.HOME() );
      intenter = new HopIntenter( activity, handler, queue );

      Hop.setHopActivityParams( activity );

      currentStage = null;
      execStage();
   }

   @Override public void onStart() {
      super.onStart();

      Log.d( "HopLauncher", "onStart" );
   }

   @Override public void onStop() {
      super.onStop();
      
      Log.d( "HopLauncher", "onStop" );
   }

   
   @Override public void onDestroy() {
      super.onDestroy();
      
      Log.d( "HopLauncher", "onDestroy currentState=" + currentStage );
      if( currentStage != null ) {
	 currentStage.abort();
      }
      
      if( hopconnected ) {
	 hopconnected = false;
	 //unbindService( hopconnection );
      }
   }

   void execStage() {
      boolean exec = true;
      if( currentStage == null ) {
	 currentStage = installer;
      } else if( currentStage == installer ) {
	 currentStage = configurer;
      } else if( currentStage == configurer ) {
	 currentStage = intenter;
      } else if( currentStage == intenter ) {
	 exec = false;
      }
      if( exec ) {
	 currentStage.exec( getApplicationContext() );
      }
   }
   
   @Override public boolean onCreateOptionsMenu( Menu menu ) {
      Log.d( "HopLauncher", "onCreateOptionsMenu" );
      MenuInflater inflater = getMenuInflater();
      inflater.inflate( R.menu.hop_menu, menu );
      return true;
   }

   @Override public boolean onOptionsItemSelected( MenuItem item ) {
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

	 case R.id.menu_reload:
	    webview.loadUrl( "http://localhost:" + Hop.port + "/hop/" + HopConfig.APP );
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
   
   @Override public void onResume() {
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

   @Override public void onPause() {
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

   private void setWifiPolicy( int policy ) {
      Settings.System.putInt( getContentResolver(), WIFI_SLEEP_POLICY, policy );
   }
      
/*    private void loadPreferences() {                                 */
/*       Log.d( "HopLauncher", "loadPreferences" );                    */
/*       try {                                                         */
/* 	 final Resources res = getResources();                         */
/* 	 final SharedPreferences sp =                                  */
/* 	    PreferenceManager.getDefaultSharedPreferences( this );     */
/*                                                                     */
/* 	 final int initial_wifi_policy =                               */
/* 	    Settings.System.getInt( getContentResolver(), WIFI_SLEEP_POLICY ); */
/*                                                                     */
/* 	 final String defaultverbose = res.getString( R.string.hopverbose ); */
/* 	 final String defaultport = res.getString( R.string.hopport ); */
/* 	 final String defaultthreads = res.getString( R.string.hopthreads ); */
/* 	 final String defaultdebug = res.getString( R.string.hopdebug ); */
/* 	 final String defaultroot = res.getString( R.string.hoproot ); */
/* 	 final boolean defaultlog = res.getString( R.string.hoplog ).equals( "true" ); */
/* 	 final boolean defaultzeroconf = res.getString( R.string.hopzeroconf ).equals( "true" ); */
/*                                                                     */
/* 	 setHopPort( sp.getString( "hop_port", defaultport ) );        */
/* 	 Hop.maxthreads = sp.getString( "hop_threads", defaultthreads ); */
/* 	 Hop.zeroconf = sp.getBoolean( "hop_zeroconf", defaultzeroconf ); */
/* 	 Hop.webdav = sp.getBoolean( "hop_webdav", false );            */
/* 	 Hop.jobs = sp.getBoolean( "hop_jobs", false );                */
/* 	 Hop.debug = sp.getString( "hop_debug", defaultdebug );        */
/* 	 Hop.verbose = sp.getString( "hop_verbose", defaultverbose );  */
/* 	 Hop.root = sp.getString( "hop_root", defaultroot );           */
/* 	 hop_log = sp.getBoolean( "hop_log", defaultlog );             */
/*                                                                     */
/* 	 // keep wifi alive                                            */
/* 	 if( sp.getBoolean( "hop_wifi", false ) ) {                    */
/* 	    setWifiPolicy( WIFI_SLEEP_POLICY_NEVER );                  */
/* 	 }                                                             */
/*                                                                     */
/* 	 if( prefslistener == null ) {                                 */
/* 	    prefslistener = new SharedPreferences.OnSharedPreferenceChangeListener() { */
/* 		  public void onSharedPreferenceChanged( SharedPreferences sp, String key ) { */
/* 		     if( key.equals( "hop_port" ) ) {                  */
/* 			setHopPort( sp.getString( "hop_port", defaultport ) ); */
/* 			return;                                        */
/* 		     }                                                 */
/* 		     if( key.equals( "hop_threads" ) ) {               */
/* 			Hop.maxthreads = sp.getString( "hop_threads", defaultthreads ); */
/* 			return;                                        */
/* 		     }                                                 */
/* 		     if( key.equals( "hop_zeroconf" ) ) {              */
/* 			Hop.zeroconf = sp.getBoolean( "hop_zeroconf", true ); */
/* 			return;                                        */
/* 		     }                                                 */
/* 		     if( key.equals( "hop_wifi" ) ) {                  */
/* 			if( sp.getBoolean( "hop_wifi", false ) ) {     */
/* 			   setWifiPolicy( WIFI_SLEEP_POLICY_NEVER );   */
/* 			} else {                                       */
/* 			   setWifiPolicy( initial_wifi_policy );       */
/* 			}                                              */
/* 			return;                                        */
/* 		     }                                                 */
/* 		     if( key.equals( "hop_webdav" ) ) {                */
/* 			Hop.webdav = sp.getBoolean( "hop_webdav", false ); */
/* 			return;                                        */
/* 		     }                                                 */
/* 		     if( key.equals( "hop_jobs" ) ) {                  */
/* 			Hop.jobs = sp.getBoolean( "hop_jobs", false ); */
/* 			return;                                        */
/* 		     }                                                 */
/* 		     if( key.equals( "hop_log" ) ) {                   */
/* 			hop_log = sp.getBoolean( "hop_log", false );   */
/* 			return;                                        */
/* 		     }                                                 */
/* 		     if( key.equals( "hop_debug" ) ) {                 */
/* 			Hop.debug = sp.getString( "hop_debug", defaultdebug ); */
/* 			return;                                        */
/* 		     }                                                 */
/* 		     if( key.equals( "hop_verbose" ) ) {               */
/* 			Hop.verbose = sp.getString( "hop_verbose", defaultverbose ); */
/* 			return;                                        */
/* 		     }                                                 */
/* 		  }                                                    */
/* 	       };                                                      */
/* 	                                                               */
/* 	    sp.registerOnSharedPreferenceChangeListener( prefslistener ); */
/* 	 }                                                             */
/*       }                                                             */
/*       catch( Throwable e ) {                                        */
/* 	 Log.d( "HopLauncher", "loadPreferences exception: " + e );    */
/* 	 e.printStackTrace();                                          */
/*       }                                                             */
/*    }                                                                */
   
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

	 if( hopconnected ) {
	    Log.d( "HopLauncher", "unbinding service..." );
	    hopconnected = false;
	    // unbindService( hopconnection );
	 }
      
	 if( hopintent != null ) {
	    stopService( hopintent );
	 }
	 
	 Log.d( "HopLauncher", "finishing activity..." );
	 //finish();
	 this.finishAffinity();
	 Log.i( "HopLauncher", "<<< kill launcher" );

	 System.exit( 0 );
      }
   }

   private void start( String hopargs ) {
/*       Log.i( "HopLauncher", "starting Hop Service" );               */
/*                                                                     */
/*       if( hopintent == null ) {                                     */
/* 	 HopService.hopargs = hopargs;                                 */
/* 	 hopintent = new Intent( getApplicationContext(), HopService.class ); */
/*       }                                                             */
/*                                                                     */
/*       if( !HopService.isBackground() ) {                            */
/* 	 Log.d( "HopLauncher", "starting new service..." );            */
/* 	 startService( hopintent );                                    */
/* 	                                                               */
/* 	 write_console( "Starting Hop...\n" );                         */
/*       } else {                                                      */
/* 	 Log.d( "HopLauncher", "background service already running..." ); */
/*       }                                                             */
/* 			                                               */
/*       Log.d( "HopLauncher", "binding the service..." );             */
/*       bindService( hopintent, hopconnection, Context.BIND_AUTO_CREATE ); */
   }


   private void stop() {
/*       Log.i( "HopLauncher", ">>> stop..." );                        */
/*                                                                     */
/*       textbuffer.delete( 0, textbuffer.length() );                  */
/*       write_console( "Stopping Hop...\n" );                         */
/*                                                                     */
/*       if( hopconnected ) {                                          */
/* 	 Log.i( "HopLauncher", ">>> stop, unbindService..." );         */
/* 	 hopconnected = false;                                         */
/* 	 unbindService( hopconnection );                               */
/* 	 Log.i( "HopLauncher", "<<< stop, unbindService..." );         */
/*       }                                                             */
/*                                                                     */
/*       if( hopintent != null ) {                                     */
/* 	 Log.i( "HopLauncher", ">>> stop, stopService..." );           */
/* 	 stopService( hopintent );                                     */
/* 	 hopintent = null;                                             */
/* 	 Log.i( "HopLauncher", "<<< stop, stopService..." );           */
/*       }                                                             */
/*       Log.i( "HopLauncher", "<<< stop" );                           */
   }
}
