/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopLauncher.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Fri Jan  1 07:28:36 2021 (serrano)                */
/*    Copyright   :  2010-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop Launcher                                                     */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import java.util.*;
import java.util.concurrent.ArrayBlockingQueue;
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
import android.media.AudioManager;
import android.graphics.drawable.*; 
import android.graphics.*; 
import android.content.pm.*;

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
   public static final int MSG_PING = 15;
   public static final int MSG_HOP_START = 16;
   public static final int MSG_HOPDROID_START = 17;
   public static final int MSG_HOPDROID_CONNECT = 18;
   public static final int MSG_HOPDROID_FAIL = 19;
   public static final int MSG_HOP_CANNOT = 20;
   public static final int MSG_UNPACKED = 21;

   public static final int MSG_INSTALL_NEXT = 100;
   public static final int MSG_INSTALL_UNPACKED = 101;
   public static final int MSG_INSTALL_ACTIVITY_READY = 102;
   public static final int MSG_INSTALL_ACTIVITY_ERROR = 103;
   public static final int MSG_INSTALL_ACTIVITY_NOTFOUND = 104;
   public static final int MSG_INSTALL_CONFIGURED = 105;
   public static final int MSG_INSTALL_PERMISSION = 106;
   public static final int MSG_INSTALL_PERMISSION_DENIED = 107;
   public static final int MSG_INSTALL_HZ_READY = 108;
   public static final int MSG_INSTALL_HZ_ERROR = 109;
   public static final int MSG_INSTALL_HZ_NOTFOUND = 110;

   public static final int HOP_ACTIVITY_UNINIT = 0;
   public static final int HOP_ACTIVITY_WAITING = 1;

   public static final int PERM_REQUEST_ID = 1966;
   
   // Command line (am) arguments
   public static String debugCmdArg = null;
   public static String verboseCmdArg = null;
   
   // ui elements
   WebView webview;
   
   // static initialization
   static {
      HopUtils.initWifiPolicy();
   }

   // hop configuration class variable
   static boolean hop_log = true;
   static String hop_wizard_url;
   static boolean hop_resuscitate = false;
   
   // instance variables
   boolean killed = false;
   
   final Activity activity = this;
   HopInstaller hopinstaller;
   HopIntenter hopintenter = null;
   HopPermission hoppermission = null;
   HopService hopservice = null; // THIS IS WRONG hopservice is never set!
   Hop hopconf = null;
   int onresume_wifi_policy;
   Context hopctx;

   String HOPLAUNCHER = "HopLauncher";
   
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
   protected void write_console( String line ) {
      Log.v( HOPLAUNCHER, line );
   }

   final Handler handler = new Handler() {
	 @Override public void handleMessage( Message msg ) {

	    switch( msg.what ) {
	       
	       // MSG_INSTALL_XXX
	       case MSG_INSTALL_NEXT:
		  Log.i( HOPLAUNCHER, "===== MSG_INSTALL_NEXT" );
		  break;
			   
	       case MSG_INSTALL_UNPACKED:
		  onInstallUnpacked();
		  break;

	       case MSG_INSTALL_ACTIVITY_READY:
		  onInstallActivityReady();
		  break;
		  
	       case MSG_INSTALL_ACTIVITY_ERROR:
		  onInstallActivityError();
		  break;
		  
	       case MSG_INSTALL_ACTIVITY_NOTFOUND:
		  onInstallActivityNotFound();
		  break;
		  
	       case MSG_INSTALL_HZ_READY:
		  onInstallHzReady();
		  break;
		  
	       case MSG_INSTALL_HZ_ERROR:
		  onInstallHzError();
		  break;
		  
	       case MSG_INSTALL_HZ_NOTFOUND:
		  onInstallHzNotFound();
		  break;
		  
	       case MSG_INSTALL_CONFIGURED:
		  onConfigured();
		  break;

	       case MSG_INSTALL_PERMISSION:
		  onPermission();
		  break;

	       case MSG_INSTALL_PERMISSION_DENIED:
		  Log.i( HOPLAUNCHER, "==== MSG_INSTALL_PERMISSION_DENIED" );
		  HopUiUtils.failExit( activity, "Hop", "permission denied",
				       "permission denied by user" );
		  break;

	       case MSG_HOP_OUTPUT_AVAILABLE:
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
		  Log.i( HOPLAUNCHER, "===== MSG_HOPDROID_ENDED" );
		  write_console( "HopDroid ended...\n" );
		  if( hop_resuscitate ) {
		     hop_resuscitate = false;
		     start( "" );
		  } else {
		     kill( 0 );
		  }
		  break;

	       case MSG_HOP_FAIL:
		  Log.i( HOPLAUNCHER, "===== MSG_HOP_FAIL: " + msg.obj );
		  HopUiUtils.failExit( activity, "Hop", "failed", msg.obj );
		  break;

	       case MSG_HOP_CANNOT:
		  Log.i( HOPLAUNCHER, "===== MSG_HOP_CANNOT: " + msg.obj );
		  HopUiUtils.failExit( activity, "Hop", "cannot start", msg.obj );
		  break;

	       case MSG_HOP_START:
		  Log.i( HOPLAUNCHER, "===== MSG_HOP_START" );
		  webview.loadUrl( "http://localhost:" + Hop.port + HopConfig.SERVICE );
		  break;

	       case MSG_HOPDROID_START:
		  onHopDroidStart();
		  break;

	       case MSG_HOPDROID_CONNECT:
		  onHopDroidConnect();
		  break;

	       case MSG_HOPDROID_FAIL:
	       case MSG_HOPDROID_FAILED:
		  Log.i( HOPLAUNCHER, "===== MSG_HOPDROID_FAIL: " + msg.obj );
		  HopUiUtils.failExit( activity, "HopDroid", "failed", msg.obj );
		  break;

	       case MSG_INSTALL_FAIL:
		  Log.e( HOPLAUNCHER, "installation failed..." );
		  HopUiUtils.failExit( activity, "HopInstaller", "failed", msg.obj );
		  break;

	       case MSG_CONFIGURE_FAIL:
		  Log.e( HOPLAUNCHER, "configuration failed..." );
		  HopUiUtils.failExit( activity, "HopConfigurer", "failed", msg.obj );
		  break;

	       case MSG_CONFIGURE:
		  break;

	       case MSG_START_HOP_SERVICE:
		  Log.i( HOPLAUNCHER, "===== MSG_START_HOP_SERVICE" );
		  webview.loadUrl( "http://localhost:" + Hop.port + "/hop" );
		  break;

	       case MSG_RESTART_HOP_SERVICE:
		  Log.i( HOPLAUNCHER, "===== MSG_RESTART_HOP_SERVICE" );
			
		  if( hopservice != null ) hopservice.inrestart = true;
		  stop();

		  break;

	       case MSG_KILL_HOP_SERVICE:
		  Log.i( HOPLAUNCHER, "===== MSG_KILL_HOP_SERVICE" );
		  hop_resuscitate = false;
		  if( hopservice != null ) hopservice.hopdroid.kill();
		  break;

	       case MSG_PING:
		  Log.i( HOPLAUNCHER, "===== MSG_PING" );
		  break;

	       default:
		  Log.i( HOPLAUNCHER, "===== MSG_UNKNOWN: " + msg.what );
	    }
	 }
      };

   protected void splashScreen( String splash ) {
      String hopdir = getApplicationInfo().dataDir;
      String path = hopdir + "/assets/splash/" + splash;
      String url = "file://" + path;
      File f = new File( path );
      Log.d( HOPLAUNCHER, "splash url=" + url );

      if( f.exists() ) {
	 webview.loadUrl( url );
      } else {
	 Log.e( HOPLAUNCHER, "splash does not exist: " + f.toString() );
      }
   }

   // onLaunch (overriden by HopHzLauncher)
   protected void onLaunch() {
      String hopapk = getApplicationInfo().sourceDir;
      String hopdir = getApplicationInfo().dataDir + "/assets";

      HopInstaller installer = new HopInstaller( activity, handler, hopapk, hopdir );

      installer.exec( hopctx, null );
   }
   
   // onCreate
   @Override public void onCreate( Bundle bundle ) {
      super.onCreate( bundle );

      HOPLAUNCHER = HopUtils.shortClassName( this.getClass() );
      Log.i( HOPLAUNCHER, "onCreate" );

      String hopapk = getApplicationInfo().sourceDir;
      String hopdir = getApplicationInfo().dataDir + "/assets";

      int debugcmd = getIntent().getIntExtra( "-g", -1 );
      int verbosecmd = getIntent().getIntExtra( "-v", -1 );
      
      Log.d( HOPLAUNCHER, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" );
      Log.d( HOPLAUNCHER, hopapk + " ("
	     + java.time.LocalDate.now() + " "
	     + java.time.LocalTime.now()
	     + ")" + " debug=" + debugcmd + " verbose=" + verbosecmd );
      Log.d( HOPLAUNCHER, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" );

      // adjust Hop parameter
      if( debugcmd >= 0 ) debugCmdArg = "-g" + debugcmd;
      if( verbosecmd >= 0 ) verboseCmdArg = "-v" + verbosecmd;
			     
      hopctx = getApplicationContext();
      HopConfig.init( hopctx );
      
      webview = HopUiUtils.initUI( this );
      
      Hop.setHopActivityParams( activity );

      onLaunch();
   }

   @Override public boolean onCreateOptionsMenu( Menu menu ) {
      Log.d( HOPLAUNCHER, "onCreateOptionsMenu" );
      MenuInflater inflater = getMenuInflater();
      inflater.inflate( R.menu.hop_menu, menu );
      return true;
   }

   @Override public boolean onOptionsItemSelected( MenuItem item ) {
      // Handle item selection
      switch( item.getItemId() ) {
	 case R.id.menu_settings:
	    Log.d( HOPLAUNCHER, "starting preference activity" );
	    Intent intent = new Intent( getBaseContext(), HopSettings.class );
	    startActivity( intent );
	    return true;

	 case R.id.menu_quit:
	    handler.sendEmptyMessage( MSG_KILL_HOP_SERVICE );
            return true;

	 case R.id.menu_reload:
	    webview.loadUrl( "http://localhost:" + Hop.port + "/hop/" + HopConfig.SERVICE );
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
      Log.d( HOPLAUNCHER, "set port=" + port );
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
   
   @Override public void onStart() {
      super.onStart();

      Log.d( HOPLAUNCHER, "onStart" );
   }

   @Override public void onStop() {
      super.onStop();
      
      // mark the activity paused
      if( hopservice != null ) hopservice.hopdroid.activity = null;
      
      Log.d( HOPLAUNCHER, "onStop" );
   }
   
   @Override public void onDestroy() {
      Log.d( HOPLAUNCHER, "onDestroy isFinishing=" + isFinishing() );

      abort();
      super.onDestroy();
      finishAndRemoveTask();
   }

   @Override public void onResume() {
      Log.d( HOPLAUNCHER, "onResume service="
	     + (hopservice == null ? "null" : hopservice.toString())
	     + " hopdroid="
	     + ((hopservice == null || hopservice.hopdroid == null) ? "null" : hopservice.hopdroid.toString()) );
      super.onResume();

      // get the current wifi policy
      try {
	 onresume_wifi_policy =
	    Settings.System.getInt( getContentResolver(), HopUtils.WIFI_SLEEP_POLICY );

	 // never switch off wifi when the hop console is on top
	 setWifiPolicy( HopUtils.WIFI_SLEEP_POLICY_NEVER );
      } catch( Throwable t ) {
	 onresume_wifi_policy = 0;
      }

      // mark the activity alive
      if( hopservice != null && hopservice.hopdroid != null ) {
	 hopservice.hopdroid.activity = this;
      }
      
      // notify the client
      if( hopservice != null && hopservice.hopdroid != null ) {
	 hopservice.hopdroid.pushEvent( "resume" , "" );
      }
   }

   @Override public void onPause() {
      Log.d( HOPLAUNCHER, "onPause isFinishing=" + isFinishing() );
      
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

   @Override public void onConfigurationChanged( Configuration newConfig ) {
      Log.d( HOPLAUNCHER, "onConfigurationChanged" );

      if( HopService.hopdroid != null ) {
	 HopService.hopdroid.pushEvent( "configurationchanged", HopConfiguration.toString( newConfig ) );
      }
   }
   
   private void setWifiPolicy( int policy ) {
      Settings.System.putInt( getContentResolver(), HopUtils.WIFI_SLEEP_POLICY, policy );
   }
      
/*    private void loadPreferences() {                                 */
/*       Log.d( HOPLAUNCHER, "loadPreferences" );                    */
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
/* 	 Log.d( HOPLAUNCHER, "loadPreferences exception: " + e );    */
/* 	 e.printStackTrace();                                          */
/*       }                                                             */
/*    }                                                                */
   
   @Override
   public void startActivityForResult( Intent intent, int requestCode ) {
      super.startActivityForResult( intent, requestCode );
   }
   
   protected void onActivityResult( int reqcode, int rescode, Intent intent ) {
      Log.v( HOPLAUNCHER, "onActivityResult reqcode=" + reqcode + " rescode=" + rescode + " intent=" + intent + " activity=" + this );
      HopPlugin.onActivityResult( reqcode, rescode, intent );
      super.onActivityResult( reqcode, rescode, intent );
   }

   private synchronized void kill( int waitms ) {
      if( !killed ) {
	 killed = true;
	 
	 // give time to read the console messages
	 if( waitms > 0 ) {
	    try {
	       Thread.sleep( waitms );
	    } catch( Exception e ) {
	       ;
	    }
	 }

	 abort();
	 
	 Log.d( HOPLAUNCHER, "finishing activity..." );
	 //finish();
	 this.finishAffinity();
	 Log.i( HOPLAUNCHER, "kill done." );
      }
   }


   @Override
   public void onRequestPermissionsResult( int requestCode,
					   String[] permissions,
					   int[] grantResults ) {
      Log.d( HOPLAUNCHER, "onRequestPermissionsResult: "
	     + requestCode
	     + " perms=" + permissions.length
	     + " grants=" + grantResults.length );

      switch( requestCode ) {
	 case PERM_REQUEST_ID:
            // If request is cancelled, the result arrays are empty.
	    if( grantResults.length == permissions.length ) {
	       int i;
	      
	       for( i = 0; i < grantResults.length; i++ ) {
		  if( grantResults[ i ] == PackageManager.PERMISSION_DENIED ) {
		     Log.d( HOPLAUNCHER, "permission denied: " + permissions[ i ] );
		     
		     handler.sendEmptyMessage( HopLauncher.MSG_INSTALL_PERMISSION_DENIED );
		     break;
		  }
	       }
		    
	       handler.sendEmptyMessage( HopLauncher.MSG_INSTALL_PERMISSION );
	    }  else {
	       handler.sendEmptyMessage( HopLauncher.MSG_INSTALL_PERMISSION_DENIED );
            }
            return;
      }
   }
   
   private void start( String hopargs ) {
/*       Log.i( HOPLAUNCHER, "starting Hop Service" );               */
/*                                                                     */
/*       if( hopintent == null ) {                                     */
/* 	 HopService.hopargs = hopargs;                                 */
/* 	 hopintent = new Intent( getApplicationContext(), HopService.class ); */
/*       }                                                             */
/*                                                                     */
/*       if( !HopService.isBackground() ) {                            */
/* 	 Log.d( HOPLAUNCHER, "starting new service..." );            */
/* 	 startService( hopintent );                                    */
/* 	                                                               */
/* 	 write_console( "Starting Hop...\n" );                         */
/*       } else {                                                      */
/* 	 Log.d( HOPLAUNCHER, "background service already running..." ); */
/*       }                                                             */
/* 			                                               */
/*       Log.d( HOPLAUNCHER, "binding the service..." );             */
   }


   private void abort() {
      Log.i( HOPLAUNCHER, "abort..." );
      
      if( hopintenter != null ) {
	 hopintenter.abort();
	 stopService( hopintenter.hopintent );
	 hopintenter = null;
      }
   }
      
   private void stop() {
   }

   // install handlers
   protected void onInstallUnpacked() {
      Log.d( HOPLAUNCHER, "===== onInstallUnpacked" );
      
      splashScreen( "splash.html" );
      new HopConfigurer( activity, handler ).exec( hopctx, null );
   }

   protected void onInstallActivityReady() {
      Log.d( HOPLAUNCHER, "===== onInstallActivityReady" );
   }

   protected void onInstallActivityError() {
      Log.d( HOPLAUNCHER, "===== onInstallActivityError" );
   }
   
   protected void onInstallActivityNotFound() {
      Log.d( HOPLAUNCHER, "===== onInstallHopNotFound" );
   }
   
   protected void onInstallHzReady() {
      Log.d( HOPLAUNCHER, "===== onInstallHzReady" );
   }

   protected void onInstallHzError() {
      Log.d( HOPLAUNCHER, "===== onInstallHzError" );
   }
   
   protected void onInstallHzNotFound() {
      Log.d( HOPLAUNCHER, "===== onInstallHopNotFound" );
   }
   
   protected void onConfigured() {
      Log.d( HOPLAUNCHER, "===== onConfigured" );
      
      hoppermission = new HopPermission( activity, handler );
      hoppermission.exec( hopctx, null );
   }
   
   protected void onPermission() {
      Log.d( HOPLAUNCHER, "===== onPermission" );
      
      hopintenter = new HopIntenter( activity, handler, queue );
      hopintenter.exec( hopctx, HopService.class );
   }
   
   protected void onHopDroidStart() {
      Log.i( HOPLAUNCHER, "===== onHopDroidStart" );
   }
   
   protected void onHopDroidConnect() {
      Log.i( HOPLAUNCHER, "===== onHopDroidConnect" );
   }
}
