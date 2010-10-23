/*=====================================================================*/
/*    .../hop/2.2.x/arch/android/src/fr/inria/hop/HopLauncher.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Sat Oct 23 07:49:08 2010 (serrano)                */
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
   public static final int MSG_HOPANDROID_FAIL = 9;

   // instance variables
   boolean infinish = false;
   
   final Activity activity = this;
   HopInstaller hopinstaller;
   ProgressDialog progress = null;

   int maxlines = 0;
   StringBuffer textbuffer = new StringBuffer( 2048 );
   CheckBox checkbox;
   TextView textview;
   ScrollView scrollview;
   final ArrayBlockingQueue<String> queue =
      new ArrayBlockingQueue<String>( 10 );
   
   final Handler handler = new Handler() {
	 @Override public void handleMessage( Message msg ) {
	    
	    if( msg.what != MSG_OUTPUT_AVAILABLE )
	       Log.i( "Hop", "message=" + msg.what );

	    if( !infinish ) {
	       switch( msg.what ) {
		  case MSG_OUTPUT_AVAILABLE:
		     try {
			write_console( queue.take() );
		     } catch( InterruptedException _ ) {
			;
		     }
		     break;

		  case MSG_PROC_END:
		     infinish = true;
		     activity.finish();
		     break;

		  case MSG_RUN_WIZARD:
		     progress.dismiss();
		     progress = null;

		     Uri uri = Uri.parse( "http://localhost:" + hop.port + "/hop/wizard" );
		     Intent intent = new Intent(Intent.ACTION_VIEW, uri);
		     startActivity( intent );
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

		  case MSG_HOPANDROID_FAIL:
		     HopUiUtils.fail( hop.activity, "HopAndroid", "failed", (Exception)msg.obj );
		     break;

		  default:
	       }
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
	       Log.i( "HopLauncher", "restarting" );

	       write_console( "\n\nRestarting session...\n" );
	       hop.restart();
	    }
	 } );

      // setup the exit button
      Button buttone = (Button)findViewById( R.id.exit );
      buttone.setOnClickListener( new OnClickListener() {
	    public void onClick( View v ) {
	       Log.i( "HopLauncher", "exit" );

	       write_console( "Exiting...\n" );
	       infinish = true;
	       
	       hop.kill();
	       activity.finish();
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
      
      // grab the text for the output log
      textview = (TextView)activity.findViewById( R.id.textview );
      scrollview = (ScrollView)activity.findViewById( R.id.scrollview );
      
      //maxlines = textview.getResources().getInteger( R.styleable.TextView_maxLines );
      maxlines = 500;

      Log.i( "HopLauncher", textview + " maxlines=" + maxlines );

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
		     } catch( Exception e ) {
			HopUiUtils.fail( activity, "HopLauncher", " failed:", e );
		     }

		     Log.v( "HopLauncher", "installation complete" );

		     TextView port = (TextView)activity.findViewById( R.id.port );
		     hop.port = port.getText().toString();
		     Log.v( "HopLauncher", "starting hop (install)" );
		     hop.start();
		     configure();
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
	    Log.v( "HopLauncher", "starting hop (install)" );
	    hop.start();
	    configure();
	 }

	 hopandroid.start();
      } catch( Exception e ) {
	 HopUiUtils.fail( activity, "HopLauncher", " failed", e );
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
      }
   }
   
   private void configure() {
      // configure Hop if needed
      if( !HopConfigurer.configured( hop ) ) {
	 HopConfigurer hopconfigurer =
	    new HopConfigurer( hop, handler,
			       "http://localhost:" + hop.port + "/hop/wizard" );

	 Log.i( "HopLauncher", "sending configure..." );
	 handler.sendEmptyMessage( MSG_CONFIGURE );
	 hopconfigurer.start();
      } else {
	 if( progress != null ) {
	    progress.dismiss();
	    progress = null;
	 }
      }
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
      Log.i( "HopLauncher", "onDestroy" );
      hop.kill();
      super.onDestroy();
   }

   protected void onActivityResult( int reqcode, int rescode, Intent intent ) {
      hopandroid.onActivityResult( reqcode, rescode, intent );
      super.onActivityResult( reqcode, rescode, intent );
   }
}
