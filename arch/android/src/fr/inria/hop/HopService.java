/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopService.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 25 17:24:05 2012                          */
/*    Last change :  Sun Dec 27 16:34:51 2020 (serrano)                */
/*    Copyright   :  2012-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Android service for the Hop process                              */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.os.*;
import android.util.Log;
import android.app.*;
import android.content.Intent;
import java.util.concurrent.ArrayBlockingQueue;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopService extends Service {
   private NotificationManager mNM;
   private int NOTIFICATION = R.string.hopservicestarted;
   private final int HOP_ID = 17051966;

   // instance variables
   public static Hop hop = null;
   public static HopDroid hopdroid = null;
   protected Boolean inrestart = false;
   Handler handler;
   
   String HOPSERVICE = "HopService";
   
   // communication with the launcher
   ArrayBlockingQueue<String> queue;
   
   public class HopBinder extends Binder {
      HopService getService() {
	 return HopService.this;
      }
   }
   
   private final IBinder hopbinder = new HopBinder();
   
   @Override
   public void onCreate() {
      Log.i( HOPSERVICE, "onCreate..." );
      // status bar notification
      mNM = (NotificationManager)getSystemService( NOTIFICATION_SERVICE );

      Notification notification = statusNotification( false );

      startForeground( HOP_ID, notification );
   }

   @Override
   public void onDestroy() {
      Log.i( HOPSERVICE, "onDestroy..." );

      kill();
      
      // status bar update
      if( mNM != null ) {
	 mNM.cancel( NOTIFICATION );
      }
      
      super.onDestroy();

      // we are in the middle of a restart, now that that service is
      // destroy, it can notify the launcher to start a new HopService instance
      if( inrestart ) {
	 inrestart = false;
	 Log.i( HOPSERVICE, "sending restart message" );
	 handler.sendEmptyMessage( HopLauncher.MSG_START_HOP_SERVICE );
      }
   }

   @Override
   public IBinder onBind( Intent intent ) {
      Log.d( HOPSERVICE, "onBind: this=" + this + " hopbinder=" + hopbinder );
      
      return hopbinder;
   }

   @Override
   public void onRebind( Intent intent ) {
      Log.d( HOPSERVICE, "onRebind: " + this );
      
      super.onRebind( intent );
      handler.sendEmptyMessage( HopLauncher.MSG_REBIND_HOP_SERVICE );
   }

   @Override
   public boolean onUnbind( Intent intent ) {
      Log.i( HOPSERVICE, "onUnbind: " + this );

      kill();
      // true is returned to get onRebind invoked
      return true;
   }

   public void onConnect() {
      // invoked by HopLauncher when the connection is established.
      // this is used to complete the plugin initialization
      hopdroid.onConnect();
   }
   
   public synchronized void kill() {
      Log.i( HOPSERVICE, "kill..." );

      if( hop != null ) {
	 hop.kill();
	 hop = null;
      }
      
      if( hopdroid != null ) {
	 hopdroid.kill();
	 hopdroid = null;
      }
   }

   @Override
    public int onStartCommand( Intent intent, int flags, int startid ) {
      Log.d( HOPSERVICE, "onStartCommand " + this + "..." + " flags=" + flags + " startid=" + startid );

      HOPSERVICE = HopUtils.shortClassName( this.getClass() );
      
      // create hopdroid
      hopdroid = new HopDroid( HopService.this, HopLauncher.class );

      if( hopdroid.state == HopDroid.HOPDROID_STATE_INIT ) {
	 // create hop 
	 hop = new Hop( HopService.this );
      
	 // starting hopdroid
	 hopdroid.start();

	 // starting hop
	 hop.start();

	 // sticky service
	 return START_NOT_STICKY;
      } else {
	 Log.d( HOPSERVICE, "stopSelf" );
	 stopSelf();

	 return 0;
      }
   }

   public void reboot() {
      Log.i( HOPSERVICE, "reboot..." );
      
      // reboot hopdroid
      hopdroid.reboot();
      
      // restar hop
      hop.reboot();
   }

   public static boolean isBackground() {
      return hop != null && hop.isRunning( 0 );
      //return HopDroid.isBackground();
   }

   private Notification statusNotification( boolean notify ) {
      // Set the icon, scrolling text and timestamp
      CharSequence text = getText( R.string.hopservicestarted );
      PendingIntent contentIntent =
	 PendingIntent.getActivity(
	    this, 0, new Intent( this, HopService.class ), 0 );
/*       Notification notification =                                   */
/* 	 new Notification( R.drawable.hopicon, text, System.currentTimeMillis()); */
/* {*       NotificationCompat.Builder builder =                          *} */
/* {* 	 new NotificationCompat.Builder( this ) ;                      *} */
/* {*       Notification notification = builder.setContentIntent( contentIntent ) *} */
/* {* 	 .setSmallIcon( R.drawable.hopicon )                           *} */
/* {* 	 .setTicker( text )                                            *} */
/* {* 	 .setWhen( System.currentTimeMillis() )                        *} */
/* {* 	 .setAutoCancel( true ).setContentTitle( HopConfig.HOPRELEASE ) *} */
/* {* 	 .setContentText( text ).build();                              *} */
/*       notification.flags = Notification.FLAG_NO_CLEAR;              */
/*                                                                     */
/*       Log.d( HOPSERVICE, "statusNotification" );                  */
/*       notification.setLatestEventInfo(                              */
/* 	 this, HopConfig.HOPRELEASE, text, contentIntent );            */

      Notification.Builder builder =
	 new Notification.Builder( getApplicationContext() )
	 .setContentIntent( contentIntent )
	 .setSmallIcon( R.drawable.hopicon )
	 .setContentTitle( HopConfig.HOPRELEASE );
      Notification notification = builder.build();
      // Send the notification.
      if( notify ) {
	 mNM.notify( NOTIFICATION, notification );
      }

      return notification;
   }
}

   
