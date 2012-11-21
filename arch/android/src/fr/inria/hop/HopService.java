/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopService.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 25 17:24:05 2012                          */
/*    Last change :  Wed Nov 21 19:12:50 2012 (serrano)                */
/*    Copyright   :  2012 Manuel Serrano                               */
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

   // class variables
   static String hopargs = "";

   // instance variables
   protected Hop hop = null;
   protected HopDroid hopdroid = null;
   protected Boolean inrestart = false;
   protected Boolean inkill = false;
   
   // communication with the launcher
   Handler handler;
   ArrayBlockingQueue<String> queue;
   
   public class HopBinder extends Binder {
      HopService getService() {
	 return HopService.this;
      }
   }
   
   private final IBinder hopbinder = new HopBinder();
   
   private void startThreadLog( Thread th ) {
      String name = th.getClass().getName();
      th.start();
   }
   
   @Override
   public void onCreate() {
      Log.i( "HopService", "onCreate..." );
      
      // status bar notification
      mNM = (NotificationManager)getSystemService( NOTIFICATION_SERVICE );
      statusNotification();
   }

   @Override
   public void onDestroy() {
      Log.i( "HopService", "onDestroy... inrestart=" + inrestart
	     + " inkill=" + inkill );
      kill();
      
      // status bar update
      mNM.cancel( NOTIFICATION );
      
      super.onDestroy();

      // we are in the middle of a restart, now that that service is
      // destroy, it can notify the launcher to start a new HopService instance
      if( inrestart ) {
	 inrestart = false;
	 Log.i( "HopService", "sending restart message" );
	 handler.sendEmptyMessage( HopLauncher.MSG_START_HOP_SERVICE );
      }
   }

   @Override
   public IBinder onBind( Intent intent ) {
      Log.d( "HopService", "onBind: " + this );
      
      return hopbinder;
   }

   @Override
   public void onRebind( Intent intent ) {
      // Log.d( "HopService", "onBind: " + this );
      
      super.onRebind( intent );
   }

   @Override
   public boolean onUnbind( Intent intent ) {
      // Log.i( "HopService", "onUnbind" + this );

      return false;
   }
   
   public synchronized void kill() {
      Log.i( "HopService", ">>> kill... inkill=" + inkill );

      if( !inkill ) {
	 inkill = true;
      
	 if( hop != null ) {
	    hop.kill();
	    hop = null;
	 }
      
	 if( hopdroid != null ) {
	    hopdroid.kill();
	    hopdroid = null;
	 } else {
	    if( hopdroid.isBackground() ) {
	       hopdroid.killBackground();
	    }
	 }
      }
      Log.i( "HopService", "<<< kill" );
   }

   @Override
    public int onStartCommand( Intent intent, int flags, int startid ) {
      Log.d( "HopService", ">>> onStartCommand " + this + "..." );
      
      // create hopdroid
      hopdroid = new HopDroid( HopService.this );

      if( hopdroid.state == HopDroid.HOPDROID_STATE_INIT ) {
	 // create hop 
	 hop = new Hop( HopService.this, hopargs );
      
	 // starting hopdroid
	 startThreadLog( hopdroid );

	 // starting hop
	 startThreadLog( hop );

	 Log.d( "HopService", "<<< onStartCommandinitialized" );
	 // sticky service
	 return START_STICKY;
      } else {
	 handler.sendMessage(
	    android.os.Message.obtain(
	       handler, HopLauncher.MSG_HOP_FAILED, 0 ) );
	 stopSelf();
	 
	 return 0;
      }
   }

   public static boolean isBackground() {
      return HopDroid.isBackground();
   }

   private void statusNotification() {
      CharSequence text = getText( R.string.hopservicestarted );

      // Set the icon, scrolling text and timestamp
      Notification notification =
	 new Notification( R.drawable.hopicon, text, System.currentTimeMillis());

      PendingIntent contentIntent =
	 PendingIntent.getActivity(
	    this, 0, new Intent( this, HopService.class ), 0 );

      notification.setLatestEventInfo(
	 this, getText( R.string.hopversion ), text, contentIntent );

      notification.flags = Notification.FLAG_NO_CLEAR;

      // Send the notification.
      mNM.notify( NOTIFICATION, notification );
   }   
}

   
