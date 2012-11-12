/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopService.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 25 17:24:05 2012                          */
/*    Last change :  Sun Nov 11 14:08:01 2012 (serrano)                */
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

   // instance variables
   protected Hop hop = null;
   protected HopDroid hopdroid = null;

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
      Log.i( "HopService", "onDestroy..." );
      kill();
      
      // status bar update
      mNM.cancel( NOTIFICATION );
      
      super.onDestroy();
   }

   public void kill() {
      Log.i( "HopService", ">>> kill service..." );
      
      if( hop != null ) {
	 hop.kill();
	 hop = null;
      }
      
      if( hopdroid != null ) {
	 Log.i( "HopService", "~~~ kill foreground hopdroid..." );
	 hopdroid.kill();
	 hopdroid = null;
      } else {
	 if( hopdroid.isBackground() ) {
	    Log.i( "HopService", "~~~ kill background hopdroid..." );
	    hopdroid.killBackground();
	 }
      }
      
      Log.i( "HopService", "<<< kill service..." );
   }

   @Override
    public int onStartCommand( Intent intent, int flags, int startid ) {
      Log.d( "HopService", "onStartCommadn: " + this );
      
      // create hop 
      hop = new Hop( null, null );
      
      // create hopdroid
      hopdroid = new HopDroid( HopService.this );

      // starting hopdroid
      startThreadLog( hopdroid );

      // starting hop
      startThreadLog( hop );

      // sticky service
      return START_STICKY;
   }

   public static boolean isBackground() {
      return HopDroid.isBackground();
   }

   @Override
   public IBinder onBind( Intent intent ) {
      Log.d( "HopService", "onBind: " + this );
      
      return hopbinder;
   }

   @Override
   public void onRebind( Intent intent ) {
      Log.d( "HopService", "onBind: " + this );
      
      super.onRebind( intent );
   }

   @Override
   public boolean onUnbind( Intent intent ) {
      Log.i( "HopService", "onUnbind" );
      
/*       if( hop != null ) {                                           */
/* 	 hop.handler = null;                                           */
/*       }                                                             */
/*       if( hopdroid != null ) {                                      */
/* 	 hopdroid.handler = null;                                      */
/*       }                                                             */

      return false;
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

//      notification.flags = Notification.FLAG_NO_CLEAR | Notification.FLAG_FOREGROUND_SERVICE;
      notification.flags = Notification.FLAG_NO_CLEAR;

      // Send the notification.
      mNM.notify( NOTIFICATION, notification );
   }   
}

   
