/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopService.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 25 17:24:05 2012                          */
/*    Last change :  Wed Jun 27 08:28:02 2012 (serrano)                */
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

   public class HopBinder extends Binder {
      HopService getService() {
	 return HopService.this;
      }
   }
   
   final Hop hop = new Hop( null, null );
   final HopDroid hopdroid = new HopDroid( 8081, HopService.this );
   

   private final IBinder hopbinder = new HopBinder();
   
   private void startThreadLog( Thread th ) {
      String name = th.getClass().getName();
      Log.i( "HopService", ">>> " + name + " starting..." );
      th.start();
      Log.i( "HopService", "<<< " + name + " started..." );
   }
   
   @Override
   public void onCreate() {
      Log.i( "HopService", "service created..." );
      mNM = (NotificationManager)getSystemService( NOTIFICATION_SERVICE );

      statusNotification();
   }

   @Override
   public void onDestroy() {
      Log.i( "HopService", ">>> onDestroy..." );

      // status bar update
      mNM.cancel( NOTIFICATION );
      
      hop.kill();
      hopdroid.kill();
      
      Log.i( "HopService", "<<< onDestroy..." );
   }

   @Override
    public int onStartCommand( Intent intent, int flags, int startid ) {
      // starting hopdroid
      startThreadLog( hopdroid );

      // starting hop
      startThreadLog( hop );

      // sticky service
      return START_STICKY;
   }

   @Override
   public IBinder onBind( Intent intent ) {
      return hopbinder;
   }

   @Override
   public boolean onUnbind( Intent intent ) {
      hop.handler = null;
      hopdroid.handler = null;

      return false;
   }
   
   private void statusNotification() {
      CharSequence text = getText (R.string.hopservicestarted );

      // Set the icon, scrolling text and timestamp
      Notification notification =
	 new Notification( R.drawable.hopicon, text, System.currentTimeMillis());

      PendingIntent contentIntent =
	 PendingIntent.getActivity(
	    this, 0, new Intent( this, HopService.class ), 0);

      notification.setLatestEventInfo(
	 this, getText( R.string.hopversion ), text, contentIntent );

      // Send the notification.
      mNM.notify( NOTIFICATION, notification );
   }   
}

   
