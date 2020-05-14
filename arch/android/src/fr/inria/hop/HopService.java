/*=====================================================================*/
/*    .../hopdac/arch/android/src/fr/inria/hop/HopService.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 25 17:24:05 2012                          */
/*    Last change :  Tue May 12 15:40:05 2020 (serrano)                */
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

   // class variables
   static Hop lasthop = null;
   static HopDroid lasthopdroid;
   
   // instance variables
   protected Hop hop = null;
   protected HopDroid hopdroid = null;
   protected Boolean inrestart = false;
   Handler handler;
   
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
      // status bar notification
      mNM = (NotificationManager)getSystemService( NOTIFICATION_SERVICE );

      Notification notification = statusNotification( false );

      startForeground( HOP_ID, notification );
   }

   @Override
   public void onDestroy() {
      Log.i( "HopService", "onDestroy..." );

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
	 Log.i( "HopService", "sending restart message" );
	 handler.sendEmptyMessage( HopLauncher.MSG_START_HOP_SERVICE );
      }
   }

   @Override
   public IBinder onBind( Intent intent ) {
      Log.d( "HopService", "onBind: this=" + this + " hopbinder=" + hopbinder );
      
      return hopbinder;
   }

   @Override
   public void onRebind( Intent intent ) {
      Log.d( "HopService", "onRebind: " + this );
      
      super.onRebind( intent );
      handler.sendEmptyMessage( HopLauncher.MSG_REBIND_HOP_SERVICE );
   }

   @Override
   public boolean onUnbind( Intent intent ) {
      Log.i( "HopService", "onUnbind: " + this );

      kill();
      // true is returned to get onRebind invoked
      return true;
   }

   public void onConnect() {
      // invoked by HopLauncher when the connection is established.
      // this is used to complete the plugin initialization
      hopdroid.onConnect();
   }
   
   static public void emergencyExit() {
      Log.i( "HopService", ">>> emergencyExit..." );
      // called by HopLauncher when the communication between the
      // launcher and the service cannot be established
      HopDroid.emergencyExit();
      Hop.emergencyExit();
      Log.i( "HopService", "<<< emergencyExit" );
   }
   
   public synchronized void kill() {
      Log.i( "HopService", ">>> kill..." );

      if( hop != null ) {
	 hop.kill();
	 lasthop = hop = null;
      }
      
      if( hopdroid != null ) {
	 hopdroid.kill();
	 lasthopdroid = hopdroid = null;
      }
      
      Log.i( "HopService", "<<< kill" );
   }

   @Override
    public int onStartCommand( Intent intent, int flags, int startid ) {
      Log.d( "HopService", "onStartCommand " + this + "..." + " flags=" + flags + " startid=" + startid );
      
      // create hopdroid
      lasthopdroid = hopdroid = new HopDroid( HopService.this );

      if( hopdroid.state == HopDroid.HOPDROID_STATE_INIT ) {
	 // create hop 
	 lasthop = hop = new Hop( HopService.this );
      
	 // starting hopdroid
	 hopdroid.start();

	 // starting hop
	 hop.start();

	 // sticky service
	 return START_NOT_STICKY;
      } else {
	 stopSelf();

	 return 0;
      }
   }

   public static boolean isBackground() {
      return lasthop != null && lasthop.isRunning( 0 );
      //return HopDroid.isBackground();
   }

   public boolean waitHop( final int timeout ) {
      // wait (timeout ms) for the Hop server to be up and running
      final boolean[] res = new boolean[ 1 ];

      Thread th = new Thread( new Runnable() {
	    public void run() {
	       int tmt = timeout;
	       synchronized( res ) {
		  while( tmt >= 1000 ) {
		     if( hop.isRunning( 1000 ) ) {

			res[ 0 ] = true;
			res.notify();
			return;
		     } else {
			try {
			   Thread.sleep( 1000 );
			} catch( Exception e ) {
			   ;
			}
			tmt -= 1000;
		     }
		  }
	       
		  res[ 0 ] = false;
		  res.notify();
		  return;
	       }
	    }
	 } );
	 
      synchronized( res ) {
	 th.start();

	 try {
	    res.wait();
	    return res[ 0 ];
	 } catch( Exception e ) {
	    return false;
	 }
      }
   }

   private Notification statusNotification( boolean notify ) {
      // Set the icon, scrolling text and timestamp
      CharSequence text = getText( R.string.hopservicestarted );
      Notification notification =
	 new Notification( R.drawable.hopicon, text, System.currentTimeMillis());
      notification.flags = Notification.FLAG_NO_CLEAR;

      PendingIntent contentIntent =
	 PendingIntent.getActivity(
	    this, 0, new Intent( this, HopService.class ), 0 );

/*       notification.setLatestEventInfo(                              */
/* 	 this, HopConfig.HOPRELEASE, text, contentIntent );            */

      // Send the notification.
      if( notify ) {
	 mNM.notify( NOTIFICATION, notification );
      }

      return notification;
   }
}

   
