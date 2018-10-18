/*=====================================================================*/
/*    .../hopdac/arch/android/src/fr/inria/hop/HopIntenter.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jul  5 09:42:40 2016                          */
/*    Last change :  Mon Jul 11 21:08:05 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Spawn Hop service (not the Hop process).                         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.util.Log;
import android.os.*;

import java.util.*;
import java.util.concurrent.ArrayBlockingQueue;
import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopIntenter implements HopStage {
   Handler handler;
   Intent hopintent = null;
   HopService hopservice = null;
   boolean hopconnected = false;
   ArrayBlockingQueue<String> queue;
   
   Activity activity;

   public HopIntenter( Activity act, Handler hdl, ArrayBlockingQueue<String> q ) {
      super();
      activity = act;
      handler = hdl;
      queue = q;
   }
   
   final ServiceConnection hopconnection = new ServiceConnection() {
	 public void onServiceConnected( ComponentName className, IBinder service ) {
	    Log.d( "HopIntenter", "onServiceConnected" );
	    hopservice = ((HopService.HopBinder)service).getService();

	    try {
	       hopservice.handler = handler;
	       hopservice.queue = queue;
	       hopservice.hopdroid.activity = activity;
	       hopconnected = true;
	       hopservice.onConnect();
		  
	       if( hopservice.waitHop( 4000 ) ) {
		  Log.d( "HopIntenter", "Hop ready..." );
		  handler.sendEmptyMessage( HopLauncher.MSG_HOP_START  );
	       } else {
		  Log.d( "HopIntenter", "Hop fail..." );
		  handler.sendEmptyMessage( HopLauncher.MSG_HOP_FAIL );
	       }
	    } catch( Exception e ) {
	       Log.e( "HopIntenter", "error while connecting to service: " +
		      e.toString() );
	       e.printStackTrace();
	       Log.e( "HopIntenter", "killing background hop because of error..." );
	       hopconnected = false;
	       activity.unbindService( hopconnection );
	       HopService.emergencyExit();
	       //kill( 4000 );
	    }
	 }

	 public void onServiceDisconnected( ComponentName className ) {
	    hopconnected = false;
	    hopservice = null;
	 }
      };
   
   void raise( String stage, int kmsg, Exception e ) {
      String msg = e.getMessage();
      
      Log.e( stage, e.toString() );
      e.printStackTrace();
      
      if( msg == null ) msg = e.getClass().getName();
      
      handler.sendMessage( android.os.Message.obtain( handler, kmsg, e ) );
   }

   void raise( String stage, int kmsg, String msg ) {
      Log.e( stage, msg );
      
      handler.sendMessage( android.os.Message.obtain( handler, kmsg, msg ) );
   }

   public void exec() {
      Log.d( "HopIntenter", "exec" );
      
      hopintent = new Intent( activity.getApplicationContext(), HopService.class );
      if( !HopService.isBackground() ) {
	 activity.startService( hopintent );
      }

      activity.bindService( hopintent, hopconnection, Context.BIND_AUTO_CREATE );
   }

   public void abort() {
      Log.d( "HopIntenter", "abort" );

      activity.unbindService( hopconnection );
   }
}
  
