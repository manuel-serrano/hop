/*=====================================================================*/
/*    .../hopdac/arch/android/src/fr/inria/hop/HopSpawner.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jul  5 09:42:40 2016                          */
/*    Last change :  Tue Jul  5 10:01:31 2016 (serrano)                */
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
public class HopSpawner implements HopStage {
   Handler handler;
   Intent hopintent = null;
   HopService hopservice = null;
   boolean hopconnected = false;
   Activity activity;
   final ArrayBlockingQueue<String> queue =
      new ArrayBlockingQueue<String>( 10 );
   

   public HopSpawner( Activity act, Handler hdl ) {
      super();
      activity = act;
      handler = hdl;
   }
   
   final ServiceConnection hopconnection = new ServiceConnection() {
	 public void onServiceConnected( ComponentName className, IBinder service ) {
	    Log.d( "HopSpawner", "onServiceConnected" );
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
   
   public void exec() {
      Log.d( "HopSpawner", "exec" );
      
      hopintent = new Intent( activity.getApplicationContext(), HopService.class );
      if( !HopService.isBackground() ) {
	 activity.startService( hopintent );
      }

      Log.d( "HopSpawner", "<<< binding activity" );
      activity.bindService( hopintent, hopconnection, Context.BIND_AUTO_CREATE );
      Log.d( "HopSpawner", "<<< activity bound" );
   }

   public void abort() {
      Log.d( "HopSpawner", "abort" );

      activity.unbindService( hopconnection );
   }
}
  
