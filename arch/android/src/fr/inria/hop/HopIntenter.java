/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopIntenter.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jul  5 09:42:40 2016                          */
/*    Last change :  Sun Dec 27 08:23:29 2020 (serrano)                */
/*    Copyright   :  2016-20 Manuel Serrano                            */
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
	       hopservice.onConnect();
	    } catch( Exception e ) {
	       Log.e( "HopIntenter", "error while connecting to service: " +
		      e.toString() );
	       e.printStackTrace();
	       activity.unbindService( hopconnection );
	    }
	 }

	 public void onServiceDisconnected( ComponentName className ) {
	    Log.d( "HopIntenter", "onServiceDisconnected" );
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

   public void exec( Context context, Object clazz ) {
      Log.d( "HopIntenter", "exec " + ((Class)clazz).getName() );
      
      hopintent = new HopIntent( context, (Class)clazz, activity );
      
      Log.d( "HopIntenter", "Hop.Service.isBackground=" + HopService.isBackground() );
      if( !HopService.isBackground() ) {
	 activity.startService( hopintent );
      }

      activity.bindService( hopintent, hopconnection, Context.BIND_AUTO_CREATE );
   }

   public void abort() {
      Log.d( "HopIntenter", "abort..." );

      activity.unbindService( hopconnection );
      if( hopservice != null ) {
	 hopservice.stopSelf();
      }
   }
}
  
