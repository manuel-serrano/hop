/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopHzService.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 25 17:24:05 2012                          */
/*    Last change :  Sun Dec 27 08:16:06 2020 (serrano)                */
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
public class HopHzService extends HopService {
   @Override
    public int onStartCommand( Intent intent, int flags, int startid ) {
      Log.d( "HopHzService", "onStartCommand " + this + "..." + " flags=" + flags + " startid=" + startid );
      Activity activity = ((HopIntent)intent).activity;

      // create hopdroid
      lasthopdroid = hopdroid = new HopDroid( HopHzService.this, activity );

      ((HopHzLauncher)activity).hzhopdroid = hopdroid;
	 
      if( hopdroid.state == HopDroid.HOPDROID_STATE_INIT ) {
	 // starting hopdroid
	 hopdroid.start();

	 // sticky service
	 return START_NOT_STICKY;
      } else {
	 Log.d( "HopHzService", "stopSelf" );
	 stopSelf();

	 return 0;
      }
   }
}

   
