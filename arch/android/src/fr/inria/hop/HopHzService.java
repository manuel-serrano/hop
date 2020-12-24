/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopHzService.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 25 17:24:05 2012                          */
/*    Last change :  Thu Dec 24 17:50:01 2020 (serrano)                */
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
public class HopService extends HopService {
   @Override
    public int onStartCommand( Intent intent, int flags, int startid ) {
      Log.d( "HopService", "onStartCommand " + this + "..." + " flags=" + flags + " startid=" + startid );
      
      // create hopdroid
      lasthopdroid = hopdroid = new HopDroid( HopHzService.this, ((HopIntent)intent).activity );

      if( hopdroid.state == HopDroid.HOPDROID_STATE_INIT ) {
	 // starting hopdroid
	 hopdroid.start();

	 // sticky service
	 return START_NOT_STICKY;
      } else {
	 stopSelf();

	 return 0;
      }
   }
}

   
