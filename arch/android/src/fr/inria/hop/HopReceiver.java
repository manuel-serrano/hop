/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopReceiver.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jan  1 06:16:50 2021                          */
/*    Last change :  Fri Jan  1 06:43:38 2021 (serrano)                */
/*    Copyright   :  2021 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Manage Broadcast receivers                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;
 
import android.os.*;
import android.content.*;
import android.app.Activity;
import android.content.Context;
import android.util.Log;

/*---------------------------------------------------------------------*/
/*    HopReceiver ...                                                  */
/*---------------------------------------------------------------------*/
public class HopReceiver implements HopStage {
   Handler handler;
   Activity activity;
   BroadcastReceiver receiver = null;
   Context context = null;
   
   // constructor
   public HopReceiver( Activity a, Handler h ) {
      super();

      handler = h;
      activity = a;
   }

   // exec
   public void exec( Context context, Object arg ) {
      receiver = new HopAppRemoved();
      context = context;
      
      IntentFilter filter = new IntentFilter( Intent.ACTION_PACKAGE_REMOVED );
      context.registerReceiver( receiver, filter );
   }

   public void abort() {
      Log.d( "HopPermission", "abort" );

      if( context != null && receiver != null ) {
	 context.unregisterReceiver( receiver );
      }
   }
}

