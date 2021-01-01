/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopReceiver.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jan  1 06:16:50 2021                          */
/*    Last change :  Fri Jan  1 06:31:53 2021 (serrano)                */
/*    Copyright   :  2021 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Manage Broadcast receivers                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;
 
import android.content.*;
import android.util.Log;

/*---------------------------------------------------------------------*/
/*    HopReceiver ...                                                  */
/*---------------------------------------------------------------------*/
public class HopReceiver implements HopStage {
   Handler handler;
   Activity activity;
   
   // constructor
   public HopReceiver( Activity a, Handler h ) {
      super();

      handler = h;
      activity = a;
   }

   // exec
   public void exec( Context context, Object arg ) {
      BroadcastReceiver br = new HopAppRemoved();
      
      IntentFilter filter = new IntentFilter( Intent.ACTION_PACKAGE_REMOVED );
      this.registerReceiver( br, filter );
   }
}

