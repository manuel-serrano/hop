/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopBootReceiver.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Fri Nov  2 17:49:08 2012 (serrano)                */
/*    Copyright   :  2010-12 Marcos Dione & Manuel Serrano             */
/*    -------------------------------------------------------------    */
/*    Hop Boot Receiver                                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.os.*;
import android.content.*;
import android.util.Log;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopBootReceiver extends BroadcastReceiver {
   @Override
   public void onReceive( Context context, Intent intent ) {
      Log.i( "HopBootReceiver", "onReceive action=" + intent.getAction() );
      
      if( "android.intent.action.BOOT_COMPLETED".equals( intent.getAction() ) ) {
	 Intent hopintent = new Intent( context, HopService.class );
	 Hop.root = context.getApplicationInfo().dataDir;
	 
	 Log.i( "HopBootReceiver", "starting Hop service" );
	 
	 context.startService( hopintent );
      }
   }
}
