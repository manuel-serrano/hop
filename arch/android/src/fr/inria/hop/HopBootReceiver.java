/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopBootReceiver.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Thu Nov  8 16:29:15 2012 (serrano)                */
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
import android.preference.*;
import android.content.res.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopBootReceiver extends BroadcastReceiver {
   @Override
   public void onReceive( Context context, Intent intent ) {
      if( "android.intent.action.BOOT_COMPLETED".equals( intent.getAction() ) ) {
	 final Resources res = context.getResources();
	 final SharedPreferences sp =
	    PreferenceManager.getDefaultSharedPreferences( context );

	 Log.i( "HopBootReceiver", "hop_autostart=" +
		sp.getBoolean( "hop_autostart", false ) );
	 
	 if( sp.getBoolean( "hop_autostart", false ) ) {
	    Intent hopintent = new Intent( context, HopService.class );
	    Hop.root = context.getApplicationInfo().dataDir;
	 
	    Log.i( "HopBootReceiver", "starting Hop service" );
	 
	    context.startService( hopintent );
	 }
      }
   }
}
