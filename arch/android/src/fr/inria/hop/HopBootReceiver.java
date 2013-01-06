/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopBootReceiver.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Sun Jan  6 07:00:44 2013 (serrano)                */
/*    Copyright   :  2010-13 Marcos Dione & Manuel Serrano             */
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
      String act = intent.getAction();

      Log.i( "HopBootReceiver", "ACTION=" + act );
      
      if( act.equals( "android.intent.action.BOOT_COMPLETED" ) ) {
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

	 return;
      }
   }
}
