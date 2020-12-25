/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopAppRemoved.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Fri Dec 25 07:24:46 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    APP AppRemoved receiver                                          */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;
 
import android.content.*;
import android.os.Bundle;
import android.telephony.gsm.SmsMessage;
import android.util.Log;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopAppRemoved extends BroadcastReceiver {
   @Override public void onReceive( Context context, Intent intent ) {
      Log.d( "HopAppRemoved", "received notification" );
      
      String packageName = intent.getData().getEncodedSchemeSpecificPart();
      boolean extra = intent.getBooleanExtra( Intent.EXTRA_DATA_REMOVED, false );
      Log.d( "HopAppRemoved", "appremoved..." + packageName + " " +  (extra ? "YES" : "NO") );
      
      if( HopService.lasthopdroid != null ) {
	 Bundle bundle = intent.getExtras();
	 
	 HopService.lasthopdroid.pushEvent( "appremoved", "???" );
      }
   }
}
