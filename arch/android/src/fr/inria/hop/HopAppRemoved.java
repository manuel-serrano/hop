/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopAppRemoved.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Sun Dec 27 09:40:25 2020 (serrano)                */
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
      
      String pkgname = intent.getData().getEncodedSchemeSpecificPart();

      HopHzLauncher.removeHopHz( pkgname );
   }
}
