/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopUninstall.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Fri Dec 25 07:02:37 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    APP Uninstall receiver                                           */
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
public class HopUninstall extends BroadcastReceiver {
   
   @Override public void onReceive( Context context, Intent intent ) {
      Log.d( "HopUninstall", "received notification" );

      if( HopService.lasthopdroid != null ) {
	 Bundle bundle = intent.getExtras();
	 
	 HopService.lasthopdroid.pushEvent( "uninstallreceived", "???" );
      }
   }
}
