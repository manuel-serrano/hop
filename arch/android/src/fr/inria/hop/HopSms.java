/*=====================================================================*/
/*    .../hop/2.2.x/arch/android/src/fr/inria/hop/HopSms.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Sun Oct 17 19:27:25 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    SMS receiver                                                     */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;
 
import android.content.*;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.telephony.gsm.SmsMessage;

import android.widget.Toast;
 
/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopSms extends BroadcastReceiver {
   @Override public void onReceive( Context context, Intent intent ) {
      // a new SMS showed up
      Bundle bundle = intent.getExtras();
      SmsMessage[] msgs = null;
      String str = "";
      if( bundle != null ) {
	 Object[] pdus = (Object[]) bundle.get( "pdus" );
	 msgs = new SmsMessage[ pdus.length ];
	 for( int i = 0; i < msgs.length; i++ ) {
	    msgs[ i ] = SmsMessage.createFromPdu( (byte[])pdus[ i ] );
	    str += "SMS from " + msgs[ i ].getOriginatingAddress();
	    str += " :";
	    str += msgs[ i ].getMessageBody().toString();
	    str += "\n";
	 }
	 Toast.makeText( context, str, Toast.LENGTH_SHORT ).show();
      }
   }
}
