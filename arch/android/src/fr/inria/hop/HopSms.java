/*=====================================================================*/
/*    .../hop/2.2.x/arch/android/src/fr/inria/hop/HopSms.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Wed Oct 20 18:47:56 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    SMS receiver                                                     */
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
public class HopSms extends BroadcastReceiver {
   
   @Override public void onReceive( Context context, Intent intent ) {
      Bundle bundle = intent.getExtras();
      SmsMessage[] msgs = null;
      
      if( bundle != null ) {
	 Object[] pdus = (Object[])bundle.get( "pdus" );
	 msgs = new SmsMessage[ pdus.length ];
	 
	 for( int i = 0; i < msgs.length; i++ ) {
	    String sms = "(\"";
	    msgs[ i ] = SmsMessage.createFromPdu( (byte[])pdus[ i ] );
	    sms += msgs[ i ].getOriginatingAddress() + "\" \""
	       + msgs[ i ].getMessageBody().toString() + "\")";
	    
	    HopAndroid.hopPushEvent( "sms-received", sms );
	 }
      }
   }
}
