/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopAndroidSms.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Sun Oct 17 19:23:57 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Dealing with SMS                                                 */
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
public class HopAndroidSms {
   final static String SENT = "SMS_SENT";
   final static String DELIVERED = "SMS_DELIVERED";
 
   // instance variables
   Activity activity;
   
   // constructor
   public HopAndroidSms( Activity a ) {
      activity = a;
   }

   // sensor manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( ip.read() ) {
	 case (byte)'s':
	    // send a SMS
	    PendingIntent sentpi = PendingIntent.getBroadcast(
	       activity, 0, new Intent( SENT ), 0 );
	    PendingIntent deliveredpi = PendingIntent.getBroadcast(
	       activity, 0, new Intent( DELIVERED ), 0 );
 
	    SmsManager sms = SmsManager.getDefault();
	    String no = HopAndroid.read_string( ip );
	    String msg = HopAndroid.read_string( ip );

	    // the SMS has been sent
	    registerReceiver( new BroadcastReceiver() {
		  @Override
		  public void onReceive( Context arg0, Intent arg1 ) {
		     switch( getResultCode() ) {
			case Activity.RESULT_OK:
			   Toast.makeText( getBaseContext(), "SMS sent", 
					   Toast.LENGTH_SHORT ).show();
			   break;
			   
			case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
			   Toast.makeText( getBaseContext(), "Generic failure", 
					   Toast.LENGTH_SHORT ).show();
			   break;
			   
			case SmsManager.RESULT_ERROR_NO_SERVICE:
			   Toast.makeText( getBaseContext(), "No service", 
					  Toast.LENGTH_SHORT ).show();
			   break;
			   
			case SmsManager.RESULT_ERROR_NULL_PDU:
			   Toast.makeText( getBaseContext(), "Null PDU", 
					   Toast.LENGTH_SHORT ).show();
			   break;
			   
			case SmsManager.RESULT_ERROR_RADIO_OFF:
			   Toast.makeText( getBaseContext(), "Radio off", 
					   Toast.LENGTH_SHORT ).show();
			   break;
		     }
		  }
	       }, new IntentFilter( SENT ) );

	    // the SMS has been delivered
	    registerReceiver( new BroadcastReceiver() {
		  @Override
		  public void onReceive( Context arg0, Intent arg1 ) {
		     switch( getResultCode() ) {
			case Activity.RESULT_OK:
			   Toast.makeText( getBaseContext(), "SMS delivered", 
					   Toast.LENGTH_SHORT ).show();
			   break;
			   
			case Activity.RESULT_CANCELED:
			   Toast.makeText( getBaseContext(), "SMS not delivered", 
					   Toast.LENGTH_SHORT ).show();
			   break;                        
		     }
		  }
	       }, new IntentFilter( DELIVERED ) );
	    
	    sms.sendTextMessage( no, null, msg, sendpi, deliveredpi );     
	    return;
      }
   }
}
