/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopAndroidSms.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Mon Oct 18 13:33:10 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Dealing with SMS                                                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;
 
import android.app.*;
import android.content.*;
import android.os.Bundle;
import android.telephony.*;
import android.util.Log;


import java.io.*;

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
	    activity.registerReceiver( new BroadcastReceiver() {
		  @Override
		  public void onReceive( Context arg0, Intent arg1 ) {
		     switch( getResultCode() ) {
			case Activity.RESULT_OK:
			   Log.v( "HopAndroidSms", "SMS sent" );
			   break;
			   
			case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
			   Log.v( "HopAndroidSms", "Generic failure" );
			   break;
			   
			case SmsManager.RESULT_ERROR_NO_SERVICE:
			   Log.v( "HopAndroidSms", "No service" );
			   break;
			   
			case SmsManager.RESULT_ERROR_NULL_PDU:
			   Log.v( "HopAndroidSms", "null PDU" );
			   break;
			   
			case SmsManager.RESULT_ERROR_RADIO_OFF:
			   Log.v( "HopAndroidSms", "Radio off" );
			   break;
		     }
		  }
	       }, new IntentFilter( SENT ) );

	    // the SMS has been delivered
	    activity.registerReceiver( new BroadcastReceiver() {
		  @Override
		  public void onReceive( Context arg0, Intent arg1 ) {
		     switch( getResultCode() ) {
			case Activity.RESULT_OK:
			   Log.v( "HopAndroidSms", "SMS delivered" );
			   break;
			   
			case Activity.RESULT_CANCELED:
			   Log.v( "HopAndroidSms", "SMS not delivered" );
			   break;                        
		     }
		  }
	       }, new IntentFilter( DELIVERED ) );
	    
	    sms.sendTextMessage( no, null, msg, sentpi, deliveredpi );     
	    return;
      }
   }
}
