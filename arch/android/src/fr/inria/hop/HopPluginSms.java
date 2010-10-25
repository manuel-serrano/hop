/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopPluginSms.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Mon Oct 25 10:17:23 2010 (serrano)                */
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
public class HopPluginSms extends HopPlugin {
   final static String SENT = "SMS_SENT";
   final static String DELIVERED = "SMS_DELIVERED";
 
   // constructor
   public HopPluginSms( HopDroid h, Activity a, String n ) {
      super( h, a, n );
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
	    String no = HopDroid.read_string( ip );
	    String msg = HopDroid.read_string( ip );

	    // the SMS has been sent
	    activity.registerReceiver( new BroadcastReceiver() {
		  @Override
		  public void onReceive( Context arg0, Intent arg1 ) {
		     switch( getResultCode() ) {
			case Activity.RESULT_OK:
			   Log.v( "HopPluginSms", "SMS sent" );
			   handroid.pushEvent( "sms-sent", "(ok)" );
			   break;
			   
			case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
			   handroid.pushEvent( "sms-sent", "(error generic-failure)" );
			   Log.v( "HopPluginSms", "Generic failure" );
			   break;
			   
			case SmsManager.RESULT_ERROR_NO_SERVICE:
			   Log.v( "HopPluginSms", "No service" );
			   handroid.pushEvent( "sms-sent", "(error no-service)" );
			   break;
			   
			case SmsManager.RESULT_ERROR_NULL_PDU:
			   Log.v( "HopPluginSms", "null PDU" );
			   handroid.pushEvent( "sms-sent", "(error null-pdu)" );
			   break;
			   
			case SmsManager.RESULT_ERROR_RADIO_OFF:
			   Log.v( "HopPluginSms", "Radio off" );
			   handroid.pushEvent( "sms-sent", "(error radio-off)" );
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
			   Log.v( "HopPluginSms", "SMS delivered" );
			   handroid.pushEvent( "sms-delivered", "(ok)" );
			   break;
			   
			case Activity.RESULT_CANCELED:
			   Log.v( "HopPluginSms", "SMS not delivered" );
			   handroid.pushEvent( "sms-delivered", "(error)" );
			   break;                        
		     }
		  }
	       }, new IntentFilter( DELIVERED ) );

	    if( msg.length() > 10 ) {
	       Log.v( "HopPluginSms", "sms send to " + no + " \""
		      + msg.substring( 0, 10 ) 
		      + "...\"" );
	    } else {
	       Log.v( "HopPluginSms", "sms send to " + no + " \""
		      + msg.substring( 0, msg.length() ) 
		      + "\"" );
	    }
	    sms.sendTextMessage( no, null, msg, sentpi, deliveredpi );     
	    return;
      }
   }
}
