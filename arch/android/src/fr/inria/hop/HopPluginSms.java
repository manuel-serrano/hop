/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopPluginSms.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Thu Jun 28 16:25:43 2012 (serrano)                */
/*    Copyright   :  2010-12 Manuel Serrano                            */
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
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginSms extends HopPlugin {
   final static String SENT = "SMS_SENT";
   final static String DELIVERED = "SMS_DELIVERED";
   
   BroadcastReceiver sent_receiver = null;
   BroadcastReceiver delivered_receiver = null;

   // constructor
   public HopPluginSms( HopDroid h, String n ) {
      super( h, n );
   }

   // sensor manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'s':
	    // send a SMS
	    PendingIntent sentpi = PendingIntent.getBroadcast(
	       hopdroid.service, 0, new Intent( SENT ), 0 );
	    PendingIntent deliveredpi = PendingIntent.getBroadcast(
	       hopdroid.service, 0, new Intent( DELIVERED ), 0 );
 
	    SmsManager sms = SmsManager.getDefault();
	    String no = HopDroid.read_string( ip );
	    String msg = HopDroid.read_string( ip );

	    // the SMS has been sent
	    initReceivers();

	    if( msg.length() > 10 ) {
	       Log.v( "HopPluginSms", "sms send to " + no + " \""
		      + msg.substring( 0, 10 ) 
		      + "...\"" );
	    } else {
	       Log.v( "HopPluginSms", "sms send to " + no + " \""
		      + msg.substring( 0, msg.length() ) 
		      + "\"" );
	    }
	    try {
	       ArrayList<String> dmsg = sms.divideMessage( msg );

	       if( dmsg.size() > 1 ) {
		  ArrayList<PendingIntent> sentpis = new ArrayList<PendingIntent>();
		  ArrayList<PendingIntent> deliveredpis = new ArrayList<PendingIntent>();

		  for( int i = 0; i < dmsg.size(); i++ ) {
		     sentpis.add( sentpi );
		     deliveredpis.add( deliveredpi );
		  }

		  sms.sendMultipartTextMessage( no, null, dmsg, sentpis, deliveredpis );
	       } else {
		  sms.sendTextMessage( no, null, msg, sentpi, deliveredpi );
	       }
	    } catch( Throwable e ) {
	       Log.e( "HopPluginSms", "Cannot send message", e );
	    }
	    return;
      }
   }

   // initRecievers
   private void initReceivers() {
      if( sent_receiver == null ) {
	 sent_receiver = new BroadcastReceiver() {
	       @Override
	       public void onReceive( Context arg0, Intent arg1 ) {
		  switch( getResultCode() ) {
		     case Activity.RESULT_OK:
			Log.v( "HopPluginSms", "SMS sent" );
			hopdroid.pushEvent( "sms-sent", "(ok)" );
			break;
			   
		     case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
			hopdroid.pushEvent( "sms-sent", "(error generic-failure)" );
			Log.v( "HopPluginSms", "Generic failure" );
			break;
			   
		     case SmsManager.RESULT_ERROR_NO_SERVICE:
			Log.v( "HopPluginSms", "No service" );
			hopdroid.pushEvent( "sms-sent", "(error no-service)" );
			break;
			   
		     case SmsManager.RESULT_ERROR_NULL_PDU:
			Log.v( "HopPluginSms", "null PDU" );
			hopdroid.pushEvent( "sms-sent", "(error null-pdu)" );
			break;
			   
		     case SmsManager.RESULT_ERROR_RADIO_OFF:
			Log.v( "HopPluginSms", "Radio off" );
			hopdroid.pushEvent( "sms-sent", "(error radio-off)" );
			break;
		  }
	       }
	    };
	 hopdroid.service.registerReceiver( sent_receiver, new IntentFilter( SENT ) );
      }

      if( delivered_receiver == null ) {
	 delivered_receiver = new BroadcastReceiver() {
	       @Override
	       public void onReceive( Context arg0, Intent arg1 ) {
		  switch( getResultCode() ) {
		     case Activity.RESULT_OK:
			Log.v( "HopPluginSms", "SMS delivered" );
			hopdroid.pushEvent( "sms-delivered", "(ok)" );
			break;
			   
		     case Activity.RESULT_CANCELED:
			Log.v( "HopPluginSms", "SMS not delivered" );
			hopdroid.pushEvent( "sms-delivered", "(error)" );
			break;                        
		  }
	       }
	    };

	 hopdroid.service.registerReceiver( delivered_receiver, new IntentFilter( DELIVERED ) );
      }
   }

   // cleanup
   public void kill() {
      super.kill();

      if( sent_receiver != null ) {
	 hopdroid.service.unregisterReceiver( sent_receiver );
      }

      if( delivered_receiver != null ) {
	 hopdroid.service.unregisterReceiver( delivered_receiver );
      }
   }
}
