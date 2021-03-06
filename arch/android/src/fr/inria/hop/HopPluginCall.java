/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopPluginCall.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Sun Dec 27 17:13:34 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Dealing with phone Calls                                         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.os.*;
import android.util.Log;
import android.database.Cursor;
import android.net.Uri;
import android.provider.CallLog.Calls;
import android.telephony.*;

import java.net.*;
import java.io.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginCall extends HopPlugin {
   TelephonyManager tm = null;
   Intent ci = null;
   int ca = 0;

   // PhoneStateListenr
   PhoneStateListener pl = new PhoneStateListener() {
	 public void onCallStateChanged( int state, String in ) {
	    switch( state ) {
	       case TelephonyManager.CALL_STATE_RINGING:
		  hopdroid.pushEvent( "call", "(call-state ringing " + in + " )" );
		  break;
	       case TelephonyManager.CALL_STATE_OFFHOOK:
		  hopdroid.pushEvent( "call", "(call-state offhook " + in + " )" );
		  break;
	       case TelephonyManager.CALL_STATE_IDLE:
		  hopdroid.pushEvent( "call", "(call-state idle " + in + " )" );
		  break;
	       default:
		  Log.d( "HopPluginCall", "Unknown phone state=" + state );
	    }
	 }
	 
	 public void onDataConnectionStateChanged( int state ) {
	    switch( state ) {
	       case TelephonyManager.DATA_DISCONNECTED:
		  hopdroid.pushEvent( "call", "(data disconnected)" );
		  break;
	       case TelephonyManager.DATA_CONNECTING:
		  hopdroid.pushEvent( "call", "(data connecting)" );
		  break;
	       case TelephonyManager.DATA_CONNECTED:
		  hopdroid.pushEvent( "call", "(data connected)" );
		  break;
	       case TelephonyManager.DATA_SUSPENDED:
		  hopdroid.pushEvent( "call", "(data suspended)" );
		  break;
	       default:
		  Log.d( "HopPluginCall", "Unknown phone state=" + state );
	    }
	 }
	 
	 public void onServiceStateChanged( ServiceState sstate ) {
	    switch( sstate.getState() ) {
	       case ServiceState.STATE_EMERGENCY_ONLY:
		  hopdroid.pushEvent( "call", "(state emergency-only)" );
		  break;
	       case ServiceState.STATE_IN_SERVICE:
		  hopdroid.pushEvent( "call", "(state in-service)" );
		  break;
	       case ServiceState.STATE_OUT_OF_SERVICE:
		  hopdroid.pushEvent( "call", "(state out-of-service)" );
		  break;
	       case ServiceState.STATE_POWER_OFF:
		  Log.d( "HopPluginCall", "state: power-off" );
		  break;
	    }
	 };
      };
   
   // constructor
   HopPluginCall( HopDroid h, String n ) {
      super( h, n );
   }

    // calllog manager
   public void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'l':
	    // get call log
	    int i = HopDroid.read_int32( ip );
	    writeCallLogList( op, i );
	    break;
	    
	 case (byte)'b':
	    // register call listener
	    registerCallListener();
	    break;
	    
	 case (byte)'e':
	    // unregister call listener
	    unregisterCallListener();
	    break;
	    
	 case (byte)'i':
	    // register call listener
	    writeCallState( op );
	    break;
	    
	 case (byte)'c':
	    // start a new call
	    startCall( ip, op );
	    break;
	    
	 case (byte)'d':
	    // dial a new call
	    dial( ip, op );
	    break;
	    
	 case (byte)'k':
	    // stop a call
	    stopCall();
	    break;
      }
   }

   // initTelephoneManager
   void initTelephoneManager() {
      if( tm == null ) {
	 tm = (TelephonyManager)hopdroid.service.getSystemService( Context.TELEPHONY_SERVICE );
      }
   }
   
   // registerCallListener
   synchronized void registerCallListener() {
      initTelephoneManager();

      tm.listen( pl, ~0 );
   }
      
   // unregisterCallListener
   synchronized void unregisterCallListener() {
      if( tm != null ) {
	 tm.listen( pl, 0 );
	 tm = null;
      }
   }

   // writeCallState
   synchronized void writeCallState( final OutputStream op ) throws IOException {
      initTelephoneManager();
      
      op.write( "(".getBytes() );

      writeInfo( "device-id", tm.getDeviceId(), op );
      writeInfo( "device-software-version", tm.getDeviceSoftwareVersion(), op );
      writeInfo( "device-line1-number", tm.getLine1Number(), op );
      writeInfo( "network-country-iso", tm.getNetworkCountryIso(), op );
      writeInfo( "network-operator", tm.getNetworkOperator(), op );
      writeInfo( "network-operator-name", tm.getNetworkOperatorName(), op );
      
      op.write( " (network-type ".getBytes() );
      switch( tm.getNetworkType() ) {
	 case TelephonyManager.NETWORK_TYPE_UNKNOWN:
	    op.write( "unknown".getBytes() );
	    break;
	 case TelephonyManager.NETWORK_TYPE_GPRS:
	    op.write( "gprs".getBytes() );
	    break;
	 case TelephonyManager.NETWORK_TYPE_EDGE:
	    op.write( "edge".getBytes() );
	    break;
	 case TelephonyManager.NETWORK_TYPE_UMTS:
	    op.write( "umts".getBytes() );
	    break;
	 case TelephonyManager.NETWORK_TYPE_HSDPA:
	    op.write( "hsdpa".getBytes() );
	    break;
	 case TelephonyManager.NETWORK_TYPE_HSUPA:
	    op.write( "hsupa".getBytes() );
	    break;
	 case TelephonyManager.NETWORK_TYPE_HSPA:
	    op.write( "hspa".getBytes() );
	    break;
	 case TelephonyManager.NETWORK_TYPE_CDMA:
	    op.write( "cdma".getBytes() );
	    break;
	 case TelephonyManager.NETWORK_TYPE_EVDO_0:
	    op.write( "evdo_0".getBytes() );
	    break;
	 case TelephonyManager.NETWORK_TYPE_EVDO_A:
	    op.write( "evdo_a".getBytes() );
	    break;
	 case TelephonyManager.NETWORK_TYPE_1xRTT:
	    op.write( "1xrtt".getBytes() );
	    break;
	 default:
	    op.write( "unknown".getBytes() );
	    break;
      }
      op.write( ")".getBytes() );

      op.write( " (phone-type ".getBytes() );
      switch( tm.getPhoneType() ) {
	 case TelephonyManager.PHONE_TYPE_NONE:
	    op.write( "none".getBytes() );
	    break;
	 case TelephonyManager.PHONE_TYPE_GSM:
	    op.write( "gsm".getBytes() );
	    break;
	 case TelephonyManager.PHONE_TYPE_CDMA:
	    op.write( "cdma".getBytes() );
	    break;
      }
      op.write( ")".getBytes() );

      writeInfo( "sim-country-iso", tm.getSimCountryIso(), op );
      writeInfo( "sim-operator", tm.getSimOperator(), op );
      writeInfo( "sim-operator-name", tm.getSimOperatorName(), op );
      writeInfo( "sim-serial-number", tm.getSimSerialNumber(), op );

      op.write( " (sim-state ".getBytes() );
      switch( tm.getSimState() ) {
	 case TelephonyManager.SIM_STATE_UNKNOWN:
	    op.write( "unknown".getBytes() );
	    break;
	 case TelephonyManager.SIM_STATE_ABSENT:
	    op.write( "absent".getBytes() );
	    break;
	 case TelephonyManager.SIM_STATE_PIN_REQUIRED:
	    op.write( "pin-required".getBytes() );
	    break;
	 case TelephonyManager.SIM_STATE_PUK_REQUIRED:
	    op.write( "puk-required".getBytes() );
	    break;
	 case TelephonyManager.SIM_STATE_NETWORK_LOCKED:
	    op.write( "network-locked".getBytes() );
	    break;
	 case TelephonyManager.SIM_STATE_READY:
	    op.write( "ready".getBytes() );
	    break;
      }
      op.write( ")".getBytes() );

      writeInfo( "voice-mail-number", tm.getVoiceMailNumber(), op );

      op.write( ")".getBytes() );
   }

   // write infod field
   void writeInfo( String key, String value, final OutputStream op ) throws IOException {
      if( value != null ) {
	 op.write( " (".getBytes() );
	 op.write( key.getBytes() );
	 op.write( " \"".getBytes() );
	 op.write( value.getBytes() );
	 op.write( "\")".getBytes() );
      }
   }

   // dial
   void dial( final InputStream ip, final OutputStream op ) throws IOException {
      String number = HopDroid.read_string( ip );

      Intent dialIntent = new Intent( Intent.ACTION_DIAL );
      dialIntent.setData( Uri.parse( "tel:" + number ) );

      hopdroid.service.startActivity( dialIntent );
      Log.d( "HopPluginCall", "dial activity started..." );
   }

   // startCall
   void startCall( final InputStream ip, final OutputStream op ) throws IOException {
      String number = HopDroid.read_string( ip );
      boolean newactivity = HopDroid.read_int( ip ) != 0;

      Intent callIntent = new Intent( Intent.ACTION_CALL );
      callIntent.setData( Uri.parse( "tel:" + number ) );

      Intent i = new Intent( hopdroid.service.getApplicationContext(), hopdroid.activityclass );
      
      i.addFlags( Intent.FLAG_ACTIVITY_CLEAR_TOP );
      i.addFlags( Intent.FLAG_ACTIVITY_REORDER_TO_FRONT );
      i.addFlags( Intent.FLAG_ACTIVITY_NEW_TASK );
      Log.d( "HopPluginCall", "raising/starting..." +  hopdroid.service.getClass().getName() );
      hopdroid.service.startActivity( i );

      if( newactivity ) {
	 ci = null;
	 ca = startHopActivityForResult( callIntent );
	 Log.d( "HopPluginCall", "startCall activity started..." );
      } else {
	 
	 ci = callIntent;
	 ca = 0;
	 hopdroid.service.startService( callIntent );
	 Log.d( "HopPluginCall", "startCall Service started..." );
      }
   }

   // stopCall
   void stopCall() {
      if( ci != null ) {
	 Log.d( "HopPluginCall", "stopping service..." );
	 hopdroid.service.stopService( ci );
	 Log.d( "HopPluginCall", "service stopped." );
      }
      if( ca > 0 ) {
	 Log.d( "HopPluginCall", "Finishing activity: " + ca );
	 // as of 10 Jan 2010, switching to airplane mode is apparantly
	 // the only way to abort a phone call
	 android.provider.Settings.System.putInt(
	    hopdroid.service.getContentResolver(),
	    android.provider.Settings.System.AIRPLANE_MODE_ON, 1 );

	 Intent intent = new Intent( Intent.ACTION_AIRPLANE_MODE_CHANGED );
	 intent.putExtra( "state", 1 );
	 hopdroid.service.sendBroadcast( new Intent( "android.intent.action.AIRPLANE_MODE" ) );
	 hopdroid.service.sendBroadcast( intent );
	 android.provider.Settings.System.putInt(
	    hopdroid.service.getContentResolver(),
	    android.provider.Settings.System.AIRPLANE_MODE_ON,
	    0 );

	 intent.putExtra( "state", 0 );
	 hopdroid.service.sendBroadcast( new Intent( "android.intent.action.AIRPLANE_MODE" ) );
	 hopdroid.service.sendBroadcast( intent );

	 if( hopdroid.activity != null ) {
	    hopdroid.activity.finishActivity( ca );
	 }
	 Log.d( "HopPluginCall", "Activity finished" );
      }
   }
   
   // writeCallLogList
   void writeCallLogList( final OutputStream op, int i ) throws IOException {
      // Run query
      final String[] projection = new String[] {
	 Calls.TYPE,
	 Calls.NUMBER,
	 Calls.DATE,
	 Calls.DURATION,
	 Calls.CACHED_NAME,
      };
      String order = Calls.DATE + " DESC";
      String limit = (i > 0) ? (order + " LIMIT " + i) : order;
      
      ContentResolver cr = hopdroid.service.getContentResolver();
      Cursor cur = cr.query( Calls.CONTENT_URI, projection, null, null, limit );

      if( cur.moveToFirst() ) {
	 
	 op.write( "(".getBytes() );
	 do {
	    op.write( "(".getBytes() );
	    
	    // type
	    switch( cur.getInt( 0 ) ) {
	       case Calls.INCOMING_TYPE: 
		  op.write( "incoming".getBytes() );
		  break;
	       case Calls.MISSED_TYPE: 
		  op.write( "missed".getBytes() );
		  break;
	       case Calls.OUTGOING_TYPE: 
		  op.write( "outgoing".getBytes() );
		  break;
	       default:
		  op.write( "unknown".getBytes() );
		  break;
	    }
	    op.write( " ".getBytes() );
	    
	    // number
	    op.write( "\"".getBytes() );
	    op.write( cur.getString( 1 ).getBytes() );
	    op.write( "\" ".getBytes() );
	    
	    // date
	    op.write( "#e".getBytes() );
	    op.write( (Long.toString( cur.getLong( 2 ) / 1000 )).getBytes() );
	    op.write( " ".getBytes() );
	    
	    // duration
	    op.write( cur.getString( 3 ).getBytes() );
	    op.write( " ".getBytes() );

	    // cached name
	    String cn = cur.getString( 4 );
	    byte[] cnb = (cn == null) ? "\"\"".getBytes() : cn.getBytes();
	    op.write( cnb );
	    op.write( ")".getBytes() );
	    
	 } while( cur.moveToNext() );
	 op.write( ")".getBytes() );
      } else {
	 op.write( "()".getBytes() );
      }
   }
}
