/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopPluginCall.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Mon Jan 10 18:06:07 2011 (serrano)                */
/*    Copyright   :  2010-11 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Dealing with phone Calls                                         */
/*=====================================================================*/

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
		  handroid.pushEvent( "call", "(call-state ringing " + in + " )" );
		  Log.d( "HopPluginCall", "state changed: ringing" );
		  break;
	       case TelephonyManager.CALL_STATE_OFFHOOK:
		  handroid.pushEvent( "call", "(call-state offhook " + in + " )" );
		  Log.d( "HopPluginCall", "state changed: offhook" );
		  break;
	       case TelephonyManager.CALL_STATE_IDLE:
		  handroid.pushEvent( "call", "(call-state idle " + in + " )" );
		  Log.d( "HopPluginCall", "state changed: idle" );
		  break;
	       default:
		  Log.d( "HopPluginCall", "Unknown phone state=" + state );
	    }
	 }
	 
	 public void onDataConnectionStateChanged( int state ) {
	    switch( state ) {
	       case TelephonyManager.DATA_DISCONNECTED:
		  handroid.pushEvent( "call", "(data disconnected)" );
		  Log.d( "HopPluginCall", "data state changed: disconected" );
		  break;
	       case TelephonyManager.DATA_CONNECTING:
		  handroid.pushEvent( "call", "(data connecting)" );
		  Log.d( "HopPluginCall", "data state changed: connecting" );
		  break;
	       case TelephonyManager.DATA_CONNECTED:
		  handroid.pushEvent( "call", "(data connected)" );
		  Log.d( "HopPluginCall", "data state changed: connected" );
		  break;
	       case TelephonyManager.DATA_SUSPENDED:
		  handroid.pushEvent( "call", "(data suspended)" );
		  Log.d( "HopPluginCall", "data state changed: suspended" );
		  break;
	       default:
		  Log.d( "HopPluginCall", "Unknown phone state=" + state );
	    }
	 }
	 
	 public void onServiceStateChanged( ServiceState sstate ) {
	    switch( sstate.getState() ) {
	       case ServiceState.STATE_EMERGENCY_ONLY:
		  handroid.pushEvent( "call", "(state emergency-only)" );
		  Log.d( "HopPluginCall", "state: emergency-only" );
		  break;
	       case ServiceState.STATE_IN_SERVICE:
		  handroid.pushEvent( "call", "(state in-service)" );
		  Log.d( "HopPluginCall", "state: in-service" );
		  break;
	       case ServiceState.STATE_OUT_OF_SERVICE:
		  handroid.pushEvent( "call", "(state out-of-service)" );
		  Log.d( "HopPluginCall", "state: out-of-service" );
		  break;
	       case ServiceState.STATE_POWER_OFF:
		  handroid.pushEvent( "call", "(state power-off)" );
		  Log.d( "HopPluginCall", "state: power-off" );
		  break;
	    }
	 };
      };
   
   // constructor
   HopPluginCall( HopDroid h, Activity a, String n ) {
      super( h, a, n );
   }

    // calllog manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
       switch( ip.read() ) {
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
	    // register call listener
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
	    
	 case (byte)'k':
	    // stop a call
	    stopCall();
	    break;
       }
   }

   // initTelephoneManager
   void initTelephoneManager() {
      if( tm == null ) {
	 tm = (TelephonyManager)activity.getSystemService( Context.TELEPHONY_SERVICE );
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

   // startCall
   void startCall( final InputStream ip, final OutputStream op ) throws IOException {
      String number = HopDroid.read_string( ip );
      boolean newactivity = ip.read() != 0;

      Log.d( "HopPluginCall", "Creating indent" );
      Intent callIntent = new Intent( Intent.ACTION_CALL );
      callIntent.setData( Uri.parse( "tel:" + number ) );

      Log.d( "HopPluginCall", "Intent created..." );

      if( true || newactivity ) {
	 ci = null;
	 ca = startHopActivityForResult( callIntent );
	 Log.d( "HopPluginCall", "Activity started..." );
      } else {
	 
	 ci = callIntent;
	 ca = 0;
	 activity.startService( callIntent );
      }
   }

   // stopCall
   void stopCall() {
      if( ci != null ) {
	 Log.d( "HopPluginCall", "stopping service..." );
	 activity.stopService( ci );
	 Log.d( "HopPluginCall", "service stopped." );
      }
      if( ca > 0 ) {
	 Log.d( "HopPluginCall", "Finishing activity: " + ca );
	 android.provider.Settings.System.putInt(
	    activity.getContentResolver(),
	    android.provider.Settings.System.AIRPLANE_MODE_ON, 1 );

	 Intent intent = new Intent( Intent.ACTION_AIRPLANE_MODE_CHANGED );
	 intent.putExtra( "state", 1 );
	 activity.sendBroadcast( new Intent( "android.intent.action.AIRPLANE_MODE" ) );
	 activity.sendBroadcast( intent );
	 android.provider.Settings.System.putInt(
	    activity.getContentResolver(),
	    android.provider.Settings.System.AIRPLANE_MODE_ON,
	    0 );

	 intent.putExtra( "state", 0 );
	 activity.sendBroadcast( new Intent( "android.intent.action.AIRPLANE_MODE" ) );
	 activity.sendBroadcast( intent );
	 activity.finishActivity( ca );
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
      Uri uri = Calls.CONTENT_URI;
      String order = Calls.DATE + " DESC";
      String limit = (i > 0) ? (order + " LIMIT " + i) : order;
      Cursor cur = activity.managedQuery( uri, projection, null, null, limit );

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
