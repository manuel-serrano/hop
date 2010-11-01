/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginBattery.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Mon Nov  1 14:18:37 2010 (serrano)                */
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
import android.util.Log;
import android.os.BatteryManager;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginBattery extends HopPlugin {
   // instance variables
   int count = 0;
   BroadcastReceiver receiver = null;
   
   // constructor
   public HopPluginBattery( HopDroid h, Activity a, String n ) {
      super( h, a, n );
   }

   // sensor manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( ip.read() ) {
	 case (byte)'b':
	    // send a SMS
	    if( count++ == 0 ) {
	       receiver = new BroadcastReceiver() {
		     @Override
		     public void onReceive( Context arg0, Intent intent ) {
			int scale = intent.getIntExtra( BatteryManager.EXTRA_SCALE, 0 );
	       
			int level = intent.getIntExtra( BatteryManager.EXTRA_LEVEL, 0 );
			int voltage = intent.getIntExtra( BatteryManager.EXTRA_VOLTAGE, 0 );
			String health;
			String plugged;
			String status;

			switch( intent.getIntExtra( BatteryManager.EXTRA_HEALTH, BatteryManager.BATTERY_HEALTH_UNKNOWN ) ) {
			   case BatteryManager.BATTERY_HEALTH_DEAD:
			      health = "dead";
			      break;
			   case BatteryManager.BATTERY_HEALTH_GOOD:
			      health = "good";
			      break;
			   case BatteryManager.BATTERY_HEALTH_OVERHEAT:
			      health = "overheat";
			      break;
			   case BatteryManager.BATTERY_HEALTH_OVER_VOLTAGE:
			      health = "over-voltage";
			      break;
			   case BatteryManager.BATTERY_HEALTH_UNKNOWN:
			      health = "unknown";
			      break;
			   case BatteryManager.BATTERY_HEALTH_UNSPECIFIED_FAILURE:
			      health = "failure";
			      break;
			   default:
			      health = "unknown";
			}

			switch( intent.getIntExtra( BatteryManager.EXTRA_PLUGGED, 0 ) ) {
			   case BatteryManager.BATTERY_PLUGGED_AC:
			      plugged = "ac";
			      break;
			   case BatteryManager.BATTERY_PLUGGED_USB:
			      plugged = "usb";
			      break;
			   default:
			      plugged = "unknown";
			}
			      
			switch( intent.getIntExtra( BatteryManager.EXTRA_STATUS, 0 ) ) {
			   case BatteryManager.BATTERY_STATUS_CHARGING:
			      status = "charging";
			      break;
			   case BatteryManager.BATTERY_STATUS_DISCHARGING:
			      status = "discharging";
			      break;
			   case BatteryManager.BATTERY_STATUS_FULL:
			      status = "full";
			      break;
			   case BatteryManager.BATTERY_STATUS_NOT_CHARGING:
			      status = "not-charging";
			      break;
			   default:
			      status = "not-charging";
			}
			   
			handroid.pushEvent( "battery", "(" + level + " " + scale + " "
					    + status + " " + plugged + " " + health
					    + " " + voltage + ")" );
		     }
		  };
	       
	       // first time we are called, install the receiver
	       activity.registerReceiver(
		  receiver, new IntentFilter( Intent.ACTION_BATTERY_CHANGED ) );
	    }
	    break;
	       
	 case (byte)'e':
	    if( --count == 0 ) {
	       // unregister the recevier
	       if( receiver != null ) {
		  activity.unregisterReceiver( receiver );
	       }
	    }
	    break;
      }
   }
}
