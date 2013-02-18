/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopPluginSystem.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov 21 08:34:30 2012                          */
/*    Last change :  Mon Feb 18 09:01:25 2013 (serrano)                */
/*    Copyright   :  2012-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Android system settings                                          */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.preference.*;
import android.os.*;
import android.util.Log;
import android.content.*;
import android.provider.*;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginSystem extends HopPlugin {
   
   // constructor
   public HopPluginSystem( HopDroid h, String n ) {
      super( h, n );
   }

   // kill
   public void kill() {
      super.kill();
   }
   
   // server
   void server( InputStream ip, OutputStream op ) throws IOException {
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'w':
	    // wifi policy
	    writeWifiPolicy( op );
	    return;
	 case (byte)'W':
	    // wifi policy
	    setWifiPolicy( op, HopDroid.read_string( ip ) );
	    return;
	 case (byte)'r':
	    // accelerometer rotation
	    writeAccelerometerRotation( op );
	    return;
	 case (byte)'R':
	    // accelerometer rotation
	    setAccelerometerRotation( op, HopDroid.read_int( ip ) );
	    return;
      }
   }

   // wifi policy
   void writeWifiPolicy( OutputStream op ) throws IOException {
      ContentResolver cr = hopdroid.service.getContentResolver();

      try {
	 switch( Settings.System.getInt( cr, Settings.System.WIFI_SLEEP_POLICY ) ) {
	    case Settings.System.WIFI_SLEEP_POLICY_NEVER:
	       op.write( "never".getBytes() );
	       return;
	    case Settings.System.WIFI_SLEEP_POLICY_NEVER_WHILE_PLUGGED:
	       op.write( "never-while-plugged".getBytes() );
	       return;
	    case Settings.System.WIFI_SLEEP_POLICY_DEFAULT:
	       op.write( "default".getBytes() );
	       return;
	    default:
	       op.write( "unknown".getBytes() );
	       return;
	 }
      } catch( android.provider.Settings.SettingNotFoundException _ ) {
	 op.write( "unknown".getBytes() );
	 return;
      }
   }

   void setWifiPolicy( OutputStream op, String policy ) throws IOException {
      if( policy.equals( "never" ) ) {
	 setWifiPolicy( Settings.System.WIFI_SLEEP_POLICY_NEVER );
	 op.write( "#t".getBytes() );
	 return;
      }

      if( policy.equals( "never-while-plugged" ) ) {
	 setWifiPolicy( Settings.System.WIFI_SLEEP_POLICY_NEVER_WHILE_PLUGGED );
	 op.write( "#t".getBytes() );
	 return;
      }

      if( policy.equals( "default" ) ) {
	 setWifiPolicy( Settings.System.WIFI_SLEEP_POLICY_DEFAULT );
	 op.write( "#t".getBytes() );
	 return;
      }

      if( policy.equals( "unknown" ) ) {
	 op.write( "#t".getBytes() );
	 return;
      }

      op.write( "#f".getBytes() );
   }
   
   private void setWifiPolicy( int policy ) throws IOException {
      ContentResolver cr = hopdroid.service.getContentResolver();
      
      Settings.System.putInt( cr, Settings.System.WIFI_SLEEP_POLICY, policy );
   }
   
   // Accelerometer Rotation
   void writeAccelerometerRotation( OutputStream op ) throws IOException {
      ContentResolver cr = hopdroid.service.getContentResolver();

      try {
	 if( Settings.System.getInt( cr, Settings.System.ACCELEROMETER_ROTATION ) == 1 ) {
	    op.write( "#t".getBytes() );
	 } else {
	    op.write( "#f".getBytes() );
	 }
      } catch( android.provider.Settings.SettingNotFoundException _ ) {
	 op.write( "#t".getBytes() );
	 return;
      }
   }

   void setAccelerometerRotation( OutputStream op, int enabled ) throws IOException {
      ContentResolver cr = hopdroid.service.getContentResolver();

      try {
	 Settings.System.putInt( cr, Settings.System.ACCELEROMETER_ROTATION, enabled );
      } catch( Throwable _ ) {
	 ;
      }
   }
}
