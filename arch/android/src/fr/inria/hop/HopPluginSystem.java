/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopPluginSystem.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov 21 08:34:30 2012                          */
/*    Last change :  Thu Nov 22 17:03:19 2012 (serrano)                */
/*    Copyright   :  2012 Manuel Serrano                               */
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
	    setWifiPolicy( op, read_string( ip ) );
	    return;
      }
   }

   // wifi policy
   void writeWifiPolicy( OutputStream op ) {
      switch( Settings.System.getInt( getContentResolver(), Settings.System.WIFI_SLEEP_POLICY ) ) {
	 case Settings.System.WIFI_SLEEP_POLICY_NEVER:
	    op.write( "'never".getBytes() );
	 default:
	    op.write( "'unknown".getBytes() );
      }
   }

   void setWifiPolicy( OutputStream op, String policy ) {
      if( policy.equal( "never" ) ) {
	 setWifiPolicy( Settings.System.WIFI_SLEEP_POLICY_NEVER );
	 op.write( "#t" );
      }

      op.write( "#f" );
   }
   
   private void setWifiPolicy( int policy ) {
      Settings.System.putInt(
	 getContentResolver(), Settings.System.WIFI_SLEEP_POLICY, policy );
   }
}
