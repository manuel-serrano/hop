/*=====================================================================*/
/*    .../2.6.x/arch/android/src/fr/inria/hop/HopPluginWifi.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec 17 06:55:59 2011                          */
/*    Last change :  Fri Feb 21 13:31:13 2014 (serrano)                */
/*    Copyright   :  2011-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Dealing with Wifi configuration                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;
 
import android.app.*;
import android.content.*;
import android.os.Bundle;
import android.util.Log;
import android.net.wifi.WifiManager;
import android.net.wifi.WifiManager.*;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*    -------------------------------------------------------------    */
/*    WARNING: NOT TESTED!                                             */
/*---------------------------------------------------------------------*/
public class HopPluginWifi extends HopPlugin {
   static MulticastLock mclock = null;
   static WifiLock wlock = null;
   static WifiManager wifi = null;

   // kill
   public void kill() {
      super.kill();
      
      if( mclock != null ) mclock.release();
      if( wlock != null ) wlock.release();

      wifi = null;
   }

   // init
   static void initWifi( HopDroid hopdroid ) {
      if( wifi == null ) {
	 wifi = (WifiManager)hopdroid.service.getSystemService( Context.WIFI_SERVICE );
      }
   }
      
   static void initMulticastLock( HopDroid hopdroid ) {
      Log.d( "HopPluginWifi", "init multicastlock" );

      if( wifi == null ) {
	 wifi = (WifiManager)hopdroid.service.getSystemService( Context.WIFI_SERVICE );
      }
      
      if( mclock == null ) {
	 mclock = wifi.createMulticastLock( "hop-multicast-lock" );
      }
   }
	 
   // init
   static void initWifiLock( HopDroid hopdroid ) {
      Log.d( "HopPluginWifi", "init wifilock" );

      if( wifi == null ) {
	 wifi = (WifiManager)hopdroid.service.getSystemService( Context.WIFI_SERVICE );
      }
      
      if( wlock == null ) {
	 wlock = wifi.createWifiLock( "hop-wifi-lock" );
      }
   }
	 
   // constructor
   public HopPluginWifi( HopDroid h, String n ) {
      super( h, n );
   }
   
   // sensor manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'m':
	    initMulticastLock( hopdroid );
	    mclock.acquire();
	    return;

	 case (byte)'M':
	    if( mclock != null )
	       mclock.release();
	    return;
	    
	 case (byte)'s':
	    if( mclock != null && mclock.isHeld() ) {
	       op.write( "#t ".getBytes() );
	    } else {
	       op.write( "#f ".getBytes() );
	    }
	    return;
	    
	 case (byte)'w':
	    initWifiLock( hopdroid );
	    wlock.acquire();
	    return;

	 case (byte)'W':
	    if( wlock != null )
	       wlock.release();
	    return;
	    
	 case (byte)'t':
	    if( wlock != null && wlock.isHeld() ) {
	       op.write( "#t ".getBytes() );
	    } else {
	       op.write( "#f ".getBytes() );
	    }
	    return;

	 case (byte)'i':
	    initWifi( hopdroid );
	    op.write( "(wifi ssid: ".getBytes() );
	    op.write( wifi.getConnectionInfo().getSSID().getBytes() );
	    op.write( ")".getBytes() );
	    return;
      }
   }
}
