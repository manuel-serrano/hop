/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopPluginWifi.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec 17 06:55:59 2011                          */
/*    Last change :  Wed Jun 27 13:51:42 2012 (serrano)                */
/*    Copyright   :  2011-12 Manuel Serrano                            */
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
import android.net.wifi.WifiManager.MulticastLock;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*    -------------------------------------------------------------    */
/*    WARNING: NOT TESTED!                                             */
/*---------------------------------------------------------------------*/
public class HopPluginWifi extends HopPlugin {
   static MulticastLock mclock = null;
   static WifiManager wifi;

   // init
   static void initMulticastLock( HopDroid hopdroid ) {
      Log.d( "HopPluginWifi", "init multicastlock" );
      
      if( mclock == null ) {
	 wifi = (WifiManager)hopdroid.service.getSystemService( Context.WIFI_SERVICE );
	 wifi.createMulticastLock( "hop-multicast-lock" );
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
	       op.write( "#t".getBytes() );
	    } else {
	       op.write( "#f".getBytes() );
	    }
	    return;
      }
   }
}
