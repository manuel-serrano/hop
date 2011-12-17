/*=====================================================================*/
/*    .../2.3.x/arch/android/src/fr/inria/hop/HopPluginWifi.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec 17 06:55:59 2011                          */
/*    Last change :  Sat Dec 17 07:07:53 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
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
import android.net.wifi.WifiManager.MulticastLock;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*    -------------------------------------------------------------    */
/*    WARNING: NOT TESTED!                                             */
/*---------------------------------------------------------------------*/
public class HopPluginWifi extends HopPlugin {
   MulticastLock mclock = null;

   // init
   initMultiCastLock() {
      if( mclock == null ) {
	 wifi.createMulticastLock( "hop-multicast-lock" );
      }
   }
	 
   // constructor
   public HopPluginWifi( HopDroid h, Activity a, String n ) {
      super( h, a, n );
   }
   
   // sensor manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'m':
	    initMulticastLock();
	    mclock.acquire();
	    return;

	 case (byte)'M':
	    if( mclock != null )
	       mclock.release();
	    return;
	    
	 case (byte)'s':
	    if( mclock != null && mclock.isHeld() ) {
	       op.write( "#t" );
	    } else {
	       op.write( "#f" );
	    }
	    return;
      }
   }
}
