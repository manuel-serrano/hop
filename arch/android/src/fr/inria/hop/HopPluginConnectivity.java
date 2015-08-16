/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginConnectivity.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jul  5 17:42:47 2012                          */
/*    Last change :  Tue Jul 10 15:17:51 2012 (serrano)                */
/*    Copyright   :  2012 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Dealing with network connections.                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;
 
import android.app.*;
import android.content.*;
import android.util.Log;
import android.net.*;

import java.io.*;
import java.util.*;

// http://developer.android.com/reference/android/net/ConnectivityManager.html

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginConnectivity extends HopPlugin {
   // instance variables
   int count = 0;
   BroadcastReceiver receiver = null;
   
   // constructor
   public HopPluginConnectivity( HopDroid h, String n ) {
      super( h, n );
   }

   // kill
   public void kill() {
      super.kill();

      if( receiver != null ) hopdroid.service.unregisterReceiver( receiver );
   }
   
   // network manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'c':
	    final ConnectivityManager conn_manager =
	       (ConnectivityManager)hopdroid.service.getSystemService( Context.CONNECTIVITY_SERVICE );
	    NetworkInfo i = conn_manager.getActiveNetworkInfo();
	    
	    // push the current state
	    Log.d( "HopPluginConnectivity",
		   "(" + i.getTypeName() + " " + i.getType() + " "
		   + i.isConnected() + ")" );
	    hopdroid.pushEvent( "connectivity", "(" + i.getTypeName() + " " + i.getType() + " "
				+ i.isConnected() + ")" );
	    
	    // register connectivity listener
	    if( count++ == 0 ) {
	       receiver = new BroadcastReceiver() {
		     @Override
		     public void onReceive( Context arg0, Intent intent ) {
			Log.d( "HopPluginConnectivity", "intent=" + intent );
			
			final NetworkInfo info = conn_manager.getActiveNetworkInfo();

			if( info != null ) {
			   Log.d( "HopPluginConnectivity", "info=" + info );
			   Log.d( "HopPluginConnectivity", "push (" + info.getTypeName() + " " + info.getSubtypeName() + " "
					       + info.isConnected() + ")" );

			   hopdroid.pushEvent( "connectivity", "(" + info.getTypeName() + " " + info.getSubtypeName() + " "
					       + info.isConnected() + ")" );
			} else {
			   hopdroid.pushEvent( "connectivity", "(down)" );
			   Log.d( "HopPluginConnectivity", "info(null)" );
			}
		     }
		  };
	       
	       // first time we are called, install the receiver
	       hopdroid.service.registerReceiver( receiver, new IntentFilter( ConnectivityManager.CONNECTIVITY_ACTION ) );
	    }
	    break;
	       
	 case (byte)'e':
	    if( --count == 0 ) {
	       // unregister the recevier
	       if( receiver != null ) {
		  hopdroid.service.unregisterReceiver( receiver );
	       }
	    }
	    break;
      }
   }
}
