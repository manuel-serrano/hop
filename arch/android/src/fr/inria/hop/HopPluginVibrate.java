/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginVibrate.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 14 08:47:19 2010                          */
/*    Last change :  Mon Oct 25 10:17:41 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Android Vibrator                                                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.os.*;
import android.util.Log;
import android.media.*;

import java.net.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginVibrate extends HopPlugin {
   // instance variables
   Vibrator vibrator = null;
   
   // constructor
   HopPluginVibrate( HopDroid h, Activity a, String n ) {
      super( h, a, n );
   }

   // get the system vibrator
   private Vibrator make_vibrator() {
      return (Vibrator)activity.getSystemService( Context.VIBRATOR_SERVICE );
   }
   
   // vibrator player
   protected void server( InputStream ip, OutputStream op )
      throws IOException {
      switch( ip.read() ) {
	 case (byte)'x':
	    // reset
	    if( vibrator != null ) vibrator.cancel();
	    vibrator = null;
	    break;
	       
	 case (byte)'e':
	    // stop
	    if( vibrator != null ) vibrator.cancel();
	    return;

	 case (byte) 'b':
	    // vibrate
	    long ms = HopDroid.read_int64( ip );
	    
	    if( vibrator == null ) vibrator = make_vibrator();
	    
	    vibrator.vibrate( ms );
	    
	 case (byte) 'p':
	    // pattern vibrate
	    int sz = HopDroid.read_int32( ip );
	    long[] vibs = new long[ sz ];
	    for( int i = 0; i < sz; i++ ) {
	       vibs[ i ] = HopDroid.read_int64( ip );
	    }
	    int repeat = HopDroid.read_int32( ip );
	    
	    if( vibrator == null ) vibrator = make_vibrator();
	    
	    vibrator.vibrate( vibs, repeat );
      }
   }
}
	    
