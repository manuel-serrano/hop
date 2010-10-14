/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopAndroidVibrator.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 14 08:47:19 2010                          */
/*    Last change :  Thu Oct 14 09:18:10 2010 (serrano)                */
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
import android.net.*;

import java.net.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopAndroidVibrator {
   // instance variables
   Activity activity;
   Vibrator vibrator = null;
   
   // constructor
   public HopAndroidVibrator( Activity a ) {
      activity = a;
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
	    long ms = HopAndroid.read_int64( ip );
	    
	    if( vibrator == null ) vibrator = make_vibrator();
	    
	    vibrator.vibrate( ms );
	    
	 case (byte) 'p':
	    // pattern vibrate
	    int sz = HopAndroid.read_int32( ip );
	    long[] vibs = new long[ sz ];
	    for( int i = 0; i < sz; i++ ) {
	       vibs[ i ] = HopAndroid.read_int64( ip );
	       Log.v( "HopAndroidVibrator", "vibs[" + i + "]=" + vibs[ i ] );
	    }
	    int repeat = HopAndroid.read_int32( ip );
	    
	    if( vibrator == null ) vibrator = make_vibrator();
	    
	    Log.v( "HopAndroidVibrator", "vibrate repeat=" + repeat );
	    vibrator.vibrate( vibs, repeat );
      }
   }
}
	    
