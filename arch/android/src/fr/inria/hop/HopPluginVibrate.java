/*=====================================================================*/
/*    .../hop/arch/android/src/fr/inria/hop/HopPluginVibrate.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 14 08:47:19 2010                          */
/*    Last change :  Thu May 21 08:40:44 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
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
   HopPluginVibrate( HopDroid h, String n ) {
      super( h, n );
   }

   // get the system vibrator
   private Vibrator make_vibrator() {
      return (Vibrator)hopdroid.service.getSystemService( Context.VIBRATOR_SERVICE );
   }
   
   // vibrator player
   protected void server( InputStream ip, OutputStream op )
      throws IOException {
      switch( HopDroid.read_int( ip ) ) {
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
	    
	    Lod.d( "HopPluginVibrate", "vibrate ms=" + ms );
	    vibrator.vibrate( ms );
	    
	 case (byte) 'p':
	    // pattern vibrate
	    long[] vibs = HopDroid.read_int64v( ip );
	    int repeat = HopDroid.read_int32( ip );
	    
	    if( vibrator == null ) vibrator = make_vibrator();

	    Lod.d( "HopPluginVibrate", "vibrate vibs[] repeat=" + repeat );
	    vibrator.vibrate( vibs, repeat );
      }
   }
}
	    
