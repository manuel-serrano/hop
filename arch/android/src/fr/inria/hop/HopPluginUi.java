/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopPluginUi.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Thu Nov 19 16:14:46 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Dealing with phone Calls                                         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.os.*;
import android.util.Log;
import android.net.Uri;
import android.graphics.*;
import android.view.*;

import java.net.*;
import java.io.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginUi extends HopPlugin {
   Intent ci = null;
   int ca = 0;

   // constructor
   HopPluginUi( HopDroid h, String n ) {
      super( h, n );
   }

    // Ui plugin manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'s':
	    // setStatusBarColor
	    String color = HopDroid.read_string( ip );
	    setStatusBarColor( hopdroid.activity, color );
	    break;
	    
	 case (byte)'g':
	    // getStatusBarColor
	    getStatusBarColor( op, hopdroid.activity );
	    break;
      }
   }

   // setStatusBarColor
   protected static void setStatusBarColor( Activity a, String color ) {
      Window window = a.getWindow();

      // clear FLAG_TRANSLUCENT_STATUS flag:
      window.clearFlags( WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS );

      // add FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS flag to the window
      window.addFlags( WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS );

      // finally change the color
      Log.d( "HopUiUtils", "setStatusBarColor: " + color );
      window.setStatusBarColor( Color.parseColor( color ) );
   }

   // getStatusBarColor
   protected static void getStatusBarColor( final OutputStream op, Activity a )
      throws IOException {
      Window window = a.getWindow();
      
      int c = window.getStatusBarColor();
      String hex = String.format( "#%02x%02x%02x", (c >> 16) & 0xff, (c >> 8) & 0xff, c & 0xff );

      op.write( "\"".getBytes() );
      op.write( hex.getBytes() );
      op.write( "\"".getBytes() );
   }
}
