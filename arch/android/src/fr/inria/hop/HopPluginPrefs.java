/*=====================================================================*/
/*    .../hop/arch/android/src/fr/inria/hop/HopPluginPrefs.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jul 17 13:19:20 2016                          */
/*    Last change :  Sun May 17 10:27:00 2020 (serrano)                */
/*    Copyright   :  2016-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Android preferences plugins                                      */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.content.*;
import android.content.res.*;
import android.preference.*;
import android.util.Log;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginPrefs extends HopPlugin {
   // instance variables
   final Resources res;
   final SharedPreferences sp;
   
   // constructor
   public HopPluginPrefs( HopDroid h, String n ) {
      super( h, n );

      res = h.activity.getResources();
      sp = PreferenceManager.getDefaultSharedPreferences( h.activity );
   }

   // kill
   public void kill() {
      super.kill();
   }

   void get( OutputStream op, InputStream ip ) throws IOException {
      String key = HopDroid.read_string( ip );
      
      Log.d( "HopPluginPrefs", "get key=" + key + " -> " + sp.getString( key, "" ) + " in=" + sp.contains( key ) );
      if( sp.contains( key ) ) {
	 op.write( "\"".getBytes() );
	 op.write( sp.getString( key, "" ).getBytes() );
	 op.write( "\"".getBytes() );
      } else {
	 op.write( "#f ".getBytes() );
      }
   }
   
   void set( OutputStream op, InputStream ip ) throws IOException {
      SharedPreferences.Editor ed = sp.edit();
      String key = HopDroid.read_string( ip );
      String val = HopDroid.read_string( ip );
      
      Log.d( "HopPluginPrefs", "set key=" + key + " val=" + val );
      ed.putString( key, val );
      ed.commit();
   }
   
   // server
   void server( InputStream ip, OutputStream op ) throws IOException {
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'s':
	    // set
	    set( op, ip );
	    return;
	 case (byte)'g':
	    // get
	    get( op, ip );
	    return;
      }
   }
}
