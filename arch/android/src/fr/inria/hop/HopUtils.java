/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopUtils.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Thu Nov 12 16:24:37 2020                          */
/*    Last change :  Fri Nov 13 10:55:28 2020 (serrano)                */
/*    Copyright   :  2020 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    Utility functions                                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import java.io.*;
import java.lang.reflect.*;

import android.app.*;
import android.content.DialogInterface;
import android.util.Log;
import android.media.AudioManager;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopUtils {
   // wifi policy constants
   protected static String WIFI_SLEEP_POLICY = null;
   protected static int WIFI_SLEEP_POLICY_NEVER = -1;

   // findSystemClass
   static Class findSystemClass( String name ) {
      try {
	 Class clazz = Class.forName( name );
	 Class[] clazzes = clazz.getClasses();
	 String global_name = clazz.getName() + "$Global";
	 String system_name = clazz.getName() + "$System";

	 for( int i = 0; i < clazzes.length; i++ ) {
	    if( clazzes[ i ].getName().equals( global_name ) ) {
	       return clazzes[ i ];
	    }
	    if( clazzes[ i ].getName().equals( system_name ) ) {
	       return clazzes[ i ];
	    }
	 }
      } catch( ClassNotFoundException c ) {
	 ;
      }

      return null;
   }

   // initWifiPolicy
   static void initWifiPolicy() {
      // bind the WIFI_SLEEP constants
      Class clazz = findSystemClass( "android.provider.Settings.Global" );

      if( clazz == null ) {
	 clazz = findSystemClass( "android.provider.Settings" );
      }

      try {
	 Field fs = clazz.getField( "WIFI_SLEEP_POLICY" );
	 WIFI_SLEEP_POLICY = (String)fs.get( clazz );
	 
	 Field fi = clazz.getField( "WIFI_SLEEP_POLICY_NEVER" );
	 WIFI_SLEEP_POLICY_NEVER = fi.getInt( clazz );
      } catch( Throwable e3 ) {
	 // Fall back API < 17
	 WIFI_SLEEP_POLICY =
	    android.provider.Settings.System.WIFI_SLEEP_POLICY;
	 WIFI_SLEEP_POLICY_NEVER =
	    android.provider.Settings.System.WIFI_SLEEP_POLICY_NEVER;
      }
   }

   // initAudio
   static void initAudioVolume( Activity a ) {
      // control the volume key when the console has the focus
      a.setVolumeControlStream( android.media.AudioManager.STREAM_MUSIC );
   }

   // chmod
   static void chmod( String abspath, int mode ) {
      try {
	 Runtime.getRuntime().exec( "/system/bin/chmod " + mode + " " + abspath );
      } catch( IOException e ) {
	 Log.d( "HopUtils", "chmod \"" + abspath + "\" failed..." );
	 e.printStackTrace();
      }
   }
}
