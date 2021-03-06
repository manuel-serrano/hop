/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopConfig.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jul 10 08:08:45 2016                          */
/*    Last change :  Thu Dec 31 09:19:52 2020 (serrano)                */
/*    Copyright   :  2016-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop configuration                                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.os.*;
import android.util.Log;
import android.content.*;
import android.widget.TextView;
import android.content.res.*;
import android.content.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopConfig {
   static String PORT;
   static String ROOT;
   static String MAXTHREADS;
   static String URL;
   static String BIGLOORELEASE;
   static String HOPRELEASE;
   static String HOPAPK;
   static String HOPHZ;
   static String APP;
   static String SERVICE;
   static String ARGS;
   static String DEBUG;
   static String VERBOSE;

   static String DEFLANG;
   static boolean NOTITLE;
   static boolean CUSTOMTITLE;

   static String UISTATUSBARCOLOR;

   static boolean PLUGINBUILD;
   static boolean PLUGINLOCALE;
   static boolean PLUGINVIBRATE;
   static boolean PLUGINMUSICPLAYER;
   static boolean PLUGINMEDIAAUDIO;
   static boolean PLUGINSENSOR;
   static boolean PLUGINBATTERY;
   static boolean PLUGINSMS;
   static boolean PLUGINWIFI;
   static boolean PLUGINCONNECTIVITY;
   static boolean PLUGINCONTACT;
   static boolean PLUGINZEROCONF;
   static boolean PLUGINSYSTEM;
   static boolean PLUGINTTS;
   static boolean PLUGINCALL;
   static boolean PLUGINPREFS;
   static boolean PLUGININTENT;

   static String HOME;
   static String RCDIR;

   static void init( Context context ) {
      final Resources res = context.getResources();
      
      PORT = res.getString( R.string.hopport );
      ROOT = res.getString( R.string.hoproot );
      MAXTHREADS = res.getString( R.string.hopthreads );
      URL = res.getString( R.string.hopapp );
      BIGLOORELEASE = res.getString( R.string.bigloorelease );
      HOPRELEASE = res.getString( R.string.hoprelease );
      HOPAPK = res.getString( R.string.hopapk );
      HOPHZ = res.getString( R.string.hophz );
      DEBUG = res.getString( R.string.hopdebug );
      VERBOSE = res.getString( R.string.hopverbose );
      APP = res.getString( R.string.hopapp );
      ARGS = res.getString( R.string.hopargs );
      SERVICE = APP.equals( "hop" ) ? "/hop/hopdroid" : "/hop/" + APP;

      UISTATUSBARCOLOR = res.getString( R.string.statusbarcolor );
      NOTITLE = res.getBoolean( R.bool.notitle );
      DEFLANG = res.getString( R.string.deflang );
      CUSTOMTITLE = res.getBoolean( R.bool.customtitle );
	 
      PLUGINBUILD = res.getBoolean( R.bool.pluginbuild );
      PLUGINLOCALE = res.getBoolean( R.bool.pluginlocale );
      PLUGINVIBRATE = res.getBoolean( R.bool.pluginvibrate );
      PLUGINMUSICPLAYER = res.getBoolean( R.bool.pluginmusicplayer );
      PLUGINMEDIAAUDIO = res.getBoolean( R.bool.pluginmediaaudio );
      PLUGINSENSOR = res.getBoolean( R.bool.pluginsensor );
      PLUGINBATTERY = res.getBoolean( R.bool.pluginbattery );
      PLUGINSMS = res.getBoolean( R.bool.pluginsms );
      PLUGINWIFI = res.getBoolean( R.bool.pluginwifi );
      PLUGINCONNECTIVITY = res.getBoolean( R.bool.pluginconnectivity );
      PLUGINCONTACT = res.getBoolean( R.bool.plugincontact );
      PLUGINZEROCONF = res.getBoolean( R.bool.pluginzeroconf );
      PLUGINSYSTEM = res.getBoolean( R.bool.pluginsystem );
      PLUGINTTS = res.getBoolean( R.bool.plugintts );
      PLUGINCALL = res.getBoolean( R.bool.plugincall );
      PLUGINPREFS = res.getBoolean( R.bool.pluginprefs );
      PLUGININTENT = res.getBoolean( R.bool.pluginintent );

      HOME = context.getExternalFilesDir( null ).getAbsolutePath();
      RCDIR = context.getExternalFilesDir( null ).getAbsolutePath() + "/rcdir";
   }

   HopConfig() {
      ;
   }
}
