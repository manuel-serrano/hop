/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopConfig.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jul 10 08:08:45 2016                          */
/*    Last change :  Sat Nov 21 08:47:22 2020 (serrano)                */
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
   static final String PORT;
   static final String ROOT;
   static final String MAXTHREADS;
   static final String URL;
   static final String BIGLOORELEASE;
   static final String HOPRELEASE;
   static final String HOPAPK;
   static final String HOPHZ;
   static final String APP;
   static final String SERVICE;
   static final String ARGS;
   static final String DEBUG;

   static final boolean NOTITLE;
   static final boolean CUSTOMTITLE;

   static final String STATUSBARCOLOR;

   static final boolean PLUGINBUILD;
   static final boolean PLUGINLOCALE;
   static final boolean PLUGINVIBRATE;
   static final boolean PLUGINMUSICPLAYER;
   static final boolean PLUGINMEDIAAUDIO;
   static final boolean PLUGINSENSOR;
   static final boolean PLUGINBATTERY;
   static final boolean PLUGINSMS;
   static final boolean PLUGINWIFI;
   static final boolean PLUGINCONNECTIVITY;
   static final boolean PLUGINCONTACT;
   static final boolean PLUGINZEROCONF;
   static final boolean PLUGINSYSTEM;
   static final boolean PLUGINTTS;
   static final boolean PLUGINCALL;
   static final boolean PLUGINPREFS;

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
      APP = res.getString( R.string.hopapp );
      ARGS = res.getString( R.string.hopargs );
      SERVICE = APP.equals( "hop" ) ? "/hop" : "/hop/" + APP;

      NOTITLE = res.getBoolean( R.bool.notitle );
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
   }

   HopConfig() {
      ;
   }
}
