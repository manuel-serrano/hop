/*=====================================================================*/
/*    .../hop/2.2.x/arch/android/src/fr/inria/hop/HopPlugin.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct 19 09:38:21 2010                          */
/*    Last change :  Mon Oct 25 10:15:35 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Root class for HopPlugins                                        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.Intent;

import java.io.*;
import java.lang.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public abstract class HopPlugin {
   // static variables
   static private int key = 1000;
   static private Hashtable atable = new Hashtable();
   
   // instance variables
   public HopDroid handroid;
   public Activity activity;
   public String name;

   public HopPlugin( HopDroid h, Activity a, String n ) {
      handroid = h;
      activity = a;
      name = n;
   }

   // the server
   abstract void server( InputStream ip, OutputStream op ) throws IOException;

   // onActivityResult (called by HopLauncher)
   static public void onActivityResult( int key, int result, Intent intent ) {
      synchronized( atable ) {
	 HopPlugin p = (HopPlugin)atable.get( key );

	 if( p != null ) {
	    atable.remove( key );
	    p.onHopActivityResult( result, intent );
	 }
      }
   }

   // getKey
   private int getKey() {
      synchronized( activity ) {
	 return key++;
      }
   }
   
   // startHopActivityForResult
   public void startHopActivityForResult( Intent intent ) {
      int key = getKey();
      activity.startActivityForResult( intent, key );
      synchronized( atable ) {
	 atable.put( key, this );
      }
   }

   // onHopActivityResult (super method of plugins)
   public void onHopActivityResult( int result, Intent intent ) {
      ;
   }
}
