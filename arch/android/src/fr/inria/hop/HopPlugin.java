/*=====================================================================*/
/*    .../hop/2.2.x/arch/android/src/fr/inria/hop/HopPlugin.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct 19 09:38:21 2010                          */
/*    Last change :  Sat Oct 23 08:09:42 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Root class for HopPlugins                                        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public abstract class HopPlugin {
   // static variables
   static private key = 1000;
   static private Hashtable atable = new Hashtable();
   
   // instance variables
   public HopAndroid handroid;
   public Activity activity;
   public String name;

   public HopPlugin( HopAndroid h, Activity a, String n ) {
      handroid = h;
      activity = a;
      name = n;
   }

   // the server
   abstract void server( InputStream ip, OutputStream op ) throws IOException;

   // onActivityResult (called by HopLauncher)
   static public void onActivityResult( int key, int result, Intent intent ) {
      synchronized( atable ) {
	 HopPlugin p = (HopPlug)atable.get( key );

	 if( p != null ) {
	    atable.delete( p );
	    p.onHopActivityresult( result, intent );
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
   void startHopActivityForResult( Intent intent ) {
      int key = getKey();
      activity.startActivityForResult( key, Intent intent );
      synchronized( atable ) {
	 atable.put( key, this );
      }
   }

   // onHopActivityResult (super method of plugins)
   public void onHopActivityResult( result, intent ) {
      ;
   }
}
