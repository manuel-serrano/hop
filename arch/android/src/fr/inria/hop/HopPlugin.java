/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopPlugin.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct 19 09:38:21 2010                          */
/*    Last change :  Sun Jul  8 07:59:48 2012 (serrano)                */
/*    Copyright   :  2010-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Root class for HopPlugins                                        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.Intent;
import android.util.Log;

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
   public HopDroid hopdroid;
   public String name;

   public HopPlugin( HopDroid h, String n ) {
      Log.v( "HopPlugin", "creating plugin: " + n );
      hopdroid = h;
      name = n;
   }

   // cleanup
   public void kill() {
      Log.v( "HopPlugin", "killing plugin: " + name );
   }
   
   // the server
   abstract void server( InputStream ip, OutputStream op ) throws IOException;

   // onActivityResult (called by HopLauncher)
   static public void onActivityResult( int key, int result, Intent intent ) {
      Log.v( "HopPlugin", "onActivityResult key=" + key + " result=" + result + " intent=" + intent );
      synchronized( atable ) {
	 HopPlugin p = (HopPlugin)atable.get( key );

	 if( p != null ) {
	    atable.remove( key );
	    p.onHopActivityResult( result, intent );
	 }
      }
   }

   // getKey
   private synchronized int getKey() {
      return key++;
   }
   
   // startHopActivityForResult
   public int startHopActivityForResult( Intent intent ) {
      int key = getKey();
      
      synchronized( atable ) {
	 atable.put( key, this );
      }
      
      Log.v( "HopPlugin", "Starting activity key=" + key + " intent=" + intent
	 + " activity=" + hopdroid.activity );
      hopdroid.activity.startActivityForResult( intent, key );

      return key;
   }

   // onHopActivityResult (super method of plugins)
   public void onHopActivityResult( int result, Intent intent ) {
      ;
   }
}
