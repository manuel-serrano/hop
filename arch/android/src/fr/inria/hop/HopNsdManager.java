/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopNsdManager.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  7 14:10:47 2012                          */
/*    Last change :  Wed Nov  7 15:01:16 2012 (serrano)                */
/*    Copyright   :  2012 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    The NsdManager (zeroconf) Hop binding                            */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.os.*;
import android.util.Log;
import android.app.*;
import android.content.*;

import android.net.wifi.WifiInfo;
import android.net.wifi.WifiManager;
import android.net.wifi.WifiManager.*;
import android.net.nsd.*;

import java.net.InetAddress;
import java.util.*;



/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopNsdManager extends HopZeroconf {
   static Class nsdinfoclass;
   
   Object nsd;

   // availability
   public static boolean isAvailable() {
      try {
	 nsdinfoclass = Class.forName( "android.net.nsd.NsdManager" );

	 return true;
      } catch( ClassNotFoundException e ) {
	 return false;
      }
   }
   
   // constructor
   public HopNsdManager( HopDroid h ) {
      super( h );
   }
   
   public void start() {
      if( nsd == null ) {
	 nsd = hopdroid.service.getSystemService( Context.NSD_SERVICE ); 
      }
   }
   
   public void stop() {
      ;
   }
   
   public String version() {
      return "NsdManager";
   }
   
   public void addServiceTypeListener( final String utype, final String type, final String event ) {
      ;
   }
   
   public void addServiceListener() {
      ;
   }
   
   public void addTypeListener( final String type ) {
      ;
   }
   
   public void publish( final String name, final int port, final String type, final String[] props ) {
      ;
   }
}
