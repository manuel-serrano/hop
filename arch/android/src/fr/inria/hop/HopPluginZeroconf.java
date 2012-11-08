/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginZeroconf.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 22 10:05:43 2010                          */
/*    Last change :  Thu Nov  8 08:51:21 2012 (serrano)                */
/*    Copyright   :  2010-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    jmdns Bonjour implementation (http://jmdns.sourceforge.net)      */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.util.Log;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginZeroconf extends HopPlugin {
   boolean inkill = false;
   HopZeroconf zeroconf = null;
   
   // constructor
   public HopPluginZeroconf( HopDroid h, String n ) {
      super( h, n );

      zeroconf = getZeroconf( h );
   }

   // kill
   public synchronized void kill() {
      if( !inkill ) {
	 zeroconf.stop();
	 inkill = true;
      }
      super.kill();
   }

   // getZeroconf, find the most efficient zeroconf available backend
   HopZeroconf getZeroconf( HopDroid h ) {
      if( isNsdAvailable() ) {
	 Log.d( "HopPluginZeroconf", "NSDMANAGER" );
	 return new HopNsdManager( h );
      } else {
	 Log.d( "HopPluginZeroconf", "JMDNS" );
	 return new HopJmDns( h );
      }
   }
   
   // availability
   public static boolean isNsdAvailable() {
      try {
	 Class.forName( "android.net.nsd.NsdManager" );

	 return true;
      } catch( ClassNotFoundException e ) {
	 return false;
      }
   }
   
   // server
   void server( InputStream ip, OutputStream op ) throws IOException {
      switch( HopDroid.read_int( ip ) ) {
	 // version
	 case (byte)'v':
	    op.write( "\"".getBytes() );
	    op.write( zeroconf.version().getBytes() );
	    op.write( "\"".getBytes() );
	    return;
	       
	 // begin
	 case (byte)'s':
	    zeroconf.start();
	    
	    op.write( "#t".getBytes() );
	    return;
	    
	 // end
	 case (byte)'e':
	    zeroconf.stop();
	    
	    op.write( "#f".getBytes() );
	    return;

	 // publish
	 case (byte)'p':
	    publish( ip );
	    return;
	    
	 // serviceListener
	 case (byte)'l':
	    zeroconf.addServiceListener();
	    return;
	    
	 // typeListener
	 case (byte)'t':
	    zeroconf.addTypeListener( HopDroid.read_string( ip ) );
	    return;
      }
   }

   void publish( InputStream ip ) throws IOException {
      if( !inkill ) {
	 final String name = HopDroid.read_string( ip );
	 final int port = HopDroid.read_int32( ip );
	 final String type = HopDroid.read_string( ip );
	 final String[] props = HopDroid.read_stringv( ip );

	 zeroconf.publish( name, port, type, props );
      }
   }
}
