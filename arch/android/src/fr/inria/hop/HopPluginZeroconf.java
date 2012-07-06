/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginZeroconf.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 22 10:05:43 2010                          */
/*    Last change :  Fri Jul  6 12:38:12 2012 (serrano)                */
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
import android.net.wifi.WifiManager;
import android.net.wifi.WifiManager.MulticastLock;

import java.io.*;
import java.util.*;

import javax.jmdns.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginZeroconf extends HopPlugin {
   JmDNS jmdns = null;
   android.net.wifi.WifiManager.MulticastLock multicast_lock;
   boolean inkill = false;
   
   // constructor
   public HopPluginZeroconf( HopDroid h, String n ) {
      super( h, n );
   }

   // enableMulticast
   public synchronized void enableMulticast() {
      if( multicast_lock == null ) {
	 WifiManager wifi = (WifiManager)hopdroid.service.getSystemService( Context.WIFI_SERVICE );
	 multicast_lock = wifi.createMulticastLock( "hop-zeroconf-lock" );
	 multicast_lock.setReferenceCounted( true );
	 multicast_lock.acquire();
	 Log.v( "HopPluginZeroconf", "multicast lock acquired" );
      }
   }
   
   // startJmDns
   public synchronized void startJmDns() {
      if( jmdns == null ) {
	 try {
	    jmdns = JmDNS.create();
	    Log.v( "HopPluginZeroconf", "jmdns created" );
	    
	    enableMulticast();
	 } catch( Exception e ) {
	    Log.e( "HopPluginZeroconf", "Cannot start JmDns", e );
	 }
      }
   }

   // addServiceTypeListener
   public void addServiceTypeListener( final String type, final String event ) {
      try {
	 jmdns.addServiceListener( type, new ServiceListener() {
	       public void serviceResolved( ServiceEvent ev ) {
		  ServiceInfo si = ev.getInfo();
		  String[] addrs = si.getHostAddresses();

		  if( addrs.length > 0 ) {
		     hopdroid.pushEvent( event
					 ,"(\"add\" 1 \"" +
					 si.getProtocol() +
					 "\" \"" +
					 ev.getName() +
					 "\" \"" +
					 type +
					 "\" \"" +
					 si.getDomain() +
					 "\" \"" +
					 si.getServer() +
					 "\" " +
					 si.getPort() +
					 " \"" +
					 addrs[ 0 ] +
					 "\" ())" );
		  }
	       }
			
	       public void serviceRemoved( ServiceEvent ev ) {
		  Log.d( "HopPluginZeroconf", "Service removed: " + ev.getName());
	       }
	       
	       public void serviceAdded( ServiceEvent ev ) {
		  // Required to force serviceResolved to be
		  // called again (after the first search)
		  jmdns.requestServiceInfo( type, ev.getName(), 1 );
	       }	
	    } );
      } catch( Exception e ) {
	 Log.e( "HopPluginZeroconf", "Cannot add ServiceTypeListener", e );
      }
   }	 
      
      
   // addServiceListener
   public synchronized void addServiceListener() {
      try {
	 jmdns.addServiceTypeListener( new ServiceTypeListener() {
	       public void serviceTypeAdded( ServiceEvent ev ) {
		  final String type = ev.getType();

		  addServiceTypeListener( type, "zeroconf-add-service" );
	       }

	       public void subTypeForServiceTypeAdded( ServiceEvent ev ) {
		  Log.d( "HopPluginZeroconf", "SubType for service type added: "
			 + ev.getType() );
	       }
	    } );
      } catch( Exception e ) {
	 Log.e( "HopPluginZeroconf", "Cannot add ServiceTypeListener", e );
      }
   }	 

   // addServiceListener
   public synchronized void addTypeListener( final String type ) {
      addServiceTypeListener( type + ".local.", "zeroconf-add-service-" + type );
   }

   // publishJmDns
   public void publishJmDns( InputStream ip ) {
      try {
	 final String name = HopDroid.read_string( ip );
	 final int port = HopDroid.read_int32( ip );
	 final String type = HopDroid.read_string( ip ) + ".local.";
	 final String txt = HopDroid.read_string( ip );

	 if( !inkill ) {
	    new Thread( new Runnable() {
		  public void run() {
		     synchronized( jmdns ) {
			if( jmdns != null ) {
			   Log.d( "HopPluginZeroconf", ">>> register-service type=" +
				  type + " name=" + name );
			   ServiceInfo si = ServiceInfo.create( type, name, port, txt );
			   try {
			      jmdns.registerService( si );
			   } catch( Exception e ) {
			      Log.d( "HopPluginZeroconf", "!!! register-service: cannot register service", e );
			   }
			   Log.d( "HopPluginZeroconf", "<<< register-service type=" +
				  type + " name=" + name );
			}
		     }
		  }
	       } ).start();
	 }
      } catch( Exception e ) {
	 Log.d( "HopPluginZeroconf", "cannot register service", e );
      }
   }
      
   // stopJmDns()
   public synchronized void stopJmDns() {
      Log.d( "HopPluginZeroconf", ">>> stopJmDns" );

      if( jmdns != null ) {
	 synchronized( jmdns ) {
	    try {
	       Log.d( "HopPluginZeroconf", ">>> jmdns.close" );
	       jmdns.close();
	       Log.d( "HopPluginZeroconf", "<<< jmdns.close" );
	    } catch( Throwable _ ) {
	       ;
	    }
	 }
      }

      Log.d( "HopPluginZeroconf", "--- release multicast lock" );
      if( multicast_lock != null ) {
	 multicast_lock.release();
	 multicast_lock = null;
      }
      Log.d( "HopPluginZeroconf", "<<< stopJmDns" );
   }      

   // kill
   public synchronized void kill() {
      inkill = true;
      super.kill();

      stopJmDns();
   }

   // server
   void server( InputStream ip, OutputStream op ) throws IOException {
      switch( HopDroid.read_int( ip ) ) {
	 // version
	 case (byte)'v':
	    op.write( JmDNS.VERSION.getBytes() );
	    return;
	       
	 // begin
	 case (byte)'s':
	    startJmDns();
	    
	    op.write( "#t".getBytes() );
	    return;
	    
	 // begin
	 case (byte)'e':
	    stopJmDns();
	    
	    op.write( "#f".getBytes() );
	    return;

	 // publish
	 case (byte)'p':
	    publishJmDns( ip );
	    return;
	    
	 // serviceListener
	 case (byte)'l':
	    addServiceListener();
	    return;
	    
	 // typeListener
	 case (byte)'t':
	    addTypeListener( HopDroid.read_string( ip ) );
	    return;
      }
   }
}
