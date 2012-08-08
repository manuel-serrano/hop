/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginZeroconf.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 22 10:05:43 2010                          */
/*    Last change :  Wed Aug  8 10:59:59 2012 (serrano)                */
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
import android.net.wifi.WifiInfo;
import android.net.wifi.WifiManager;
import android.net.wifi.WifiManager.*;

import java.io.*;
import java.util.*;
import java.net.InetAddress;

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
   public void enableMulticast() {
      if( multicast_lock == null ) {
	 Log.d( ">>> HopPluginZeroconf", "multicast lock acquired" );
	 WifiManager wifi = (WifiManager)hopdroid.service.getSystemService( Context.WIFI_SERVICE );
	 
	 multicast_lock = wifi.createMulticastLock( "hop-zeroconf-lock" );
	 multicast_lock.setReferenceCounted( true );
	 multicast_lock.acquire();
	 Log.d( "<<< HopPluginZeroconf", "multicast lock acquired" );
      }
   }
   
   // startJmDns
   public synchronized void startJmDns() {
      if( jmdns == null ) {
	 try {
	    WifiManager wifi = (WifiManager)hopdroid.service.getSystemService( Context.WIFI_SERVICE );
	    WifiInfo wifiinfo = wifi.getConnectionInfo();
	    int intaddr = wifiinfo.getIpAddress();

	    enableMulticast();
	    
	    if( intaddr != 0 ) {
	       byte[] byteaddr = new byte[] {
		  (byte) (intaddr & 0xff), (byte) (intaddr >> 8 & 0xff),
		  (byte) (intaddr >> 16 & 0xff), (byte) (intaddr >> 24 & 0xff)
	       };
	       InetAddress addr = InetAddress.getByAddress( byteaddr );

	       jmdns = JmDNS.create( addr, "hop" );
	    } else {
	       jmdns = JmDNS.create();
	    }
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

		  Log.i( "HopPluginZeroconf", "ServiceResolved: name=" +
			 ev.getName() + " type=" + type +
			 " server=" + si.getServer() +
			 " port=" + si.getPort() + " addr=" +
			 (addrs.length > 0 ? addrs[ 0 ] : "") );
		  if( addrs.length > 0 ) {
		     hopdroid.pushEvent( event ,
					 "(\"found\" 1 \"" +
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
		  Log.i( "HopPluginZeroconf", "Service removed: " + ev.getName());
		  hopdroid.pushEvent( event,
				      "(\"removed\" 1\"" +
					 "\" \"" +
					 ev.getName() +
					 "\" \"" +
					 type +
					 "\" \"" +
					 "\" \"" +
					 "\" " +
					 0 +
					 " \"" +
					 "\" ())" );
	       }
	       
	       public void serviceAdded( ServiceEvent ev ) {
		  // Required to force serviceResolved to be
		  // called again (after the first search)
		  Log.i( "HopPluginZeroconf", "serviceAdded: " + ev.getName() );
			 
		  jmdns.requestServiceInfo( type, ev.getName() );
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

		  Log.d( "hopPluginZeroconf", "addServiceTypeListener: " + type );
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
   public void addTypeListener( final String type ) {
      addServiceTypeListener( type + ".local.", "zeroconf-add-service-" + type );
   }

   // publishJmDns
   public void publishJmDns( InputStream ip ) {
      try {
	 final String name = HopDroid.read_string( ip );
	 final int port = HopDroid.read_int32( ip );
	 final String type = HopDroid.read_string( ip ) + ".local.";
	 final String[] props = HopDroid.read_stringv( ip );

	 if( !inkill ) {
	    final HashMap<String, String> values = new HashMap<String, String>();

	    for( int i = 0; i < props.length; i += 2 ) {
	       values.put( props[ i ], props[ i + 1 ] );
	    }
	    
/* 	    new Thread( new Runnable() {                               */
/* 		  public void run() {                                  */
/* 		     if( jmdns != null ) {                             */
/* {* 			synchronized( jmdns ) {                        *} */
/* 			if( !inkill ) {                                */
/* 			   Log.d( "HopPluginZeroconf", ">>> register-service type=" + */
/* 				  type + " name=" + name );            */
/* 			   ServiceInfo si = ServiceInfo.create( type, name, port, 0, 0, values ); */
/* 			   try {                                       */
/* 			      jmdns.registerService( si );             */
/* 			   } catch( Exception e ) {                    */
/* 			      Log.d( "HopPluginZeroconf", "!!! register-service: cannot register service", e ); */
/* 			   }                                           */
/* 			   Log.d( "HopPluginZeroconf", "<<< register-service type=" + */
/* 				  type + " name=" + name );            */
/* 			}                                              */
/* 		     }                                                 */
/* 		  }                                                    */
/* 	       } ).start();                                            */
	 }
      } catch( Exception e ) {
	 Log.d( "HopPluginZeroconf", "cannot register service", e );
      }
   }
      
   // stopJmDns()
   public synchronized void stopJmDns() {
      Log.d( "HopPluginZeroconf", ">>> stopJmDns" );

      if( jmdns != null ) {
	 try {
	    Log.d( "HopPluginZeroconf", "--- jmdns.close" );
	    jmdns.close();
	 } catch( Throwable _ ) {
	    ;
	 }
	 Log.d( "HopPluginZeroconf", "<<< jmdns.close" );
      }

      if( multicast_lock != null ) {
	 Log.d( "HopPluginZeroconf", "--- release multicast lock" );
	 multicast_lock.release();
	 multicast_lock = null;
      }
      Log.d( "HopPluginZeroconf", "<<< stopJmDns" );
   }      

   // kill
   public synchronized void kill() {
      inkill = true;
      super.kill();
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
	    
	 // end
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
