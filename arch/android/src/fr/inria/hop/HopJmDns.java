/*=====================================================================*/
/*    .../hop/3.1.x/arch/android/src/fr/inria/hop/HopJmDns.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  7 12:03:59 2012                          */
/*    Last change :  Sun Jun 19 08:35:22 2016 (serrano)                */
/*    Copyright   :  2012-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The JmDns (zeroconf) Hop binding                                 */
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

import java.net.*;
import java.util.*;

import javax.jmdns.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopJmDns extends HopZeroconf {
   JmDNS jmdns = null;
   android.net.wifi.WifiManager.MulticastLock multicast_lock;

   // constructor
   public HopJmDns( HopDroid h ) {
      super( h );
   }

   // version
   public String version() {
      return "jmdns " + JmDNS.VERSION;
   }
   
   // start
   public synchronized void start( String hostname ) {
      Log.d( "HopJmDms", "starting plugin..." );
      if( jmdns == null ) {
	 try {
	    WifiManager wifi = (WifiManager)hopdroid.service.getSystemService( Context.WIFI_SERVICE );
	    WifiInfo wifiinfo = wifi.getConnectionInfo();
	    int intaddr = wifiinfo.getIpAddress();

	    Log.d( "HopJmDns", "enabling multicast" );
	    enableMulticast();
	    
	    if( intaddr != 0 ) {
	       byte[] byteaddr = new byte[] {
		  (byte) (intaddr & 0xff), (byte) (intaddr >> 8 & 0xff),
		  (byte) (intaddr >> 16 & 0xff), (byte) (intaddr >> 24 & 0xff)
	       };
	       InetAddress addr = InetAddress.getByAddress( byteaddr );

	       jmdns = JmDNS.create( addr, hostname );
	       Log.d( "HopJmDns", "creating instance addr="
		      + addr + " hostname=" + jmdns.getHostName() );
	       
	    } else {
	       Log.d( "HopJmDns", "creating instance" );
	       jmdns = JmDNS.create();
	    }
	 } catch( Exception e ) {
	    Log.e( "HopJmDns", "Cannot start JmDns", e );
	 }
      }
   }

   // stop
   public synchronized void stop() {
      Log.d( "HopJmDns", ">>> stopJmDns" );

      if( jmdns != null ) {
	 try {
	    jmdns.close();
	    jmdns = null;
	 } catch( Throwable t ) {
	    ;
	 }
      }

      if( multicast_lock != null ) {
	 multicast_lock.release();
	 multicast_lock = null;
      }
      Log.d( "HopJmDns", "<<< stopJmDns" );
   }
   
   // addServiceTypeListener
   private void addServiceTypeListener( final String utype, final String type, final String event ) {
      try {
	 jmdns.addServiceListener( type, new ServiceListener() {
	       
	       public void serviceResolved( ServiceEvent ev ) {
		  ServiceInfo si = ev.getInfo();
		  String[] addrs = si.getHostAddresses();
		  InetAddress[] iaddrs = si.getInetAddresses();

		  for( int i = iaddrs.length - 1; i >= 0; i-- ) {
		     String proto = (iaddrs[ i ] instanceof Inet4Address) ? "ipv4"
			: (iaddrs[ i ] instanceof Inet6Address) ? "ipv6" : "tcp";

		     Log.i( "HopJmDns", "ServiceResolved name="
			    + ev.getName()
			    + " type=" + utype
			    + " proto=" + proto
			    + " server=" + si.getServer()
			    + " port=" + si.getPort()
			    + " addr=" + iaddrs[ i ].getHostAddress() );
			
		     hopdroid.pushEvent( event,
					 "(\"found\" 1 \"" +
					 proto +
					 "\" \"" +
					 ev.getName() +
					 "\" \"" +
					 utype +
					 "\" \"" +
					 si.getDomain() +
					 "\" \"" +
					 si.getServer() +
					 "\" " +
					 si.getPort() +
					 " \"" +
					 iaddrs[ i ].getHostAddress() +
					 "\" ())" );
		  }
	       }
			
	       public void serviceRemoved( ServiceEvent ev ) {
		  Log.i( "HopJmDns", "Service removed: " + ev.getName());
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
		  Log.i( "HopJmDns", "serviceAdded: name=" +
			 ev.getName() + " type=" + type );
			 
		  jmdns.requestServiceInfo( ev.getType(), ev.getName() );
	       }
	    } );
      } catch( Exception e ) {
	 Log.e( "HopJmDns", "Cannot add ServiceTypeListener", e );
      }
   }

   // addServiceTypeListener
   public void addServiceTypeListener( final String type, final String event ) {
      addServiceTypeListener( type, type, event );
   }
      
   // addServiceListener
   public synchronized void addServiceListener() {
      try {
	 jmdns.addServiceTypeListener( new ServiceTypeListener() {
	       public void serviceTypeAdded( ServiceEvent ev ) {
		  final String type = ev.getType();

		  Log.d( "HopJmDns", "addServiceTypeListener: " + type );
		  addServiceTypeListener( type, type, "zeroconf-add-service" );
	       }

	       public void subTypeForServiceTypeAdded( ServiceEvent ev ) {
		  Log.d( "HopJmDns", "SubType for service type added: "
			 + ev.getType() );
	       }
	    } );
      } catch( Exception e ) {
	 Log.e( "HopJmDns", "Cannot add ServiceTypeListener", e );
      }
   }	 

   // addServiceListener
   public void addTypeListener( final String type ) {
      addServiceTypeListener( type, type + ".local.", "zeroconf-add-service-" + type );
   }

   // publishJmDns
   public void publish( final String name, final int port, final String type, final String[] props ) {
      try {
	 Log.d( "HopJmDns", "publish-service " + name + " type=" + type );

	 final HashMap<String, String> values = new HashMap<String, String>();

	 for( int i = 0; i < props.length; i += 2 ) {
	    Log.d( "HopJmDns", "prop=" + props[ i ] + " " + props[ i + 1 ] );
	    values.put( props[ i ], props[ i + 1 ] );
	 }

	 new Thread( new Runnable() {
	       public void run() {
		  ServiceInfo si = ServiceInfo.create( type + ".local.", name, port, 0, 0, true, values );
		  if( jmdns != null ) {
		     try {
			;
			jmdns.registerService( si );
		     } catch( Exception e ) {
			Log.d( "HopJmDns", "!!! publish-service error, "
			       + name + " type=" + type
			       + " err=" + e );
		     }
		  }
	       }
	    } ).start();
      } catch( Exception e ) {
	 Log.d( "HopJmDns", "cannot register service", e );
      }
   }

   // enableMulticast
   public void enableMulticast() {
      if( multicast_lock == null ) {
	 WifiManager wifi = (WifiManager)hopdroid.service.getSystemService( Context.WIFI_SERVICE );
	 
	 multicast_lock = wifi.createMulticastLock( "hop-zeroconf-lock" );
	 multicast_lock.setReferenceCounted( true );
	 multicast_lock.acquire();
      }
   }
}
   

