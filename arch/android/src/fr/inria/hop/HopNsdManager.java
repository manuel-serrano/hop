/*=====================================================================*/
/*    .../2.6.x/arch/android/src/fr/inria/hop/HopNsdManager.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  7 14:10:47 2012                          */
/*    Last change :  Fri Feb 21 13:28:49 2014 (serrano)                */
/*    Copyright   :  2012-14 Manuel Serrano                            */
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

import android.net.nsd.*;

import java.net.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopNsdManager extends HopZeroconf {
   NsdManager nsd;
   NsdManager.RegistrationListener registerer;
   NsdManager.ResolveListener resolver;
   
   Hashtable dlisteners = new Hashtable();
   Hashtable events = new Hashtable();

   // constructor
   public HopNsdManager( HopDroid h ) {
      super( h );

      registerer = new NsdManager.RegistrationListener() {
	    public void onRegistrationFailed( NsdServiceInfo si, int err ) {
	       Log.d( "HopNsdManager", "registration failed: " + si );
	    }
	    public void onServiceRegistered( NsdServiceInfo si ) {
	       Log.d( "HopNsdManager", "registration succeeded: " + si );
	    }
	    public void onServiceUnregistered( NsdServiceInfo si ) { ; }
	    public void onUnregistrationFailed( NsdServiceInfo si, int err ) { ; }
	 };
   }
   
   public void start( String name ) {
      if( nsd == null ) {
	 nsd = (NsdManager)hopdroid.service.getSystemService( Context.NSD_SERVICE );
	 resolver = new ResolveListener( this );
	 Log.d( "HopNsdManager", "nsd=" + nsd );
      }
   }
   
   public void stop() {
      if( nsd != null ) {
	 Enumeration l = dlisteners.elements();

	 while( l.hasMoreElements() ) {
	    DiscoveryListener s = (DiscoveryListener)l.nextElement();
	    nsd.stopServiceDiscovery( s );
	 }

	 nsd.unregisterService( registerer );
	 nsd = null;
      }
   }
   
   public String version() {
      return "NsdManager " + android.os.Build.VERSION.SDK_INT;
   }

   public void addServiceTypeListener( final String type, final String event ) {
      if( dlisteners.get( type ) == null ) {
	 DiscoveryListener l = new DiscoveryListener( this, type );
	 
	 dlisteners.put( type, l );
	 events.put( "." + type, event );

	 Log.d( "HopNsdManager", ">>> addServiceTypeListener type=" + type
		+ " event=" + event );
      
	 nsd.discoverServices( type, NsdManager.PROTOCOL_DNS_SD, l );
	 
	 Log.d( "HopNsdManager", "<<< addServiceTypeListener type=" + type
		+ " event=" + event );
      }
   }

   public void addServiceListener() {
      ;
   }
   
   public void addTypeListener( final String type ) {
      Log.d( "HopNsdManager", ">>> addTypeListener type=" + type );
      addServiceTypeListener( type, "zeroconf-add-service-" + type );
      Log.d( "HopNsdManager", "<<< addTypeListener type=" + type );
   }
   
   public InetAddress getLocalIpAddress() throws Exception {
      for( Enumeration<NetworkInterface> en = NetworkInterface
	      .getNetworkInterfaces(); en.hasMoreElements(); ) {
	 NetworkInterface intf = en.nextElement();
	 Enumeration<InetAddress> enumIpAddr = intf.getInetAddresses();

	 Log.d( "HopNsdManager", "interface=" + intf + " more="
		+ enumIpAddr.hasMoreElements() );

	 while( enumIpAddr.hasMoreElements() ) {
	    InetAddress inetAddress = enumIpAddr.nextElement();

	    Log.d( "HopNsdManager", "checking local ip=" +
		   inetAddress.getHostAddress() );

	    if( !inetAddress.isLoopbackAddress() ) {
	       return inetAddress;
	    }
	 }
      }

      return null;
   }
   
   public void publish( final String name, final int port, final String type, final String[] props ) {
      Log.d( "HopNsdManager", "publish name=" + name + " type=" + type + " port=" + port );

      NsdServiceInfo si = new NsdServiceInfo();

      si.setServiceName( name );
      si.setPort( port );
      si.setServiceType( type );

      nsd.registerService( si, NsdManager.PROTOCOL_DNS_SD, registerer );
   }
}

/*---------------------------------------------------------------------*/
/*    DiscoveryListener                                                */
/*---------------------------------------------------------------------*/
class DiscoveryListener implements NsdManager.DiscoveryListener {
   final HopNsdManager hopnsd;
   final String type;

   // constructor
   DiscoveryListener( final HopNsdManager h, final String t ) {
      hopnsd = h;
      type = t;
   }

   @Override public void onDiscoveryStarted( String regType ) { ; }

   @Override public void onServiceFound( NsdServiceInfo svc ) {
      Log.d( "HopNsdManager", "service found name=" + svc.getServiceName()
	     + " type=" + svc.getServiceType()
	     + " host=" + svc.getHost() );
      
      hopnsd.nsd.resolveService( svc, hopnsd.resolver );
   }

   @Override public void onServiceLost( NsdServiceInfo service ) {
      Log.e( "HopNsdManager", "service lost" + service );
   }

   @Override public void onDiscoveryStopped( String serviceType ) {
      Log.i( "HopNsdManager", "Discovery stopped: " + serviceType );
   }

   @Override public void onStartDiscoveryFailed( String serviceType, int errorCode ) {
      Log.e( "HopNsdManager", "Discovery failed: Error code:" + errorCode );
      hopnsd.nsd.stopServiceDiscovery(this);
   }

   @Override public void onStopDiscoveryFailed( String serviceType, int errorCode ) {
      Log.e( "HopNsdManager", "Discovery failed: Error code:" + errorCode );
      hopnsd.nsd.stopServiceDiscovery( this );
   }
}
      
/*---------------------------------------------------------------------*/
/*    resolveListener                                                  */
/*---------------------------------------------------------------------*/
class ResolveListener implements NsdManager.ResolveListener {
   final HopNsdManager hopnsd;
 
   String NsdErrorMessage( int err ) {
      if( err == NsdManager.FAILURE_ALREADY_ACTIVE ) {
	 return "already active";
      }
      if( err == NsdManager.FAILURE_INTERNAL_ERROR ) {
	 return "internal error";
      }
      if( err == NsdManager.FAILURE_MAX_LIMIT ) {
	 return "max limit";
      }
      return "unknown error";
   }
	
      
   @Override
   public void onResolveFailed( NsdServiceInfo svc, int errorCode ) {
      Log.e( "HopNsdManager", "Resolve failed r=" + errorCode
	     + " " + NsdErrorMessage( errorCode )
	     + " si.type=" + svc.getServiceType()
	     + " si.event=" + svc.getServiceName() );
   }

   @Override
   public void onServiceResolved( NsdServiceInfo svc ) {
      String event = (String)hopnsd.events.get( svc.getServiceType() );
      InetAddress addr = svc.getHost();
      String proto = (addr instanceof Inet4Address) ? "ipv4"
	 : (addr instanceof Inet6Address) ? "ipv6" : "tcp";

      Log.d( "HopNsdManager", "service resolved event=" + event
	     + " name=" + svc.getServiceName()
	     + " type=" + svc.getServiceType()
	     + " proto=" + proto
	     + " port=" + svc.getPort()
	     + " host=" + addr.getHostName()
	     + " addr=" + addr.getHostAddress() );

      if( event != null ) {
	 hopnsd.hopdroid.pushEvent( event,
				    "(\"found\" 1 \""
				    + proto
				    + "\" \""
				    + svc.getServiceName()
				    + "\" \""
				    + svc.getServiceType()
				    + "\" \""
				    + "local"
				    + "\" \""
				    + addr.getHostName()
				    + "\" "
				    + svc.getPort()
				    + " \""
				    + addr.getHostAddress()
				    + "\" ())" );
      }
   }

   // constructor
   ResolveListener( final HopNsdManager n ) {
      hopnsd = n;
   }
}
   
   
