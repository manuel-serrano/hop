/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopNsdManager.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  7 14:10:47 2012                          */
/*    Last change :  Fri Nov  9 15:12:17 2012 (serrano)                */
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

import android.net.nsd.*;

import java.net.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopNsdManager extends HopZeroconf {
   NsdManager nsd;
   NsdManager.RegistrationListener reglistener;
   NsdManager.DiscoveryListener discoverylistener;
   NsdManager.ResolveListener resolvelistener;
   
   Hashtable dlisteners = new Hashtable();

   // constructor
   public HopNsdManager( HopDroid h ) {
      super( h );

      reglistener = new NsdManager.RegistrationListener() {
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
   
   public void start() {
      if( nsd == null ) {
	 nsd = (NsdManager)hopdroid.service.getSystemService( Context.NSD_SERVICE );
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

	 nsd = null;
	 // nsd.unregisterService( mRegistrationListener );
	 //nsd.stopServiceDiscovery( mDiscoveryListener );
      }
   }
   
   public String version() {
      return "NsdManager " + android.os.Build.VERSION.SDK_INT;
   }

   public void addServiceTypeListener( final String type, final String event ) {
      DiscoveryListener l = (DiscoveryListener)dlisteners.get( type );

      Log.d( "HopNsdManager", "addServiceTypeListener type=" + type
	     + " event=" + event );
      
      if( l != null ) {
	 l.events.add( event );
      } else {
	 l = new DiscoveryListener( hopdroid, nsd, type, event );
	 dlisteners.put( type, l );

	 nsd.discoverServices( type, NsdManager.PROTOCOL_DNS_SD, l );
      }
   }

   public void addServiceListener() {
      ;
   }
   
   public void addTypeListener( final String type ) {
      addServiceTypeListener( type, "zeroconf-add-service-" + type );
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

	    Log.d( "HopNsdManager", "checkiing local ip=" +
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

      nsd.registerService( si, NsdManager.PROTOCOL_DNS_SD, reglistener );
   }
}

/*---------------------------------------------------------------------*/
/*    DiscoveryListener                                                */
/*---------------------------------------------------------------------*/
class DiscoveryListener implements NsdManager.DiscoveryListener {
   final NsdManager nsd;
   final String type;
   final Vector events = new Vector( 1 );
   HopDroid hopdroid;

   NsdManager.ResolveListener makeResolveListener( final String event ) {
      return new NsdManager.ResolveListener() {
	 @Override
	 public void onResolveFailed( NsdServiceInfo svc, int errorCode ) {
	    Log.e( "HopNsdManager", "Resolve failed r=" + errorCode
		   + " " + NsdErrorMessage( errorCode )
		   + " event=" + event + " si.event=" + svc.getServiceName() );
	 }

	 @Override
	 public void onServiceResolved( NsdServiceInfo svc ) {
	    Enumeration e = events.elements();
	    
	    while( e.hasMoreElements() ) {
	       String event = (String)e.nextElement();
	       InetAddress addr = svc.getHost();
	       String proto = (addr instanceof Inet4Address) ? "ipv4"
			: (addr instanceof Inet6Address) ? "ipv6" : "tcp";

	       Log.d( "HopNsdManager", "service resolved name="
		      + svc.getServiceName()
		      + " type=" + svc.getServiceType()
		      + " proto=" + proto
		      + " port=" + svc.getPort()
		      + " host=" + addr.getHostName()
		      + " addr=" + addr.getHostAddress() );

	       hopdroid.pushEvent( event,
				   "(\"found\" 1 \""
				   + proto
				   + "\" \""
				   + svc.getServiceName()
				   + "\" \""
				   + type
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
      };
   }
   
   // constructor
   DiscoveryListener( final HopDroid h, final NsdManager n, final String t, final String e ) {
      nsd = n;
      type = t;
      hopdroid = h;
      events.add( e );
   }

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
	
      
   @Override public void onDiscoveryStarted( String regType ) { ; }

   @Override public void onServiceFound( NsdServiceInfo svc ) {
      String event = svc.getServiceName();
      Log.d( "HopNsdManager", "service found name=" + event
	     + " type=" + svc.getServiceType() );
	 
      nsd.resolveService( svc, makeResolveListener( event ) );
   }

   @Override public void onServiceLost( NsdServiceInfo service ) {
      Log.e( "HopNsdManager", "service lost" + service );
   }

   @Override public void onDiscoveryStopped( String serviceType ) {
      Log.i( "HopNsdManager", "Discovery stopped: " + serviceType );
   }

   @Override public void onStartDiscoveryFailed( String serviceType, int errorCode ) {
      Log.e( "HopNsdManager", "Discovery failed: Error code:" + errorCode );
      nsd.stopServiceDiscovery(this);
   }

   @Override public void onStopDiscoveryFailed( String serviceType, int errorCode ) {
      Log.e( "HopNsdManager", "Discovery failed: Error code:" + errorCode );
      nsd.stopServiceDiscovery( this );
   }
}
      
