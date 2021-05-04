/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopNsdManager.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  7 14:10:47 2012                          */
/*    Last change :  Fri Apr 30 19:16:12 2021 (serrano)                */
/*    Copyright   :  2012-21 Manuel Serrano                            */
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

import java.util.concurrent.ArrayBlockingQueue;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopNsdManager extends HopZeroconf {
   NsdManager nsd;
   
   Hashtable dlisteners = new Hashtable();
   Hashtable plisteners = new Hashtable();
   Hashtable events = new Hashtable();
   final ArrayBlockingQueue<NsdServiceInfo> queue =
      new ArrayBlockingQueue<NsdServiceInfo>( 10 );
   
   // constructor
   public HopNsdManager( HopDroid h ) {
      super( h );
      final HopNsdManager _this = this;
      
      Thread resolverthread = new Thread( new Runnable() {
	    public void run() {
	       boolean keep = true;
	       Log.d( "hopNsdManager", "starting resolverThread" );
	       while( keep ) {
		  try {
		     NsdServiceInfo info = queue.take();
		     if( info == null ) {
			// A null value is pushed when the thread
			// is terminated. See the stop method below. 
			keep = false;
		     } else {
			NsdManager.ResolveListener resolver =
			   new ResolveListener( _this );
			
			Log.d( "hopNsdManager", ">>> resolveService..."
			       + info.getServiceName() + " "
			       + info.getServiceType() );
			nsd.resolveService( info, resolver );
			Log.d( "hopNsdManager", "<<< resolveService..."
			       + info.getServiceName() + " "
			       + info.getServiceType() );
			Thread.sleep( 150 );
		     }
		  } catch( Exception _e ) {
		     Log.e( "hopNsdManager", "resolverThread error "
			    + _e.getClass().getName(), _e );
		  }
	       }
	       Log.d( "hopNsdManager", "ending resolverThread" );
	    }
	 } );
   
      resolverthread.start();
   }
   
   public void start( String name ) {
      if( nsd == null ) {
	 nsd = (NsdManager)hopdroid.service.getSystemService( Context.NSD_SERVICE );
	 Log.d( "HopNsdManager", "nsd=" + nsd );
      }
   }
   
   public void stop() {
      if( nsd != null ) {
	 Log.d( "hopNsdManager", "stopping..." );
	 Enumeration l = dlisteners.elements();

	 while( l.hasMoreElements() ) {
	    DiscoveryListener s = (DiscoveryListener)l.nextElement();
	    nsd.stopServiceDiscovery( s );
	 }

	 l = plisteners.elements();

	 while( l.hasMoreElements() ) {
	    NsdManager.RegistrationListener s = (NsdManager.RegistrationListener)l.nextElement();
	    nsd.unregisterService( s );
	 }
	 
	 nsd = null;

	 queue.clear();
	 try {
	    queue.put( (NsdServiceInfo)null );
	 } catch( Exception _e ) {
	    Log.e( "hopNsdManager", "cannot put in queue.1..."
		   + _e.getClass().getName(), _e );
	 }
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

	 Log.d( "HopNsdManager", "addServiceTypeListener type=" + type
		+ " event=" + event );
      
	 nsd.discoverServices( type, NsdManager.PROTOCOL_DNS_SD, l );
      }
   }

   public void addServiceListener() {
      ;
   }
   
   public void addTypeListener( final String type ) {
      Log.d( "HopNsdManager", "addTypeListener type=" + type );
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

      if( plisteners.get( name ) == null ) {
	 NsdServiceInfo si = new NsdServiceInfo();
	 NsdManager.RegistrationListener registerer;

	 si.setServiceName( name );
	 si.setPort( port );
	 si.setServiceType( type );

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

	 plisteners.put( name, registerer );
      
	 nsd.registerService( si, NsdManager.PROTOCOL_DNS_SD, registerer );
      }
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
      try {
	 hopnsd.queue.put( svc );
      } catch( Exception _e ) {
	 Log.e( "hopNsdManager", "cannot put in queue.2..."
		+ _e.getClass().getName(), _e );
      }
   }

   @Override public void onServiceLost( NsdServiceInfo service ) {
      Log.e( "HopNsdManager", "service lost: " + service );
   }

   @Override public void onDiscoveryStopped( String serviceType ) {
      Log.i( "HopNsdManager", "Discovery stopped: " + serviceType );
   }

   @Override public void onStartDiscoveryFailed( String serviceType, int errorCode ) {
      Log.e( "HopNsdManager", "Discovery failed: Error code: " + errorCode );
      hopnsd.nsd.stopServiceDiscovery(this);
   }

   @Override public void onStopDiscoveryFailed( String serviceType, int errorCode ) {
      Log.e( "HopNsdManager", "Discovery failed: Error code: " + errorCode );
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
      Log.e( "HopNsdManager.ResolveListener", "Resolve failed r=" + errorCode
	     + " " + NsdErrorMessage( errorCode )
	     + " si.type=" + svc.getServiceType()
	     + " si.event=" + svc.getServiceName()
	     + " queue.size=" + hopnsd.queue.size() );
      if( errorCode == 3 ) {
	 // already active error
	 try {
	    // random number
	    // hopnsd.queue.put( svc );
	    ;
	 } catch( Exception _e ) {
	    Log.e( "hopNsdManager", "cannot put in queue.3..."
		   + _e.getClass().getName(), _e );
	 }
      }
   }

   @Override
   public void onServiceResolved( NsdServiceInfo svc ) {
      String event = (String)hopnsd.events.get( svc.getServiceType() );
      InetAddress addr = svc.getHost();
      String proto = (addr instanceof Inet4Address) ? "ipv4"
	 : (addr instanceof Inet6Address) ? "ipv6" : "tcp";

      Log.d( "HopNsdManager.ResolveListener", "service resolved event=" + event
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
   
   
