/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginZeroconf.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 22 10:05:43 2010                          */
/*    Last change :  Thu Jun 28 17:46:57 2012 (serrano)                */
/*    Copyright   :  2010-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    jmdns Bonjour implementation (http://jmdns.sourceforge.net)      */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.util.Log;

import java.io.*;
import java.util.*;

import javax.jmdns.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginZeroconf extends HopPlugin {
   JmDNS jmdns = null;
   
   // constructor
   public HopPluginJmDns( HopDroid h, String n ) {
      super( h, n );
      Log.d( "HopPluginJmDns", "Plugin registered..." );
      
      try {
	 String tmp =
	    Environment.getExternalStorageDirectory().getAbsolutePath();
	 DexClassLoader dexLoader = new DexClassLoader(
	    "jmdns.jar", tmp, null, HopDroid.class.getClassLoader() );

	 Log.v( "HopPluginZeroconf", "Loading class \"" + cname + "\""
		   + " from JAR file \"" + name + "\"" );
	 Class<?> clazz = dexLoader.loadClass( cname );
	    
	 Method create = clazz.getMethod( "create", null );
	 jmdns = create.invoke( null, null );
	 Log.v( "HopPlulgZeroconf", "jmdns=" + jmdns );

/* 	    JmDNS.create();                                            */
	 
/* 	 jmdns.addServiceListener( "_http._tcp.local.", new ServiceListener() { */
/* 	       public void serviceResolved( ServiceEvent ev ) {        */
/* 		  Log.d( "HopPluginJmDns", "Service resolved: "        */
/* 			 + ev.getInfo().getQualifiedName()             */
/* 			 + " port:" + ev.getInfo().getPort());         */
/* 	       }                                                       */
/* 	       public void serviceRemoved( ServiceEvent ev ) {         */
/* 		  Log.d( "HopPluginJmDns", "Service removed: " + ev.getName()); */
/* 	       }                                                       */
/* 	       public void serviceAdded( ServiceEvent ev ) {           */
/* 		  // Required to force serviceResolved to be called again */
/* 		  // (after the first search)                          */
/* 		  //jmdns.requestServiceInfo( ev.getType(), ev.getName(), 1 ); */
/* 		  ServiceInfo si = ev.getInfo();                       */
/* 		                                                       */
/* 		  Log.d( "HopPluginJmDns", "Service aded: "            */
/* 			 + si.getQualifiedName() + " port:" + si.getPort() ); */
/* 		  try {                                                */
/* 		     handroid.pushEvent( "jmdns-add-service"           */
/* 					 ,"(\"add\" 1 \"" +            */
/* 					 si.getProtocol() +            */
/* 					 "\" \"" +                     */
/* 					 ev.getName() +                */
/* 					 "\" \"" +                     */
/* 					 ev.getType() +                */
/* 					 "\" \"" +                     */
/* 					 si.getDomain() +              */
/* 					 "\" \"" +                     */
/* 					 si.getServer() +              */
/* 					 "\" " +                       */
/* 					 si.getPort() +                */
/* 					 " \"" +                       */
/* 					 "si.getAddress().toString()" + */
/* 					 "\" ())" );                   */
/* 		  } catch( Throwable e ) {                             */
/* 		     Log.e( "HopPluginJmDns", "ERROR: " + e + " " + si ); */
/* 		  }                                                    */
/* 	       }                                                       */
/* 	    } );                                                       */
/*                                                                     */
	 jmdns.addServiceTypeListener( new ServiceTypeListener() {
	       public void serviceTypeAdded( ServiceEvent ev ) {
		  final String type = ev.getType();
		  Log.d( "HopPluginJmDns", "Service type aded: " + type );

		  jmdns.addServiceListener( type, new ServiceListener() {
			public void serviceResolved( ServiceEvent ev ) {
			   Log.d( "HopPluginJmDns", "Service resolved: "
				  + ev.getInfo().getQualifiedName()
				  + " port:" + ev.getInfo().getPort()
				  + " addr: " + ev.getInfo().getHostAddresses()[ 0 ] );
			   ServiceInfo si = ev.getInfo();
			   String[] addrs = si.getHostAddresses();

			   if( addrs.length > 0 ) {
			      handroid.pushEvent( "jmdns-add-service"
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
			   Log.d( "HopPluginJmDns", "Service removed: " + ev.getName());
			}
	       
			public void serviceAdded( ServiceEvent ev ) {
			   // Required to force serviceResolved to be called again
			   // (after the first search)
			   jmdns.requestServiceInfo( type, ev.getName(), 1 );
			}	
			
		     } );
	       }

	       public void subTypeForServiceTypeAdded( ServiceEvent ev ) {
		  Log.d( "HopPlugJmDns", "SubType for service type added: "
			 + ev.getType() );
	       }
	    } );
      } catch( Throwable _ ) {
	 ;
      }
   }

   // kill
   public void kill() {
      super.kill();

      if( jmdns != null ) {
	 synchronized( jmdns ) {
	    Log.d( "HopPluginJmDns", "Unregistering and cleaning plugin..." );
	    jmdns.unregisterAllServices();
	    Log.d( "HopPluginJmDns", "services unregisterd" );
	    try {
	       jmdns.close();
	       Log.d( "HopPluginJmDns", "jmdns closed" );
	    } catch( Throwable _ ) {
	       ;
	    }
	    jmdns = null;
	 }
      }
   }

   // server
   void server( InputStream ip, OutputStream op ) throws IOException {
      switch( HopDroid.read_int( ip ) ) {
	 // version
	 case (byte)'v':
	    op.write( JmDNS.VERSION.getBytes() );
	    return;
	       
	 // begin
	 case (byte)'b':
	    String s = HopDroid.read_string( ip );
	    
	    op.write( "#f".getBytes() );
	    return;
      }
   }
}
