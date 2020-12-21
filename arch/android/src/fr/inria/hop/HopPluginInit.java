/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopPluginInit.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct 19 09:44:16 2010                          */
/*    Last change :  Mon Dec 21 08:28:59 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The initial plugin that allows plugin installation               */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.os.*;
import android.util.Log;

import dalvik.system.*;

import java.lang.*;
import java.io.*;
import java.lang.reflect.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginInit extends HopPlugin {
   final Class[] classes = new Class[ 2 ];
   
   HopPluginInit( HopDroid h, String n ) throws ClassNotFoundException {
      super( h, n );

      try {
	 classes[ 0 ] = Class.forName( "fr.inria.hop.HopDroid" );
	 classes[ 1 ] = Class.forName( "java.lang.String" );
      } catch( ClassNotFoundException e ) {
	 Log.e( "HopPluginInit", "server error "
		+ e.toString() + " class not found." );
	 throw e;
      }
   }

   // loadClassFromJar
   public Class<?> loadClassFromJar( String name ) throws Exception {
      int i = name.lastIndexOf( '/' );
      int j = name.lastIndexOf( '.' );
      String cname = "fr.inria.hop."
	 + name.substring( (i < 0 ? 0 : i + 1), (j <= i ? name.length() : j) );
      String tmp =
	 Environment.getExternalStorageDirectory().getAbsolutePath();

      DexClassLoader dexLoader = new DexClassLoader(
	 name, tmp, null, HopDroid.class.getClassLoader() );
      Log.v( "HopPluginInit", "Loading class \"" + cname + "\""
	     + " from JAR file \"" + name + "\"" );
      return dexLoader.loadClass( cname );
	    
   }

   // loadPlugin
   public HopPlugin loadPlugin( String name )
      throws Exception {
      Class<?> clazz = null;
      
      try {
	 clazz = Class.forName( name );
      } catch( ClassNotFoundException e ) {
	 clazz = loadClassFromJar( name );
      }
      
      Constructor constr = clazz.getConstructor( classes );
      Object[] args = { hopdroid, name };
      return (HopPlugin)constr.newInstance( args );
   }
   
   // static variables
   public void server( InputStream ip, OutputStream op ) throws IOException {
      String name = HopDroid.read_string( ip );

      if( name.equals( "reboot" ) ) {
	 Log.d( "HopPluginInit", "reboot..." );
	 hopdroid.service.reboot();
      } else {
	 int id = HopDroid.getPlugin( name );

	 Log.d( "HopPluginInit", "name=" + name + " id=" + id );

	 if( id < 0 ) {
	    // we don't have loaded that plugin yet
	    try {
	       HopPlugin p = loadPlugin( name );
	       id = HopDroid.registerPlugin( p );
	       Log.v( "HopPluginInit", "plugin " + p.name + " registered..." );
	    } catch( ClassNotFoundException e ) {
	       Log.e( "HopPlugInit", "Class Not Found: " + name );
	       e.printStackTrace();
	       op.write( "-2 ".getBytes() );
	       return;
	    } catch( NoSuchMethodException e ) {
	       Log.e( "HopPlugInit", "No such method: " + name );
	       e.printStackTrace();
	       op.write( "-3 ".getBytes() );
	       return;
	    } catch( SecurityException e ) {
	       Log.e( "HopPlugInit", "Security exception: " + name );
	       e.printStackTrace();
	       op.write( "-4 ".getBytes() );
	       return;
	    } catch( InstantiationException e ) {
	       Log.e( "HopPlugInit", "Instantiate exception: " + name );
	       e.printStackTrace();
	       op.write( "-5 ".getBytes() );
	       return;
	    } catch( IllegalAccessException e ) {
	       Log.e( "HopPlugInit", "Illegal access: " + name );
	       e.printStackTrace();
	       op.write( "-6 ".getBytes() );
	       return;
	    } catch( IllegalArgumentException e ) {
	       Log.e( "HopPlugInit", "Illegal argument: " + name );
	       e.printStackTrace();
	       op.write( "-7 ".getBytes() );
	       return;
	    } catch( InvocationTargetException e ) {
	       Log.e( "HopPlugInit", "Invocation target exception: " + name );
	       e.printStackTrace();
	       op.write( "-8 ".getBytes() );
	       return;
	    } catch ( Exception e ) {
	       Log.e( "HopPlugInit", "Unknown exception: " + name );
	       e.printStackTrace();
	       op.write( "-9 ".getBytes() );
	       return;
	    
	    }
	 }

	 op.write( Integer.toString( id ).getBytes() );
	 op.write( " ".getBytes() );
      }
   }
}
