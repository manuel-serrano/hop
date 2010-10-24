/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopPluginInit.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct 19 09:44:16 2010                          */
/*    Last change :  Sat Oct 23 12:10:08 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
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
   final Class[] classes = new Class[ 3 ];
   
   HopPluginInit( HopAndroid h, Activity a, String n ) throws ClassNotFoundException {
      super( h, a, n );

      try {
	 classes[ 0 ] = Class.forName( "fr.inria.hop.HopAndroid" );
	 classes[ 1 ] = Class.forName( "android.app.Activity" );
	 classes[ 2 ] = Class.forName( "java.lang.String" );
      } catch( ClassNotFoundException e ) {
	 Log.e( "HopPluginInit", "server error "
		+ e.toString() + " class not found." );
	 throw e;
      }
   }
   
   // static variables
   void server( InputStream ip, OutputStream op ) throws IOException {
      String name = HopAndroid.read_string( ip );
      int id = HopAndroid.getPlugin( name );

      Log.v( "HopPluginInit", "name=" + name + " id=" + id );

      if( id < 0 ) {
	 // we don't have loaded that plugin yet
	 int i = name.lastIndexOf( '/' );
	 int j = name.lastIndexOf( '.' );
	 String cname = "fr.inria.hop."
	    + name.substring( (i < 0 ? 0 : i + 1), (j < i ? name.length() : j) );
		 
	 String tmp =
	    Environment.getExternalStorageDirectory().getAbsolutePath();
	    
	 try {
	    DexClassLoader dexLoader = new DexClassLoader(
	       name, tmp, null, HopAndroid.class.getClassLoader() );
	    Log.v( "HopPluginInit", "Loading class \"" + cname + "\""
		   + " from JAR file \"" + name + "\"" );
	    Class<?> clazz = dexLoader.loadClass( cname );
	    
	    Constructor constr = clazz.getConstructor( classes );
	    Object[] args = { handroid, activity, name };
	    HopPlugin p = (HopPlugin)constr.newInstance( args );

	    id = HopAndroid.registerPlugin( p );
	 } catch( ClassNotFoundException e ) {
	    Log.e( "HopPlugInit", "Class Not Found: " + cname );
	    op.write( "-2".getBytes() );
	    return;
	 } catch( NoSuchMethodException e ) {
	    Log.e( "HopPlugInit", "No such method: " + cname );
	    op.write( "-3".getBytes() );
	    return;
	 } catch( SecurityException e ) {
	    Log.e( "HopPlugInit", "Security exception: " + cname );
	    op.write( "-4".getBytes() );
	    return;
	 } catch( InstantiationException e ) {
	    Log.e( "HopPlugInit", "Instantiate exception: " + cname );
	    op.write( "-5".getBytes() );
	    return;
	 } catch( IllegalAccessException e ) {
	    Log.e( "HopPlugInit", "Illegal access: " + cname );
	    op.write( "-6".getBytes() );
	    return;
	 } catch( IllegalArgumentException e ) {
	    Log.e( "HopPlugInit", "Illegal argument: " + cname );
	    op.write( "-7".getBytes() );
	    return;
	 } catch( InvocationTargetException e ) {
	    Log.e( "HopPlugInit", "Invocation target exception: " + cname );
	    op.write( "-8".getBytes() );
	    return;
	 }
      }

      op.write( Integer.toString( id ).getBytes() );
   }
}
