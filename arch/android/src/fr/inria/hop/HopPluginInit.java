/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopPluginInit.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct 19 09:44:16 2010                          */
/*    Last change :  Fri Oct 22 14:37:42 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    The initial plugin that allows plugin installation               */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
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
	 try {
	    int i = name.lastIndexOf( '/', 0 );
	    int j = name.lastIndexOf( '.', i < 0 ? 0 : i);
	    String cname = name.substring( i < 0 ? 0 : i,
					   j < 0 ? name.length() : j );
	    Log.v( "HopPluginInit", "Loading dex file: " + name );
	    DexClassLoader dexLoader = new DexClassLoader(
	       name, "/tmp", null, HopAndroid.class.getClassLoader() );
	    Log.v( "HopPluginInit", "Loading class: " + cname );
	    Class<?> clazz = dexLoader.loadClass( cname );
	    
	    Constructor constr = clazz.getConstructor( classes );
	    Object[] args = { activity, name };
	    HopPlugin p = (HopPlugin)constr.newInstance( args );

	    id = HopAndroid.registerPlugin( p );
	 } catch( ClassNotFoundException e ) {
	    op.write( "-2".getBytes() );
	    return;
	 } catch( NoSuchMethodException e ) {
	    op.write( "-3".getBytes() );
	    return;
	 } catch( SecurityException e ) {
	    op.write( "-4".getBytes() );
	    return;
	 } catch( InstantiationException e ) {
	    op.write( "-5".getBytes() );
	    return;
	 } catch( IllegalAccessException e ) {
	    op.write( "-6".getBytes() );
	    return;
	 } catch( IllegalArgumentException e ) {
	    op.write( "-7".getBytes() );
	    return;
	 } catch( InvocationTargetException e ) {
	    op.write( "-8".getBytes() );
	    return;
	 }
      }

      op.write( Integer.toString( id ).getBytes() );
   }
}
