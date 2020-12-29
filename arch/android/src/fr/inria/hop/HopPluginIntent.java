/*=====================================================================*/
/*    .../hop/arch/android/src/fr/inria/hop/HopPluginIntent.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 25 09:26:00 2010                          */
/*    Last change :  Tue Dec 29 07:20:31 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Common Android Intent                                            */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.content.res.*;
import android.os.*;
import android.util.Log;
import android.net.Uri;
import android.content.Intent;

import java.net.*;
import java.io.*;
import java.util.*;
import java.lang.reflect.Field;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginIntent extends HopPlugin {
   
   // constructor
   HopPluginIntent( HopDroid h, String n ) {
      super( h, n );
   }

   // intent server
   public void server( final InputStream ip, final OutputStream op ) 
      throws IOException {
      String action = HopDroid.read_string( ip );
      String uri = HopDroid.read_string( ip );
      int len = HopDroid.read_int( ip );

      Log.d( "HopPluginIntent", "Intent action=" + action + " uri=" + uri + " len=" + len );
      try {
	 Intent intent = new Intent( stringToAction( action ) );

	 if( uri.length() > 0 ) {
	    intent.setData( Uri.parse( uri ) );
	 }

	 if( len > 0 ) {
	    for( int i = 0; i < len; i += 3 ) {
	       String name = stringToExtra( HopDroid.read_string( ip ) );
	       String type = HopDroid.read_string( ip );

	       if( type.equals( "String" ) ) {
		  intent.putExtra( name, HopDroid.read_string( ip ) );
	       } else if( type.equals( "Uri" ) ) {
		  intent.putExtra( name, Uri.parse( HopDroid.read_string( ip ) ) );
	       } else if( type.equals( "int" ) ) {
		  intent.putExtra( name, HopDroid.read_int( ip ) );
	       } else if( type.equals( "long" ) ) {
		  intent.putExtra( name, HopDroid.read_int64( ip ) );
	       } else if( type.equals( "float" ) ) {
		  intent.putExtra( name, HopDroid.read_float( ip ) );
	       } else if( type.equals( "boolean" ) ) {
		  intent.putExtra( name, HopDroid.read_int( ip ) != 0 );
	       } else {
		  op.write( "-2".getBytes() );
		  return;
	       }
	    }
	 }
      
	 if( intent.resolveActivity( hopdroid.service.getPackageManager() ) != null ) {
	    Intent i = new Intent( hopdroid.service.getApplicationContext(), hopdroid.activityclass );
      
	    i.addFlags( Intent.FLAG_ACTIVITY_CLEAR_TOP );
	    i.addFlags( Intent.FLAG_ACTIVITY_REORDER_TO_FRONT );
	    i.addFlags( Intent.FLAG_ACTIVITY_NEW_TASK );
	    Log.d( "HopPluginIntent", "starting: " + action );
	    
	    if( hopdroid.activity != null ) {
	       hopdroid.activity.startActivity( intent );
	    }
	       
	    op.write( "0".getBytes() );
	 } else {
	    op.write( "-1".getBytes() );
	 }
      } catch( NoSuchFieldException e ) {
	 Log.d( "HopPluginIntent", "No such field " + action );
	 op.write( "-1".getBytes() );
      }
   }

   // stringToAction
   private static String stringToAction( String action ) throws NoSuchFieldException {
      Field f = android.content.Intent.class.getField( action );
      try {
	 return (String)f.get( null );
      } catch( IllegalArgumentException e1 ) {
	 return "";
      } catch( IllegalAccessException e2 ) {
	 return "";
      }
   }
   
   // stringToExtra
   private static String stringToExtra( String name ) {
      try {
	 Field f = android.content.Intent.class.getField( name );
	 return (String)f.get( null );
      } catch( Throwable e ) {
	 return name;
      }
   }
}
