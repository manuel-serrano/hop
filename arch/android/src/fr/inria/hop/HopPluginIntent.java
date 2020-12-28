/*=====================================================================*/
/*    .../hop/arch/android/src/fr/inria/hop/HopPluginIntent.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 25 09:26:00 2010                          */
/*    Last change :  Mon Dec 28 17:29:31 2020 (serrano)                */
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
import android.net.Uri;
import android.content.Intent;

import java.net.*;
import java.io.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginIntent extends HopPlugin {
   
   // constructor
   HopPluginIntent( HopDroid h, String n ) {
      super( h, n );
   }

   // intenter server
   public void server( final InputStream ip, final OutputStream op ) {
      throws IOException {
	 String action = HopDroid.read_string( ip );
	 String uri = HopDroid.read_string( ip );

	 try {
	    Intent intent = new Intent( stringToAction( action ) );
	    intent.setData( Uri.parse( geoLocation ) );
      
	    if( intent.resolveActivity( getPackageManager() ) != null ) {
	       Intent i = new Intent( hopdroid.service.getApplicationContext(), hopdroid.activityclass );
      
	       i.addFlags( Intent.FLAG_ACTIVITY_CLEAR_TOP );
	       i.addFlags( Intent.FLAG_ACTIVITY_REORDER_TO_FRONT );
	       i.addFlags( Intent.FLAG_ACTIVITY_NEW_TASK );
	       Log.d( "HopPluginIntent", "starting: " + action );
      
	       startActivity( intent );
	       
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
   private static int stringToAction( String action ) throws NoSuchFieldException {
      Field f = android.content.Intent.class.getField( action );
      return f.getInt( null );
   }
}
