/*=====================================================================*/
/*    .../hop/arch/android/src/fr/inria/hop/HopPluginBuild.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Nov 30 17:35:50 2010                          */
/*    Last change :  Sun Dec 27 15:00:27 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Get the BUILD info                                               */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.os.*;
import android.util.*;
import android.util.Log;
import android.content.pm.*;
import android.os.Environment;
import android.content.Context;
import android.content.res.*;
import android.provider.Settings.*;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginBuild extends HopPlugin {
   // constructor
   public HopPluginBuild( HopDroid h, String n ) {
      super( h, n );
   }

   // server
   public void server( InputStream ip, OutputStream op ) throws IOException {
      switch( HopDroid.read_int( ip ) ) {
	 // sdk version
	 case (byte)'v':
	    op.write( "\"".getBytes() );
	    op.write( android.os.Build.VERSION.RELEASE.getBytes() );
	    op.write( "\"".getBytes() );
	    return;
	    
	 // sdk version
	 case (byte)'k':
	    op.write( String.valueOf( android.os.Build.VERSION.SDK_INT ).getBytes() );
	    return;
	    
	 // model
	 case (byte)'m':
	    op.write( "\"".getBytes() );
	    op.write( android.os.Build.MODEL.getBytes() );
	    op.write( "\"".getBytes() );
	    return;
	    
	 // product
	 case (byte)'p':
	    op.write( "\"".getBytes() );
	    op.write( android.os.Build.PRODUCT.getBytes() );
	    op.write( "\"".getBytes() );
	    return;

	 // device name
	 case (byte)'n':
	    String deviceName = Global.getString( hopdroid.service.getContentResolver(), Global.DEVICE_NAME );
	    op.write( "\"".getBytes() );
	    op.write( deviceName.getBytes() );
	    op.write( "\"".getBytes() );
	    return;
	    
         // home
	 case (byte)'h':
	    op.write( "\"".getBytes() );
	    op.write( Hop.HOME().getAbsolutePath().getBytes() );
	    op.write( "\"".getBytes() );
	    return;
		      
         // storage
	 case (byte)'s':
	    op.write( "\"".getBytes() );
	    op.write( hopdroid.service.getApplicationContext().getExternalFilesDir( null ).getAbsolutePath().getBytes() );
	    op.write( "\"".getBytes() );
	    return;
	    
         // application info
	 case (byte)'i':
	    ApplicationInfo info = hopdroid.service.getApplicationInfo();

	    if( info == null ) {
	       op.write( "()".getBytes() );
	    } else {
	       op.write( "(".getBytes() );
	       
	       if( info.className != null ) {
		  op.write( " class-name: \"".getBytes() );
		  op.write( info.className.getBytes() );
		  op.write( "\"".getBytes() );
	       }

	       if( info.permission != null ) {
		  op.write( " permission: \"".getBytes() );
		  op.write( info.permission.getBytes() );
		  op.write( "\"".getBytes() );
	       }

	       if( info.processName != null ) {
		  op.write( " process-name: \"".getBytes() );
		  op.write( info.processName.getBytes() );
		  op.write( "\"".getBytes() );
	       }

	       if( info.dataDir !=null ) {
		  op.write( " data-dir: \"".getBytes() );
		  op.write( info.dataDir.getBytes() );
		  op.write( "\"".getBytes() );
	       }

	       op.write( ")".getBytes() );
	    }
	    return;

	 // configuration
	 case (byte)'c':
	    Configuration config = hopdroid.service.getResources().getConfiguration();
	    
	    if( config == null ) {
	       op.write( "()".getBytes() );
	    } else {
	       op.write( HopConfiguration.toString( config ).getBytes() );
	    }
	    return;
      }
   }
}
      
	 

