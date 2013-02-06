/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopPluginBuild.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Nov 30 17:35:50 2010                          */
/*    Last change :  Wed Feb  6 09:57:00 2013 (serrano)                */
/*    Copyright   :  2010-13 Manuel Serrano                            */
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
   void server( InputStream ip, OutputStream op ) throws IOException {
      
      switch( HopDroid.read_int( ip ) ) {
	 // sdk version
	 case (byte)'v':
	    op.write( android.os.Build.VERSION.RELEASE.getBytes() );
	    return;
	    
	 // model
	 case (byte)'m':
	    Log.d( "HopPluginBuild", "model=" + android.os.Build.MODEL );
	    op.write( "\"".getBytes() );
	    op.write( android.os.Build.MODEL.getBytes() );
	    op.write( "\"".getBytes() );
	    return;
	    
	 // product
	 case (byte)'p':
	    Log.d( "HopPluginBuild", "product=" + android.os.Build.PRODUCT );
	    op.write( "\"".getBytes() );
	    op.write( android.os.Build.PRODUCT.getBytes() );
	    op.write( "\"".getBytes() );
	    return;
      }
   }
}
      
	 

