/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopPluginBuild.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Nov 30 17:35:50 2010                          */
/*    Last change :  Wed Feb  6 09:19:17 2013 (serrano)                */
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
	    op.write( android.os.Build.MODEL.getBytes() );
	    return;
	    
	 // product
	 case (byte)'p':
	    op.write( android.os.Build.PRODUCT.getBytes() );
	    return;
      }
   }
}
      
	 

