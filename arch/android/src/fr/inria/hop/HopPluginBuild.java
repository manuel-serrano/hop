/*=====================================================================*/
/*    .../hopdac/arch/android/src/fr/inria/hop/HopPluginBuild.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Nov 30 17:35:50 2010                          */
/*    Last change :  Wed Jul  6 10:51:10 2016 (serrano)                */
/*    Copyright   :  2010-16 Manuel Serrano                            */
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
	    op.write( "\"".getBytes() );
	    op.write( android.os.Build.VERSION.RELEASE.getBytes() );
	    op.write( "\"".getBytes() );
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
      }
   }
}
      
	 

