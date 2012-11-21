/*=====================================================================*/
/*    .../2.4.x/arch/android/src/fr/inria/hop/HopPluginSystem.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov 21 08:34:30 2012                          */
/*    Last change :  Wed Nov 21 08:41:21 2012 (serrano)                */
/*    Copyright   :  2012 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Android system settings                                          */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.preference.*;
import android.os.*;
import android.util.Log;
import android.content.*;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginSystem extends HopPlugin {
   
   // constructor
   public HopPluginSystem( HopDroid h, String n ) {
      super( h, n );
   }

   // kill
   public void kill() {
      super.kill();
   }
   
   // battery manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
   }
}
