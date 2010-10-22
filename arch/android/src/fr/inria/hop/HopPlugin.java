/*=====================================================================*/
/*    .../hop/2.2.x/arch/android/src/fr/inria/hop/HopPlugin.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct 19 09:38:21 2010                          */
/*    Last change :  Fri Oct 22 11:53:09 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Root class for HopPlugins                                        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public abstract class HopPlugin {
   // instance variables
   public HopAndroid handroid;
   public Activity activity;
   public String name;

   public HopPlugin( HopAndroid h, Activity a, String n ) {
      handroid = h;
      activity = a;
      name = n;
   }

   // the server
   abstract void server( InputStream ip, OutputStream op ) throws IOException;
}
