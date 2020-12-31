/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopPermission.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Dec 31 10:12:32 2020                          */
/*    Last change :  Thu Dec 31 10:40:44 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Asking runtime (dangerous) permissions for Android >= 23         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import java.util.*;
import java.net.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    HopPermission ...                                                */
/*---------------------------------------------------------------------*/
public class HopPermission implements HopStage {
   Handler handler;
   Activity activity;
   
   // constructor
   public HopPermission( Activity a, Handler h ) {
      super();

      handler = h;
      activity = a;
   }

   // exec
   public void exec( Context context, Object arg ) {
      if( Build.VERSION.SDK_INT >= Build.VERSION_CODES.M ) {
	 // runtime permissions are only needed for Android SDK >= 23
	 try {
	    PackageInfo info = activity.getPackageManager()
	       .getPackageInfo( context.getPackageName(), PackageManager.GET_PERMISSIONS );
	    String[] permissions = info.requestedPermissions;
	    int i;

	    for( i = 0; i < permissions.length(); i++ ) {
	       Log.d( "HopPermission", "permission=" + permissions[ i ] );
	    }
	 } catch (Exception e) {
	    e.printStackTrace();
	 }

      }
      handler.sendMessage( HopLauncher.MSG_INSTALL_PERMISSION);
   }
}
