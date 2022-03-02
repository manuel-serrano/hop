/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopPermission.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Dec 31 10:12:32 2020                          */
/*    Last change :  Wed Mar  2 08:33:50 2022 (serrano)                */
/*    Copyright   :  2020-22 Manuel Serrano                            */
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

import android.os.*;
import android.util.Log;
import android.app.Activity;
import android.app.ProgressDialog;
import android.content.DialogInterface;
import android.content.res.*;
import android.content.Context;
import android.content.pm.*;

/*---------------------------------------------------------------------*/
/*    HopPermission ...                                                */
/*---------------------------------------------------------------------*/
public class HopPermission implements HopStage {
   Handler handler;
   Activity activity;
   
   // constructor
   public HopPermission(Activity a, Handler h) {
      super();

      handler = h;
      activity = a;
   }

   // exec
   public void exec(Context context, Object arg) {
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
	 // runtime permissions are only needed for Android SDK >= 23
	 Log.d("HopPermission", "checking permissions...");

	 try {
	    PackageManager pm = activity.getPackageManager();
	    PackageInfo info = pm
	       .getPackageInfo(context.getPackageName(), PackageManager.GET_PERMISSIONS);
	    String[] permissions = info.requestedPermissions;

	    if (permissions != null && permissions.length > 0) {
	       String[] tmp = new String[permissions.length];
	       int i, j = 0;

	       Log.d("HopPermission", "permissions.length=" + permissions.length);
	       
	       for (i = 0; i < permissions.length; i++) {
		  Log.d("HopPermission", "perm="
			+ permissions[i] + " level="
			+ pm.getPermissionInfo(permissions[i],  0).protectionLevel
			+ " granted="
			+ ((context.checkSelfPermission(permissions[i]) == PackageManager.PERMISSION_GRANTED) ? "true" : "false"));

		  if (pm.getPermissionInfo(permissions[i], 0).protectionLevel == PermissionInfo.PROTECTION_DANGEROUS) {
		     if (context.checkSelfPermission(permissions[i]) != PackageManager.PERMISSION_GRANTED) {
			if (true || !activity.shouldShowRequestPermissionRationale(permissions[i])) {
			   tmp[j++] = permissions[i];
			}
		     }
		  }
	       }

	       if (j > 0) {
		  String[] perms = new String[j];
		  System.arraycopy(tmp, 0, perms, 0, j);
	    
		  activity.requestPermissions(perms, HopLauncher.PERM_REQUEST_ID);
	       } else {
		  handler.sendEmptyMessage(HopLauncher.MSG_INSTALL_PERMISSION);
	       }
	    } else {
	       handler.sendEmptyMessage(HopLauncher.MSG_INSTALL_PERMISSION);
	    }
	 } catch (Exception e) {
	    e.printStackTrace();
	 }
      } else {
	 handler.sendEmptyMessage(HopLauncher.MSG_INSTALL_PERMISSION);
      }
   }
   
   public void abort() {
      Log.d("HopPermission", "abort");
   }
}
