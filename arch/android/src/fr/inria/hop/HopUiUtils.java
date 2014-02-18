/*=====================================================================*/
/*    .../hop/2.5.x/arch/android/src/fr/inria/hop/HopUiUtils.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct  1 09:13:38 2010                          */
/*    Last change :  Mon Feb 17 08:55:54 2014 (serrano)                */
/*    Copyright   :  2010-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    UI Utility functions                                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.DialogInterface;
import android.util.Log;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopUiUtils {

   // Show a simple message
   public static void alert( final Activity activity,
			     final String msg,
			     final String ok,
			     final boolean exit ) {
      AlertDialog.Builder builder = new AlertDialog.Builder( activity );
      builder.setMessage( msg )
	 .setCancelable( false )
	 .setPositiveButton( ok, new DialogInterface.OnClickListener() {
	       public void onClick( DialogInterface dialog, int id ) {
		  dialog.dismiss();
		  if( exit ) {
		     activity.setResult( activity.RESULT_CANCELED );
		     activity.finish();
		  }
	       }
	    } );
      AlertDialog alert = builder.create();
      alert.show();
   }
   
   // Show a simple message
   public static void alert( final Activity activity,
			     final String msg ) {
      alert( activity, msg, "ok", false );
   }
   
   // A failure
   public static void failExit( final Activity activity,
				final String task,
				final String msg,
				final Object o ) {
      if( o instanceof Exception ) {
	 Exception e = (Exception)o;
	 String emsg = e.getClass().getName() + ": " + e.getMessage();
	 String m = task + " " + msg + ": " + emsg;

	 Log.e( task, m );
	 alert( activity, m, "ok", true );
      } else {
	 alert( activity, "Hop Fail Exit: " + o.toString(), "ok", true );
      }
   }
}
