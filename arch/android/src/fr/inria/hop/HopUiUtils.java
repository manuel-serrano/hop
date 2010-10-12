/*=====================================================================*/
/*    .../hop/linux/android/src/fr/inria/hop/HopUiUtils.java           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct  1 09:13:38 2010                          */
/*    Last change :  Sat Oct  9 07:11:27 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
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
		  if( exit ) activity.finish();
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
   public static void fail( final Activity activity,
			    final String task,
			    final String msg,
			    final Exception e,
			    final boolean exit ) {
      String emsg = e.getMessage();
      if( emsg == null ) emsg = e.getClass().getName();
      String m = task + " " + msg + ": " + emsg;

      Log.e( task, m );
      alert( activity, m, "ok", exit );
   }
   
   // A failure
   public static void fail( final Activity activity,
			    final String task,
			    final String msg,
			    final Exception e ) {
      fail( activity, task, msg, e, true );
   }
}
