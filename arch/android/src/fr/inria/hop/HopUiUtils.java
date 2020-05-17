/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopUiUtils.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct  1 09:13:38 2010                          */
/*    Last change :  Sun May 17 10:26:50 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
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
      try {
	 Log.e( "HopUiUtils", "alert.1" );
	 AlertDialog.Builder builder = new AlertDialog.Builder( activity );
	 Log.e( "HopUiUtils", "alert.2" );
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
	 Log.e( "HopUiUtils", "alert.3" );
	 AlertDialog alert = builder.create();
	 Log.e( "HopUiUtils", "alert.4" );
	 alert.show();
	 Log.e( "HopUiUtils", "alert.5" );
      } catch( Exception e ) {
	 Log.e( "HopUiUtils", "e=" + e.toString() );
	 e.printStackTrace();
      }
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
	 Log.d( "HopUiUtils", "msg=" + msg );
	 Log.d( "HopUiUtils", "o=" + o.toString() );
	 Log.d( "HopUiUtils", "task=" + task );
	 alert( activity, "Hop Fail Exit: " + o.toString(), "ok", true );
      }
   }
}
