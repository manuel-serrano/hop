/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopAppRemoved.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 17 18:30:34 2010                          */
/*    Last change :  Fri Jan  1 06:31:48 2021 (serrano)                */
/*    Copyright   :  2010-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    APP AppRemoved receiver                                          */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;
 
import android.content.*;
import android.util.Log;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopAppRemoved extends BroadcastReceiver {
   @Override public void onReceive( Context context, Intent intent ) {
      Log.d( "HopAppRemoved", "received notification" );

      final PendingResult pendingResult = goAsync();
      Task asyncTask = new Task( pendingResult, intent );
      asyncTask.execute();
   }

   private static class Task extends AsyncTask<String, Integer, String> {
      private final PendingResult pendingResult;
      private final Intent intent;

      private Task(PendingResult pendingResult, Intent intent) {
	 this.pendingResult = pendingResult;
	 this.intent = intent;
      }

      @Override
      protected String doInBackground(String... strings) {
	 String pkgname = intent.getData().getEncodedSchemeSpecificPart();

	 synchronized( pkgname ) {
	    HopHzLauncher.removeHopHz( pkgname );
	    pkgname.wait();
	    Log.d( "HopAppRemoved", "receiver complete" );
	 }
      }

      @Override
      protected void onPostExecute(String s) {
	 super.onPostExecute(s);
	 // Must call finish() so the BroadcastReceiver can be recycled.
	 pendingResult.finish();
      }
   }

   }
}
