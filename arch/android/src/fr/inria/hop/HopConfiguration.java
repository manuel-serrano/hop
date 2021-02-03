/*=====================================================================*/
/*    .../hop/arch/android/src/fr/inria/hop/HopConfiguration.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 11 05:54:16 2020                          */
/*    Last change :  Fri Dec 11 12:33:12 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    JavaConfiguration                                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.os.*;
import android.util.Log;
import android.content.*;
import android.widget.TextView;
import android.content.res.*;
import android.content.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopConfiguration {
   static String toString( Configuration conf ) {
      int currentNightMode = conf.uiMode & Configuration.UI_MODE_NIGHT_MASK;

      String cfg = "(";

      cfg += " theme: " +
	 ((currentNightMode == conf.UI_MODE_NIGHT_NO) ? "\"light\""
	  : (currentNightMode == conf.UI_MODE_NIGHT_YES) ? "\"dark\""
	  : "\"default\"");

      cfg += ")";

      return cfg;
   }
}
