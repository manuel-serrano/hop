/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopIntent.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jul  5 09:42:40 2016                          */
/*    Last change :  Thu Dec 24 17:50:42 2020 (serrano)                */
/*    Copyright   :  2016-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HopIntent, just to keep track of the activity that created it.   */
/*    This is only needed by HopHzService.                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.util.Log;
import android.os.*;

import java.util.*;
import java.util.concurrent.ArrayBlockingQueue;
import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    HopIntent                                                        */
/*---------------------------------------------------------------------*/
public class HopIntent extends HopIntent {
   Activity activity;
   
   public HopIntent( Context ct, Class cz, Activity a ) {
      super( ct, cz );
      activity = a;
   }
}
