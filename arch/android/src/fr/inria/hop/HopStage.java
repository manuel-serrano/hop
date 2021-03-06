/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopStage.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Thu Dec 24 18:11:36 2020 (serrano)                */
/*    Copyright   :  2010-20 Marcos Dione & Manuel Serrano             */
/*    -------------------------------------------------------------    */
/*    Hop Launcher Stage                                               */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.content.Context;
import android.os.*;

/*---------------------------------------------------------------------*/
/*    HopStage                                                         */
/*---------------------------------------------------------------------*/
public interface HopStage {
   abstract void exec( Context context, Object arg );
   abstract void abort();
}
