/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopStage.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Tue Sep 28 08:26:30 2010                          */
/*    Last change :  Fri May 15 18:05:14 2020 (serrano)                */
/*    Copyright   :  2010-20 Marcos Dione & Manuel Serrano             */
/*    -------------------------------------------------------------    */
/*    Hop Launcher Stage                                               */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;
import android.content.Context;

/*---------------------------------------------------------------------*/
/*    HopStage                                                         */
/*---------------------------------------------------------------------*/
public interface HopStage {
   abstract void exec( Context context );
   abstract void abort();
}
