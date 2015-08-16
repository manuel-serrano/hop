/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopZeroconf.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  7 12:01:04 2012                          */
/*    Last change :  Wed Feb  6 11:05:51 2013 (serrano)                */
/*    Copyright   :  2012-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Java abstract class for Zeroconf implementations                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

/*---------------------------------------------------------------------*/
/*    The interface                                                    */
/*---------------------------------------------------------------------*/
public abstract class HopZeroconf {
   HopDroid hopdroid;
 
   // constructor
   public HopZeroconf( HopDroid h ) {
      hopdroid = h;
   }
  
   public abstract void start( String hostname );
   public abstract void stop();
   public abstract String version();
   public abstract void addServiceTypeListener( final String type, final String event );
   public abstract void addServiceListener();
   public abstract void addTypeListener( final String type );
   public abstract void publish( final String name, final int port, final String type, final String[] props );
}
