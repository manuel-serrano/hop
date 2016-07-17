/*=====================================================================*/
/*    .../3.1.x/arch/android/src/fr/inria/hop/HopPluginPrefs.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jul 17 13:19:20 2016                          */
/*    Last change :  Sun Jul 17 14:18:17 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Android preferences plugins                                      */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginPrefs extends HopPlugin {
   //instance variables
   final Resources res;
   final SharedPreferences sp;
   
   // constructor
   public HopPluginPrefs( HopDroid h, String n ) {
      super( h, n );

      res = getResources();
      sp = PreferenceManager.getDefaultSharedPreferences( h.activity );
   }

   // kill
   public void kill() {
      super.kill();
   }

   String get( String key ) {
      return sp.getString( key, "" );
   }
   
   String set( String key, String value ) {
      return sp.edit().putString( key, value );
   }
   
   // server
   void server( InputStream ip, OutputStream op ) throws IOException {
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'s':
	    // set
	    return;
	 case (byte)'g':
	    // get
	    get( op, HopDroid.read_string( ip ) );
	    return;
      }
   }
}
