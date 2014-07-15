/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/share/hop-require.js              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue May 27 06:09:16 2014                          */
/*    Last change :  Thu Jul 10 17:09:26 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Client side implementation of the "require" form                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_modules ...                                                  */
/*    -------------------------------------------------------------    */
/*    Builtin modules                                                  */
/*---------------------------------------------------------------------*/
var hop_modules = {
   hop: {
      List: sc_list,
      Cons: sc_cons
   }
}

/*---------------------------------------------------------------------*/
/*    hop_requires ...                                                 */
/*---------------------------------------------------------------------*/
var hop_requires = {};

/*---------------------------------------------------------------------*/
/*    require ...                                                      */
/*---------------------------------------------------------------------*/
function require( url ) {
   if( hop_modules[ url ] ) {
      return hop_modules[ url ];
   } else {
      if( hop_requires[ url ] ) {
	 var exports = hop_requires[ url ]();
	 hop_modules[ url ] = exports;
	 return exports;
      } else {
	 throw new Error( "Cannot require client-side module \"" + url + "\"" );
      }
   }
}

   
