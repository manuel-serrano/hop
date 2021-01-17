/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/systime/systime.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Aug 23 08:07:57 2015                          */
/*    Last change :  Wed May 17 14:21:57 2017 (serrano)                */
/*    Copyright   :  2015-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    An example of SYSTEM                                             */
/*    -------------------------------------------------------------    */
/*    run: hop --no-server -v -g system.js                             */
/*=====================================================================*/

import { systemSync as system } from hop.system;

const { status, data } = system( "ls -l /tmp" );

if( status ) {
   console.log( "error", status, data );
} else {
   console.log( data.split() );
}
