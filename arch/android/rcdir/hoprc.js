/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/arch/android/rcdir/hoprc.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec  4 09:30:34 2020                          */
/*    Last change :  Fri Dec  4 09:31:14 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Default hoprc.js Android file                                    */
/*=====================================================================*/
const path = require( "path" );
const fs = require( "fs" );

import { add as userAdd } from hop.user;
import { config } from hop.config;

/*---------------------------------------------------------------------*/
/*    Load the managed file (manipulated by Hop).                      */
/*---------------------------------------------------------------------*/
const cfgpath = path.join( config.rcDirectory, "config.json" );

try {
   if( fs.existsSync( cfgpath ) ) {
      Object.assign( config, require( cfgpath ) );
   }
} catch( e ) {
   console.error( "hoprc.js", "cannot load \"" + cfgpath + "\"" );
   console.error( e.toString() );
}

/*---------------------------------------------------------------------*/
/*    Anonymous user ...                                               */
/*---------------------------------------------------------------------*/
userAdd( { name: "anonymous", services: '*', directories: '*' } );

