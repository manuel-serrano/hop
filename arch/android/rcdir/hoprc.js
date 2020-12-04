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
/*    Default configuration                                            */
/*---------------------------------------------------------------------*/
let config = {};

/*---------------------------------------------------------------------*/
/*    Load the managed file (manipulated by Hop).                      */
/*---------------------------------------------------------------------*/
try {
   console.log( "avant load..." );
   config.init( require( "./config.json" ) );
   console.log( "apres load..." );
} catch( e ) {
   console.log( "raise load..." );
   ;
}

/*---------------------------------------------------------------------*/
/*    Anonymous user ...                                               */
/*---------------------------------------------------------------------*/
console.log( ">>> userAdd..." );
userAdd( { name: "anonymous", services: '*', directories: '*' } );
console.log( "<<< userAdd..." );

/*---------------------------------------------------------------------*/
/*    Load the user file.                                              */
/*---------------------------------------------------------------------*/
/* if( fs.existsSync( ) ) {                                            */
/* }                                                                   */
