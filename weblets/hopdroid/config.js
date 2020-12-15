/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/weblets/hopdroid/config.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec  4 14:20:21 2020                          */
/*    Last change :  Fri Dec  4 14:23:23 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    User on-device configuration                                     */
/*=====================================================================*/
"use hopscript"

const fs = require( "fs" );
const path = require( "path" );

import { phone } from './phone.js';
import { config } from hop.config;

/*---------------------------------------------------------------------*/
/*    localConfig ...                                                  */
/*---------------------------------------------------------------------*/
const localConfig = {
   get enableWebdav() { return config.enableWebdav; },
   set enableWebdav( v ) { return config.enableWebdav = v; },
   theme: undefined
}
/*---------------------------------------------------------------------*/
/*    init ...                                                         */
/*---------------------------------------------------------------------*/
export function init() {
   localConfig.enableWebdav = config.enableWebdav;
}

/*---------------------------------------------------------------------*/
/*    purge ...                                                        */
/*---------------------------------------------------------------------*/
function purge( config ) {
   if( ("password" in config) && !config.password ) delete config.password;
}

/*---------------------------------------------------------------------*/
/*    update ...                                                       */
/*---------------------------------------------------------------------*/
export function update() {
   const fd = fs.openSync( path.join( config.rcDirectory, "config.json" ), "w+" );
   purge( localConfig );
   fs.writeSync( fd, JSON.stringify( localConfig ) );
   fs.close( fd );
}

/*---------------------------------------------------------------------*/
/*    export                                                           */
/*---------------------------------------------------------------------*/
export { localConfig as config };
