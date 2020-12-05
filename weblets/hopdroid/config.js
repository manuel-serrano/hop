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
const fs = require( "fs" );
const path = require( "path" );

import { config } from hop.config;

/*---------------------------------------------------------------------*/
/*    localConfig ...                                                  */
/*---------------------------------------------------------------------*/
const localConfig = {
   get enableWebdav() { return config.enableWebdav; },
   set enableWebdav( v ) { return config.enableWebdav = v; }
}
/*---------------------------------------------------------------------*/
/*    init ...                                                         */
/*---------------------------------------------------------------------*/
export function init() {
   localConfig.enableWebdav = config.enableWebdav;
   console.log( "config init enable=", localConfig.enableWebdav, config.enableWebdav );
}

/*---------------------------------------------------------------------*/
/*    update ...                                                       */
/*---------------------------------------------------------------------*/
export function update() {
   const fd = fs.openSync( path.join( config.rcDirectory, "config.json" ), "w+" );
   fs.writeSync( fd, JSON.stringify( localConfig ) );
   fs.close( fd );
}

/*---------------------------------------------------------------------*/
/*    export                                                           */
/*---------------------------------------------------------------------*/
export { localConfig as config };
